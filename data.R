# ==============================================================================
# 1. REQUIRED PACKAGES 
# ==============================================================================

required_packages <- c("dplyr", "tidyr", "readr", "stringr", "lubridate", "tibble", "DBI", "duckdb")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    paste0(
      "The following required packages are not installed: ",
      paste(missing_packages, collapse = ", "),
      "."
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages(
  invisible(lapply(required_packages, library, character.only = TRUE))
)


# ==============================================================================
# 2. ANALYSIS SETTINGS
# ==============================================================================

TIME_ZONE <- "UTC"
WINDOW_START_HOURS <- 0
WINDOW_END_HOURS <- 24
PRINT_EXCLUDED_EPISODE_IDS <- FALSE

INPUT_DIR <- Sys.getenv("MIMIC_INPUT_DIR", unset = ".")
OUTPUT_DIR <- Sys.getenv("MIMIC_OUTPUT_DIR", unset = ".")

# ==============================================================================
# 2.1 INPUT AND OUTPUT DIRECTORIES
# ==============================================================================

INPUT_DIR <- normalizePath(
  INPUT_DIR,
  winslash = "/",
  mustWork = TRUE
)

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(
    OUTPUT_DIR,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

if (!dir.exists(OUTPUT_DIR)) {
  stop(
    paste0(
      "The output directory could not be created: ",
      OUTPUT_DIR,
      "."
    ),
    call. = FALSE
  )
}

OUTPUT_DIR <- normalizePath(
  OUTPUT_DIR,
  winslash = "/",
  mustWork = TRUE
)

cat(
  "\nInput directory: ",
  INPUT_DIR,
  "\nOutput directory: ",
  OUTPUT_DIR,
  "\n",
  sep = ""
)
# ==============================================================================
# 3. GENERAL FUNCTIONS
# ==============================================================================

input_path <- function(file_name) {
  path <- file.path(INPUT_DIR, file_name)
  
  if (!file.exists(path)) {
    stop(paste0("Required input file not found: ", file_name, "."), call. = FALSE)
  }
  
  normalizePath(path, winslash = "/", mustWork = TRUE)
}


assert_unique_key <- function(data, key_columns, object_name, allow_missing = FALSE) {
  absent_columns <- setdiff(key_columns, names(data))
  
  if (length(absent_columns) > 0L) {
    stop(
      paste0(
        object_name,
        " does not contain the following key columns: ",
        paste(absent_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  
  missing_key_rows <- data %>%
    filter(if_any(all_of(key_columns), is.na)) %>%
    nrow()
  
  if (!allow_missing && missing_key_rows > 0L) {
    stop(
      paste0(object_name, " contains ", missing_key_rows, " rows with a missing key."),
      call. = FALSE
    )
  }
  
  duplicated_keys <- data %>%
    count(across(all_of(key_columns)), name = "n") %>%
    filter(n > 1L)
  
  if (nrow(duplicated_keys) > 0L) {
    stop(
      paste0(
        object_name,
        " contains duplicated values for key: ",
        paste(key_columns, collapse = " + "),
        "."
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


quote_sql_path <- function(connection, path) {
  normalized_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  as.character(DBI::dbQuoteString(connection, normalized_path))
}


# ==============================================================================
# 4. DUCKDB CONNECTION
# ==============================================================================

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

DBI::dbExecute(con, "SET memory_limit = '8GB';")

duckdb_temp_directory <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)

DBI::dbExecute(
  con,
  paste0(
    "SET temp_directory = ",
    as.character(DBI::dbQuoteString(con, duckdb_temp_directory)),
    ";"
  )
)

DBI::dbExecute(con, "SET preserve_insertion_order = false;")

# ==============================================================================
# 5. HOSPITAL ADMISSIONS
# ==============================================================================

# BigQuery export query:
#
# SELECT *
# FROM `physionet-data.mimiciv_v3_1_hosp.admissions`;
#

admissions_csv_path <- quote_sql_path(con, input_path("admissions.csv"))

admissions_view_query <- sprintf(
  "
  CREATE OR REPLACE TEMP VIEW admissions_selected AS
  SELECT
    NULLIF(TRIM(subject_id), '') AS subject_id,
    NULLIF(TRIM(hadm_id), '') AS hadm_id,
    TRY_CAST(admittime AS TIMESTAMP) AS admittime,
    TRY_CAST(dischtime AS TIMESTAMP) AS dischtime,
    admission_type, admit_provider_id, admission_location, discharge_location,
    insurance, language, marital_status, race AS race_admission,
    TRY_CAST(edregtime AS TIMESTAMP) AS edregtime,
    TRY_CAST(edouttime AS TIMESTAMP) AS edouttime
   FROM read_csv_auto(
    %s,
    header = TRUE,
    all_varchar = TRUE,
    sample_size = -1,
    ignore_errors = FALSE
  );
  ",
  admissions_csv_path
)

DBI::dbExecute(con, admissions_view_query)


# ==============================================================================
# 5.1 DERIVED AGE
# ==============================================================================

derived_age_csv_path <- quote_sql_path(con, input_path("derived_age.csv"))

derived_age_view_query <- sprintf(
  "
  CREATE OR REPLACE TEMP VIEW derived_age_selected AS
  SELECT
    NULLIF(TRIM(subject_id), '') AS subject_id,
    NULLIF(TRIM(hadm_id), '') AS hadm_id,
    TRY_CAST(age AS DOUBLE) AS age
  FROM read_csv_auto(
    %s,
    header = TRUE,
    all_varchar = TRUE,
    sample_size = -1,
    ignore_errors = FALSE
  );
  ",
  derived_age_csv_path
)

DBI::dbExecute(con, derived_age_view_query)


derived_age_key_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*)::BIGINT AS row_n,
    COUNT(DISTINCT (subject_id, hadm_id))::BIGINT AS distinct_key_n,
    COUNT(*) FILTER (
      WHERE subject_id IS NULL OR hadm_id IS NULL
    )::BIGINT AS missing_key_n,
    COUNT(*) FILTER (
      WHERE age IS NULL OR NOT isfinite(age) OR age < 0
    )::BIGINT AS invalid_age_n
  FROM derived_age_selected;
  "
) %>%
  tibble::as_tibble()


if (
  derived_age_key_check$row_n != derived_age_key_check$distinct_key_n ||
  derived_age_key_check$missing_key_n != 0L ||
  derived_age_key_check$invalid_age_n != 0L
) {
  stop(
    "derived_age.csv has duplicated keys, missing identifiers, or invalid ages.",
    call. = FALSE
  )
}


# ==============================================================================
# 6. EMERGENCY DEPARTMENT STAYS
# ==============================================================================

# BigQuery export query:
#
# SELECT *
# FROM `physionet-data.mimiciv_ed.edstays`;
#

edstays_csv_path <- quote_sql_path(con, input_path("edstays.csv"))

edstays_view_query <- sprintf(
  "
  CREATE OR REPLACE TEMP VIEW edstays_selected AS
  SELECT
    NULLIF(TRIM(subject_id), '') AS subject_id,
    NULLIF(TRIM(hadm_id), '') AS hadm_id,
    NULLIF(TRIM(stay_id), '') AS stay_id,
    TRY_CAST(intime AS TIMESTAMP) AS ed_intime,
    TRY_CAST(outtime AS TIMESTAMP) AS ed_outtime,
    gender, race AS race_ed, arrival_transport, disposition
  FROM read_csv_auto(
    %s,
    header = TRUE,
    all_varchar = TRUE,
    sample_size = -1,
    ignore_errors = FALSE
  );
  ",
  edstays_csv_path
)

DBI::dbExecute(con, edstays_view_query)


# ==============================================================================
# 7. EMERGENCY DEPARTMENT EPISODE SPINE
# ==============================================================================

adult_source_eligibility <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*)::BIGINT AS ed_stay_n,
    COUNT(*) FILTER (
      WHERE age.age >= 18
    )::BIGINT AS adult_ed_stay_n,
    COUNT(*) FILTER (
      WHERE age.age < 18
    )::BIGINT AS pediatric_ed_stay_n,
    COUNT(*) FILTER (
      WHERE age.age IS NULL
    )::BIGINT AS age_unavailable_ed_stay_n
  FROM edstays_selected AS ed
  LEFT JOIN derived_age_selected AS age
    ON ed.subject_id = age.subject_id
   AND ed.hadm_id = age.hadm_id;
  "
) %>%
  tibble::as_tibble()


cat("\nADULT SOURCE-COHORT ELIGIBILITY\n")
print(adult_source_eligibility, width = Inf)

episode_spine <- DBI::dbGetQuery(
  con,
  "
  SELECT
    e.subject_id, e.hadm_id, e.stay_id, e.ed_intime, e.ed_outtime,
    e.gender, e.race_ed, e.arrival_transport, e.disposition,
    a.admittime, a.dischtime, a.admission_type,
    a.admit_provider_id, a.admission_location, a.discharge_location,
    a.insurance, a.language, a.marital_status, a.race_admission,
    a.edregtime, a.edouttime,
    age.age
  FROM edstays_selected AS e
  LEFT JOIN admissions_selected AS a
    ON e.subject_id = a.subject_id
   AND e.hadm_id = a.hadm_id
  INNER JOIN derived_age_selected AS age
    ON e.subject_id = age.subject_id
   AND e.hadm_id = age.hadm_id
  WHERE age.age >= 18;
  "
) %>%
  tibble::as_tibble() %>%
  mutate(
    episode_id = paste(subject_id, stay_id, sep = "_"),
    has_hospital_admission = !is.na(hadm_id),
    ed_length_hours = as.numeric(difftime(ed_outtime, ed_intime, units = "hours")),
    invalid_ed_interval_flag = is.na(ed_intime) | is.na(ed_outtime) | ed_outtime < ed_intime,
    lab_window_start = ed_intime,
    lab_window_end = ed_intime + lubridate::hours(WINDOW_END_HOURS)
  ) %>%
  group_by(subject_id) %>%
  mutate(
    repeated_subject_flag = n() > 1L,
    n_ed_episodes_subject = n()
  ) %>%
  ungroup() %>%
  group_by(hadm_id) %>%
  mutate(
    n_ed_stays_hadm = case_when(
      is.na(hadm_id) ~ NA_integer_,
      TRUE ~ as.integer(n())
    )
  ) %>%
  ungroup() %>%
  mutate(
    multiple_ed_stays_same_hadm_flag = !is.na(hadm_id) & n_ed_stays_hadm > 1L
  ) %>%
  relocate(episode_id, subject_id, hadm_id, stay_id)

assert_unique_key(episode_spine, "episode_id", "episode_spine")
assert_unique_key(episode_spine, "stay_id", "episode_spine")


# ==============================================================================
# 10. REGISTER THE ED EPISODE SPINE IN DUCKDB
# ==============================================================================

DBI::dbWriteTable(
  con,
  "episode_spine_table",
  as.data.frame(episode_spine),
  overwrite = TRUE,
  temporary = TRUE
)


# ==============================================================================
# 11. COMPLETE BLOOD COUNT
# ==============================================================================

# BigQuery export query:
#
# SELECT *
# FROM `physionet-data.mimiciv_3_1_derived.complete_blood_count`;
#

cbc_csv_path <- quote_sql_path(con, input_path("derived_cbc.csv"))

# Date-only values remain missing; a complete date and time are required.
cbc_view_query <- sprintf(
  "
  CREATE OR REPLACE TEMP VIEW cbc_source AS
  SELECT
    NULLIF(TRIM(subject_id), '') AS cbc_subject_id,
    NULLIF(TRIM(hadm_id), '') AS cbc_hadm_id,
    NULLIF(TRIM(specimen_id), '') AS specimen_id,
    NULLIF(TRIM(charttime), '') AS cbc_charttime_raw,

    CASE
      WHEN LENGTH(TRIM(charttime)) >= 19
       AND SUBSTR(TRIM(charttime), 5, 1) = '-'
       AND SUBSTR(TRIM(charttime), 8, 1) = '-'
       AND SUBSTR(TRIM(charttime), 11, 1) IN (' ', 'T')
       AND SUBSTR(TRIM(charttime), 14, 1) = ':'
       AND SUBSTR(TRIM(charttime), 17, 1) = ':'
      THEN TRY_CAST(REPLACE(TRIM(charttime), 'T', ' ') AS TIMESTAMP)
      ELSE NULL
    END AS cbc_charttime,

    TRY_CAST(hematocrit AS DOUBLE) AS hematocrit,
    TRY_CAST(hemoglobin AS DOUBLE) AS hemoglobin,
    TRY_CAST(mch AS DOUBLE) AS mch,
    TRY_CAST(mchc AS DOUBLE) AS mchc,
    TRY_CAST(mcv AS DOUBLE) AS mcv,
    TRY_CAST(platelet AS DOUBLE) AS platelet,
    TRY_CAST(rbc AS DOUBLE) AS rbc,
    TRY_CAST(rdw AS DOUBLE) AS rdw,
    TRY_CAST(rdwsd AS DOUBLE) AS rdwsd,
    TRY_CAST(wbc AS DOUBLE) AS wbc_cbc

  FROM read_csv_auto(
    %s,
    header = TRUE,
    all_varchar = TRUE,
    sample_size = -1,
    ignore_errors = FALSE
  );
  ",
  cbc_csv_path
)

DBI::dbExecute(con, cbc_view_query)


# ==============================================================================
# 12. BLOOD DIFFERENTIAL
# ==============================================================================

# BigQuery export query:
#
# SELECT *
# FROM `physionet-data.mimiciv_3_1_derived.blood_differential`;
#

blood_differential_csv_path <- quote_sql_path(con, input_path("derived_blood_diff.csv"))

blood_differential_view_query <- sprintf(
  "
  CREATE OR REPLACE TEMP VIEW blood_differential_source AS
  SELECT
    NULLIF(TRIM(subject_id), '') AS diff_subject_id,
    NULLIF(TRIM(hadm_id), '') AS diff_hadm_id,
    NULLIF(TRIM(specimen_id), '') AS specimen_id,
    NULLIF(TRIM(charttime), '') AS diff_charttime_raw,

    CASE
      WHEN LENGTH(TRIM(charttime)) >= 19
       AND SUBSTR(TRIM(charttime), 5, 1) = '-'
       AND SUBSTR(TRIM(charttime), 8, 1) = '-'
       AND SUBSTR(TRIM(charttime), 11, 1) IN (' ', 'T')
       AND SUBSTR(TRIM(charttime), 14, 1) = ':'
       AND SUBSTR(TRIM(charttime), 17, 1) = ':'
      THEN TRY_CAST(REPLACE(TRIM(charttime), 'T', ' ') AS TIMESTAMP)
      ELSE NULL
    END AS diff_charttime,

    TRY_CAST(wbc AS DOUBLE) AS wbc_diff,
    TRY_CAST(basophils_abs AS DOUBLE) AS basophils_abs,
    TRY_CAST(eosinophils_abs AS DOUBLE) AS eosinophils_abs,
    TRY_CAST(lymphocytes_abs AS DOUBLE) AS lymphocytes_abs_derived,
    TRY_CAST(monocytes_abs AS DOUBLE) AS monocytes_abs_derived,
    TRY_CAST(neutrophils_abs AS DOUBLE) AS neutrophils_abs_derived,
    TRY_CAST(basophils AS DOUBLE) AS basophils,
    TRY_CAST(eosinophils AS DOUBLE) AS eosinophils,
    TRY_CAST(lymphocytes AS DOUBLE) AS lymphocytes,
    TRY_CAST(monocytes AS DOUBLE) AS monocytes,
    TRY_CAST(neutrophils AS DOUBLE) AS neutrophils,
    TRY_CAST(atypical_lymphocytes AS DOUBLE) AS atypical_lymphocytes,
    TRY_CAST(bands AS DOUBLE) AS bands,
    TRY_CAST(immature_granulocytes AS DOUBLE) AS immature_granulocytes,
    TRY_CAST(metamyelocytes AS DOUBLE) AS metamyelocytes,
    TRY_CAST(nrbc AS DOUBLE) AS nrbc

  FROM read_csv_auto(
    %s,
    header = TRUE,
    all_varchar = TRUE,
    sample_size = -1,
    ignore_errors = FALSE
  );
  ",
  blood_differential_csv_path
)

DBI::dbExecute(con, blood_differential_view_query)

# ==============================================================================
# 14. MERGE CBC AND DIFFERENTIAL AND ASSIGN SPECIMENS TO ED EPISODES
# ==============================================================================

# The CBC table is primary. Differential data are joined by specimen_id.
# Official absolute counts are preferred; missing counts are recalculated from
# WBC and percentages. Specimens are assigned within ED arrival through +24 h.
#
# Assignment hierarchy:
# 1. Actual ED interval contains charttime.
# 2. Smallest nonnegative time from ED arrival.
# 3. Latest ED arrival.
# 4. Smallest stay_id.

cbc_assignment_query <- "
CREATE OR REPLACE TEMP TABLE cbc_assigned_0_24h AS

WITH cbc_differential_base AS (
  SELECT
    c.cbc_subject_id, c.cbc_hadm_id, c.specimen_id,
    c.cbc_charttime_raw, c.cbc_charttime,
    c.hematocrit, c.hemoglobin, c.mch, c.mchc, c.mcv,
    c.platelet, c.rbc, c.rdw, c.rdwsd, c.wbc_cbc,

    d.diff_subject_id, d.diff_hadm_id, d.diff_charttime_raw, d.diff_charttime,
    d.wbc_diff, d.basophils_abs, d.eosinophils_abs,
    d.lymphocytes_abs_derived, d.monocytes_abs_derived, d.neutrophils_abs_derived,
    d.basophils, d.eosinophils, d.lymphocytes, d.monocytes, d.neutrophils,
    d.atypical_lymphocytes, d.bands, d.immature_granulocytes,
    d.metamyelocytes, d.nrbc,

    COALESCE(c.cbc_subject_id, d.diff_subject_id) AS subject_id_lab,
    COALESCE(c.cbc_hadm_id, d.diff_hadm_id) AS hadm_id_lab,
    d.specimen_id IS NOT NULL AS blood_differential_available_flag,

    CASE
      WHEN c.cbc_subject_id IS NOT NULL
       AND d.diff_subject_id IS NOT NULL
       AND c.cbc_subject_id <> d.diff_subject_id
      THEN TRUE ELSE FALSE
    END AS cbc_diff_subject_mismatch_flag,

    CASE
      WHEN c.cbc_hadm_id IS NOT NULL
       AND d.diff_hadm_id IS NOT NULL
       AND c.cbc_hadm_id <> d.diff_hadm_id
      THEN TRUE ELSE FALSE
    END AS cbc_diff_hadm_mismatch_flag,

    CASE
      WHEN c.cbc_charttime IS NOT NULL AND d.diff_charttime IS NOT NULL
      THEN date_diff('microsecond', c.cbc_charttime, d.diff_charttime) / 60000000.0
      ELSE NULL
    END AS cbc_diff_time_difference_minutes,

    CASE
      WHEN c.cbc_charttime IS NOT NULL
       AND d.diff_charttime IS NOT NULL
       AND ABS(
         date_diff('microsecond', c.cbc_charttime, d.diff_charttime) / 60000000.0
       ) > 1
      THEN TRUE ELSE FALSE
    END AS cbc_diff_time_mismatch_flag,

    COALESCE(c.wbc_cbc, d.wbc_diff) AS wbc_for_calculation

  FROM cbc_source AS c
  LEFT JOIN blood_differential_source AS d USING (specimen_id)
),

recalculated_counts AS (
  SELECT
    *,

    CASE
      WHEN wbc_cbc IS NOT NULL AND wbc_diff IS NOT NULL
      THEN ABS(wbc_cbc - wbc_diff)
      ELSE NULL
    END AS wbc_absolute_difference,

    CASE
      WHEN wbc_cbc IS NOT NULL AND wbc_diff IS NOT NULL
      THEN ABS(wbc_cbc - wbc_diff) /
           GREATEST(ABS(wbc_cbc), ABS(wbc_diff), 0.0001)
      ELSE NULL
    END AS wbc_relative_difference,

    CASE
      WHEN wbc_cbc IS NOT NULL
       AND wbc_diff IS NOT NULL
       AND ABS(wbc_cbc - wbc_diff) /
           GREATEST(ABS(wbc_cbc), ABS(wbc_diff), 0.0001) > 0.05
      THEN TRUE ELSE FALSE
    END AS wbc_discordance_flag,

    CASE
      WHEN wbc_for_calculation IS NOT NULL AND lymphocytes IS NOT NULL
      THEN wbc_for_calculation * lymphocytes / 100.0
      ELSE NULL
    END AS lymphocytes_abs_recalculated,

    CASE
      WHEN wbc_for_calculation IS NOT NULL AND monocytes IS NOT NULL
      THEN wbc_for_calculation * monocytes / 100.0
      ELSE NULL
    END AS monocytes_abs_recalculated,

    CASE
      WHEN wbc_for_calculation IS NOT NULL AND neutrophils IS NOT NULL
      THEN wbc_for_calculation * neutrophils / 100.0
      ELSE NULL
    END AS neutrophils_abs_recalculated

  FROM cbc_differential_base
),

final_counts AS (
  SELECT
    *,

    COALESCE(
      lymphocytes_abs_derived,
      lymphocytes_abs_recalculated
    ) AS lymphocytes_abs_final,

    COALESCE(
      monocytes_abs_derived,
      monocytes_abs_recalculated
    ) AS monocytes_abs_final,

    COALESCE(
      neutrophils_abs_derived,
      neutrophils_abs_recalculated
    ) AS neutrophils_abs_final,

    CASE
      WHEN lymphocytes_abs_derived IS NOT NULL THEN 'official_derived_value'
      WHEN lymphocytes_abs_recalculated IS NOT NULL THEN 'recalculated_in_R'
      ELSE 'missing'
    END AS lymphocytes_abs_source,

    CASE
      WHEN monocytes_abs_derived IS NOT NULL THEN 'official_derived_value'
      WHEN monocytes_abs_recalculated IS NOT NULL THEN 'recalculated_in_R'
      ELSE 'missing'
    END AS monocytes_abs_source,

    CASE
      WHEN neutrophils_abs_derived IS NOT NULL THEN 'official_derived_value'
      WHEN neutrophils_abs_recalculated IS NOT NULL THEN 'recalculated_in_R'
      ELSE 'missing'
    END AS neutrophils_abs_source

  FROM recalculated_counts
),

candidate_episode_links AS (
  SELECT
    e.episode_id,
    e.subject_id AS subject_id_episode,
    e.hadm_id AS episode_hadm_id,
    e.stay_id, e.ed_intime, e.ed_outtime,
    e.repeated_subject_flag, e.multiple_ed_stays_same_hadm_flag,
    c.*,

    date_diff(
      'microsecond',
      e.ed_intime,
      c.cbc_charttime
    ) / 3600000000.0 AS cbc_hours_from_ed,

    CASE
      WHEN e.ed_outtime IS NOT NULL
       AND c.cbc_charttime BETWEEN e.ed_intime AND e.ed_outtime
      THEN 1 ELSE 0
    END AS cbc_within_ed_interval_integer

  FROM episode_spine_table AS e
  INNER JOIN final_counts AS c
    ON e.hadm_id = c.hadm_id_lab
   AND c.cbc_charttime BETWEEN e.ed_intime
                           AND e.ed_intime + INTERVAL '24 hours'

  WHERE e.hadm_id IS NOT NULL
    AND e.ed_intime IS NOT NULL
    AND c.hadm_id_lab IS NOT NULL
    AND c.cbc_charttime IS NOT NULL
),

ranked_episode_links AS (
  SELECT
    *,

    COUNT(*) OVER (
      PARTITION BY specimen_id
    ) AS cbc_n_candidate_episodes,

    ROW_NUMBER() OVER (
      PARTITION BY specimen_id
      ORDER BY
        cbc_within_ed_interval_integer DESC,
        date_diff('microsecond', ed_intime, cbc_charttime) ASC,
        ed_intime DESC,
        TRY_CAST(stay_id AS BIGINT) ASC,
        stay_id ASC
    ) AS cbc_episode_assignment_rank

  FROM candidate_episode_links
),

assigned_episode_links AS (
  SELECT
    *,

    cbc_within_ed_interval_integer = 1 AS cbc_within_ed_interval_flag,
    cbc_n_candidate_episodes > 1 AS cbc_assignment_ambiguous_flag,

    CASE
      WHEN cbc_n_candidate_episodes = 1
        THEN 'single_candidate_episode'
      WHEN cbc_within_ed_interval_integer = 1
        THEN 'within_ed_interval_preferred'
      WHEN cbc_n_candidate_episodes > 1
        THEN 'nearest_prior_ed_intime_within_24h'
      ELSE 'unclassified'
    END AS cbc_episode_assignment_rule,

    CASE
      WHEN cbc_n_candidate_episodes > 1
      THEN 'within_ed_interval > smallest_nonnegative_hours > latest_ed_intime > smallest_stay_id'
      ELSE 'not_required'
    END AS cbc_episode_assignment_tiebreak

  FROM ranked_episode_links
  WHERE cbc_episode_assignment_rank = 1
),

standardized_counts AS (
  SELECT
    *,

    platelet AS platelet_count,
    neutrophils_abs_final AS neutrophil_count,
    lymphocytes_abs_final AS lymphocyte_count,
    monocytes_abs_final AS monocyte_count,

    'mimiciv_derived_complete_blood_count' AS platelet_count_source,
    neutrophils_abs_source AS neutrophil_count_source,
    lymphocytes_abs_source AS lymphocyte_count_source,
    monocytes_abs_source AS monocyte_count_source,

    CONCAT('specimen_id:', specimen_id) AS cbc_source_row_id

  FROM assigned_episode_links
)

SELECT
  *,

  platelet_count IS NOT NULL
    AND isfinite(platelet_count)
    AND platelet_count >= 0 AS platelet_structurally_valid,

  neutrophil_count IS NOT NULL
    AND isfinite(neutrophil_count)
    AND neutrophil_count >= 0 AS neutrophil_structurally_valid,

  lymphocyte_count IS NOT NULL
    AND isfinite(lymphocyte_count)
    AND lymphocyte_count >= 0 AS lymphocyte_structurally_valid,

  monocyte_count IS NOT NULL
    AND isfinite(monocyte_count)
    AND monocyte_count >= 0 AS monocyte_structurally_valid,

  platelet_count IS NOT NULL
    AND isfinite(platelet_count)
    AND platelet_count >= 0
    AND neutrophil_count IS NOT NULL
    AND isfinite(neutrophil_count)
    AND neutrophil_count >= 0
    AND lymphocyte_count IS NOT NULL
    AND isfinite(lymphocyte_count)
    AND lymphocyte_count >= 0
    AND monocyte_count IS NOT NULL
    AND isfinite(monocyte_count)
    AND monocyte_count >= 0 AS cbc_complete_four_cell_flag

FROM standardized_counts;
"

DBI::dbExecute(con, cbc_assignment_query)

# ==============================================================================
# 15. CBC AVAILABILITY BY ED EPISODE
# ==============================================================================

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE cbc_episode_availability AS
  SELECT
    episode_id,
    COUNT(*) AS cbc_n_specimens_0_24h,
    COUNT(*) FILTER (WHERE cbc_complete_four_cell_flag) AS cbc_n_complete_specimens_0_24h,
    MIN(cbc_charttime) AS cbc_first_any_charttime_0_24h
  FROM cbc_assigned_0_24h
  GROUP BY episode_id;
  "
)


# ==============================================================================
# 16. SELECT THE FIRST COMPLETE FOUR-CELL CBC
# ==============================================================================

# The index CBC is the earliest specimen containing valid platelet,
# neutrophil, lymphocyte, and monocyte counts from the same specimen.
# Zero denominators remain missing; no pseudocount is applied.

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE cbc_index_episode AS

  WITH ranked_complete_cbc AS (
    SELECT
      *,
      ROW_NUMBER() OVER (
        PARTITION BY episode_id
        ORDER BY
          cbc_charttime,
          TRY_CAST(specimen_id AS BIGINT) NULLS LAST,
          specimen_id,
          cbc_source_row_id
      ) AS complete_cbc_rank
    FROM cbc_assigned_0_24h
    WHERE cbc_complete_four_cell_flag
  ),

  selected_cbc AS (
    SELECT
      episode_id,
      specimen_id AS cbc_index_specimen_id,
      cbc_charttime AS cbc_index_charttime,
      cbc_hours_from_ed AS cbc_index_hours_from_ed,

      platelet_count AS cbc_index_platelet_count,
      neutrophil_count AS cbc_index_neutrophil_count,
      lymphocyte_count AS cbc_index_lymphocyte_count,
      monocyte_count AS cbc_index_monocyte_count,

      platelet_count_source AS cbc_index_platelet_source,
      neutrophil_count_source AS cbc_index_neutrophil_source,
      lymphocyte_count_source AS cbc_index_lymphocyte_source,
      monocyte_count_source AS cbc_index_monocyte_source,

      hematocrit AS cbc_index_hematocrit,
      hemoglobin AS cbc_index_hemoglobin,
      mch AS cbc_index_mch,
      mchc AS cbc_index_mchc,
      mcv AS cbc_index_mcv,
      rbc AS cbc_index_rbc,
      rdw AS cbc_index_rdw,
      rdwsd AS cbc_index_rdwsd,

      wbc_cbc AS cbc_index_wbc_cbc,
      wbc_diff AS cbc_index_wbc_diff,
      wbc_for_calculation AS cbc_index_wbc_for_calculation,
      wbc_absolute_difference AS cbc_index_wbc_absolute_difference,
      wbc_relative_difference AS cbc_index_wbc_relative_difference,
      wbc_discordance_flag AS cbc_index_wbc_discordance_flag,

      basophils_abs AS cbc_index_basophils_abs,
      eosinophils_abs AS cbc_index_eosinophils_abs,
      basophils AS cbc_index_basophils_percent,
      eosinophils AS cbc_index_eosinophils_percent,
      lymphocytes AS cbc_index_lymphocytes_percent,
      monocytes AS cbc_index_monocytes_percent,
      neutrophils AS cbc_index_neutrophils_percent,
      atypical_lymphocytes AS cbc_index_atypical_lymphocytes_percent,
      bands AS cbc_index_bands_percent,
      immature_granulocytes AS cbc_index_immature_granulocytes_percent,
      metamyelocytes AS cbc_index_metamyelocytes_percent,
      nrbc AS cbc_index_nrbc,

      blood_differential_available_flag AS cbc_index_blood_differential_available_flag,
      cbc_diff_subject_mismatch_flag AS cbc_index_cbc_diff_subject_mismatch_flag,
      cbc_diff_hadm_mismatch_flag AS cbc_index_cbc_diff_hadm_mismatch_flag,
      cbc_diff_time_difference_minutes AS cbc_index_cbc_diff_time_difference_minutes,
      cbc_diff_time_mismatch_flag AS cbc_index_cbc_diff_time_mismatch_flag,

      cbc_episode_assignment_rule AS cbc_index_episode_assignment_rule,
      cbc_episode_assignment_tiebreak AS cbc_index_episode_assignment_tiebreak,
      cbc_assignment_ambiguous_flag AS cbc_index_assignment_ambiguous_flag,
      cbc_n_candidate_episodes AS cbc_index_n_candidate_episodes,
      cbc_source_row_id AS cbc_index_source_row_id

    FROM ranked_complete_cbc
    WHERE complete_cbc_rank = 1
  ),

  calculated_ratios AS (
    SELECT
      *,
      cbc_index_lymphocyte_count = 0 AS lymphocyte_zero_flag,
      cbc_index_monocyte_count = 0 AS monocyte_zero_flag,

      CASE
        WHEN cbc_index_lymphocyte_count > 0
        THEN cbc_index_neutrophil_count / cbc_index_lymphocyte_count
      END AS nlr,

      CASE
        WHEN cbc_index_lymphocyte_count > 0
        THEN cbc_index_platelet_count / cbc_index_lymphocyte_count
      END AS plr,

      CASE
        WHEN cbc_index_lymphocyte_count > 0
        THEN cbc_index_monocyte_count / cbc_index_lymphocyte_count
      END AS mlr,

      CASE
        WHEN cbc_index_monocyte_count > 0
        THEN cbc_index_neutrophil_count / cbc_index_monocyte_count
      END AS nmr

    FROM selected_cbc
  ),

  ratio_availability AS (
    SELECT
      *,
      nlr IS NOT NULL AND isfinite(nlr) AS nlr_available_flag,
      plr IS NOT NULL AND isfinite(plr) AS plr_available_flag,
      mlr IS NOT NULL AND isfinite(mlr) AS mlr_available_flag,
      nmr IS NOT NULL AND isfinite(nmr) AS nmr_available_flag
    FROM calculated_ratios
  )

  SELECT
    *,
    nlr_available_flag
      AND plr_available_flag
      AND mlr_available_flag
      AND nmr_available_flag AS all_four_ratios_available_flag,

    lymphocyte_zero_flag
      OR monocyte_zero_flag
      OR NOT (
        nlr_available_flag
        AND plr_available_flag
        AND mlr_available_flag
        AND nmr_available_flag
      ) AS cbc_ratio_review_flag

  FROM ratio_availability;
  "
)


# ==============================================================================
# 17. CONSTRUCT THE CBC-ELIGIBLE SOURCE COHORT
# ==============================================================================

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE candidate_cohort_working_table AS
  SELECT
    episode.*,
    availability.cbc_n_specimens_0_24h,
    availability.cbc_n_complete_specimens_0_24h,
    availability.cbc_first_any_charttime_0_24h,
    index_cbc.* EXCLUDE (episode_id),
    FALSE AS exclusion_no_cbc_0_24h,
    FALSE AS exclusion_cbc_present_but_no_complete_four_cell_0_24h,
    TRUE AS candidate_eligible_current_flag

  FROM episode_spine_table AS episode

  INNER JOIN cbc_episode_availability AS availability
    USING (episode_id)

  INNER JOIN cbc_index_episode AS index_cbc
    USING (episode_id)

  WHERE availability.cbc_n_complete_specimens_0_24h > 0;
  "
)

candidate_cohort_working <- DBI::dbGetQuery(
  con,
  "
  SELECT *
  FROM candidate_cohort_working_table
  ORDER BY episode_id;
  "
) %>%
  tibble::as_tibble()

assert_unique_key(candidate_cohort_working, "episode_id", "candidate_cohort_working")

# ==============================================================================
# 18. CRP SOURCE FILES
# ==============================================================================

all_csv_files <- list.files(
  INPUT_DIR, pattern = "\\.csv$", recursive = TRUE,
  full.names = TRUE, ignore.case = TRUE
)

normalized_csv_paths <- gsub("\\\\", "/", all_csv_files)
csv_base_names <- tolower(basename(all_csv_files))

crp_source_files <- all_csv_files[
  grepl("(^|/)labevents_study_raw(/|$)", normalized_csv_paths) |
    grepl("^labevents_study_raw.*\\.csv$", csv_base_names) |
    grepl("^labevents_selected.*\\.csv$", csv_base_names)
]

crp_source_files <- sort(unique(crp_source_files))

if (length(crp_source_files) == 0L) {
  stop(
    paste0(
      "No selected labevents CSV files were found. Store the export in ",
      "'labevents_study_raw', or use a file name beginning with ",
      "'labevents_study_raw' or 'labevents_selected'."
    ),
    call. = FALSE
  )
}

crp_required_source_columns <- c(
  "labevent_id", "subject_id", "hadm_id", "specimen_id", "itemid", "label",
  "charttime", "storetime", "value", "valuenum", "valueuom", "flag"
)

for (current_file in crp_source_files) {
  current_schema <- DBI::dbGetQuery(
    con,
    paste0(
      "DESCRIBE SELECT * FROM read_csv_auto(",
      as.character(DBI::dbQuoteString(con, current_file)),
      ", header = TRUE, all_varchar = TRUE, sample_size = 20480, ",
      "ignore_errors = FALSE);"
    )
  )
  
  missing_current_columns <- setdiff(
    crp_required_source_columns,
    current_schema$column_name
  )
  
  if (length(missing_current_columns) > 0L) {
    stop(
      paste0(
        "The file ", basename(current_file),
        " is missing the following columns: ",
        paste(missing_current_columns, collapse = ", "), "."
      ),
      call. = FALSE
    )
  }
}

crp_file_list_sql <- paste0(
  "[",
  paste(
    vapply(
      crp_source_files,
      function(path) as.character(DBI::dbQuoteString(con, path)),
      character(1L)
    ),
    collapse = ", "
  ),
  "]"
)

# ==============================================================================
# 19. IMPORT SELECTED LABEVENTS AND ISOLATE CRP
# ==============================================================================

# BigQuery export query:
#
# SELECT
#   le.labevent_id, le.subject_id, le.hadm_id, le.specimen_id, le.itemid,
#   dli.label, dli.fluid, dli.category, le.charttime, le.storetime,
#   le.value, le.valuenum, le.valueuom, le.ref_range_lower,
#   le.ref_range_upper, le.flag, le.priority, le.comments
# FROM `physionet-data.mimiciv_v3_1_hosp.labevents` AS le
# INNER JOIN `physionet-data.mimiciv_v3_1_hosp.d_labitems` AS dli
#   ON le.itemid = dli.itemid
# WHERE le.itemid IN (
#   50816, 50821, 52033, 50885, 53089,
#   50889, 50912, 52546, 51265, 53189
# );

crp_import_query <- sprintf(
  "
  CREATE OR REPLACE TEMP TABLE crp_source_rows AS

  WITH imported_rows AS (
    SELECT
      CASE WHEN UPPER(TRIM(CAST(labevent_id AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL')
        THEN NULL ELSE TRIM(CAST(labevent_id AS VARCHAR)) END AS labevent_id,

      CASE WHEN UPPER(TRIM(CAST(subject_id AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL')
        THEN NULL ELSE TRIM(CAST(subject_id AS VARCHAR)) END AS subject_id,

      CASE WHEN UPPER(TRIM(CAST(hadm_id AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL')
        THEN NULL ELSE TRIM(CAST(hadm_id AS VARCHAR)) END AS hadm_id,

      CASE WHEN UPPER(TRIM(CAST(specimen_id AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL')
        THEN NULL ELSE TRIM(CAST(specimen_id AS VARCHAR)) END AS specimen_id,

      TRY_CAST(itemid AS BIGINT) AS itemid,
      CAST(label AS VARCHAR) AS crp_label,
      CAST(charttime AS VARCHAR) AS crp_charttime_text,
      CAST(storetime AS VARCHAR) AS crp_storetime_text,
      CAST(value AS VARCHAR) AS crp_value_text,
      CAST(valuenum AS VARCHAR) AS crp_valuenum_text,
      CAST(valueuom AS VARCHAR) AS crp_unit_text,
      CAST(flag AS VARCHAR) AS crp_original_flag,
      regexp_extract(filename, '[^/]+$') AS crp_source_file,
      DENSE_RANK() OVER (
        PARTITION BY filename
        ORDER BY
          TRY_CAST(labevent_id AS BIGINT) NULLS LAST,
          CAST(labevent_id AS VARCHAR) NULLS LAST,
      
          TRY_CAST(subject_id AS BIGINT) NULLS LAST,
          CAST(subject_id AS VARCHAR) NULLS LAST,
      
          TRY_CAST(hadm_id AS BIGINT) NULLS LAST,
          CAST(hadm_id AS VARCHAR) NULLS LAST,
      
          TRY_CAST(specimen_id AS BIGINT) NULLS LAST,
          CAST(specimen_id AS VARCHAR) NULLS LAST,
      
          TRY_CAST(itemid AS BIGINT) NULLS LAST,
          CAST(label AS VARCHAR) NULLS LAST,
          CAST(charttime AS VARCHAR) NULLS LAST,
          CAST(storetime AS VARCHAR) NULLS LAST,
          CAST(valuenum AS VARCHAR) NULLS LAST,
          CAST(value AS VARCHAR) NULLS LAST,
          CAST(valueuom AS VARCHAR) NULLS LAST,
          CAST(flag AS VARCHAR) NULLS LAST
      ) AS crp_source_row_in_file

    FROM read_csv_auto(
      %s,
      header = TRUE, all_varchar = TRUE, union_by_name = TRUE,
      filename = TRUE, sample_size = 20480, ignore_errors = FALSE
    )
  )

  SELECT
    labevent_id, subject_id, hadm_id, specimen_id, itemid, crp_label,
    crp_charttime_text, crp_storetime_text, crp_value_text, crp_valuenum_text,
    crp_unit_text, crp_original_flag, crp_source_file, crp_source_row_in_file
  FROM imported_rows
  WHERE itemid = 50889;
  ",
  crp_file_list_sql
)

DBI::dbExecute(con, crp_import_query)

# ==============================================================================
# 20. CRP DEDUPLICATION AND UNIT STANDARDIZATION
# ==============================================================================

crp_events_query <- "
CREATE OR REPLACE TEMP TABLE crp_events AS

WITH deduplicated_rows AS (
  SELECT
    labevent_id, subject_id, hadm_id, specimen_id, itemid, crp_label,
    TRY_CAST(crp_charttime_text AS TIMESTAMP) AS crp_charttime,
    TRY_CAST(crp_storetime_text AS TIMESTAMP) AS crp_storetime,
    TRY_CAST(crp_valuenum_text AS DOUBLE) AS crp_value_raw,
    crp_value_text,

    CASE
      WHEN UPPER(TRIM(crp_unit_text)) IN ('', 'NA', 'NAN', 'NULL') THEN NULL
      ELSE TRIM(crp_unit_text)
    END AS crp_unit_raw,

    crp_original_flag, crp_source_file, crp_source_row_in_file

  FROM crp_source_rows

  QUALIFY ROW_NUMBER() OVER (
    PARTITION BY labevent_id
    ORDER BY crp_source_file, crp_source_row_in_file
  ) = 1
),

cleaned_units AS (
  SELECT
    *,
    regexp_replace(LOWER(TRIM(crp_unit_raw)), '\\s+', '', 'g') AS crp_unit_clean
  FROM deduplicated_rows
),

standardized_values AS (
  SELECT
    *,

    CASE
      WHEN crp_unit_clean IN ('mg/l', 'mgperliter', 'mgperlitre') THEN 'mg/L'
      WHEN crp_unit_clean = 'mg/dl' THEN 'mg/dL'
      WHEN crp_unit_clean IS NULL OR crp_unit_clean = '' THEN NULL
      ELSE crp_unit_raw
    END AS crp_unit_standardized,

    CASE
      WHEN crp_unit_clean IN ('mg/l', 'mgperliter', 'mgperlitre') THEN 1.0
      WHEN crp_unit_clean = 'mg/dl' THEN 10.0
      ELSE NULL
    END AS crp_conversion_factor

  FROM cleaned_units
),

converted_values AS (
  SELECT
    *,
    crp_value_raw * crp_conversion_factor AS crp_value_mg_l
  FROM standardized_values
)

SELECT
  labevent_id, subject_id, hadm_id, specimen_id, itemid, crp_label,
  crp_charttime, crp_storetime, crp_value_raw, crp_value_text,
  crp_unit_raw, crp_unit_clean, crp_unit_standardized,
  crp_conversion_factor, crp_value_mg_l,

  crp_value_mg_l IS NOT NULL
    AND isfinite(crp_value_mg_l)
    AND crp_value_mg_l >= 0 AS crp_event_valid_flag,

  crp_value_raw IS NOT NULL
    AND isfinite(crp_value_raw)
    AND crp_conversion_factor IS NULL AS crp_unit_review_flag,

  crp_value_mg_l IS NOT NULL
    AND isfinite(crp_value_mg_l)
    AND crp_value_mg_l > 500 AS crp_extreme_value_review_flag,

  crp_original_flag, crp_source_file, crp_source_row_in_file,

  DENSE_RANK() OVER (
    ORDER BY
      TRY_CAST(labevent_id AS BIGINT) NULLS LAST,
      labevent_id NULLS LAST,
      subject_id NULLS LAST,
      hadm_id NULLS LAST,
      specimen_id NULLS LAST,
      crp_charttime NULLS LAST,
      crp_storetime NULLS LAST,
      crp_value_raw NULLS LAST,
      crp_value_text NULLS LAST,
      crp_unit_standardized NULLS LAST,
      crp_original_flag NULLS LAST,
      crp_source_file NULLS LAST,
      crp_source_row_in_file NULLS LAST
  ) AS crp_raw_row_id

FROM converted_values;
"

DBI::dbExecute(con, crp_events_query)

# ==============================================================================
# 21. COHORT ANCHORS FOR CRP ASSIGNMENT
# ==============================================================================

candidate_table_columns <- DBI::dbListFields(con, "candidate_cohort_working_table")
existing_crp_columns <- grep("^crp_", candidate_table_columns, value = TRUE)

if (length(existing_crp_columns) == 0L) {
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE candidate_cohort_before_crp_table AS
    SELECT *
    FROM candidate_cohort_working_table;
    "
  )
} else {
  crp_exclusion_sql <- paste(
    vapply(
      existing_crp_columns,
      function(column_name) {
        as.character(DBI::dbQuoteIdentifier(con, column_name))
      },
      character(1L)
    ),
    collapse = ", "
  )
  
  DBI::dbExecute(
    con,
    sprintf(
      "
      CREATE OR REPLACE TEMP TABLE candidate_cohort_before_crp_table AS
      SELECT * EXCLUDE (%s)
      FROM candidate_cohort_working_table;
      ",
      crp_exclusion_sql
    )
  )
}


# ==============================================================================
# 22. UNIQUE CRP EVENT-TO-EPISODE ASSIGNMENT
# ==============================================================================

# Primary assignment uses subject_id + hadm_id.
# Events must occur from ED arrival through ED arrival +24 hours.
# Multiple candidate episodes are resolved by unique proximity to the index CBC.

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE crp_hadm_candidates AS
  SELECT
    e.labevent_id,
    c.episode_id,
    date_diff('second', c.cbc_index_charttime, e.crp_charttime) / 3600.0
      AS crp_time_from_cbc_hours,
    ABS(date_diff('second', c.cbc_index_charttime, e.crp_charttime) / 3600.0)
      AS crp_abs_time_from_cbc_hours
  FROM crp_events AS e
  INNER JOIN candidate_cohort_before_crp_table AS c
    ON e.subject_id = c.subject_id
   AND e.hadm_id = c.hadm_id
  WHERE e.crp_charttime IS NOT NULL
    AND e.crp_charttime BETWEEN c.ed_intime
                            AND c.ed_intime + INTERVAL '24 hours';
  "
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE crp_hadm_resolution AS

  WITH candidate_distances AS (
    SELECT
      *,
      COUNT(*) OVER (PARTITION BY labevent_id) AS assignment_candidate_n,
      MIN(crp_abs_time_from_cbc_hours) OVER (PARTITION BY labevent_id)
        AS nearest_distance
    FROM crp_hadm_candidates
  )

  SELECT
    *,
    COUNT(*) FILTER (
      WHERE ABS(crp_abs_time_from_cbc_hours - nearest_distance) < 1e-9
    ) OVER (PARTITION BY labevent_id) AS assignment_nearest_tie_n
  FROM candidate_distances;
  "
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE crp_hadm_assigned AS
  SELECT
    labevent_id, episode_id,
    crp_time_from_cbc_hours, crp_abs_time_from_cbc_hours,
    assignment_candidate_n, assignment_nearest_tie_n,
    'subject_id_hadm_id_primary_window' AS assignment_method,
    FALSE AS assignment_review_flag
  FROM crp_hadm_resolution
  WHERE ABS(crp_abs_time_from_cbc_hours - nearest_distance) < 1e-9
    AND assignment_nearest_tie_n = 1;
  "
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE crp_hadm_ambiguous AS
  SELECT DISTINCT labevent_id
  FROM crp_hadm_resolution
  WHERE assignment_nearest_tie_n > 1;
  "
)


# Time-only fallback is restricted to events not assigned or ambiguous above,
# and to cases where the event or candidate episode lacks hadm_id.

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE crp_time_candidates AS
  SELECT
    e.labevent_id,
    c.episode_id,
    date_diff('second', c.cbc_index_charttime, e.crp_charttime) / 3600.0
      AS crp_time_from_cbc_hours,
    ABS(date_diff('second', c.cbc_index_charttime, e.crp_charttime) / 3600.0)
      AS crp_abs_time_from_cbc_hours
  FROM crp_events AS e
  INNER JOIN candidate_cohort_before_crp_table AS c
    ON e.subject_id = c.subject_id
  LEFT JOIN crp_hadm_assigned AS assigned
    ON e.labevent_id = assigned.labevent_id
  LEFT JOIN crp_hadm_ambiguous AS ambiguous
    ON e.labevent_id = ambiguous.labevent_id
  WHERE assigned.labevent_id IS NULL
    AND ambiguous.labevent_id IS NULL
    AND e.crp_charttime IS NOT NULL
    AND e.crp_charttime BETWEEN c.ed_intime
                            AND c.ed_intime + INTERVAL '24 hours'
    AND (e.hadm_id IS NULL OR c.hadm_id IS NULL);
  "
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE crp_time_resolution AS

  WITH candidate_distances AS (
    SELECT
      *,
      COUNT(*) OVER (PARTITION BY labevent_id) AS assignment_candidate_n,
      MIN(crp_abs_time_from_cbc_hours) OVER (PARTITION BY labevent_id)
        AS nearest_distance
    FROM crp_time_candidates
  )

  SELECT
    *,
    COUNT(*) FILTER (
      WHERE ABS(crp_abs_time_from_cbc_hours - nearest_distance) < 1e-9
    ) OVER (PARTITION BY labevent_id) AS assignment_nearest_tie_n
  FROM candidate_distances;
  "
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE crp_time_assigned AS
  SELECT
    labevent_id, episode_id,
    crp_time_from_cbc_hours, crp_abs_time_from_cbc_hours,
    assignment_candidate_n, assignment_nearest_tie_n,
    'subject_id_time_primary_window' AS assignment_method,
    TRUE AS assignment_review_flag
  FROM crp_time_resolution
  WHERE ABS(crp_abs_time_from_cbc_hours - nearest_distance) < 1e-9
    AND assignment_nearest_tie_n = 1;
  "
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE crp_time_ambiguous AS
  SELECT DISTINCT labevent_id
  FROM crp_time_resolution
  WHERE assignment_nearest_tie_n > 1;
  "
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE crp_assignment_table AS
  SELECT
    labevent_id, episode_id,
    crp_time_from_cbc_hours, crp_abs_time_from_cbc_hours,
    assignment_candidate_n, assignment_nearest_tie_n,
    assignment_method, assignment_review_flag
  FROM crp_hadm_assigned

  UNION ALL

  SELECT
    labevent_id, episode_id,
    crp_time_from_cbc_hours, crp_abs_time_from_cbc_hours,
    assignment_candidate_n, assignment_nearest_tie_n,
    assignment_method, assignment_review_flag
  FROM crp_time_assigned;
  "
)

# ==============================================================================
# CLINICAL EVIDENCE SOURCES FOR EXCLUSION SCREENING
# ==============================================================================

# This block exports factual evidence only.
# No exclusion criterion or clinical adjudication is applied here.


# ------------------------------------------------------------------------------
# ED diagnoses
# ------------------------------------------------------------------------------

# BigQuery export query:
#
# SELECT *
# FROM `physionet-data.mimiciv_ed.diagnosis`;

ed_diagnosis_file <- input_path("diagnosis.csv")
ed_diagnosis_file_sql <- quote_sql_path(con, ed_diagnosis_file)

DBI::dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP VIEW ed_diagnosis_source AS
    SELECT
      NULLIF(TRIM(CAST(subject_id AS VARCHAR)), '') AS subject_id,
      NULLIF(TRIM(CAST(stay_id AS VARCHAR)), '') AS stay_id,
      TRY_CAST(seq_num AS INTEGER) AS diagnosis_sequence,
      NULLIF(TRIM(CAST(icd_code AS VARCHAR)), '') AS diagnosis_icd_code_raw,
      TRY_CAST(icd_version AS INTEGER) AS diagnosis_icd_version,
      NULLIF(TRIM(CAST(icd_title AS VARCHAR)), '') AS diagnosis_icd_title_raw
    FROM read_csv_auto(
      %s, header = TRUE, all_varchar = TRUE,
      sample_size = -1, ignore_errors = FALSE
    );
    ",
    ed_diagnosis_file_sql
  )
)


# ------------------------------------------------------------------------------
# Medication reconciliation
# ------------------------------------------------------------------------------

# BigQuery export query:
#
# SELECT *
# FROM `physionet-data.mimiciv_ed.medrecon`;

medrecon_file <- input_path("medrecon.csv")
medrecon_file_sql <- quote_sql_path(con, medrecon_file)

DBI::dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP VIEW medrecon_source AS
    SELECT
      NULLIF(TRIM(CAST(subject_id AS VARCHAR)), '') AS subject_id,
      NULLIF(TRIM(CAST(stay_id AS VARCHAR)), '') AS stay_id,
      TRY_CAST(charttime AS TIMESTAMP) AS medrecon_charttime,
      NULLIF(TRIM(CAST(name AS VARCHAR)), '') AS medrecon_name_raw,
      NULLIF(TRIM(CAST(gsn AS VARCHAR)), '') AS medrecon_gsn,
      NULLIF(TRIM(CAST(ndc AS VARCHAR)), '') AS medrecon_ndc,
      TRY_CAST(etc_rn AS INTEGER) AS medrecon_etc_rank,
      NULLIF(TRIM(CAST(etccode AS VARCHAR)), '') AS medrecon_etc_code,
      NULLIF(TRIM(CAST(etcdescription AS VARCHAR)), '')
        AS medrecon_etc_description_raw
    FROM read_csv_auto(
      %s, header = TRUE, all_varchar = TRUE,
      sample_size = -1, ignore_errors = FALSE
    );
    ",
    medrecon_file_sql
  )
)


# ------------------------------------------------------------------------------
# Hospital diagnoses
# ------------------------------------------------------------------------------

# BigQuery export query:
#
# SELECT *
# FROM `physionet-data.mimiciv_v3_1_hosp.diagnoses_icd`;

hospital_diagnosis_file <- input_path("diagnoses_icd.csv")
hospital_diagnosis_file_sql <- quote_sql_path(con, hospital_diagnosis_file)

DBI::dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP VIEW hospital_diagnosis_source AS
    SELECT
      NULLIF(TRIM(CAST(subject_id AS VARCHAR)), '') AS subject_id,
      NULLIF(TRIM(CAST(hadm_id AS VARCHAR)), '') AS hadm_id,
      TRY_CAST(seq_num AS INTEGER) AS diagnosis_sequence,
      NULLIF(TRIM(CAST(icd_code AS VARCHAR)), '') AS diagnosis_icd_code_raw,
      TRY_CAST(icd_version AS INTEGER) AS diagnosis_icd_version
    FROM read_csv_auto(
      %s, header = TRUE, all_varchar = TRUE,
      sample_size = -1, ignore_errors = FALSE
    );
    ",
    hospital_diagnosis_file_sql
  )
)


# BigQuery export query:
#
# SELECT *
# FROM `physionet-data.mimiciv_v3_1_hosp.d_icd_diagnoses`;

diagnosis_dictionary_file <- input_path("d_icd_diagnoses.csv")
diagnosis_dictionary_file_sql <- quote_sql_path(con, diagnosis_dictionary_file)

DBI::dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP VIEW diagnosis_dictionary_source AS
    SELECT
      NULLIF(TRIM(CAST(icd_code AS VARCHAR)), '') AS diagnosis_icd_code_raw,
      TRY_CAST(icd_version AS INTEGER) AS diagnosis_icd_version,
      NULLIF(TRIM(CAST(long_title AS VARCHAR)), '') AS diagnosis_icd_title_raw
    FROM read_csv_auto(
      %s, header = TRUE, all_varchar = TRUE,
      sample_size = -1, ignore_errors = FALSE
    );
    ",
    diagnosis_dictionary_file_sql
  )
)


# ------------------------------------------------------------------------------
# Charlson comorbidity table
# ------------------------------------------------------------------------------

# BigQuery export query:
#
# SELECT *
# FROM `physionet-data.mimiciv_3_1_derived.charlson`;

charlson_file <- input_path("derived_charlson.csv")
charlson_file_sql <- quote_sql_path(con, charlson_file)

DBI::dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP VIEW charlson_source AS
    SELECT *
    FROM read_csv_auto(
      %s, header = TRUE, all_varchar = TRUE,
      sample_size = -1, ignore_errors = FALSE
    );
    ",
    charlson_file_sql
  )
)


# ==============================================================================
# EPISODE KEYS FOR CLINICAL EVIDENCE
# ==============================================================================

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE clinical_evidence_episode_keys AS
  SELECT
    episode_id, subject_id, hadm_id, stay_id, ed_intime, ed_outtime
  FROM candidate_cohort_working_table;
  "
)


# ==============================================================================
# CURRENT-EPISODE ED DIAGNOSES
# ==============================================================================

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE episode_ed_diagnosis_long AS

  SELECT
    episode.episode_id, episode.subject_id, episode.stay_id,
    diagnosis.diagnosis_sequence,
    diagnosis.diagnosis_icd_version,
    diagnosis.diagnosis_icd_code_raw,

    UPPER(
      regexp_replace(
        diagnosis.diagnosis_icd_code_raw,
        '[^A-Za-z0-9]',
        '',
        'g'
      )
    ) AS diagnosis_icd_code_normalized,

    diagnosis.diagnosis_icd_title_raw,

    LOWER(
      TRIM(
        regexp_replace(
          diagnosis.diagnosis_icd_title_raw,
          '[^A-Za-z0-9]+',
          ' ',
          'g'
        )
      )
    ) AS diagnosis_icd_title_normalized

  FROM clinical_evidence_episode_keys AS episode
  INNER JOIN ed_diagnosis_source AS diagnosis
    ON episode.subject_id = diagnosis.subject_id
   AND episode.stay_id = diagnosis.stay_id;
  "
)


# ==============================================================================
# MEDICATION RECONCILIATION
# ==============================================================================

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE episode_medrecon_long AS

  SELECT
    episode.episode_id, episode.subject_id, episode.stay_id,
    medication.medrecon_charttime,

    date_diff(
      'second',
      episode.ed_intime,
      medication.medrecon_charttime
    ) / 3600.0 AS medrecon_hours_from_ed,

    medication.medrecon_name_raw,

    LOWER(
      TRIM(
        regexp_replace(
          medication.medrecon_name_raw,
          '[^A-Za-z0-9]+',
          ' ',
          'g'
        )
      )
    ) AS medrecon_name_normalized,

    medication.medrecon_gsn,
    medication.medrecon_ndc,
    medication.medrecon_etc_rank,
    medication.medrecon_etc_code,
    medication.medrecon_etc_description_raw,

    LOWER(
      TRIM(
        regexp_replace(
          medication.medrecon_etc_description_raw,
          '[^A-Za-z0-9]+',
          ' ',
          'g'
        )
      )
    ) AS medrecon_etc_description_normalized,

    medication.medrecon_charttime BETWEEN
      episode.ed_intime AND episode.ed_outtime
      AS medrecon_within_ed_stay_flag,

    medication.medrecon_charttime BETWEEN
      episode.ed_intime AND episode.ed_intime + INTERVAL '24 hours'
      AS medrecon_within_0_24h_flag

  FROM clinical_evidence_episode_keys AS episode
  INNER JOIN medrecon_source AS medication
    ON episode.subject_id = medication.subject_id
   AND episode.stay_id = medication.stay_id;
  "
)


# ==============================================================================
# PRIOR HOSPITAL DIAGNOSES
# ==============================================================================

# Only completed hospitalizations ending before ED arrival are treated as prior.
# Diagnoses from the current or future hospitalization are not included.

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE episode_prior_diagnosis_long AS

  WITH prior_diagnosis_rows AS (
    SELECT
      episode.episode_id,
      episode.subject_id,
      diagnosis.hadm_id AS prior_hadm_id,
      admission.dischtime AS prior_dischtime,
      diagnosis.diagnosis_sequence,
      diagnosis.diagnosis_icd_version,
      diagnosis.diagnosis_icd_code_raw,
      dictionary.diagnosis_icd_title_raw,

      date_diff(
        'second',
        admission.dischtime,
        episode.ed_intime
      ) / 86400.0 AS days_before_ed

    FROM clinical_evidence_episode_keys AS episode

    INNER JOIN admissions_selected AS admission
      ON episode.subject_id = admission.subject_id
     AND admission.dischtime IS NOT NULL
     AND admission.dischtime < episode.ed_intime
     AND (
       episode.hadm_id IS NULL
       OR admission.hadm_id <> episode.hadm_id
     )

    INNER JOIN hospital_diagnosis_source AS diagnosis
      ON admission.subject_id = diagnosis.subject_id
     AND admission.hadm_id = diagnosis.hadm_id

    LEFT JOIN diagnosis_dictionary_source AS dictionary
      ON diagnosis.diagnosis_icd_code_raw =
         dictionary.diagnosis_icd_code_raw
     AND diagnosis.diagnosis_icd_version =
         dictionary.diagnosis_icd_version
  ),

  normalized_rows AS (
    SELECT
      *,

      UPPER(
        regexp_replace(
          diagnosis_icd_code_raw,
          '[^A-Za-z0-9]',
          '',
          'g'
        )
      ) AS diagnosis_icd_code_normalized,

      LOWER(
        TRIM(
          regexp_replace(
            diagnosis_icd_title_raw,
            '[^A-Za-z0-9]+',
            ' ',
            'g'
          )
        )
      ) AS diagnosis_icd_title_normalized

    FROM prior_diagnosis_rows
  )

  SELECT
    episode_id, subject_id,
    diagnosis_icd_version,
    diagnosis_icd_code_raw,
    diagnosis_icd_code_normalized,
    diagnosis_icd_title_raw,
    diagnosis_icd_title_normalized,

    COUNT(*) AS prior_diagnosis_row_count,
    COUNT(DISTINCT prior_hadm_id) AS prior_admission_count,
    MIN(prior_dischtime) AS first_prior_dischtime,
    MAX(prior_dischtime) AS last_prior_dischtime,
    MIN(days_before_ed) AS minimum_days_before_ed,
    MAX(days_before_ed) AS maximum_days_before_ed

  FROM normalized_rows

  GROUP BY
    episode_id, subject_id,
    diagnosis_icd_version,
    diagnosis_icd_code_raw,
    diagnosis_icd_code_normalized,
    diagnosis_icd_title_raw,
    diagnosis_icd_title_normalized;
  "
)


# ==============================================================================
# CHARLSON EVIDENCE LINKED TO THE CURRENT HOSPITALIZATION
# ==============================================================================

# Charlson is exported as supporting evidence. It should not be treated as
# proof that every component was known before ED arrival.

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE episode_charlson AS

  SELECT
    episode.episode_id,
    episode.subject_id,
    episode.hadm_id,
    charlson.* EXCLUDE (subject_id, hadm_id)

  FROM clinical_evidence_episode_keys AS episode

  INNER JOIN charlson_source AS charlson
    ON episode.subject_id =
       NULLIF(TRIM(CAST(charlson.subject_id AS VARCHAR)), '')
   AND episode.hadm_id =
       NULLIF(TRIM(CAST(charlson.hadm_id AS VARCHAR)), '');
  "
)

# ==============================================================================
# COMPACT DIAGNOSIS AND MEDICATION EVIDENCE
# ==============================================================================

# The wide episode-level table is descriptive only.
# Clinical exclusion rules must use the complete deduplicated tables below.

WIDE_ITEM_LIMIT <- 20L


make_wide_columns <- function(
    value_column,
    rank_column,
    prefix,
    n = WIDE_ITEM_LIMIT
) {
  paste(
    sprintf(
      "MAX(%s) FILTER (WHERE %s = %d) AS %s_%02d",
      value_column,
      rank_column,
      seq_len(n),
      prefix,
      seq_len(n)
    ),
    collapse = ",\n    "
  )
}


# ==============================================================================
# DISTINCT CURRENT-EPISODE ED DIAGNOSES
# ==============================================================================

# Study-defined deduplication:
# episode_id + ICD version + normalized ICD code.
#
# diagnosis_sequence is used for ordering, not as proof of clinical priority.

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE episode_ed_diagnosis_distinct AS

  WITH ranked_rows AS (
    SELECT
      *,
      ROW_NUMBER() OVER (
        PARTITION BY
          episode_id,
          diagnosis_icd_version,
          diagnosis_icd_code_normalized
        ORDER BY
          diagnosis_sequence NULLS LAST,
          diagnosis_icd_code_raw NULLS LAST,
          diagnosis_icd_title_raw NULLS LAST
      ) AS duplicate_code_rank

    FROM episode_ed_diagnosis_long

    WHERE NULLIF(diagnosis_icd_code_normalized, '') IS NOT NULL
  )

  SELECT
    episode_id,
    subject_id,
    stay_id,
    diagnosis_sequence,
    diagnosis_icd_version,
    diagnosis_icd_code_raw,
    diagnosis_icd_code_normalized,
    diagnosis_icd_title_raw,
    diagnosis_icd_title_normalized,

    concat_ws(
      ' | ',
      concat(
        'ICD',
        coalesce(CAST(diagnosis_icd_version AS VARCHAR), '?'),
        ':',
        diagnosis_icd_code_normalized
      ),
      NULLIF(diagnosis_icd_title_raw, '')
    ) AS ed_diagnosis_display

  FROM ranked_rows

  WHERE duplicate_code_rank = 1;
  "
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE episode_ed_diagnosis_compact AS

  WITH source_counts AS (
    SELECT
      episode_id,
      COUNT(*) AS ed_diagnosis_source_row_count

    FROM episode_ed_diagnosis_long

    GROUP BY episode_id
  ),

  distinct_summary AS (
    SELECT
      episode_id,
      COUNT(*) AS ed_diagnosis_distinct_icd_count,

      string_agg(
        ed_diagnosis_display,
        ' || '
        ORDER BY
          diagnosis_sequence NULLS LAST,
          diagnosis_icd_version NULLS LAST,
          diagnosis_icd_code_normalized
      ) AS ed_diagnoses_compact

    FROM episode_ed_diagnosis_distinct

    GROUP BY episode_id
  )

  SELECT
    episode.episode_id,

    coalesce(source.ed_diagnosis_source_row_count, 0)
      AS ed_diagnosis_source_row_count,

    coalesce(summary.ed_diagnosis_distinct_icd_count, 0)
      AS ed_diagnosis_distinct_icd_count,

    summary.ed_diagnoses_compact,

    coalesce(summary.ed_diagnosis_distinct_icd_count, 0) > 0
      AS ed_diagnosis_available_flag

  FROM clinical_evidence_episode_keys AS episode

  LEFT JOIN source_counts AS source
    USING (episode_id)

  LEFT JOIN distinct_summary AS summary
    USING (episode_id);
  "
)


# ==============================================================================
# DISTINCT AND RANKED MEDICATION RECONCILIATION
# ==============================================================================

# Study-defined compact medication unit:
# episode_id + normalized medication name.
#
# This does not assert that these are pharmacologically unique medications.
# GSN, NDC and ETC information remains in episode_medrecon_long and in the
# overflow export.

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE episode_medrecon_ranked AS

  WITH candidate_rows AS (
    SELECT
      *,

      ROW_NUMBER() OVER (
        PARTITION BY
          episode_id,
          medrecon_name_normalized
        ORDER BY
          medrecon_charttime NULLS LAST,
          medrecon_name_raw NULLS LAST,
          medrecon_gsn NULLS LAST,
          medrecon_ndc NULLS LAST,
          medrecon_etc_rank NULLS LAST,
          medrecon_etc_code NULLS LAST
      ) AS representative_row_rank,

      COUNT(*) OVER (
        PARTITION BY
          episode_id,
          medrecon_name_normalized
      ) AS medrecon_source_rows_for_name

    FROM episode_medrecon_long

    WHERE NULLIF(medrecon_name_normalized, '') IS NOT NULL
  ),

  representative_rows AS (
    SELECT
      episode_id,
      subject_id,
      stay_id,
      medrecon_charttime,
      medrecon_hours_from_ed,
      medrecon_name_raw,
      medrecon_name_normalized,
      medrecon_gsn,
      medrecon_ndc,
      medrecon_etc_rank,
      medrecon_etc_code,
      medrecon_etc_description_raw,
      medrecon_etc_description_normalized,
      medrecon_within_ed_stay_flag,
      medrecon_within_0_24h_flag,
      medrecon_source_rows_for_name

    FROM candidate_rows

    WHERE representative_row_rank = 1
  )

  SELECT
    *,

    coalesce(
      NULLIF(medrecon_name_raw, ''),
      medrecon_name_normalized
    ) AS medrecon_display,

    ROW_NUMBER() OVER (
      PARTITION BY episode_id
      ORDER BY
        medrecon_charttime NULLS LAST,
        medrecon_name_normalized
    ) AS medrecon_rank,

    COUNT(*) OVER (
      PARTITION BY episode_id
    ) AS medrecon_distinct_name_count

  FROM representative_rows;
  "
)


medrecon_wide_columns <- make_wide_columns(
  value_column = "medrecon_display",
  rank_column = "medrecon_rank",
  prefix = "medrecon"
)


DBI::dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE episode_medrecon_compact AS

    WITH source_counts AS (
      SELECT
        episode_id,
        COUNT(*) AS medrecon_source_row_count,

        COUNT(*) FILTER (
          WHERE NULLIF(medrecon_name_normalized, '') IS NULL
        ) AS medrecon_missing_name_row_count

      FROM episode_medrecon_long

      GROUP BY episode_id
    ),

    wide_items AS (
      SELECT
        episode_id,
        COUNT(*) AS medrecon_distinct_name_count,

        greatest(
          COUNT(*) - %d,
          0
        ) AS medrecon_overflow_count,

        COUNT(*) > %d AS medrecon_over_20_flag,

        %s

      FROM episode_medrecon_ranked

      GROUP BY episode_id
    )

    SELECT
      episode.episode_id,

      coalesce(source.medrecon_source_row_count, 0)
        AS medrecon_source_row_count,

      coalesce(source.medrecon_missing_name_row_count, 0)
        AS medrecon_missing_name_row_count,

      coalesce(items.medrecon_distinct_name_count, 0)
        AS medrecon_distinct_name_count,

      coalesce(items.medrecon_overflow_count, 0)
        AS medrecon_overflow_count,

      coalesce(items.medrecon_over_20_flag, FALSE)
        AS medrecon_over_20_flag,

      coalesce(items.medrecon_distinct_name_count, 0) > 0
        AS medrecon_available_flag,

      items.* EXCLUDE (
        episode_id,
        medrecon_distinct_name_count,
        medrecon_overflow_count,
        medrecon_over_20_flag
      )

    FROM clinical_evidence_episode_keys AS episode

    LEFT JOIN source_counts AS source
      USING (episode_id)

    LEFT JOIN wide_items AS items
      USING (episode_id);
    ",
    WIDE_ITEM_LIMIT,
    WIDE_ITEM_LIMIT,
    medrecon_wide_columns
  )
)


# Preserve all source rows, including multiple ETC rows, for medications whose
# distinct normalized-name rank is greater than 20.

DBI::dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE episode_medrecon_overflow_long AS

    SELECT
      ranked.episode_id,
      ranked.medrecon_rank,
      ranked.medrecon_distinct_name_count,
      ranked.medrecon_source_rows_for_name,

      source.* EXCLUDE (episode_id)

    FROM episode_medrecon_ranked AS ranked

    INNER JOIN episode_medrecon_long AS source
      ON ranked.episode_id = source.episode_id
     AND ranked.medrecon_name_normalized =
         source.medrecon_name_normalized

    WHERE ranked.medrecon_rank > %d;
    ",
    WIDE_ITEM_LIMIT
  )
)


# ==============================================================================
# DISTINCT AND RANKED PRIOR HOSPITAL DIAGNOSES
# ==============================================================================

# One compact item is defined by:
# episode_id + ICD version + normalized ICD code.
#
# Codes are ranked by the most recent prior discharge.

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE episode_prior_diagnosis_ranked AS

  WITH candidate_rows AS (
    SELECT
      *,

      ROW_NUMBER() OVER (
        PARTITION BY
          episode_id,
          diagnosis_icd_version,
          diagnosis_icd_code_normalized
        ORDER BY
          last_prior_dischtime DESC NULLS LAST,
          minimum_days_before_ed ASC NULLS LAST,
          diagnosis_icd_code_raw NULLS LAST,
          diagnosis_icd_title_raw NULLS LAST
      ) AS representative_code_rank

    FROM episode_prior_diagnosis_long

    WHERE NULLIF(diagnosis_icd_code_normalized, '') IS NOT NULL
  ),

  representative_rows AS (
    SELECT *

    FROM candidate_rows

    WHERE representative_code_rank = 1
  )

  SELECT
    * EXCLUDE (representative_code_rank),

    concat_ws(
      ' | ',
      concat(
        'ICD',
        coalesce(CAST(diagnosis_icd_version AS VARCHAR), '?'),
        ':',
        diagnosis_icd_code_normalized
      ),
      NULLIF(diagnosis_icd_title_raw, '')
    ) AS prior_diagnosis_display,

    ROW_NUMBER() OVER (
      PARTITION BY episode_id
      ORDER BY
        last_prior_dischtime DESC NULLS LAST,
        minimum_days_before_ed ASC NULLS LAST,
        diagnosis_icd_version NULLS LAST,
        diagnosis_icd_code_normalized
    ) AS prior_diagnosis_rank,

    COUNT(*) OVER (
      PARTITION BY episode_id
    ) AS prior_diagnosis_distinct_icd_count

  FROM representative_rows;
  "
)


prior_diagnosis_wide_columns <- make_wide_columns(
  value_column = "prior_diagnosis_display",
  rank_column = "prior_diagnosis_rank",
  prefix = "prior_diagnosis"
)


DBI::dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE episode_prior_diagnosis_compact AS

    WITH source_counts AS (
      SELECT
        episode_id,

        SUM(prior_diagnosis_row_count)
          AS prior_diagnosis_source_row_count,

        COUNT(*)
          AS prior_diagnosis_grouped_row_count

      FROM episode_prior_diagnosis_long

      GROUP BY episode_id
    ),

    wide_items AS (
      SELECT
        episode_id,
        COUNT(*) AS prior_diagnosis_distinct_icd_count,

        greatest(
          COUNT(*) - %d,
          0
        ) AS prior_diagnosis_overflow_count,

        COUNT(*) > %d AS prior_diagnosis_over_20_flag,

        %s

      FROM episode_prior_diagnosis_ranked

      GROUP BY episode_id
    )

    SELECT
      episode.episode_id,

      coalesce(source.prior_diagnosis_source_row_count, 0)
        AS prior_diagnosis_source_row_count,

      coalesce(source.prior_diagnosis_grouped_row_count, 0)
        AS prior_diagnosis_grouped_row_count,

      coalesce(items.prior_diagnosis_distinct_icd_count, 0)
        AS prior_diagnosis_distinct_icd_count,

      coalesce(items.prior_diagnosis_overflow_count, 0)
        AS prior_diagnosis_overflow_count,

      coalesce(items.prior_diagnosis_over_20_flag, FALSE)
        AS prior_diagnosis_over_20_flag,

      coalesce(items.prior_diagnosis_distinct_icd_count, 0) > 0
        AS prior_diagnosis_available_flag,

      items.* EXCLUDE (
        episode_id,
        prior_diagnosis_distinct_icd_count,
        prior_diagnosis_overflow_count,
        prior_diagnosis_over_20_flag
      )

    FROM clinical_evidence_episode_keys AS episode

    LEFT JOIN source_counts AS source
      USING (episode_id)

    LEFT JOIN wide_items AS items
      USING (episode_id);
    ",
    WIDE_ITEM_LIMIT,
    WIDE_ITEM_LIMIT,
    prior_diagnosis_wide_columns
  )
)


DBI::dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE episode_prior_diagnosis_overflow_long AS

    SELECT
      ranked.episode_id,
      ranked.prior_diagnosis_rank,
      ranked.prior_diagnosis_distinct_icd_count,

      source.* EXCLUDE (episode_id)

    FROM episode_prior_diagnosis_ranked AS ranked

    INNER JOIN episode_prior_diagnosis_long AS source
      ON ranked.episode_id = source.episode_id
     AND ranked.diagnosis_icd_version
         IS NOT DISTINCT FROM source.diagnosis_icd_version
     AND ranked.diagnosis_icd_code_normalized =
         source.diagnosis_icd_code_normalized

    WHERE ranked.prior_diagnosis_rank > %d;
    ",
    WIDE_ITEM_LIMIT
  )
)


# ==============================================================================
# CHARLSON KEY VALIDATION
# ==============================================================================

charlson_duplicate_episode_n <- DBI::dbGetQuery(
  con,
  "
  SELECT COUNT(*)
  FROM (
    SELECT episode_id
    FROM episode_charlson
    GROUP BY episode_id
    HAVING COUNT(*) > 1
  );
  "
)[[1L]]


if (charlson_duplicate_episode_n > 0L) {
  stop(
    paste0(
      "episode_charlson contains ",
      charlson_duplicate_episode_n,
      " duplicated episode_id values."
    ),
    call. = FALSE
  )
}


# ==============================================================================
# EPISODE-LEVEL TABLE WITH COMPACT CLINICAL EVIDENCE
# ==============================================================================

# This table leaves candidate_cohort_working_table unchanged.
# The CRP block can subsequently use this enriched table as its episode spine.

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE candidate_cohort_clinical_evidence_table AS

  SELECT
    cohort.*,

    ed.* EXCLUDE (episode_id),

    medication.* EXCLUDE (episode_id),

    prior_diagnosis.* EXCLUDE (episode_id),

    charlson.episode_id IS NOT NULL
      AS charlson_available_flag,

    charlson.* EXCLUDE (
      episode_id,
      subject_id,
      hadm_id
    )

  FROM candidate_cohort_working_table AS cohort

  LEFT JOIN episode_ed_diagnosis_compact AS ed
    USING (episode_id)

  LEFT JOIN episode_medrecon_compact AS medication
    USING (episode_id)

  LEFT JOIN episode_prior_diagnosis_compact AS prior_diagnosis
    USING (episode_id)

  LEFT JOIN episode_charlson AS charlson
    USING (episode_id);
  "
)


# ==============================================================================
# COMPACTION VALIDATION
# ==============================================================================

clinical_evidence_compaction_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
    ) AS source_episode_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_clinical_evidence_table
    ) AS output_episode_n,

    (
      SELECT COUNT(*) - COUNT(DISTINCT episode_id)
      FROM candidate_cohort_clinical_evidence_table
    ) AS duplicated_output_episode_n,

    (
      SELECT COUNT(*)
      FROM episode_medrecon_ranked
      WHERE medrecon_rank > 20
    ) AS medrecon_expected_overflow_item_n,

    (
      SELECT coalesce(SUM(medrecon_overflow_count), 0)
      FROM episode_medrecon_compact
    ) AS medrecon_reported_overflow_item_n,

    (
      SELECT COUNT(*)
      FROM episode_prior_diagnosis_ranked
      WHERE prior_diagnosis_rank > 20
    ) AS prior_diagnosis_expected_overflow_item_n,

    (
      SELECT coalesce(SUM(prior_diagnosis_overflow_count), 0)
      FROM episode_prior_diagnosis_compact
    ) AS prior_diagnosis_reported_overflow_item_n;
  "
) %>%
  tibble::as_tibble()


if (
  clinical_evidence_compaction_check$source_episode_n !=
  clinical_evidence_compaction_check$output_episode_n ||
  clinical_evidence_compaction_check$duplicated_output_episode_n != 0L ||
  clinical_evidence_compaction_check$medrecon_expected_overflow_item_n !=
  clinical_evidence_compaction_check$medrecon_reported_overflow_item_n ||
  clinical_evidence_compaction_check$prior_diagnosis_expected_overflow_item_n !=
  clinical_evidence_compaction_check$prior_diagnosis_reported_overflow_item_n
) {
  stop(
    "Clinical evidence compaction validation failed.",
    call. = FALSE
  )
}


candidate_cohort_clinical_evidence <- DBI::dbGetQuery(
  con,
  "
  SELECT *
  FROM candidate_cohort_clinical_evidence_table
  ORDER BY episode_id;
  "
) %>%
  tibble::as_tibble()


assert_unique_key(
  candidate_cohort_clinical_evidence,
  "episode_id",
  "candidate_cohort_clinical_evidence"
)


print(clinical_evidence_compaction_check)

# ==============================================================================
# 24. BUILD EPISODE-LEVEL CRP CANDIDATES
# ==============================================================================

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE crp_candidates_primary AS

  SELECT
    assignment.labevent_id,
    assignment.episode_id,
    assignment.crp_time_from_cbc_hours,
    assignment.crp_abs_time_from_cbc_hours,
    assignment.assignment_candidate_n,
    assignment.assignment_nearest_tie_n,
    assignment.assignment_method,
    assignment.assignment_review_flag,

    event.subject_id,
    event.hadm_id,
    event.specimen_id,
    event.crp_charttime,
    event.crp_storetime,
    event.crp_value_raw,
    event.crp_value_text,
    event.crp_unit_raw,
    event.crp_unit_standardized,
    event.crp_value_mg_l,
    event.crp_event_valid_flag,
    event.crp_unit_review_flag,
    event.crp_extreme_value_review_flag,
    event.crp_label,
    event.crp_original_flag,
    event.crp_source_file,
    event.crp_source_row_in_file,
    event.crp_raw_row_id,

    cohort.ed_intime,
    cohort.cbc_index_charttime,

    date_diff(
      'second',
      cohort.ed_intime,
      event.crp_charttime
    ) / 3600.0 AS crp_time_from_ed_hours,

    CASE
      WHEN assignment.crp_time_from_cbc_hours = 0 THEN 'same_time'
      WHEN assignment.crp_time_from_cbc_hours < 0 THEN 'before_cbc'
      ELSE 'after_cbc'
    END AS crp_time_relation,

    CASE
      WHEN assignment.crp_time_from_cbc_hours = 0 THEN 0
      WHEN assignment.crp_time_from_cbc_hours < 0 THEN 1
      ELSE 2
    END AS crp_time_relation_rank

  FROM crp_assignment_table AS assignment

  INNER JOIN crp_events AS event
    USING (labevent_id)

  INNER JOIN candidate_cohort_before_crp_table AS cohort
    USING (episode_id);
  "
)


crp_window_violation_count <- DBI::dbGetQuery(
  con,
  "
  SELECT COUNT(*)
  FROM crp_candidates_primary
  WHERE crp_charttime IS NULL
     OR crp_charttime < ed_intime
     OR crp_charttime > ed_intime + INTERVAL '24 hours';
  "
)[[1L]]


if (crp_window_violation_count > 0L) {
  stop(
    paste0(
      crp_window_violation_count,
      " assigned CRP events are outside the 0-24 hour window."
    ),
    call. = FALSE
  )
}


# ==============================================================================
# 25. SUMMARIZE CRP AVAILABILITY AND SELECT THE INDEX VALUE
# ==============================================================================

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE crp_episode_availability AS

  SELECT
    episode_id,

    COUNT(*) AS crp_n_events_primary_window,

    COUNT(*) FILTER (
      WHERE crp_event_valid_flag
    ) AS crp_n_valid_events_primary_window,

    COUNT(*) FILTER (
      WHERE crp_unit_review_flag
    ) AS crp_n_unknown_unit_events_primary_window,

    COUNT(*) FILTER (
      WHERE crp_extreme_value_review_flag
    ) AS crp_n_extreme_value_events_primary_window,

    COUNT(*) FILTER (
      WHERE assignment_method = 'subject_id_time_primary_window'
    ) AS crp_n_time_fallback_events_primary_window,

    COUNT(*) FILTER (
      WHERE assignment_review_flag
    ) AS crp_n_assignment_review_events_primary_window,

    bool_or(assignment_review_flag)
      AS crp_any_assignment_review_flag,

    bool_or(crp_unit_review_flag)
      AS crp_any_unit_review_flag,

    bool_or(crp_extreme_value_review_flag)
      AS crp_any_extreme_value_review_flag

  FROM crp_candidates_primary

  GROUP BY episode_id;
  "
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE crp_index_episode AS

  WITH valid_candidates AS (
    SELECT
      *,

      MIN(crp_abs_time_from_cbc_hours) OVER (
        PARTITION BY episode_id
      ) AS nearest_valid_distance

    FROM crp_candidates_primary

    WHERE crp_event_valid_flag
  ),

  candidates_with_ties AS (
    SELECT
      *,

      COUNT(*) FILTER (
        WHERE ABS(
          crp_abs_time_from_cbc_hours -
          nearest_valid_distance
        ) < 1e-9
      ) OVER (
        PARTITION BY episode_id
      ) AS crp_nearest_tie_n

    FROM valid_candidates
  ),

  ranked_candidates AS (
    SELECT
      *,

      ROW_NUMBER() OVER (
        PARTITION BY episode_id
        ORDER BY
          crp_abs_time_from_cbc_hours,
          crp_time_relation_rank,
          crp_charttime,
          labevent_id,
          crp_raw_row_id
      ) AS selection_rank

    FROM candidates_with_ties
  )

  SELECT
    episode_id,

    labevent_id
      AS crp_index_labevent_id,

    specimen_id
      AS crp_index_specimen_id,

    crp_charttime
      AS crp_index_charttime,

    crp_storetime
      AS crp_index_storetime,

    crp_value_raw
      AS crp_index_value_raw,

    crp_value_text
      AS crp_index_value_text,

    crp_unit_raw
      AS crp_index_unit_raw,

    crp_unit_standardized
      AS crp_index_unit_standardized,

    crp_value_mg_l
      AS crp_index_value_mg_l,

    crp_time_from_ed_hours
      AS crp_index_time_from_ed_hours,

    crp_time_from_cbc_hours
      AS crp_index_time_from_cbc_hours,

    crp_abs_time_from_cbc_hours
      AS crp_index_abs_time_from_cbc_hours,

    crp_time_relation
      AS crp_index_time_relation,

    assignment_candidate_n
      AS crp_index_assignment_candidate_n,

    assignment_nearest_tie_n
      AS crp_index_assignment_nearest_tie_n,

    assignment_method
      AS crp_index_assignment_method,

    assignment_review_flag
      AS crp_index_assignment_review_flag,

    crp_original_flag
      AS crp_index_original_flag,

    crp_extreme_value_review_flag
      AS crp_index_extreme_value_review_flag,

    crp_source_file
      AS crp_index_source_file,

    crp_source_row_in_file
      AS crp_index_source_row_in_file,

    crp_raw_row_id
      AS crp_index_raw_row_id,

    crp_nearest_tie_n

  FROM ranked_candidates

  WHERE selection_rank = 1;
  "
)


# ==============================================================================
# 26. INTEGRATE CRP INTO THE CLINICAL-EVIDENCE COHORT
# ==============================================================================

# candidate_cohort_clinical_evidence_table is already materialized, so the
# working table can be safely replaced without creating a circular dependency.

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE candidate_cohort_working_table AS

  SELECT
    cohort.*,

    coalesce(
      availability.crp_n_events_primary_window,
      0
    ) AS crp_n_events_primary_window,

    coalesce(
      availability.crp_n_valid_events_primary_window,
      0
    ) AS crp_n_valid_events_primary_window,

    coalesce(
      availability.crp_n_unknown_unit_events_primary_window,
      0
    ) AS crp_n_unknown_unit_events_primary_window,

    coalesce(
      availability.crp_n_extreme_value_events_primary_window,
      0
    ) AS crp_n_extreme_value_events_primary_window,

    coalesce(
      availability.crp_n_time_fallback_events_primary_window,
      0
    ) AS crp_n_time_fallback_events_primary_window,

    coalesce(
      availability.crp_n_assignment_review_events_primary_window,
      0
    ) AS crp_n_assignment_review_events_primary_window,

    coalesce(
      availability.crp_any_assignment_review_flag,
      FALSE
    ) AS crp_any_assignment_review_flag,

    coalesce(
      availability.crp_any_unit_review_flag,
      FALSE
    ) AS crp_any_unit_review_flag,

    coalesce(
      availability.crp_any_extreme_value_review_flag,
      FALSE
    ) AS crp_any_extreme_value_review_flag,

    selected.* EXCLUDE (episode_id),

    coalesce(
      selected.crp_index_value_mg_l IS NOT NULL
        AND isfinite(selected.crp_index_value_mg_l),
      FALSE
    ) AS crp_available_primary_window_flag,

    cohort.candidate_eligible_current_flag
      AND selected.crp_index_value_mg_l IS NULL
      AS crp_missing_primary_window_flag,

    cohort.candidate_eligible_current_flag
      AND coalesce(
        availability.crp_n_events_primary_window,
        0
      ) > 0
      AND coalesce(
        availability.crp_n_valid_events_primary_window,
        0
      ) = 0
      AS crp_present_but_no_valid_value_flag

  FROM candidate_cohort_clinical_evidence_table AS cohort

  LEFT JOIN crp_episode_availability AS availability
    USING (episode_id)

  LEFT JOIN crp_index_episode AS selected
    USING (episode_id);
  "
)


# ==============================================================================
# 27. CRP INTEGRITY AND COHORT VALIDATION
# ==============================================================================

crp_integrity_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    (
      SELECT COUNT(*)
      FROM candidate_cohort_clinical_evidence_table
    ) AS source_episode_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
    ) AS output_episode_n,

    (
      SELECT COUNT(*) - COUNT(DISTINCT episode_id)
      FROM candidate_cohort_working_table
    ) AS duplicated_output_episode_n,

    (
      SELECT COUNT(*)
      FROM crp_assignment_table
    ) AS assigned_crp_event_n,

    (
      SELECT COUNT(*) - COUNT(DISTINCT labevent_id)
      FROM crp_assignment_table
    ) AS duplicated_assigned_crp_event_n,

    (
      SELECT COUNT(*)
      FROM crp_candidates_primary
      WHERE crp_charttime IS NULL
         OR crp_charttime < ed_intime
         OR crp_charttime > ed_intime + INTERVAL '24 hours'
    ) AS crp_window_violation_n,

    (
      SELECT COUNT(*)
      FROM crp_index_episode
    ) AS episodes_with_selected_crp_n,

    (
      SELECT COUNT(*)
      FROM crp_index_episode
      WHERE crp_index_value_mg_l IS NULL
         OR NOT isfinite(crp_index_value_mg_l)
         OR crp_index_value_mg_l < 0
    ) AS invalid_selected_crp_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
      WHERE crp_available_primary_window_flag
    ) AS episodes_flagged_crp_available_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
      WHERE crp_present_but_no_valid_value_flag
    ) AS episodes_with_crp_but_no_valid_value_n,

    (
      SELECT COUNT(*)
      FROM crp_index_episode
      WHERE crp_nearest_tie_n > 1
    ) AS selected_crp_nearest_tie_episode_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
      WHERE crp_any_assignment_review_flag
    ) AS episodes_with_assignment_review_n

  ;
  "
) %>%
  tibble::as_tibble()


if (
  crp_integrity_check$source_episode_n !=
  crp_integrity_check$output_episode_n ||
  crp_integrity_check$duplicated_output_episode_n != 0L ||
  crp_integrity_check$duplicated_assigned_crp_event_n != 0L ||
  crp_integrity_check$crp_window_violation_n != 0L ||
  crp_integrity_check$invalid_selected_crp_n != 0L ||
  crp_integrity_check$episodes_with_selected_crp_n !=
  crp_integrity_check$episodes_flagged_crp_available_n
) {
  stop(
    "CRP integration validation failed.",
    call. = FALSE
  )
}


candidate_cohort_working <- DBI::dbGetQuery(
  con,
  "
  SELECT *
  FROM candidate_cohort_working_table
  ORDER BY episode_id;
  "
) %>%
  tibble::as_tibble()


assert_unique_key(
  candidate_cohort_working,
  "episode_id",
  "candidate_cohort_working"
)


print(crp_integrity_check)

# ==============================================================================
# 28. SOFA SOURCES AND EPISODE ANCHORS
# ==============================================================================


# ------------------------------------------------------------------------------
# 28.1 Source files
# ------------------------------------------------------------------------------

sofa_source_files <- c(
  icustays = input_path("icustays.csv"),
  gcs = input_path("derived_gcs_timed.csv"),
  urine = input_path("derived_urine_output_rate.csv"),
  vasoactive = input_path("derived_vasoactive_agent.csv"),
  ventilation = input_path("derived_ventilation.csv"),
  icu_vitals = input_path("derived_vitalsign_icu.csv"),
  ed_vitals = input_path("vitalsign.csv")
)

missing_sofa_source_files <- sofa_source_files[
  !file.exists(sofa_source_files)
]

if (length(missing_sofa_source_files) > 0L) {
  stop(
    paste0(
      "Missing SOFA source file(s): ",
      paste(
        basename(missing_sofa_source_files),
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

# Preserve vector names while normalizing paths.

sofa_source_files <- stats::setNames(
  normalizePath(
    unname(sofa_source_files),
    winslash = "/",
    mustWork = TRUE
  ),
  names(sofa_source_files)
)

sofa_icustays_file <- sofa_source_files[["icustays"]]
sofa_gcs_file <- sofa_source_files[["gcs"]]
sofa_urine_file <- sofa_source_files[["urine"]]
sofa_vasoactive_file <- sofa_source_files[["vasoactive"]]
sofa_ventilation_file <- sofa_source_files[["ventilation"]]
sofa_icu_vitals_file <- sofa_source_files[["icu_vitals"]]
sofa_ed_vitals_file <- sofa_source_files[["ed_vitals"]]


all_sofa_csv_files <- list.files(
  INPUT_DIR,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

all_sofa_csv_files <- normalizePath(
  all_sofa_csv_files,
  winslash = "/",
  mustWork = TRUE
)

sofa_csv_paths_lower <- tolower(all_sofa_csv_files)
sofa_csv_names_lower <- tolower(basename(all_sofa_csv_files))

sofa_labevents_files <- all_sofa_csv_files[
  grepl(
    "(^|/)labevents_study_raw(/|$)",
    sofa_csv_paths_lower
  ) |
    grepl(
      "^labevents_study_raw.*\\.csv$",
      sofa_csv_names_lower
    ) |
    grepl(
      "^labevents_selected.*\\.csv$",
      sofa_csv_names_lower
    )
]

sofa_labevents_files <- sort(unique(sofa_labevents_files))

if (length(sofa_labevents_files) == 0L) {
  stop(
    "No selected labevents files were found for SOFA.",
    call. = FALSE
  )
}

sofa_labevents_file_list_sql <- paste0(
  "[",
  paste(
    vapply(
      sofa_labevents_files,
      function(path) quote_sql_path(con, path),
      character(1L)
    ),
    collapse = ", "
  ),
  "]"
)


assert_sofa_source_columns <- function(
    table_name,
    required_columns
) {
  missing_columns <- setdiff(
    required_columns,
    DBI::dbListFields(con, table_name)
  )
  
  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        table_name,
        " is missing required column(s): ",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


# ------------------------------------------------------------------------------
# 28.2 Candidate-cohort episode identifiers
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_candidate_episode_ids AS

  SELECT episode_id
  FROM candidate_cohort_working_table;
  "
)


sofa_candidate_episode_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS episode_n,
    COUNT(DISTINCT episode_id) AS distinct_episode_n
  FROM sofa_candidate_episode_ids;
  "
) %>%
  tibble::as_tibble()

if (
  sofa_candidate_episode_check$episode_n !=
  sofa_candidate_episode_check$distinct_episode_n
) {
  stop(
    "The SOFA candidate cohort is not unique by episode_id.",
    call. = FALSE
  )
}


# ==============================================================================
# 28.3 ICU STAY LINKAGE
# ==============================================================================

sofa_icustays_file_sql <- quote_sql_path(
  con,
  sofa_icustays_file
)

DBI::dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP VIEW sofa_icustays_source_raw AS

    SELECT *
    FROM read_csv_auto(
      %s,
      header = TRUE,
      all_varchar = TRUE,
      sample_size = -1,
      ignore_errors = FALSE
    );
    ",
    sofa_icustays_file_sql
  )
)

assert_sofa_source_columns(
  "sofa_icustays_source_raw",
  c(
    "subject_id",
    "hadm_id",
    "stay_id",
    "first_careunit",
    "last_careunit",
    "intime",
    "outtime",
    "los"
  )
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_icustays AS

  SELECT
    NULLIF(TRIM(CAST(subject_id AS VARCHAR)), '')
      AS subject_id,

    NULLIF(TRIM(CAST(hadm_id AS VARCHAR)), '')
      AS hadm_id,

    NULLIF(TRIM(CAST(stay_id AS VARCHAR)), '')
      AS icu_stay_id,

    NULLIF(TRIM(CAST(first_careunit AS VARCHAR)), '')
      AS first_careunit,

    NULLIF(TRIM(CAST(last_careunit AS VARCHAR)), '')
      AS last_careunit,

    CASE
      WHEN LENGTH(TRIM(CAST(intime AS VARCHAR))) >= 19
      THEN TRY_CAST(
        REPLACE(
          TRIM(CAST(intime AS VARCHAR)),
          'T',
          ' '
        ) AS TIMESTAMP
      )
    END AS icu_intime,

    CASE
      WHEN LENGTH(TRIM(CAST(outtime AS VARCHAR))) >= 19
      THEN TRY_CAST(
        REPLACE(
          TRIM(CAST(outtime AS VARCHAR)),
          'T',
          ' '
        ) AS TIMESTAMP
      )
    END AS icu_outtime,

    TRY_CAST(los AS DOUBLE)
      AS icu_los_days

  FROM sofa_icustays_source_raw;
  "
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_icu_episode_candidates AS

  SELECT
    episode.episode_id,
    episode.subject_id AS episode_subject_id,
    episode.hadm_id AS episode_hadm_id,
    episode.stay_id AS ed_stay_id,
    episode.ed_intime,
    episode.ed_outtime,

    icu.subject_id AS icu_subject_id,
    icu.hadm_id AS icu_hadm_id,
    icu.icu_stay_id,
    icu.first_careunit,
    icu.last_careunit,
    icu.icu_intime,
    icu.icu_outtime,
    icu.icu_los_days,

    date_diff(
      'second',
      episode.ed_intime,
      icu.icu_intime
    ) / 3600.0 AS hours_ed_to_icu_start,

    date_diff(
      'second',
      episode.ed_intime,
      icu.icu_outtime
    ) / 3600.0 AS hours_ed_to_icu_end,

    episode.ed_outtime IS NOT NULL
      AND icu.icu_intime BETWEEN
        episode.ed_intime AND episode.ed_outtime
      AS icu_start_inside_ed_interval_flag,

    icu.icu_intime >= episode.ed_intime
      AS icu_started_after_ed_arrival_flag,

    icu.icu_intime <= episode.ed_intime
      AND (
        icu.icu_outtime IS NULL
        OR icu.icu_outtime >= episode.ed_intime
      )
      AS icu_active_at_ed_arrival_flag

  FROM episode_spine_table AS episode

  INNER JOIN sofa_icustays AS icu
    ON CAST(episode.subject_id AS VARCHAR) = icu.subject_id
   AND CAST(episode.hadm_id AS VARCHAR) = icu.hadm_id

  WHERE icu.icu_stay_id IS NOT NULL
    AND icu.icu_intime IS NOT NULL
    AND (
      icu.icu_outtime IS NULL
      OR icu.icu_outtime >= icu.icu_intime
    )
    AND episode.ed_intime IS NOT NULL
    AND icu.icu_intime <=
      episode.ed_intime + INTERVAL '24 hours'
    AND (
      icu.icu_outtime IS NULL
      OR icu.icu_outtime >= episode.ed_intime
    );
  "
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_icu_episode_map AS

  WITH counted AS (
    SELECT
      *,
      COUNT(*) OVER (
        PARTITION BY icu_stay_id
      ) AS icu_n_candidate_episodes

    FROM sofa_icu_episode_candidates
  ),

  ranked AS (
    SELECT
      *,

      ROW_NUMBER() OVER (
        PARTITION BY icu_stay_id
        ORDER BY
          icu_start_inside_ed_interval_flag DESC,
          icu_started_after_ed_arrival_flag DESC,

          CASE
            WHEN hours_ed_to_icu_start >= 0
            THEN hours_ed_to_icu_start
            ELSE 1000000000.0
          END,

          ed_intime DESC,
          TRY_CAST(ed_stay_id AS BIGINT) NULLS LAST,
          ed_stay_id,
          episode_id
      ) AS icu_episode_assignment_rank

    FROM counted
  )

  SELECT
    *,

    icu_n_candidate_episodes > 1
      AS icu_assignment_ambiguous_flag,

    CASE
      WHEN icu_n_candidate_episodes = 1
        THEN 'single_candidate_episode'

      WHEN icu_start_inside_ed_interval_flag
        THEN 'icu_start_inside_ed_interval'

      WHEN icu_started_after_ed_arrival_flag
        THEN 'smallest_nonnegative_time_from_ed'

      WHEN icu_active_at_ed_arrival_flag
        THEN 'icu_active_at_ed_arrival'

      ELSE 'latest_ed_arrival'
    END AS icu_episode_assignment_rule

  FROM ranked

  WHERE icu_episode_assignment_rank = 1;
  "
)


# Restriction occurs after global event-to-episode assignment.

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_icu_episode_map_candidate AS

  SELECT icu.*

  FROM sofa_icu_episode_map AS icu

  INNER JOIN sofa_candidate_episode_ids AS cohort
    USING (episode_id);
  "
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_icu_episode_summary AS

  WITH ranked AS (
    SELECT
      *,

      COUNT(*) OVER (
        PARTITION BY episode_id
      ) AS icu_n_stays_0_24h,

      ROW_NUMBER() OVER (
        PARTITION BY episode_id
        ORDER BY
          icu_intime,
          TRY_CAST(icu_stay_id AS BIGINT) NULLS LAST,
          icu_stay_id
      ) AS episode_icu_rank

    FROM sofa_icu_episode_map_candidate
  )

  SELECT
    episode_id,
    icu_n_stays_0_24h,
    TRUE AS icu_overlap_0_24h_flag,

    icu_stay_id
      AS first_icu_stay_id_0_24h,

    icu_intime
      AS first_icu_intime_0_24h,

    icu_outtime
      AS first_icu_outtime_0_24h,

    hours_ed_to_icu_start
      AS first_icu_hours_from_ed,

    first_careunit
      AS first_icu_careunit_0_24h,

    icu_episode_assignment_rule
      AS first_icu_assignment_rule,

    icu_assignment_ambiguous_flag
      AS first_icu_assignment_ambiguous_flag

  FROM ranked

  WHERE episode_icu_rank = 1;
  "
)


# ==============================================================================
# 28.4 SOFA LABORATORY EVENTS
# ==============================================================================

DBI::dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP VIEW sofa_labevents_source_raw AS

    SELECT *

    FROM read_csv_auto(
      %s,
      header = TRUE,
      all_varchar = TRUE,
      union_by_name = TRUE,
      filename = TRUE,
      sample_size = 20480,
      ignore_errors = FALSE
    );
    ",
    sofa_labevents_file_list_sql
  )
)

assert_sofa_source_columns(
  "sofa_labevents_source_raw",
  c(
    "labevent_id",
    "subject_id",
    "hadm_id",
    "specimen_id",
    "itemid",
    "label",
    "fluid",
    "category",
    "charttime",
    "storetime",
    "value",
    "valuenum",
    "valueuom",
    "ref_range_lower",
    "ref_range_upper",
    "flag",
    "priority",
    "comments"
  )
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_labevents_source AS

  WITH selected AS (
    SELECT
      NULLIF(TRIM(CAST(labevent_id AS VARCHAR)), '')
        AS labevent_id,

      NULLIF(TRIM(CAST(subject_id AS VARCHAR)), '')
        AS lab_subject_id,

      NULLIF(TRIM(CAST(hadm_id AS VARCHAR)), '')
        AS lab_hadm_id,

      NULLIF(TRIM(CAST(specimen_id AS VARCHAR)), '')
        AS specimen_id,

      TRY_CAST(itemid AS BIGINT)
        AS itemid,

      NULLIF(TRIM(CAST(label AS VARCHAR)), '')
        AS label,

      NULLIF(TRIM(CAST(fluid AS VARCHAR)), '')
        AS fluid,

      NULLIF(TRIM(CAST(category AS VARCHAR)), '')
        AS category,

      CASE
        WHEN LENGTH(TRIM(CAST(charttime AS VARCHAR))) >= 19
        THEN TRY_CAST(
          REPLACE(
            TRIM(CAST(charttime AS VARCHAR)),
            'T',
            ' '
          ) AS TIMESTAMP
        )
      END AS charttime,

      CASE
        WHEN LENGTH(TRIM(CAST(storetime AS VARCHAR))) >= 19
        THEN TRY_CAST(
          REPLACE(
            TRIM(CAST(storetime AS VARCHAR)),
            'T',
            ' '
          ) AS TIMESTAMP
        )
      END AS storetime,

      NULLIF(TRIM(CAST(value AS VARCHAR)), '')
        AS value_original,

      TRY_CAST(valuenum AS DOUBLE)
        AS valuenum,

      NULLIF(TRIM(CAST(valueuom AS VARCHAR)), '')
        AS valueuom_original,

      regexp_replace(
        lower(
          trim(
            coalesce(
              CAST(valueuom AS VARCHAR),
              ''
            )
          )
        ),
        '\\s+',
        '',
        'g'
      ) AS valueuom_normalized,

      TRY_CAST(ref_range_lower AS DOUBLE)
        AS ref_range_lower,

      TRY_CAST(ref_range_upper AS DOUBLE)
        AS ref_range_upper,

      NULLIF(TRIM(CAST(flag AS VARCHAR)), '')
        AS laboratory_flag,

      NULLIF(TRIM(CAST(priority AS VARCHAR)), '')
        AS laboratory_priority,

      NULLIF(TRIM(CAST(comments AS VARCHAR)), '')
        AS laboratory_comments,

      regexp_extract(
        CAST(filename AS VARCHAR),
        '[^/]+$'
      ) AS source_file,

      ROW_NUMBER() OVER ()
        AS source_row_id

    FROM sofa_labevents_source_raw

    WHERE TRY_CAST(itemid AS BIGINT) IN (
      50816,
      50821,
      52033,
      50885,
      53089,
      50889,
      50912,
      52546,
      51265,
      53189
    )
  )

  SELECT
    *,

    CASE
      WHEN itemid = 50816
        THEN 'fio2'

      WHEN itemid = 50821
        THEN 'pao2'

      WHEN itemid = 52033
        THEN 'blood_gas_specimen_type'

      WHEN itemid IN (50885, 53089)
        THEN 'bilirubin'

      WHEN itemid IN (50912, 52546)
        THEN 'creatinine'

      WHEN itemid IN (51265, 53189)
        THEN 'platelet_count'

      ELSE 'unexpected'
    END AS sofa_lab_concept

  FROM selected;
  "
)


# ------------------------------------------------------------------------------
# 28.5 Primary laboratory linkage: subject_id + hadm_id
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_lab_primary_candidates AS

  SELECT
    laboratory.*,
    episode.episode_id,
    episode.stay_id AS ed_stay_id,
    episode.ed_intime,
    episode.ed_outtime,

    'subject_id_and_hadm_id'
      AS sofa_lab_linkage_method,

    date_diff(
      'second',
      episode.ed_intime,
      laboratory.charttime
    ) / 3600.0 AS sofa_lab_hours_from_ed,

    episode.ed_outtime IS NOT NULL
      AND laboratory.charttime BETWEEN
        episode.ed_intime AND episode.ed_outtime
      AS sofa_lab_within_ed_interval_flag

  FROM sofa_labevents_source AS laboratory

  INNER JOIN episode_spine_table AS episode
    ON laboratory.lab_subject_id =
         CAST(episode.subject_id AS VARCHAR)
   AND laboratory.lab_hadm_id =
         CAST(episode.hadm_id AS VARCHAR)

  WHERE laboratory.labevent_id IS NOT NULL
    AND laboratory.lab_hadm_id IS NOT NULL
    AND laboratory.charttime IS NOT NULL
    AND episode.ed_intime IS NOT NULL
    AND laboratory.charttime BETWEEN
      episode.ed_intime
      AND episode.ed_intime + INTERVAL '24 hours';
  "
)


# ------------------------------------------------------------------------------
# 28.6 Fallback laboratory linkage: subject_id + time
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_lab_fallback_candidates AS

  SELECT
    laboratory.*,
    episode.episode_id,
    episode.stay_id AS ed_stay_id,
    episode.ed_intime,
    episode.ed_outtime,

    'subject_id_and_time'
      AS sofa_lab_linkage_method,

    date_diff(
      'second',
      episode.ed_intime,
      laboratory.charttime
    ) / 3600.0 AS sofa_lab_hours_from_ed,

    episode.ed_outtime IS NOT NULL
      AND laboratory.charttime BETWEEN
        episode.ed_intime AND episode.ed_outtime
      AS sofa_lab_within_ed_interval_flag

  FROM sofa_labevents_source AS laboratory

  INNER JOIN episode_spine_table AS episode
    ON laboratory.lab_subject_id =
         CAST(episode.subject_id AS VARCHAR)

  WHERE laboratory.labevent_id IS NOT NULL
    AND laboratory.charttime IS NOT NULL
    AND episode.ed_intime IS NOT NULL
    AND laboratory.charttime BETWEEN
      episode.ed_intime
      AND episode.ed_intime + INTERVAL '24 hours'

    AND (
      laboratory.lab_hadm_id IS NULL
      OR episode.hadm_id IS NULL
    )

    AND NOT EXISTS (
      SELECT 1
      FROM sofa_lab_primary_candidates AS primary_candidate
      WHERE primary_candidate.labevent_id =
        laboratory.labevent_id
    );
  "
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_lab_assignment_candidates AS

  SELECT *
  FROM sofa_lab_primary_candidates

  UNION ALL

  SELECT *
  FROM sofa_lab_fallback_candidates;
  "
)


# ------------------------------------------------------------------------------
# 28.7 Unique laboratory event-to-episode assignment
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_labevents_assigned_0_24h AS

  WITH counted AS (
    SELECT
      *,

      COUNT(*) OVER (
        PARTITION BY labevent_id
      ) AS sofa_lab_n_candidate_episodes

    FROM sofa_lab_assignment_candidates
  ),

  ranked AS (
    SELECT
      *,

      ROW_NUMBER() OVER (
        PARTITION BY labevent_id
        ORDER BY
          sofa_lab_within_ed_interval_flag DESC,
          sofa_lab_hours_from_ed,
          ed_intime DESC,
          TRY_CAST(ed_stay_id AS BIGINT) NULLS LAST,
          ed_stay_id,
          episode_id
      ) AS sofa_lab_episode_assignment_rank

    FROM counted
  )

  SELECT
    *,

    sofa_lab_n_candidate_episodes > 1
      AS sofa_lab_assignment_ambiguous_flag,

    CASE
      WHEN sofa_lab_n_candidate_episodes = 1
        THEN 'single_candidate_episode'

      WHEN sofa_lab_within_ed_interval_flag
        THEN 'within_ed_interval_preferred'

      ELSE 'smallest_nonnegative_time_from_ed'
    END AS sofa_lab_episode_assignment_rule

  FROM ranked

  WHERE sofa_lab_episode_assignment_rank = 1;
  "
)


# Restriction occurs after global event-to-episode assignment.

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_labevents_candidate_0_24h AS

  SELECT laboratory.*

  FROM sofa_labevents_assigned_0_24h AS laboratory

  INNER JOIN sofa_candidate_episode_ids AS cohort
    USING (episode_id);
  "
)


# ==============================================================================
# 28.8 SOFA ANCHOR VALIDATION
# ==============================================================================

sofa_anchor_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    (
      SELECT COUNT(*)
      FROM sofa_candidate_episode_ids
    ) AS candidate_episode_n,

    (
      SELECT COUNT(*) - COUNT(DISTINCT episode_id)
      FROM sofa_candidate_episode_ids
    ) AS duplicated_candidate_episode_n,

    (
      SELECT COUNT(*)
      FROM sofa_icu_episode_map
    ) AS assigned_icu_stay_n,

    (
      SELECT COUNT(*) - COUNT(DISTINCT icu_stay_id)
      FROM sofa_icu_episode_map
    ) AS duplicated_assigned_icu_stay_n,

    (
      SELECT COUNT(*)
      FROM sofa_icu_episode_summary
    ) AS candidate_episode_with_icu_n,

    (
      SELECT COUNT(*) - COUNT(DISTINCT episode_id)
      FROM sofa_icu_episode_summary
    ) AS duplicated_icu_summary_episode_n,

    (
      SELECT COUNT(*)
      FROM sofa_labevents_assigned_0_24h
    ) AS assigned_sofa_labevent_n,

    (
      SELECT COUNT(*) - COUNT(DISTINCT labevent_id)
      FROM sofa_labevents_assigned_0_24h
    ) AS duplicated_assigned_labevent_n,

    (
      SELECT COUNT(*)
      FROM sofa_labevents_assigned_0_24h
      WHERE charttime IS NULL
         OR charttime < ed_intime
         OR charttime > ed_intime + INTERVAL '24 hours'
    ) AS labevent_window_violation_n,

    (
      SELECT COUNT(*)
      FROM sofa_labevents_candidate_0_24h
    ) AS candidate_sofa_labevent_n,

    (
      SELECT COUNT(DISTINCT episode_id)
      FROM sofa_labevents_candidate_0_24h
    ) AS candidate_episode_with_sofa_laboratory_n;
  "
) %>%
  tibble::as_tibble()


if (
  sofa_anchor_check$duplicated_candidate_episode_n != 0L ||
  sofa_anchor_check$duplicated_assigned_icu_stay_n != 0L ||
  sofa_anchor_check$duplicated_icu_summary_episode_n != 0L ||
  sofa_anchor_check$duplicated_assigned_labevent_n != 0L ||
  sofa_anchor_check$labevent_window_violation_n != 0L
) {
  stop(
    "SOFA source and episode-anchor validation failed.",
    call. = FALSE
  )
}

print(sofa_anchor_check)

# ==============================================================================
# 29. RESPIRATORY SOFA COMPONENT
# ==============================================================================

# PaO2 and FiO2 are paired within the same specimen.
# SOFA scores 3-4 require invasive respiratory support at PaO2 measurement.


# ------------------------------------------------------------------------------
# 29.1 Ventilation intervals
# ------------------------------------------------------------------------------

sofa_ventilation_file_sql <- quote_sql_path(
  con,
  sofa_ventilation_file
)

DBI::dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP VIEW sofa_ventilation_source_raw AS

    SELECT *
    FROM read_csv_auto(
      %s,
      header = TRUE,
      all_varchar = TRUE,
      sample_size = -1,
      ignore_errors = FALSE
    );
    ",
    sofa_ventilation_file_sql
  )
)

assert_sofa_source_columns(
  "sofa_ventilation_source_raw",
  c(
    "stay_id",
    "starttime",
    "endtime",
    "ventilation_status"
  )
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_ventilation_source AS

  WITH parsed AS (
    SELECT
      CASE
        WHEN upper(trim(CAST(stay_id AS VARCHAR)))
          IN ('', 'NA', 'NAN', 'NULL')
        THEN NULL
        ELSE trim(CAST(stay_id AS VARCHAR))
      END AS icu_stay_id,

      CASE
        WHEN length(trim(CAST(starttime AS VARCHAR))) >= 19
        THEN TRY_CAST(
          replace(
            trim(CAST(starttime AS VARCHAR)),
            'T',
            ' '
          ) AS TIMESTAMP
        )
      END AS starttime,

      CASE
        WHEN length(trim(CAST(endtime AS VARCHAR))) >= 19
        THEN TRY_CAST(
          replace(
            trim(CAST(endtime AS VARCHAR)),
            'T',
            ' '
          ) AS TIMESTAMP
        )
      END AS endtime,

      NULLIF(
        trim(CAST(ventilation_status AS VARCHAR)),
        ''
      ) AS ventilation_status,

      ROW_NUMBER() OVER () AS ventilation_source_row_id

    FROM sofa_ventilation_source_raw
  ),

  normalized AS (
    SELECT
      *,

      regexp_replace(
        lower(coalesce(ventilation_status, '')),
        '[^a-z0-9]+',
        '',
        'g'
      ) AS ventilation_status_norm

    FROM parsed
  ),

  classified AS (
    SELECT
      *,

      CASE
        WHEN ventilation_status_norm = 'invasivevent'
          THEN 'invasive'
        WHEN ventilation_status_norm = 'noninvasivevent'
          THEN 'noninvasive'
        WHEN ventilation_status_norm = 'hfnc'
          THEN 'hfnc'
        WHEN ventilation_status_norm = 'supplementaloxygen'
          THEN 'supplemental_oxygen'
        WHEN ventilation_status_norm = 'tracheostomy'
          THEN 'tracheostomy'
        WHEN ventilation_status_norm = 'none'
          THEN 'none'
        ELSE 'other_or_unknown'
      END AS ventilation_class

    FROM normalized
  )

  SELECT
    *,

    ventilation_class = 'invasive'
      AS respiratory_support_flag,

    icu_stay_id IS NOT NULL
      AND starttime IS NOT NULL
      AND endtime IS NOT NULL
      AND endtime > starttime
      AS ventilation_interval_valid_flag,

    concat(
      'VENT_',
      coalesce(icu_stay_id, 'missing'),
      '_',
      CAST(ventilation_source_row_id AS VARCHAR)
    ) AS ventilation_event_id

  FROM classified;
  "
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_ventilation_events_0_24h AS

  SELECT
    icu.episode_id,
    icu.icu_stay_id,
    icu.ed_intime,
    icu.ed_outtime,

    ventilation.ventilation_event_id,
    ventilation.ventilation_source_row_id,
    ventilation.starttime,
    ventilation.endtime,
    ventilation.ventilation_status,
    ventilation.ventilation_status_norm,
    ventilation.ventilation_class,
    ventilation.respiratory_support_flag,

    greatest(
      ventilation.starttime,
      icu.ed_intime
    ) AS ventilation_overlap_start,

    least(
      ventilation.endtime,
      icu.ed_intime + INTERVAL '24 hours'
    ) AS ventilation_overlap_end

  FROM sofa_icu_episode_map_candidate AS icu

  INNER JOIN sofa_ventilation_source AS ventilation
    USING (icu_stay_id)

  WHERE ventilation.ventilation_interval_valid_flag
    AND ventilation.starttime <
      icu.ed_intime + INTERVAL '24 hours'
    AND ventilation.endtime > icu.ed_intime;
  "
)


# ------------------------------------------------------------------------------
# 29.2 Pair PaO2 and FiO2 within the same specimen
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_blood_gas_specimens AS

  WITH specimen_rows AS (
    SELECT *

    FROM sofa_labevents_candidate_0_24h

    WHERE specimen_id IS NOT NULL
      AND sofa_lab_concept IN (
        'pao2',
        'fio2',
        'blood_gas_specimen_type'
      )
  ),

  aggregated AS (
    SELECT
      episode_id,
      specimen_id,

      COUNT(*) FILTER (
        WHERE sofa_lab_concept = 'pao2'
      ) AS n_po2_rows,

      COUNT(*) FILTER (
        WHERE sofa_lab_concept = 'fio2'
      ) AS n_fio2_rows,

      COUNT(DISTINCT valuenum) FILTER (
        WHERE sofa_lab_concept = 'pao2'
          AND valuenum IS NOT NULL
          AND isfinite(valuenum)
      ) AS n_po2_unique_numeric,

      COUNT(DISTINCT valuenum) FILTER (
        WHERE sofa_lab_concept = 'fio2'
          AND valuenum IS NOT NULL
          AND isfinite(valuenum)
      ) AS n_fio2_unique_numeric,

      CASE
        WHEN COUNT(DISTINCT valuenum) FILTER (
          WHERE sofa_lab_concept = 'pao2'
            AND valuenum IS NOT NULL
            AND isfinite(valuenum)
        ) = 1
        THEN MIN(valuenum) FILTER (
          WHERE sofa_lab_concept = 'pao2'
            AND valuenum IS NOT NULL
            AND isfinite(valuenum)
        )
      END AS po2_value,

      CASE
        WHEN COUNT(DISTINCT valuenum) FILTER (
          WHERE sofa_lab_concept = 'fio2'
            AND valuenum IS NOT NULL
            AND isfinite(valuenum)
        ) = 1
        THEN MIN(valuenum) FILTER (
          WHERE sofa_lab_concept = 'fio2'
            AND valuenum IS NOT NULL
            AND isfinite(valuenum)
        )
      END AS fio2_percent,

      MIN(charttime) FILTER (
        WHERE sofa_lab_concept = 'pao2'
          AND charttime IS NOT NULL
      ) AS pf_charttime,

      MIN(charttime) FILTER (
        WHERE sofa_lab_concept = 'fio2'
          AND charttime IS NOT NULL
      ) AS fio2_charttime,

      MIN(lower(trim(value_original))) FILTER (
        WHERE sofa_lab_concept = 'blood_gas_specimen_type'
          AND value_original IS NOT NULL
      ) AS specimen_type_value_example,

      bool_or(
        sofa_lab_concept = 'blood_gas_specimen_type'
        AND regexp_matches(
          lower(trim(coalesce(value_original, ''))),
          '^art\\.?$|arterial'
        )
      ) AS arterial_specimen_flag,

      bool_or(
        sofa_lab_concept = 'blood_gas_specimen_type'
        AND regexp_matches(
          lower(trim(coalesce(value_original, ''))),
          'ven|mixed|cap'
        )
      ) AS nonarterial_specimen_flag,

      bool_or(
        regexp_matches(
          lower(trim(coalesce(laboratory_comments, ''))),
          'not arterial'
        )
      ) AS not_arterial_comment_flag

    FROM specimen_rows

    GROUP BY episode_id, specimen_id
  ),

  validity AS (
    SELECT
      *,

      arterial_specimen_flag
        AND (
          nonarterial_specimen_flag
          OR not_arterial_comment_flag
        ) AS specimen_type_conflict_flag,

      abs(
        date_diff(
          'second',
          pf_charttime,
          fio2_charttime
        )
      ) / 60.0 AS pf_pair_time_difference_minutes,

      po2_value IS NOT NULL
        AND isfinite(po2_value)
        AND po2_value > 0
        AND po2_value <= 800
        AS po2_structurally_valid,

      fio2_percent IS NOT NULL
        AND isfinite(fio2_percent)
        AND fio2_percent >= 21
        AND fio2_percent <= 100
        AS fio2_structurally_valid

    FROM aggregated
  ),

  primary_pairs AS (
    SELECT
      *,

      arterial_specimen_flag
        AND NOT specimen_type_conflict_flag
        AND n_po2_unique_numeric = 1
        AND n_fio2_unique_numeric = 1
        AND po2_structurally_valid
        AND fio2_structurally_valid
        AS pf_valid_primary

    FROM validity
  )

  SELECT
    *,

    CASE
      WHEN pf_valid_primary
      THEN po2_value / (fio2_percent / 100.0)
    END AS pf_ratio,

    pf_valid_primary
      AND (
        n_po2_rows != 1
        OR n_fio2_rows != 1
        OR pf_pair_time_difference_minutes > 5
        OR po2_value < 20
        OR po2_value > 600
      ) AS pf_review_flag,

    concat(
      'PF_',
      episode_id,
      '_',
      specimen_id
    ) AS pf_event_id

  FROM primary_pairs;
  "
)

# ------------------------------------------------------------------------------
# 29.3 Respiratory support at each PaO2 measurement
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_pf_support_assessment AS

  WITH pf_events AS (
    SELECT *

    FROM sofa_blood_gas_specimens

    WHERE pf_valid_primary
      AND pf_charttime IS NOT NULL
  ),

  support AS (
    SELECT
      pf.episode_id,
      pf.specimen_id,
      pf.pf_event_id,
      pf.pf_charttime,
      pf.fio2_charttime,
      pf.pf_ratio,
      pf.po2_value,
      pf.fio2_percent,
      pf.pf_pair_time_difference_minutes,
      pf.pf_review_flag,

      COUNT(ventilation.ventilation_event_id) FILTER (
        WHERE pf.pf_charttime BETWEEN
          ventilation.ventilation_overlap_start
          AND ventilation.ventilation_overlap_end
      ) AS n_ventilation_intervals_at_pf,

      coalesce(bool_or(
        pf.pf_charttime BETWEEN
          ventilation.ventilation_overlap_start
          AND ventilation.ventilation_overlap_end
        AND ventilation.ventilation_class = 'invasive'
      ), FALSE) AS pf_on_invasive_support,

      coalesce(bool_or(
        pf.pf_charttime BETWEEN
          ventilation.ventilation_overlap_start
          AND ventilation.ventilation_overlap_end
        AND ventilation.ventilation_class = 'noninvasive'
      ), FALSE) AS pf_on_noninvasive_support,

      coalesce(bool_or(
        pf.pf_charttime BETWEEN
          ventilation.ventilation_overlap_start
          AND ventilation.ventilation_overlap_end
        AND ventilation.ventilation_class = 'hfnc'
      ), FALSE) AS pf_on_hfnc,

      coalesce(bool_or(
        pf.pf_charttime BETWEEN
          ventilation.ventilation_overlap_start
          AND ventilation.ventilation_overlap_end
        AND ventilation.ventilation_class =
          'supplemental_oxygen'
      ), FALSE) AS pf_on_supplemental_oxygen,

      coalesce(bool_or(
        pf.pf_charttime BETWEEN
          ventilation.ventilation_overlap_start
          AND ventilation.ventilation_overlap_end
        AND ventilation.ventilation_class = 'tracheostomy'
      ), FALSE) AS pf_on_tracheostomy,

      coalesce(bool_or(
        pf.pf_charttime BETWEEN
          ventilation.ventilation_overlap_start
          AND ventilation.ventilation_overlap_end
        AND ventilation.ventilation_class = 'none'
      ), FALSE) AS pf_on_no_support_interval,

      coalesce(bool_or(
        pf.pf_charttime BETWEEN
          ventilation.ventilation_overlap_start
          AND ventilation.ventilation_overlap_end
        AND ventilation.ventilation_class =
          'other_or_unknown'
      ), FALSE) AS pf_on_unknown_ventilation_interval

    FROM pf_events AS pf

    LEFT JOIN sofa_ventilation_events_0_24h AS ventilation
      USING (episode_id)

    GROUP BY
      pf.episode_id,
      pf.specimen_id,
      pf.pf_event_id,
      pf.pf_charttime,
      pf.fio2_charttime,
      pf.pf_ratio,
      pf.po2_value,
      pf.fio2_percent,
      pf.pf_pair_time_difference_minutes,
      pf.pf_review_flag
  ),

  classified AS (
    SELECT
      *,

      pf_on_invasive_support
        AS pf_respiratory_support_flag,

      CASE
        WHEN pf_on_invasive_support THEN 'invasive'
        WHEN pf_on_noninvasive_support THEN 'noninvasive'
        WHEN pf_on_hfnc THEN 'hfnc'
        WHEN pf_on_tracheostomy THEN 'tracheostomy'
        WHEN pf_on_supplemental_oxygen
          THEN 'supplemental_oxygen'
        WHEN pf_on_no_support_interval THEN 'none'
        WHEN pf_on_unknown_ventilation_interval
          THEN 'other_or_unknown'
        ELSE 'no_interval_coverage'
      END AS pf_ventilation_status_at_measurement

    FROM support
  )

  SELECT
    *,

    CASE
      WHEN pf_ratio < 100
        AND pf_respiratory_support_flag THEN 4
      WHEN pf_ratio < 200
        AND pf_respiratory_support_flag THEN 3
      WHEN pf_ratio < 300 THEN 2
      WHEN pf_ratio < 400 THEN 1
      WHEN pf_ratio >= 400 THEN 0
    END AS sofa_respiratory_structured_0_24h,

    pf_review_flag
      OR pf_on_unknown_ventilation_interval
      OR (
        pf_ratio < 200
        AND pf_ventilation_status_at_measurement =
          'no_interval_coverage'
      ) AS respiratory_manual_review_required

  FROM classified;
  "
)

# ------------------------------------------------------------------------------
# 29.4 Worst respiratory score per episode
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_respiratory_episode AS

  WITH ranked AS (
    SELECT
      *,

      COUNT(*) OVER (
        PARTITION BY episode_id
      ) AS respiratory_n_valid_pf_0_24h,

      bool_or(pf_respiratory_support_flag) OVER (
        PARTITION BY episode_id
      ) AS respiratory_any_support_0_24h,

      bool_or(respiratory_manual_review_required) OVER (
        PARTITION BY episode_id
      ) AS respiratory_any_review_0_24h,

      ROW_NUMBER() OVER (
        PARTITION BY episode_id
        ORDER BY
          sofa_respiratory_structured_0_24h DESC NULLS LAST,
          pf_ratio ASC NULLS LAST,
          pf_charttime,
          TRY_CAST(specimen_id AS BIGINT) NULLS LAST,
          specimen_id
      ) AS respiratory_selection_rank

    FROM sofa_pf_support_assessment
  )

  SELECT
    episode_id,
    respiratory_n_valid_pf_0_24h,
    respiratory_any_support_0_24h,
    respiratory_any_review_0_24h,
    sofa_respiratory_structured_0_24h,

    pf_ratio
      AS respiratory_worst_pf_ratio_0_24h,

    pf_charttime
      AS respiratory_worst_pf_time_0_24h,

    po2_value
      AS respiratory_worst_po2_0_24h,

    fio2_percent
      AS respiratory_worst_fio2_percent_0_24h,

    pf_respiratory_support_flag
      AS respiratory_support_at_worst_pf,

    pf_ventilation_status_at_measurement
      AS respiratory_ventilation_status_at_worst_pf,

    specimen_id
      AS respiratory_worst_specimen_id,

    pf_event_id
      AS respiratory_worst_pf_event_id,

    pf_pair_time_difference_minutes
      AS respiratory_worst_pf_pair_time_difference_minutes,

    respiratory_manual_review_required
      AS respiratory_worst_review_flag

  FROM ranked

  WHERE respiratory_selection_rank = 1;
  "
)


# ==============================================================================
# 29.5 RESPIRATORY SOFA VALIDATION
# ==============================================================================

respiratory_sofa_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS episode_with_respiratory_score_n,

    COUNT(*) - COUNT(DISTINCT episode_id)
      AS duplicated_episode_n,

    COUNT(*) FILTER (
      WHERE sofa_respiratory_structured_0_24h
        NOT BETWEEN 0 AND 4
    ) AS invalid_score_n,

    COUNT(*) FILTER (
      WHERE respiratory_worst_pf_ratio_0_24h IS NULL
         OR respiratory_worst_pf_ratio_0_24h <= 0
         OR NOT isfinite(
           respiratory_worst_pf_ratio_0_24h
         )
    ) AS invalid_selected_pf_n,

    COUNT(*) FILTER (
      WHERE respiratory_any_review_0_24h
    ) AS episode_requiring_review_n

  FROM sofa_respiratory_episode;
  "
) %>%
  tibble::as_tibble()


if (
  respiratory_sofa_check$duplicated_episode_n != 0L ||
  respiratory_sofa_check$invalid_score_n != 0L ||
  respiratory_sofa_check$invalid_selected_pf_n != 0L
) {
  stop(
    "Respiratory SOFA validation failed.",
    call. = FALSE
  )
}

print(respiratory_sofa_check)

# ==============================================================================
# 30. COAGULATION SOFA COMPONENT
# ==============================================================================


# ------------------------------------------------------------------------------
# 30.1 Platelet events in the 0-24 h window
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_platelet_events_0_24h AS

  SELECT
    episode_id,
    labevent_id,
    specimen_id,
    itemid,
    charttime,
    sofa_lab_hours_from_ed,

    valuenum AS platelet_value_k_ul,

    valueuom_original AS platelet_unit_original,
    valueuom_normalized AS platelet_unit_normalized,

    valuenum IS NOT NULL
      AND isfinite(valuenum)
      AND valuenum > 0
      AND valuenum <= 5000
      AS platelet_valid_flag,

    coalesce(valueuom_normalized, '') NOT IN (
      'k/ul',
      '10^3/ul',
      '10*3/ul',
      'thou/ul'
    ) AS platelet_unit_review_flag,

    coalesce(valueuom_normalized, '') NOT IN (
      'k/ul',
      '10^3/ul',
      '10*3/ul',
      'thou/ul'
    )
      OR (
        valuenum IS NOT NULL
        AND (
          valuenum < 5
          OR valuenum > 1000
        )
      ) AS platelet_review_flag,

    concat(
      'PLT_',
      labevent_id
    ) AS platelet_event_id,

    sofa_lab_linkage_method,
    sofa_lab_n_candidate_episodes,
    sofa_lab_assignment_ambiguous_flag

  FROM sofa_labevents_candidate_0_24h

  WHERE sofa_lab_concept = 'platelet_count';
  "
)

# ------------------------------------------------------------------------------
# 30.2 First and minimum valid platelet values per episode
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_coagulation_episode AS

  WITH episode_counts AS (
    SELECT
      episode_id,

      COUNT(*)::BIGINT
        AS platelet_n_all_0_24h,

      COUNT(*) FILTER (
        WHERE platelet_valid_flag
      )::BIGINT
        AS platelet_n_valid_0_24h,

      bool_or(platelet_review_flag)
        AS platelet_any_review_flag_0_24h

    FROM sofa_platelet_events_0_24h

    GROUP BY episode_id
  ),

  valid_events AS (
    SELECT *

    FROM sofa_platelet_events_0_24h

    WHERE platelet_valid_flag
  ),

  first_ranked AS (
    SELECT
      *,

      ROW_NUMBER() OVER (
        PARTITION BY episode_id
        ORDER BY
          charttime,
          TRY_CAST(labevent_id AS BIGINT) NULLS LAST,
          labevent_id
      ) AS platelet_first_rank

    FROM valid_events
  ),

  minimum_ranked AS (
    SELECT
      *,

      ROW_NUMBER() OVER (
        PARTITION BY episode_id
        ORDER BY
          platelet_value_k_ul,
          charttime,
          TRY_CAST(labevent_id AS BIGINT) NULLS LAST,
          labevent_id
      ) AS platelet_min_rank

    FROM valid_events
  ),

  first_events AS (
    SELECT
      episode_id,

      platelet_value_k_ul
        AS platelet_first_value_0_24h,

      charttime
        AS platelet_first_time_0_24h,

      sofa_lab_hours_from_ed
        AS platelet_first_hours_from_ed,

      itemid
        AS platelet_first_itemid_0_24h,

      platelet_event_id
        AS platelet_first_event_id_0_24h

    FROM first_ranked

    WHERE platelet_first_rank = 1
  ),

  minimum_events AS (
    SELECT
      episode_id,

      platelet_value_k_ul
        AS platelet_min_value_0_24h,

      charttime
        AS platelet_min_time_0_24h,

      sofa_lab_hours_from_ed
        AS platelet_min_hours_from_ed,

      itemid
        AS platelet_min_itemid_0_24h,

      platelet_event_id
        AS platelet_min_event_id_0_24h,

      platelet_review_flag
        AS platelet_min_review_flag_0_24h

    FROM minimum_ranked

    WHERE platelet_min_rank = 1
  )

  SELECT
    counts.*,

    first_event.platelet_first_value_0_24h,
    first_event.platelet_first_time_0_24h,
    first_event.platelet_first_hours_from_ed,
    first_event.platelet_first_itemid_0_24h,
    first_event.platelet_first_event_id_0_24h,

    minimum_event.platelet_min_value_0_24h,
    minimum_event.platelet_min_time_0_24h,
    minimum_event.platelet_min_hours_from_ed,
    minimum_event.platelet_min_itemid_0_24h,
    minimum_event.platelet_min_event_id_0_24h,
    minimum_event.platelet_min_review_flag_0_24h,

    CASE
      WHEN minimum_event.platelet_min_value_0_24h IS NULL
        THEN NULL
      WHEN minimum_event.platelet_min_value_0_24h < 20
        THEN 4
      WHEN minimum_event.platelet_min_value_0_24h < 50
        THEN 3
      WHEN minimum_event.platelet_min_value_0_24h < 100
        THEN 2
      WHEN minimum_event.platelet_min_value_0_24h < 150
        THEN 1
      ELSE 0
    END AS sofa_coagulation_structured_0_24h

  FROM episode_counts AS counts

  LEFT JOIN first_events AS first_event
    USING (episode_id)

  LEFT JOIN minimum_events AS minimum_event
    USING (episode_id);
  "
)

# ==============================================================================
# 30.3 COAGULATION SOFA VALIDATION
# ==============================================================================

coagulation_sofa_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS episode_with_platelet_event_n,

    COUNT(*) FILTER (
      WHERE sofa_coagulation_structured_0_24h IS NOT NULL
    ) AS episode_with_coagulation_score_n,

    COUNT(*) - COUNT(DISTINCT episode_id)
      AS duplicated_episode_n,

    COUNT(*) FILTER (
      WHERE sofa_coagulation_structured_0_24h
        NOT BETWEEN 0 AND 4
    ) AS invalid_score_n,

    COUNT(*) FILTER (
      WHERE sofa_coagulation_structured_0_24h IS NOT NULL
        AND (
          platelet_min_value_0_24h IS NULL
          OR NOT isfinite(platelet_min_value_0_24h)
          OR platelet_min_value_0_24h <= 0
          OR platelet_min_value_0_24h > 5000
        )
    ) AS invalid_selected_platelet_n,

    COUNT(*) FILTER (
      WHERE platelet_n_valid_0_24h = 0
        AND sofa_coagulation_structured_0_24h IS NOT NULL
    ) AS score_without_valid_platelet_n,

    COUNT(*) FILTER (
      WHERE platelet_any_review_flag_0_24h
    ) AS episode_requiring_review_n

  FROM sofa_coagulation_episode;
  "
) %>%
  tibble::as_tibble()


if (
  coagulation_sofa_check$duplicated_episode_n != 0L ||
  coagulation_sofa_check$invalid_score_n != 0L ||
  coagulation_sofa_check$invalid_selected_platelet_n != 0L ||
  coagulation_sofa_check$score_without_valid_platelet_n != 0L
) {
  stop(
    "Coagulation SOFA validation failed.",
    call. = FALSE
  )
}

print(coagulation_sofa_check)

# ==============================================================================
# 31. HEPATIC SOFA COMPONENT
# ==============================================================================


# ------------------------------------------------------------------------------
# 31.1 Bilirubin events in the 0-24 h window
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_bilirubin_events_0_24h AS

  SELECT
    episode_id,
    labevent_id,
    specimen_id,
    itemid,
    charttime,
    sofa_lab_hours_from_ed,

    valuenum
      AS bilirubin_value_mg_dl,

    valueuom_original
      AS bilirubin_unit_original,

    valueuom_normalized
      AS bilirubin_unit_normalized,

    valuenum IS NOT NULL
      AND isfinite(valuenum)
      AND valuenum > 0
      AND valuenum <= 100
      AS bilirubin_valid_flag,

    coalesce(valueuom_normalized, '') != 'mg/dl'
      AS bilirubin_unit_review_flag,

    coalesce(valueuom_normalized, '') != 'mg/dl'
      OR (
        valuenum IS NOT NULL
        AND valuenum > 40
      )
      AS bilirubin_review_flag,

    concat(
      'BILI_',
      labevent_id
    ) AS bilirubin_event_id,

    sofa_lab_linkage_method,
    sofa_lab_n_candidate_episodes,
    sofa_lab_assignment_ambiguous_flag

  FROM sofa_labevents_candidate_0_24h

  WHERE sofa_lab_concept = 'bilirubin';
  "
)

# ------------------------------------------------------------------------------
# 31.2 First and maximum valid bilirubin values per episode
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_hepatic_episode AS

  WITH episode_counts AS (
    SELECT
      episode_id,

      COUNT(*)::BIGINT
        AS bilirubin_n_all_0_24h,

      COUNT(*) FILTER (
        WHERE bilirubin_valid_flag
      )::BIGINT
        AS bilirubin_n_valid_0_24h,

      bool_or(bilirubin_review_flag)
        AS bilirubin_any_review_flag_0_24h

    FROM sofa_bilirubin_events_0_24h

    GROUP BY episode_id
  ),

  valid_events AS (
    SELECT *

    FROM sofa_bilirubin_events_0_24h

    WHERE bilirubin_valid_flag
  ),

  first_ranked AS (
    SELECT
      *,

      ROW_NUMBER() OVER (
        PARTITION BY episode_id
        ORDER BY
          charttime,
          TRY_CAST(labevent_id AS BIGINT) NULLS LAST,
          labevent_id
      ) AS bilirubin_first_rank

    FROM valid_events
  ),

  maximum_ranked AS (
    SELECT
      *,

      ROW_NUMBER() OVER (
        PARTITION BY episode_id
        ORDER BY
          bilirubin_value_mg_dl DESC,
          charttime,
          TRY_CAST(labevent_id AS BIGINT) NULLS LAST,
          labevent_id
      ) AS bilirubin_max_rank

    FROM valid_events
  ),

  first_events AS (
    SELECT
      episode_id,

      bilirubin_value_mg_dl
        AS bilirubin_first_value_0_24h,

      charttime
        AS bilirubin_first_time_0_24h,

      sofa_lab_hours_from_ed
        AS bilirubin_first_hours_from_ed,

      itemid
        AS bilirubin_first_itemid_0_24h,

      bilirubin_event_id
        AS bilirubin_first_event_id_0_24h

    FROM first_ranked

    WHERE bilirubin_first_rank = 1
  ),

  maximum_events AS (
    SELECT
      episode_id,

      bilirubin_value_mg_dl
        AS bilirubin_max_value_0_24h,

      charttime
        AS bilirubin_max_time_0_24h,

      sofa_lab_hours_from_ed
        AS bilirubin_max_hours_from_ed,

      itemid
        AS bilirubin_max_itemid_0_24h,

      bilirubin_event_id
        AS bilirubin_max_event_id_0_24h,

      bilirubin_review_flag
        AS bilirubin_max_review_flag_0_24h

    FROM maximum_ranked

    WHERE bilirubin_max_rank = 1
  )

  SELECT
    counts.*,

    first_event.bilirubin_first_value_0_24h,
    first_event.bilirubin_first_time_0_24h,
    first_event.bilirubin_first_hours_from_ed,
    first_event.bilirubin_first_itemid_0_24h,
    first_event.bilirubin_first_event_id_0_24h,

    maximum_event.bilirubin_max_value_0_24h,
    maximum_event.bilirubin_max_time_0_24h,
    maximum_event.bilirubin_max_hours_from_ed,
    maximum_event.bilirubin_max_itemid_0_24h,
    maximum_event.bilirubin_max_event_id_0_24h,
    maximum_event.bilirubin_max_review_flag_0_24h,

    CASE
      WHEN maximum_event.bilirubin_max_value_0_24h IS NULL
        THEN NULL

      WHEN maximum_event.bilirubin_max_value_0_24h >= 12
        THEN 4

      WHEN maximum_event.bilirubin_max_value_0_24h >= 6
        THEN 3

      WHEN maximum_event.bilirubin_max_value_0_24h >= 2
        THEN 2

      WHEN maximum_event.bilirubin_max_value_0_24h >= 1.2
        THEN 1

      WHEN maximum_event.bilirubin_max_value_0_24h < 1.2
        THEN 0

      ELSE NULL
    END AS sofa_hepatic_structured_0_24h

  FROM episode_counts AS counts

  LEFT JOIN first_events AS first_event
    USING (episode_id)

  LEFT JOIN maximum_events AS maximum_event
    USING (episode_id);
  "
)

# ==============================================================================
# 31.3 HEPATIC SOFA VALIDATION
# ==============================================================================

hepatic_sofa_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS episode_with_bilirubin_event_n,

    COUNT(*) FILTER (
      WHERE sofa_hepatic_structured_0_24h IS NOT NULL
    ) AS episode_with_hepatic_score_n,

    COUNT(*) - COUNT(DISTINCT episode_id)
      AS duplicated_episode_n,

    COUNT(*) FILTER (
      WHERE sofa_hepatic_structured_0_24h
        NOT BETWEEN 0 AND 4
    ) AS invalid_score_n,

    COUNT(*) FILTER (
      WHERE sofa_hepatic_structured_0_24h IS NOT NULL
        AND (
          bilirubin_max_value_0_24h IS NULL
          OR NOT isfinite(bilirubin_max_value_0_24h)
          OR bilirubin_max_value_0_24h <= 0
          OR bilirubin_max_value_0_24h > 100
        )
    ) AS invalid_selected_bilirubin_n,

    COUNT(*) FILTER (
      WHERE bilirubin_n_valid_0_24h = 0
        AND sofa_hepatic_structured_0_24h IS NOT NULL
    ) AS score_without_valid_bilirubin_n,

    COUNT(*) FILTER (
      WHERE bilirubin_any_review_flag_0_24h
    ) AS episode_requiring_review_n

  FROM sofa_hepatic_episode;
  "
) %>%
  tibble::as_tibble()


if (
  hepatic_sofa_check$duplicated_episode_n != 0L ||
  hepatic_sofa_check$invalid_score_n != 0L ||
  hepatic_sofa_check$invalid_selected_bilirubin_n != 0L ||
  hepatic_sofa_check$score_without_valid_bilirubin_n != 0L
) {
  stop(
    "Hepatic SOFA validation failed.",
    call. = FALSE
  )
}

print(hepatic_sofa_check)

# ==============================================================================
# 32. CARDIOVASCULAR SOFA COMPONENT
# ==============================================================================


# ------------------------------------------------------------------------------
# 32.1 Emergency-department MAP events
# ------------------------------------------------------------------------------

sofa_ed_vitals_file_sql <- quote_sql_path(
  con,
  sofa_ed_vitals_file
)

DBI::dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP VIEW sofa_ed_vitals_source_raw AS

    SELECT *
    FROM read_csv_auto(
      %s,
      header = TRUE,
      all_varchar = TRUE,
      sample_size = -1,
      ignore_errors = FALSE
    );
    ",
    sofa_ed_vitals_file_sql
  )
)

assert_sofa_source_columns(
  "sofa_ed_vitals_source_raw",
  c(
    "subject_id",
    "stay_id",
    "charttime",
    "temperature",
    "heartrate",
    "resprate",
    "o2sat",
    "sbp",
    "dbp"
  )
)

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_ed_vitals_source AS

  SELECT
    CASE
      WHEN upper(trim(CAST(subject_id AS VARCHAR)))
        IN ('', 'NA', 'NAN', 'NULL')
      THEN NULL
      ELSE trim(CAST(subject_id AS VARCHAR))
    END AS subject_id,

    CASE
      WHEN upper(trim(CAST(stay_id AS VARCHAR)))
        IN ('', 'NA', 'NAN', 'NULL')
      THEN NULL
      ELSE trim(CAST(stay_id AS VARCHAR))
    END AS ed_stay_id,

    CASE
      WHEN length(trim(CAST(charttime AS VARCHAR))) >= 19
      THEN TRY_CAST(
        replace(
          trim(CAST(charttime AS VARCHAR)),
          'T',
          ' '
        ) AS TIMESTAMP
      )
    END AS charttime,

    TRY_CAST(temperature AS DOUBLE) AS temperature_raw,
    TRY_CAST(heartrate AS DOUBLE) AS heartrate,
    TRY_CAST(resprate AS DOUBLE) AS resprate,
    TRY_CAST(o2sat AS DOUBLE) AS o2sat,
    TRY_CAST(sbp AS DOUBLE) AS sbp,
    TRY_CAST(dbp AS DOUBLE) AS dbp,

    ROW_NUMBER() OVER () AS ed_vitals_source_row_id

  FROM sofa_ed_vitals_source_raw;
  "
)


# First valid ED value for each arrival-physiology variable. Temperature values
# above 45 are interpreted as Fahrenheit and converted to degrees Celsius.
DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE ed_arrival_vitals_episode AS

  WITH linked AS (
    SELECT
      cohort.episode_id,
      source.charttime,
      source.ed_vitals_source_row_id,

      CASE
        WHEN source.temperature_raw > 45
        THEN (source.temperature_raw - 32.0) * 5.0 / 9.0
        ELSE source.temperature_raw
      END AS temperature_c,

      source.heartrate,
      source.resprate,
      source.o2sat,

      CASE
        WHEN source.sbp IS NOT NULL
         AND source.dbp IS NOT NULL
         AND isfinite(source.sbp)
         AND isfinite(source.dbp)
         AND source.sbp > 0
         AND source.dbp > 0
         AND source.sbp >= source.dbp
        THEN (source.sbp + 2.0 * source.dbp) / 3.0
      END AS map_value

    FROM candidate_cohort_working_table AS cohort

    LEFT JOIN sofa_ed_vitals_source AS source
      ON CAST(cohort.stay_id AS VARCHAR) = source.ed_stay_id
     AND CAST(cohort.subject_id AS VARCHAR) = source.subject_id
     AND cohort.ed_intime IS NOT NULL
     AND source.charttime BETWEEN
           cohort.ed_intime
           AND cohort.ed_intime + INTERVAL '24 hours'
  )

  SELECT
    episode_id,

    first(temperature_c ORDER BY charttime, ed_vitals_source_row_id)
      FILTER (
        WHERE temperature_c BETWEEN 25 AND 45
          AND isfinite(temperature_c)
      ) AS ed_arrival_temperature_c,

    first(heartrate ORDER BY charttime, ed_vitals_source_row_id)
      FILTER (
        WHERE heartrate > 0 AND heartrate <= 300
          AND isfinite(heartrate)
      ) AS ed_arrival_heart_rate,

    first(resprate ORDER BY charttime, ed_vitals_source_row_id)
      FILTER (
        WHERE resprate > 0 AND resprate <= 80
          AND isfinite(resprate)
      ) AS ed_arrival_respiratory_rate,

    first(map_value ORDER BY charttime, ed_vitals_source_row_id)
      FILTER (
        WHERE map_value > 0 AND map_value <= 200
          AND isfinite(map_value)
      ) AS ed_arrival_map_mmhg,

    first(o2sat ORDER BY charttime, ed_vitals_source_row_id)
      FILTER (
        WHERE o2sat > 0 AND o2sat <= 100
          AND isfinite(o2sat)
      ) AS ed_arrival_spo2_percent

  FROM linked

  GROUP BY episode_id;
  "
)

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_ed_map_events_0_24h AS

  WITH linked_events AS (
    SELECT
      cohort.episode_id,
      CAST(cohort.subject_id AS VARCHAR)
        AS episode_subject_id,
      CAST(cohort.stay_id AS VARCHAR)
        AS ed_stay_id,

      cohort.ed_intime,
      source.charttime,
      source.sbp,
      source.dbp,
      source.ed_vitals_source_row_id,

      date_diff(
        'second',
        cohort.ed_intime,
        source.charttime
      ) / 3600.0 AS map_hours_from_ed,

      source.sbp IS NOT NULL
        AND source.dbp IS NOT NULL
        AND isfinite(source.sbp)
        AND isfinite(source.dbp)
        AND source.sbp > 0
        AND source.dbp > 0
        AND source.sbp >= source.dbp
        AS bp_pair_structurally_valid

    FROM candidate_cohort_working_table AS cohort

    INNER JOIN sofa_ed_vitals_source AS source
      ON CAST(cohort.stay_id AS VARCHAR) =
           source.ed_stay_id
     AND CAST(cohort.subject_id AS VARCHAR) =
           source.subject_id

    WHERE cohort.ed_intime IS NOT NULL
      AND source.charttime BETWEEN
        cohort.ed_intime
        AND cohort.ed_intime + INTERVAL '24 hours'
  ),

  calculated_events AS (
    SELECT
      *,

      CASE
        WHEN bp_pair_structurally_valid
        THEN (sbp + 2.0 * dbp) / 3.0
      END AS map_value

    FROM linked_events
  )

  SELECT
    episode_id,
    episode_subject_id,
    ed_stay_id,

    charttime AS event_time,
    map_hours_from_ed,

    concat(
      'ED_MAP_',
      ed_stay_id,
      '_',
      CAST(ed_vitals_source_row_id AS VARCHAR)
    ) AS map_event_id,

    'emergency_department'
      AS map_source_domain,

    'ed_calculated_from_sbp_dbp'
      AS map_source,

    ed_stay_id AS map_source_stay_id,

    sbp,
    dbp,

    CAST(NULL AS DOUBLE) AS mbp_original,
    CAST(NULL AS DOUBLE) AS mbp_ni_original,

    map_value,

    map_value IS NOT NULL
      AND isfinite(map_value)
      AND map_value > 0
      AND map_value <= 300
      AS map_structurally_valid,

    map_value IS NOT NULL
      AND (
        map_value < 20
        OR map_value > 200
      ) AS map_extreme_review_flag,

    map_value IS NOT NULL
      AND (
        map_value < 20
        OR map_value > 200
      ) AS map_review_flag

  FROM calculated_events;
  "
)


# ------------------------------------------------------------------------------
# 32.2 Intensive-care MAP events
# ------------------------------------------------------------------------------

sofa_icu_vitals_file_sql <- quote_sql_path(
  con,
  sofa_icu_vitals_file
)

DBI::dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP VIEW sofa_icu_vitals_source_raw AS

    SELECT *
    FROM read_csv_auto(
      %s,
      header = TRUE,
      all_varchar = TRUE,
      sample_size = -1,
      ignore_errors = FALSE
    );
    ",
    sofa_icu_vitals_file_sql
  )
)

assert_sofa_source_columns(
  "sofa_icu_vitals_source_raw",
  c(
    "subject_id",
    "stay_id",
    "charttime",
    "sbp",
    "dbp",
    "mbp",
    "mbp_ni"
  )
)

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_icu_vitals_source AS

  SELECT
    CASE
      WHEN upper(trim(CAST(subject_id AS VARCHAR)))
        IN ('', 'NA', 'NAN', 'NULL')
      THEN NULL
      ELSE trim(CAST(subject_id AS VARCHAR))
    END AS subject_id,

    CASE
      WHEN upper(trim(CAST(stay_id AS VARCHAR)))
        IN ('', 'NA', 'NAN', 'NULL')
      THEN NULL
      ELSE trim(CAST(stay_id AS VARCHAR))
    END AS icu_stay_id,

    CASE
      WHEN length(trim(CAST(charttime AS VARCHAR))) >= 19
      THEN TRY_CAST(
        replace(
          trim(CAST(charttime AS VARCHAR)),
          'T',
          ' '
        ) AS TIMESTAMP
      )
    END AS charttime,

    TRY_CAST(sbp AS DOUBLE) AS sbp,
    TRY_CAST(dbp AS DOUBLE) AS dbp,
    TRY_CAST(mbp AS DOUBLE) AS mbp,
    TRY_CAST(mbp_ni AS DOUBLE) AS mbp_ni,

    ROW_NUMBER() OVER () AS icu_vitals_source_row_id

  FROM sofa_icu_vitals_source_raw;
  "
)

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_icu_map_events_0_24h AS

  WITH linked_events AS (
    SELECT
      icu.episode_id,
      icu.episode_subject_id,
      icu.icu_stay_id,
      icu.ed_intime,

      source.subject_id AS source_subject_id,
      source.charttime,
      source.sbp,
      source.dbp,
      source.mbp,
      source.mbp_ni,
      source.icu_vitals_source_row_id,

      date_diff(
        'second',
        icu.ed_intime,
        source.charttime
      ) / 3600.0 AS map_hours_from_ed,

      source.subject_id IS NOT NULL
        AND source.subject_id !=
          CAST(icu.episode_subject_id AS VARCHAR)
        AS icu_vitals_subject_mismatch_flag,

      source.sbp IS NOT NULL
        AND source.dbp IS NOT NULL
        AND isfinite(source.sbp)
        AND isfinite(source.dbp)
        AND source.sbp > 0
        AND source.dbp > 0
        AND source.sbp >= source.dbp
        AS bp_pair_structurally_valid

    FROM sofa_icu_episode_map_candidate AS icu

    INNER JOIN sofa_icu_vitals_source AS source
      USING (icu_stay_id)

    WHERE source.charttime BETWEEN
      icu.ed_intime
      AND icu.ed_intime + INTERVAL '24 hours'
  ),

  selected_map_values AS (
    SELECT
      *,

      CASE
        WHEN mbp IS NOT NULL
          AND isfinite(mbp)
        THEN mbp

        WHEN mbp_ni IS NOT NULL
          AND isfinite(mbp_ni)
        THEN mbp_ni

        WHEN bp_pair_structurally_valid
        THEN (sbp + 2.0 * dbp) / 3.0
      END AS map_value,

      CASE
        WHEN mbp IS NOT NULL
          AND isfinite(mbp)
        THEN 'icu_derived_mbp'

        WHEN mbp_ni IS NOT NULL
          AND isfinite(mbp_ni)
        THEN 'icu_noninvasive_mbp_fallback'

        WHEN bp_pair_structurally_valid
        THEN 'icu_calculated_from_sbp_dbp_fallback'

        ELSE 'icu_map_unavailable'
      END AS map_source

    FROM linked_events
  )

  SELECT
    episode_id,
    episode_subject_id,

    icu_stay_id AS ed_stay_id,

    charttime AS event_time,
    map_hours_from_ed,

    concat(
      'ICU_MAP_',
      icu_stay_id,
      '_',
      CAST(icu_vitals_source_row_id AS VARCHAR)
    ) AS map_event_id,

    'intensive_care_unit'
      AS map_source_domain,

    map_source,

    icu_stay_id AS map_source_stay_id,

    sbp,
    dbp,

    mbp AS mbp_original,
    mbp_ni AS mbp_ni_original,

    map_value,

    map_value IS NOT NULL
      AND isfinite(map_value)
      AND map_value > 0
      AND map_value <= 300
      AS map_structurally_valid,

    map_value IS NOT NULL
      AND (
        map_value < 20
        OR map_value > 200
      ) AS map_extreme_review_flag,

    icu_vitals_subject_mismatch_flag
      OR (
        map_value IS NOT NULL
        AND (
          map_value < 20
          OR map_value > 200
        )
      )
      OR map_source =
        'icu_calculated_from_sbp_dbp_fallback'
      AS map_review_flag

  FROM selected_map_values;
  "
)

# ------------------------------------------------------------------------------
# 32.3 Combined MAP events and episode summary
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_map_events_0_24h AS

  SELECT *
  FROM sofa_ed_map_events_0_24h

  UNION ALL

  SELECT *
  FROM sofa_icu_map_events_0_24h;
  "
)

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_map_episode AS

  WITH episode_counts AS (
    SELECT
      episode_id,

      COUNT(*)::BIGINT
        AS map_n_all_0_24h,

      COUNT(*) FILTER (
        WHERE map_structurally_valid
      )::BIGINT AS map_n_valid_0_24h,

      COUNT(*) FILTER (
        WHERE map_source_domain =
          'emergency_department'
      )::BIGINT AS map_n_ed_0_24h,

      COUNT(*) FILTER (
        WHERE map_source_domain =
          'intensive_care_unit'
      )::BIGINT AS map_n_icu_0_24h,

      bool_or(map_review_flag)
        AS map_any_review_flag_0_24h,

      bool_or(
        map_structurally_valid
        AND map_value < 70
      ) AS map_any_below_70_0_24h

    FROM sofa_map_events_0_24h

    GROUP BY episode_id
  ),

  valid_events AS (
    SELECT
      *,

      ROW_NUMBER() OVER (
        PARTITION BY episode_id
        ORDER BY
          event_time,
          map_event_id
      ) AS map_first_rank,

      ROW_NUMBER() OVER (
        PARTITION BY episode_id
        ORDER BY
          map_value,
          event_time,
          map_event_id
      ) AS map_min_rank

    FROM sofa_map_events_0_24h

    WHERE map_structurally_valid
  ),

  first_events AS (
    SELECT
      episode_id,

      map_value
        AS map_first_value_0_24h,

      event_time
        AS map_first_time_0_24h,

      map_hours_from_ed
        AS map_first_hours_from_ed,

      map_source
        AS map_first_source_0_24h,

      map_source_domain
        AS map_first_source_domain_0_24h,

      map_event_id
        AS map_first_event_id_0_24h

    FROM valid_events

    WHERE map_first_rank = 1
  ),

  minimum_events AS (
    SELECT
      episode_id,

      map_value
        AS map_min_value_0_24h,

      event_time
        AS map_min_time_0_24h,

      map_hours_from_ed
        AS map_min_hours_from_ed,

      map_source
        AS map_min_source_0_24h,

      map_source_domain
        AS map_min_source_domain_0_24h,

      map_event_id
        AS map_min_event_id_0_24h,

      map_value < 70
        AS map_min_below_70_flag,

      map_review_flag
        AS map_min_review_flag

    FROM valid_events

    WHERE map_min_rank = 1
  )

  SELECT
    counts.*,

    first_event.* EXCLUDE (episode_id),

    minimum_event.* EXCLUDE (episode_id)

  FROM episode_counts AS counts

  LEFT JOIN first_events AS first_event
    USING (episode_id)

  LEFT JOIN minimum_events AS minimum_event
    USING (episode_id);
  "
)

# ------------------------------------------------------------------------------
# 32.4 Vasoactive medication intervals
# ------------------------------------------------------------------------------

sofa_vasoactive_file_sql <- quote_sql_path(
  con,
  sofa_vasoactive_file
)

DBI::dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP VIEW sofa_vasoactive_source_raw AS

    SELECT *
    FROM read_csv_auto(
      %s,
      header = TRUE,
      all_varchar = TRUE,
      sample_size = -1,
      ignore_errors = FALSE
    );
    ",
    sofa_vasoactive_file_sql
  )
)

assert_sofa_source_columns(
  "sofa_vasoactive_source_raw",
  c(
    "stay_id",
    "starttime",
    "endtime",
    "dopamine",
    "epinephrine",
    "norepinephrine",
    "phenylephrine",
    "vasopressin",
    "dobutamine",
    "milrinone"
  )
)

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_vasoactive_source AS

  WITH parsed AS (
    SELECT
      CASE
        WHEN upper(trim(CAST(stay_id AS VARCHAR)))
          IN ('', 'NA', 'NAN', 'NULL')
        THEN NULL
        ELSE trim(CAST(stay_id AS VARCHAR))
      END AS icu_stay_id,

      CASE
        WHEN length(trim(CAST(starttime AS VARCHAR))) >= 19
        THEN TRY_CAST(
          replace(
            trim(CAST(starttime AS VARCHAR)),
            'T',
            ' '
          ) AS TIMESTAMP
        )
      END AS starttime,

      CASE
        WHEN length(trim(CAST(endtime AS VARCHAR))) >= 19
        THEN TRY_CAST(
          replace(
            trim(CAST(endtime AS VARCHAR)),
            'T',
            ' '
          ) AS TIMESTAMP
        )
      END AS endtime,

      TRY_CAST(dopamine AS DOUBLE) AS dopamine,
      TRY_CAST(epinephrine AS DOUBLE) AS epinephrine,
      TRY_CAST(norepinephrine AS DOUBLE) AS norepinephrine,
      TRY_CAST(phenylephrine AS DOUBLE) AS phenylephrine,
      TRY_CAST(vasopressin AS DOUBLE) AS vasopressin,
      TRY_CAST(dobutamine AS DOUBLE) AS dobutamine,
      TRY_CAST(milrinone AS DOUBLE) AS milrinone,

      ROW_NUMBER() OVER () AS vasoactive_source_row_id

    FROM sofa_vasoactive_source_raw
  )

  SELECT
    *,

    icu_stay_id IS NOT NULL
      AND starttime IS NOT NULL
      AND endtime IS NOT NULL
      AND endtime > starttime
      AS vaso_interval_valid_flag,

    dopamine IS NULL
      AND epinephrine IS NULL
      AND norepinephrine IS NULL
      AND phenylephrine IS NULL
      AND vasopressin IS NULL
      AND dobutamine IS NULL
      AND milrinone IS NULL
      AS vaso_all_agents_missing_flag,

    coalesce(dopamine < 0, FALSE)
      OR coalesce(epinephrine < 0, FALSE)
      OR coalesce(norepinephrine < 0, FALSE)
      OR coalesce(phenylephrine < 0, FALSE)
      OR coalesce(vasopressin < 0, FALSE)
      OR coalesce(dobutamine < 0, FALSE)
      OR coalesce(milrinone < 0, FALSE)
      AS vaso_any_negative_rate_flag,

    concat(
      'VASO_',
      coalesce(icu_stay_id, 'missing'),
      '_',
      CAST(vasoactive_source_row_id AS VARCHAR)
    ) AS vasoactive_event_id

  FROM parsed;
  "
)

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_vasoactive_events_0_24h AS

  WITH linked_intervals AS (
    SELECT
      icu.episode_id,
      icu.episode_subject_id,
      icu.icu_stay_id,
      icu.ed_intime,

      source.vasoactive_event_id,
      source.starttime,
      source.endtime,

      source.dopamine,
      source.epinephrine,
      source.norepinephrine,
      source.phenylephrine,
      source.vasopressin,
      source.dobutamine,
      source.milrinone,

      source.vaso_all_agents_missing_flag,
      source.vaso_any_negative_rate_flag,

      greatest(
        source.starttime,
        icu.ed_intime
      ) AS vaso_overlap_start,

      least(
        source.endtime,
        icu.ed_intime + INTERVAL '24 hours'
      ) AS vaso_overlap_end

    FROM sofa_icu_episode_map_candidate AS icu

    INNER JOIN sofa_vasoactive_source AS source
      USING (icu_stay_id)

    WHERE source.vaso_interval_valid_flag
      AND source.starttime <
        icu.ed_intime + INTERVAL '24 hours'
      AND source.endtime > icu.ed_intime
  )

  SELECT
    *,

    date_diff(
      'second',
      vaso_overlap_start,
      vaso_overlap_end
    ) / 3600.0 AS vaso_overlap_duration_hours,

    date_diff(
      'second',
      ed_intime,
      vaso_overlap_start
    ) / 3600.0 AS vaso_start_hours_from_ed,

    coalesce(
      isfinite(dopamine) AND dopamine > 0,
      FALSE
    ) AS dopamine_active_flag,

    coalesce(
      isfinite(epinephrine) AND epinephrine > 0,
      FALSE
    ) AS epinephrine_active_flag,

    coalesce(
      isfinite(norepinephrine) AND norepinephrine > 0,
      FALSE
    ) AS norepinephrine_active_flag,

    coalesce(
      isfinite(phenylephrine) AND phenylephrine > 0,
      FALSE
    ) AS phenylephrine_active_flag,

    coalesce(
      isfinite(vasopressin) AND vasopressin > 0,
      FALSE
    ) AS vasopressin_active_flag,

    coalesce(
      isfinite(dobutamine) AND dobutamine > 0,
      FALSE
    ) AS dobutamine_active_flag,

    coalesce(
      isfinite(milrinone) AND milrinone > 0,
      FALSE
    ) AS milrinone_active_flag,

    coalesce(
      isfinite(dopamine) AND dopamine > 0,
      FALSE
    )
      OR coalesce(
        isfinite(epinephrine) AND epinephrine > 0,
        FALSE
      )
      OR coalesce(
        isfinite(norepinephrine) AND norepinephrine > 0,
        FALSE
      )
      OR coalesce(
        isfinite(dobutamine) AND dobutamine > 0,
        FALSE
      ) AS classic_sofa_agent_active_flag,

    coalesce(
      isfinite(dopamine) AND dopamine > 0,
      FALSE
    )
      OR coalesce(
        isfinite(epinephrine) AND epinephrine > 0,
        FALSE
      )
      OR coalesce(
        isfinite(norepinephrine) AND norepinephrine > 0,
        FALSE
      )
      OR coalesce(
        isfinite(phenylephrine) AND phenylephrine > 0,
        FALSE
      )
      OR coalesce(
        isfinite(vasopressin) AND vasopressin > 0,
        FALSE
      )
      OR coalesce(
        isfinite(dobutamine) AND dobutamine > 0,
        FALSE
      )
      OR coalesce(
        isfinite(milrinone) AND milrinone > 0,
        FALSE
      ) AS any_vasoactive_agent_active_flag,

    coalesce(
      isfinite(dopamine) AND dopamine > 0,
      FALSE
    )
      OR coalesce(
        isfinite(epinephrine) AND epinephrine > 0,
        FALSE
      )
      OR coalesce(
        isfinite(norepinephrine) AND norepinephrine > 0,
        FALSE
      )
      OR coalesce(
        isfinite(phenylephrine) AND phenylephrine > 0,
        FALSE
      )
      OR coalesce(
        isfinite(vasopressin) AND vasopressin > 0,
        FALSE
      ) AS shock_vasopressor_active_flag

  FROM linked_intervals;
  "
)

# ------------------------------------------------------------------------------
# 32.5 Vasoactive episode summary
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_vasoactive_episode AS

  SELECT
    episode_id,

    COUNT(*)::BIGINT
      AS vasoactive_n_intervals_0_24h,

    COUNT(*) FILTER (
      WHERE any_vasoactive_agent_active_flag
    )::BIGINT AS vasoactive_n_active_intervals_0_24h,

    MIN(vaso_overlap_start) FILTER (
      WHERE any_vasoactive_agent_active_flag
    ) AS vasoactive_first_any_start_0_24h,

    MIN(vaso_overlap_start) FILTER (
      WHERE shock_vasopressor_active_flag
    ) AS vasopressor_first_start_0_24h,

    MAX(dopamine) FILTER (
      WHERE dopamine > 0 AND isfinite(dopamine)
    ) AS dopamine_max_0_24h,

    MAX(epinephrine) FILTER (
      WHERE epinephrine > 0 AND isfinite(epinephrine)
    ) AS epinephrine_max_0_24h,

    MAX(norepinephrine) FILTER (
      WHERE norepinephrine > 0
        AND isfinite(norepinephrine)
    ) AS norepinephrine_max_0_24h,

    MAX(phenylephrine) FILTER (
      WHERE phenylephrine > 0
        AND isfinite(phenylephrine)
    ) AS phenylephrine_max_0_24h,

    MAX(vasopressin) FILTER (
      WHERE vasopressin > 0
        AND isfinite(vasopressin)
    ) AS vasopressin_max_0_24h,

    MAX(dobutamine) FILTER (
      WHERE dobutamine > 0
        AND isfinite(dobutamine)
    ) AS dobutamine_max_0_24h,

    MAX(milrinone) FILTER (
      WHERE milrinone > 0
        AND isfinite(milrinone)
    ) AS milrinone_max_0_24h,

    bool_or(classic_sofa_agent_active_flag)
      AS any_classic_sofa_agent_0_24h,

    bool_or(any_vasoactive_agent_active_flag)
      AS any_vasoactive_agent_0_24h,

    bool_or(shock_vasopressor_active_flag)
      AS any_shock_vasopressor_0_24h,

    bool_or(vaso_any_negative_rate_flag)
      AS vasoactive_any_negative_rate_0_24h

  FROM sofa_vasoactive_events_0_24h

  GROUP BY episode_id;
  "
)

# ------------------------------------------------------------------------------
# 32.6 Cardiovascular SOFA score
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_cardiovascular_episode AS

  WITH merged_sources AS (
    SELECT
      cohort.episode_id,

      map.* EXCLUDE (episode_id),

      vaso.vasoactive_n_intervals_0_24h,
      vaso.vasoactive_n_active_intervals_0_24h,
      vaso.vasoactive_first_any_start_0_24h,
      vaso.vasopressor_first_start_0_24h,

      vaso.dopamine_max_0_24h,
      vaso.epinephrine_max_0_24h,
      vaso.norepinephrine_max_0_24h,
      vaso.phenylephrine_max_0_24h,
      vaso.vasopressin_max_0_24h,
      vaso.dobutamine_max_0_24h,
      vaso.milrinone_max_0_24h,

      coalesce(
        vaso.any_classic_sofa_agent_0_24h,
        FALSE
      ) AS any_classic_sofa_agent_0_24h,

      coalesce(
        vaso.any_vasoactive_agent_0_24h,
        FALSE
      ) AS any_vasoactive_agent_0_24h,

      coalesce(
        vaso.any_shock_vasopressor_0_24h,
        FALSE
      ) AS any_shock_vasopressor_0_24h,

      coalesce(
        vaso.vasoactive_any_negative_rate_0_24h,
        FALSE
      ) AS vasoactive_any_negative_rate_0_24h

    FROM sofa_candidate_episode_ids AS cohort

    LEFT JOIN sofa_map_episode AS map
      USING (episode_id)

    LEFT JOIN sofa_vasoactive_episode AS vaso
      USING (episode_id)
  ),

  availability AS (
    SELECT
      *,

      map_min_value_0_24h IS NOT NULL
        OR any_classic_sofa_agent_0_24h
        AS cardiovascular_data_available_flag

    FROM merged_sources
  ),

  scored AS (
    SELECT
      *,

      CASE
        WHEN dopamine_max_0_24h > 15
          OR epinephrine_max_0_24h > 0.1
          OR norepinephrine_max_0_24h > 0.1
        THEN 4

        WHEN dopamine_max_0_24h > 5
          OR (
            epinephrine_max_0_24h > 0
            AND epinephrine_max_0_24h <= 0.1
          )
          OR (
            norepinephrine_max_0_24h > 0
            AND norepinephrine_max_0_24h <= 0.1
          )
        THEN 3

        WHEN dopamine_max_0_24h > 0
          OR dobutamine_max_0_24h > 0
        THEN 2

        WHEN map_min_value_0_24h < 70
        THEN 1

        WHEN cardiovascular_data_available_flag
        THEN 0
      END AS sofa_cardiovascular_structured_0_24h

    FROM availability
  )

  SELECT
    *,

    CASE sofa_cardiovascular_structured_0_24h
      WHEN 4 THEN
        'high_dose_dopamine_epinephrine_or_norepinephrine'
      WHEN 3 THEN
        'moderate_dopamine_or_low_dose_epinephrine_norepinephrine'
      WHEN 2 THEN
        'dopamine_or_dobutamine'
      WHEN 1 THEN
        'map_below_70'
      WHEN 0 THEN
        'no_cardiovascular_sofa_criterion'
    END AS sofa_cardiovascular_determining_factor,

    any_shock_vasopressor_0_24h
      AS shock_vasopressor_evidence_0_24h,

    coalesce(
      map_any_review_flag_0_24h,
      FALSE
    )
      OR vasoactive_any_negative_rate_0_24h
      AS cardiovascular_review_flag

  FROM scored;
  "
)

# ==============================================================================
# 32.7 CARDIOVASCULAR SOFA VALIDATION
# ==============================================================================

cardiovascular_sofa_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS episode_n,

    COUNT(*) FILTER (
      WHERE sofa_cardiovascular_structured_0_24h
        IS NOT NULL
    ) AS episode_with_cardiovascular_score_n,

    COUNT(*) FILTER (
      WHERE map_min_value_0_24h IS NOT NULL
    ) AS episode_with_valid_map_n,

    COUNT(*) FILTER (
      WHERE any_classic_sofa_agent_0_24h
    ) AS episode_with_classic_sofa_agent_n,

    COUNT(*) - COUNT(DISTINCT episode_id)
      AS duplicated_episode_n,

    COUNT(*) FILTER (
      WHERE sofa_cardiovascular_structured_0_24h
        NOT BETWEEN 0 AND 4
    ) AS invalid_score_n,

    COUNT(*) FILTER (
      WHERE sofa_cardiovascular_structured_0_24h IS NOT NULL
        AND NOT cardiovascular_data_available_flag
    ) AS score_without_cardiovascular_data_n,

    COUNT(*) FILTER (
      WHERE sofa_cardiovascular_structured_0_24h = 1
        AND (
          map_min_value_0_24h IS NULL
          OR map_min_value_0_24h >= 70
        )
    ) AS score_one_without_low_map_n,

    COUNT(*) FILTER (
      WHERE sofa_cardiovascular_structured_0_24h >= 2
        AND NOT any_classic_sofa_agent_0_24h
    ) AS high_score_without_classic_agent_n,

    COUNT(*) FILTER (
      WHERE cardiovascular_review_flag
    ) AS episode_requiring_review_n

  FROM sofa_cardiovascular_episode;
  "
) %>%
  tibble::as_tibble()

if (
  cardiovascular_sofa_check$episode_n !=   sofa_candidate_episode_check$episode_n ||
  cardiovascular_sofa_check$duplicated_episode_n != 0L ||
  cardiovascular_sofa_check$invalid_score_n != 0L ||
  cardiovascular_sofa_check$score_without_cardiovascular_data_n != 0L ||
  cardiovascular_sofa_check$score_one_without_low_map_n != 0L ||
  cardiovascular_sofa_check$high_score_without_classic_agent_n != 0L
) {
  stop(
    "Cardiovascular SOFA validation failed.",
    call. = FALSE
  )
}

print(cardiovascular_sofa_check)

# ==============================================================================
# 33. NEUROLOGIC SOFA COMPONENT
# ==============================================================================


# ------------------------------------------------------------------------------
# 33.1 Timed GCS source
# ------------------------------------------------------------------------------

sofa_gcs_file_sql <- quote_sql_path(
  con,
  sofa_gcs_file
)

DBI::dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP VIEW sofa_gcs_source_raw AS

    SELECT *
    FROM read_csv_auto(
      %s,
      header = TRUE,
      all_varchar = TRUE,
      sample_size = -1,
      ignore_errors = FALSE
    );
    ",
    sofa_gcs_file_sql
  )
)

assert_sofa_source_columns(
  "sofa_gcs_source_raw",
  c(
    "subject_id",
    "stay_id",
    "charttime",
    "gcs",
    "gcs_motor",
    "gcs_verbal",
    "gcs_eyes",
    "gcs_unable"
  )
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_gcs_source AS

  SELECT
    CASE
      WHEN upper(trim(CAST(subject_id AS VARCHAR)))
        IN ('', 'NA', 'NAN', 'NULL')
      THEN NULL
      ELSE trim(CAST(subject_id AS VARCHAR))
    END AS subject_id,

    CASE
      WHEN upper(trim(CAST(stay_id AS VARCHAR)))
        IN ('', 'NA', 'NAN', 'NULL')
      THEN NULL
      ELSE trim(CAST(stay_id AS VARCHAR))
    END AS icu_stay_id,

    CASE
      WHEN length(trim(CAST(charttime AS VARCHAR))) >= 19
      THEN TRY_CAST(
        replace(
          trim(CAST(charttime AS VARCHAR)),
          'T',
          ' '
        ) AS TIMESTAMP
      )
    END AS charttime,

    TRY_CAST(gcs AS INTEGER)
      AS gcs,

    TRY_CAST(gcs_motor AS INTEGER)
      AS gcs_motor,

    TRY_CAST(gcs_verbal AS INTEGER)
      AS gcs_verbal,

    TRY_CAST(gcs_eyes AS INTEGER)
      AS gcs_eyes,

    TRY_CAST(gcs_unable AS INTEGER)
      AS gcs_unable,

    ROW_NUMBER() OVER ()
      AS gcs_source_row_id

  FROM sofa_gcs_source_raw;
  "
)

# ------------------------------------------------------------------------------
# 33.2 GCS events in the 0-24 h window
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_gcs_events_0_24h AS

  WITH linked_events AS (
    SELECT
      icu.episode_id,
      icu.episode_subject_id,
      icu.icu_stay_id,
      icu.ed_intime,

      source.subject_id AS gcs_subject_id,
      source.charttime,
      source.gcs,
      source.gcs_motor,
      source.gcs_verbal,
      source.gcs_eyes,
      source.gcs_unable,
      source.gcs_source_row_id,

      date_diff(
        'second',
        icu.ed_intime,
        source.charttime
      ) / 3600.0 AS gcs_hours_from_ed,

      source.subject_id IS NOT NULL
        AND source.subject_id !=
          CAST(icu.episode_subject_id AS VARCHAR)
        AS gcs_subject_mismatch_flag

    FROM sofa_icu_episode_map_candidate AS icu

    INNER JOIN sofa_gcs_source AS source
      USING (icu_stay_id)

    WHERE source.charttime BETWEEN
      icu.ed_intime
      AND icu.ed_intime + INTERVAL '24 hours'
  )

  SELECT
    *,

    gcs IS NOT NULL
      AND gcs BETWEEN 3 AND 15
      AS gcs_structurally_valid,

    gcs_unable = 1
      AS gcs_unable_flag,

    gcs_motor IS NULL
      OR gcs_verbal IS NULL
      OR gcs_eyes IS NULL
      AS gcs_component_missing_flag,

    gcs_subject_mismatch_flag
      OR NOT (
        gcs IS NOT NULL
        AND gcs BETWEEN 3 AND 15
      )
      OR coalesce(gcs_unable = 1, FALSE)
      OR gcs_motor IS NULL
      OR gcs_verbal IS NULL
      OR gcs_eyes IS NULL
      AS gcs_review_flag,

    concat(
      'GCS_',
      icu_stay_id,
      '_',
      CAST(gcs_source_row_id AS VARCHAR)
    ) AS gcs_event_id

  FROM linked_events;
  "
)

# ------------------------------------------------------------------------------
# 33.3 First and minimum valid GCS per episode
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_neurologic_episode AS

  WITH episode_counts AS (
    SELECT
      episode_id,

      COUNT(*)::BIGINT
        AS gcs_n_all_0_24h,

      COUNT(*) FILTER (
        WHERE gcs_structurally_valid
      )::BIGINT AS gcs_n_valid_0_24h,

      bool_or(
        coalesce(gcs_unable_flag, FALSE)
      ) AS gcs_any_endotracheal_flag_0_24h,

      bool_or(
        gcs_component_missing_flag
      ) AS gcs_any_component_missing_0_24h,

      bool_or(
        gcs_review_flag
      ) AS gcs_any_review_flag_0_24h

    FROM sofa_gcs_events_0_24h

    GROUP BY episode_id
  ),

  valid_events AS (
    SELECT
      *,

      ROW_NUMBER() OVER (
        PARTITION BY episode_id
        ORDER BY
          charttime,
          gcs_event_id
      ) AS gcs_first_rank,

      ROW_NUMBER() OVER (
        PARTITION BY episode_id
        ORDER BY
          gcs,
          charttime,
          gcs_event_id
      ) AS gcs_min_rank

    FROM sofa_gcs_events_0_24h

    WHERE gcs_structurally_valid
  ),

  first_events AS (
    SELECT
      episode_id,

      gcs
        AS gcs_first_value_0_24h,

      charttime
        AS gcs_first_time_0_24h,

      gcs_hours_from_ed
        AS gcs_first_hours_from_ed,

      icu_stay_id
        AS gcs_first_icu_stay_id_0_24h,

      gcs_event_id
        AS gcs_first_event_id_0_24h,

      gcs_motor
        AS gcs_first_motor_0_24h,

      gcs_verbal
        AS gcs_first_verbal_0_24h,

      gcs_eyes
        AS gcs_first_eyes_0_24h,

      gcs_unable_flag
        AS gcs_first_endotracheal_flag_0_24h

    FROM valid_events

    WHERE gcs_first_rank = 1
  ),

  minimum_events AS (
    SELECT
      episode_id,

      gcs
        AS gcs_min_value_0_24h,

      charttime
        AS gcs_min_time_0_24h,

      gcs_hours_from_ed
        AS gcs_min_hours_from_ed,

      icu_stay_id
        AS gcs_min_icu_stay_id_0_24h,

      gcs_event_id
        AS gcs_min_event_id_0_24h,

      gcs_motor
        AS gcs_min_motor_0_24h,

      gcs_verbal
        AS gcs_min_verbal_0_24h,

      gcs_eyes
        AS gcs_min_eyes_0_24h,

      gcs_unable_flag
        AS gcs_min_endotracheal_flag_0_24h,

      gcs_component_missing_flag
        AS gcs_min_component_missing_flag_0_24h,

      gcs_review_flag
        AS gcs_min_review_flag_0_24h

    FROM valid_events

    WHERE gcs_min_rank = 1
  ),

  merged_events AS (
    SELECT
      counts.*,

      first_event.* EXCLUDE (episode_id),

      minimum_event.* EXCLUDE (episode_id)

    FROM episode_counts AS counts

    LEFT JOIN first_events AS first_event
      USING (episode_id)

    LEFT JOIN minimum_events AS minimum_event
      USING (episode_id)
  ),

  scored AS (
    SELECT
      *,

      CASE
        WHEN gcs_min_value_0_24h IS NULL
          THEN NULL
        WHEN gcs_min_value_0_24h = 15
          THEN 0
        WHEN gcs_min_value_0_24h >= 13
          THEN 1
        WHEN gcs_min_value_0_24h >= 10
          THEN 2
        WHEN gcs_min_value_0_24h >= 6
          THEN 3
        WHEN gcs_min_value_0_24h < 6
          THEN 4
      END AS sofa_neurologic_structured_0_24h

    FROM merged_events
  ),

  reviewed AS (
    SELECT
      *,

      coalesce(
        gcs_min_endotracheal_flag_0_24h,
        FALSE
      )
        OR coalesce(
          gcs_min_component_missing_flag_0_24h,
          FALSE
        )
        OR coalesce(
          gcs_min_review_flag_0_24h,
          FALSE
        ) AS sofa_neurologic_manual_review_required

    FROM scored
  )

  SELECT
    *,

    sofa_neurologic_manual_review_required
      AS sofa_neurologic_review_required,

    CASE
      WHEN coalesce(
        gcs_min_endotracheal_flag_0_24h,
        FALSE
      )
        AND coalesce(
          gcs_min_component_missing_flag_0_24h,
          FALSE
        )
      THEN
        'selected_minimum_has_ett_flag_and_missing_component'

      WHEN coalesce(
        gcs_min_endotracheal_flag_0_24h,
        FALSE
      )
      THEN
        'selected_minimum_has_ett_flag'

      WHEN coalesce(
        gcs_min_component_missing_flag_0_24h,
        FALSE
      )
      THEN
        'selected_minimum_has_missing_component'

      WHEN coalesce(
        gcs_min_review_flag_0_24h,
        FALSE
      )
      THEN
        'selected_minimum_other_review_flag'
    END AS sofa_neurologic_review_reason

  FROM reviewed;
  "
)

# ==============================================================================
# 33.4 NEUROLOGIC SOFA VALIDATION
# ==============================================================================

neurologic_sofa_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS episode_with_gcs_event_n,

    COUNT(*) FILTER (
      WHERE sofa_neurologic_structured_0_24h
        IS NOT NULL
    ) AS episode_with_neurologic_score_n,

    COUNT(*) - COUNT(DISTINCT episode_id)
      AS duplicated_episode_n,

    COUNT(*) FILTER (
      WHERE sofa_neurologic_structured_0_24h
        NOT BETWEEN 0 AND 4
    ) AS invalid_score_n,

    COUNT(*) FILTER (
      WHERE sofa_neurologic_structured_0_24h
        IS NOT NULL
        AND (
          gcs_min_value_0_24h IS NULL
          OR gcs_min_value_0_24h NOT BETWEEN 3 AND 15
        )
    ) AS invalid_selected_gcs_n,

    COUNT(*) FILTER (
      WHERE gcs_n_valid_0_24h = 0
        AND sofa_neurologic_structured_0_24h
          IS NOT NULL
    ) AS score_without_valid_gcs_n,

    COUNT(*) FILTER (
      WHERE sofa_neurologic_manual_review_required
    ) AS episode_requiring_review_n,

    COUNT(*) FILTER (
      WHERE gcs_min_endotracheal_flag_0_24h
    ) AS selected_minimum_with_ett_flag_n,

    COUNT(*) FILTER (
      WHERE gcs_min_component_missing_flag_0_24h
    ) AS selected_minimum_with_missing_component_n

  FROM sofa_neurologic_episode;
  "
) %>%
  tibble::as_tibble()


if (
  neurologic_sofa_check$duplicated_episode_n != 0L ||
  neurologic_sofa_check$invalid_score_n != 0L ||
  neurologic_sofa_check$invalid_selected_gcs_n != 0L ||
  neurologic_sofa_check$score_without_valid_gcs_n != 0L
) {
  stop(
    "Neurologic SOFA validation failed.",
    call. = FALSE
  )
}

print(neurologic_sofa_check)

# ==============================================================================
# 34. RENAL SOFA COMPONENT
# ==============================================================================


# ------------------------------------------------------------------------------
# 34.1 Creatinine events
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_creatinine_events_0_24h AS

  SELECT
    episode_id,
    labevent_id,
    specimen_id,
    itemid,
    charttime,
    sofa_lab_hours_from_ed,

    valuenum AS creatinine_value_mg_dl,

    valueuom_original
      AS creatinine_unit_original,

    valueuom_normalized
      AS creatinine_unit_normalized,

    valuenum IS NOT NULL
      AND isfinite(valuenum)
      AND valuenum > 0
      AND valuenum <= 50
      AND valueuom_normalized = 'mg/dl'
      AS creatinine_valid_flag,

    valuenum IS NOT NULL
      AND (
        valuenum > 20
        OR coalesce(valueuom_normalized, '') != 'mg/dl'
      ) AS creatinine_review_flag,

    concat(
      'CREAT_',
      labevent_id
    ) AS creatinine_event_id

  FROM sofa_labevents_candidate_0_24h

  WHERE sofa_lab_concept = 'creatinine';
  "
)

# ------------------------------------------------------------------------------
# 34.2 Creatinine episode summary
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_creatinine_episode AS

  WITH episode_counts AS (
    SELECT
      episode_id,

      COUNT(*)::BIGINT
        AS creatinine_n_all_0_24h,

      COUNT(*) FILTER (
        WHERE creatinine_valid_flag
      )::BIGINT AS creatinine_n_valid_0_24h,

      bool_or(creatinine_review_flag)
        AS creatinine_any_review_flag_0_24h

    FROM sofa_creatinine_events_0_24h

    GROUP BY episode_id
  ),

  valid_events AS (
    SELECT
      *,

      ROW_NUMBER() OVER (
        PARTITION BY episode_id
        ORDER BY
          charttime,
          creatinine_event_id
      ) AS creatinine_first_rank,

      ROW_NUMBER() OVER (
        PARTITION BY episode_id
        ORDER BY
          creatinine_value_mg_dl DESC,
          charttime,
          creatinine_event_id
      ) AS creatinine_max_rank

    FROM sofa_creatinine_events_0_24h

    WHERE creatinine_valid_flag
  ),

  first_events AS (
    SELECT
      episode_id,

      creatinine_value_mg_dl
        AS creatinine_first_value_0_24h,

      charttime
        AS creatinine_first_time_0_24h,

      sofa_lab_hours_from_ed
        AS creatinine_first_hours_from_ed,

      itemid
        AS creatinine_first_itemid_0_24h,

      creatinine_event_id
        AS creatinine_first_event_id_0_24h

    FROM valid_events

    WHERE creatinine_first_rank = 1
  ),

  maximum_events AS (
    SELECT
      episode_id,

      creatinine_value_mg_dl
        AS creatinine_max_value_0_24h,

      charttime
        AS creatinine_max_time_0_24h,

      sofa_lab_hours_from_ed
        AS creatinine_max_hours_from_ed,

      itemid
        AS creatinine_max_itemid_0_24h,

      creatinine_event_id
        AS creatinine_max_event_id_0_24h,

      creatinine_review_flag
        AS creatinine_max_review_flag_0_24h

    FROM valid_events

    WHERE creatinine_max_rank = 1
  ),

  merged_events AS (
    SELECT
      counts.*,

      first_event.* EXCLUDE (episode_id),

      maximum_event.* EXCLUDE (episode_id)

    FROM episode_counts AS counts

    LEFT JOIN first_events AS first_event
      USING (episode_id)

    LEFT JOIN maximum_events AS maximum_event
      USING (episode_id)
  )

  SELECT
    *,

    CASE
      WHEN creatinine_max_value_0_24h IS NULL
        THEN NULL
      WHEN creatinine_max_value_0_24h >= 5.0
        THEN 4
      WHEN creatinine_max_value_0_24h >= 3.5
        THEN 3
      WHEN creatinine_max_value_0_24h >= 2.0
        THEN 2
      WHEN creatinine_max_value_0_24h >= 1.2
        THEN 1
      WHEN creatinine_max_value_0_24h < 1.2
        THEN 0
    END AS sofa_renal_creatinine_structured_0_24h

  FROM merged_events;
  "
)

# ------------------------------------------------------------------------------
# 34.3 Reliable 24-hour urine output
# ------------------------------------------------------------------------------

sofa_urine_file_sql <- quote_sql_path(
  con,
  sofa_urine_file
)

DBI::dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP VIEW sofa_urine_source_raw AS

    SELECT *
    FROM read_csv_auto(
      %s,
      header = TRUE,
      all_varchar = TRUE,
      sample_size = -1,
      ignore_errors = FALSE
    );
    ",
    sofa_urine_file_sql
  )
)

assert_sofa_source_columns(
  "sofa_urine_source_raw",
  c(
    "stay_id",
    "charttime",
    "uo",
    "urineoutput_24hr",
    "uo_tm_24hr"
  )
)

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_urine_source AS

  SELECT
    CASE
      WHEN upper(trim(CAST(stay_id AS VARCHAR)))
        IN ('', 'NA', 'NAN', 'NULL')
      THEN NULL
      ELSE trim(CAST(stay_id AS VARCHAR))
    END AS icu_stay_id,

    CASE
      WHEN length(trim(CAST(charttime AS VARCHAR))) >= 19
      THEN TRY_CAST(
        replace(
          trim(CAST(charttime AS VARCHAR)),
          'T',
          ' '
        ) AS TIMESTAMP
      )
    END AS charttime,

    TRY_CAST(uo AS DOUBLE)
      AS urineoutput_current_ml,

    TRY_CAST(urineoutput_24hr AS DOUBLE)
      AS urineoutput_24hr_ml,

    TRY_CAST(uo_tm_24hr AS DOUBLE)
      AS urine_observed_hours_24hr,

    ROW_NUMBER() OVER ()
      AS urine_source_row_id

  FROM sofa_urine_source_raw;
  "
)

# ------------------------------------------------------------------------------
# 34.4 Urine-output events in the 0-24 h window
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_urine_events_0_24h AS

  WITH linked_events AS (
    SELECT
      icu.episode_id,
      icu.icu_stay_id,
      icu.ed_intime,

      source.charttime,
      source.urineoutput_current_ml,
      source.urineoutput_24hr_ml,
      source.urine_observed_hours_24hr,
      source.urine_source_row_id,

      date_diff(
        'second',
        icu.ed_intime,
        source.charttime
      ) / 3600.0 AS urine_hours_from_ed

    FROM sofa_icu_episode_map_candidate AS icu

    INNER JOIN sofa_urine_source AS source
      USING (icu_stay_id)

    WHERE source.charttime BETWEEN
      icu.ed_intime
      AND icu.ed_intime + INTERVAL '24 hours'
  ),

  window_assessment AS (
    SELECT
      *,

      urine_hours_from_ed -
        urine_observed_hours_24hr
        AS urine_window_start_hours_from_ed

    FROM linked_events
  )

  SELECT
    *,

    urineoutput_24hr_ml IS NOT NULL
      AND isfinite(urineoutput_24hr_ml)
      AND urineoutput_24hr_ml >= 0

      AND urine_observed_hours_24hr IS NOT NULL
      AND isfinite(urine_observed_hours_24hr)
      AND urine_observed_hours_24hr >= 22
      AND urine_observed_hours_24hr <= 24.5

      AND urine_window_start_hours_from_ed >= -0.000001
      AS urine_24h_reliable_flag,

    urineoutput_24hr_ml IS NOT NULL
      AND urineoutput_24hr_ml < 0
      AS urine_negative_flag,

    concat(
      'URINE_',
      icu_stay_id,
      '_',
      CAST(urine_source_row_id AS VARCHAR)
    ) AS urine_event_id

  FROM window_assessment;
  "
)

# ------------------------------------------------------------------------------
# 34.5 Urine-output episode summary
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_urine_episode AS

  WITH episode_counts AS (
    SELECT
      episode_id,

      COUNT(*)::BIGINT
        AS urine_n_rows_0_24h,

      COUNT(*) FILTER (
        WHERE urine_24h_reliable_flag
      )::BIGINT AS urine_n_reliable_24h_rows,

      bool_or(urine_negative_flag)
        AS urine_any_negative_24h_value

    FROM sofa_urine_events_0_24h

    GROUP BY episode_id
  ),

  ranked_reliable_events AS (
    SELECT
      *,

      ROW_NUMBER() OVER (
        PARTITION BY episode_id
        ORDER BY
          urine_observed_hours_24hr DESC,
          urine_hours_from_ed DESC,
          charttime,
          urine_event_id
      ) AS urine_selection_rank

    FROM sofa_urine_events_0_24h

    WHERE urine_24h_reliable_flag
  ),

  selected_events AS (
    SELECT
      episode_id,

      urineoutput_24hr_ml
        AS urineoutput_observed_ml,

      (
        urineoutput_24hr_ml /
        urine_observed_hours_24hr
      ) * 24.0 AS urineoutput_standardized_24h_ml,

      charttime
        AS urineoutput_reliable_charttime,

      urine_hours_from_ed
        AS urineoutput_reliable_hours_from_ed,

      urine_observed_hours_24hr
        AS urineoutput_reliable_observed_hours,

      urine_window_start_hours_from_ed
        AS urineoutput_reliable_window_start_hours_from_ed,

      icu_stay_id
        AS urineoutput_reliable_icu_stay_id,

      urine_event_id
        AS urineoutput_reliable_event_id

    FROM ranked_reliable_events

    WHERE urine_selection_rank = 1
  ),

  merged_events AS (
    SELECT
      counts.*,

      selected.* EXCLUDE (episode_id)

    FROM episode_counts AS counts

    LEFT JOIN selected_events AS selected
      USING (episode_id)
  )

  SELECT
    *,

    CASE
      WHEN urineoutput_standardized_24h_ml IS NULL
        THEN NULL
      WHEN urineoutput_standardized_24h_ml < 200
        THEN 4
      WHEN urineoutput_standardized_24h_ml < 500
        THEN 3
      ELSE 0
    END AS sofa_renal_urine_structured_0_24h

  FROM merged_events;
  "
)

# ------------------------------------------------------------------------------
# 34.6 Combined renal SOFA score
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_renal_episode AS

  WITH merged_sources AS (
    SELECT
      cohort.episode_id,

      creatinine.* EXCLUDE (episode_id),

      urine.* EXCLUDE (episode_id)

    FROM sofa_candidate_episode_ids AS cohort

    LEFT JOIN sofa_creatinine_episode AS creatinine
      USING (episode_id)

    LEFT JOIN sofa_urine_episode AS urine
      USING (episode_id)
  ),

  scored AS (
    SELECT
      *,

      CASE
        WHEN sofa_renal_creatinine_structured_0_24h
          IS NOT NULL
          AND sofa_renal_urine_structured_0_24h
            IS NOT NULL
        THEN greatest(
          sofa_renal_creatinine_structured_0_24h,
          sofa_renal_urine_structured_0_24h
        )

        WHEN sofa_renal_creatinine_structured_0_24h
          IS NOT NULL
        THEN sofa_renal_creatinine_structured_0_24h

        WHEN sofa_renal_urine_structured_0_24h
          IS NOT NULL
        THEN sofa_renal_urine_structured_0_24h
      END AS sofa_renal_structured_0_24h

    FROM merged_sources
  )

  SELECT
    *,

    CASE
      WHEN sofa_renal_creatinine_structured_0_24h
        IS NOT NULL
        AND sofa_renal_urine_structured_0_24h
          IS NOT NULL
        AND sofa_renal_creatinine_structured_0_24h =
          sofa_renal_urine_structured_0_24h
      THEN 'both_equal'

      WHEN sofa_renal_urine_structured_0_24h
        IS NOT NULL
        AND (
          sofa_renal_creatinine_structured_0_24h
            IS NULL
          OR sofa_renal_urine_structured_0_24h >
            sofa_renal_creatinine_structured_0_24h
        )
      THEN 'urine_output'

      WHEN sofa_renal_creatinine_structured_0_24h
        IS NOT NULL
      THEN 'creatinine'
    END AS sofa_renal_determining_source,

    sofa_renal_creatinine_structured_0_24h IS NOT NULL
      OR sofa_renal_urine_structured_0_24h IS NOT NULL
      AS renal_data_available_flag,

    coalesce(
      creatinine_any_review_flag_0_24h,
      FALSE
    )
      OR coalesce(
        urine_any_negative_24h_value,
        FALSE
      ) AS renal_review_flag

  FROM scored;
  "
)

# ==============================================================================
# 34.7 RENAL SOFA VALIDATION
# ==============================================================================

renal_sofa_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS episode_n,

    COUNT(*) FILTER (
      WHERE sofa_renal_creatinine_structured_0_24h
        IS NOT NULL
    ) AS episode_with_creatinine_score_n,

    COUNT(*) FILTER (
      WHERE sofa_renal_urine_structured_0_24h
        IS NOT NULL
    ) AS episode_with_reliable_urine_score_n,

    COUNT(*) FILTER (
      WHERE sofa_renal_structured_0_24h IS NOT NULL
    ) AS episode_with_renal_score_n,

    COUNT(*) - COUNT(DISTINCT episode_id)
      AS duplicated_episode_n,

    COUNT(*) FILTER (
      WHERE sofa_renal_structured_0_24h
        NOT BETWEEN 0 AND 4
    ) AS invalid_score_n,

    COUNT(*) FILTER (
      WHERE renal_data_available_flag
        AND sofa_renal_structured_0_24h IS NULL
    ) AS data_without_score_n,

    COUNT(*) FILTER (
      WHERE NOT renal_data_available_flag
        AND sofa_renal_structured_0_24h IS NOT NULL
    ) AS score_without_data_n,

    COUNT(*) FILTER (
      WHERE creatinine_max_value_0_24h IS NOT NULL
        AND (
          creatinine_max_value_0_24h <= 0
          OR creatinine_max_value_0_24h > 50
          OR NOT isfinite(creatinine_max_value_0_24h)
        )
    ) AS invalid_selected_creatinine_n,

    COUNT(*) FILTER (
      WHERE urineoutput_standardized_24h_ml IS NOT NULL
        AND (
          urineoutput_standardized_24h_ml < 0
          OR NOT isfinite(
            urineoutput_standardized_24h_ml
          )
        )
    ) AS invalid_selected_urine_n,

    COUNT(*) FILTER (
      WHERE renal_review_flag
    ) AS episode_requiring_review_n

  FROM sofa_renal_episode;
  "
) %>%
  tibble::as_tibble()


if (
  renal_sofa_check$episode_n !=
  sofa_candidate_episode_check$episode_n ||
  renal_sofa_check$duplicated_episode_n != 0L ||
  renal_sofa_check$invalid_score_n != 0L ||
  renal_sofa_check$data_without_score_n != 0L ||
  renal_sofa_check$score_without_data_n != 0L ||
  renal_sofa_check$invalid_selected_creatinine_n != 0L ||
  renal_sofa_check$invalid_selected_urine_n != 0L
) {
  stop(
    "Renal SOFA validation failed.",
    call. = FALSE
  )
}

print(renal_sofa_check)

# ==============================================================================
# 35. SOFA COMPONENT ASSEMBLY AND TOTAL SCORES
# ==============================================================================


# ------------------------------------------------------------------------------
# 35.1 Verify required component tables
# ------------------------------------------------------------------------------

required_sofa_component_tables <- c(
  "sofa_candidate_episode_ids",
  "ed_arrival_vitals_episode",
  "sofa_respiratory_episode",
  "sofa_coagulation_episode",
  "sofa_hepatic_episode",
  "sofa_cardiovascular_episode",
  "sofa_neurologic_episode",
  "sofa_renal_episode"
)

missing_sofa_component_tables <- required_sofa_component_tables[
  !vapply(
    required_sofa_component_tables,
    function(table_name) {
      DBI::dbExistsTable(
        con,
        table_name
      )
    },
    logical(1L)
  )
]

if (length(missing_sofa_component_tables) > 0L) {
  stop(
    paste0(
      "Missing required SOFA component table(s): ",
      paste(
        missing_sofa_component_tables,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# 35.2 Assemble component scores and traceability fields
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE sofa_components_episode AS

  WITH merged_components AS (
    SELECT
      cohort.episode_id,

      respiratory.sofa_respiratory_structured_0_24h,
      respiratory.respiratory_n_valid_pf_0_24h,
      respiratory.respiratory_worst_pf_ratio_0_24h,
      respiratory.respiratory_worst_pf_time_0_24h,
      respiratory.respiratory_worst_po2_0_24h,
      respiratory.respiratory_worst_fio2_percent_0_24h,
      respiratory.respiratory_support_at_worst_pf,
      respiratory.respiratory_ventilation_status_at_worst_pf,
      respiratory.respiratory_worst_specimen_id,
      respiratory.respiratory_worst_pf_event_id,

      coalesce(
        respiratory.respiratory_any_review_0_24h,
        FALSE
      ) AS sofa_respiratory_review_flag,

      coagulation.sofa_coagulation_structured_0_24h,
      coagulation.platelet_n_valid_0_24h,
      coagulation.platelet_min_value_0_24h,
      coagulation.platelet_min_time_0_24h,
      coagulation.platelet_min_itemid_0_24h,
      coagulation.platelet_min_event_id_0_24h,

      coalesce(
        coagulation.platelet_any_review_flag_0_24h,
        FALSE
      ) AS sofa_coagulation_review_flag,

      hepatic.sofa_hepatic_structured_0_24h,
      hepatic.bilirubin_n_valid_0_24h,
      hepatic.bilirubin_first_value_0_24h,
      hepatic.bilirubin_first_time_0_24h,
      hepatic.bilirubin_max_value_0_24h,
      hepatic.bilirubin_max_time_0_24h,
      hepatic.bilirubin_max_itemid_0_24h,
      hepatic.bilirubin_max_event_id_0_24h,

      coalesce(
        hepatic.bilirubin_any_review_flag_0_24h,
        FALSE
      ) AS sofa_hepatic_review_flag,

      cardiovascular.sofa_cardiovascular_structured_0_24h,
      cardiovascular.cardiovascular_data_available_flag,
      cardiovascular.map_n_valid_0_24h,
      cardiovascular.map_min_value_0_24h,
      cardiovascular.map_min_time_0_24h,
      cardiovascular.map_min_source_0_24h,
      cardiovascular.map_min_source_domain_0_24h,
      cardiovascular.map_min_event_id_0_24h,

      cardiovascular.dopamine_max_0_24h,
      cardiovascular.epinephrine_max_0_24h,
      cardiovascular.norepinephrine_max_0_24h,
      cardiovascular.dobutamine_max_0_24h,

      cardiovascular.any_classic_sofa_agent_0_24h,
      cardiovascular.shock_vasopressor_evidence_0_24h,
      cardiovascular.sofa_cardiovascular_determining_factor,

      coalesce(
        cardiovascular.cardiovascular_review_flag,
        FALSE
      ) AS sofa_cardiovascular_review_flag,

      neurologic.sofa_neurologic_structured_0_24h,
      neurologic.gcs_n_valid_0_24h,
      neurologic.gcs_min_value_0_24h,
      neurologic.gcs_min_time_0_24h,
      neurologic.gcs_min_icu_stay_id_0_24h,
      neurologic.gcs_min_event_id_0_24h,

      coalesce(
        neurologic.sofa_neurologic_manual_review_required,
        FALSE
      ) AS sofa_neurologic_review_required,

      neurologic.sofa_neurologic_review_reason,

      renal.sofa_renal_structured_0_24h,
      renal.sofa_renal_creatinine_structured_0_24h,
      renal.sofa_renal_urine_structured_0_24h,

      renal.creatinine_n_valid_0_24h,
      renal.creatinine_first_value_0_24h,
      renal.creatinine_first_time_0_24h,
      renal.creatinine_max_value_0_24h,
      renal.creatinine_max_time_0_24h,
      renal.creatinine_max_itemid_0_24h,
      renal.creatinine_max_event_id_0_24h,

      renal.urine_n_reliable_24h_rows,
      renal.urineoutput_observed_ml,
      renal.urineoutput_standardized_24h_ml,
      renal.urineoutput_reliable_charttime,
      renal.urineoutput_reliable_observed_hours,
      renal.urineoutput_reliable_event_id,

      renal.sofa_renal_determining_source,

      coalesce(
        renal.renal_review_flag,
        FALSE
      ) AS sofa_renal_review_flag,

      arrival.ed_arrival_temperature_c,
      arrival.ed_arrival_heart_rate,
      arrival.ed_arrival_respiratory_rate,
      arrival.ed_arrival_map_mmhg,
      arrival.ed_arrival_spo2_percent

    FROM sofa_candidate_episode_ids AS cohort

    LEFT JOIN ed_arrival_vitals_episode AS arrival
      USING (episode_id)

    LEFT JOIN sofa_respiratory_episode AS respiratory
      USING (episode_id)

    LEFT JOIN sofa_coagulation_episode AS coagulation
      USING (episode_id)

    LEFT JOIN sofa_hepatic_episode AS hepatic
      USING (episode_id)

    LEFT JOIN sofa_cardiovascular_episode AS cardiovascular
      USING (episode_id)

    LEFT JOIN sofa_neurologic_episode AS neurologic
      USING (episode_id)

    LEFT JOIN sofa_renal_episode AS renal
      USING (episode_id)
  ),

  availability_flags AS (
    SELECT
      *,

      sofa_respiratory_structured_0_24h IS NOT NULL
        AS sofa_respiratory_observed_flag,

      sofa_coagulation_structured_0_24h IS NOT NULL
        AS sofa_coagulation_observed_flag,

      sofa_hepatic_structured_0_24h IS NOT NULL
        AS sofa_hepatic_observed_flag,

      sofa_cardiovascular_structured_0_24h IS NOT NULL
        AS sofa_cardiovascular_observed_flag,

      sofa_neurologic_structured_0_24h IS NOT NULL
        AS sofa_neurologic_observed_flag,

      sofa_renal_structured_0_24h IS NOT NULL
        AS sofa_renal_observed_flag

    FROM merged_components
  ),

  availability_counts AS (
    SELECT
      *,

      CAST(sofa_respiratory_observed_flag AS INTEGER) +
      CAST(sofa_coagulation_observed_flag AS INTEGER) +
      CAST(sofa_hepatic_observed_flag AS INTEGER) +
      CAST(sofa_cardiovascular_observed_flag AS INTEGER) +
      CAST(sofa_neurologic_observed_flag AS INTEGER) +
      CAST(sofa_renal_observed_flag AS INTEGER)
        AS sofa_components_observed_n,

      CAST(sofa_respiratory_observed_flag AS INTEGER) +
      CAST(sofa_hepatic_observed_flag AS INTEGER) +
      CAST(sofa_cardiovascular_observed_flag AS INTEGER) +
      CAST(sofa_neurologic_observed_flag AS INTEGER) +
      CAST(sofa_renal_observed_flag AS INTEGER)
        AS sofa_no_coag_components_observed_n

    FROM availability_flags
  ),

  zero_filled_components AS (
    SELECT
      *,

      6 - sofa_components_observed_n
        AS sofa_components_missing_n,

      5 - sofa_no_coag_components_observed_n
        AS sofa_no_coag_components_missing_n,

      sofa_components_observed_n = 6
        AS sofa_all_components_observed_flag,

      sofa_components_observed_n > 0
        AS sofa_any_component_observed_flag,

      sofa_no_coag_components_observed_n = 5
        AS sofa_no_coag_all_components_observed_flag,

      coalesce(
        sofa_respiratory_structured_0_24h,
        0
      ) AS sofa_respiratory_zero,

      coalesce(
        sofa_coagulation_structured_0_24h,
        0
      ) AS sofa_coagulation_zero,

      coalesce(
        sofa_hepatic_structured_0_24h,
        0
      ) AS sofa_hepatic_zero,

      coalesce(
        sofa_cardiovascular_structured_0_24h,
        0
      ) AS sofa_cardiovascular_zero,

      coalesce(
        sofa_neurologic_structured_0_24h,
        0
      ) AS sofa_neurologic_zero,

      coalesce(
        sofa_renal_structured_0_24h,
        0
      ) AS sofa_renal_zero

    FROM availability_counts
  ),

  total_scores AS (
    SELECT
      *,

      CASE
        WHEN sofa_all_components_observed_flag
        THEN
          sofa_respiratory_structured_0_24h +
          sofa_coagulation_structured_0_24h +
          sofa_hepatic_structured_0_24h +
          sofa_cardiovascular_structured_0_24h +
          sofa_neurologic_structured_0_24h +
          sofa_renal_structured_0_24h
      END AS sofa_total_complete_0_24h,

      sofa_respiratory_zero +
      sofa_coagulation_zero +
      sofa_hepatic_zero +
      sofa_cardiovascular_zero +
      sofa_neurologic_zero +
      sofa_renal_zero
        AS sofa_total_structured_0_24h,

      CASE
        WHEN sofa_no_coag_all_components_observed_flag
        THEN
          sofa_respiratory_structured_0_24h +
          sofa_hepatic_structured_0_24h +
          sofa_cardiovascular_structured_0_24h +
          sofa_neurologic_structured_0_24h +
          sofa_renal_structured_0_24h
      END AS sofa_no_coag_complete_0_24h,

      sofa_respiratory_zero +
      sofa_hepatic_zero +
      sofa_cardiovascular_zero +
      sofa_neurologic_zero +
      sofa_renal_zero
        AS sofa_no_coag_structured_0_24h

    FROM zero_filled_components
  ),

  review_and_threshold_flags AS (
    SELECT
      *,

      sofa_all_components_observed_flag
        AS sofa_complete_available_flag,

      sofa_any_component_observed_flag
        AS sofa_structured_available_flag,

      sofa_no_coag_all_components_observed_flag
        AS sofa_no_coag_complete_available_flag,

      sofa_components_missing_n > 0
        AS sofa_missing_components_assigned_zero_flag,

      sofa_total_structured_0_24h >= 2
        AS sofa_total_at_least_2_flag,

      sofa_no_coag_structured_0_24h >= 2
        AS sofa_no_coag_at_least_2_flag,

      CASE
        WHEN sofa_total_complete_0_24h IS NOT NULL
        THEN sofa_total_complete_0_24h >= 2
      END AS sofa_total_complete_at_least_2_flag,

      CASE
        WHEN sofa_no_coag_complete_0_24h IS NOT NULL
        THEN sofa_no_coag_complete_0_24h >= 2
      END AS sofa_no_coag_complete_at_least_2_flag,

      CAST(sofa_respiratory_review_flag AS INTEGER) +
      CAST(sofa_coagulation_review_flag AS INTEGER) +
      CAST(sofa_hepatic_review_flag AS INTEGER) +
      CAST(sofa_cardiovascular_review_flag AS INTEGER) +
      CAST(sofa_neurologic_review_required AS INTEGER) +
      CAST(sofa_renal_review_flag AS INTEGER)
        AS sofa_component_review_count,

      sofa_respiratory_review_flag
        OR sofa_coagulation_review_flag
        OR sofa_hepatic_review_flag
        OR sofa_cardiovascular_review_flag
        OR sofa_neurologic_review_required
        OR sofa_renal_review_flag
        AS sofa_any_component_review_flag

    FROM total_scores
  )

  SELECT
    *,

    sofa_missing_components_assigned_zero_flag
      OR sofa_any_component_review_flag
      AS sofa_structured_review_required_flag

  FROM review_and_threshold_flags;
  "
)

# ==============================================================================
# 35.3 SOFA ASSEMBLY VALIDATION
# ==============================================================================

sofa_assembly_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS episode_n,

    COUNT(*) - COUNT(DISTINCT episode_id)
      AS duplicated_episode_n,

    COUNT(*) FILTER (
      WHERE sofa_components_observed_n NOT BETWEEN 0 AND 6
    ) AS invalid_observed_component_count_n,

    COUNT(*) FILTER (
      WHERE sofa_components_missing_n !=
        6 - sofa_components_observed_n
    ) AS invalid_missing_component_count_n,

    COUNT(*) FILTER (
      WHERE sofa_total_structured_0_24h
        NOT BETWEEN 0 AND 24
    ) AS invalid_structured_total_n,

    COUNT(*) FILTER (
      WHERE sofa_no_coag_structured_0_24h
        NOT BETWEEN 0 AND 20
    ) AS invalid_no_coag_total_n,

    COUNT(*) FILTER (
      WHERE sofa_total_complete_0_24h IS NOT NULL
        AND NOT sofa_all_components_observed_flag
    ) AS complete_score_without_all_components_n,

    COUNT(*) FILTER (
      WHERE sofa_all_components_observed_flag
        AND sofa_total_complete_0_24h IS NULL
    ) AS all_components_without_complete_score_n,

    COUNT(*) FILTER (
      WHERE sofa_total_complete_0_24h IS NOT NULL
        AND sofa_total_complete_0_24h !=
          sofa_total_structured_0_24h
    ) AS complete_structured_score_disagreement_n,

    COUNT(*) FILTER (
      WHERE sofa_no_coag_complete_0_24h IS NOT NULL
        AND sofa_no_coag_complete_0_24h !=
          sofa_no_coag_structured_0_24h
    ) AS no_coag_complete_structured_disagreement_n,

    COUNT(*) FILTER (
      WHERE sofa_no_coag_structured_0_24h !=
        sofa_total_structured_0_24h -
        sofa_coagulation_zero
    ) AS invalid_no_coag_subtraction_n,

    COUNT(*) FILTER (
      WHERE sofa_component_review_count NOT BETWEEN 0 AND 6
    ) AS invalid_review_count_n,

    COUNT(*) FILTER (
      WHERE sofa_missing_components_assigned_zero_flag !=
        (sofa_components_missing_n > 0)
    ) AS invalid_missing_component_flag_n

  FROM sofa_components_episode;
  "
) %>%
  tibble::as_tibble()


if (
  sofa_assembly_check$episode_n !=
  sofa_candidate_episode_check$episode_n ||
  sofa_assembly_check$duplicated_episode_n != 0L ||
  sofa_assembly_check$invalid_observed_component_count_n != 0L ||
  sofa_assembly_check$invalid_missing_component_count_n != 0L ||
  sofa_assembly_check$invalid_structured_total_n != 0L ||
  sofa_assembly_check$invalid_no_coag_total_n != 0L ||
  sofa_assembly_check$complete_score_without_all_components_n != 0L ||
  sofa_assembly_check$all_components_without_complete_score_n != 0L ||
  sofa_assembly_check$complete_structured_score_disagreement_n != 0L ||
  sofa_assembly_check$no_coag_complete_structured_disagreement_n != 0L ||
  sofa_assembly_check$invalid_no_coag_subtraction_n != 0L ||
  sofa_assembly_check$invalid_review_count_n != 0L ||
  sofa_assembly_check$invalid_missing_component_flag_n != 0L
) {
  stop(
    "SOFA component assembly validation failed.",
    call. = FALSE
  )
}

print(sofa_assembly_check)

# ------------------------------------------------------------------------------
# 35.4 Temporary SOFA coverage summary
# ------------------------------------------------------------------------------

sofa_coverage_summary <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS episode_n,

    COUNT(*) FILTER (
      WHERE sofa_all_components_observed_flag
    ) AS episode_with_all_components_n,

    COUNT(*) FILTER (
      WHERE sofa_missing_components_assigned_zero_flag
    ) AS episode_with_missing_components_n,

    COUNT(*) FILTER (
      WHERE sofa_total_structured_0_24h >= 2
    ) AS episode_with_structured_sofa_at_least_2_n,

    COUNT(*) FILTER (
      WHERE sofa_no_coag_structured_0_24h >= 2
    ) AS episode_with_no_coag_sofa_at_least_2_n,

    COUNT(*) FILTER (
      WHERE sofa_total_structured_0_24h >= 2
        AND sofa_no_coag_structured_0_24h < 2
    ) AS episode_with_coagulation_dependent_threshold_n,

    COUNT(*) FILTER (
      WHERE sofa_any_component_review_flag
    ) AS episode_with_component_review_flag_n,

    AVG(sofa_components_observed_n)
      AS mean_components_observed

  FROM sofa_components_episode;
  "
) %>%
  tibble::as_tibble()

print(sofa_coverage_summary)


# ==============================================================================
# 36. INTEGRATE SOFA INTO THE WORKING COHORT
# ==============================================================================


# ------------------------------------------------------------------------------
# 36.1 Identify SOFA and cohort columns
# ------------------------------------------------------------------------------

sofa_integration_columns <- setdiff(
  DBI::dbListFields(
    con,
    "sofa_components_episode"
  ),
  "episode_id"
)

candidate_columns_before_sofa <- DBI::dbListFields(
  con,
  "candidate_cohort_working_table"
)

# Remove previously integrated SOFA columns when this section is rerun.

candidate_base_columns <- setdiff(
  candidate_columns_before_sofa,
  sofa_integration_columns
)

if (!"episode_id" %in% candidate_base_columns) {
  stop(
    "The candidate cohort does not contain episode_id.",
    call. = FALSE
  )
}


qualified_column_sql <- function(
    table_alias,
    columns
) {
  paste(
    vapply(
      columns,
      function(column_name) {
        paste0(
          table_alias,
          ".",
          as.character(
            DBI::dbQuoteIdentifier(
              con,
              column_name
            )
          )
        )
      },
      character(1L)
    ),
    collapse = ",\n    "
  )
}


candidate_base_column_sql <- qualified_column_sql(
  "cohort",
  candidate_base_columns
)

sofa_addition_column_sql <- qualified_column_sql(
  "sofa",
  sofa_integration_columns
)

# ------------------------------------------------------------------------------
# 36.2 Preserve the base cohort before integration
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  paste0(
    "
    CREATE OR REPLACE TEMP TABLE
      candidate_cohort_before_sofa_table AS

    SELECT
      ",
    candidate_base_column_sql,
    "

    FROM candidate_cohort_working_table AS cohort;
    "
  )
)

# ------------------------------------------------------------------------------
# 36.3 Add SOFA fields without changing cohort membership
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  paste0(
    "
    CREATE OR REPLACE TEMP TABLE
      candidate_cohort_with_sofa_table AS

    SELECT
      ",
    candidate_base_column_sql,
    ",
      ",
    sofa_addition_column_sql,
    "

    FROM candidate_cohort_before_sofa_table AS cohort

    LEFT JOIN sofa_components_episode AS sofa
      USING (episode_id);
    "
  )
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    candidate_cohort_working_table AS

  SELECT *
  FROM candidate_cohort_with_sofa_table;
  "
)

# ==============================================================================
# 36.4 SOFA INTEGRATION VALIDATION
# ==============================================================================

sofa_integration_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    (
      SELECT COUNT(*)
      FROM candidate_cohort_before_sofa_table
    ) AS source_episode_n,

    (
      SELECT COUNT(*)
      FROM sofa_components_episode
    ) AS sofa_episode_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
    ) AS output_episode_n,

    (
      SELECT
        COUNT(*) - COUNT(DISTINCT episode_id)
      FROM candidate_cohort_working_table
    ) AS duplicated_output_episode_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
      WHERE sofa_total_structured_0_24h IS NULL
    ) AS episode_without_integrated_sofa_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
      WHERE sofa_all_components_observed_flag
    ) AS episode_with_complete_sofa_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
      WHERE sofa_missing_components_assigned_zero_flag
    ) AS episode_with_zero_filled_component_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
      WHERE sofa_total_structured_0_24h >= 2
        AND sofa_no_coag_structured_0_24h < 2
    ) AS coagulation_dependent_threshold_episode_n;
  "
) %>%
  tibble::as_tibble()


if (
  sofa_integration_check$source_episode_n !=
  sofa_integration_check$output_episode_n ||
  sofa_integration_check$source_episode_n !=
  sofa_integration_check$sofa_episode_n ||
  sofa_integration_check$duplicated_output_episode_n != 0L ||
  sofa_integration_check$episode_without_integrated_sofa_n != 0L
) {
  stop(
    "SOFA cohort integration failed.",
    call. = FALSE
  )
}


candidate_cohort_working <- DBI::dbGetQuery(
  con,
  "
  SELECT *
  FROM candidate_cohort_working_table
  ORDER BY episode_id;
  "
) %>%
  tibble::as_tibble()


assert_unique_key(
  candidate_cohort_working,
  "episode_id",
  "candidate_cohort_working"
)


print(sofa_integration_check)

# ==============================================================================
# 36.5 SAFE SOFA SOURCE CLEANUP
# ==============================================================================

sofa_temporary_source_tables <- c(
  "sofa_icustays",
  "sofa_icu_episode_candidates",
  "sofa_icu_episode_map",
  "sofa_icu_episode_summary",
  "sofa_lab_primary_candidates",
  "sofa_lab_fallback_candidates",
  "sofa_lab_assignment_candidates",
  "sofa_labevents_source",
  "sofa_labevents_assigned_0_24h",
  "sofa_candidate_episode_ids",
  "sofa_icu_episode_map_candidate",
  "sofa_labevents_candidate_0_24h",
  "sofa_ventilation_source",
  "sofa_ventilation_events_0_24h",
  "sofa_blood_gas_specimens",
  "sofa_pf_support_assessment",
  "sofa_respiratory_episode",
  "sofa_platelet_events_0_24h",
  "sofa_coagulation_episode",
  "sofa_bilirubin_events_0_24h",
  "sofa_hepatic_episode",
  "sofa_ed_vitals_source",
  "sofa_ed_map_events_0_24h",
  "sofa_icu_vitals_source",
  "sofa_icu_map_events_0_24h",
  "sofa_map_events_0_24h",
  "sofa_map_episode",
  "sofa_vasoactive_source",
  "sofa_vasoactive_events_0_24h",
  "sofa_vasoactive_episode",
  "sofa_cardiovascular_episode",
  "sofa_gcs_source",
  "sofa_gcs_events_0_24h",
  "sofa_neurologic_episode",
  "sofa_creatinine_events_0_24h",
  "sofa_creatinine_episode",
  "sofa_urine_source",
  "sofa_urine_events_0_24h",
  "sofa_urine_episode",
  "sofa_renal_episode",
  "candidate_cohort_before_sofa_table",
  "candidate_cohort_with_sofa_table"
)


for (table_name in sofa_temporary_source_tables) {
  DBI::dbExecute(
    con,
    paste0(
      "DROP TABLE IF EXISTS ",
      as.character(
        DBI::dbQuoteIdentifier(
          con,
          table_name
        )
      ),
      ";"
    )
  )
}


sofa_temporary_source_views <- c(
  "sofa_icustays_source_raw",
  "sofa_labevents_source_raw",
  "sofa_ventilation_source_raw",
  "sofa_ed_vitals_source_raw",
  "sofa_icu_vitals_source_raw",
  "sofa_vasoactive_source_raw",
  "sofa_gcs_source_raw",
  "sofa_urine_source_raw"
)


for (view_name in sofa_temporary_source_views) {
  DBI::dbExecute(
    con,
    paste0(
      "DROP VIEW IF EXISTS ",
      as.character(
        DBI::dbQuoteIdentifier(
          con,
          view_name
        )
      ),
      ";"
    )
  )
}


if (
  !DBI::dbExistsTable(
    con,
    "sofa_components_episode"
  ) ||
  !DBI::dbExistsTable(
    con,
    "candidate_cohort_working_table"
  )
) {
  stop(
    "SOFA cleanup removed a required final table.",
    call. = FALSE
  )
}

invisible(gc())


# ==============================================================================
# 37. PRIMARY SYSTEMIC ANTIMICROBIAL PRESCRIPTIONS
# ==============================================================================


# ------------------------------------------------------------------------------
# 37.1 Helper for safely quoting file paths in DuckDB SQL
# ------------------------------------------------------------------------------

quote_infection_sql_path <- function(con, path) {
  normalized_path <- normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )
  
  as.character(
    DBI::dbQuoteString(
      con,
      normalized_path
    )
  )
}


# ------------------------------------------------------------------------------
# 37.2 Locate prescription fragments
# ------------------------------------------------------------------------------

infection_prescriptions_directory <- file.path(
  INPUT_DIR,
  "prescriptions_infection_raw",
  "v1"
)

if (!dir.exists(infection_prescriptions_directory)) {
  stop(
    paste0(
      "The prescriptions directory was not found: ",
      infection_prescriptions_directory,
      "."
    ),
    call. = FALSE
  )
}


infection_prescriptions_files <- list.files(
  infection_prescriptions_directory,
  pattern = "\\.csv\\.gz$",
  full.names = TRUE,
  recursive = FALSE,
  ignore.case = TRUE
)

infection_prescriptions_files <- sort(
  unique(
    normalizePath(
      infection_prescriptions_files,
      winslash = "/",
      mustWork = TRUE
    )
  )
)


if (length(infection_prescriptions_files) == 0L) {
  stop(
    paste0(
      "No compressed prescription fragments were found in: ",
      infection_prescriptions_directory,
      "."
    ),
    call. = FALSE
  )
}


infection_prescriptions_file_list_sql <- paste0(
  "[",
  paste(
    vapply(
      infection_prescriptions_files,
      function(path) {
        quote_infection_sql_path(
          con = con,
          path = path
        )
      },
      character(1L)
    ),
    collapse = ", "
  ),
  "]"
)


# ------------------------------------------------------------------------------
# 37.3 Import prescription fragments
# ------------------------------------------------------------------------------

# BigQuery extraction used to create the source files:
#
# EXPORT DATA OPTIONS (
#   uri = 'gs://<bucket>/prescriptions_infection_raw/v1/prescriptions_infection_raw-*.csv.gz',
#   format = 'CSV',
#   compression = 'GZIP',
#   header = TRUE,
#   field_delimiter = ',',
#   overwrite = FALSE
# ) AS
# SELECT
#   subject_id,
#   hadm_id,
#   pharmacy_id,
#   poe_id,
#   starttime,
#   stoptime,
#   drug_type,
#   drug,
#   formulary_drug_cd,
#   route
# FROM `physionet-data.mimiciv_v3_1_hosp.prescriptions`
# WHERE subject_id IS NOT NULL
#   AND starttime IS NOT NULL
#   AND drug IS NOT NULL
#   AND drug_type IN ('MAIN', 'ADDITIVE');


DBI::dbExecute(
  con,
  paste0(
    "
    CREATE OR REPLACE TEMP VIEW
      infection_prescriptions_source_raw AS

    SELECT *

    FROM read_csv(
      ",
    infection_prescriptions_file_list_sql,
    ",
      auto_detect = FALSE,
      delim = ',',
      quote = '\"',
      escape = '\"',
      header = TRUE,
      columns = {
        'subject_id': 'VARCHAR',
        'hadm_id': 'VARCHAR',
        'pharmacy_id': 'VARCHAR',
        'poe_id': 'VARCHAR',
        'starttime': 'VARCHAR',
        'stoptime': 'VARCHAR',
        'drug_type': 'VARCHAR',
        'drug': 'VARCHAR',
        'formulary_drug_cd': 'VARCHAR',
        'route': 'VARCHAR'
      },
      union_by_name = TRUE,
      filename = TRUE,
      strict_mode = TRUE,
      ignore_errors = FALSE
    );
    "
  )
)


required_infection_prescription_columns <- c(
  "subject_id",
  "hadm_id",
  "pharmacy_id",
  "poe_id",
  "starttime",
  "stoptime",
  "drug_type",
  "drug",
  "formulary_drug_cd",
  "route",
  "filename"
)


missing_infection_prescription_columns <- setdiff(
  required_infection_prescription_columns,
  DBI::dbListFields(
    con,
    "infection_prescriptions_source_raw"
  )
)


if (length(missing_infection_prescription_columns) > 0L) {
  stop(
    paste0(
      "The prescription source is missing required column(s): ",
      paste(
        missing_infection_prescription_columns,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 37.4 Candidate subjects
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    infection_candidate_subjects AS

  SELECT DISTINCT
    trim(
      CAST(subject_id AS VARCHAR)
    ) AS subject_id

  FROM candidate_cohort_working_table

  WHERE subject_id IS NOT NULL
    AND trim(
      CAST(subject_id AS VARCHAR)
    ) != '';
  "
)


# ------------------------------------------------------------------------------
# 37.5 Frozen antimicrobial vocabulary
# ------------------------------------------------------------------------------

infection_reference_antibiotic_terms <- c(
  "adoxa",
  "ala-tet",
  "alodox",
  "amikacin",
  "amikin",
  "amoxicill",
  "amphotericin",
  "anidulafungin",
  "ancef",
  "clavulanate",
  "ampicillin",
  "augmentin",
  "avelox",
  "avidoxy",
  "azactam",
  "azithromycin",
  "aztreonam",
  "axetil",
  "bactocill",
  "bactrim",
  "bactroban",
  "bethkis",
  "biaxin",
  "bicillin l-a",
  "cayston",
  "cefazolin",
  "cedax",
  "cefoxitin",
  "ceftazidime",
  "cefaclor",
  "cefadroxil",
  "cefdinir",
  "cefditoren",
  "cefepime",
  "cefotan",
  "cefotetan",
  "cefotaxime",
  "ceftaroline",
  "cefpodoxime",
  "cefpirome",
  "cefprozil",
  "ceftibuten",
  "ceftin",
  "ceftriaxone",
  "cefuroxime",
  "cephalexin",
  "cephalothin",
  "cephapririn",
  "chloramphenicol",
  "cipro",
  "ciprofloxacin",
  "claforan",
  "clarithromycin",
  "cleocin",
  "clindamycin",
  "cubicin",
  "dicloxacillin",
  "dirithromycin",
  "doryx",
  "doxycy",
  "duricef",
  "dynacin",
  "ery-tab",
  "eryped",
  "eryc",
  "erythrocin",
  "erythromycin",
  "factive",
  "flagyl",
  "fortaz",
  "furadantin",
  "garamycin",
  "gentamicin",
  "kanamycin",
  "keflex",
  "kefzol",
  "ketek",
  "levaquin",
  "levofloxacin",
  "lincocin",
  "linezolid",
  "macrobid",
  "macrodantin",
  "maxipime",
  "mefoxin",
  "metronidazole",
  "meropenem",
  "methicillin",
  "minocin",
  "minocycline",
  "monodox",
  "monurol",
  "morgidox",
  "moxatag",
  "moxifloxacin",
  "mupirocin",
  "myrac",
  "nafcillin",
  "neomycin",
  "nicazel doxy 30",
  "nitrofurantoin",
  "norfloxacin",
  "noroxin",
  "ocudox",
  "ofloxacin",
  "omnicef",
  "oracea",
  "oraxyl",
  "oxacillin",
  "pc pen vk",
  "pce dispertab",
  "panixine",
  "pediazole",
  "penicillin",
  "periostat",
  "pfizerpen",
  "piperacillin",
  "tazobactam",
  "primsol",
  "proquin",
  "raniclor",
  "rifadin",
  "rifampin",
  "rocephin",
  "smz-tmp",
  "septra",
  "solodyn",
  "spectracef",
  "streptomycin",
  "sulfadiazine",
  "sulfamethoxazole",
  "trimethoprim",
  "sulfatrim",
  "sulfisoxazole",
  "suprax",
  "synercid",
  "tazicef",
  "tetracycline",
  "timentin",
  "tobramycin",
  "unasyn",
  "vancocin",
  "vancomycin",
  "vantin",
  "vibativ",
  "vibra-tabs",
  "vibramycin",
  "zinacef",
  "zithromax",
  "zosyn",
  "zyvox"
)


infection_supplemental_antimicrobial_terms <- c(
  "avibactam",
  "caspofungin",
  "cefiderocol",
  "cefixime",
  "cefoperazone",
  "ceftolozane",
  "cilastatin",
  "cloxacillin",
  "colistin",
  "dalbavancin",
  "daptomycin",
  "delafloxacin",
  "doripenem",
  "ertapenem",
  "fidaxomicin",
  "flucytosine",
  "fluconazole",
  "fosfomycin",
  "imipenem",
  "isavuconazole",
  "micafungin",
  "oritavancin",
  "polymyxin",
  "posaconazole",
  "sulbactam",
  "tedizolid",
  "telavancin",
  "tigecycline",
  "voriconazole"
)


infection_excluded_local_formulation_terms <- c(
  "cream",
  "desensitization",
  "ophth oint",
  "ophthalmic",
  "otic",
  "eye drop",
  "ear drop",
  "topical",
  "ointment",
  "gel"
)


infection_reference_antibiotic_pattern_sql <- as.character(
  DBI::dbQuoteString(
    con,
    paste(
      infection_reference_antibiotic_terms,
      collapse = "|"
    )
  )
)


infection_supplemental_antimicrobial_pattern_sql <- as.character(
  DBI::dbQuoteString(
    con,
    paste(
      infection_supplemental_antimicrobial_terms,
      collapse = "|"
    )
  )
)


infection_excluded_local_formulation_pattern_sql <- as.character(
  DBI::dbQuoteString(
    con,
    paste(
      infection_excluded_local_formulation_terms,
      collapse = "|"
    )
  )
)


# ------------------------------------------------------------------------------
# 37.6 Classify antimicrobial names and routes
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  paste0(
    "
    CREATE OR REPLACE TEMP TABLE
      infection_prescription_vocabulary AS

    WITH normalized_values AS (
      SELECT DISTINCT
        lower(
          trim(
            CAST(drug AS VARCHAR)
          )
        ) AS drug_name_normalized,

        regexp_replace(
          upper(
            trim(
              coalesce(
                CAST(route AS VARCHAR),
                ''
              )
            )
          ),
          '\\s+',
          ' ',
          'g'
        ) AS route_normalized

      FROM infection_prescriptions_source_raw

      WHERE NULLIF(
        trim(
          CAST(drug AS VARCHAR)
        ),
        ''
      ) IS NOT NULL
    ),

    name_classification AS (
      SELECT
        *,

        regexp_matches(
          drug_name_normalized,
          ",
    infection_reference_antibiotic_pattern_sql,
    "
        ) AS reference_antibiotic_name_flag,

        regexp_matches(
          drug_name_normalized,
          ",
    infection_supplemental_antimicrobial_pattern_sql,
    "
        ) AS supplemental_antimicrobial_name_flag,

        regexp_matches(
          drug_name_normalized,
          ",
    infection_excluded_local_formulation_pattern_sql,
    "
        ) AS excluded_local_formulation_flag

      FROM normalized_values
    ),

    route_classification AS (
      SELECT
        *,

        (
          route_normalized IN (
            'IV',
            'IV DRIP',
            'IV BOLUS',
            'IVP',
            'IVPB',
            'IVT',
            'PB',
            'PO',
            'PO/NG',
            'ORAL',
            'IM',
            'SC',
            'SQ',
            'SUBCUT',
            'SUBCUTANEOUS',
            'NG',
            'NG TUBE',
            'G TUBE',
            'J TUBE',
            'PEG',
            'PEJ',
            'NJ',
            'OG',
            'ENTERAL'
          )

          OR regexp_matches(
            route_normalized,
            '^(IV|INTRAVENOUS)( |$)'
          )

          OR regexp_matches(
            route_normalized,
            'ORAL|ENTERAL|NASOGASTRIC|OROGASTRIC'
          )

          OR regexp_matches(
            route_normalized,
            'INTRAMUSCULAR|SUBCUTANEOUS'
          )
        ) AS antimicrobial_systemic_route_flag,

        route_normalized = ''
          AS antimicrobial_route_missing_flag,

        (
          route_normalized IN (
            'OU',
            'OS',
            'OD',
            'AU',
            'AS',
            'AD',
            'TP'
          )

          OR regexp_matches(
            route_normalized,
            'EYE|EAR|OPHTH|OTIC|TOPICAL|DERMAL'
          )
        ) AS excluded_local_route_flag

      FROM name_classification
    )

    SELECT
      *,

      (
        reference_antibiotic_name_flag
        OR supplemental_antimicrobial_name_flag
      ) AS antimicrobial_name_flag,

      (
        (
          reference_antibiotic_name_flag
          OR supplemental_antimicrobial_name_flag
        )
        AND antimicrobial_systemic_route_flag
        AND NOT excluded_local_formulation_flag
        AND NOT excluded_local_route_flag
      ) AS antimicrobial_primary_flag,

      CASE
        WHEN reference_antibiotic_name_flag
        THEN 'reference_antibiotic_vocabulary'

        WHEN supplemental_antimicrobial_name_flag
        THEN 'supplemental_antimicrobial_vocabulary'
      END AS antimicrobial_vocabulary_source,

      (
        (
          reference_antibiotic_name_flag
          OR supplemental_antimicrobial_name_flag
        )
        AND (
          antimicrobial_route_missing_flag
          OR (
            NOT antimicrobial_systemic_route_flag
            AND NOT excluded_local_route_flag
          )
        )
      ) AS antimicrobial_route_limitation_flag

    FROM route_classification;
    "
  )
)


# ------------------------------------------------------------------------------
# 37.7 Construct antimicrobial prescription events
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    infection_antimicrobial_prescriptions AS

  WITH parsed_rows AS (
    SELECT
      trim(
        CAST(source.subject_id AS VARCHAR)
      ) AS subject_id,

      CASE
        WHEN upper(
          trim(
            coalesce(
              CAST(source.hadm_id AS VARCHAR),
              ''
            )
          )
        ) IN ('', 'NA', 'NAN', 'NULL')
        THEN NULL

        ELSE trim(
          CAST(source.hadm_id AS VARCHAR)
        )
      END AS hadm_id,

      NULLIF(
        trim(
          CAST(source.pharmacy_id AS VARCHAR)
        ),
        ''
      ) AS pharmacy_id,

      NULLIF(
        trim(
          CAST(source.poe_id AS VARCHAR)
        ),
        ''
      ) AS poe_id,

      TRY_CAST(
        replace(
          trim(
            CAST(source.starttime AS VARCHAR)
          ),
          'T',
          ' '
        ) AS TIMESTAMP
      ) AS antimicrobial_starttime,

      TRY_CAST(
        replace(
          trim(
            CAST(source.stoptime AS VARCHAR)
          ),
          'T',
          ' '
        ) AS TIMESTAMP
      ) AS antimicrobial_stoptime,

      upper(
        trim(
          CAST(source.drug_type AS VARCHAR)
        )
      ) AS drug_type,

      trim(
        CAST(source.drug AS VARCHAR)
      ) AS antimicrobial_name,

      lower(
        trim(
          CAST(source.drug AS VARCHAR)
        )
      ) AS drug_name_normalized,

      NULLIF(
        trim(
          CAST(source.formulary_drug_cd AS VARCHAR)
        ),
        ''
      ) AS formulary_drug_cd,

      NULLIF(
        trim(
          CAST(source.route AS VARCHAR)
        ),
        ''
      ) AS antimicrobial_route,

      regexp_replace(
        upper(
          trim(
            coalesce(
              CAST(source.route AS VARCHAR),
              ''
            )
          )
        ),
        '\\s+',
        ' ',
        'g'
      ) AS route_normalized,

      regexp_extract(
        CAST(source.filename AS VARCHAR),
        '[^/]+$'
      ) AS source_file

    FROM infection_prescriptions_source_raw AS source

    INNER JOIN infection_candidate_subjects AS candidate
      ON trim(
           CAST(source.subject_id AS VARCHAR)
         ) = candidate.subject_id

    WHERE NULLIF(
      trim(
        CAST(source.drug AS VARCHAR)
      ),
      ''
    ) IS NOT NULL
  ),

  vocabulary_matched AS (
    SELECT
      parsed.*,

      vocabulary.reference_antibiotic_name_flag,
      vocabulary.supplemental_antimicrobial_name_flag,
      vocabulary.antimicrobial_name_flag,
      vocabulary.antimicrobial_systemic_route_flag,
      vocabulary.antimicrobial_route_missing_flag,
      vocabulary.excluded_local_route_flag,
      vocabulary.excluded_local_formulation_flag,
      vocabulary.antimicrobial_primary_flag,
      vocabulary.antimicrobial_vocabulary_source,
      vocabulary.antimicrobial_route_limitation_flag

    FROM parsed_rows AS parsed

    INNER JOIN infection_prescription_vocabulary
      AS vocabulary
      ON parsed.drug_name_normalized =
           vocabulary.drug_name_normalized
     AND parsed.route_normalized =
           vocabulary.route_normalized

    WHERE vocabulary.antimicrobial_name_flag
  ),

  deduplicated_rows AS (
    SELECT DISTINCT *
    FROM vocabulary_matched
  )

  SELECT
    concat(
      'ABX_',
      md5(
        concat_ws(
          '|',
          coalesce(subject_id, ''),
          coalesce(hadm_id, ''),
          coalesce(pharmacy_id, ''),
          coalesce(poe_id, ''),
          coalesce(
            CAST(
              antimicrobial_starttime AS VARCHAR
            ),
            ''
          ),
          coalesce(
            CAST(
              antimicrobial_stoptime AS VARCHAR
            ),
            ''
          ),
          coalesce(drug_type, ''),
          coalesce(drug_name_normalized, ''),
          coalesce(formulary_drug_cd, ''),
          coalesce(route_normalized, '')
        )
      )
    ) AS antimicrobial_event_id,

    *,

    antimicrobial_starttime IS NOT NULL
      AND antimicrobial_stoptime IS NOT NULL
      AND antimicrobial_stoptime <
        antimicrobial_starttime
      AS antimicrobial_stoptime_before_start_flag

  FROM deduplicated_rows;
  "
)


# ------------------------------------------------------------------------------
# 37.8 Antimicrobial prescription validation
# ------------------------------------------------------------------------------

infection_antimicrobial_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS antimicrobial_event_n,

    COUNT(DISTINCT antimicrobial_event_id)
      AS distinct_antimicrobial_event_n,

    COUNT(*) FILTER (
      WHERE antimicrobial_primary_flag
    ) AS primary_antimicrobial_event_n,

    COUNT(*) FILTER (
      WHERE subject_id IS NULL
         OR subject_id = ''
    ) AS missing_subject_id_n,

    COUNT(*) FILTER (
      WHERE antimicrobial_primary_flag
        AND antimicrobial_starttime IS NULL
    ) AS primary_missing_starttime_n,

    COUNT(*) FILTER (
      WHERE antimicrobial_primary_flag
        AND hadm_id IS NULL
    ) AS primary_missing_hadm_id_n,

    COUNT(*) FILTER (
      WHERE antimicrobial_primary_flag
        AND NOT antimicrobial_systemic_route_flag
    ) AS primary_non_systemic_route_n,

    COUNT(*) FILTER (
      WHERE antimicrobial_primary_flag
        AND (
          excluded_local_route_flag
          OR excluded_local_formulation_flag
        )
    ) AS primary_local_exclusion_n,

    COUNT(*) FILTER (
      WHERE antimicrobial_stoptime_before_start_flag
    ) AS stoptime_before_start_n,

    COUNT(*) FILTER (
      WHERE antimicrobial_route_limitation_flag
    ) AS route_limitation_event_n

  FROM infection_antimicrobial_prescriptions;
  "
) %>%
  tibble::as_tibble()


if (
  infection_antimicrobial_check$antimicrobial_event_n !=
  infection_antimicrobial_check$distinct_antimicrobial_event_n ||
  infection_antimicrobial_check$missing_subject_id_n != 0L ||
  infection_antimicrobial_check$primary_missing_starttime_n != 0L ||
  infection_antimicrobial_check$primary_missing_hadm_id_n != 0L ||
  infection_antimicrobial_check$primary_non_systemic_route_n != 0L ||
  infection_antimicrobial_check$primary_local_exclusion_n != 0L
) {
  stop(
    "Primary antimicrobial prescriptions failed validation.",
    call. = FALSE
  )
}


print(infection_antimicrobial_check)

# ==============================================================================
# 38. PRIMARY-ELIGIBLE MICROBIOLOGY SPECIMENS
# ==============================================================================


# ------------------------------------------------------------------------------
# 38.1 Helpers
# ------------------------------------------------------------------------------

quote_infection_sql_value <- function(value) {
  as.character(
    DBI::dbQuoteString(
      con,
      value
    )
  )
}


# ------------------------------------------------------------------------------
# 38.2 Locate and import microbiology events
# ------------------------------------------------------------------------------

infection_microbiology_file <- input_path(
  "microbiologyevents.csv"
)


if (!file.exists(infection_microbiology_file)) {
  stop(
    paste0(
      "The microbiology source file was not found: ",
      infection_microbiology_file,
      "."
    ),
    call. = FALSE
  )
}


infection_microbiology_file <- normalizePath(
  infection_microbiology_file,
  winslash = "/",
  mustWork = TRUE
)


infection_microbiology_file_sql <-
  quote_infection_sql_path(
    con = con,
    path = infection_microbiology_file
  )


# BigQuery extraction used to create the source file:
#
# SELECT *
# FROM `physionet-data.mimiciv_v3_1_hosp.microbiologyevents`;


DBI::dbExecute(
  con,
  paste0(
    "
    CREATE OR REPLACE TEMP VIEW
      infection_microbiology_source_raw AS

    SELECT *

    FROM read_csv_auto(
      ",
    infection_microbiology_file_sql,
    ",
      header = TRUE,
      all_varchar = TRUE,
      delim = ',',
      quote = '\"',
      escape = '\"',
      sample_size = 20480,
      ignore_errors = FALSE
    );
    "
  )
)


required_infection_microbiology_columns <- c(
  "microevent_id",
  "subject_id",
  "hadm_id",
  "micro_specimen_id",
  "chartdate",
  "charttime",
  "spec_type_desc"
)


available_infection_microbiology_columns <- DBI::dbListFields(
  con,
  "infection_microbiology_source_raw"
)


missing_infection_microbiology_columns <- setdiff(
  required_infection_microbiology_columns,
  available_infection_microbiology_columns
)


if (length(missing_infection_microbiology_columns) > 0L) {
  stop(
    paste0(
      "The microbiology source is missing required column(s): ",
      paste(
        missing_infection_microbiology_columns,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 38.3 Excluded broad-microbiology specimen types
# ------------------------------------------------------------------------------

infection_excluded_microbiology_specimen_types <- c(
  "BLOOD (CMV AB)",
  "BLOOD (EBV)",
  "BLOOD (LYME)",
  "BLOOD (TOXO)",
  "BLOOD BAG FLUID",
  "BLOOD CULTURE (POST-MORTEM)",
  "BONE MARROW - CYTOGENETICS",
  "C, E, & A SCREENING",
  "CHORIONIC VILLUS SAMPLE",
  "CIPRO RESISTANT SCREEN",
  "CRE SCREEN",
  "IMMUNOLOGY",
  "IMMUNOLOGY (CMV)",
  "INFECTION CONTROL YEAST",
  "ISOLATE",
  "MICRO PROBLEM PATIENT",
  "MRSA SCREEN",
  "NEOPLASTIC BLOOD",
  "PERIPHERAL BLOOD LYMPHOCYTES",
  "POST-MORTEM VIRAL CULTURE",
  "POSTMORTEM CULTURE",
  "SEROLOGY/BLOOD",
  "STAPH AUREUS SWAB",
  "SWAB - R/O YEAST",
  "SWAB R/O YEAST SCREEN",
  "TOUCH PREP/SECTIONS",
  "XXX"
)


infection_excluded_microbiology_specimen_sql <- paste(
  vapply(
    infection_excluded_microbiology_specimen_types,
    quote_infection_sql_value,
    character(1L)
  ),
  collapse = ", "
)


# ------------------------------------------------------------------------------
# 38.4 Collapse microbiology rows to one specimen
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  paste0(
    "
    CREATE OR REPLACE TEMP TABLE
      infection_microbiology_specimens_all AS

    WITH parsed_rows AS (
      SELECT
        CASE
          WHEN upper(
            trim(
              coalesce(
                CAST(microevent_id AS VARCHAR),
                ''
              )
            )
          ) IN ('', 'NA', 'NAN', 'NULL')
          THEN NULL
          ELSE trim(
            CAST(microevent_id AS VARCHAR)
          )
        END AS microevent_id,

        CASE
          WHEN upper(
            trim(
              coalesce(
                CAST(subject_id AS VARCHAR),
                ''
              )
            )
          ) IN ('', 'NA', 'NAN', 'NULL')
          THEN NULL
          ELSE trim(
            CAST(subject_id AS VARCHAR)
          )
        END AS subject_id,

        CASE
          WHEN upper(
            trim(
              coalesce(
                CAST(hadm_id AS VARCHAR),
                ''
              )
            )
          ) IN ('', 'NA', 'NAN', 'NULL')
          THEN NULL
          ELSE trim(
            CAST(hadm_id AS VARCHAR)
          )
        END AS hadm_id,

        CASE
          WHEN upper(
            trim(
              coalesce(
                CAST(micro_specimen_id AS VARCHAR),
                ''
              )
            )
          ) IN ('', 'NA', 'NAN', 'NULL')
          THEN NULL
          ELSE trim(
            CAST(micro_specimen_id AS VARCHAR)
          )
        END AS micro_specimen_id,

        TRY_CAST(
          trim(
            CAST(chartdate AS VARCHAR)
          ) AS DATE
        ) AS chartdate,

        CASE
          WHEN length(
            trim(
              CAST(charttime AS VARCHAR)
            )
          ) >= 19
          THEN TRY_CAST(
            replace(
              trim(
                CAST(charttime AS VARCHAR)
              ),
              'T',
              ' '
            ) AS TIMESTAMP
          )
        END AS charttime,

        upper(
          trim(
            coalesce(
              CAST(spec_type_desc AS VARCHAR),
              ''
            )
          )
        ) AS specimen_type_normalized,

        NULLIF(
          trim(
            CAST(spec_type_desc AS VARCHAR)
          ),
          ''
        ) AS specimen_type_original

      FROM infection_microbiology_source_raw
    ),

    keyed_rows AS (
      SELECT
        *,

        CASE
          WHEN micro_specimen_id IS NOT NULL
          THEN concat(
            'SPEC_',
            micro_specimen_id
          )

          WHEN microevent_id IS NOT NULL
          THEN concat(
            'EVENT_',
            microevent_id
          )
        END AS microbiology_specimen_key

      FROM parsed_rows
    ),

    collapsed_specimens AS (
      SELECT
        microbiology_specimen_key,

        MAX(micro_specimen_id)
          AS micro_specimen_id,

        MAX(subject_id)
          AS subject_id,

        MAX(hadm_id)
          AS hadm_id,

        MAX(chartdate)
          AS chartdate,

        MAX(charttime)
          AS microbiology_event_time,

        MAX(specimen_type_normalized)
          AS specimen_type_normalized,

        MAX(specimen_type_original)
          AS specimen_type_original,

        COUNT(*)::BIGINT
          AS microbiology_row_count,

        COUNT(
          DISTINCT microevent_id
        )::BIGINT
          AS microbiology_microevent_count,

        COUNT(
          DISTINCT subject_id
        )::BIGINT
          AS microbiology_subject_count,

        COUNT(
          DISTINCT hadm_id
        ) FILTER (
          WHERE hadm_id IS NOT NULL
        )::BIGINT
          AS microbiology_hadm_count,

        COUNT(
          DISTINCT specimen_type_normalized
        )::BIGINT
          AS microbiology_specimen_type_count,

        COUNT(
          DISTINCT charttime
        ) FILTER (
          WHERE charttime IS NOT NULL
        )::BIGINT
          AS microbiology_charttime_count

      FROM keyed_rows

      WHERE microbiology_specimen_key IS NOT NULL

      GROUP BY microbiology_specimen_key
    )

    SELECT
      *,

      CASE
        WHEN microbiology_event_time IS NOT NULL
        THEN 'exact_timestamp'

        WHEN chartdate IS NOT NULL
        THEN 'date_only'

        ELSE 'missing'
      END AS microbiology_time_precision,

      (
        specimen_type_normalized != ''

        AND specimen_type_normalized NOT IN (
          ",
    infection_excluded_microbiology_specimen_sql,
    "
        )

        AND microbiology_event_time IS NOT NULL
      ) AS microbiology_primary_eligible_flag,

      specimen_type_normalized IN (
        ",
    infection_excluded_microbiology_specimen_sql,
    "
      ) AS excluded_microbiology_specimen_flag,

      (
        microbiology_subject_count > 1
        OR microbiology_hadm_count > 1
        OR microbiology_specimen_type_count > 1
        OR microbiology_charttime_count > 1
      ) AS microbiology_specimen_limitation_flag

    FROM collapsed_specimens;
    "
  )
)

# ------------------------------------------------------------------------------
# 38.5 Restrict specimens to candidate subjects
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    infection_microbiology_specimens AS

  SELECT
    specimen.*

  FROM infection_microbiology_specimens_all AS specimen

  INNER JOIN infection_candidate_subjects AS candidate
    ON specimen.subject_id =
       candidate.subject_id;
  "
)


# ------------------------------------------------------------------------------
# 38.6 Microbiology validation
# ------------------------------------------------------------------------------

infection_microbiology_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS candidate_microbiology_specimen_n,

    COUNT(DISTINCT microbiology_specimen_key)
      AS distinct_microbiology_specimen_n,

    COUNT(*) FILTER (
      WHERE microbiology_primary_eligible_flag
    ) AS primary_eligible_microbiology_specimen_n,

    COUNT(*) FILTER (
      WHERE excluded_microbiology_specimen_flag
    ) AS excluded_specimen_type_n,

    COUNT(*) FILTER (
      WHERE microbiology_time_precision =
        'exact_timestamp'
    ) AS exact_timestamp_specimen_n,

    COUNT(*) FILTER (
      WHERE microbiology_time_precision =
        'date_only'
    ) AS date_only_specimen_n,

    COUNT(*) FILTER (
      WHERE microbiology_time_precision =
        'missing'
    ) AS missing_time_specimen_n,

    COUNT(*) FILTER (
      WHERE microbiology_specimen_limitation_flag
    ) AS specimen_limitation_n,

    COUNT(*) FILTER (
      WHERE microbiology_primary_eligible_flag
        AND microbiology_event_time IS NULL
    ) AS eligible_without_exact_timestamp_n,

    COUNT(*) FILTER (
      WHERE microbiology_primary_eligible_flag
        AND excluded_microbiology_specimen_flag
    ) AS eligible_but_excluded_type_n,

    COUNT(*) FILTER (
      WHERE subject_id IS NULL
         OR subject_id = ''
    ) AS missing_subject_id_n,

    COUNT(*) FILTER (
      WHERE microbiology_subject_count > 1
    ) AS multiple_subject_specimen_n,

    COUNT(*) FILTER (
      WHERE microbiology_hadm_count > 1
    ) AS multiple_hadm_specimen_n,

    COUNT(*) FILTER (
      WHERE microbiology_specimen_type_count > 1
    ) AS multiple_type_specimen_n,

    COUNT(*) FILTER (
      WHERE microbiology_charttime_count > 1
    ) AS multiple_charttime_specimen_n

  FROM infection_microbiology_specimens;
  "
) %>%
  tibble::as_tibble()


if (
  infection_microbiology_check$candidate_microbiology_specimen_n !=
  infection_microbiology_check$distinct_microbiology_specimen_n ||
  infection_microbiology_check$eligible_without_exact_timestamp_n != 0L ||
  infection_microbiology_check$eligible_but_excluded_type_n != 0L ||
  infection_microbiology_check$missing_subject_id_n != 0L
) {
  stop(
    "Primary-eligible microbiology specimens failed validation.",
    call. = FALSE
  )
}


print(infection_microbiology_check, width = Inf)
# ==============================================================================
# 39. PRIMARY ANTIMICROBIAL-MICROBIOLOGY PAIRING
# ==============================================================================


# ------------------------------------------------------------------------------
# 39.1 Verify candidate-cohort columns
# ------------------------------------------------------------------------------

infection_candidate_columns <- DBI::dbListFields(
  con,
  "candidate_cohort_working_table"
)


required_infection_candidate_columns <- c(
  "episode_id",
  "subject_id",
  "hadm_id",
  "ed_intime"
)


missing_infection_candidate_columns <- setdiff(
  required_infection_candidate_columns,
  infection_candidate_columns
)


if (length(missing_infection_candidate_columns) > 0L) {
  stop(
    paste0(
      "The candidate cohort is missing required column(s): ",
      paste(
        missing_infection_candidate_columns,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 39.2 Build optional-column SQL expressions
# ------------------------------------------------------------------------------

infection_episode_outtime_expression <- if (
  "ed_outtime" %in% infection_candidate_columns
) {
  "
  TRY_CAST(
    replace(
      trim(
        CAST(ed_outtime AS VARCHAR)
      ),
      'T',
      ' '
    ) AS TIMESTAMP
  )
  "
} else {
  "
  CAST(NULL AS TIMESTAMP)
  "
}


infection_episode_stay_expression <- if (
  "stay_id" %in% infection_candidate_columns
) {
  "
  NULLIF(
    trim(
      CAST(stay_id AS VARCHAR)
    ),
    ''
  )
  "
} else if (
  "ed_stay_id" %in% infection_candidate_columns
) {
  "
  NULLIF(
    trim(
      CAST(ed_stay_id AS VARCHAR)
    ),
    ''
  )
  "
} else {
  "
  CAST(NULL AS VARCHAR)
  "
}


# ------------------------------------------------------------------------------
# 39.3 Construct the ED episode denominator
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  paste0(
    "
    CREATE OR REPLACE TEMP TABLE
      infection_episode_base AS

    SELECT DISTINCT
      NULLIF(
        trim(
          CAST(episode_id AS VARCHAR)
        ),
        ''
      ) AS episode_id,

      NULLIF(
        trim(
          CAST(subject_id AS VARCHAR)
        ),
        ''
      ) AS subject_id,

      CASE
        WHEN upper(
          trim(
            coalesce(
              CAST(hadm_id AS VARCHAR),
              ''
            )
          )
        ) IN (
          '',
          'NA',
          'NAN',
          'NULL'
        )
        THEN NULL

        ELSE trim(
          CAST(hadm_id AS VARCHAR)
        )
      END AS hadm_id,

      TRY_CAST(
        replace(
          trim(
            CAST(ed_intime AS VARCHAR)
          ),
          'T',
          ' '
        ) AS TIMESTAMP
      ) AS ed_intime,

      ",
    infection_episode_outtime_expression,
    " AS ed_outtime,

      ",
    infection_episode_stay_expression,
    " AS episode_stay_id

    FROM candidate_cohort_working_table;
    "
  )
)


# ------------------------------------------------------------------------------
# 39.4 Restrict events to maximum relevant temporal ranges
# ------------------------------------------------------------------------------

# Antimicrobials may occur up to 96 h after ED arrival because:
# suspected-infection onset must occur within 24 h, and a microbiology-first
# pair may be followed by antimicrobial therapy up to 72 h later.

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    infection_antimicrobial_pool AS

  SELECT
    antimicrobial.*

  FROM infection_antimicrobial_prescriptions
    AS antimicrobial

  WHERE antimicrobial.antimicrobial_primary_flag
    AND antimicrobial.antimicrobial_starttime IS NOT NULL
    AND antimicrobial.hadm_id IS NOT NULL

    AND EXISTS (
      SELECT 1

      FROM infection_episode_base AS episode

      WHERE episode.subject_id =
              antimicrobial.subject_id

        AND episode.hadm_id =
              antimicrobial.hadm_id

        AND antimicrobial.antimicrobial_starttime >=
              episode.ed_intime

        AND antimicrobial.antimicrobial_starttime <=
              episode.ed_intime + INTERVAL '96 hours'
    );
  "
)


# Microbiology may occur up to 48 h after ED arrival because:
# suspected-infection onset must occur within 24 h, and an antimicrobial-first
# pair may be followed by microbiology up to 24 h later.

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    infection_microbiology_pool AS

  SELECT
    microbiology.*

  FROM infection_microbiology_specimens
    AS microbiology

  WHERE microbiology.microbiology_primary_eligible_flag
    AND microbiology.microbiology_event_time IS NOT NULL

    AND EXISTS (
      SELECT 1

      FROM infection_episode_base AS episode

      WHERE episode.subject_id =
              microbiology.subject_id

        AND (
          microbiology.hadm_id IS NULL
          OR microbiology.hadm_id =
             episode.hadm_id
        )

        AND microbiology.microbiology_event_time >=
              episode.ed_intime

        AND microbiology.microbiology_event_time <=
              episode.ed_intime + INTERVAL '48 hours'
    );
  "
)


# ------------------------------------------------------------------------------
# 39.5 Construct valid 24/72-hour pairs
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    infection_candidate_pairs AS

  WITH both_events_same_hadm AS (
    SELECT
      antimicrobial.antimicrobial_event_id,
      microbiology.microbiology_specimen_key,

      antimicrobial.subject_id,

      antimicrobial.hadm_id
        AS antimicrobial_hadm_id,

      microbiology.hadm_id
        AS microbiology_hadm_id,

      antimicrobial.hadm_id
        AS pair_hadm_id,

      'both_events_same_hadm'
        AS infection_pair_linkage_category,

      antimicrobial.antimicrobial_starttime,
      microbiology.microbiology_event_time,

      antimicrobial.antimicrobial_name,
      antimicrobial.antimicrobial_route,

      microbiology.specimen_type_normalized,

      microbiology.microbiology_specimen_limitation_flag

    FROM infection_antimicrobial_pool
      AS antimicrobial

    INNER JOIN infection_microbiology_pool
      AS microbiology

      ON antimicrobial.subject_id =
           microbiology.subject_id

     AND microbiology.hadm_id IS NOT NULL

     AND antimicrobial.hadm_id =
           microbiology.hadm_id
  ),

  antimicrobial_hadm_time_linkage AS (
    SELECT
      antimicrobial.antimicrobial_event_id,
      microbiology.microbiology_specimen_key,

      antimicrobial.subject_id,

      antimicrobial.hadm_id
        AS antimicrobial_hadm_id,

      microbiology.hadm_id
        AS microbiology_hadm_id,

      antimicrobial.hadm_id
        AS pair_hadm_id,

      'antimicrobial_hadm_time_linkage'
        AS infection_pair_linkage_category,

      antimicrobial.antimicrobial_starttime,
      microbiology.microbiology_event_time,

      antimicrobial.antimicrobial_name,
      antimicrobial.antimicrobial_route,

      microbiology.specimen_type_normalized,

      microbiology.microbiology_specimen_limitation_flag

    FROM infection_antimicrobial_pool
      AS antimicrobial

    INNER JOIN infection_microbiology_pool
      AS microbiology

      ON antimicrobial.subject_id =
           microbiology.subject_id

    WHERE microbiology.hadm_id IS NULL
  ),

  all_candidate_pairs AS (
    SELECT *
    FROM both_events_same_hadm

    UNION ALL

    SELECT *
    FROM antimicrobial_hadm_time_linkage
  ),

  temporally_valid_pairs AS (
    SELECT
      *,

      least(
        antimicrobial_starttime,
        microbiology_event_time
      ) AS suspected_infection_onset,

      abs(
        date_diff(
          'second',
          antimicrobial_starttime,
          microbiology_event_time
        )
      ) / 3600.0 AS infection_pair_gap_hours,

      CASE
        WHEN antimicrobial_starttime <=
             microbiology_event_time
        THEN 'antimicrobial_first_or_same'

        ELSE 'microbiology_first'
      END AS infection_pair_temporal_order

    FROM all_candidate_pairs

    WHERE (
      antimicrobial_starttime <=
        microbiology_event_time

      AND microbiology_event_time <=
        antimicrobial_starttime + INTERVAL '24 hours'
    )

    OR (
      microbiology_event_time <
        antimicrobial_starttime

      AND antimicrobial_starttime <=
        microbiology_event_time + INTERVAL '72 hours'
    )
  )

  SELECT
    concat(
      'PAIR_',
      md5(
        concat_ws(
          '|',
          antimicrobial_event_id,
          microbiology_specimen_key
        )
      )
    ) AS infection_pair_id,

    *

  FROM temporally_valid_pairs AS pair

  WHERE EXISTS (
    SELECT 1

    FROM infection_episode_base AS episode

    WHERE episode.subject_id =
            pair.subject_id

      AND episode.hadm_id =
            pair.pair_hadm_id

      AND pair.suspected_infection_onset >=
            episode.ed_intime

      AND pair.suspected_infection_onset <=
            episode.ed_intime + INTERVAL '24 hours'
  );
  "
)

# ==============================================================================
# 39.6 PRIMARY PAIRING VALIDATION
# ==============================================================================

infection_pairing_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    (
      SELECT COUNT(*)
      FROM infection_episode_base
    ) AS episode_n,

    (
      SELECT
        COUNT(*) - COUNT(DISTINCT episode_id)
      FROM infection_episode_base
    ) AS duplicated_episode_n,

    (
      SELECT COUNT(*)
      FROM infection_antimicrobial_pool
    ) AS antimicrobial_pool_event_n,

    (
      SELECT
        COUNT(*) -
        COUNT(DISTINCT antimicrobial_event_id)
      FROM infection_antimicrobial_pool
    ) AS duplicated_antimicrobial_pool_event_n,

    (
      SELECT COUNT(*)
      FROM infection_microbiology_pool
    ) AS microbiology_pool_specimen_n,

    (
      SELECT
        COUNT(*) -
        COUNT(DISTINCT microbiology_specimen_key)
      FROM infection_microbiology_pool
    ) AS duplicated_microbiology_pool_specimen_n,

    (
      SELECT COUNT(*)
      FROM infection_candidate_pairs
    ) AS qualifying_pair_n,

    (
      SELECT
        COUNT(*) -
        COUNT(DISTINCT infection_pair_id)
      FROM infection_candidate_pairs
    ) AS duplicated_pair_n,

    (
      SELECT COUNT(*)
      FROM infection_candidate_pairs
      WHERE antimicrobial_event_id IS NULL
         OR microbiology_specimen_key IS NULL
         OR subject_id IS NULL
         OR pair_hadm_id IS NULL
         OR antimicrobial_starttime IS NULL
         OR microbiology_event_time IS NULL
         OR suspected_infection_onset IS NULL
    ) AS pair_missing_required_field_n,

    (
      SELECT COUNT(*)
      FROM infection_candidate_pairs
      WHERE NOT (
        (
          antimicrobial_starttime <=
            microbiology_event_time

          AND microbiology_event_time <=
            antimicrobial_starttime + INTERVAL '24 hours'
        )

        OR (
          microbiology_event_time <
            antimicrobial_starttime

          AND antimicrobial_starttime <=
            microbiology_event_time + INTERVAL '72 hours'
        )
      )
    ) AS invalid_temporal_window_pair_n,

    (
      SELECT COUNT(*)
      FROM infection_candidate_pairs AS pair
      WHERE NOT EXISTS (
        SELECT 1

        FROM infection_episode_base AS episode

        WHERE episode.subject_id =
                pair.subject_id

          AND episode.hadm_id =
                pair.pair_hadm_id

          AND pair.suspected_infection_onset >=
                episode.ed_intime

          AND pair.suspected_infection_onset <=
                episode.ed_intime + INTERVAL '24 hours'
      )
    ) AS pair_without_eligible_episode_n,

    (
      SELECT COUNT(*)
      FROM infection_candidate_pairs
      WHERE infection_pair_temporal_order =
        'antimicrobial_first_or_same'
    ) AS antimicrobial_first_pair_n,

    (
      SELECT COUNT(*)
      FROM infection_candidate_pairs
      WHERE infection_pair_temporal_order =
        'microbiology_first'
    ) AS microbiology_first_pair_n,

    (
      SELECT COUNT(*)
      FROM infection_candidate_pairs
      WHERE infection_pair_linkage_category =
        'both_events_same_hadm'
    ) AS same_hadm_pair_n,

    (
      SELECT COUNT(*)
      FROM infection_candidate_pairs
      WHERE infection_pair_linkage_category =
        'antimicrobial_hadm_time_linkage'
    ) AS microbiology_missing_hadm_pair_n;
  "
) %>%
  tibble::as_tibble()


if (
  infection_pairing_check$episode_n !=
  sofa_integration_check$output_episode_n ||
  infection_pairing_check$duplicated_episode_n != 0L ||
  infection_pairing_check$
  duplicated_antimicrobial_pool_event_n != 0L ||
  infection_pairing_check$
  duplicated_microbiology_pool_specimen_n != 0L ||
  infection_pairing_check$duplicated_pair_n != 0L ||
  infection_pairing_check$
  pair_missing_required_field_n != 0L ||
  infection_pairing_check$
  invalid_temporal_window_pair_n != 0L ||
  infection_pairing_check$
  pair_without_eligible_episode_n != 0L
) {
  stop(
    "Primary antimicrobial-microbiology pairing failed validation.",
    call. = FALSE
  )
}


print(
  infection_pairing_check,
  width = Inf
)

# ==============================================================================
# 40. EPISODE ASSIGNMENT AND PRIMARY SUSPECTED-INFECTION PHENOTYPE
# ==============================================================================


# ------------------------------------------------------------------------------
# 40.1 Assign every qualifying pair to exactly one ED episode
#
# Assignment hierarchy:
# 1. Onset inside the recorded ED interval.
# 2. Smallest nonnegative time from ED arrival.
# 3. Latest ED arrival.
# 4. Smallest numerical stay identifier.
# 5. Smallest text stay identifier.
# 6. Smallest episode identifier.
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    infection_pairs_assigned AS

  SELECT
    pair.*,

    episode.episode_id,
    episode.ed_intime,
    episode.ed_outtime,
    episode.episode_stay_id,

    date_diff(
      'second',
      episode.ed_intime,
      pair.suspected_infection_onset
    ) / 3600.0
      AS suspected_infection_hours_from_ed,

    episode.ed_outtime IS NOT NULL
      AND pair.suspected_infection_onset
        BETWEEN episode.ed_intime AND episode.ed_outtime
      AS infection_onset_inside_ed_interval_flag,

    CASE
      WHEN episode.ed_outtime IS NULL
        THEN NULL
      ELSE pair.suspected_infection_onset >
        episode.ed_outtime
    END AS infection_onset_after_ed_outtime_flag,

    COUNT(*) OVER (
      PARTITION BY pair.infection_pair_id
    ) AS infection_candidate_episode_count

  FROM infection_candidate_pairs AS pair

  INNER JOIN infection_episode_base AS episode
    ON pair.subject_id = episode.subject_id
   AND pair.pair_hadm_id = episode.hadm_id
   AND pair.suspected_infection_onset >=
       episode.ed_intime
   AND pair.suspected_infection_onset <=
       episode.ed_intime + INTERVAL '24 hours'

  QUALIFY
    ROW_NUMBER() OVER (
      PARTITION BY pair.infection_pair_id

      ORDER BY
        CASE
          WHEN episode.ed_outtime IS NOT NULL
            AND pair.suspected_infection_onset
              BETWEEN episode.ed_intime
                  AND episode.ed_outtime
          THEN 0
          ELSE 1
        END,

        date_diff(
          'second',
          episode.ed_intime,
          pair.suspected_infection_onset
        ),

        episode.ed_intime DESC,

        CASE
          WHEN TRY_CAST(
            episode.episode_stay_id AS BIGINT
          ) IS NULL
          THEN 1
          ELSE 0
        END,

        TRY_CAST(
          episode.episode_stay_id AS BIGINT
        ),

        episode.episode_stay_id,
        episode.episode_id
    ) = 1;
  "
)


# ------------------------------------------------------------------------------
# 40.2 Validate unique pair-to-episode assignment
# ------------------------------------------------------------------------------

infection_assignment_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS assigned_pair_n,

    COUNT(DISTINCT infection_pair_id)
      AS distinct_assigned_pair_n,

    COUNT(*) FILTER (
      WHERE episode_id IS NULL
    ) AS missing_episode_id_n,

    COUNT(*) FILTER (
      WHERE suspected_infection_hours_from_ed < 0
    ) AS negative_onset_time_n,

    COUNT(*) FILTER (
      WHERE suspected_infection_hours_from_ed > 24
    ) AS onset_after_24_hours_n,

    COUNT(*) FILTER (
      WHERE infection_candidate_episode_count < 1
    ) AS invalid_candidate_episode_count_n,

    COUNT(*) FILTER (
      WHERE NOT EXISTS (
        SELECT 1

        FROM infection_episode_base AS episode

        WHERE episode.episode_id =
                infection_pairs_assigned.episode_id

          AND episode.hadm_id =
                infection_pairs_assigned.pair_hadm_id

          AND episode.subject_id =
                infection_pairs_assigned.subject_id
      )
    ) AS pair_episode_key_mismatch_n,

    COUNT(*) FILTER (
      WHERE infection_candidate_episode_count > 1
    ) AS pair_with_multiple_candidate_episodes_n,

    MAX(infection_candidate_episode_count)
      AS maximum_candidate_episode_count

  FROM infection_pairs_assigned;
  "
) %>%
  tibble::as_tibble()


if (
  infection_assignment_check$assigned_pair_n !=
  infection_assignment_check$distinct_assigned_pair_n ||
  infection_assignment_check$missing_episode_id_n != 0L ||
  infection_assignment_check$negative_onset_time_n != 0L ||
  infection_assignment_check$onset_after_24_hours_n != 0L ||
  infection_assignment_check$
  invalid_candidate_episode_count_n != 0L ||
  infection_assignment_check$
  pair_episode_key_mismatch_n != 0L
) {
  stop(
    "Infection-pair episode assignment failed validation.",
    call. = FALSE
  )
}


print(
  infection_assignment_check,
  width = Inf
)


# ------------------------------------------------------------------------------
# 40.3 Select one deterministic index pair per positive episode
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    suspected_infection_positive_episode_base AS

  SELECT
    episode_id,
    subject_id,

    infection_pair_id,

    suspected_infection_onset,
    suspected_infection_hours_from_ed,

    infection_pair_linkage_category,
    infection_pair_temporal_order,
    infection_pair_gap_hours,

    antimicrobial_event_id
      AS infection_index_antimicrobial_event_id,

    antimicrobial_name
      AS infection_index_antimicrobial_name,

    antimicrobial_route
      AS infection_index_antimicrobial_route,

    microbiology_specimen_key
      AS infection_index_microbiology_specimen_key,

    specimen_type_normalized
      AS infection_index_microbiology_specimen_type,

    COUNT(*) OVER (
      PARTITION BY episode_id
    ) AS infection_qualifying_pair_count,

    infection_candidate_episode_count,

    infection_onset_inside_ed_interval_flag,

    infection_onset_after_ed_outtime_flag,

    microbiology_specimen_limitation_flag

  FROM infection_pairs_assigned

  QUALIFY
    ROW_NUMBER() OVER (
      PARTITION BY episode_id

      ORDER BY
        suspected_infection_onset,

        CASE
          WHEN infection_pair_linkage_category =
            'both_events_same_hadm'
          THEN 0
          ELSE 1
        END,

        infection_pair_gap_hours,
        antimicrobial_event_id,
        microbiology_specimen_key,
        infection_pair_id
    ) = 1;
  "
)


# ------------------------------------------------------------------------------
# 40.4 Detect reuse and structural limitations
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    suspected_infection_positive_episode AS

  WITH antimicrobial_reuse AS (
    SELECT
      infection_index_antimicrobial_event_id,

      COUNT(DISTINCT episode_id)
        AS episode_count

    FROM suspected_infection_positive_episode_base

    GROUP BY
      infection_index_antimicrobial_event_id
  ),

  microbiology_reuse AS (
    SELECT
      infection_index_microbiology_specimen_key,

      COUNT(DISTINCT episode_id)
        AS episode_count

    FROM suspected_infection_positive_episode_base

    GROUP BY
      infection_index_microbiology_specimen_key
  ),

  pair_reuse AS (
    SELECT
      infection_pair_id,

      COUNT(DISTINCT episode_id)
        AS episode_count

    FROM suspected_infection_positive_episode_base

    GROUP BY infection_pair_id
  )

  SELECT
    positive.*,

    antimicrobial_reuse.episode_count
      AS infection_antimicrobial_reuse_episode_count,

    microbiology_reuse.episode_count
      AS infection_microbiology_reuse_episode_count,

    pair_reuse.episode_count
      AS infection_pair_reuse_episode_count,

    antimicrobial_reuse.episode_count > 1
      OR microbiology_reuse.episode_count > 1
      OR pair_reuse.episode_count > 1
      AS infection_event_reuse_flag,

    positive.infection_qualifying_pair_count > 1
      AS infection_multiple_qualifying_pairs_flag,

    positive.infection_qualifying_pair_count > 1
      OR positive.infection_candidate_episode_count > 1
      OR antimicrobial_reuse.episode_count > 1
      OR microbiology_reuse.episode_count > 1
      OR pair_reuse.episode_count > 1
      OR positive.microbiology_specimen_limitation_flag
      AS infection_structural_limitation_flag

  FROM suspected_infection_positive_episode_base
    AS positive

  INNER JOIN antimicrobial_reuse
    USING (infection_index_antimicrobial_event_id)

  INNER JOIN microbiology_reuse
    USING (infection_index_microbiology_specimen_key)

  INNER JOIN pair_reuse
    USING (infection_pair_id);
  "
)

# ------------------------------------------------------------------------------
# 40.5 Construct one phenotype row for every candidate episode
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    suspected_infection_episode AS

  SELECT
    episode.episode_id,

    CAST(
      positive.episode_id IS NOT NULL
      AS INTEGER
    ) AS suspected_infection_primary_flag,

    positive.suspected_infection_onset,
    positive.suspected_infection_hours_from_ed,

    positive.infection_pair_id,

    positive.infection_pair_linkage_category,
    positive.infection_pair_temporal_order,
    positive.infection_pair_gap_hours,

    positive.infection_index_antimicrobial_event_id,
    positive.infection_index_antimicrobial_name,
    positive.infection_index_antimicrobial_route,

    positive.infection_index_microbiology_specimen_key,
    positive.infection_index_microbiology_specimen_type,

    coalesce(
      positive.infection_qualifying_pair_count,
      0
    )::BIGINT
      AS infection_qualifying_pair_count,

    coalesce(
      CAST(
        positive.infection_multiple_qualifying_pairs_flag
        AS INTEGER
      ),
      0
    ) AS infection_multiple_qualifying_pairs_flag,

    coalesce(
      positive.infection_candidate_episode_count,
      0
    )::BIGINT
      AS infection_candidate_episode_count,

    CASE
      WHEN positive.episode_id IS NULL
        THEN NULL
      ELSE CAST(
        positive.infection_onset_inside_ed_interval_flag
        AS INTEGER
      )
    END AS infection_onset_inside_ed_interval_flag,

    CASE
      WHEN positive.episode_id IS NULL
        THEN NULL
      WHEN positive.infection_onset_after_ed_outtime_flag
        IS NULL
        THEN NULL
      ELSE CAST(
        positive.infection_onset_after_ed_outtime_flag
        AS INTEGER
      )
    END AS infection_onset_after_ed_outtime_flag,

    coalesce(
      positive.infection_antimicrobial_reuse_episode_count,
      0
    )::BIGINT
      AS infection_antimicrobial_reuse_episode_count,

    coalesce(
      positive.infection_microbiology_reuse_episode_count,
      0
    )::BIGINT
      AS infection_microbiology_reuse_episode_count,

    coalesce(
      positive.infection_pair_reuse_episode_count,
      0
    )::BIGINT
      AS infection_pair_reuse_episode_count,

    coalesce(
      CAST(
        positive.infection_event_reuse_flag
        AS INTEGER
      ),
      0
    ) AS infection_event_reuse_flag,

    coalesce(
      CAST(
        positive.infection_structural_limitation_flag
        AS INTEGER
      ),
      0
    ) AS infection_structural_limitation_flag

  FROM infection_episode_base AS episode

  LEFT JOIN suspected_infection_positive_episode
    AS positive
    USING (episode_id);
  "
)


# ------------------------------------------------------------------------------
# 40.6 Validate the episode-level suspected-infection phenotype
# ------------------------------------------------------------------------------

suspected_infection_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS episode_n,

    COUNT(DISTINCT episode_id)
      AS distinct_episode_n,

    COUNT(*) FILTER (
      WHERE suspected_infection_primary_flag = 1
    ) AS suspected_infection_episode_n,

    COUNT(*) FILTER (
      WHERE suspected_infection_primary_flag = 0
    ) AS no_suspected_infection_episode_n,

    COUNT(*) FILTER (
      WHERE suspected_infection_primary_flag
        NOT IN (0, 1)
    ) AS invalid_primary_flag_n,

    COUNT(*) FILTER (
      WHERE suspected_infection_primary_flag = 1
        AND (
          suspected_infection_onset IS NULL
          OR infection_pair_id IS NULL
          OR infection_index_antimicrobial_event_id IS NULL
          OR infection_index_microbiology_specimen_key IS NULL
        )
    ) AS positive_episode_missing_index_event_n,

    COUNT(*) FILTER (
      WHERE suspected_infection_primary_flag = 0
        AND (
          suspected_infection_onset IS NOT NULL
          OR infection_pair_id IS NOT NULL
          OR infection_qualifying_pair_count != 0
        )
    ) AS negative_episode_with_positive_evidence_n,

    COUNT(*) FILTER (
      WHERE suspected_infection_primary_flag = 1
        AND suspected_infection_hours_from_ed
          NOT BETWEEN 0 AND 24
    ) AS positive_onset_outside_0_24h_n,

    COUNT(*) FILTER (
      WHERE suspected_infection_primary_flag = 1
        AND infection_qualifying_pair_count < 1
    ) AS positive_without_qualifying_pair_n,

    COUNT(*) FILTER (
      WHERE infection_multiple_qualifying_pairs_flag = 1
    ) AS episode_with_multiple_qualifying_pairs_n,

    COUNT(*) FILTER (
      WHERE infection_event_reuse_flag = 1
    ) AS episode_with_event_reuse_n,

    COUNT(*) FILTER (
      WHERE infection_structural_limitation_flag = 1
    ) AS episode_with_structural_limitation_n

  FROM suspected_infection_episode;
  "
) %>%
  tibble::as_tibble()


if (
  suspected_infection_check$episode_n !=
  suspected_infection_check$distinct_episode_n ||
  suspected_infection_check$episode_n !=
  infection_pairing_check$episode_n ||
  suspected_infection_check$invalid_primary_flag_n != 0L ||
  suspected_infection_check$
  positive_episode_missing_index_event_n != 0L ||
  suspected_infection_check$
  negative_episode_with_positive_evidence_n != 0L ||
  suspected_infection_check$
  positive_onset_outside_0_24h_n != 0L ||
  suspected_infection_check$
  positive_without_qualifying_pair_n != 0L
) {
  stop(
    "Episode-level suspected-infection phenotype failed validation.",
    call. = FALSE
  )
}


print(
  suspected_infection_check,
  width = Inf
)

# ------------------------------------------------------------------------------
# 40.7 Integrate suspected-infection fields into the working cohort
# ------------------------------------------------------------------------------

infection_output_columns <- setdiff(
  DBI::dbListFields(
    con,
    "suspected_infection_episode"
  ),
  "episode_id"
)


candidate_columns_before_infection <- DBI::dbListFields(
  con,
  "candidate_cohort_working_table"
)


candidate_base_columns_before_infection <- setdiff(
  candidate_columns_before_infection,
  infection_output_columns
)


candidate_base_column_sql <- paste(
  vapply(
    candidate_base_columns_before_infection,
    function(column_name) {
      paste0(
        "cohort.",
        as.character(
          DBI::dbQuoteIdentifier(
            con,
            column_name
          )
        )
      )
    },
    character(1L)
  ),
  collapse = ",\n    "
)


infection_addition_column_sql <- paste(
  vapply(
    infection_output_columns,
    function(column_name) {
      paste0(
        "infection.",
        as.character(
          DBI::dbQuoteIdentifier(
            con,
            column_name
          )
        )
      )
    },
    character(1L)
  ),
  collapse = ",\n    "
)


DBI::dbExecute(
  con,
  paste0(
    "
    CREATE OR REPLACE TEMP TABLE
      candidate_cohort_before_infection_table AS

    SELECT
      ",
    candidate_base_column_sql,
    "

    FROM candidate_cohort_working_table AS cohort;
    "
  )
)


DBI::dbExecute(
  con,
  paste0(
    "
    CREATE OR REPLACE TEMP TABLE
      candidate_cohort_with_infection_table AS

    SELECT
      ",
    candidate_base_column_sql,
    ",
      ",
    infection_addition_column_sql,
    "

    FROM candidate_cohort_before_infection_table
      AS cohort

    INNER JOIN suspected_infection_episode
      AS infection
      USING (episode_id);
    "
  )
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    candidate_cohort_working_table AS

  SELECT *
  FROM candidate_cohort_with_infection_table;
  "
)


# ------------------------------------------------------------------------------
# 40.8 Validate cohort integration
# ------------------------------------------------------------------------------

infection_integration_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    (
      SELECT COUNT(*)
      FROM candidate_cohort_before_infection_table
    ) AS source_episode_n,

    (
      SELECT COUNT(*)
      FROM suspected_infection_episode
    ) AS phenotype_episode_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
    ) AS output_episode_n,

    (
      SELECT
        COUNT(*) - COUNT(DISTINCT episode_id)
      FROM candidate_cohort_working_table
    ) AS duplicated_output_episode_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
      WHERE suspected_infection_primary_flag = 1
    ) AS suspected_infection_episode_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
      WHERE suspected_infection_primary_flag IS NULL
    ) AS missing_primary_flag_n;
  "
) %>%
  tibble::as_tibble()


if (
  infection_integration_check$source_episode_n !=
  infection_integration_check$phenotype_episode_n ||
  infection_integration_check$source_episode_n !=
  infection_integration_check$output_episode_n ||
  infection_integration_check$
  duplicated_output_episode_n != 0L ||
  infection_integration_check$missing_primary_flag_n != 0L
) {
  stop(
    "Suspected-infection cohort integration failed.",
    call. = FALSE
  )
}


candidate_cohort_working <- DBI::dbGetQuery(
  con,
  "
  SELECT *
  FROM candidate_cohort_working_table
  ORDER BY episode_id;
  "
) %>%
  tibble::as_tibble()


assert_unique_key(
  candidate_cohort_working,
  "episode_id",
  "candidate_cohort_working"
)


print(
  infection_integration_check,
  width = Inf
)

# ==============================================================================
# 41. PRIMARY SEPSIS-3 PHENOTYPE
# ==============================================================================


# ------------------------------------------------------------------------------
# 41.1 Definition
#
# Primary Sepsis-3 phenotype:
#   suspected infection in the primary 0-24 h window
#   AND structured total SOFA >= 2.
#
# The primary total includes coagulation.
# The no-coagulation total is retained to identify episodes whose classification
# depends exclusively on the platelet component.
# ------------------------------------------------------------------------------

SEPSIS3_SOFA_THRESHOLD <- 2L


required_sepsis3_columns <- c(
  "episode_id",
  "subject_id",
  "suspected_infection_primary_flag",
  "sofa_total_structured_0_24h",
  "sofa_no_coag_structured_0_24h",
  "sofa_all_components_observed_flag",
  "sofa_missing_components_assigned_zero_flag"
)


missing_sepsis3_columns <- setdiff(
  required_sepsis3_columns,
  DBI::dbListFields(
    con,
    "candidate_cohort_working_table"
  )
)


if (length(missing_sepsis3_columns) > 0L) {
  stop(
    paste0(
      "The candidate cohort is missing required Sepsis-3 column(s): ",
      paste(
        missing_sepsis3_columns,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 41.2 Construct one Sepsis-3 row per episode
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  paste0(
    "
    CREATE OR REPLACE TEMP TABLE
      sepsis3_episode AS

    WITH source_values AS (
      SELECT
        trim(
          CAST(episode_id AS VARCHAR)
        ) AS episode_id,

        trim(
          CAST(subject_id AS VARCHAR)
        ) AS subject_id,

        TRY_CAST(
          suspected_infection_primary_flag
          AS INTEGER
        ) AS suspected_infection_primary_flag,

        TRY_CAST(
          sofa_total_structured_0_24h
          AS INTEGER
        ) AS sofa_total_structured_0_24h,

        TRY_CAST(
          sofa_no_coag_structured_0_24h
          AS INTEGER
        ) AS sofa_no_coag_structured_0_24h,

        TRY_CAST(
          sofa_all_components_observed_flag
          AS BOOLEAN
        ) AS sofa_all_components_observed_flag,

        TRY_CAST(
          sofa_missing_components_assigned_zero_flag
          AS BOOLEAN
        ) AS sofa_missing_components_assigned_zero_flag

      FROM candidate_cohort_working_table
    ),

    primary_classification AS (
      SELECT
        source.*,

        CASE
          WHEN
            source.suspected_infection_primary_flag = 1

            AND source.sofa_total_structured_0_24h >=
              ",
    SEPSIS3_SOFA_THRESHOLD,
    "

          THEN 1

          WHEN
            source.suspected_infection_primary_flag IN (0, 1)

            AND source.sofa_total_structured_0_24h
              IS NOT NULL

          THEN 0

          ELSE NULL
        END::INTEGER AS sepsis3_primary_flag

      FROM source_values AS source
    )

    SELECT
      classified.*,

      CASE
        WHEN
          classified.sepsis3_primary_flag = 1

          AND classified.sofa_no_coag_structured_0_24h <
            ",
    SEPSIS3_SOFA_THRESHOLD,
    "

        THEN 1

        WHEN
          classified.sepsis3_primary_flag = 1

          AND classified.sofa_no_coag_structured_0_24h >=
            ",
    SEPSIS3_SOFA_THRESHOLD,
    "

        THEN 0

        ELSE NULL
      END::INTEGER
        AS sepsis_coagulation_dependent_flag

    FROM primary_classification AS classified;
    "
  )
)

# ------------------------------------------------------------------------------
# 41.3 Validate the primary Sepsis-3 classification
# ------------------------------------------------------------------------------

sepsis3_check <- DBI::dbGetQuery(
  con,
  paste0(
    "
    SELECT
      COUNT(*) AS episode_n,

      COUNT(DISTINCT episode_id)
        AS distinct_episode_n,

      COUNT(*) FILTER (
        WHERE sepsis3_primary_flag = 1
      ) AS sepsis_episode_n,

      COUNT(*) FILTER (
        WHERE sepsis3_primary_flag = 0
      ) AS nonsepsis_episode_n,

      COUNT(*) FILTER (
        WHERE sepsis3_primary_flag IS NULL
      ) AS unclassified_episode_n,

      COUNT(*) FILTER (
        WHERE sepsis3_primary_flag NOT IN (0, 1)
      ) AS invalid_sepsis_flag_n,

      COUNT(*) FILTER (
        WHERE sepsis3_primary_flag = 1
          AND suspected_infection_primary_flag != 1
      ) AS sepsis_without_suspected_infection_n,

      COUNT(*) FILTER (
        WHERE sepsis3_primary_flag = 1
          AND sofa_total_structured_0_24h <
            ",
    SEPSIS3_SOFA_THRESHOLD,
    "
      ) AS sepsis_below_sofa_threshold_n,

      COUNT(*) FILTER (
        WHERE sepsis3_primary_flag = 0
          AND suspected_infection_primary_flag = 1
          AND sofa_total_structured_0_24h >=
            ",
    SEPSIS3_SOFA_THRESHOLD,
    "
      ) AS nonsepsis_despite_meeting_definition_n,

      COUNT(*) FILTER (
        WHERE sepsis_coagulation_dependent_flag = 1
      ) AS coagulation_dependent_sepsis_n,

      COUNT(*) FILTER (
        WHERE sepsis_coagulation_dependent_flag = 1
          AND (
            sepsis3_primary_flag != 1
            OR sofa_total_structured_0_24h <
              ",
    SEPSIS3_SOFA_THRESHOLD,
    "
            OR sofa_no_coag_structured_0_24h >=
              ",
    SEPSIS3_SOFA_THRESHOLD,
    "
          )
      ) AS invalid_coagulation_dependent_flag_n,

      COUNT(*) FILTER (
        WHERE sepsis3_primary_flag = 1
          AND sofa_no_coag_structured_0_24h <
            ",
    SEPSIS3_SOFA_THRESHOLD,
    "
          AND sepsis_coagulation_dependent_flag != 1
      ) AS missing_coagulation_dependent_flag_n,

      COUNT(*) FILTER (
        WHERE sepsis3_primary_flag = 1
          AND sofa_no_coag_structured_0_24h >=
            ",
    SEPSIS3_SOFA_THRESHOLD,
    "
          AND sepsis_coagulation_dependent_flag != 0
      ) AS invalid_nondependent_sepsis_flag_n,

      COUNT(*) FILTER (
        WHERE sepsis3_primary_flag = 1
          AND sofa_missing_components_assigned_zero_flag
      ) AS sepsis_with_zero_filled_component_n,

      COUNT(*) FILTER (
        WHERE sepsis3_primary_flag = 1
          AND sofa_all_components_observed_flag
      ) AS sepsis_with_complete_sofa_n

    FROM sepsis3_episode;
    "
  )
) %>%
  tibble::as_tibble()


if (
  sepsis3_check$episode_n !=
  sepsis3_check$distinct_episode_n ||
  sepsis3_check$episode_n !=
  infection_integration_check$output_episode_n ||
  sepsis3_check$invalid_sepsis_flag_n != 0L ||
  sepsis3_check$sepsis_without_suspected_infection_n != 0L ||
  sepsis3_check$sepsis_below_sofa_threshold_n != 0L ||
  sepsis3_check$nonsepsis_despite_meeting_definition_n != 0L ||
  sepsis3_check$invalid_coagulation_dependent_flag_n != 0L ||
  sepsis3_check$missing_coagulation_dependent_flag_n != 0L ||
  sepsis3_check$invalid_nondependent_sepsis_flag_n != 0L
) {
  stop(
    "Primary Sepsis-3 phenotype failed validation.",
    call. = FALSE
  )
}


print(
  sepsis3_check,
  width = Inf
)

# ------------------------------------------------------------------------------
# 41.4 Integrate Sepsis-3 fields into the working cohort
# ------------------------------------------------------------------------------

sepsis3_output_columns <- c(
  "sepsis3_primary_flag",
  "sepsis_coagulation_dependent_flag"
)


candidate_columns_before_sepsis3 <- DBI::dbListFields(
  con,
  "candidate_cohort_working_table"
)


candidate_base_columns_before_sepsis3 <- setdiff(
  candidate_columns_before_sepsis3,
  sepsis3_output_columns
)


if (length(candidate_base_columns_before_sepsis3) == 0L) {
  stop(
    "No candidate-cohort base columns remain for Sepsis-3 integration.",
    call. = FALSE
  )
}


candidate_base_columns_sepsis3_sql <- paste(
  vapply(
    candidate_base_columns_before_sepsis3,
    function(column_name) {
      paste0(
        "cohort.",
        as.character(
          DBI::dbQuoteIdentifier(
            con,
            column_name
          )
        )
      )
    },
    character(1L)
  ),
  collapse = ",\n    "
)


sepsis3_addition_columns_sql <- paste(
  vapply(
    sepsis3_output_columns,
    function(column_name) {
      paste0(
        "sepsis.",
        as.character(
          DBI::dbQuoteIdentifier(
            con,
            column_name
          )
        )
      )
    },
    character(1L)
  ),
  collapse = ",\n    "
)


DBI::dbExecute(
  con,
  paste0(
    "
    CREATE OR REPLACE TEMP TABLE
      candidate_cohort_before_sepsis3_table AS

    SELECT
      ",
    candidate_base_columns_sepsis3_sql,
    "

    FROM candidate_cohort_working_table AS cohort;
    "
  )
)


DBI::dbExecute(
  con,
  paste0(
    "
    CREATE OR REPLACE TEMP TABLE
      candidate_cohort_with_sepsis3_table AS

    SELECT
      ",
    candidate_base_columns_sepsis3_sql,
    ",
      ",
    sepsis3_addition_columns_sql,
    "

    FROM candidate_cohort_before_sepsis3_table
      AS cohort

    INNER JOIN sepsis3_episode AS sepsis
      USING (episode_id);
    "
  )
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    candidate_cohort_working_table AS

  SELECT *
  FROM candidate_cohort_with_sepsis3_table;
  "
)

# ------------------------------------------------------------------------------
# 41.5 Validate Sepsis-3 integration
# ------------------------------------------------------------------------------

sepsis3_integration_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    (
      SELECT COUNT(*)
      FROM candidate_cohort_before_sepsis3_table
    ) AS source_episode_n,

    (
      SELECT COUNT(*)
      FROM sepsis3_episode
    ) AS phenotype_episode_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
    ) AS output_episode_n,

    (
      SELECT
        COUNT(*) - COUNT(DISTINCT episode_id)
      FROM candidate_cohort_working_table
    ) AS duplicated_output_episode_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
      WHERE sepsis3_primary_flag = 1
    ) AS sepsis_episode_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
      WHERE sepsis_coagulation_dependent_flag = 1
    ) AS coagulation_dependent_sepsis_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
      WHERE sepsis3_primary_flag IS NULL
    ) AS missing_sepsis3_flag_n;
  "
) %>%
  tibble::as_tibble()


if (
  sepsis3_integration_check$source_episode_n !=
  sepsis3_integration_check$phenotype_episode_n ||
  sepsis3_integration_check$source_episode_n !=
  sepsis3_integration_check$output_episode_n ||
  sepsis3_integration_check$duplicated_output_episode_n != 0L ||
  sepsis3_integration_check$missing_sepsis3_flag_n != 0L
) {
  stop(
    "Sepsis-3 cohort integration failed.",
    call. = FALSE
  )
}


candidate_cohort_working <- DBI::dbGetQuery(
  con,
  "
  SELECT *
  FROM candidate_cohort_working_table
  ORDER BY episode_id;
  "
) %>%
  tibble::as_tibble()


assert_unique_key(
  candidate_cohort_working,
  "episode_id",
  "candidate_cohort_working"
)


print(
  sepsis3_integration_check,
  width = Inf
)

# ==============================================================================
# 42. MUTUALLY EXCLUSIVE PRIMARY PHENOTYPE GROUPS
# ==============================================================================


# ------------------------------------------------------------------------------
# 42.1 Verify required phenotype columns
# ------------------------------------------------------------------------------

required_group_columns <- c(
  "episode_id",
  "suspected_infection_primary_flag",
  "sepsis3_primary_flag",
  "sepsis_coagulation_dependent_flag",
  "sofa_total_structured_0_24h",
  "sofa_no_coag_structured_0_24h",
  "sofa_all_components_observed_flag",
  "sofa_missing_components_assigned_zero_flag"
)


missing_group_columns <- setdiff(
  required_group_columns,
  DBI::dbListFields(
    con,
    "candidate_cohort_working_table"
  )
)


if (length(missing_group_columns) > 0L) {
  stop(
    paste0(
      "The candidate cohort is missing required phenotype column(s): ",
      paste(
        missing_group_columns,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 42.2 Assign one mutually exclusive phenotype group per episode
#
# sepsis:
#   suspected infection = 1 and Sepsis-3 = 1
#
# infection_without_sepsis:
#   suspected infection = 1 and Sepsis-3 = 0
#
# control:
#   suspected infection = 0 and Sepsis-3 = 0
#
# unclassified:
#   insufficient or internally inconsistent phenotype information
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    primary_phenotype_group_episode AS

  WITH classified AS (
    SELECT
      episode_id,

      CASE
        WHEN sepsis3_primary_flag = 1
        THEN 'sepsis'

        WHEN suspected_infection_primary_flag = 1
          AND sepsis3_primary_flag = 0
        THEN 'infection_without_sepsis'

        WHEN suspected_infection_primary_flag = 0
          AND sepsis3_primary_flag = 0
        THEN 'control'

        ELSE 'unclassified'
      END AS primary_phenotype_group

    FROM candidate_cohort_working_table
  )

  SELECT
    episode_id,
    primary_phenotype_group,

    CASE primary_phenotype_group
      WHEN 'control' THEN 1
      WHEN 'infection_without_sepsis' THEN 2
      WHEN 'sepsis' THEN 3
      WHEN 'unclassified' THEN 4
    END::INTEGER AS primary_phenotype_group_order,

    CAST(
      primary_phenotype_group = 'sepsis'
      AS INTEGER
    ) AS primary_sepsis_group_flag,

    CAST(
      primary_phenotype_group =
        'infection_without_sepsis'
      AS INTEGER
    ) AS primary_infection_without_sepsis_group_flag,

    CAST(
      primary_phenotype_group = 'control'
      AS INTEGER
    ) AS primary_control_group_flag,

    CAST(
      primary_phenotype_group = 'unclassified'
      AS INTEGER
    ) AS primary_unclassified_group_flag,

    CAST(
      primary_phenotype_group != 'unclassified'
      AS INTEGER
    ) AS primary_phenotype_classifiable_flag

  FROM classified;
  "
)


# ------------------------------------------------------------------------------
# 42.3 Validate mutually exclusive group assignment
# ------------------------------------------------------------------------------

primary_phenotype_group_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS episode_n,

    COUNT(DISTINCT group_table.episode_id)
      AS distinct_episode_n,

    COUNT(*) FILTER (
      WHERE primary_phenotype_group = 'sepsis'
    ) AS sepsis_episode_n,

    COUNT(*) FILTER (
      WHERE primary_phenotype_group =
        'infection_without_sepsis'
    ) AS infection_without_sepsis_episode_n,

    COUNT(*) FILTER (
      WHERE primary_phenotype_group = 'control'
    ) AS control_episode_n,

    COUNT(*) FILTER (
      WHERE primary_phenotype_group = 'unclassified'
    ) AS unclassified_episode_n,

    COUNT(*) FILTER (
      WHERE primary_phenotype_group NOT IN (
        'sepsis',
        'infection_without_sepsis',
        'control',
        'unclassified'
      )
      OR primary_phenotype_group IS NULL
    ) AS invalid_group_n,

    COUNT(*) FILTER (
      WHERE
        primary_sepsis_group_flag +
        primary_infection_without_sepsis_group_flag +
        primary_control_group_flag +
        primary_unclassified_group_flag != 1
    ) AS nonexclusive_group_flag_n,

    COUNT(*) FILTER (
      WHERE primary_phenotype_group = 'sepsis'
        AND (
          cohort.sepsis3_primary_flag != 1
          OR cohort.suspected_infection_primary_flag != 1
        )
    ) AS invalid_sepsis_group_n,

    COUNT(*) FILTER (
      WHERE primary_phenotype_group =
        'infection_without_sepsis'
        AND (
          cohort.suspected_infection_primary_flag != 1
          OR cohort.sepsis3_primary_flag != 0
        )
    ) AS invalid_infection_without_sepsis_group_n,

    COUNT(*) FILTER (
      WHERE primary_phenotype_group = 'control'
        AND (
          cohort.suspected_infection_primary_flag != 0
          OR cohort.sepsis3_primary_flag != 0
        )
    ) AS invalid_control_group_n,

    COUNT(*) FILTER (
      WHERE primary_phenotype_group = 'unclassified'
        AND cohort.suspected_infection_primary_flag
          IN (0, 1)
        AND cohort.sepsis3_primary_flag IN (0, 1)
    ) AS unnecessary_unclassified_group_n,

    COUNT(*) FILTER (
      WHERE primary_phenotype_group = 'sepsis'
        AND cohort.sepsis_coagulation_dependent_flag = 1
    ) AS coagulation_dependent_sepsis_n,

    COUNT(*) FILTER (
      WHERE primary_phenotype_group = 'sepsis'
        AND cohort.sofa_missing_components_assigned_zero_flag
    ) AS sepsis_with_zero_filled_component_n,

    COUNT(*) FILTER (
      WHERE primary_phenotype_group = 'sepsis'
        AND cohort.sofa_all_components_observed_flag
    ) AS sepsis_with_complete_sofa_n

  FROM primary_phenotype_group_episode AS group_table

  INNER JOIN candidate_cohort_working_table AS cohort
    USING (episode_id);
  "
) %>%
  tibble::as_tibble()


if (
  primary_phenotype_group_check$episode_n !=
  primary_phenotype_group_check$distinct_episode_n ||
  primary_phenotype_group_check$episode_n !=
  sepsis3_integration_check$output_episode_n ||
  primary_phenotype_group_check$invalid_group_n != 0L ||
  primary_phenotype_group_check$nonexclusive_group_flag_n != 0L ||
  primary_phenotype_group_check$invalid_sepsis_group_n != 0L ||
  primary_phenotype_group_check$
  invalid_infection_without_sepsis_group_n != 0L ||
  primary_phenotype_group_check$invalid_control_group_n != 0L ||
  primary_phenotype_group_check$
  unnecessary_unclassified_group_n != 0L
) {
  stop(
    "Primary phenotype group assignment failed validation.",
    call. = FALSE
  )
}


print(
  primary_phenotype_group_check,
  width = Inf
)


# ------------------------------------------------------------------------------
# 42.4 Integrate phenotype-group fields into the working cohort
# ------------------------------------------------------------------------------

primary_group_output_columns <- setdiff(
  DBI::dbListFields(
    con,
    "primary_phenotype_group_episode"
  ),
  "episode_id"
)


candidate_columns_before_primary_group <- DBI::dbListFields(
  con,
  "candidate_cohort_working_table"
)


candidate_base_columns_before_primary_group <- setdiff(
  candidate_columns_before_primary_group,
  primary_group_output_columns
)


quote_qualified_columns <- function(
    table_alias,
    columns
) {
  paste(
    vapply(
      columns,
      function(column_name) {
        paste0(
          table_alias,
          ".",
          as.character(
            DBI::dbQuoteIdentifier(
              con,
              column_name
            )
          )
        )
      },
      character(1L)
    ),
    collapse = ",\n    "
  )
}


candidate_base_columns_primary_group_sql <-
  quote_qualified_columns(
    table_alias = "cohort",
    columns = candidate_base_columns_before_primary_group
  )


primary_group_addition_columns_sql <-
  quote_qualified_columns(
    table_alias = "phenotype",
    columns = primary_group_output_columns
  )


DBI::dbExecute(
  con,
  paste0(
    "
    CREATE OR REPLACE TEMP TABLE
      candidate_cohort_before_primary_group_table AS

    SELECT
      ",
    candidate_base_columns_primary_group_sql,
    "

    FROM candidate_cohort_working_table AS cohort;
    "
  )
)


DBI::dbExecute(
  con,
  paste0(
    "
    CREATE OR REPLACE TEMP TABLE
      candidate_cohort_with_primary_group_table AS

    SELECT
      ",
    candidate_base_columns_primary_group_sql,
    ",
      ",
    primary_group_addition_columns_sql,
    "

    FROM candidate_cohort_before_primary_group_table
      AS cohort

    INNER JOIN primary_phenotype_group_episode
      AS phenotype
      USING (episode_id);
    "
  )
)


DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    candidate_cohort_working_table AS

  SELECT *
  FROM candidate_cohort_with_primary_group_table;
  "
)


# ------------------------------------------------------------------------------
# 42.5 Validate phenotype-group integration
# ------------------------------------------------------------------------------

primary_group_integration_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    (
      SELECT COUNT(*)
      FROM candidate_cohort_before_primary_group_table
    ) AS source_episode_n,

    (
      SELECT COUNT(*)
      FROM primary_phenotype_group_episode
    ) AS phenotype_episode_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
    ) AS output_episode_n,

    (
      SELECT
        COUNT(*) - COUNT(DISTINCT episode_id)
      FROM candidate_cohort_working_table
    ) AS duplicated_output_episode_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
      WHERE primary_phenotype_group = 'sepsis'
    ) AS sepsis_episode_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
      WHERE primary_phenotype_group =
        'infection_without_sepsis'
    ) AS infection_without_sepsis_episode_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
      WHERE primary_phenotype_group = 'control'
    ) AS control_episode_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
      WHERE primary_phenotype_group = 'unclassified'
    ) AS unclassified_episode_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
      WHERE primary_phenotype_group IS NULL
    ) AS missing_group_n;
  "
) %>%
  tibble::as_tibble()


if (
  primary_group_integration_check$source_episode_n !=
  primary_group_integration_check$phenotype_episode_n ||
  primary_group_integration_check$source_episode_n !=
  primary_group_integration_check$output_episode_n ||
  primary_group_integration_check$
  duplicated_output_episode_n != 0L ||
  primary_group_integration_check$missing_group_n != 0L
) {
  stop(
    "Primary phenotype group integration failed.",
    call. = FALSE
  )
}


candidate_cohort_working <- DBI::dbGetQuery(
  con,
  "
  SELECT *
  FROM candidate_cohort_working_table
  ORDER BY episode_id;
  "
) %>%
  tibble::as_tibble()


assert_unique_key(
  candidate_cohort_working,
  "episode_id",
  "candidate_cohort_working"
)


print(
  primary_group_integration_check,
  width = Inf
)

# ==============================================================================
# 43. PRIMARY ANALYTICAL COHORT
#
# Eligibility:
#   1. Valid CRP in the primary window.
#   2. Complete NLR, PLR, MLR and NMR.
#   3. First eligible episode per patient.
#
# The complete phenotype cohort remains unchanged in:
#   candidate_cohort_working_table
# ==============================================================================


# ------------------------------------------------------------------------------
# 43.1 Verify prerequisites
# ------------------------------------------------------------------------------

if (
  !DBI::dbExistsTable(
    con,
    "candidate_cohort_working_table"
  )
) {
  stop(
    "The complete phenotype cohort was not found.",
    call. = FALSE
  )
}


required_primary_analytic_columns <- c(
  "episode_id",
  "subject_id",
  "hadm_id",
  "ed_intime",
  "crp_available_primary_window_flag",
  "crp_index_value_mg_l",
  "nlr",
  "plr",
  "mlr",
  "nmr",
  "nlr_available_flag",
  "plr_available_flag",
  "mlr_available_flag",
  "nmr_available_flag",
  "all_four_ratios_available_flag",
  "primary_phenotype_group",
  "primary_phenotype_group_order",
  "suspected_infection_primary_flag",
  "sepsis3_primary_flag",
  "sepsis_coagulation_dependent_flag"
)


available_primary_analytic_columns <- DBI::dbListFields(
  con,
  "candidate_cohort_working_table"
)


missing_primary_analytic_columns <- setdiff(
  required_primary_analytic_columns,
  available_primary_analytic_columns
)


if (length(missing_primary_analytic_columns) > 0L) {
  stop(
    paste0(
      "The phenotype cohort is missing required analytical column(s): ",
      paste(
        missing_primary_analytic_columns,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 43.2 Identify episodes with valid CRP
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    analytic_valid_crp_episodes AS

  SELECT *

  FROM candidate_cohort_working_table

  WHERE coalesce(
          TRY_CAST(
            crp_available_primary_window_flag
            AS BOOLEAN
          ),
          FALSE
        )

    AND TRY_CAST(
          crp_index_value_mg_l
          AS DOUBLE
        ) IS NOT NULL

    AND isfinite(
          TRY_CAST(
            crp_index_value_mg_l
            AS DOUBLE
          )
        );
  "
)


# ------------------------------------------------------------------------------
# 43.3 Require all four complete and finite indices
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    analytic_complete_indices_episodes AS

  SELECT *

  FROM analytic_valid_crp_episodes

  WHERE coalesce(
          TRY_CAST(
            all_four_ratios_available_flag
            AS BOOLEAN
          ),
          FALSE
        )

    AND coalesce(
          TRY_CAST(
            nlr_available_flag
            AS BOOLEAN
          ),
          FALSE
        )

    AND coalesce(
          TRY_CAST(
            plr_available_flag
            AS BOOLEAN
          ),
          FALSE
        )

    AND coalesce(
          TRY_CAST(
            mlr_available_flag
            AS BOOLEAN
          ),
          FALSE
        )

    AND coalesce(
          TRY_CAST(
            nmr_available_flag
            AS BOOLEAN
          ),
          FALSE
        )

    AND TRY_CAST(nlr AS DOUBLE) IS NOT NULL
    AND TRY_CAST(plr AS DOUBLE) IS NOT NULL
    AND TRY_CAST(mlr AS DOUBLE) IS NOT NULL
    AND TRY_CAST(nmr AS DOUBLE) IS NOT NULL

    AND isfinite(TRY_CAST(nlr AS DOUBLE))
    AND isfinite(TRY_CAST(plr AS DOUBLE))
    AND isfinite(TRY_CAST(mlr AS DOUBLE))
    AND isfinite(TRY_CAST(nmr AS DOUBLE));
  "
)


# ------------------------------------------------------------------------------
# 43.4 Rank eligible episodes within each patient
#
# Ranking:
#   1. Earliest ED arrival.
#   2. Lowest episode_id as deterministic tie-break.
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    analytic_eligible_ranked AS

  SELECT
    cohort.*,

    COUNT(*) OVER (
      PARTITION BY subject_id
    )::BIGINT
      AS analytic_eligible_episode_count_subject,

    ROW_NUMBER() OVER (
      PARTITION BY subject_id

      ORDER BY
        ed_intime ASC NULLS LAST,
        episode_id ASC
    )::BIGINT
      AS analytic_eligible_episode_rank,

    COUNT(*) OVER (
      PARTITION BY subject_id
    )::BIGINT - 1
      AS later_analytic_eligible_episode_n_subject

  FROM analytic_complete_indices_episodes AS cohort;
  "
)


# ------------------------------------------------------------------------------
# 43.5 Select the first eligible episode per patient
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    analytic_cohort_primary_table AS

  SELECT
    *,

    TRUE
      AS analytic_primary_inclusion_flag,

    'valid_crp_complete_indices_first_eligible_episode'
      AS analytic_primary_selection_rule

  FROM analytic_eligible_ranked

  WHERE analytic_eligible_episode_rank = 1;
  "
)


# ------------------------------------------------------------------------------
# 43.6 Validate the primary analytical cohort
# ------------------------------------------------------------------------------

analytic_primary_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
    ) AS phenotype_episode_n,

    (
      SELECT COUNT(*)
      FROM analytic_valid_crp_episodes
    ) AS valid_crp_episode_n,

    (
      SELECT COUNT(*)
      FROM analytic_complete_indices_episodes
    ) AS valid_crp_complete_indices_episode_n,

    (
      SELECT COUNT(DISTINCT subject_id)
      FROM analytic_complete_indices_episodes
    ) AS eligible_patient_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_table
    ) AS analytic_episode_n,

    (
      SELECT COUNT(DISTINCT episode_id)
      FROM analytic_cohort_primary_table
    ) AS distinct_analytic_episode_n,

    (
      SELECT COUNT(DISTINCT subject_id)
      FROM analytic_cohort_primary_table
    ) AS distinct_analytic_patient_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
    ) -
    (
      SELECT COUNT(*)
      FROM analytic_valid_crp_episodes
    ) AS excluded_without_valid_crp_n,

    (
      SELECT COUNT(*)
      FROM analytic_valid_crp_episodes
    ) -
    (
      SELECT COUNT(*)
      FROM analytic_complete_indices_episodes
    ) AS excluded_incomplete_indices_n,

    (
      SELECT COUNT(*)
      FROM analytic_complete_indices_episodes
    ) -
    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_table
    ) AS excluded_later_eligible_episode_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_table
      WHERE subject_id IS NULL
         OR trim(CAST(subject_id AS VARCHAR)) = ''
    ) AS missing_subject_id_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_table
      WHERE episode_id IS NULL
         OR trim(CAST(episode_id AS VARCHAR)) = ''
    ) AS missing_episode_id_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_table
      WHERE ed_intime IS NULL
    ) AS missing_ed_intime_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_table
      WHERE analytic_eligible_episode_rank != 1
    ) AS selected_nonfirst_episode_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_table
      WHERE NOT coalesce(
        TRY_CAST(
          crp_available_primary_window_flag
          AS BOOLEAN
        ),
        FALSE
      )
    ) AS invalid_crp_availability_flag_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_table
      WHERE TRY_CAST(
              crp_index_value_mg_l
              AS DOUBLE
            ) IS NULL

         OR NOT isfinite(
           TRY_CAST(
             crp_index_value_mg_l
             AS DOUBLE
           )
         )
    ) AS invalid_crp_value_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_table
      WHERE NOT coalesce(
        TRY_CAST(
          all_four_ratios_available_flag
          AS BOOLEAN
        ),
        FALSE
      )
    ) AS incomplete_ratio_flag_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_table
      WHERE TRY_CAST(nlr AS DOUBLE) IS NULL
         OR TRY_CAST(plr AS DOUBLE) IS NULL
         OR TRY_CAST(mlr AS DOUBLE) IS NULL
         OR TRY_CAST(nmr AS DOUBLE) IS NULL
         OR NOT isfinite(TRY_CAST(nlr AS DOUBLE))
         OR NOT isfinite(TRY_CAST(plr AS DOUBLE))
         OR NOT isfinite(TRY_CAST(mlr AS DOUBLE))
         OR NOT isfinite(TRY_CAST(nmr AS DOUBLE))
    ) AS invalid_ratio_value_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_table
      WHERE primary_phenotype_group IS NULL

         OR primary_phenotype_group NOT IN (
           'control',
           'infection_without_sepsis',
           'sepsis'
         )
    ) AS invalid_phenotype_group_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_table
      WHERE analytic_primary_inclusion_flag
        IS DISTINCT FROM TRUE
    ) AS invalid_inclusion_flag_n;
  "
) %>%
  tibble::as_tibble()


if (
  analytic_primary_check$analytic_episode_n !=
  analytic_primary_check$distinct_analytic_episode_n ||
  analytic_primary_check$analytic_episode_n !=
  analytic_primary_check$distinct_analytic_patient_n ||
  analytic_primary_check$analytic_episode_n !=
  analytic_primary_check$eligible_patient_n ||
  analytic_primary_check$missing_subject_id_n != 0L ||
  analytic_primary_check$missing_episode_id_n != 0L ||
  analytic_primary_check$missing_ed_intime_n != 0L ||
  analytic_primary_check$selected_nonfirst_episode_n != 0L ||
  analytic_primary_check$invalid_crp_availability_flag_n != 0L ||
  analytic_primary_check$invalid_crp_value_n != 0L ||
  analytic_primary_check$incomplete_ratio_flag_n != 0L ||
  analytic_primary_check$invalid_ratio_value_n != 0L ||
  analytic_primary_check$invalid_phenotype_group_n != 0L ||
  analytic_primary_check$invalid_inclusion_flag_n != 0L
) {
  stop(
    "The primary analytical cohort failed validation.",
    call. = FALSE
  )
}


print(
  analytic_primary_check,
  width = Inf
)

# ------------------------------------------------------------------------------
# 43.7 Cohort-flow summary
# ------------------------------------------------------------------------------

cohort_flow_counts <- DBI::dbGetQuery(
  con,
  "
  WITH source_with_cbc AS (
    SELECT
      episode.episode_id,
      episode.subject_id,

      coalesce(
        availability.cbc_n_specimens_0_24h,
        0
      )::BIGINT AS cbc_n_specimens_0_24h,

      coalesce(
        availability.cbc_n_complete_specimens_0_24h,
        0
      )::BIGINT AS cbc_n_complete_specimens_0_24h

    FROM episode_spine_table AS episode

    LEFT JOIN cbc_episode_availability AS availability
      USING (episode_id)
  )

  SELECT
    COUNT(*)::BIGINT
      AS source_episode_n,

    COUNT(DISTINCT subject_id)::BIGINT
      AS source_patient_n,

    COUNT(*) FILTER (
      WHERE cbc_n_specimens_0_24h > 0
    )::BIGINT AS any_cbc_episode_n,

    COUNT(DISTINCT subject_id) FILTER (
      WHERE cbc_n_specimens_0_24h > 0
    )::BIGINT AS any_cbc_patient_n,

    COUNT(*) FILTER (
      WHERE cbc_n_specimens_0_24h = 0
    )::BIGINT AS no_cbc_episode_n,

    COUNT(*) FILTER (
      WHERE cbc_n_specimens_0_24h > 0
        AND cbc_n_complete_specimens_0_24h = 0
    )::BIGINT AS incomplete_cbc_episode_n,

    COUNT(*) FILTER (
      WHERE cbc_n_complete_specimens_0_24h > 0
    )::BIGINT AS complete_cbc_availability_episode_n,

    COUNT(DISTINCT subject_id) FILTER (
      WHERE cbc_n_complete_specimens_0_24h > 0
    )::BIGINT AS complete_cbc_availability_patient_n,

    (
      SELECT COUNT(*)
      FROM candidate_cohort_working_table
    )::BIGINT AS candidate_episode_n,

    (
      SELECT COUNT(DISTINCT subject_id)
      FROM candidate_cohort_working_table
    )::BIGINT AS candidate_patient_n,

    (
      SELECT COUNT(*)
      FROM analytic_valid_crp_episodes
    )::BIGINT AS valid_crp_episode_n,

    (
      SELECT COUNT(DISTINCT subject_id)
      FROM analytic_valid_crp_episodes
    )::BIGINT AS valid_crp_patient_n,

    (
      SELECT COUNT(*)
      FROM analytic_complete_indices_episodes
    )::BIGINT AS complete_indices_episode_n,

    (
      SELECT COUNT(DISTINCT subject_id)
      FROM analytic_complete_indices_episodes
    )::BIGINT AS complete_indices_patient_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_table
    )::BIGINT AS final_episode_n,

    (
      SELECT COUNT(DISTINCT subject_id)
      FROM analytic_cohort_primary_table
    )::BIGINT AS final_patient_n

  FROM source_with_cbc;
  "
) |>
  tibble::as_tibble()


if (nrow(cohort_flow_counts) != 1L) {
  stop(
    "The cohort-flow count query did not return exactly one row.",
    call. = FALSE
  )
}


cohort_flow_count_names <- c(
  "source_episode_n",
  "source_patient_n",
  "any_cbc_episode_n",
  "any_cbc_patient_n",
  "no_cbc_episode_n",
  "incomplete_cbc_episode_n",
  "complete_cbc_availability_episode_n",
  "complete_cbc_availability_patient_n",
  "candidate_episode_n",
  "candidate_patient_n",
  "valid_crp_episode_n",
  "valid_crp_patient_n",
  "complete_indices_episode_n",
  "complete_indices_patient_n",
  "final_episode_n",
  "final_patient_n"
)


missing_cohort_flow_counts <- setdiff(
  cohort_flow_count_names,
  names(cohort_flow_counts)
)


if (length(missing_cohort_flow_counts) > 0L) {
  stop(
    paste0(
      "The cohort-flow query is missing count(s): ",
      paste(
        missing_cohort_flow_counts,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


cohort_flow_count_values <- vapply(
  cohort_flow_count_names,
  function(column_name) {
    as.numeric(
      cohort_flow_counts[[column_name]][[1L]]
    )
  },
  numeric(1L)
)


if (
  anyNA(cohort_flow_count_values) ||
  any(!is.finite(cohort_flow_count_values)) ||
  any(cohort_flow_count_values < 0)
) {
  stop(
    "The cohort-flow counts contain missing, non-finite or negative values.",
    call. = FALSE
  )
}


source_episode_n <-
  cohort_flow_count_values[["source_episode_n"]]

source_patient_n <-
  cohort_flow_count_values[["source_patient_n"]]

any_cbc_episode_n <-
  cohort_flow_count_values[["any_cbc_episode_n"]]

any_cbc_patient_n <-
  cohort_flow_count_values[["any_cbc_patient_n"]]

no_cbc_episode_n <-
  cohort_flow_count_values[["no_cbc_episode_n"]]

incomplete_cbc_episode_n <-
  cohort_flow_count_values[["incomplete_cbc_episode_n"]]

complete_cbc_availability_episode_n <-
  cohort_flow_count_values[["complete_cbc_availability_episode_n"]]

complete_cbc_availability_patient_n <-
  cohort_flow_count_values[["complete_cbc_availability_patient_n"]]

candidate_episode_n <-
  cohort_flow_count_values[["candidate_episode_n"]]

candidate_patient_n <-
  cohort_flow_count_values[["candidate_patient_n"]]

valid_crp_episode_n <-
  cohort_flow_count_values[["valid_crp_episode_n"]]

valid_crp_patient_n <-
  cohort_flow_count_values[["valid_crp_patient_n"]]

complete_indices_episode_n <-
  cohort_flow_count_values[["complete_indices_episode_n"]]

complete_indices_patient_n <-
  cohort_flow_count_values[["complete_indices_patient_n"]]

final_episode_n <-
  cohort_flow_count_values[["final_episode_n"]]

final_patient_n <-
  cohort_flow_count_values[["final_patient_n"]]


if (
  source_episode_n !=
  no_cbc_episode_n + any_cbc_episode_n
) {
  stop(
    "The source cohort does not reconcile with CBC availability.",
    call. = FALSE
  )
}


if (
  any_cbc_episode_n !=
  incomplete_cbc_episode_n +
  complete_cbc_availability_episode_n
) {
  stop(
    "CBC availability does not reconcile with complete-CBC availability.",
    call. = FALSE
  )
}


if (
  complete_cbc_availability_episode_n !=
  candidate_episode_n ||
  complete_cbc_availability_patient_n !=
  candidate_patient_n
) {
  stop(
    paste(
      "The complete-CBC availability cohort does not match",
      "candidate_cohort_working_table."
    ),
    call. = FALSE
  )
}


if (
  candidate_episode_n < valid_crp_episode_n ||
  valid_crp_episode_n < complete_indices_episode_n ||
  complete_indices_episode_n < final_episode_n
) {
  stop(
    "The cohort-flow stages are not monotonically decreasing.",
    call. = FALSE
  )
}


if (final_episode_n != final_patient_n) {
  stop(
    paste(
      "The final cohort does not contain exactly one episode",
      "per patient."
    ),
    call. = FALSE
  )
}


analytic_cohort_flow <- tibble::tibble(
  stage_order = 1:6,
  
  cohort_stage = c(
    "Source adult ED episode cohort",
    "At least one CBC specimen within 0–24 hours",
    paste(
      "Complete platelet, neutrophil, lymphocyte and",
      "monocyte counts in one specimen"
    ),
    "Valid CRP within 0–24 hours",
    "Complete and finite NLR, PLR, MLR and NMR",
    "First eligible episode per patient"
  ),
  
  counting_unit = rep(
    "episodes",
    6L
  ),
  
  input_n = c(
    NA_real_,
    source_episode_n,
    any_cbc_episode_n,
    candidate_episode_n,
    valid_crp_episode_n,
    complete_indices_episode_n
  ),
  
  excluded_n = c(
    NA_real_,
    no_cbc_episode_n,
    incomplete_cbc_episode_n,
    candidate_episode_n - valid_crp_episode_n,
    valid_crp_episode_n - complete_indices_episode_n,
    complete_indices_episode_n - final_episode_n
  ),
  
  retained_n = c(
    source_episode_n,
    any_cbc_episode_n,
    candidate_episode_n,
    valid_crp_episode_n,
    complete_indices_episode_n,
    final_episode_n
  ),
  
  retained_unique_patient_n = c(
    source_patient_n,
    any_cbc_patient_n,
    candidate_patient_n,
    valid_crp_patient_n,
    complete_indices_patient_n,
    final_patient_n
  ),
  
  exclusion_reason = c(
    NA_character_,
    "No CBC specimen within 0–24 hours",
    paste(
      "CBC present but no specimen contained complete valid",
      "platelet, neutrophil, lymphocyte and monocyte counts"
    ),
    "No valid CRP value within 0–24 hours",
    "One or more required derived ratios were missing or non-finite",
    "Later eligible episode from a patient already represented"
  )
)


flow_input_values <- analytic_cohort_flow$input_n[-1L]

flow_excluded_values <- analytic_cohort_flow$excluded_n[-1L]

flow_retained_values <- analytic_cohort_flow$retained_n[-1L]


if (
  any(
    flow_input_values !=
    flow_excluded_values + flow_retained_values
  )
) {
  stop(
    "At least one cohort-flow stage does not reconcile.",
    call. = FALSE
  )
}


if (
  any(
    flow_input_values !=
    head(
      analytic_cohort_flow$retained_n,
      -1L
    )
  )
) {
  stop(
    "The input count of a cohort-flow stage does not match the prior stage.",
    call. = FALSE
  )
}


print(
  analytic_cohort_flow,
  width = Inf
)


# Optional episode-level trace. The summary above is always printed; the full
# identifier list is printed only when explicitly requested because the first
# exclusions contain tens of thousands of rows.
cohort_exclusion_cases <- DBI::dbGetQuery(
  con,
  "
  WITH source_with_cbc AS (
    SELECT
      episode.episode_id,
      episode.subject_id,
      coalesce(availability.cbc_n_specimens_0_24h, 0)::BIGINT
        AS cbc_n_specimens_0_24h,
      coalesce(availability.cbc_n_complete_specimens_0_24h, 0)::BIGINT
        AS cbc_n_complete_specimens_0_24h
    FROM episode_spine_table AS episode
    LEFT JOIN cbc_episode_availability AS availability
      USING (episode_id)
  )

  SELECT
    1 AS exclusion_stage_order,
    'no_cbc_0_24h' AS exclusion_stage,
    'No CBC specimen within 0-24 hours' AS exclusion_reason,
    episode_id,
    subject_id
  FROM source_with_cbc
  WHERE cbc_n_specimens_0_24h = 0

  UNION ALL

  SELECT
    2,
    'incomplete_cbc_0_24h',
    'CBC present but no complete four-cell specimen',
    episode_id,
    subject_id
  FROM source_with_cbc
  WHERE cbc_n_specimens_0_24h > 0
    AND cbc_n_complete_specimens_0_24h = 0

  UNION ALL

  SELECT
    3,
    'no_valid_crp_0_24h',
    'No valid CRP within 0-24 hours',
    candidate.episode_id,
    candidate.subject_id
  FROM candidate_cohort_working_table AS candidate
  LEFT JOIN analytic_valid_crp_episodes AS retained
    USING (episode_id)
  WHERE retained.episode_id IS NULL

  UNION ALL

  SELECT
    4,
    'incomplete_or_nonfinite_ratios',
    'Incomplete or non-finite NLR, PLR, MLR, or NMR',
    candidate.episode_id,
    candidate.subject_id
  FROM analytic_valid_crp_episodes AS candidate
  LEFT JOIN analytic_complete_indices_episodes AS retained
    USING (episode_id)
  WHERE retained.episode_id IS NULL

  UNION ALL

  SELECT
    5,
    'later_eligible_episode',
    'Later eligible episode from a previously represented patient',
    episode_id,
    subject_id
  FROM analytic_eligible_ranked
  WHERE analytic_eligible_episode_rank > 1

  ORDER BY exclusion_stage_order, subject_id, episode_id;
  "
) %>%
  tibble::as_tibble()


cohort_exclusion_case_summary <- cohort_exclusion_cases %>%
  dplyr::count(
    exclusion_stage_order,
    exclusion_stage,
    exclusion_reason,
    name = "excluded_episodes"
  ) %>%
  dplyr::arrange(exclusion_stage_order)


cat("\nEPISODE-LEVEL EXCLUSION TRACE: COUNTS\n")
print(cohort_exclusion_case_summary, n = Inf, width = Inf)


if (isTRUE(PRINT_EXCLUDED_EPISODE_IDS)) {
  cat("\nEPISODE-LEVEL EXCLUSION TRACE: IDENTIFIERS\n")
  print(cohort_exclusion_cases, n = Inf, width = Inf)
}

# ------------------------------------------------------------------------------
# 43.8 Final analytical-group summary
# ------------------------------------------------------------------------------

analytic_primary_group_summary <- DBI::dbGetQuery(
  con,
  "
  SELECT
    primary_phenotype_group,

    MIN(primary_phenotype_group_order)
      AS primary_phenotype_group_order,

    COUNT(*)::BIGINT
      AS patient_n,

    ROUND(
      100.0 * COUNT(*) /
      SUM(COUNT(*)) OVER (),
      1
    ) AS patient_percent,

    median(crp_index_value_mg_l)
      AS median_crp_mg_l,

    quantile_cont(
      crp_index_value_mg_l,
      0.25
    ) AS crp_q1_mg_l,

    quantile_cont(
      crp_index_value_mg_l,
      0.75
    ) AS crp_q3_mg_l,

    median(nlr)
      AS median_nlr,

    median(plr)
      AS median_plr,

    median(mlr)
      AS median_mlr,

    median(nmr)
      AS median_nmr,

    COUNT(*) FILTER (
      WHERE sepsis_coagulation_dependent_flag = 1
    )::BIGINT
      AS coagulation_dependent_sepsis_n

  FROM analytic_cohort_primary_table

  GROUP BY primary_phenotype_group

  ORDER BY primary_phenotype_group_order;
  "
) %>%
  tibble::as_tibble()


print(
  analytic_primary_group_summary,
  width = Inf
)


# ------------------------------------------------------------------------------
# 43.9 Load the final analytical cohort into R
# ------------------------------------------------------------------------------

analytic_cohort_primary <- DBI::dbGetQuery(
  con,
  "
  SELECT *

  FROM analytic_cohort_primary_table

  ORDER BY
    primary_phenotype_group_order,
    subject_id,
    ed_intime,
    episode_id;
  "
) %>%
  tibble::as_tibble()


assert_unique_key(
  analytic_cohort_primary,
  "episode_id",
  "analytic_cohort_primary"
)


assert_unique_key(
  analytic_cohort_primary,
  "subject_id",
  "analytic_cohort_primary"
)

# ==============================================================================
# 44. DISCHARGE NOTES — IMPORT, NORMALIZATION AND COHORT RESTRICTION
#
# Purpose:
#   1. Locate the MIMIC-IV discharge-note source file.
#   2. Restrict notes to the final analytical cohort.
#   3. Preserve every original note in a long-format DuckDB table.
#   4. Characterize note availability and multiplicity before note selection.
#
# Important:
#   - Original note text is never modified.
#   - No primary note is selected in this section.
#   - No biomarker redaction is performed in this section.
#   - No Parquet file is created.
# ==============================================================================


# ------------------------------------------------------------------------------
# 44.1 Verify prerequisites
# ------------------------------------------------------------------------------

if (
  !exists(
    "con",
    inherits = FALSE
  ) ||
  !DBI::dbIsValid(
    con
  )
) {
  stop(
    "A valid DuckDB connection named 'con' is required.",
    call. = FALSE
  )
}


if (
  !DBI::dbExistsTable(
    con,
    "analytic_cohort_primary_table"
  )
) {
  stop(
    "The table analytic_cohort_primary_table was not found.",
    call. = FALSE
  )
}


if (
  !exists(
    "INPUT_DIR",
    inherits = FALSE
  ) ||
  !dir.exists(
    INPUT_DIR
  )
) {
  stop(
    "A valid input directory named INPUT_DIR is required.",
    call. = FALSE
  )
}


if (
  !exists(
    "OUTPUT_DIR",
    inherits = FALSE
  ) ||
  !dir.exists(
    OUTPUT_DIR
  )
) {
  stop(
    "A valid output directory named OUTPUT_DIR is required.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 44.2 Helper for quoting file paths in DuckDB SQL
# ------------------------------------------------------------------------------

quote_note_sql_path <- function(
    con,
    path
) {
  
  normalized_path <- normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )
  
  
  as.character(
    DBI::dbQuoteString(
      con,
      normalized_path
    )
  )
}


# ------------------------------------------------------------------------------
# 44.3 Locate the discharge-note source file
#
# Accepted names:
#   discharge.csv
#   discharge.csv.gz
# ------------------------------------------------------------------------------

# BigQuery export query:
# MIMIC-IV-Note v2.2
# SELECT *
#   FROM `physionet-data.mimiciv_note.discharge`;


existing_discharge_note_file <- (
  exists(
    "DISCHARGE_NOTE_FILE",
    inherits = FALSE
  ) &&
    length(DISCHARGE_NOTE_FILE) == 1L &&
    !is.na(DISCHARGE_NOTE_FILE) &&
    file.exists(DISCHARGE_NOTE_FILE)
)


if (existing_discharge_note_file) {
  
  DISCHARGE_NOTE_FILE <- normalizePath(
    DISCHARGE_NOTE_FILE,
    winslash = "/",
    mustWork = TRUE
  )
  
} else {
  
  discharge_note_files <- list.files(
    path = INPUT_DIR,
    pattern = "^discharge\\.csv(\\.gz)?$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  
  if (length(discharge_note_files) == 0L) {
    stop(
      paste0(
        "No discharge.csv or discharge.csv.gz file was found under INPUT_DIR: ",
        INPUT_DIR,
        "."
      ),
      call. = FALSE
    )
  }
  
  
  discharge_note_files <- sort(
    unique(
      normalizePath(
        discharge_note_files,
        winslash = "/",
        mustWork = TRUE
      )
    )
  )
  
  
  if (length(discharge_note_files) > 1L) {
    
    print(
      tibble::tibble(
        candidate_discharge_note_file =
          discharge_note_files
      ),
      n = Inf,
      width = Inf
    )
    
    
    stop(
      paste0(
        "More than one discharge-note source file was found. ",
        "Assign the intended file manually to DISCHARGE_NOTE_FILE ",
        "and rerun Section 44."
      ),
      call. = FALSE
    )
  }
  
  
  DISCHARGE_NOTE_FILE <- discharge_note_files[[1L]]
}


discharge_note_file_sql <- quote_note_sql_path(
  con = con,
  path = DISCHARGE_NOTE_FILE
)


message(
  "Discharge-note source: ",
  DISCHARGE_NOTE_FILE
)


# ------------------------------------------------------------------------------
# 44.4 Verify source columns
# ------------------------------------------------------------------------------

discharge_note_source_schema <- DBI::dbGetQuery(
  con,
  paste0(
    "
    DESCRIBE

    SELECT *

    FROM read_csv_auto(
      ",
    discharge_note_file_sql,
    ",
      header = TRUE,
      all_varchar = TRUE,
      sample_size = -1,
      parallel = FALSE,
      max_line_size = 100000000
    );
    "
  )
) %>%
  tibble::as_tibble()


discharge_note_source_columns <-
  discharge_note_source_schema$column_name


required_discharge_note_source_columns <- c(
  "note_id",
  "subject_id",
  "hadm_id",
  "note_type",
  "note_seq",
  "charttime",
  "storetime",
  "text"
)


missing_discharge_note_source_columns <- setdiff(
  required_discharge_note_source_columns,
  discharge_note_source_columns
)


if (
  length(
    missing_discharge_note_source_columns
  ) > 0L
) {
  stop(
    paste0(
      "The discharge-note file is missing required column(s): ",
      paste(
        missing_discharge_note_source_columns,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


print(
  discharge_note_source_schema,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 44.5 Create the analytical episode key table
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    analytic_note_episode_keys AS

  SELECT
    CAST(episode_id AS VARCHAR)
      AS episode_id,

    TRY_CAST(subject_id AS BIGINT)
      AS subject_id,

    TRY_CAST(hadm_id AS BIGINT)
      AS hadm_id,

    TRY_CAST(stay_id AS BIGINT)
      AS stay_id,

    TRY_CAST(ed_intime AS TIMESTAMP)
      AS ed_intime,

    TRY_CAST(ed_outtime AS TIMESTAMP)
      AS ed_outtime,

    TRY_CAST(admittime AS TIMESTAMP)
      AS admittime,

    TRY_CAST(dischtime AS TIMESTAMP)
      AS dischtime

  FROM analytic_cohort_primary_table;
  "
)


analytic_note_episode_key_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*)::BIGINT
      AS episode_n,

    COUNT(DISTINCT episode_id)::BIGINT
      AS distinct_episode_n,

    COUNT(DISTINCT subject_id)::BIGINT
      AS distinct_patient_n,

    COUNT(*) FILTER (
      WHERE subject_id IS NULL
    )::BIGINT
      AS missing_subject_id_n,

    COUNT(*) FILTER (
      WHERE episode_id IS NULL
         OR trim(episode_id) = ''
    )::BIGINT
      AS missing_episode_id_n,

    COUNT(*) FILTER (
      WHERE hadm_id IS NULL
    )::BIGINT
      AS episode_without_hadm_id_n,

    COUNT(*) FILTER (
      WHERE hadm_id IS NOT NULL
    )::BIGINT
      AS episode_with_hadm_id_n

  FROM analytic_note_episode_keys;
  "
) %>%
  tibble::as_tibble()


if (
  analytic_note_episode_key_check$episode_n !=
  analytic_note_episode_key_check$distinct_episode_n ||
  analytic_note_episode_key_check$episode_n !=
  analytic_note_episode_key_check$distinct_patient_n ||
  analytic_note_episode_key_check$missing_subject_id_n != 0L ||
  analytic_note_episode_key_check$missing_episode_id_n != 0L
) {
  stop(
    "The analytical episode key table failed validation.",
    call. = FALSE
  )
}


print(
  analytic_note_episode_key_check,
  width = Inf
)


# ------------------------------------------------------------------------------
# 44.6 Create a temporary view of the raw discharge-note source
#
# All columns are initially read as text to avoid automatic conversion errors.
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  paste0(
    "
    CREATE OR REPLACE TEMP VIEW
      discharge_notes_source_raw AS

    SELECT
      CAST(note_id AS VARCHAR)
        AS note_id_raw,

      CAST(subject_id AS VARCHAR)
        AS subject_id_raw,

      CAST(hadm_id AS VARCHAR)
        AS hadm_id_raw,

      CAST(note_type AS VARCHAR)
        AS note_type_raw,

      CAST(note_seq AS VARCHAR)
        AS note_seq_raw,

      CAST(charttime AS VARCHAR)
        AS charttime_raw,

      CAST(storetime AS VARCHAR)
        AS storetime_raw,

      CAST(\"text\" AS VARCHAR)
        AS note_text_raw

    FROM read_csv_auto(
      ",
    discharge_note_file_sql,
    ",
      header = TRUE,
      all_varchar = TRUE,
      sample_size = -1,
      parallel = FALSE,
      max_line_size = 100000000
    );
    "
  )
)


# ------------------------------------------------------------------------------
# 44.7 Normalize and restrict notes to the analytical cohort
#
# Primary linkage:
#   subject_id + hadm_id
#
# Notes without hadm_id are not assigned through a temporal fallback.
#
# note_line_count is calculated without regexp_count(), for compatibility
# with older DuckDB versions.
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  paste0(
    "
    CREATE OR REPLACE TABLE
      analytic_discharge_notes_long AS

    WITH normalized_notes_base AS (

      SELECT
        nullif(
          trim(note_id_raw),
          ''
        )
          AS note_id,

        TRY_CAST(
          subject_id_raw
          AS BIGINT
        )
          AS subject_id,

        TRY_CAST(
          hadm_id_raw
          AS BIGINT
        )
          AS hadm_id,

        nullif(
          trim(note_type_raw),
          ''
        )
          AS note_type,

        TRY_CAST(
          note_seq_raw
          AS BIGINT
        )
          AS note_seq,

        TRY_CAST(
          charttime_raw
          AS TIMESTAMP
        )
          AS note_charttime,

        TRY_CAST(
          storetime_raw
          AS TIMESTAMP
        )
          AS note_storetime,

        CAST(
          note_text_raw
          AS VARCHAR
        )
          AS note_text_original

      FROM discharge_notes_source_raw
    ),

    normalized_notes AS (

      SELECT
        *,

        length(
          note_text_original
        )::BIGINT
          AS note_character_count,

        CASE
          WHEN note_text_original IS NULL
            THEN 0

          WHEN note_text_original = ''
            THEN 0

          ELSE
            (
              length(
                note_text_original
              )
              -
              length(
                replace(
                  note_text_original,
                  chr(10),
                  ''
                )
              )
              + 1
            )
        END::BIGINT
          AS note_line_count,

        CASE
          WHEN nullif(
            trim(note_text_original),
            ''
          ) IS NULL
            THEN TRUE

          ELSE FALSE
        END
          AS note_empty_text_flag

      FROM normalized_notes_base
    )

    SELECT
      row_number() OVER (
        ORDER BY
          coalesce(notes.note_id, ''),
          coalesce(notes.subject_id, -1),
          coalesce(notes.hadm_id, -1),
          coalesce(notes.note_seq, -1),
          coalesce(
            notes.note_charttime,
            TIMESTAMP '1900-01-01'
          ),
          coalesce(
            notes.note_storetime,
            TIMESTAMP '1900-01-01'
          )
      )::BIGINT
        AS note_source_row_id,

      keys.episode_id,
      keys.subject_id,
      keys.hadm_id,
      keys.stay_id,
      keys.ed_intime,
      keys.ed_outtime,
      keys.admittime,
      keys.dischtime,

      notes.note_id,
      notes.note_type,
      notes.note_seq,
      notes.note_charttime,
      notes.note_storetime,

      date_diff(
        'second',
        keys.ed_intime,
        notes.note_charttime
      ) / 3600.0
        AS note_charttime_hours_from_ed,

      date_diff(
        'second',
        keys.ed_intime,
        notes.note_storetime
      ) / 3600.0
        AS note_storetime_hours_from_ed,

      CASE
        WHEN notes.note_charttime IS NULL
          THEN NULL

        WHEN notes.note_charttime < keys.ed_intime
          THEN TRUE

        ELSE FALSE
      END
        AS note_charttime_before_ed_flag,

      CASE
        WHEN notes.note_charttime IS NULL
          OR keys.dischtime IS NULL
          THEN NULL

        WHEN notes.note_charttime > keys.dischtime
          THEN TRUE

        ELSE FALSE
      END
        AS note_charttime_after_discharge_flag,

      notes.note_character_count,
      notes.note_line_count,
      notes.note_empty_text_flag,

      notes.note_text_original,

      'subject_id_hadm_id'
        AS note_episode_linkage_method,

      ",
    DBI::dbQuoteString(
      con,
      DISCHARGE_NOTE_FILE
    ),
    "::VARCHAR
        AS note_source_file

    FROM analytic_note_episode_keys AS keys

    INNER JOIN normalized_notes AS notes
      ON notes.subject_id = keys.subject_id
     AND notes.hadm_id = keys.hadm_id

    WHERE keys.hadm_id IS NOT NULL;
    "
  )
)


# ------------------------------------------------------------------------------
# 44.8 Validate the restricted long-format note table
# ------------------------------------------------------------------------------

analytic_discharge_notes_check <- DBI::dbGetQuery(
  con,
  "
  WITH duplicate_note_ids AS (

    SELECT
      note_id

    FROM analytic_discharge_notes_long

    WHERE note_id IS NOT NULL

    GROUP BY note_id

    HAVING COUNT(*) > 1
  ),

  duplicate_episode_note_pairs AS (

    SELECT
      episode_id,
      note_id

    FROM analytic_discharge_notes_long

    WHERE note_id IS NOT NULL

    GROUP BY
      episode_id,
      note_id

    HAVING COUNT(*) > 1
  )

  SELECT
    (
      SELECT COUNT(*)
      FROM analytic_note_episode_keys
    )::BIGINT
      AS analytic_episode_n,

    (
      SELECT COUNT(*)
      FROM analytic_note_episode_keys
      WHERE hadm_id IS NOT NULL
    )::BIGINT
      AS analytic_episode_with_hadm_id_n,

    (
      SELECT COUNT(*)
      FROM analytic_note_episode_keys
      WHERE hadm_id IS NULL
    )::BIGINT
      AS analytic_episode_without_hadm_id_n,

    (
      SELECT COUNT(*)
      FROM analytic_discharge_notes_long
    )::BIGINT
      AS linked_note_n,

    (
      SELECT COUNT(DISTINCT note_id)
      FROM analytic_discharge_notes_long
      WHERE note_id IS NOT NULL
    )::BIGINT
      AS distinct_note_id_n,

    (
      SELECT COUNT(DISTINCT episode_id)
      FROM analytic_discharge_notes_long
    )::BIGINT
      AS episode_with_note_n,

    (
      SELECT COUNT(*)
      FROM analytic_note_episode_keys
    )
    -
    (
      SELECT COUNT(DISTINCT episode_id)
      FROM analytic_discharge_notes_long
    )
      AS episode_without_note_n,

    (
      SELECT COUNT(DISTINCT subject_id)
      FROM analytic_discharge_notes_long
    )::BIGINT
      AS patient_with_note_n,

    (
      SELECT COUNT(*)
      FROM analytic_discharge_notes_long
      WHERE note_id IS NULL
         OR trim(note_id) = ''
    )::BIGINT
      AS missing_note_id_n,

    (
      SELECT COUNT(*)
      FROM analytic_discharge_notes_long
      WHERE subject_id IS NULL
    )::BIGINT
      AS missing_subject_id_n,

    (
      SELECT COUNT(*)
      FROM analytic_discharge_notes_long
      WHERE hadm_id IS NULL
    )::BIGINT
      AS missing_hadm_id_n,

    (
      SELECT COUNT(*)
      FROM analytic_discharge_notes_long
      WHERE note_empty_text_flag
    )::BIGINT
      AS empty_note_text_n,

    (
      SELECT COUNT(*)
      FROM analytic_discharge_notes_long
      WHERE note_charttime IS NULL
    )::BIGINT
      AS missing_note_charttime_n,

    (
      SELECT COUNT(*)
      FROM analytic_discharge_notes_long
      WHERE note_storetime IS NULL
    )::BIGINT
      AS missing_note_storetime_n,

    (
      SELECT COUNT(*)
      FROM analytic_discharge_notes_long
      WHERE note_line_count < 0
    )::BIGINT
      AS invalid_note_line_count_n,

    (
      SELECT COUNT(*)
      FROM analytic_discharge_notes_long
      WHERE note_charttime_before_ed_flag
    )::BIGINT
      AS note_charttime_before_ed_n,

    (
      SELECT COUNT(*)
      FROM analytic_discharge_notes_long
      WHERE note_charttime_after_discharge_flag
    )::BIGINT
      AS note_charttime_after_discharge_n,

    (
      SELECT COUNT(*)
      FROM duplicate_note_ids
    )::BIGINT
      AS duplicated_note_id_n,

    (
      SELECT COUNT(*)
      FROM duplicate_episode_note_pairs
    )::BIGINT
      AS duplicated_episode_note_pair_n,

    (
      SELECT MIN(note_character_count)
      FROM analytic_discharge_notes_long
    )::BIGINT
      AS minimum_note_character_count,

    (
      SELECT median(note_character_count)
      FROM analytic_discharge_notes_long
    )::DOUBLE
      AS median_note_character_count,

    (
      SELECT MAX(note_character_count)
      FROM analytic_discharge_notes_long
    )::BIGINT
      AS maximum_note_character_count,

    (
      SELECT MIN(note_line_count)
      FROM analytic_discharge_notes_long
    )::BIGINT
      AS minimum_note_line_count,

    (
      SELECT median(note_line_count)
      FROM analytic_discharge_notes_long
    )::DOUBLE
      AS median_note_line_count,

    (
      SELECT MAX(note_line_count)
      FROM analytic_discharge_notes_long
    )::BIGINT
      AS maximum_note_line_count;
  "
) %>%
  tibble::as_tibble()


if (
  analytic_discharge_notes_check$linked_note_n == 0L ||
  analytic_discharge_notes_check$missing_note_id_n != 0L ||
  analytic_discharge_notes_check$missing_subject_id_n != 0L ||
  analytic_discharge_notes_check$missing_hadm_id_n != 0L ||
  analytic_discharge_notes_check$empty_note_text_n != 0L ||
  analytic_discharge_notes_check$invalid_note_line_count_n != 0L ||
  analytic_discharge_notes_check$duplicated_note_id_n != 0L ||
  analytic_discharge_notes_check$
  duplicated_episode_note_pair_n != 0L
) {
  stop(
    "The restricted discharge-note table failed critical validation.",
    call. = FALSE
  )
}


print(
  analytic_discharge_notes_check,
  width = Inf
)


# ------------------------------------------------------------------------------
# 44.9 Describe the number of notes per analytical episode
# ------------------------------------------------------------------------------

analytic_discharge_note_count_distribution <- DBI::dbGetQuery(
  con,
  "
  WITH episode_note_counts AS (

    SELECT
      keys.episode_id,

      COUNT(notes.note_id)::BIGINT
        AS discharge_note_n

    FROM analytic_note_episode_keys AS keys

    LEFT JOIN analytic_discharge_notes_long AS notes
      ON notes.episode_id = keys.episode_id

    GROUP BY keys.episode_id
  )

  SELECT
    discharge_note_n,

    COUNT(*)::BIGINT
      AS episode_n

  FROM episode_note_counts

  GROUP BY discharge_note_n

  ORDER BY discharge_note_n;
  "
) %>%
  tibble::as_tibble()


print(
  analytic_discharge_note_count_distribution,
  n = Inf,
  width = Inf
)


analytic_discharge_note_multiplicity_check <- DBI::dbGetQuery(
  con,
  "
  WITH episode_note_counts AS (

    SELECT
      episode_id,

      COUNT(*)::BIGINT
        AS discharge_note_n

    FROM analytic_discharge_notes_long

    GROUP BY episode_id
  )

  SELECT
    COUNT(*) FILTER (
      WHERE discharge_note_n = 1
    )::BIGINT
      AS episode_with_one_note_n,

    COUNT(*) FILTER (
      WHERE discharge_note_n > 1
    )::BIGINT
      AS episode_with_multiple_notes_n,

    SUM(
      CASE
        WHEN discharge_note_n > 1
          THEN discharge_note_n

        ELSE 0
      END
    )::BIGINT
      AS notes_from_multiple_note_episodes_n,

    MAX(discharge_note_n)::BIGINT
      AS maximum_notes_per_episode

  FROM episode_note_counts;
  "
) %>%
  tibble::as_tibble()


print(
  analytic_discharge_note_multiplicity_check,
  width = Inf
)


# ------------------------------------------------------------------------------
# 44.10 Create note availability for every analytical episode
#
# One row is retained for every analytical patient, including those without
# a linked discharge note.
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    analytic_discharge_note_availability AS

  SELECT
    keys.episode_id,
    keys.subject_id,
    keys.hadm_id,

    COUNT(notes.note_id)::BIGINT
      AS discharge_note_count,

    COUNT(notes.note_id) > 0
      AS discharge_note_available_flag,

    COUNT(notes.note_id) = 0
      AS discharge_note_missing_flag,

    COUNT(notes.note_id) > 1
      AS discharge_note_multiple_flag,

    MIN(notes.note_charttime)
      AS discharge_note_first_charttime,

    MAX(notes.note_charttime)
      AS discharge_note_last_charttime,

    MIN(notes.note_storetime)
      AS discharge_note_first_storetime,

    MAX(notes.note_storetime)
      AS discharge_note_last_storetime,

    MIN(notes.note_character_count)
      AS discharge_note_min_character_count,

    MAX(notes.note_character_count)
      AS discharge_note_max_character_count

  FROM analytic_note_episode_keys AS keys

  LEFT JOIN analytic_discharge_notes_long AS notes
    ON notes.episode_id = keys.episode_id

  GROUP BY
    keys.episode_id,
    keys.subject_id,
    keys.hadm_id;
  "
)


analytic_discharge_note_availability_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*)::BIGINT
      AS episode_n,

    COUNT(DISTINCT episode_id)::BIGINT
      AS distinct_episode_n,

    COUNT(DISTINCT subject_id)::BIGINT
      AS distinct_patient_n,

    COUNT(*) FILTER (
      WHERE discharge_note_available_flag
    )::BIGINT
      AS episode_with_note_n,

    COUNT(*) FILTER (
      WHERE discharge_note_missing_flag
    )::BIGINT
      AS episode_without_note_n,

    COUNT(*) FILTER (
      WHERE discharge_note_multiple_flag
    )::BIGINT
      AS episode_with_multiple_notes_n,

    SUM(discharge_note_count)::BIGINT
      AS linked_note_n,

    COUNT(*) FILTER (
      WHERE discharge_note_available_flag
        IS DISTINCT FROM (
          discharge_note_count > 0
        )
    )::BIGINT
      AS inconsistent_availability_flag_n,

    COUNT(*) FILTER (
      WHERE discharge_note_missing_flag
        IS DISTINCT FROM (
          discharge_note_count = 0
        )
    )::BIGINT
      AS inconsistent_missing_flag_n,

    COUNT(*) FILTER (
      WHERE discharge_note_multiple_flag
        IS DISTINCT FROM (
          discharge_note_count > 1
        )
    )::BIGINT
      AS inconsistent_multiple_flag_n

  FROM analytic_discharge_note_availability;
  "
) %>%
  tibble::as_tibble()


if (
  analytic_discharge_note_availability_check$episode_n !=
  analytic_discharge_note_availability_check$distinct_episode_n ||
  analytic_discharge_note_availability_check$episode_n !=
  analytic_discharge_note_availability_check$distinct_patient_n ||
  analytic_discharge_note_availability_check$
  inconsistent_availability_flag_n != 0L ||
  analytic_discharge_note_availability_check$
  inconsistent_missing_flag_n != 0L ||
  analytic_discharge_note_availability_check$
  inconsistent_multiple_flag_n != 0L
) {
  stop(
    "The discharge-note availability table failed validation.",
    call. = FALSE
  )
}


print(
  analytic_discharge_note_availability_check,
  width = Inf
)


# ------------------------------------------------------------------------------
# 44.11 Confirm that original note text remains stored in DuckDB
#
# The complete text remains in:
#   analytic_discharge_notes_long
#
# No Parquet or full-text CSV is produced in this section.
# ------------------------------------------------------------------------------

analytic_discharge_note_storage_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*)::BIGINT
      AS stored_note_n,

    COUNT(*) FILTER (
      WHERE note_text_original IS NOT NULL
        AND trim(note_text_original) != ''
    )::BIGINT
      AS stored_nonempty_text_n,

    COUNT(DISTINCT episode_id)::BIGINT
      AS episode_with_stored_note_n,

    SUM(note_character_count)::HUGEINT
      AS total_stored_character_n

  FROM analytic_discharge_notes_long;
  "
) %>%
  tibble::as_tibble()


if (
  analytic_discharge_note_storage_check$stored_note_n !=
  analytic_discharge_note_storage_check$stored_nonempty_text_n
) {
  stop(
    "One or more stored discharge notes have empty original text.",
    call. = FALSE
  )
}


print(
  analytic_discharge_note_storage_check,
  width = Inf
)


# 45. INTEGRATION OF DISCHARGE NOTES INTO THE ANALYTICAL COHORT
#
# Purpose:
#   1. Create one discharge-note record per analytical episode.
#   2. Integrate the original note text and metadata into the analytical cohort.
#   3. Preserve analytical episodes without a discharge note.
#   4. Confirm that the integration does not duplicate or exclude patients.
#
# Important:
#   - Exactly one discharge note was observed per episode with notes.
#   - No temporal-review flags are created.
#   - Original note text remains unchanged.
#   - No biomarker redaction is performed in this section.
# ==============================================================================


# ------------------------------------------------------------------------------
# 45.1 Verify prerequisites
# ------------------------------------------------------------------------------

if (
  !DBI::dbExistsTable(
    con,
    "analytic_cohort_primary_table"
  )
) {
  stop(
    "The table analytic_cohort_primary_table was not found.",
    call. = FALSE
  )
}


if (
  !DBI::dbExistsTable(
    con,
    "analytic_discharge_notes_long"
  )
) {
  stop(
    "The table analytic_discharge_notes_long was not found.",
    call. = FALSE
  )
}


if (
  !DBI::dbExistsTable(
    con,
    "analytic_discharge_note_availability"
  )
) {
  stop(
    "The table analytic_discharge_note_availability was not found.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 45.2 Confirm that there is no note multiplicity
#
# Each episode with a discharge note must have exactly one linked note.
# ------------------------------------------------------------------------------

analytic_discharge_note_uniqueness_check <- DBI::dbGetQuery(
  con,
  "
  WITH episode_note_counts AS (

    SELECT
      episode_id,
      COUNT(*)::BIGINT AS note_n

    FROM analytic_discharge_notes_long

    GROUP BY episode_id
  )

  SELECT
    COUNT(*)::BIGINT
      AS episode_with_note_n,

    COUNT(*) FILTER (
      WHERE note_n = 1
    )::BIGINT
      AS episode_with_one_note_n,

    COUNT(*) FILTER (
      WHERE note_n > 1
    )::BIGINT
      AS episode_with_multiple_notes_n,

    MAX(note_n)::BIGINT
      AS maximum_notes_per_episode

  FROM episode_note_counts;
  "
) %>%
  tibble::as_tibble()


if (
  analytic_discharge_note_uniqueness_check$
  episode_with_note_n !=
  analytic_discharge_note_uniqueness_check$
  episode_with_one_note_n ||
  analytic_discharge_note_uniqueness_check$
  episode_with_multiple_notes_n != 0L ||
  analytic_discharge_note_uniqueness_check$
  maximum_notes_per_episode != 1L
) {
  stop(
    paste0(
      "More than one discharge note was found for at least one episode. ",
      "A deterministic note-selection rule would be required."
    ),
    call. = FALSE
  )
}


print(
  analytic_discharge_note_uniqueness_check,
  width = Inf
)


# ------------------------------------------------------------------------------
# 45.3 Create one discharge-note row for every analytical episode
#
# Episodes without a discharge note remain present with missing note fields.
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    analytic_discharge_note_episode AS

  SELECT
    availability.episode_id,
    availability.subject_id,
    availability.hadm_id,

    availability.discharge_note_count,
    availability.discharge_note_available_flag,
    availability.discharge_note_missing_flag,
    availability.discharge_note_multiple_flag,

    notes.note_source_row_id
      AS discharge_note_source_row_id,

    notes.note_id
      AS discharge_note_id,

    notes.note_type
      AS discharge_note_type,

    notes.note_seq
      AS discharge_note_sequence,

    notes.note_charttime
      AS discharge_note_charttime,

    notes.note_storetime
      AS discharge_note_storetime,

    notes.note_charttime_hours_from_ed
      AS discharge_note_charttime_hours_from_ed,

    notes.note_storetime_hours_from_ed
      AS discharge_note_storetime_hours_from_ed,

    notes.note_character_count
      AS discharge_note_character_count,

    notes.note_line_count
      AS discharge_note_line_count,

    notes.note_episode_linkage_method
      AS discharge_note_linkage_method,

    notes.note_source_file
      AS discharge_note_source_file,

    notes.note_text_original
      AS discharge_note_text_original

  FROM analytic_discharge_note_availability AS availability

  LEFT JOIN analytic_discharge_notes_long AS notes
    ON notes.episode_id = availability.episode_id;
  "
)


# ------------------------------------------------------------------------------
# 45.4 Validate the episode-level note table
# ------------------------------------------------------------------------------

analytic_discharge_note_episode_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*)::BIGINT
      AS episode_n,

    COUNT(DISTINCT episode_id)::BIGINT
      AS distinct_episode_n,

    COUNT(DISTINCT subject_id)::BIGINT
      AS distinct_patient_n,

    COUNT(*) FILTER (
      WHERE discharge_note_available_flag
    )::BIGINT
      AS episode_with_note_n,

    COUNT(*) FILTER (
      WHERE discharge_note_missing_flag
    )::BIGINT
      AS episode_without_note_n,

    COUNT(*) FILTER (
      WHERE discharge_note_multiple_flag
    )::BIGINT
      AS episode_with_multiple_notes_n,

    SUM(discharge_note_count)::BIGINT
      AS linked_note_n,

    COUNT(*) FILTER (
      WHERE discharge_note_available_flag
        AND (
          discharge_note_id IS NULL
          OR trim(discharge_note_id) = ''
        )
    )::BIGINT
      AS available_note_missing_id_n,

    COUNT(*) FILTER (
      WHERE discharge_note_available_flag
        AND (
          discharge_note_text_original IS NULL
          OR trim(discharge_note_text_original) = ''
        )
    )::BIGINT
      AS available_note_missing_text_n,

    COUNT(*) FILTER (
      WHERE discharge_note_available_flag
        AND discharge_note_character_count <= 0
    )::BIGINT
      AS available_note_invalid_character_count_n,

    COUNT(*) FILTER (
      WHERE discharge_note_missing_flag
        AND discharge_note_id IS NOT NULL
    )::BIGINT
      AS missing_note_with_note_id_n,

    COUNT(*) FILTER (
      WHERE discharge_note_missing_flag
        AND discharge_note_text_original IS NOT NULL
    )::BIGINT
      AS missing_note_with_text_n,

    COUNT(*) FILTER (
      WHERE discharge_note_available_flag
        IS DISTINCT FROM (
          discharge_note_count = 1
        )
    )::BIGINT
      AS inconsistent_available_flag_n,

    COUNT(*) FILTER (
      WHERE discharge_note_missing_flag
        IS DISTINCT FROM (
          discharge_note_count = 0
        )
    )::BIGINT
      AS inconsistent_missing_flag_n

  FROM analytic_discharge_note_episode;
  "
) %>%
  tibble::as_tibble()


if (
  analytic_discharge_note_episode_check$episode_n !=
  analytic_discharge_note_episode_check$distinct_episode_n ||
  analytic_discharge_note_episode_check$episode_n !=
  analytic_discharge_note_episode_check$distinct_patient_n ||
  analytic_discharge_note_episode_check$
  episode_with_multiple_notes_n != 0L ||
  analytic_discharge_note_episode_check$
  available_note_missing_id_n != 0L ||
  analytic_discharge_note_episode_check$
  available_note_missing_text_n != 0L ||
  analytic_discharge_note_episode_check$
  available_note_invalid_character_count_n != 0L ||
  analytic_discharge_note_episode_check$
  missing_note_with_note_id_n != 0L ||
  analytic_discharge_note_episode_check$
  missing_note_with_text_n != 0L ||
  analytic_discharge_note_episode_check$
  inconsistent_available_flag_n != 0L ||
  analytic_discharge_note_episode_check$
  inconsistent_missing_flag_n != 0L
) {
  stop(
    "The episode-level discharge-note table failed validation.",
    call. = FALSE
  )
}


print(
  analytic_discharge_note_episode_check,
  width = Inf
)


# ------------------------------------------------------------------------------
# 45.5 Integrate notes into the analytical cohort
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    analytic_cohort_primary_with_notes_table AS

  SELECT
    cohort.*,

    notes.discharge_note_count,
    notes.discharge_note_available_flag,
    notes.discharge_note_missing_flag,
    notes.discharge_note_multiple_flag,

    notes.discharge_note_source_row_id,
    notes.discharge_note_id,
    notes.discharge_note_type,
    notes.discharge_note_sequence,

    notes.discharge_note_charttime,
    notes.discharge_note_storetime,

    notes.discharge_note_charttime_hours_from_ed,
    notes.discharge_note_storetime_hours_from_ed,

    notes.discharge_note_character_count,
    notes.discharge_note_line_count,

    notes.discharge_note_linkage_method,
    notes.discharge_note_source_file,

    notes.discharge_note_text_original

  FROM analytic_cohort_primary_table AS cohort

  LEFT JOIN analytic_discharge_note_episode AS notes
    ON notes.episode_id = cohort.episode_id;
  "
)


# ------------------------------------------------------------------------------
# 45.6 Validate the integrated analytical cohort
# ------------------------------------------------------------------------------

analytic_cohort_with_notes_check <- DBI::dbGetQuery(
  con,
  "
  SELECT
    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_table
    )::BIGINT
      AS source_episode_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_with_notes_table
    )::BIGINT
      AS output_episode_n,

    (
      SELECT COUNT(DISTINCT episode_id)
      FROM analytic_cohort_primary_with_notes_table
    )::BIGINT
      AS distinct_output_episode_n,

    (
      SELECT COUNT(DISTINCT subject_id)
      FROM analytic_cohort_primary_with_notes_table
    )::BIGINT
      AS distinct_output_patient_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_with_notes_table
      WHERE discharge_note_available_flag
    )::BIGINT
      AS episode_with_note_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_with_notes_table
      WHERE discharge_note_missing_flag
    )::BIGINT
      AS episode_without_note_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_with_notes_table
      WHERE discharge_note_available_flag IS NULL
         OR discharge_note_missing_flag IS NULL
         OR discharge_note_count IS NULL
    )::BIGINT
      AS missing_note_availability_information_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_with_notes_table
      WHERE discharge_note_available_flag
        AND (
          discharge_note_id IS NULL
          OR discharge_note_text_original IS NULL
        )
    )::BIGINT
      AS incomplete_available_note_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_with_notes_table
      WHERE discharge_note_missing_flag
        AND (
          discharge_note_id IS NOT NULL
          OR discharge_note_text_original IS NOT NULL
        )
    )::BIGINT
      AS unexpected_note_data_when_missing_n,

    (
      SELECT COUNT(*)
      FROM analytic_cohort_primary_with_notes_table
      WHERE analytic_primary_inclusion_flag
        IS DISTINCT FROM TRUE
    )::BIGINT
      AS invalid_primary_inclusion_flag_n

  ;
  "
) %>%
  tibble::as_tibble()


if (
  analytic_cohort_with_notes_check$source_episode_n !=
  analytic_cohort_with_notes_check$output_episode_n ||
  analytic_cohort_with_notes_check$output_episode_n !=
  analytic_cohort_with_notes_check$
  distinct_output_episode_n ||
  analytic_cohort_with_notes_check$output_episode_n !=
  analytic_cohort_with_notes_check$
  distinct_output_patient_n ||
  analytic_cohort_with_notes_check$
  missing_note_availability_information_n != 0L ||
  analytic_cohort_with_notes_check$
  incomplete_available_note_n != 0L ||
  analytic_cohort_with_notes_check$
  unexpected_note_data_when_missing_n != 0L ||
  analytic_cohort_with_notes_check$
  invalid_primary_inclusion_flag_n != 0L
) {
  stop(
    "The analytical cohort with discharge notes failed validation.",
    call. = FALSE
  )
}


print(
  analytic_cohort_with_notes_check,
  width = Inf
)


# ------------------------------------------------------------------------------
# 45.7 Summarize note availability by phenotype group
# ------------------------------------------------------------------------------

analytic_note_availability_by_group <- DBI::dbGetQuery(
  con,
  "
  SELECT
    primary_phenotype_group,

    MIN(primary_phenotype_group_order)
      AS primary_phenotype_group_order,

    COUNT(*)::BIGINT
      AS patient_n,

    COUNT(*) FILTER (
      WHERE discharge_note_available_flag
    )::BIGINT
      AS patient_with_note_n,

    COUNT(*) FILTER (
      WHERE discharge_note_missing_flag
    )::BIGINT
      AS patient_without_note_n,

    ROUND(
      100.0 *
      COUNT(*) FILTER (
        WHERE discharge_note_available_flag
      ) /
      COUNT(*),
      1
    )
      AS note_availability_percent,

    median(
      discharge_note_character_count
    ) FILTER (
      WHERE discharge_note_available_flag
    )
      AS median_note_character_count,

    MAX(
      discharge_note_character_count
    ) FILTER (
      WHERE discharge_note_available_flag
    )
      AS maximum_note_character_count

  FROM analytic_cohort_primary_with_notes_table

  GROUP BY primary_phenotype_group

  ORDER BY primary_phenotype_group_order;
  "
) %>%
  tibble::as_tibble()


print(
  analytic_note_availability_by_group,
  width = Inf
)


# ------------------------------------------------------------------------------
# 45.8 Load the integrated analytical cohort into R
# ------------------------------------------------------------------------------

analytic_cohort_primary_with_notes <- DBI::dbGetQuery(
  con,
  "
  SELECT *

  FROM analytic_cohort_primary_with_notes_table

  ORDER BY
    primary_phenotype_group_order,
    subject_id,
    ed_intime,
    episode_id;
  "
) %>%
  tibble::as_tibble()


assert_unique_key(
  analytic_cohort_primary_with_notes,
  "episode_id",
  "analytic_cohort_primary_with_notes"
)


assert_unique_key(
  analytic_cohort_primary_with_notes,
  "subject_id",
  "analytic_cohort_primary_with_notes"
)


# ------------------------------------------------------------------------------
# 45.9 Confirm integration in the R object
# ------------------------------------------------------------------------------

analytic_cohort_with_notes_r_check <- tibble::tibble(
  row_n = nrow(
    analytic_cohort_primary_with_notes
  ),
  
  column_n = ncol(
    analytic_cohort_primary_with_notes
  ),
  
  distinct_episode_n = dplyr::n_distinct(
    analytic_cohort_primary_with_notes$episode_id
  ),
  
  distinct_patient_n = dplyr::n_distinct(
    analytic_cohort_primary_with_notes$subject_id
  ),
  
  episode_with_note_n = sum(
    analytic_cohort_primary_with_notes$
      discharge_note_available_flag,
    na.rm = TRUE
  ),
  
  episode_without_note_n = sum(
    analytic_cohort_primary_with_notes$
      discharge_note_missing_flag,
    na.rm = TRUE
  ),
  
  note_text_column_present = (
    "discharge_note_text_original" %in%
      names(
        analytic_cohort_primary_with_notes
      )
  )
)


if (
  analytic_cohort_with_notes_r_check$row_n !=
  analytic_cohort_with_notes_r_check$
  distinct_episode_n ||
  analytic_cohort_with_notes_r_check$row_n !=
  analytic_cohort_with_notes_r_check$
  distinct_patient_n ||
  !analytic_cohort_with_notes_r_check$
  note_text_column_present
) {
  stop(
    "The R analytical-cohort object with notes failed validation.",
    call. = FALSE
  )
}


print(
  analytic_cohort_with_notes_r_check,
  width = Inf
)

# ==============================================================================
# 46. CLINICAL ADJUDICATION WORKLIST
#
# Purpose:
#   1. Preserve the complete analytical cohort.
#   2. Create a clinical-review worklist for episodes with a usable
#      discharge note.
#   3. Include:
#        - Identifiers and relevant timing.
#        - Demographics.
#        - ED and prior diagnoses.
#        - Medication reconciliation.
#        - Original discharge-note text.
#        - SOFA component-review flags.
#        - One compact review_reason_auto field.
#        - excluded and excluded_cause.
#        - One manual score field for each SOFA component.
#
# Episodes without a usable discharge note remain in the analytical cohort
# but are not included in the clinical-review worklist.
#
# Manual adjudication fields are initialized as missing. This section does not:
#   - Exclude patients automatically.
#   - Apply completed adjudication decisions.
#   - Complete or recalculate final SOFA automatically.
#   - Reclassify Sepsis-3.
#   - Modify the original discharge-note text.
#
# Patient-level adjudication products contain subject-level identifiers and
# original note text and are intended for local controlled use only.
#
# Main objects:
#   - clinical_adjudication_worklist
#   - clinical_adjudication_worklist_table
# ==============================================================================

# ------------------------------------------------------------------------------
# 46.1 Verify prerequisites
# ------------------------------------------------------------------------------

required_adjudication_objects <- c(
  "analytic_cohort_primary_with_notes",
  "OUTPUT_DIR",
  "con"
)


adjudication_environment <- environment()


adjudication_object_check <- tibble::tibble(
  object_name = required_adjudication_objects,
  
  object_present = vapply(
    required_adjudication_objects,
    
    function(object_name) {
      
      exists(
        object_name,
        envir = adjudication_environment,
        inherits = FALSE
      )
    },
    
    logical(1)
  )
)


print(
  adjudication_object_check,
  n = Inf,
  width = Inf
)


missing_adjudication_objects <-
  adjudication_object_check$
  object_name[
    !adjudication_object_check$
      object_present
  ]


if (
  length(
    missing_adjudication_objects
  ) > 0L
) {
  
  stop(
    paste0(
      "Missing object(s) required for adjudication: ",
      paste(
        missing_adjudication_objects,
        collapse = ", "
      ),
      ". Run Sections 1-45 first."
    ),
    call. = FALSE
  )
}


if (!DBI::dbIsValid(con)) {
  
  stop(
    "The DuckDB connection is not valid.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 46.2 Helper for logical and binary flags
# ------------------------------------------------------------------------------

as_review_boolean <- function(x) {
  
  if (is.logical(x)) {
    
    return(
      !is.na(x) & x
    )
  }
  
  
  if (
    is.numeric(x) ||
    is.integer(x)
  ) {
    
    return(
      !is.na(x) & x != 0
    )
  }
  
  
  tolower(
    trimws(
      as.character(x)
    )
  ) %in% c(
    "true",
    "t",
    "1",
    "yes",
    "y"
  )
}


# ------------------------------------------------------------------------------
# 46.3 Restrict the adjudication worklist to patients with usable notes
#
# Important:
#   - The complete analytical cohort remains unchanged.
#   - Patients without notes are removed only from the adjudication worklist.
#   - Missing note is not coded as a clinical exclusion.
# ------------------------------------------------------------------------------

adjudication_source_all <-
  analytic_cohort_primary_with_notes


required_note_filter_columns <- c(
  "episode_id",
  "subject_id",
  "hadm_id",
  "stay_id",
  
  "ed_intime",
  "cbc_index_charttime",
  
  "discharge_note_available_flag",
  "discharge_note_missing_flag",
  "discharge_note_text_original"
)


missing_note_filter_columns <- setdiff(
  required_note_filter_columns,
  names(
    adjudication_source_all
  )
)


if (
  length(
    missing_note_filter_columns
  ) > 0L
) {
  
  stop(
    paste0(
      "Missing required note-filter column(s): ",
      paste(
        missing_note_filter_columns,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


note_text_usable_flag <- (
  !is.na(
    adjudication_source_all$
      discharge_note_text_original
  ) &
    nzchar(
      trimws(
        adjudication_source_all$
          discharge_note_text_original
      )
    )
)


note_available_flag <- as_review_boolean(
  adjudication_source_all$
    discharge_note_available_flag
)


note_missing_flag <- as_review_boolean(
  adjudication_source_all$
    discharge_note_missing_flag
)


adjudication_note_eligible_flag <- (
  note_available_flag &
    !note_missing_flag &
    note_text_usable_flag
)


adjudication_source <-
  adjudication_source_all[
    adjudication_note_eligible_flag,
    ,
    drop = FALSE
  ]


adjudication_missing_note_cases <-
  adjudication_source_all[
    !adjudication_note_eligible_flag,
    ,
    drop = FALSE
  ]


# ------------------------------------------------------------------------------
# 46.3.1 Validate source and note restriction
# ------------------------------------------------------------------------------

if (
  anyDuplicated(
    adjudication_source_all$
    episode_id
  ) > 0L
) {
  
  stop(
    "Duplicated episode_id values were found in the full analytical source.",
    call. = FALSE
  )
}


if (
  anyDuplicated(
    adjudication_source$
    episode_id
  ) > 0L
) {
  
  stop(
    "Duplicated episode_id values were found after note restriction.",
    call. = FALSE
  )
}


if (
  anyDuplicated(
    adjudication_source$
    subject_id
  ) > 0L
) {
  
  stop(
    "Duplicated subject_id values were found after note restriction.",
    call. = FALSE
  )
}


remaining_empty_note_n <- sum(
  is.na(
    adjudication_source$
      discharge_note_text_original
  ) |
    !nzchar(
      trimws(
        dplyr::coalesce(
          adjudication_source$
            discharge_note_text_original,
          ""
        )
      )
    )
)


if (
  remaining_empty_note_n != 0L
) {
  
  stop(
    paste0(
      remaining_empty_note_n,
      " patient(s) with an empty note remained in the adjudication source."
    ),
    call. = FALSE
  )
}


adjudication_note_filter_check <- tibble::tibble(
  full_analytical_patient_n =
    nrow(
      adjudication_source_all
    ),
  
  adjudication_patient_n =
    nrow(
      adjudication_source
    ),
  
  removed_without_usable_note_n =
    nrow(
      adjudication_missing_note_cases
    ),
  
  remaining_empty_note_n =
    remaining_empty_note_n,
  
  reconciliation_n =
    nrow(
      adjudication_source
    ) +
    nrow(
      adjudication_missing_note_cases
    )
)


if (
  adjudication_note_filter_check$
  reconciliation_n !=
  adjudication_note_filter_check$
  full_analytical_patient_n
) {
  
  stop(
    "The note-availability restriction failed reconciliation.",
    call. = FALSE
  )
}


print(
  adjudication_note_filter_check,
  width = Inf
)


# ------------------------------------------------------------------------------
# 46.3.2 Summarize removed patients by phenotype
# ------------------------------------------------------------------------------

if (
  "primary_phenotype_group" %in%
  names(
    adjudication_missing_note_cases
  )
) {
  
  adjudication_missing_note_summary <-
    adjudication_missing_note_cases %>%
    dplyr::count(
      primary_phenotype_group,
      name = "removed_without_note_n"
    ) %>%
    dplyr::arrange(
      primary_phenotype_group
    )
  
} else {
  
  adjudication_missing_note_summary <-
    tibble::tibble(
      primary_phenotype_group =
        "not_available",
      
      removed_without_note_n =
        nrow(
          adjudication_missing_note_cases
        )
    )
}


adjudication_missing_note_summary <-
  dplyr::bind_rows(
    adjudication_missing_note_summary,
    
    tibble::tibble(
      primary_phenotype_group =
        "total",
      
      removed_without_note_n =
        nrow(
          adjudication_missing_note_cases
        )
    )
  )


print(
  adjudication_missing_note_summary,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------------------------
# 46.4 Define columns required for clinical adjudication
# ------------------------------------------------------------------------------

source_column_names <-
  names(
    adjudication_source
  )


adjudication_identifier_columns <- c(
  "episode_id",
  "subject_id",
  "hadm_id",
  "stay_id"
)


adjudication_timing_columns <- c(
  "ed_intime",
  "ed_outtime",
  "admittime",
  "dischtime",
  "lab_window_start",
  "lab_window_end",
  "ed_length_hours",
  "cbc_index_charttime",
  "cbc_index_hours_from_ed"
)


adjudication_demographic_columns <- unique(
  c(
    "gender",
    "race_ed",
    "race_admission",
    "arrival_transport",
    "admission_type",
    "admission_location",
    "insurance",
    "language",
    "marital_status",
    
    grep(
      pattern = paste0(
        "(^age$",
        "|^anchor_age$",
        "|age_at_",
        "|_age$",
        "|age_year",
        "|pregnan",
        "|gestat",
        "|obstetric)"
      ),
      
      x = source_column_names,
      value = TRUE,
      ignore.case = TRUE
    )
  )
)


adjudication_episode_structure_columns <- c(
  "repeated_subject_flag",
  "n_ed_episodes_subject",
  "n_ed_stays_hadm",
  "multiple_ed_stays_same_hadm_flag",
  
  "analytic_eligible_episode_count_subject",
  "analytic_eligible_episode_rank",
  "later_analytic_eligible_episode_n_subject",
  "analytic_primary_inclusion_flag"
)


adjudication_ed_diagnosis_columns <- grep(
  "^ed_diagn",
  source_column_names,
  value = TRUE
)


adjudication_prior_diagnosis_columns <- grep(
  "^prior_diagnosis_",
  source_column_names,
  value = TRUE
)


adjudication_other_diagnosis_columns <- grep(
  "diagnos",
  source_column_names,
  value = TRUE,
  ignore.case = TRUE
)


adjudication_medrecon_columns <- grep(
  "^medrecon_",
  source_column_names,
  value = TRUE
)


adjudication_charlson_columns <- c(
  "charlson_available_flag",
  "age_score",
  "myocardial_infarct",
  "congestive_heart_failure",
  "peripheral_vascular_disease",
  "cerebrovascular_disease",
  "dementia",
  "chronic_pulmonary_disease",
  "rheumatic_disease",
  "peptic_ulcer_disease",
  "mild_liver_disease",
  "diabetes_without_cc",
  "diabetes_with_cc",
  "paraplegia",
  "renal_disease",
  "malignant_cancer",
  "severe_liver_disease",
  "metastatic_solid_tumor",
  "aids",
  "charlson_comorbidity_index"
)


adjudication_sofa_flag_columns <- c(
  "sofa_respiratory_observed_flag",
  "sofa_respiratory_review_flag",
  
  "sofa_coagulation_observed_flag",
  "sofa_coagulation_review_flag",
  
  "sofa_hepatic_observed_flag",
  "sofa_hepatic_review_flag",
  
  "sofa_cardiovascular_observed_flag",
  "sofa_cardiovascular_review_flag",
  
  "sofa_neurologic_observed_flag",
  "sofa_neurologic_review_required",
  
  "sofa_renal_observed_flag",
  "sofa_renal_review_flag"
)


adjudication_note_columns <- c(
  "discharge_note_count",
  "discharge_note_available_flag",
  "discharge_note_missing_flag",
  "discharge_note_multiple_flag",
  
  "discharge_note_source_row_id",
  "discharge_note_id",
  "discharge_note_type",
  "discharge_note_sequence",
  
  "discharge_note_charttime",
  "discharge_note_storetime",
  
  "discharge_note_charttime_hours_from_ed",
  "discharge_note_storetime_hours_from_ed",
  
  "discharge_note_character_count",
  "discharge_note_line_count",
  
  "discharge_note_linkage_method",
  "discharge_note_source_file",
  
  "discharge_note_text_original"
)


adjudication_keep_columns <- unique(
  c(
    adjudication_identifier_columns,
    adjudication_timing_columns,
    adjudication_demographic_columns,
    adjudication_episode_structure_columns,
    
    adjudication_ed_diagnosis_columns,
    adjudication_prior_diagnosis_columns,
    adjudication_other_diagnosis_columns,
    adjudication_medrecon_columns,
    adjudication_charlson_columns,
    
    adjudication_sofa_flag_columns,
    adjudication_note_columns
  )
)


adjudication_keep_columns <- intersect(
  adjudication_keep_columns,
  source_column_names
)


# ------------------------------------------------------------------------------
# 46.5 Construct the adjudication worklist
# ------------------------------------------------------------------------------

clinical_adjudication_worklist <-
  adjudication_source %>%
  dplyr::select(
    dplyr::all_of(
      adjudication_keep_columns
    )
  )


# ------------------------------------------------------------------------------
# 46.6 Determine which SOFA components require review
#
# A component is flagged when:
#   - No structured component was observed; OR
#   - Its source-specific review flag is active.
# ------------------------------------------------------------------------------

clinical_adjudication_worklist$
  review_respiratory_needed_flag <- (
    !as_review_boolean(
      clinical_adjudication_worklist$
        sofa_respiratory_observed_flag
    ) |
      as_review_boolean(
        clinical_adjudication_worklist$
          sofa_respiratory_review_flag
      )
  )


clinical_adjudication_worklist$
  review_coagulation_needed_flag <- (
    !as_review_boolean(
      clinical_adjudication_worklist$
        sofa_coagulation_observed_flag
    ) |
      as_review_boolean(
        clinical_adjudication_worklist$
          sofa_coagulation_review_flag
      )
  )


clinical_adjudication_worklist$
  review_hepatic_needed_flag <- (
    !as_review_boolean(
      clinical_adjudication_worklist$
        sofa_hepatic_observed_flag
    ) |
      as_review_boolean(
        clinical_adjudication_worklist$
          sofa_hepatic_review_flag
      )
  )


clinical_adjudication_worklist$
  review_cardiovascular_needed_flag <- (
    !as_review_boolean(
      clinical_adjudication_worklist$
        sofa_cardiovascular_observed_flag
    ) |
      as_review_boolean(
        clinical_adjudication_worklist$
          sofa_cardiovascular_review_flag
      )
  )


clinical_adjudication_worklist$
  review_neurologic_needed_flag <- (
    !as_review_boolean(
      clinical_adjudication_worklist$
        sofa_neurologic_observed_flag
    ) |
      as_review_boolean(
        clinical_adjudication_worklist$
          sofa_neurologic_review_required
      )
  )


clinical_adjudication_worklist$
  review_renal_needed_flag <- (
    !as_review_boolean(
      clinical_adjudication_worklist$
        sofa_renal_observed_flag
    ) |
      as_review_boolean(
        clinical_adjudication_worklist$
          sofa_renal_review_flag
      )
  )


clinical_adjudication_worklist$
  review_any_sofa_component_needed_flag <- (
    clinical_adjudication_worklist$
      review_respiratory_needed_flag |
      clinical_adjudication_worklist$
      review_coagulation_needed_flag |
      clinical_adjudication_worklist$
      review_hepatic_needed_flag |
      clinical_adjudication_worklist$
      review_cardiovascular_needed_flag |
      clinical_adjudication_worklist$
      review_neurologic_needed_flag |
      clinical_adjudication_worklist$
      review_renal_needed_flag
  )


# ------------------------------------------------------------------------------
# 46.7 Create review_reason_auto
# ------------------------------------------------------------------------------

review_reason_matrix <- data.frame(
  respiratory = ifelse(
    clinical_adjudication_worklist$
      review_respiratory_needed_flag,
    "respiratory",
    NA_character_
  ),
  
  coagulation = ifelse(
    clinical_adjudication_worklist$
      review_coagulation_needed_flag,
    "coagulation",
    NA_character_
  ),
  
  hepatic = ifelse(
    clinical_adjudication_worklist$
      review_hepatic_needed_flag,
    "hepatic",
    NA_character_
  ),
  
  cardiovascular = ifelse(
    clinical_adjudication_worklist$
      review_cardiovascular_needed_flag,
    "cardiovascular",
    NA_character_
  ),
  
  neurologic = ifelse(
    clinical_adjudication_worklist$
      review_neurologic_needed_flag,
    "neurologic",
    NA_character_
  ),
  
  renal = ifelse(
    clinical_adjudication_worklist$
      review_renal_needed_flag,
    "renal",
    NA_character_
  ),
  
  stringsAsFactors = FALSE
)


clinical_adjudication_worklist$
  review_reason_auto <- apply(
    review_reason_matrix,
    1L,
    
    function(x) {
      
      reasons <- unique(
        stats::na.omit(x)
      )
      
      
      if (
        length(
          reasons
        ) == 0L
      ) {
        
        return(
          "routine_review"
        )
      }
      
      
      paste(
        reasons,
        collapse = "; "
      )
    }
  )


# ------------------------------------------------------------------------------
# 46.8 Add only the requested manual fields
#
# excluded:
#   blank = not reviewed
#   0     = not excluded
#   1     = excluded
#
# excluded_cause:
#   Controlled text category.
#   Multiple causes may be separated with "; ".
#
# Manual SOFA fields:
#   blank = not reviewed or no manual score entered
#   0-4   = manually assigned component score
# ------------------------------------------------------------------------------

clinical_adjudication_worklist <-
  clinical_adjudication_worklist %>%
  dplyr::mutate(
    excluded =
      NA_integer_,
    
    excluded_cause =
      NA_character_,
    
    manual_respiratory_sofa_score =
      NA_integer_,
    
    manual_coagulation_sofa_score =
      NA_integer_,
    
    manual_hepatic_sofa_score =
      NA_integer_,
    
    manual_cardiovascular_sofa_score =
      NA_integer_,
    
    manual_neurologic_sofa_score =
      NA_integer_,
    
    manual_renal_sofa_score =
      NA_integer_
  )


# ------------------------------------------------------------------------------
# 46.9 Create deterministic randomized review order
#
# episode_id remains the join key.
# ------------------------------------------------------------------------------

ADJUDICATION_RANDOM_SEED <- 20260721L


set.seed(
  ADJUDICATION_RANDOM_SEED
)


adjudication_random_order <- sample.int(
  n = nrow(
    clinical_adjudication_worklist
  ),
  
  size = nrow(
    clinical_adjudication_worklist
  ),
  
  replace = FALSE
)


clinical_adjudication_worklist <-
  clinical_adjudication_worklist[
    adjudication_random_order,
    ,
    drop = FALSE
  ]


clinical_adjudication_worklist <-
  clinical_adjudication_worklist %>%
  dplyr::mutate(
    review_order =
      dplyr::row_number(),
    
    review_case_id = sprintf(
      "ADR%05d",
      review_order
    )
  ) %>%
  dplyr::relocate(
    review_case_id,
    review_order,
    .before = episode_id
  ) %>%
  dplyr::relocate(
    review_respiratory_needed_flag,
    review_coagulation_needed_flag,
    review_hepatic_needed_flag,
    review_cardiovascular_needed_flag,
    review_neurologic_needed_flag,
    review_renal_needed_flag,
    review_any_sofa_component_needed_flag,
    review_reason_auto,
    
    excluded,
    excluded_cause,
    
    manual_respiratory_sofa_score,
    manual_coagulation_sofa_score,
    manual_hepatic_sofa_score,
    manual_cardiovascular_sofa_score,
    manual_neurologic_sofa_score,
    manual_renal_sofa_score,
    
    .after = cbc_index_charttime
  )


# ------------------------------------------------------------------------------
# 46.10 Validate the worklist
# ------------------------------------------------------------------------------

manual_adjudication_columns <- c(
  "excluded",
  "excluded_cause",
  "manual_respiratory_sofa_score",
  "manual_coagulation_sofa_score",
  "manual_hepatic_sofa_score",
  "manual_cardiovascular_sofa_score",
  "manual_neurologic_sofa_score",
  "manual_renal_sofa_score"
)


manual_nonmissing_n <- sum(
  vapply(
    clinical_adjudication_worklist[
      manual_adjudication_columns
    ],
    
    function(x) {
      
      sum(
        !is.na(x)
      )
    },
    
    numeric(1)
  )
)

clinical_adjudication_worklist_check <- tibble::tibble(
  full_analytical_row_n =
    nrow(
      adjudication_source_all
    ),
  
  removed_without_note_n =
    nrow(
      adjudication_missing_note_cases
    ),
  
  adjudication_source_row_n =
    nrow(
      adjudication_source
    ),
  
  worklist_row_n =
    nrow(
      clinical_adjudication_worklist
    ),
  
  distinct_review_case_n =
    dplyr::n_distinct(
      clinical_adjudication_worklist$
        review_case_id
    ),
  
  distinct_episode_n =
    dplyr::n_distinct(
      clinical_adjudication_worklist$
        episode_id
    ),
  
  distinct_patient_n =
    dplyr::n_distinct(
      clinical_adjudication_worklist$
        subject_id
    ),
  
  patient_with_note_n = sum(
    as_review_boolean(
      clinical_adjudication_worklist$
        discharge_note_available_flag
    )
  ),
  
  patient_without_note_n = sum(
    as_review_boolean(
      clinical_adjudication_worklist$
        discharge_note_missing_flag
    )
  ),
  
  empty_note_text_n = sum(
    is.na(
      clinical_adjudication_worklist$
        discharge_note_text_original
    ) |
      !nzchar(
        trimws(
          dplyr::coalesce(
            clinical_adjudication_worklist$
              discharge_note_text_original,
            ""
          )
        )
      )
  ),
  
  manual_nonmissing_n =
    manual_nonmissing_n,
  
  reconciliation_n =
    nrow(
      clinical_adjudication_worklist
    ) +
    nrow(
      adjudication_missing_note_cases
    )
)


if (
  clinical_adjudication_worklist_check$
  full_analytical_row_n !=
  clinical_adjudication_worklist_check$
  reconciliation_n ||
  clinical_adjudication_worklist_check$
  adjudication_source_row_n !=
  clinical_adjudication_worklist_check$
  worklist_row_n ||
  clinical_adjudication_worklist_check$
  worklist_row_n !=
  clinical_adjudication_worklist_check$
  distinct_review_case_n ||
  clinical_adjudication_worklist_check$
  worklist_row_n !=
  clinical_adjudication_worklist_check$
  distinct_episode_n ||
  clinical_adjudication_worklist_check$
  worklist_row_n !=
  clinical_adjudication_worklist_check$
  distinct_patient_n ||
  clinical_adjudication_worklist_check$
  patient_with_note_n !=
  clinical_adjudication_worklist_check$
  worklist_row_n ||
  clinical_adjudication_worklist_check$
  patient_without_note_n != 0L ||
  clinical_adjudication_worklist_check$
  empty_note_text_n != 0L ||
  clinical_adjudication_worklist_check$
  manual_nonmissing_n != 0L
) {
  
  stop(
    "The clinical adjudication worklist failed validation.",
    call. = FALSE
  )
}


print(
  clinical_adjudication_worklist_check,
  width = Inf
)


# ------------------------------------------------------------------------------
# 46.11 Summarize SOFA review flags
# ------------------------------------------------------------------------------

clinical_adjudication_component_summary <- tibble::tibble(
  review_component = c(
    "respiratory",
    "coagulation",
    "hepatic",
    "cardiovascular",
    "neurologic",
    "renal",
    "any_sofa_component"
  ),
  
  patient_requiring_review_n = c(
    sum(
      clinical_adjudication_worklist$
        review_respiratory_needed_flag,
      na.rm = TRUE
    ),
    
    sum(
      clinical_adjudication_worklist$
        review_coagulation_needed_flag,
      na.rm = TRUE
    ),
    
    sum(
      clinical_adjudication_worklist$
        review_hepatic_needed_flag,
      na.rm = TRUE
    ),
    
    sum(
      clinical_adjudication_worklist$
        review_cardiovascular_needed_flag,
      na.rm = TRUE
    ),
    
    sum(
      clinical_adjudication_worklist$
        review_neurologic_needed_flag,
      na.rm = TRUE
    ),
    
    sum(
      clinical_adjudication_worklist$
        review_renal_needed_flag,
      na.rm = TRUE
    ),
    
    sum(
      clinical_adjudication_worklist$
        review_any_sofa_component_needed_flag,
      na.rm = TRUE
    )
  )
) %>%
  dplyr::mutate(
    percent_of_worklist = round(
      100 *
        patient_requiring_review_n /
        nrow(
          clinical_adjudication_worklist
        ),
      1
    )
  )


print(
  clinical_adjudication_component_summary,
  n = Inf,
  width = Inf
)


# 47. PREPARE FINAL ANALYTICAL AND ADJUDICATION OBJECTS
#
# Main changes:
#
#   1. The exact adjudication worklist is preserved as an RDS object.
#      It retains the original notes, including their original line breaks.
#
#   2. The Excel worklist is exported as a real XLSX workbook.
#      Newline characters are replaced by the visible marker " ⏎ ".
#
#   3. Notes longer than 30,000 characters are divided into multiple columns.
#      Blank part_002 means that chunk_n == 1, not that content was lost.
#
#   4. Microbiology study types and identified organisms are added to
#      analytic_cohort_primary_unblinded before final export.
#
#   5. Antimicrobial susceptibility information is not included.
# ==============================================================================


# ------------------------------------------------------------------------------
# 47.1 Verify prerequisites and packages
# ------------------------------------------------------------------------------

required_section47_objects <- c(
  "analytic_cohort_primary_with_notes",
  "clinical_adjudication_worklist",
  "clinical_adjudication_component_summary",
  "adjudication_missing_note_summary",
  "adjudication_missing_note_cases",
  "OUTPUT_DIR",
  "con"
)


section47_environment <- environment()


missing_section47_objects <- required_section47_objects[
  !vapply(
    required_section47_objects,
    function(object_name) {
      exists(
        object_name,
        envir = section47_environment,
        inherits = FALSE
      )
    },
    logical(1)
  )
]


if (length(missing_section47_objects) > 0L) {
  stop(
    paste0(
      "Missing object(s) required for Sections 47-48: ",
      paste(
        missing_section47_objects,
        collapse = ", "
      ),
      ". Run Sections 1-46 first."
    ),
    call. = FALSE
  )
}


if (!DBI::dbIsValid(con)) {
  stop(
    "The DuckDB connection is not valid.",
    call. = FALSE
  )
}


if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop(
    paste0(
      "Package 'openxlsx' is required to create the Excel workbook. ",
      "Install it once with: install.packages('openxlsx')"
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 47.2 General export helpers
# ------------------------------------------------------------------------------

prepare_final_export <- function(data) {
  
  output <- as.data.frame(
    data,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  
  duplicated_column_names <- names(output)[
    duplicated(names(output))
  ]
  
  
  if (length(duplicated_column_names) > 0L) {
    stop(
      paste0(
        "Duplicated column name(s): ",
        paste(
          unique(duplicated_column_names),
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  
  list_columns <- names(output)[
    vapply(
      output,
      is.list,
      logical(1)
    )
  ]
  
  
  if (length(list_columns) > 0L) {
    stop(
      paste0(
        "List-column(s) cannot be exported: ",
        paste(
          list_columns,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  
  for (column_name in names(output)) {
    
    current_column <- output[[column_name]]
    
    
    if (inherits(current_column, "POSIXt")) {
      
      output[[column_name]] <- ifelse(
        is.na(current_column),
        NA_character_,
        format(
          current_column,
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        )
      )
    }
    
    
    if (inherits(current_column, "Date")) {
      
      output[[column_name]] <- ifelse(
        is.na(current_column),
        NA_character_,
        format(
          current_column,
          format = "%Y-%m-%d"
        )
      )
    }
    
    
    if (inherits(current_column, "integer64")) {
      
      output[[column_name]] <- as.character(
        current_column
      )
    }
  }
  
  
  output
}


sanitize_text_for_excel <- function(x) {
  
  output <- as.character(x)
  
  
  nonmissing_flag <- !is.na(output)
  
  
  output[nonmissing_flag] <- enc2utf8(
    output[nonmissing_flag]
  )
  
  
  # Remove XML-incompatible control characters.
  output[nonmissing_flag] <- gsub(
    pattern = paste0(
      "[",
      "\\x{0001}-\\x{0008}",
      "\\x{000B}",
      "\\x{000C}",
      "\\x{000E}-\\x{001F}",
      "]"
    ),
    replacement = " ",
    x = output[nonmissing_flag],
    perl = TRUE
  )
  
  
  # Replace tabs with spaces.
  output[nonmissing_flag] <- gsub(
    pattern = "\t",
    replacement = "    ",
    x = output[nonmissing_flag],
    fixed = TRUE
  )
  
  
  # Replace every line-break convention with a visible marker.
  output[nonmissing_flag] <- gsub(
    pattern = "\r\n|\r|\n",
    replacement = " \u23ce ",
    x = output[nonmissing_flag],
    perl = TRUE
  )
  
  
  output
}


# ==============================================================================
# 47.3 CREATE THE UNBLINDED ANALYTICAL COHORT
# ==============================================================================

note_related_columns <- grep(
  pattern = "^(discharge_note_|note_)",
  x = names(
    analytic_cohort_primary_with_notes
  ),
  value = TRUE
)


analytic_cohort_primary_unblinded <-
  analytic_cohort_primary_with_notes %>%
  dplyr::select(
    -dplyr::all_of(
      note_related_columns
    )
  )


if (
  nrow(analytic_cohort_primary_unblinded) !=
  nrow(analytic_cohort_primary_with_notes) ||
  anyDuplicated(
    analytic_cohort_primary_unblinded$episode_id
  ) > 0L ||
  anyDuplicated(
    analytic_cohort_primary_unblinded$subject_id
  ) > 0L
) {
  stop(
    "The recreated unblinded analytical cohort failed validation.",
    call. = FALSE
  )
}


unblinded_pre_microbiology_row_n <-
  nrow(
    analytic_cohort_primary_unblinded
  )


unblinded_pre_microbiology_column_n <-
  ncol(
    analytic_cohort_primary_unblinded
  )

# ------------------------------------------------------------------------------
# Validate the prespecified biomarker set
# ------------------------------------------------------------------------------

required_biomarker_source_columns <- c(
  "crp_index_value_mg_l",
  "cbc_index_wbc_for_calculation",
  "cbc_index_neutrophil_count",
  "cbc_index_lymphocyte_count",
  "cbc_index_monocyte_count",
  "cbc_index_platelet_count",
  "nlr",
  "plr",
  "mlr",
  "nmr"
)


missing_biomarker_source_columns <- setdiff(
  required_biomarker_source_columns,
  names(analytic_cohort_primary_unblinded)
)


if (length(missing_biomarker_source_columns) > 0L) {
  stop(
    paste0(
      "The analytical cohort is missing biomarker source column(s): ",
      paste(
        missing_biomarker_source_columns,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


non_numeric_biomarker_source_columns <-
  required_biomarker_source_columns[
    !vapply(
      analytic_cohort_primary_unblinded[
        required_biomarker_source_columns
      ],
      is.numeric,
      logical(1L)
    )
  ]


if (length(non_numeric_biomarker_source_columns) > 0L) {
  stop(
    paste0(
      "Biomarker source column(s) are not numeric: ",
      paste(
        non_numeric_biomarker_source_columns,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


biomarker_validation_data <-
  analytic_cohort_primary_unblinded |>
  dplyr::transmute(
    crp =
      crp_index_value_mg_l,
    
    wbc =
      cbc_index_wbc_for_calculation,
    
    neutrophil_count =
      cbc_index_neutrophil_count,
    
    lymphocyte_count =
      cbc_index_lymphocyte_count,
    
    monocyte_count =
      cbc_index_monocyte_count,
    
    nlr =
      nlr,
    
    plr =
      plr,
    
    mlr =
      mlr,
    
    nmr =
      nmr,
    
    siri = dplyr::if_else(
      !is.na(cbc_index_neutrophil_count) &
        !is.na(cbc_index_monocyte_count) &
        !is.na(cbc_index_lymphocyte_count) &
        cbc_index_lymphocyte_count > 0,
      (
        cbc_index_neutrophil_count *
          cbc_index_monocyte_count
      ) /
        cbc_index_lymphocyte_count,
      NA_real_
    ),
    
    sii = dplyr::if_else(
      !is.na(cbc_index_neutrophil_count) &
        !is.na(cbc_index_platelet_count) &
        !is.na(cbc_index_lymphocyte_count) &
        cbc_index_lymphocyte_count > 0,
      (
        cbc_index_neutrophil_count *
          cbc_index_platelet_count
      ) /
        cbc_index_lymphocyte_count,
      NA_real_
    )
  )


prespecified_biomarker_columns <- c(
  "crp",
  "wbc",
  "neutrophil_count",
  "lymphocyte_count",
  "monocyte_count",
  "nlr",
  "plr",
  "mlr",
  "nmr",
  "siri",
  "sii"
)


if (
  length(prespecified_biomarker_columns) != 11L ||
  anyDuplicated(prespecified_biomarker_columns) > 0L
) {
  stop(
    "The prespecified biomarker validation registry is invalid.",
    call. = FALSE
  )
}


biomarker_completeness_check <- tibble::tibble(
  biomarker =
    prespecified_biomarker_columns,
  
  missing_n = vapply(
    biomarker_validation_data[
      prespecified_biomarker_columns
    ],
    function(x) {
      sum(is.na(x))
    },
    integer(1L)
  ),
  
  nonfinite_n = vapply(
    biomarker_validation_data[
      prespecified_biomarker_columns
    ],
    function(x) {
      sum(
        !is.na(x) &
          !is.finite(x)
      )
    },
    integer(1L)
  ),
  
  negative_n = vapply(
    biomarker_validation_data[
      prespecified_biomarker_columns
    ],
    function(x) {
      sum(
        !is.na(x) &
          is.finite(x) &
          x < 0
      )
    },
    integer(1L)
  )
)


print(
  biomarker_completeness_check,
  n = Inf,
  width = Inf
)


invalid_biomarker_results <-
  biomarker_completeness_check |>
  dplyr::filter(
    missing_n > 0L |
      nonfinite_n > 0L |
      negative_n > 0L
  )


if (nrow(invalid_biomarker_results) > 0L) {
  stop(
    paste(
      "The analytical cohort contains missing, non-finite or negative",
      "values in one or more prespecified biomarkers."
    ),
    call. = FALSE
  )
}


cat(
  "\nPrespecified biomarker validation: PASS\n",
  "Biomarkers checked: ",
  length(prespecified_biomarker_columns),
  "\nAnalytical rows checked: ",
  nrow(biomarker_validation_data),
  "\n",
  sep = ""
)

# ------------------------------------------------------------------------------
# Validate the final phenotype groups
# ------------------------------------------------------------------------------

allowed_final_phenotype_groups <- c(
  "control",
  "infection_without_sepsis",
  "sepsis"
)


final_phenotype_group_check <-
  analytic_cohort_primary_unblinded |>
  dplyr::count(
    primary_phenotype_group,
    name = "episode_n",
    .drop = FALSE
  ) |>
  dplyr::arrange(
    primary_phenotype_group
  )


print(
  final_phenotype_group_check,
  n = Inf,
  width = Inf
)


invalid_final_phenotype_groups <- setdiff(
  unique(
    analytic_cohort_primary_unblinded$
      primary_phenotype_group
  ),
  allowed_final_phenotype_groups
)


invalid_final_phenotype_groups <-
  invalid_final_phenotype_groups[
    !is.na(invalid_final_phenotype_groups)
  ]


missing_final_phenotype_n <- sum(
  is.na(
    analytic_cohort_primary_unblinded$
      primary_phenotype_group
  )
)


if (
  length(invalid_final_phenotype_groups) > 0L ||
  missing_final_phenotype_n > 0L
) {
  stop(
    paste0(
      "The final analytical cohort contains missing or unclassified ",
      "phenotype assignments."
    ),
    call. = FALSE
  )
}


cat(
  "\nFinal phenotype-group validation: PASS\n",
  "Allowed groups: ",
  paste(
    allowed_final_phenotype_groups,
    collapse = ", "
  ),
  "\n",
  sep = ""
)

# ==============================================================================
# 47.4 MICROBIOLOGY ENRICHMENT
#
# Scope:
#   - Every microbiology specimen belonging to a qualifying infection pair.
#   - Specimen/study type.
#   - Organism name when org_name is nonempty.
#
# Excluded:
#   - ab_name
#   - interpretation
#   - dilution information
#   - susceptibility results
# ==============================================================================


# ------------------------------------------------------------------------------
# 47.4.1 Verify microbiology tables and columns
# ------------------------------------------------------------------------------

required_microbiology_tables <- c(
  "infection_microbiology_source_raw",
  "infection_microbiology_specimens_all",
  "infection_pairs_assigned"
)


missing_microbiology_tables <- required_microbiology_tables[
  !vapply(
    required_microbiology_tables,
    function(table_name) {
      DBI::dbExistsTable(
        con,
        table_name
      )
    },
    logical(1)
  )
]


if (length(missing_microbiology_tables) > 0L) {
  stop(
    paste0(
      "Missing microbiology table/view(s): ",
      paste(
        missing_microbiology_tables,
        collapse = ", "
      ),
      ". Run Sections 38-40 first."
    ),
    call. = FALSE
  )
}


required_microbiology_raw_columns <- c(
  "subject_id",
  "micro_specimen_id",
  "spec_type_desc",
  "test_name",
  "org_name"
)


missing_microbiology_raw_columns <- setdiff(
  required_microbiology_raw_columns,
  DBI::dbListFields(
    con,
    "infection_microbiology_source_raw"
  )
)


if (length(missing_microbiology_raw_columns) > 0L) {
  stop(
    paste0(
      "The raw microbiology source is missing column(s): ",
      paste(
        missing_microbiology_raw_columns,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


required_microbiology_specimen_columns <- c(
  "microbiology_specimen_key",
  "subject_id",
  "micro_specimen_id",
  "specimen_type_normalized"
)


missing_microbiology_specimen_columns <- setdiff(
  required_microbiology_specimen_columns,
  DBI::dbListFields(
    con,
    "infection_microbiology_specimens_all"
  )
)


if (length(missing_microbiology_specimen_columns) > 0L) {
  stop(
    paste0(
      "The collapsed microbiology table is missing column(s): ",
      paste(
        missing_microbiology_specimen_columns,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


required_infection_pair_columns <- c(
  "episode_id",
  "microbiology_specimen_key"
)


missing_infection_pair_columns <- setdiff(
  required_infection_pair_columns,
  DBI::dbListFields(
    con,
    "infection_pairs_assigned"
  )
)


if (length(missing_infection_pair_columns) > 0L) {
  stop(
    paste0(
      "The assigned infection-pair table is missing column(s): ",
      paste(
        missing_infection_pair_columns,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 47.4.2 Register analytical episodes
# ------------------------------------------------------------------------------

analytic_episode_ids_for_microbiology <- data.frame(
  episode_id = as.character(
    analytic_cohort_primary_unblinded$episode_id
  ),
  stringsAsFactors = FALSE
)


if (
  DBI::dbExistsTable(
    con,
    "analytic_episode_ids_for_microbiology"
  )
) {
  DBI::dbRemoveTable(
    con,
    "analytic_episode_ids_for_microbiology"
  )
}


DBI::dbWriteTable(
  conn = con,
  name = "analytic_episode_ids_for_microbiology",
  value = analytic_episode_ids_for_microbiology,
  temporary = TRUE,
  overwrite = TRUE
)


# ------------------------------------------------------------------------------
# 47.4.3 Normalize raw microbiology study and organism information
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    microbiology_raw_review_detail AS

  SELECT DISTINCT

    CASE
      WHEN upper(
        trim(
          coalesce(
            CAST(subject_id AS VARCHAR),
            ''
          )
        )
      ) IN ('', 'NA', 'NAN', 'NULL')
      THEN NULL
      ELSE trim(
        CAST(subject_id AS VARCHAR)
      )
    END AS subject_id,

    CASE
      WHEN upper(
        trim(
          coalesce(
            CAST(micro_specimen_id AS VARCHAR),
            ''
          )
        )
      ) IN ('', 'NA', 'NAN', 'NULL')
      THEN NULL
      ELSE trim(
        CAST(micro_specimen_id AS VARCHAR)
      )
    END AS micro_specimen_id,

    CASE
      WHEN upper(
        trim(
          coalesce(
            CAST(spec_type_desc AS VARCHAR),
            ''
          )
        )
      ) IN ('', 'NA', 'NAN', 'NULL')
      THEN NULL
      ELSE upper(
        trim(
          CAST(spec_type_desc AS VARCHAR)
        )
      )
    END AS specimen_type,

    CASE
      WHEN upper(
        trim(
          coalesce(
            CAST(test_name AS VARCHAR),
            ''
          )
        )
      ) IN ('', 'NA', 'NAN', 'NULL')
      THEN NULL
      ELSE trim(
        CAST(test_name AS VARCHAR)
      )
    END AS test_name,

    CASE
      WHEN upper(
        trim(
          coalesce(
            CAST(org_name AS VARCHAR),
            ''
          )
        )
      ) IN ('', 'NA', 'NAN', 'NULL')
      THEN NULL
      ELSE upper(
        trim(
          CAST(org_name AS VARCHAR)
        )
      )
    END AS organism_name

  FROM infection_microbiology_source_raw;
  "
)


# ------------------------------------------------------------------------------
# 47.4.4 Identify qualifying microbiology specimens
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    analytic_episode_qualifying_microbiology_specimens AS

  SELECT DISTINCT

    CAST(
      pair.episode_id AS VARCHAR
    ) AS episode_id,

    pair.microbiology_specimen_key,

    CASE
      WHEN upper(
        trim(
          coalesce(
            CAST(specimen.subject_id AS VARCHAR),
            ''
          )
        )
      ) IN ('', 'NA', 'NAN', 'NULL')
      THEN NULL
      ELSE trim(
        CAST(specimen.subject_id AS VARCHAR)
      )
    END AS subject_id,

    CASE
      WHEN upper(
        trim(
          coalesce(
            CAST(specimen.micro_specimen_id AS VARCHAR),
            ''
          )
        )
      ) IN ('', 'NA', 'NAN', 'NULL')
      THEN NULL
      ELSE trim(
        CAST(specimen.micro_specimen_id AS VARCHAR)
      )
    END AS micro_specimen_id,

    coalesce(
      nullif(
        trim(
          CAST(
            specimen.specimen_type_normalized
            AS VARCHAR
          )
        ),
        ''
      ),
      '[SPECIMEN TYPE NOT RECORDED]'
    ) AS collapsed_specimen_type

  FROM infection_pairs_assigned AS pair

  INNER JOIN analytic_episode_ids_for_microbiology
    AS analytical
    ON CAST(
         pair.episode_id AS VARCHAR
       ) = analytical.episode_id

  LEFT JOIN infection_microbiology_specimens_all
    AS specimen
    ON pair.microbiology_specimen_key =
       specimen.microbiology_specimen_key

  WHERE pair.microbiology_specimen_key IS NOT NULL;
  "
)


# ------------------------------------------------------------------------------
# 47.4.5 Create study-level microbiology detail
#
# Rows repeated because of susceptibility records are removed with DISTINCT.
# No susceptibility field is selected.
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    analytic_episode_microbiology_detail AS

  SELECT DISTINCT

    qualifying.episode_id,
    qualifying.microbiology_specimen_key,

    coalesce(
      raw.specimen_type,
      qualifying.collapsed_specimen_type,
      '[SPECIMEN TYPE NOT RECORDED]'
    ) AS microbiology_specimen_type,

    coalesce(
      raw.test_name,
      '[TEST NOT RECORDED]'
    ) AS microbiology_test_name,

    raw.organism_name,

    concat(
      coalesce(
        raw.specimen_type,
        qualifying.collapsed_specimen_type,
        '[SPECIMEN TYPE NOT RECORDED]'
      ),
      ' :: ',
      coalesce(
        raw.test_name,
        '[TEST NOT RECORDED]'
      )
    ) AS microbiology_study_label,

    CASE
      WHEN raw.organism_name IS NOT NULL
      THEN concat(
        coalesce(
          raw.specimen_type,
          qualifying.collapsed_specimen_type,
          '[SPECIMEN TYPE NOT RECORDED]'
        ),
        ' :: ',
        coalesce(
          raw.test_name,
          '[TEST NOT RECORDED]'
        ),
        ' => ',
        raw.organism_name
      )
      ELSE NULL
    END AS microbiology_positive_result_label

  FROM analytic_episode_qualifying_microbiology_specimens
    AS qualifying

  LEFT JOIN microbiology_raw_review_detail AS raw
    ON qualifying.subject_id =
       raw.subject_id
   AND qualifying.micro_specimen_id IS NOT NULL
   AND qualifying.micro_specimen_id =
       raw.micro_specimen_id;
  "
)



# ==============================================================================
# 47.4.5A SUPPLEMENTARY BLOOD-CULTURE EPISODE FLAGS
# ==============================================================================

supplementary_blood_culture_episode_sidecar <-
  DBI::dbGetQuery(
    con,
    "
    WITH analytical_episodes AS (

      SELECT DISTINCT
        CAST(episode_id AS VARCHAR) AS episode_id

      FROM analytic_episode_ids_for_microbiology
    ),

    blood_culture_obtained AS (

      SELECT DISTINCT
        CAST(episode_id AS VARCHAR) AS episode_id

      FROM analytic_episode_microbiology_detail

      WHERE upper(trim(microbiology_specimen_type)) =
            'BLOOD CULTURE'
    ),

    valid_positive_blood_culture AS (

      SELECT DISTINCT
        CAST(episode_id AS VARCHAR) AS episode_id

      FROM analytic_episode_microbiology_detail

      WHERE upper(trim(microbiology_specimen_type)) =
            'BLOOD CULTURE'

        AND organism_name IS NOT NULL

        AND trim(organism_name) <> ''

        AND upper(trim(organism_name)) <>
            'CANCELLED'
    )

    SELECT

      a.episode_id,

      CAST(
        b.episode_id IS NOT NULL
        AS INTEGER
      ) AS blood_culture_obtained_flag,

      CAST(
        p.episode_id IS NOT NULL
        AS INTEGER
      ) AS blood_culture_valid_positive_flag

    FROM analytical_episodes AS a

    LEFT JOIN blood_culture_obtained AS b
      USING (episode_id)

    LEFT JOIN valid_positive_blood_culture AS p
      USING (episode_id)

    ORDER BY a.episode_id;
    "
  ) %>%
  tibble::as_tibble()


# ------------------------------------------------------------------------------
# Integrity
# ------------------------------------------------------------------------------

if (
  anyDuplicated(
    supplementary_blood_culture_episode_sidecar$episode_id
  ) > 0L
) {
  stop(
    "Duplicated episode_id in blood-culture sidecar.",
    call. = FALSE
  )
}


if (
  any(
    supplementary_blood_culture_episode_sidecar$
    blood_culture_valid_positive_flag >
    supplementary_blood_culture_episode_sidecar$
    blood_culture_obtained_flag
  )
) {
  stop(
    paste0(
      "Blood-culture integrity failure: ",
      "positive finding without blood culture obtained."
    ),
    call. = FALSE
  )
}


cat(
  "\n=== BLOOD-CULTURE EPISODE SUMMARY ===\n"
)

print(
  supplementary_blood_culture_episode_sidecar %>%
    dplyr::summarise(
      
      episodes =
        dplyr::n(),
      
      blood_culture_obtained_n =
        sum(
          blood_culture_obtained_flag
        ),
      
      blood_culture_valid_positive_n =
        sum(
          blood_culture_valid_positive_flag
        ),
      
      positivity_percent_among_cultured =
        100 *
        blood_culture_valid_positive_n /
        blood_culture_obtained_n
    ),
  width = Inf
)
# ------------------------------------------------------------------------------
# 47.4.6 Aggregate microbiology to one row per analytical episode
# ------------------------------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE
    analytic_episode_microbiology_summary AS

  WITH specimen_summary AS (

    SELECT
      episode_id,

      COUNT(
        DISTINCT microbiology_specimen_key
      )::BIGINT
        AS infection_microbiology_qualifying_specimen_n

    FROM analytic_episode_qualifying_microbiology_specimens

    GROUP BY episode_id
  ),

  distinct_studies AS (

    SELECT DISTINCT
      episode_id,
      microbiology_study_label

    FROM analytic_episode_microbiology_detail
  ),

  study_summary AS (

    SELECT
      episode_id,

      COUNT(*)::BIGINT
        AS infection_microbiology_distinct_study_n,

      string_agg(
        microbiology_study_label,
        ' || '
        ORDER BY microbiology_study_label
      ) AS infection_microbiology_studies_compact

    FROM distinct_studies

    GROUP BY episode_id
  ),

  distinct_positive_results AS (

    SELECT DISTINCT
      episode_id,
      microbiology_specimen_key,
      organism_name,
      microbiology_positive_result_label

    FROM analytic_episode_microbiology_detail

    WHERE organism_name IS NOT NULL
  ),

  positive_summary AS (

    SELECT
      episode_id,

      COUNT(
        DISTINCT microbiology_specimen_key
      )::BIGINT
        AS infection_microbiology_positive_specimen_n,

      COUNT(
        DISTINCT organism_name
      )::BIGINT
        AS infection_microbiology_positive_organism_n,

      string_agg(
        microbiology_positive_result_label,
        ' || '
        ORDER BY microbiology_positive_result_label
      ) AS infection_microbiology_positive_results_compact

    FROM distinct_positive_results

    GROUP BY episode_id
  )

  SELECT

    specimen_summary.episode_id,

    specimen_summary.
      infection_microbiology_qualifying_specimen_n,

    study_summary.
      infection_microbiology_distinct_study_n,

    study_summary.
      infection_microbiology_studies_compact,

    CAST(
      positive_summary.episode_id IS NOT NULL
      AS INTEGER
    ) AS infection_microbiology_any_positive_flag,

    coalesce(
      positive_summary.
        infection_microbiology_positive_specimen_n,
      0
    )::BIGINT
      AS infection_microbiology_positive_specimen_n,

    coalesce(
      positive_summary.
        infection_microbiology_positive_organism_n,
      0
    )::BIGINT
      AS infection_microbiology_positive_organism_n,

    positive_summary.
      infection_microbiology_positive_results_compact

  FROM specimen_summary

  INNER JOIN study_summary
    USING (
      episode_id
    )

  LEFT JOIN positive_summary
    USING (
      episode_id
    );
  "
)


# ------------------------------------------------------------------------------
# 47.4.7 Join microbiology to the analytical cohort
# ------------------------------------------------------------------------------

analytic_episode_microbiology_summary <-
  DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM analytic_episode_microbiology_summary
    ORDER BY episode_id;
    "
  ) %>%
  tibble::as_tibble()


analytic_episode_microbiology_summary$episode_id <-
  as.character(
    analytic_episode_microbiology_summary$episode_id
  )


microbiology_enrichment_columns <- c(
  "infection_microbiology_qualifying_specimen_n",
  "infection_microbiology_distinct_study_n",
  "infection_microbiology_studies_compact",
  "infection_microbiology_any_positive_flag",
  "infection_microbiology_positive_specimen_n",
  "infection_microbiology_positive_organism_n",
  "infection_microbiology_positive_results_compact",
  "blood_culture_obtained_flag",
  "blood_culture_valid_positive_flag"
)


analytic_cohort_primary_unblinded <-
  analytic_cohort_primary_unblinded %>%
  dplyr::select(
    -dplyr::any_of(
      microbiology_enrichment_columns
    )
  ) %>%
  dplyr::mutate(
    episode_id = as.character(
      episode_id
    )
  ) %>%
  dplyr::left_join(
    analytic_episode_microbiology_summary,
    by = "episode_id"
  ) %>%
  dplyr::left_join(
    supplementary_blood_culture_episode_sidecar,
    by = "episode_id"
  )


microbiology_count_columns <- c(
  "infection_microbiology_qualifying_specimen_n",
  "infection_microbiology_distinct_study_n",
  "infection_microbiology_any_positive_flag",
  "infection_microbiology_positive_specimen_n",
  "infection_microbiology_positive_organism_n",
  "blood_culture_obtained_flag",
  "blood_culture_valid_positive_flag"
)


for (column_name in microbiology_count_columns) {
  
  analytic_cohort_primary_unblinded[[column_name]][is.na(analytic_cohort_primary_unblinded[[column_name]])] <- 0L
  
  
  analytic_cohort_primary_unblinded[[column_name]] <- as.integer(
    analytic_cohort_primary_unblinded[[column_name]]
  )
}


if (
  "infection_index_microbiology_specimen_type" %in%
  names(
    analytic_cohort_primary_unblinded
  )
) {
  
  analytic_cohort_primary_unblinded <-
    analytic_cohort_primary_unblinded %>%
    dplyr::relocate(
      dplyr::all_of(
        microbiology_enrichment_columns
      ),
      .after =
        infection_index_microbiology_specimen_type
    )
}


# ------------------------------------------------------------------------------
# 47.4.8 Validate microbiology enrichment
# ------------------------------------------------------------------------------

analytic_microbiology_enrichment_check <- tibble::tibble(
  
  source_row_n =
    unblinded_pre_microbiology_row_n,
  
  output_row_n =
    nrow(
      analytic_cohort_primary_unblinded
    ),
  
  source_column_n =
    unblinded_pre_microbiology_column_n,
  
  output_column_n =
    ncol(
      analytic_cohort_primary_unblinded
    ),
  
  added_column_n =
    ncol(
      analytic_cohort_primary_unblinded
    ) -
    unblinded_pre_microbiology_column_n,
  
  distinct_episode_n =
    dplyr::n_distinct(
      analytic_cohort_primary_unblinded$episode_id
    ),
  
  distinct_patient_n =
    dplyr::n_distinct(
      analytic_cohort_primary_unblinded$subject_id
    ),
  
  episode_with_qualifying_microbiology_n =
    sum(
      analytic_cohort_primary_unblinded$
        infection_microbiology_qualifying_specimen_n >
        0L,
      na.rm = TRUE
    ),
  
  episode_with_positive_microbiology_n =
    sum(
      analytic_cohort_primary_unblinded$
        infection_microbiology_any_positive_flag ==
        1L,
      na.rm = TRUE
    ),
  
  study_text_missing_with_specimen_n =
    sum(
      analytic_cohort_primary_unblinded$
        infection_microbiology_qualifying_specimen_n >
        0L &
        (
          is.na(
            analytic_cohort_primary_unblinded$
              infection_microbiology_studies_compact
          ) |
            trimws(
              dplyr::coalesce(
                analytic_cohort_primary_unblinded$
                  infection_microbiology_studies_compact,
                ""
              )
            ) == ""
        ),
      na.rm = TRUE
    ),
  
  positive_flag_count_mismatch_n =
    sum(
      (
        analytic_cohort_primary_unblinded$
          infection_microbiology_any_positive_flag ==
          1L
      ) !=
        (
          analytic_cohort_primary_unblinded$
            infection_microbiology_positive_organism_n >
            0L
        ),
      na.rm = TRUE
    ),
  
  positive_text_missing_n =
    sum(
      analytic_cohort_primary_unblinded$
        infection_microbiology_any_positive_flag ==
        1L &
        (
          is.na(
            analytic_cohort_primary_unblinded$
              infection_microbiology_positive_results_compact
          ) |
            trimws(
              dplyr::coalesce(
                analytic_cohort_primary_unblinded$
                  infection_microbiology_positive_results_compact,
                ""
              )
            ) == ""
        ),
      na.rm = TRUE
    )
)


if (
  analytic_microbiology_enrichment_check$output_row_n !=
  analytic_microbiology_enrichment_check$source_row_n ||
  analytic_microbiology_enrichment_check$output_row_n !=
  analytic_microbiology_enrichment_check$distinct_episode_n ||
  analytic_microbiology_enrichment_check$output_row_n !=
  analytic_microbiology_enrichment_check$distinct_patient_n ||
  analytic_microbiology_enrichment_check$added_column_n != 9L ||
  analytic_microbiology_enrichment_check$
  study_text_missing_with_specimen_n != 0L ||
  analytic_microbiology_enrichment_check$
  positive_flag_count_mismatch_n != 0L ||
  analytic_microbiology_enrichment_check$
  positive_text_missing_n != 0L
) {
  stop(
    "The microbiology enrichment failed validation.",
    call. = FALSE
  )
}


print(
  analytic_microbiology_enrichment_check,
  width = Inf
)


if (
  "primary_phenotype_group" %in%
  names(
    analytic_cohort_primary_unblinded
  )
) {
  
  analytic_microbiology_summary_by_group <-
    analytic_cohort_primary_unblinded %>%
    dplyr::group_by(
      primary_phenotype_group
    ) %>%
    dplyr::summarise(
      
      episode_n =
        dplyr::n(),
      
      episode_with_qualifying_microbiology_n =
        sum(
          infection_microbiology_qualifying_specimen_n >
            0L,
          na.rm = TRUE
        ),
      
      episode_with_positive_microbiology_n =
        sum(
          infection_microbiology_any_positive_flag ==
            1L,
          na.rm = TRUE
        ),
      
      positive_microbiology_percent =
        round(
          100 *
            mean(
              infection_microbiology_any_positive_flag ==
                1L,
              na.rm = TRUE
            ),
          1
        ),
      
      .groups = "drop"
    )
  
} else {
  
  analytic_microbiology_summary_by_group <-
    tibble::tibble(
      
      primary_phenotype_group =
        "total",
      
      episode_n =
        nrow(
          analytic_cohort_primary_unblinded
        ),
      
      episode_with_qualifying_microbiology_n =
        sum(
          analytic_cohort_primary_unblinded$
            infection_microbiology_qualifying_specimen_n >
            0L,
          na.rm = TRUE
        ),
      
      episode_with_positive_microbiology_n =
        sum(
          analytic_cohort_primary_unblinded$
            infection_microbiology_any_positive_flag ==
            1L,
          na.rm = TRUE
        ),
      
      positive_microbiology_percent =
        round(
          100 *
            mean(
              analytic_cohort_primary_unblinded$
                infection_microbiology_any_positive_flag ==
                1L,
              na.rm = TRUE
            ),
          1
        )
    )
}


print(
  analytic_microbiology_summary_by_group,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 47.5 PREPARE LOSSLESS CANONICAL ADJUDICATION OBJECT
# ==============================================================================

clinical_adjudication_worklist_canonical <-
  clinical_adjudication_worklist


canonical_note_text <- as.character(
  clinical_adjudication_worklist_canonical$
    discharge_note_text_original
)


canonical_empty_note_n <- sum(
  is.na(canonical_note_text) |
    !nzchar(
      trimws(
        dplyr::coalesce(
          canonical_note_text,
          ""
        )
      )
    )
)


if (canonical_empty_note_n != 0L) {
  stop(
    paste0(
      canonical_empty_note_n,
      " empty note(s) were found in the canonical adjudication worklist."
    ),
    call. = FALSE
  )
}


# ==============================================================================
# 47.6 PREPARE EXCEL-SAFE ADJUDICATION WORKLIST
#
# The Excel copy:
#   - contains one physical spreadsheet row per patient;
#   - does not contain CR or LF characters;
#   - uses the marker " ⏎ " to indicate an original line break;
#   - divides notes into <=30,000-character fields.
# ==============================================================================

EXCEL_NOTE_CHUNK_SIZE <- 30000L


clinical_adjudication_worklist_excel_base <-
  prepare_final_export(
    clinical_adjudication_worklist_canonical
  )


original_note_text <- as.character(
  clinical_adjudication_worklist_excel_base$
    discharge_note_text_original
)


excel_note_text <- sanitize_text_for_excel(
  original_note_text
)


original_note_character_n <- nchar(
  original_note_text,
  type = "chars",
  allowNA = TRUE,
  keepNA = TRUE
)


excel_note_character_n <- nchar(
  excel_note_text,
  type = "chars",
  allowNA = TRUE,
  keepNA = TRUE
)


note_present_flag <- (
  !is.na(excel_note_text) &
    nzchar(
      trimws(
        excel_note_text
      )
    )
)


note_chunk_n <- ifelse(
  note_present_flag,
  pmax(
    1L,
    as.integer(
      ceiling(
        excel_note_character_n /
          EXCEL_NOTE_CHUNK_SIZE
      )
    )
  ),
  0L
)


maximum_note_chunk_n <- max(
  c(
    1L,
    note_chunk_n
  ),
  na.rm = TRUE
)


clinical_adjudication_worklist_excel_base$
  discharge_note_text_original <- NULL


excel_character_columns <- names(
  clinical_adjudication_worklist_excel_base
)[
  vapply(
    clinical_adjudication_worklist_excel_base,
    is.character,
    logical(1)
  )
]


for (column_name in excel_character_columns) {
  
  clinical_adjudication_worklist_excel_base[[column_name]] <- sanitize_text_for_excel(
    clinical_adjudication_worklist_excel_base[[column_name]]
  )
}


note_chunk_matrix <- matrix(
  NA_character_,
  nrow = length(
    excel_note_text
  ),
  ncol = maximum_note_chunk_n
)


for (chunk_index in seq_len(maximum_note_chunk_n)) {
  
  chunk_start <- (
    chunk_index - 1L
  ) *
    EXCEL_NOTE_CHUNK_SIZE +
    1L
  
  
  chunk_end <- chunk_index *
    EXCEL_NOTE_CHUNK_SIZE
  
  
  row_has_chunk_flag <- (
    note_present_flag &
      excel_note_character_n >= chunk_start
  )
  
  
  note_chunk_matrix[
    row_has_chunk_flag,
    chunk_index
  ] <- substr(
    excel_note_text[
      row_has_chunk_flag
    ],
    start = chunk_start,
    stop = chunk_end
  )
}


note_part_columns <- sprintf(
  "discharge_note_text_original_part_%03d",
  seq_len(
    maximum_note_chunk_n
  )
)


colnames(
  note_chunk_matrix
) <- note_part_columns


note_chunk_table <- tibble::as_tibble(
  note_chunk_matrix,
  .name_repair = "minimal"
)


clinical_adjudication_worklist_excel <-
  clinical_adjudication_worklist_excel_base %>%
  dplyr::mutate(
    
    discharge_note_text_original_character_n =
      original_note_character_n,
    
    discharge_note_text_excel_character_n =
      excel_note_character_n,
    
    discharge_note_text_original_chunk_n =
      note_chunk_n
    
  ) %>%
  dplyr::bind_cols(
    note_chunk_table
  )


# ------------------------------------------------------------------------------
# 47.6.1 Validate note reconstruction
# ------------------------------------------------------------------------------

reconstructed_excel_note_text <- do.call(
  paste0,
  lapply(
    clinical_adjudication_worklist_excel[
      note_part_columns
    ],
    function(x) {
      dplyr::coalesce(
        as.character(x),
        ""
      )
    }
  )
)


expected_excel_note_text <- dplyr::coalesce(
  excel_note_text,
  ""
)


note_reconstruction_failure_n <- sum(
  reconstructed_excel_note_text !=
    expected_excel_note_text,
  na.rm = TRUE
)


excel_character_columns_final <- names(
  clinical_adjudication_worklist_excel
)[
  vapply(
    clinical_adjudication_worklist_excel,
    is.character,
    logical(1)
  )
]


remaining_linebreak_cell_n <- sum(
  vapply(
    clinical_adjudication_worklist_excel[
      excel_character_columns_final
    ],
    function(x) {
      sum(
        grepl(
          pattern = "\r|\n",
          x = x,
          perl = TRUE
        ),
        na.rm = TRUE
      )
    },
    numeric(1)
  )
)


maximum_exported_note_part_character_n <- max(
  unlist(
    lapply(
      clinical_adjudication_worklist_excel[
        note_part_columns
      ],
      function(x) {
        nchar(
          x,
          type = "chars",
          allowNA = TRUE,
          keepNA = TRUE
        )
      }
    )
  ),
  na.rm = TRUE
)


expected_part_002_nonempty_n <- if (
  maximum_note_chunk_n >= 2L
) {
  sum(
    note_chunk_n >= 2L,
    na.rm = TRUE
  )
} else {
  0L
}


actual_part_002_nonempty_n <- if (
  "discharge_note_text_original_part_002" %in%
  names(
    clinical_adjudication_worklist_excel
  )
) {
  sum(
    !is.na(
      clinical_adjudication_worklist_excel$
        discharge_note_text_original_part_002
    ) &
      nzchar(
        clinical_adjudication_worklist_excel$
          discharge_note_text_original_part_002
      )
  )
} else {
  0L
}


clinical_adjudication_note_check <- tibble::tibble(
  
  row_n =
    nrow(
      clinical_adjudication_worklist_excel
    ),
  
  note_present_n =
    sum(
      note_present_flag
    ),
  
  maximum_note_chunk_n =
    maximum_note_chunk_n,
  
  note_part_column_n =
    length(
      note_part_columns
    ),
  
  note_reconstruction_failure_n =
    note_reconstruction_failure_n,
  
  remaining_linebreak_cell_n =
    remaining_linebreak_cell_n,
  
  maximum_exported_note_part_character_n =
    maximum_exported_note_part_character_n,
  
  expected_part_002_nonempty_n =
    expected_part_002_nonempty_n,
  
  actual_part_002_nonempty_n =
    actual_part_002_nonempty_n
)


if (
  clinical_adjudication_note_check$note_present_n !=
  nrow(clinical_adjudication_worklist_excel) ||
  clinical_adjudication_note_check$
  note_reconstruction_failure_n != 0L ||
  clinical_adjudication_note_check$
  remaining_linebreak_cell_n != 0L ||
  clinical_adjudication_note_check$
  maximum_exported_note_part_character_n >
  EXCEL_NOTE_CHUNK_SIZE ||
  clinical_adjudication_note_check$
  expected_part_002_nonempty_n !=
  clinical_adjudication_note_check$
  actual_part_002_nonempty_n
) {
  stop(
    "The Excel-safe adjudication notes failed validation.",
    call. = FALSE
  )
}


print(
  clinical_adjudication_note_check,
  width = Inf
)


clinical_adjudication_note_chunk_summary <-
  tibble::tibble(
    discharge_note_text_original_chunk_n =
      note_chunk_n
  ) %>%
  dplyr::count(
    discharge_note_text_original_chunk_n,
    name = "patient_n"
  ) %>%
  dplyr::mutate(
    percent = round(
      100 *
        patient_n /
        sum(patient_n),
      1
    )
  ) %>%
  dplyr::arrange(
    discharge_note_text_original_chunk_n
  )


print(
  clinical_adjudication_note_chunk_summary,
  n = Inf,
  width = Inf
)



# ==============================================================================
# 48. MINIMAL FINAL EXPORTS
# ==============================================================================
# Only products needed to reconstruct the analysis or repeat adjudication are
# written. Cohort-flow and quality-control objects remain printed in the log.

analytic_export <- analytic_cohort_primary_unblinded %>%
  dplyr::select(
    -dplyr::matches("^(discharge_note_|note_)")
  ) %>%
  prepare_final_export()


excel_flat_export <- prepare_final_export(
  clinical_adjudication_worklist_excel
)


if (
  anyDuplicated(analytic_export$episode_id) > 0L ||
  anyDuplicated(analytic_export$subject_id) > 0L
) {
  stop(
    "The final analytical export is not one episode per patient.",
    call. = FALSE
  )
}


if (
  nrow(clinical_adjudication_worklist_canonical) !=
    nrow(excel_flat_export)
) {
  stop(
    "The canonical and Excel-safe adjudication worklists do not reconcile.",
    call. = FALSE
  )
}


final_files <- c(
  analytical_csv = file.path(
    OUTPUT_DIR,
    "analytic_cohort_primary_unblinded.csv"
  ),
  canonical_rds = file.path(
    OUTPUT_DIR,
    "clinical_adjudication_worklist_canonical.rds"
  ),
  excel_workbook = file.path(
    OUTPUT_DIR,
    "clinical_adjudication_worklist_excel.xlsx"
  ),
  excel_flat_csv = file.path(
    OUTPUT_DIR,
    "clinical_adjudication_worklist_excel_flat.csv"
  )
)


readr::write_csv(
  analytic_export,
  final_files[["analytical_csv"]],
  na = ""
)


saveRDS(
  clinical_adjudication_worklist_canonical,
  final_files[["canonical_rds"]]
)


openxlsx::write.xlsx(
  as.data.frame(
    clinical_adjudication_worklist_excel,
    stringsAsFactors = FALSE,
    check.names = FALSE
  ),
  final_files[["excel_workbook"]],
  overwrite = TRUE
)


readr::write_csv(
  excel_flat_export,
  final_files[["excel_flat_csv"]],
  na = ""
)


missing_final_files <- final_files[!file.exists(final_files)]

if (length(missing_final_files) > 0L) {
  stop(
    paste0(
      "The following final product(s) were not written: ",
      paste(basename(missing_final_files), collapse = ", "),
      "."
    ),
    call. = FALSE
  )
}


cat("\nFINAL PRODUCTS\n")
print(
  tibble::tibble(
    file = basename(final_files),
    rows = c(
      nrow(analytic_export),
      nrow(clinical_adjudication_worklist_canonical),
      nrow(clinical_adjudication_worklist_excel),
      nrow(excel_flat_export)
    )
  ),
  n = Inf,
  width = Inf
)


if (DBI::dbIsValid(con)) {
  DBI::dbDisconnect(con, shutdown = TRUE)
}


cat(
  "\nData preparation completed successfully.\n",
  "Analytical cohort rows: ", nrow(analytic_export), "\n",
  "Output directory: ", OUTPUT_DIR, "\n",
  sep = ""
)
