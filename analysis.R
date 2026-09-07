# Sepsis biomarker discrimination analysis
#
# Unit of analysis: clinical episode
# Analysis A: sepsis vs controls
# Analysis B: sepsis vs infection without sepsis
# Primary inference: matched empirical AUROC with DeLong intervals
# Eleven biomarkers are evaluated without designating a single primary marker.

# ==============================================================================
# RECONCILED MANUSCRIPT ANALYSIS
#
# Analysis A:
#   Sepsis versus controls
#
# Analysis B:
#   Sepsis versus infection without sepsis
#
# Primary design:
#   Cardinality ATT profile matching
#
# Biomarkers:
#   CRP, WBC, absolute neutrophils, absolute lymphocytes,
#   absolute monocytes, NLR, PLR, MLR, NMR, SIRI and SII
# ==============================================================================


# ==============================================================================
# 0. Configuration and source datasets
# ==============================================================================

required_packages <- c(
  "dplyr", "tidyr", "readr", "stringr", "purrr", "tibble",
  "MatchIt", "highs", "cobalt", "pROC", "ggplot2"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    paste0(
      "Install the following required package(s): ",
      paste(missing_packages, collapse = ", "),
      "."
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages(
  invisible(lapply(required_packages, library, character.only = TRUE))
)


acp <- read_csv(
  "analytic_cohort_primary_unblinded.csv",
  show_col_types = FALSE,
  progress = FALSE
)

caw_revised <- read_csv(
  "clinical_adjudication_worklist_revised.csv",
  show_col_types = FALSE
)

OUTPUT_DIRECTORY <- file.path(
  getwd(),
  "resultados_analisis_manuscrito"
)

SITE_VALIDATION_KEY_FILE <-
  "suspected_infection_site_reviewer_1.csv"

SITE_VALIDATION_ADJUDICATED_FILE <-
  "suspected_infection_site_reviewer_2.csv"

SAVE_FINAL_FIGURES <- FALSE

# 0.2. Standardize identifier types
# ==============================================================================

required_acp_identifiers <- c(
  "episode_id",
  "subject_id",
  "hadm_id",
  "age"
)


required_caw_identifiers <- c(
  "episode_id"
)


missing_acp_identifiers <- setdiff(
  required_acp_identifiers,
  names(acp)
)


missing_caw_identifiers <- setdiff(
  required_caw_identifiers,
  names(caw_revised)
)


if (
  length(
    missing_acp_identifiers
  ) > 0L
) {
  stop(
    paste0(
      "analytic_cohort_primary_unblinded.csv is missing: ",
      paste(
        missing_acp_identifiers,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


if (
  length(
    missing_caw_identifiers
  ) > 0L
) {
  stop(
    paste0(
      "caw_revised2_1.csv is missing: ",
      paste(
        missing_caw_identifiers,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


acp <- acp %>%
  mutate(
    episode_id =
      as.character(
        episode_id
      ),

    subject_id =
      as.character(
        subject_id
      ),

    hadm_id =
      as.character(
        hadm_id
      )
  )


caw_revised <- caw_revised %>%
  mutate(
    episode_id =
      as.character(
        episode_id
      )
  )

# ==============================================================================
# 0.3. Retain episodes represented in the revised-classification dataset
# ==============================================================================

if (
  anyDuplicated(
    acp$
      episode_id
  ) > 0L
) {
  stop(
    "Duplicated episode_id values were found in acp.",
    call. = FALSE
  )
}


if (
  anyDuplicated(
    caw_revised$
      episode_id
  ) > 0L
) {
  stop(
    "Duplicated episode_id values were found in caw_revised.",
    call. = FALSE
  )
}


acp_episode_n_before_revised_join <- nrow(
  acp
)


episodes_without_usable_discharge_note <- acp %>%
  dplyr::anti_join(
    caw_revised %>%
      dplyr::select(episode_id),
    by = "episode_id"
  ) %>%
  dplyr::select(
    dplyr::any_of(c("episode_id", "subject_id", "primary_phenotype_group"))
  )


acp <- acp %>%
  semi_join(
    caw_revised %>%
      select(
        episode_id
      ),

    by = "episode_id"
  )


revised_episode_linkage_summary <- tibble::tibble(
  episodes_before_linkage =
    acp_episode_n_before_revised_join,

  episodes_retained =
    nrow(
      acp
    ),

  episodes_not_represented_in_revised_dataset =
    acp_episode_n_before_revised_join -
      nrow(
        acp
      )
)


print(
  revised_episode_linkage_summary,
  width = Inf
)

cat("\nEPISODES REMOVED BECAUSE NO USABLE DISCHARGE NOTE WAS AVAILABLE\n")
print(
  episodes_without_usable_discharge_note,
  n = Inf,
  width = Inf
)

# ==============================================================================
# 0.4. Integrate revised-classification variables
# ==============================================================================

new_caw_columns <- setdiff(
  names(
    caw_revised
  ),
  names(
    acp
  )
)


if (
  length(
    new_caw_columns
  ) == 0L
) {
  stop(
    paste0(
      "No additional variables were found in caw_revised. ",
      "The revised-classification dataset cannot be integrated."
    ),
    call. = FALSE
  )
}


acp_rows_before_caw_join <- nrow(
  acp
)


analysis_working_base <- acp %>%
  left_join(
    caw_revised %>%
      select(
        episode_id,
        all_of(
          new_caw_columns
        )
      ),

    by = "episode_id"
  )


if (
  nrow(
    analysis_working_base
  ) !=
    acp_rows_before_caw_join
) {
  stop(
    "The revised-classification join changed the number of rows.",
    call. = FALSE
  )
}


if (
  anyDuplicated(
    analysis_working_base$
      episode_id
  ) > 0L
) {
  stop(
    paste0(
      "Duplicated episode_id values were created during ",
      "revised-classification integration."
    ),
    call. = FALSE
  )
}


if (
  ncol(
    analysis_working_base
  ) !=
    ncol(
      acp
    ) +
      length(
        new_caw_columns
      )
) {
  stop(
    "The number of integrated revised-classification variables is inconsistent.",
    call. = FALSE
  )
}


revised_variable_integration_summary <- tibble::tibble(
  episodes =
    nrow(
      analysis_working_base
    ),

  original_acp_columns =
    ncol(
      acp
    ),

  revised_columns_added =
    length(
      new_caw_columns
    ),

  final_columns =
    ncol(
      analysis_working_base
    )
)


print(
  revised_variable_integration_summary,
  width = Inf
)

# ==============================================================================
# 01. GENERAL HELPER FUNCTIONS
# ==============================================================================

require_columns <- function(
  data,
  required_columns,
  object_name
) {

  missing_columns <- setdiff(
    required_columns,
    names(
      data
    )
  )


  if (
    length(
      missing_columns
    ) > 0L
  ) {
    stop(
      paste0(
        object_name,
        " is missing required column(s): ",
        paste(
          missing_columns,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }


  invisible(
    TRUE
  )
}


strict_numeric <- function(
  values,
  variable_name,
  expected_length = NULL
) {

  if (
    is.null(
      values
    )
  ) {
    stop(
      paste0(
        "The variable '",
        variable_name,
        "' was not found in the analytical dataset."
      ),
      call. = FALSE
    )
  }


  input_length <- length(
    values
  )


  if (
    !is.null(
      expected_length
    ) &&
      input_length != expected_length
  ) {
    stop(
      paste0(
        "Variable '",
        variable_name,
        "' has length ",
        input_length,
        ", but ",
        expected_length,
        " rows were expected."
      ),
      call. = FALSE
    )
  }


  if (
    is.numeric(
      values
    )
  ) {
    numeric_values <- as.numeric(
      values
    )

  } else {

    original_character <- trimws(
      as.character(
        values
      )
    )


    original_missing <-
      is.na(
        values
      ) |
        toupper(
          original_character
        ) %in% c(
          "",
          "NA",
          "N/A",
          "NAN",
          "NULL"
        )


    numeric_values <- suppressWarnings(
      as.numeric(
        original_character
      )
    )


    conversion_failure <-
      !original_missing &
        is.na(
          numeric_values
        )


    if (
      any(
        conversion_failure
      )
    ) {
      invalid_examples <- unique(
        original_character[
          conversion_failure
        ]
      )


      stop(
        paste0(
          "Non-numeric values were found in '",
          variable_name,
          "'. Examples: ",
          paste(
            utils::head(
              invalid_examples,
              10L
            ),
            collapse = " | "
          ),
          "."
        ),
        call. = FALSE
      )
    }


    numeric_values[
      original_missing
    ] <- NA_real_
  }


  if (
    length(
      numeric_values
    ) != input_length
  ) {
    stop(
      paste0(
        "Numeric conversion changed the length of '",
        variable_name,
        "'."
      ),
      call. = FALSE
    )
  }


  numeric_values
}


binary_integer <- function(
  values,
  variable_name
) {

  if (
    is.logical(
      values
    )
  ) {
    return(
      ifelse(
        is.na(
          values
        ),
        NA_integer_,
        as.integer(
          values
        )
      )
    )
  }


  value_character <- toupper(
    trimws(
      as.character(
        values
      )
    )
  )


  missing_value <-
    is.na(
      values
    ) |
      value_character %in% c(
        "",
        "NA",
        "NAN",
        "NULL"
      )


  output <- rep(
    NA_integer_,
    length(
      values
    )
  )


  output[
    value_character %in% c(
      "1",
      "TRUE",
      "T"
    )
  ] <- 1L


  output[
    value_character %in% c(
      "0",
      "FALSE",
      "F"
    )
  ] <- 0L


  unexpected_value <-
    !missing_value &
      is.na(
        output
      )


  if (
    any(
      unexpected_value
    )
  ) {
    stop(
      paste0(
        "Unexpected binary values were found in ",
        variable_name,
        ": ",
        paste(
          unique(
            value_character[
              unexpected_value
            ]
          ),
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }


  output
}


safe_ratio <- function(
  numerator,
  denominator
) {

  result <- rep(
    NA_real_,
    length(
      numerator
    )
  )


  valid <-
    !is.na(
      numerator
    ) &
      is.finite(
        numerator
      ) &
      !is.na(
        denominator
      ) &
      is.finite(
        denominator
      ) &
      denominator > 0


  result[
    valid
  ] <-
    numerator[
      valid
    ] /
      denominator[
        valid
      ]


  result
}


safe_product_ratio <- function(
  component_1,
  component_2,
  denominator
) {

  result <- rep(
    NA_real_,
    length(
      component_1
    )
  )


  valid <-
    !is.na(
      component_1
    ) &
      is.finite(
        component_1
      ) &
      !is.na(
        component_2
      ) &
      is.finite(
        component_2
      ) &
      !is.na(
        denominator
      ) &
      is.finite(
        denominator
      ) &
      denominator > 0


  result[
    valid
  ] <-
    component_1[
      valid
    ] *
      component_2[
        valid
      ] /
      denominator[
        valid
      ]


  result
}


values_agree <- function(
  x,
  y,
  tolerance = 1e-8
) {

  both_missing <-
    is.na(
      x
    ) &
      is.na(
        y
      )


  both_present <-
    !is.na(
      x
    ) &
      !is.na(
        y
      )


  difference_allowed <-
    tolerance *
      pmax(
        1,
        abs(
          x
        ),
        abs(
          y
        ),
        na.rm = TRUE
      )


  both_missing |
    (
      both_present &
        abs(
          x - y
        ) <=
          difference_allowed
    )
}


format_ci <- function(
  estimate,
  lower,
  upper,
  digits = 3L
) {

  paste0(
    formatC(
      estimate,
      format = "f",
      digits = digits
    ),
    " [",
    formatC(
      lower,
      format = "f",
      digits = digits
    ),
    ", ",
    formatC(
      upper,
      format = "f",
      digits = digits
    ),
    "]"
  )
}


percentile_interval <- function(
  values,
  confidence_level = 0.95
) {

  valid_values <- values[
    !is.na(
      values
    )
  ]


  if (
    length(
      valid_values
    ) == 0L
  ) {
    return(
      c(
        lower = NA_real_,
        upper = NA_real_
      )
    )
  }


  alpha <-
    1 -
    confidence_level


  as.numeric(
    stats::quantile(
      valid_values,
      probs = c(
        alpha / 2,
        1 - alpha / 2
      ),
      na.rm = TRUE,
      names = FALSE,
      type = 7
    )
  ) |>
    stats::setNames(
      c(
        "lower",
        "upper"
      )
    )
}

# ==============================================================================
# 01.0. APPLY FINAL EPISODE ELIGIBILITY
# ==============================================================================

require_columns(
  data = analysis_working_base,
  required_columns = "excluded_final_flag",
  object_name = "analysis_working_base"
)


analysis_working_base <- analysis_working_base %>%
  dplyr::mutate(
    excluded_final_flag = binary_integer(
      values = excluded_final_flag,
      variable_name = "excluded_final_flag"
    )
  )


cohort_eligibility_summary <- analysis_working_base %>%
  dplyr::summarise(
    total_episodes = dplyr::n(),

    excluded_episodes = sum(
      excluded_final_flag == 1L,
      na.rm = TRUE
    ),

    retained_episodes = sum(
      excluded_final_flag != 1L |
        is.na(
          excluded_final_flag
        )
    ),

    unavailable_exclusion_flag = sum(
      is.na(
        excluded_final_flag
      )
    )
  )


print(
  cohort_eligibility_summary,
  width = Inf
)


clinical_exclusion_cases <- analysis_working_base %>%
  dplyr::filter(excluded_final_flag == 1L) %>%
  dplyr::select(
    dplyr::any_of(
      c(
        "episode_id",
        "subject_id",
        "excluded_cause",
        "primary_phenotype_group"
      )
    )
  )


if ("excluded_cause" %in% names(clinical_exclusion_cases)) {
  clinical_exclusion_cause_combination_summary <- clinical_exclusion_cases %>%
    dplyr::count(excluded_cause, sort = TRUE, name = "episodes")

  clinical_exclusion_category_summary <- clinical_exclusion_cases %>%
    dplyr::filter(!is.na(excluded_cause)) %>%
    tidyr::separate_rows(excluded_cause, sep = ";") %>%
    dplyr::mutate(
      exclusion_category = stringr::str_squish(excluded_cause)
    ) %>%
    dplyr::filter(exclusion_category != "") %>%
    dplyr::distinct(episode_id, exclusion_category) %>%
    dplyr::count(exclusion_category, sort = TRUE, name = "episodes")
} else {
  clinical_exclusion_cause_combination_summary <- tibble::tibble(
    excluded_cause = "cause column unavailable",
    episodes = nrow(clinical_exclusion_cases)
  )

  clinical_exclusion_category_summary <- tibble::tibble(
    exclusion_category = "cause column unavailable",
    episodes = nrow(clinical_exclusion_cases)
  )
}


cat("\nCLINICAL EXCLUSIONS: NON-MUTUALLY-EXCLUSIVE CATEGORIES\n")
print(clinical_exclusion_category_summary, n = Inf, width = Inf)

cat("\nEPISODES REMOVED BY PRESPECIFIED CLINICAL CRITERIA\n")
print(clinical_exclusion_cases, n = Inf, width = Inf)


analysis_working_base <- analysis_working_base %>%
  dplyr::filter(
    excluded_final_flag != 1L |
      is.na(
        excluded_final_flag
      )
  )


if (
  any(
    analysis_working_base$
      excluded_final_flag == 1L,
    na.rm = TRUE
  )
) {
  stop(
    "Episodes with excluded_final_flag == 1 remain in the analytical cohort.",
    call. = FALSE
  )
}


if (
  nrow(
    analysis_working_base
  ) !=
    cohort_eligibility_summary$
      retained_episodes
) {
  stop(
    "The retained cohort size is inconsistent with excluded_final_flag.",
    call. = FALSE
  )
}

# ==============================================================================
# 01.1. CONSTRUCT THE REVISED SOFA COMPONENTS
# ==============================================================================

structured_sofa_columns <- c(
  "sofa_respiratory_structured_0_24h",
  "sofa_coagulation_structured_0_24h",
  "sofa_hepatic_structured_0_24h",
  "sofa_cardiovascular_structured_0_24h",
  "sofa_neurologic_structured_0_24h",
  "sofa_renal_structured_0_24h"
)


manual_sofa_columns <- c(
  "manual_respiratory_sofa_score",
  "manual_coagulation_sofa_score",
  "manual_hepatic_sofa_score",
  "manual_cardiovascular_sofa_score",
  "manual_neurologic_sofa_score",
  "manual_renal_sofa_score"
)


required_sofa_source_columns <- c(
  structured_sofa_columns,
  manual_sofa_columns
)


require_columns(
  data = analysis_working_base,
  required_columns = required_sofa_source_columns,
  object_name = "analysis_working_base"
)


# Convert the source SOFA variables before applying coalesce()
for (
  variable_name in required_sofa_source_columns
) {

  converted_values <- strict_numeric(
    values =
      analysis_working_base[[
        variable_name
      ]],

    variable_name =
      variable_name
  )


  if (
    length(
      converted_values
    ) !=
      nrow(
        analysis_working_base
      )
  ) {
    stop(
      paste0(
        "Numeric conversion produced an invalid length for '",
        variable_name,
        "'."
      ),
      call. = FALSE
    )
  }


  analysis_working_base[[
    variable_name
  ]] <- converted_values
}


# Structured values have priority.
# Manual values are used only when the structured component is unavailable.
analysis_working_base <- analysis_working_base %>%
  dplyr::mutate(
    sofa_respiratory_revised_0_24h =
      dplyr::coalesce(
        sofa_respiratory_structured_0_24h,
        manual_respiratory_sofa_score
      ),

    sofa_coagulation_revised_0_24h =
      dplyr::coalesce(
        sofa_coagulation_structured_0_24h,
        manual_coagulation_sofa_score
      ),

    sofa_hepatic_revised_0_24h =
      dplyr::coalesce(
        sofa_hepatic_structured_0_24h,
        manual_hepatic_sofa_score
      ),

    sofa_cardiovascular_revised_0_24h =
      dplyr::coalesce(
        sofa_cardiovascular_structured_0_24h,
        manual_cardiovascular_sofa_score
      ),

    sofa_neurologic_revised_0_24h =
      dplyr::coalesce(
        sofa_neurologic_structured_0_24h,
        manual_neurologic_sofa_score
      ),

    sofa_renal_revised_0_24h =
      dplyr::coalesce(
        sofa_renal_structured_0_24h,
        manual_renal_sofa_score
      )
  )


revised_sofa_columns <- c(
  "sofa_respiratory_revised_0_24h",
  "sofa_coagulation_revised_0_24h",
  "sofa_hepatic_revised_0_24h",
  "sofa_cardiovascular_revised_0_24h",
  "sofa_neurologic_revised_0_24h",
  "sofa_renal_revised_0_24h"
)


# Validate that every observed component is an integer from 0 to 4
invalid_revised_sofa_count <- sum(
  vapply(
    revised_sofa_columns,

    function(variable_name) {

      values <- analysis_working_base[[
        variable_name
      ]]


      sum(
        !is.na(values) &
          (
            !is.finite(values) |
              values < 0 |
              values > 4 |
              values != floor(values)
          )
      )
    },

    integer(1)
  )
)


if (
  invalid_revised_sofa_count > 0L
) {
  stop(
    paste0(
      invalid_revised_sofa_count,
      " invalid revised SOFA component value(s) were found."
    ),
    call. = FALSE
  )
}


sofa_component_source_summary <- tibble::tibble(
  component = c(
    "Respiratory",
    "Coagulation",
    "Hepatic",
    "Cardiovascular",
    "Neurologic",
    "Renal"
  ),

  structured_available_n = vapply(
    structured_sofa_columns,

    function(variable_name) {
      sum(
        !is.na(
          analysis_working_base[[
            variable_name
          ]]
        )
      )
    },

    integer(1)
  ),

  manual_fallback_n = purrr::map2_int(
    structured_sofa_columns,
    manual_sofa_columns,

    function(
      structured_variable,
      manual_variable
    ) {

      sum(
        is.na(
          analysis_working_base[[
            structured_variable
          ]]
        ) &
          !is.na(
            analysis_working_base[[
              manual_variable
            ]]
          )
      )
    }
  ),

  revised_available_n = vapply(
    revised_sofa_columns,

    function(variable_name) {
      sum(
        !is.na(
          analysis_working_base[[
            variable_name
          ]]
        )
      )
    },

    integer(1)
  ),

  revised_unavailable_n = vapply(
    revised_sofa_columns,

    function(variable_name) {
      sum(
        is.na(
          analysis_working_base[[
            variable_name
          ]]
        )
      )
    },

    integer(1)
  )
)


print(
  sofa_component_source_summary,
  n = Inf,
  width = Inf
)

# ==============================================================================
# 02. FREEZE AND VALIDATE THE INTEGRATED ANALYTICAL COHORT
# ==============================================================================

analysis_final_base <- analysis_working_base


required_input_columns <- c(
  "episode_id",
  "subject_id",
  "primary_phenotype_group",

  "age",
  "gender",
  "race_ed",
  "insurance",

  "chronic_pulmonary_disease",
  "mild_liver_disease",
  "severe_liver_disease",
  "renal_disease",
  "diabetes_without_cc",
  "diabetes_with_cc",

  "infection_site_index_broad",
  "multiple_infection_sites_flag",

  "sofa_respiratory_revised_0_24h",
  "sofa_coagulation_revised_0_24h",
  "sofa_hepatic_revised_0_24h",
  "sofa_cardiovascular_revised_0_24h",
  "sofa_neurologic_revised_0_24h",
  "sofa_renal_revised_0_24h",

  "sepsis_coagulation_dependent_flag",

  "crp_index_value_mg_l",
  "cbc_index_hemoglobin",
  "cbc_index_wbc_for_calculation",
  "cbc_index_neutrophil_count",
  "cbc_index_lymphocyte_count",
  "cbc_index_monocyte_count",
  "cbc_index_platelet_count",

  "nlr",
  "plr",
  "mlr",
  "nmr",

  "bilirubin_first_value_0_24h",
  "creatinine_first_value_0_24h",
  "ed_arrival_temperature_c",
  "ed_arrival_heart_rate",
  "ed_arrival_respiratory_rate",
  "ed_arrival_map_mmhg",
  "ed_arrival_spo2_percent",

  "blood_culture_obtained_flag",
  "blood_culture_valid_positive_flag"
)


require_columns(
  data = analysis_final_base,
  required_columns = required_input_columns,
  object_name = "analysis_final_base"
)

analysis_final_base <- analysis_final_base %>%
  dplyr::mutate(
    episode_id = as.character(
      episode_id
    ),

    subject_id = as.character(
      subject_id
    ),

    primary_phenotype_group =
      stringr::str_to_lower(
        stringr::str_squish(
          as.character(
            primary_phenotype_group
          )
        )
      ),

    infected_phenotype_flag =
      dplyr::case_when(
        primary_phenotype_group == "control" ~
          0L,

        primary_phenotype_group %in% c(
          "sepsis",
          "infection_without_sepsis"
        ) ~
          1L,

        TRUE ~
          NA_integer_
      )
  )

unexpected_primary_phenotypes <- setdiff(
  unique(
    analysis_final_base$
      primary_phenotype_group
  ),
  c(
    "control",
    "infection_without_sepsis",
    "sepsis"
  )
)


unexpected_primary_phenotypes <-
  unexpected_primary_phenotypes[
    !is.na(
      unexpected_primary_phenotypes
    )
  ]


if (
  length(
    unexpected_primary_phenotypes
  ) > 0L
) {
  stop(
    paste0(
      "Unexpected primary phenotype value(s): ",
      paste(
        unexpected_primary_phenotypes,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


if (
  anyNA(
    analysis_final_base$
      infected_phenotype_flag
  )
) {
  stop(
    paste0(
      "infected_phenotype_flag could not be assigned ",
      "for at least one episode."
    ),
    call. = FALSE
  )
}

# ==============================================================================
# 03. NORMALIZE VARIABLES USED IN THE FINAL DESIGN
# ==============================================================================

numeric_columns <- c(
  "age",

  "sofa_respiratory_revised_0_24h",
  "sofa_coagulation_revised_0_24h",
  "sofa_hepatic_revised_0_24h",
  "sofa_cardiovascular_revised_0_24h",
  "sofa_neurologic_revised_0_24h",
  "sofa_renal_revised_0_24h",

  "crp_index_value_mg_l",
  "cbc_index_hemoglobin",
  "cbc_index_wbc_for_calculation",
  "cbc_index_neutrophil_count",
  "cbc_index_lymphocyte_count",
  "cbc_index_monocyte_count",
  "cbc_index_platelet_count",

  "nlr",
  "plr",
  "mlr",
  "nmr",

  "bilirubin_first_value_0_24h",
  "creatinine_first_value_0_24h",
  "ed_arrival_temperature_c",
  "ed_arrival_heart_rate",
  "ed_arrival_respiratory_rate",
  "ed_arrival_map_mmhg",
  "ed_arrival_spo2_percent"
)


missing_numeric_columns <- setdiff(
  numeric_columns,
  names(
    analysis_final_base
  )
)


if (
  length(
    missing_numeric_columns
  ) > 0L
) {
  stop(
    paste0(
      "The following numeric variable(s) are missing from ",
      "analysis_final_base: ",
      paste(
        missing_numeric_columns,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


duplicated_numeric_columns <- numeric_columns[
  duplicated(
    numeric_columns
  )
]


if (
  length(
    duplicated_numeric_columns
  ) > 0L
) {
  stop(
    paste0(
      "Duplicated variable names were found in numeric_columns: ",
      paste(
        unique(
          duplicated_numeric_columns
        ),
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


for (
  variable_name in numeric_columns
) {

  converted_values <- strict_numeric(
    values =
      analysis_final_base[[
        variable_name
      ]],

    variable_name =
      variable_name,

    expected_length =
      nrow(
        analysis_final_base
      )
  )


  analysis_final_base[[
    variable_name
  ]] <- converted_values
}

binary_columns <- c(
  "chronic_pulmonary_disease",
  "mild_liver_disease",
  "severe_liver_disease",
  "renal_disease",
  "diabetes_without_cc",
  "diabetes_with_cc",
  "multiple_infection_sites_flag",
  "sepsis_coagulation_dependent_flag",
  "blood_culture_obtained_flag",
  "blood_culture_valid_positive_flag"
)

missing_binary_columns <- setdiff(
  binary_columns,
  names(
    analysis_final_base
  )
)


if (
  length(
    missing_binary_columns
  ) > 0L
) {
  stop(
    paste0(
      "The following binary variable(s) are missing from ",
      "analysis_final_base: ",
      paste(
        missing_binary_columns,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}

for (
  variable_name in binary_columns
) {
  analysis_final_base[[
    variable_name
  ]] <- binary_integer(
    values =
      analysis_final_base[[
        variable_name
      ]],

    variable_name =
      variable_name
  )
}


# ==============================================================================
# 04. CONSTRUCT THE FINAL MATCHING VARIABLES
# ==============================================================================

noncoagulation_sofa_columns <- c(
  "sofa_respiratory_revised_0_24h",
  "sofa_hepatic_revised_0_24h",
  "sofa_cardiovascular_revised_0_24h",
  "sofa_neurologic_revised_0_24h",
  "sofa_renal_revised_0_24h"
)


all_sofa_columns <- c(
  "sofa_respiratory_revised_0_24h",
  "sofa_coagulation_revised_0_24h",
  "sofa_hepatic_revised_0_24h",
  "sofa_cardiovascular_revised_0_24h",
  "sofa_neurologic_revised_0_24h",
  "sofa_renal_revised_0_24h"
)


expected_insurance_levels <- c(
  "Private",
  "Medicaid",
  "Medicare",
  "Other",
  "Unknown"
)


infection_site_levels <- c(
  "unknown",
  "skin_soft_tissue",
  "bone_joint",
  "gastrointestinal_intraabdominal",
  "device_procedure_related",
  "urinary",
  "respiratory",
  "bloodstream_endovascular",
  "multiple_sites",
  "other_less_frequent"
)


analysis_final_base <- analysis_final_base %>%
  dplyr::mutate(
    gender_match = factor(
      gender
    ),

    race_ed_clean = race_ed %>%
      as.character() %>%
      stringr::str_squish() %>%
      stringr::str_to_upper(),

    race_match_group = dplyr::case_when(
      is.na(
        race_ed_clean
      ) |
        race_ed_clean == "" |
        race_ed_clean %in% c(
          "UNKNOWN",
          "PATIENT DECLINED TO ANSWER",
          "UNABLE TO OBTAIN"
        ) ~
        "Unknown",

      stringr::str_starts(
        race_ed_clean,
        "WHITE"
      ) ~
        "White",

      stringr::str_starts(
        race_ed_clean,
        "BLACK"
      ) ~
        "Black",

      stringr::str_starts(
        race_ed_clean,
        "HISPANIC"
      ) |
        stringr::str_starts(
          race_ed_clean,
          "LATINO"
        ) ~
        "Hispanic_Latino",

      stringr::str_starts(
        race_ed_clean,
        "ASIAN"
      ) ~
        "Asian",

      TRUE ~
        "Other"
    ),

    race_match_group = factor(
      race_match_group,
      levels = c(
        "White",
        "Black",
        "Hispanic_Latino",
        "Asian",
        "Other",
        "Unknown"
      )
    ),

    insurance_match =
      stringr::str_squish(
        as.character(
          insurance
        )
      ),

    insurance_match = dplyr::case_when(
      is.na(
        insurance_match
      ) |
        insurance_match == "" |
        stringr::str_to_upper(
          insurance_match
        ) %in% c(
          "UNKNOWN",
          "UNABLE TO OBTAIN"
        ) ~
        "Unknown",

      stringr::str_to_upper(
        insurance_match
      ) == "PRIVATE" ~
        "Private",

      stringr::str_to_upper(
        insurance_match
      ) == "MEDICAID" ~
        "Medicaid",

      stringr::str_to_upper(
        insurance_match
      ) == "MEDICARE" ~
        "Medicare",

      stringr::str_to_upper(
        insurance_match
      ) == "OTHER" ~
        "Other",

      TRUE ~
        insurance_match
    ),

    insurance_match_group = factor(
      insurance_match,
      levels =
        expected_insurance_levels
    ),

    any_liver_disease = dplyr::case_when(
      mild_liver_disease == 1L |
        severe_liver_disease == 1L ~
        1L,

      mild_liver_disease == 0L &
        severe_liver_disease == 0L ~
        0L,

      TRUE ~
        NA_integer_
    ),

    any_diabetes = dplyr::case_when(
      diabetes_without_cc == 1L |
        diabetes_with_cc == 1L ~
        1L,

      diabetes_without_cc == 0L &
        diabetes_with_cc == 0L ~
        0L,

      TRUE ~
        NA_integer_
    ),

    sofa_noncoag_unavailable_component_n =
      as.integer(
        rowSums(
          dplyr::across(
            dplyr::all_of(
              noncoagulation_sofa_columns
            ),
            ~ is.na(
              .x
            )
          )
        )
      ),

    sofa_noncoag_observed_0_24h =
      rowSums(
        dplyr::across(
          dplyr::all_of(
            noncoagulation_sofa_columns
          )
        ),
        na.rm = TRUE
      ),

    sofa_total_revised_observed_0_24h =
      rowSums(
        dplyr::across(
          dplyr::all_of(
            all_sofa_columns
          )
        ),
        na.rm = TRUE
      ),

    sofa_noncoag_observed_category =
      dplyr::case_when(
        sofa_noncoag_observed_0_24h == 0 ~
          "0",

        sofa_noncoag_observed_0_24h == 1 ~
          "1",

        sofa_noncoag_observed_0_24h >= 2 &
          sofa_noncoag_observed_0_24h <= 3 ~
          "2_3",

        sofa_noncoag_observed_0_24h >= 4 ~
          "4_plus",

        TRUE ~
          NA_character_
      ),

    sofa_noncoag_observed_category = factor(
      sofa_noncoag_observed_category,
      levels = c(
        "0",
        "1",
        "2_3",
        "4_plus"
      )
    ),

    infection_site_index_broad_clean =
      stringr::str_to_lower(
        stringr::str_squish(
          as.character(
            infection_site_index_broad
          )
        )
      ),

    infection_site_index_broad_clean =
      dplyr::case_when(
        is.na(
          infection_site_index_broad_clean
        ) |
          infection_site_index_broad_clean == "" ~
          "unknown",

        multiple_infection_sites_flag == 1L ~
          "multiple_sites",

        TRUE ~
          infection_site_index_broad_clean
      ),

    infection_site_match_group =
      dplyr::case_when(
        infection_site_index_broad_clean == "unknown" ~
          "unknown",

        infection_site_index_broad_clean ==
          "skin_soft_tissue" ~
          "skin_soft_tissue",

        infection_site_index_broad_clean ==
          "bone_joint" ~
          "bone_joint",

        stringr::str_detect(
          infection_site_index_broad_clean,
          "^gastrointestinal"
        ) ~
          "gastrointestinal_intraabdominal",

        infection_site_index_broad_clean ==
          "device_procedure_related" ~
          "device_procedure_related",

        infection_site_index_broad_clean ==
          "urinary" ~
          "urinary",

        infection_site_index_broad_clean ==
          "respiratory" ~
          "respiratory",

        infection_site_index_broad_clean %in% c(
          "cardiovascular_endovascular",
          "primary_bloodstream"
        ) ~
          "bloodstream_endovascular",

        infection_site_index_broad_clean ==
          "multiple_sites" ~
          "multiple_sites",

        TRUE ~
          "other_less_frequent"
      ),

    infection_site_match_group = factor(
      infection_site_match_group,
      levels =
        infection_site_levels
    ),

    primary_phenotype_group_revised =
      dplyr::case_when(
        primary_phenotype_group == "control" ~
          "control",

        infected_phenotype_flag == 1L &
          sofa_total_revised_observed_0_24h >= 2 ~
          "sepsis",

        infected_phenotype_flag == 1L &
          sofa_total_revised_observed_0_24h < 2 ~
          "infection_without_sepsis",

        TRUE ~
          "unclassified"
      ),

    sepsis_revised_flag =
      dplyr::case_when(
        primary_phenotype_group_revised == "sepsis" ~
          1L,

        primary_phenotype_group_revised %in% c(
          "infection_without_sepsis",
          "control"
        ) ~
          0L,

        TRUE ~
          NA_integer_
      ),

    newly_reclassified_to_sepsis_flag =
      as.integer(
        primary_phenotype_group ==
          "infection_without_sepsis" &
          primary_phenotype_group_revised ==
            "sepsis"
      ),

    primary_phenotype_group_revised =
      factor(
        primary_phenotype_group_revised,
        levels = c(
          "control",
          "infection_without_sepsis",
          "sepsis"
        )
      )
  )

analysis_final_base %>%
  dplyr::count(
    primary_phenotype_group_revised,
    name = "episodes"
  ) %>%
  print(
    n = Inf
  )

unexpected_insurance_levels <- setdiff(
  unique(
    as.character(
      analysis_final_base$
        insurance_match
    )
  ),
  expected_insurance_levels
)


unexpected_insurance_levels <- unexpected_insurance_levels[
  !is.na(
    unexpected_insurance_levels
  )
]


if (
  length(
    unexpected_insurance_levels
  ) > 0L
) {
  stop(
    paste0(
      "Unexpected insurance category or categories: ",
      paste(
        unexpected_insurance_levels,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


if (
  any(
    is.na(
      analysis_final_base$
        sepsis_coagulation_dependent_flag[
        analysis_final_base$
          primary_phenotype_group_revised ==
          "sepsis"
      ]
    )
  )
) {
  stop(
    paste0(
      "sepsis_coagulation_dependent_flag is unavailable for at least ",
      "one sepsis episode."
    ),
    call. = FALSE
  )
}


# ==============================================================================
# 05. CONSTRUCT ANALYSES A AND B
# ==============================================================================

match_data_A_sepsis_vs_control <-
  analysis_final_base %>%
  dplyr::filter(
    primary_phenotype_group_revised %in% c(
      "control",
      "sepsis"
    )
  ) %>%
  dplyr::mutate(
    treatment_sepsis = as.integer(
      primary_phenotype_group_revised ==
        "sepsis"
    ),

    gender_match = droplevels(
      gender_match
    ),

    race_match_group = droplevels(
      race_match_group
    ),

    insurance_match_group = droplevels(
      insurance_match_group
    ),

    sofa_noncoag_observed_category =
      droplevels(
        sofa_noncoag_observed_category
      )
  )


match_data_B_sepsis_vs_infection <-
  analysis_final_base %>%
  dplyr::filter(
    primary_phenotype_group_revised %in% c(
      "infection_without_sepsis",
      "sepsis"
    )
  ) %>%
  dplyr::mutate(
    treatment_sepsis = as.integer(
      primary_phenotype_group_revised ==
        "sepsis"
    ),

    gender_match = droplevels(
      gender_match
    ),

    race_match_group = droplevels(
      race_match_group
    ),

    insurance_match_group = droplevels(
      insurance_match_group
    ),

    infection_site_match_group =
      droplevels(
        infection_site_match_group
      )
  )


sepsis_episode_ids_A <-
  match_data_A_sepsis_vs_control$
    episode_id[
    match_data_A_sepsis_vs_control$
      treatment_sepsis ==
      1L
  ]


sepsis_episode_ids_B <-
  match_data_B_sepsis_vs_infection$
    episode_id[
    match_data_B_sepsis_vs_infection$
      treatment_sepsis ==
      1L
  ]


if (
  !setequal(
    sepsis_episode_ids_A,
    sepsis_episode_ids_B
  )
) {
  stop(
    "Analyses A and B do not contain the same sepsis episodes.",
    call. = FALSE
  )
}


# ==============================================================================
# 06. FINAL MATCHING FORMULAS
# ==============================================================================

balance_formula_A <- treatment_sepsis ~
  age +
  gender_match +
  race_match_group +
  insurance_match_group +
  chronic_pulmonary_disease +
  any_liver_disease +
  renal_disease +
  any_diabetes +
  sofa_noncoag_observed_category +
  sofa_noncoag_unavailable_component_n


balance_formula_B <- treatment_sepsis ~
  age +
  gender_match +
  race_match_group +
  insurance_match_group +
  chronic_pulmonary_disease +
  any_liver_disease +
  renal_disease +
  any_diabetes +
  infection_site_match_group


validate_model_frame <- function(
  data,
  formula,
  analysis_id
) {

  model_data <- stats::model.frame(
    formula =
      formula,

    data =
      data,

    na.action =
      stats::na.pass
  )


  unavailable_rows <- sum(
    !stats::complete.cases(
      model_data
    )
  )


  if (
    unavailable_rows > 0L
  ) {
    stop(
      paste0(
        unavailable_rows,
        " row(s) contain unavailable matching covariates in analysis ",
        analysis_id,
        "."
      ),
      call. = FALSE
    )
  }


  invisible(
    TRUE
  )
}


validate_model_frame(
  data =
    match_data_A_sepsis_vs_control,
  formula =
    balance_formula_A,
  analysis_id =
    "A"
)


validate_model_frame(
  data =
    match_data_B_sepsis_vs_infection,
  formula =
    balance_formula_B,
  analysis_id =
    "B"
)

# ==============================================================================
# 06.1. VALIDATE THE PRE-MATCHING ANALYSIS POPULATIONS
# ==============================================================================

pre_matching_population_summary <- dplyr::bind_rows(
  match_data_A_sepsis_vs_control %>%
    dplyr::summarise(
      analysis_id = "A",
      comparison = "Sepsis versus control",
      total_episodes = dplyr::n(),
      unique_subjects = dplyr::n_distinct(
        subject_id
      ),
      sepsis_episodes = sum(
        treatment_sepsis == 1L
      ),
      comparator_episodes = sum(
        treatment_sepsis == 0L
      ),
      duplicated_episode_ids = sum(
        duplicated(
          episode_id
        )
      ),
      duplicated_subject_ids = sum(
        duplicated(
          subject_id
        )
      )
    ),

  match_data_B_sepsis_vs_infection %>%
    dplyr::summarise(
      analysis_id = "B",
      comparison = "Sepsis versus infection without sepsis",
      total_episodes = dplyr::n(),
      unique_subjects = dplyr::n_distinct(
        subject_id
      ),
      sepsis_episodes = sum(
        treatment_sepsis == 1L
      ),
      comparator_episodes = sum(
        treatment_sepsis == 0L
      ),
      duplicated_episode_ids = sum(
        duplicated(
          episode_id
        )
      ),
      duplicated_subject_ids = sum(
        duplicated(
          subject_id
        )
      )
    )
)


print(
  pre_matching_population_summary,
  n = Inf,
  width = Inf
)


if (
  any(
    pre_matching_population_summary$
      duplicated_episode_ids > 0L
  )
) {
  stop(
    "Duplicated episode_id values were found before matching.",
    call. = FALSE
  )
}


if (
  any(
    pre_matching_population_summary$
      duplicated_subject_ids > 0L
  )
) {
  stop(
    "Duplicated subject_id values were found before matching.",
    call. = FALSE
  )
}


if (
  any(
    pre_matching_population_summary$
      total_episodes !=
      pre_matching_population_summary$
        unique_subjects
  )
) {
  stop(
    "The pre-matching datasets do not contain one episode per subject.",
    call. = FALSE
  )
}


if (
  any(
    pre_matching_population_summary$
      sepsis_episodes == 0L
  ) ||
    any(
      pre_matching_population_summary$
        comparator_episodes == 0L
    )
) {
  stop(
    "At least one analysis does not contain both comparison groups.",
    call. = FALSE
  )
}


if (
  pre_matching_population_summary$
    sepsis_episodes[
    pre_matching_population_summary$
      analysis_id == "A"
  ] !=
    pre_matching_population_summary$
      sepsis_episodes[
      pre_matching_population_summary$
        analysis_id == "B"
    ]
) {
  stop(
    "Analyses A and B contain different numbers of sepsis episodes.",
    call. = FALSE
  )
}


pre_matching_factor_summary <- dplyr::bind_rows(
  tibble::tibble(
    analysis_id = "A",
    variable = c(
      "gender_match",
      "race_match_group",
      "insurance_match_group",
      "sofa_noncoag_observed_category"
    ),
    observed_levels = c(
      nlevels(
        droplevels(
          match_data_A_sepsis_vs_control$
            gender_match
        )
      ),
      nlevels(
        droplevels(
          match_data_A_sepsis_vs_control$
            race_match_group
        )
      ),
      nlevels(
        droplevels(
          match_data_A_sepsis_vs_control$
            insurance_match_group
        )
      ),
      nlevels(
        droplevels(
          match_data_A_sepsis_vs_control$
            sofa_noncoag_observed_category
        )
      )
    )
  ),

  tibble::tibble(
    analysis_id = "B",
    variable = c(
      "gender_match",
      "race_match_group",
      "insurance_match_group",
      "infection_site_match_group"
    ),
    observed_levels = c(
      nlevels(
        droplevels(
          match_data_B_sepsis_vs_infection$
            gender_match
        )
      ),
      nlevels(
        droplevels(
          match_data_B_sepsis_vs_infection$
            race_match_group
        )
      ),
      nlevels(
        droplevels(
          match_data_B_sepsis_vs_infection$
            insurance_match_group
        )
      ),
      nlevels(
        droplevels(
          match_data_B_sepsis_vs_infection$
            infection_site_match_group
        )
      )
    )
  )
)


print(
  pre_matching_factor_summary,
  n = Inf,
  width = Inf
)


if (
  any(
    pre_matching_factor_summary$
      observed_levels < 2L
  )
) {
  stop(
    "At least one categorical matching variable has fewer than two observed levels.",
    call. = FALSE
  )
}

# ==============================================================================
# 07. EXECUTE CARDINALITY ATT PROFILE MATCHING
# ==============================================================================
MATCHING_SEED <- 20260729
BALANCE_LIMIT <- 0.1

set.seed(
  MATCHING_SEED
)


match_object_A <- MatchIt::matchit(
  formula =
    balance_formula_A,

  data =
    match_data_A_sepsis_vs_control,

  method =
    "cardinality",

  estimand =
    "ATT",

  ratio =
    NA,

  tols =
    BALANCE_LIMIT,

  std.tols =
    TRUE,

  solver =
    "highs",

  time =
    1800,

  verbose =
    TRUE,

  include.obj =
    TRUE
)


set.seed(
  MATCHING_SEED
)


match_object_B <- MatchIt::matchit(
  formula =
    balance_formula_B,

  data =
    match_data_B_sepsis_vs_infection,

  method =
    "cardinality",

  estimand =
    "ATT",

  ratio =
    NA,

  tols =
    BALANCE_LIMIT,

  std.tols =
    TRUE,

  solver =
    "highs",

  time =
    1800,

  verbose =
    TRUE,

  include.obj =
    TRUE
)


analysis_A_final_data <-
  match_data_A_sepsis_vs_control %>%
  dplyr::mutate(
    matching_weight =
      as.numeric(
        match_object_A$
          weights
      )
  ) %>%
  dplyr::filter(
    matching_weight > 0
  )


analysis_B_final_data <-
  match_data_B_sepsis_vs_infection %>%
  dplyr::mutate(
    matching_weight =
      as.numeric(
        match_object_B$
          weights
      )
  ) %>%
  dplyr::filter(
    matching_weight > 0
  )


if (
  sum(
    analysis_A_final_data$
      treatment_sepsis ==
      1L
  ) !=
    sum(
      match_data_A_sepsis_vs_control$
        treatment_sepsis ==
        1L
    )
) {
  stop(
    "Analysis A did not retain every sepsis episode.",
    call. = FALSE
  )
}


if (
  sum(
    analysis_B_final_data$
      treatment_sepsis ==
      1L
  ) !=
    sum(
      match_data_B_sepsis_vs_infection$
        treatment_sepsis ==
        1L
    )
) {
  stop(
    "Analysis B did not retain every sepsis episode.",
    call. = FALSE
  )
}


if (
  !setequal(
    analysis_A_final_data$
      episode_id[
      analysis_A_final_data$
        treatment_sepsis ==
        1L
    ],

    analysis_B_final_data$
      episode_id[
      analysis_B_final_data$
        treatment_sepsis ==
        1L
    ]
  )
) {
  stop(
    "The matched analyses do not contain the same sepsis episodes.",
    call. = FALSE
  )
}


# ==============================================================================
# 08. BALANCE CHECKS
# ==============================================================================

create_balance_table <- function(
  match_object
) {

  balance_object <- cobalt::bal.tab(
    match_object,
    un = TRUE,
    abs = TRUE,
    continuous = "std",
    binary = "std",
    s.d.denom = "treated",
    quick = FALSE
  )


  balance_table <-
    balance_object$
    Balance %>%
    as.data.frame() %>%
    tibble::rownames_to_column(
      var = "covariate"
    ) %>%
    tibble::as_tibble()


  list(
    object =
      balance_object,

    table =
      balance_table
  )
}


balance_A <- create_balance_table(
  match_object_A
)


balance_B <- create_balance_table(
  match_object_B
)


balance_issues_A <-
  balance_A$
  table %>%
  dplyr::filter(
    covariate != "distance",
    is.na(
      Diff.Adj
    ) |
      abs(
        Diff.Adj
      ) >=
        BALANCE_LIMIT
  )


balance_issues_B <-
  balance_B$
  table %>%
  dplyr::filter(
    covariate != "distance",
    is.na(
      Diff.Adj
    ) |
      abs(
        Diff.Adj
      ) >=
        BALANCE_LIMIT
  )


if (
  nrow(
    balance_issues_A
  ) > 0L
) {
  stop(
    "At least one modeled term failed the analysis A balance criterion.",
    call. = FALSE
  )
}


if (
  nrow(
    balance_issues_B
  ) > 0L
) {
  stop(
    "At least one modeled term failed the analysis B balance criterion.",
    call. = FALSE
  )
}


matching_sample_sizes <- dplyr::bind_rows(
  match_data_A_sepsis_vs_control %>%
    dplyr::count(
      treatment_sepsis,
      name = "episodes"
    ) %>%
    dplyr::mutate(
      analysis_id = "A",
      design = "before_matching"
    ),

  analysis_A_final_data %>%
    dplyr::count(
      treatment_sepsis,
      name = "episodes"
    ) %>%
    dplyr::mutate(
      analysis_id = "A",
      design = "after_matching"
    ),

  match_data_B_sepsis_vs_infection %>%
    dplyr::count(
      treatment_sepsis,
      name = "episodes"
    ) %>%
    dplyr::mutate(
      analysis_id = "B",
      design = "before_matching"
    ),

  analysis_B_final_data %>%
    dplyr::count(
      treatment_sepsis,
      name = "episodes"
    ) %>%
    dplyr::mutate(
      analysis_id = "B",
      design = "after_matching"
    )
) %>%
  dplyr::mutate(
    group = dplyr::case_when(
      treatment_sepsis == 1L ~
        "Sepsis",

      analysis_id == "A" ~
        "Control",

      TRUE ~
        "Infection without sepsis"
    )
  ) %>%
  dplyr::select(
    analysis_id,
    design,
    group,
    episodes
  )


print(
  matching_sample_sizes,
  n = Inf,
  width = Inf
)

# ==============================================================================
# 08.1. COAGULATION-DEPENDENT SEPSIS SENSITIVITY
#       EXCLUDE BEFORE MATCHING AND REMATCH
# ==============================================================================

# This sensitivity analysis removes sepsis episodes whose Sepsis-3
# classification depends on the coagulation SOFA component BEFORE matching.
# Matching is then repeated using the original analysis-specific formulas
# and the same cardinality matching specifications.


# ------------------------------------------------------------------------------
# 08.1.1. Verify the shared sepsis population and coagulation-dependence flag
# ------------------------------------------------------------------------------

coagulation_sensitivity_sepsis_A <-
  match_data_A_sepsis_vs_control %>%
  dplyr::filter(
    treatment_sepsis == 1L
  ) %>%
  dplyr::select(
    episode_id,
    sepsis_coagulation_dependent_flag
  ) %>%
  dplyr::arrange(
    episode_id
  )


coagulation_sensitivity_sepsis_B <-
  match_data_B_sepsis_vs_infection %>%
  dplyr::filter(
    treatment_sepsis == 1L
  ) %>%
  dplyr::select(
    episode_id,
    sepsis_coagulation_dependent_flag
  ) %>%
  dplyr::arrange(
    episode_id
  )


if (
  anyNA(
    coagulation_sensitivity_sepsis_A$
    sepsis_coagulation_dependent_flag
  ) ||
  anyNA(
    coagulation_sensitivity_sepsis_B$
    sepsis_coagulation_dependent_flag
  )
) {
  stop(
    paste(
      "The coagulation-dependence flag is unavailable for at least",
      "one sepsis episode."
    ),
    call. = FALSE
  )
}


if (
  !identical(
    as.character(
      coagulation_sensitivity_sepsis_A$
      episode_id
    ),
    as.character(
      coagulation_sensitivity_sepsis_B$
      episode_id
    )
  ) ||
  !identical(
    as.integer(
      coagulation_sensitivity_sepsis_A$
      sepsis_coagulation_dependent_flag
    ),
    as.integer(
      coagulation_sensitivity_sepsis_B$
      sepsis_coagulation_dependent_flag
    )
  )
) {
  stop(
    paste(
      "Analyses A and B disagree on the shared sepsis population",
      "or on the coagulation-dependence flag."
    ),
    call. = FALSE
  )
}


coagulation_sensitivity_exclusion_summary <-
  tibble::tibble(
    original_sepsis_n =
      nrow(
        coagulation_sensitivity_sepsis_A
      ),
    
    excluded_coagulation_dependent_sepsis_n =
      sum(
        coagulation_sensitivity_sepsis_A$
          sepsis_coagulation_dependent_flag ==
          1L
      ),
    
    eligible_sepsis_n =
      sum(
        coagulation_sensitivity_sepsis_A$
          sepsis_coagulation_dependent_flag ==
          0L
      ),
    
    excluded_percent =
      100 *
      excluded_coagulation_dependent_sepsis_n /
      original_sepsis_n
  )


cat(
  "\nCOAGULATION SENSITIVITY EXCLUSION SUMMARY\n"
)

print(
  coagulation_sensitivity_exclusion_summary,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------------------------
# 08.1.2. Construct restricted PRE-MATCH populations
#         and enforce common support for non-coagulation SOFA in analysis A
# ------------------------------------------------------------------------------

# First exclude coagulation-dependent sepsis while retaining the complete
# comparator candidate pools.

match_data_A_coagulation_sensitivity_pre_support <-
  match_data_A_sepsis_vs_control %>%
  dplyr::filter(
    treatment_sepsis == 0L |
      (
        treatment_sepsis == 1L &
          sepsis_coagulation_dependent_flag == 0L
      )
  )


match_data_B_coagulation_sensitivity <-
  match_data_B_sepsis_vs_infection %>%
  dplyr::filter(
    treatment_sepsis == 0L |
      (
        treatment_sepsis == 1L &
          sepsis_coagulation_dependent_flag == 0L
      )
  )


# Identify the levels of non-coagulation SOFA represented in the eligible
# sepsis population. Analysis A controls are restricted to this support.

eligible_sepsis_sofa_levels_A <-
  match_data_A_coagulation_sensitivity_pre_support %>%
  dplyr::filter(
    treatment_sepsis == 1L
  ) %>%
  dplyr::pull(
    sofa_noncoag_observed_category
  ) %>%
  as.character() %>%
  unique() %>%
  sort()


if (
  length(
    eligible_sepsis_sofa_levels_A
  ) < 2L
) {
  stop(
    paste(
      "Fewer than two non-coagulation SOFA categories remain",
      "among eligible sepsis episodes in analysis A."
    ),
    call. = FALSE
  )
}


match_data_A_coagulation_sensitivity <-
  match_data_A_coagulation_sensitivity_pre_support %>%
  dplyr::filter(
    treatment_sepsis == 1L |
      as.character(
        sofa_noncoag_observed_category
      ) %in%
      eligible_sepsis_sofa_levels_A
  ) %>%
  dplyr::mutate(
    sofa_noncoag_observed_category =
      droplevels(
        sofa_noncoag_observed_category
      )
  )


# Explicit audit of the support restriction.

coagulation_sensitivity_A_support_summary <-
  tibble::tibble(
    supported_sofa_categories =
      paste(
        eligible_sepsis_sofa_levels_A,
        collapse = ", "
      ),
    
    eligible_sepsis_n =
      sum(
        match_data_A_coagulation_sensitivity$
          treatment_sepsis ==
          1L
      ),
    
    comparator_candidates_before_support_n =
      sum(
        match_data_A_coagulation_sensitivity_pre_support$
          treatment_sepsis ==
          0L
      ),
    
    comparator_candidates_after_support_n =
      sum(
        match_data_A_coagulation_sensitivity$
          treatment_sepsis ==
          0L
      ),
    
    comparators_excluded_outside_support_n =
      comparator_candidates_before_support_n -
      comparator_candidates_after_support_n
  )


cat(
  "\nANALYSIS A COAGULATION SENSITIVITY COMMON-SUPPORT SUMMARY\n"
)

print(
  coagulation_sensitivity_A_support_summary,
  n = Inf,
  width = Inf
)


# Validate the restricted model frames before rematching.

validate_model_frame(
  data =
    match_data_A_coagulation_sensitivity,
  
  formula =
    balance_formula_A,
  
  analysis_id =
    "A coagulation sensitivity rematch"
)


validate_model_frame(
  data =
    match_data_B_coagulation_sensitivity,
  
  formula =
    balance_formula_B,
  
  analysis_id =
    "B coagulation sensitivity rematch"
)


# Confirm that the support restriction removed no eligible sepsis episodes.

if (
  sum(
    match_data_A_coagulation_sensitivity$
    treatment_sepsis ==
    1L
  ) !=
  sum(
    match_data_A_coagulation_sensitivity_pre_support$
    treatment_sepsis ==
    1L
  )
) {
  stop(
    "The analysis A common-support restriction removed eligible sepsis episodes.",
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# 08.1.3. Repeat cardinality matching using ORIGINAL specifications
# ------------------------------------------------------------------------------

set.seed(
  MATCHING_SEED
)


match_object_A_coagulation_sensitivity <-
  MatchIt::matchit(
    formula =
      balance_formula_A,
    
    data =
      match_data_A_coagulation_sensitivity,
    
    method =
      "cardinality",
    
    estimand =
      "ATT",
    
    ratio =
      NA,
    
    tols =
      BALANCE_LIMIT,
    
    std.tols =
      TRUE,
    
    solver =
      "highs",
    
    time =
      1800,
    
    verbose =
      TRUE,
    
    include.obj =
      TRUE
  )


set.seed(
  MATCHING_SEED
)


match_object_B_coagulation_sensitivity <-
  MatchIt::matchit(
    formula =
      balance_formula_B,
    
    data =
      match_data_B_coagulation_sensitivity,
    
    method =
      "cardinality",
    
    estimand =
      "ATT",
    
    ratio =
      NA,
    
    tols =
      BALANCE_LIMIT,
    
    std.tols =
      TRUE,
    
    solver =
      "highs",
    
    time =
      1800,
    
    verbose =
      TRUE,
    
    include.obj =
      TRUE
  )


# ------------------------------------------------------------------------------
# 08.1.4. Extract rematched sensitivity samples
# ------------------------------------------------------------------------------

analysis_A_coagulation_sensitivity <-
  match_data_A_coagulation_sensitivity %>%
  dplyr::mutate(
    matching_weight =
      as.numeric(
        match_object_A_coagulation_sensitivity$
          weights
      )
  ) %>%
  dplyr::filter(
    matching_weight > 0
  )


analysis_B_coagulation_sensitivity <-
  match_data_B_coagulation_sensitivity %>%
  dplyr::mutate(
    matching_weight =
      as.numeric(
        match_object_B_coagulation_sensitivity$
          weights
      )
  ) %>%
  dplyr::filter(
    matching_weight > 0
  )


# ------------------------------------------------------------------------------
# 08.1.5. Assess balance after sensitivity rematching
# ------------------------------------------------------------------------------

balance_A_coagulation_sensitivity <-
  create_balance_table(
    match_object_A_coagulation_sensitivity
  )


balance_B_coagulation_sensitivity <-
  create_balance_table(
    match_object_B_coagulation_sensitivity
  )


balance_issues_A_coagulation_sensitivity <-
  balance_A_coagulation_sensitivity$
  table %>%
  dplyr::filter(
    covariate != "distance",
    is.na(
      Diff.Adj
    ) |
      abs(
        Diff.Adj
      ) >=
      BALANCE_LIMIT
  )


balance_issues_B_coagulation_sensitivity <-
  balance_B_coagulation_sensitivity$
  table %>%
  dplyr::filter(
    covariate != "distance",
    is.na(
      Diff.Adj
    ) |
      abs(
        Diff.Adj
      ) >=
      BALANCE_LIMIT
  )


# ------------------------------------------------------------------------------
# 08.1.6. Sample-size audit
# ------------------------------------------------------------------------------

coagulation_sensitivity_matching_sample_sizes <-
  dplyr::bind_rows(
    
    match_data_A_coagulation_sensitivity %>%
      dplyr::count(
        treatment_sepsis,
        name = "episodes"
      ) %>%
      dplyr::mutate(
        analysis_id = "A",
        design =
          "after_exclusion_before_rematching"
      ),
    
    analysis_A_coagulation_sensitivity %>%
      dplyr::count(
        treatment_sepsis,
        name = "episodes"
      ) %>%
      dplyr::mutate(
        analysis_id = "A",
        design =
          "after_rematching"
      ),
    
    match_data_B_coagulation_sensitivity %>%
      dplyr::count(
        treatment_sepsis,
        name = "episodes"
      ) %>%
      dplyr::mutate(
        analysis_id = "B",
        design =
          "after_exclusion_before_rematching"
      ),
    
    analysis_B_coagulation_sensitivity %>%
      dplyr::count(
        treatment_sepsis,
        name = "episodes"
      ) %>%
      dplyr::mutate(
        analysis_id = "B",
        design =
          "after_rematching"
      )
  ) %>%
  dplyr::mutate(
    group =
      dplyr::case_when(
        treatment_sepsis == 1L ~
          "Sepsis",
        
        analysis_id == "A" ~
          "Control",
        
        TRUE ~
          "Infection without sepsis"
      )
  ) %>%
  dplyr::select(
    analysis_id,
    design,
    group,
    episodes
  )


cat(
  "\nCOAGULATION SENSITIVITY REMATCH SAMPLE SIZES\n"
)

print(
  coagulation_sensitivity_matching_sample_sizes,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 08.1.7. Validate the rematched sensitivity design
# ------------------------------------------------------------------------------

eligible_sepsis_ids_A <-
  sort(
    as.character(
      match_data_A_coagulation_sensitivity$
        episode_id[
          match_data_A_coagulation_sensitivity$
            treatment_sepsis ==
            1L
        ]
    )
  )


eligible_sepsis_ids_B <-
  sort(
    as.character(
      match_data_B_coagulation_sensitivity$
        episode_id[
          match_data_B_coagulation_sensitivity$
            treatment_sepsis ==
            1L
        ]
    )
  )


rematched_sepsis_ids_A <-
  sort(
    as.character(
      analysis_A_coagulation_sensitivity$
        episode_id[
          analysis_A_coagulation_sensitivity$
            treatment_sepsis ==
            1L
        ]
    )
  )


rematched_sepsis_ids_B <-
  sort(
    as.character(
      analysis_B_coagulation_sensitivity$
        episode_id[
          analysis_B_coagulation_sensitivity$
            treatment_sepsis ==
            1L
        ]
    )
  )

expected_supported_comparator_ids_A <-
  sort(
    as.character(
      match_data_A_coagulation_sensitivity_pre_support$
        episode_id[
          match_data_A_coagulation_sensitivity_pre_support$
            treatment_sepsis ==
            0L &
            as.character(
              match_data_A_coagulation_sensitivity_pre_support$
                sofa_noncoag_observed_category
            ) %in%
            eligible_sepsis_sofa_levels_A
        ]
    )
  )


restricted_comparator_ids_A <-
  sort(
    as.character(
      match_data_A_coagulation_sensitivity$
        episode_id[
          match_data_A_coagulation_sensitivity$
            treatment_sepsis ==
            0L
        ]
    )
  )


original_comparator_ids_B <-
  sort(
    as.character(
      match_data_B_sepsis_vs_infection$
        episode_id[
          match_data_B_sepsis_vs_infection$
            treatment_sepsis ==
            0L
        ]
    )
  )


restricted_comparator_ids_B <-
  sort(
    as.character(
      match_data_B_coagulation_sensitivity$
        episode_id[
          match_data_B_coagulation_sensitivity$
            treatment_sepsis ==
            0L
        ]
    )
  )


coagulation_sensitivity_rematch_validation_checks <-
  tibble::tibble(
    check = c(
      "Restricted A and B contain the same eligible sepsis episodes",
      "Restricted A contains no coagulation-dependent sepsis",
      "Restricted B contains no coagulation-dependent sepsis",
      "Analysis A comparator pool is restricted exactly to eligible sepsis SOFA support",
      "Analysis B comparator candidate pool is unchanged",
      "Analysis A rematch retains every eligible sepsis episode",
      "Analysis B rematch retains every eligible sepsis episode",
      "Rematched A and B contain the same sepsis episodes",
      "Analysis A rematch satisfies the balance criterion",
      "Analysis B rematch satisfies the balance criterion",
      "Both sensitivity rematches retain at least one comparator"
    ),
    
    pass = c(
      identical(
        eligible_sepsis_ids_A,
        eligible_sepsis_ids_B
      ),
      
      sum(
        match_data_A_coagulation_sensitivity$
          treatment_sepsis ==
          1L &
          match_data_A_coagulation_sensitivity$
          sepsis_coagulation_dependent_flag ==
          1L
      ) ==
        0L,
      
      sum(
        match_data_B_coagulation_sensitivity$
          treatment_sepsis ==
          1L &
          match_data_B_coagulation_sensitivity$
          sepsis_coagulation_dependent_flag ==
          1L
      ) ==
        0L,
      
      identical(
        expected_supported_comparator_ids_A,
        restricted_comparator_ids_A
      ),
      
      identical(
        original_comparator_ids_B,
        restricted_comparator_ids_B
      ),
      
      identical(
        eligible_sepsis_ids_A,
        rematched_sepsis_ids_A
      ),
      
      identical(
        eligible_sepsis_ids_B,
        rematched_sepsis_ids_B
      ),
      
      identical(
        rematched_sepsis_ids_A,
        rematched_sepsis_ids_B
      ),
      
      nrow(
        balance_issues_A_coagulation_sensitivity
      ) ==
        0L,
      
      nrow(
        balance_issues_B_coagulation_sensitivity
      ) ==
        0L,
      
      sum(
        analysis_A_coagulation_sensitivity$
          treatment_sepsis ==
          0L
      ) >
        0L &&
        sum(
          analysis_B_coagulation_sensitivity$
            treatment_sepsis ==
            0L
        ) >
        0L
    )
  )


cat(
  "\nCOAGULATION SENSITIVITY REMATCH VALIDATION CHECKS\n"
)

print(
  coagulation_sensitivity_rematch_validation_checks,
  n = Inf,
  width = Inf
)


if (
  anyNA(
    coagulation_sensitivity_rematch_validation_checks$
    pass
  ) ||
  any(
    !coagulation_sensitivity_rematch_validation_checks$
    pass
  )
) {
  stop(
    paste(
      "The coagulation sensitivity rematching failed",
      "one or more validation checks."
    ),
    call. = FALSE
  )
}

# ==============================================================================
# 09. BIOMARKER REGISTRY
# ==============================================================================

biomarker_registry <- tibble::tribble(
  ~biomarker_id,
  ~biomarker_label,
  ~value_variable,
  ~marker_class,
  ~clinical_direction,
  ~score_multiplier,

  "crp",
  "C-reactive protein",
  "crp_index_value_mg_l",
  "inflammatory_simple",
  "higher_values_indicate_sepsis",
  1,

  "wbc",
  "White blood cell count",
  "cbc_index_wbc_for_calculation",
  "hematologic_simple",
  "higher_values_indicate_sepsis",
  1,

  "neutrophil_count",
  "Absolute neutrophil count",
  "cbc_index_neutrophil_count",
  "hematologic_simple",
  "higher_values_indicate_sepsis",
  1,

  "lymphocyte_count",
  "Absolute lymphocyte count",
  "cbc_index_lymphocyte_count",
  "hematologic_simple",
  "lower_values_indicate_sepsis",
  -1,

  "monocyte_count",
  "Absolute monocyte count",
  "cbc_index_monocyte_count",
  "hematologic_simple",
  "higher_values_indicate_sepsis",
  1,

  "nlr",
  "Neutrophil-to-lymphocyte ratio",
  "nlr",
  "hematologic_composite",
  "higher_values_indicate_sepsis",
  1,

  "plr",
  "Platelet-to-lymphocyte ratio",
  "plr",
  "hematologic_composite",
  "higher_values_indicate_sepsis",
  1,

  "mlr",
  "Monocyte-to-lymphocyte ratio",
  "mlr",
  "hematologic_composite",
  "higher_values_indicate_sepsis",
  1,

  "nmr",
  "Neutrophil-to-monocyte ratio",
  "nmr",
  "hematologic_composite",
  "higher_values_indicate_sepsis",
  1,

  "siri",
  "Systemic Inflammation Response Index",
  "siri",
  "hematologic_composite",
  "higher_values_indicate_sepsis",
  1,

  "sii",
  "Systemic Immune-Inflammation Index",
  "sii",
  "hematologic_composite",
  "higher_values_indicate_sepsis",
  1
)


if (
  nrow(
    biomarker_registry
  ) !=
    11L ||
    anyDuplicated(
      biomarker_registry$
        biomarker_id
    ) > 0L
) {
  stop(
    "The final biomarker registry is invalid.",
    call. = FALSE
  )
}


# ==============================================================================
# 10. RECALCULATE CBC-DERIVED INDICES
# ==============================================================================

derive_biomarkers <- function(
  data
) {

  required_source_columns <- c(
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


  require_columns(
    data =
      data,

    required_columns =
      required_source_columns,

    object_name =
      "biomarker dataset"
  )


  data %>%
    dplyr::mutate(
      nlr_source_value =
        nlr,

      plr_source_value =
        plr,

      mlr_source_value =
        mlr,

      nmr_source_value =
        nmr,

      nlr = safe_ratio(
        cbc_index_neutrophil_count,
        cbc_index_lymphocyte_count
      ),

      plr = safe_ratio(
        cbc_index_platelet_count,
        cbc_index_lymphocyte_count
      ),

      mlr = safe_ratio(
        cbc_index_monocyte_count,
        cbc_index_lymphocyte_count
      ),

      nmr = safe_ratio(
        cbc_index_neutrophil_count,
        cbc_index_monocyte_count
      ),

      siri = safe_product_ratio(
        cbc_index_neutrophil_count,
        cbc_index_monocyte_count,
        cbc_index_lymphocyte_count
      ),

      sii = safe_product_ratio(
        cbc_index_neutrophil_count,
        cbc_index_platelet_count,
        cbc_index_lymphocyte_count
      )
    )
}


analysis_A_final_data <-
  derive_biomarkers(
    analysis_A_final_data
  )


analysis_B_final_data <-
  derive_biomarkers(
    analysis_B_final_data
  )


match_data_A_sepsis_vs_control <-
  derive_biomarkers(
    match_data_A_sepsis_vs_control
  )


match_data_B_sepsis_vs_infection <-
  derive_biomarkers(
    match_data_B_sepsis_vs_infection
  )


analysis_A_coagulation_sensitivity <-
  derive_biomarkers(
    analysis_A_coagulation_sensitivity
  )

analysis_B_coagulation_sensitivity <-
  derive_biomarkers(
    analysis_B_coagulation_sensitivity
  )


formula_check_for_dataset <- function(
  data,
  design_id,
  analysis_id
) {

  tibble::tibble(
    design_id =
      design_id,

    analysis_id =
      analysis_id,

    biomarker_id = c(
      "nlr",
      "plr",
      "mlr",
      "nmr"
    ),

    compared_n = c(
      sum(
        !is.na(
          data$
            nlr_source_value
        ) &
          !is.na(
            data$
              nlr
          )
      ),

      sum(
        !is.na(
          data$
            plr_source_value
        ) &
          !is.na(
            data$
              plr
          )
      ),

      sum(
        !is.na(
          data$
            mlr_source_value
        ) &
          !is.na(
            data$
              mlr
          )
      ),

      sum(
        !is.na(
          data$
            nmr_source_value
        ) &
          !is.na(
            data$
              nmr
          )
      )
    ),

    disagreement_n = c(
      sum(
        !values_agree(
          data$
            nlr_source_value,
          data$
            nlr
        )
      ),

      sum(
        !values_agree(
          data$
            plr_source_value,
          data$
            plr
        )
      ),

      sum(
        !values_agree(
          data$
            mlr_source_value,
          data$
            mlr
        )
      ),

      sum(
        !values_agree(
          data$
            nmr_source_value,
          data$
            nmr
        )
      )
    )
  )
}


biomarker_formula_check <- dplyr::bind_rows(
  formula_check_for_dataset(
    analysis_A_final_data,
    "matched",
    "A"
  ),
  
  formula_check_for_dataset(
    analysis_B_final_data,
    "matched",
    "B"
  ),
  
  formula_check_for_dataset(
    match_data_A_sepsis_vs_control,
    "unmatched",
    "A"
  ),
  
  formula_check_for_dataset(
    match_data_B_sepsis_vs_infection,
    "unmatched",
    "B"
  ),
  
  formula_check_for_dataset(
    analysis_A_coagulation_sensitivity,
    "coagulation_sensitivity",
    "A"
  ),
  
  formula_check_for_dataset(
    analysis_B_coagulation_sensitivity,
    "coagulation_sensitivity",
    "B"
  )
)


if (
  any(
    biomarker_formula_check$
      disagreement_n >
      0L
  )
) {
  stop(
    "At least one recalculated CBC-derived index disagrees with its source value.",
    call. = FALSE
  )
}


# ==============================================================================
# 11. CREATE THE DATASET INDEX
# ==============================================================================

analysis_dataset_index <- tibble::tibble(
  design_id = c(
    "matched",
    "matched",
    "unmatched",
    "unmatched",
    "coagulation_sensitivity",
    "coagulation_sensitivity"
  ),

  analysis_id = c(
    "A",
    "B",
    "A",
    "B",
    "A",
    "B"
  ),

  data = list(
    analysis_A_final_data,
    analysis_B_final_data,
    match_data_A_sepsis_vs_control,
    match_data_B_sepsis_vs_infection,
    analysis_A_coagulation_sensitivity,
    analysis_B_coagulation_sensitivity
  )
)


# ==============================================================================
# 12. BIOMARKER AVAILABILITY AND DISTRIBUTIONS
# ==============================================================================

biomarker_long <- purrr::pmap_dfr(
  analysis_dataset_index,

  function(
    design_id,
    analysis_id,
    data
  ) {

    purrr::map_dfr(
      seq_len(
        nrow(
          biomarker_registry
        )
      ),

      function(
        marker_index
      ) {

        marker <-
          biomarker_registry[
            marker_index,
          ]


        tibble::tibble(
          design_id =
            design_id,

          analysis_id =
            analysis_id,

          episode_id =
            data$
              episode_id,

          treatment_sepsis =
            data$
              treatment_sepsis,

          biomarker_id =
            marker$
              biomarker_id,

          biomarker_label =
            marker$
              biomarker_label,

          marker_class =
            marker$
              marker_class,

          clinical_direction =
            marker$
              clinical_direction,

          score_multiplier =
            marker$
              score_multiplier,

          value =
            data[[
              marker$
                value_variable
            ]]
        )
      }
    )
  }
)


biomarker_availability <- biomarker_long %>%
  dplyr::group_by(
    design_id,
    analysis_id,
    biomarker_id,
    biomarker_label
  ) %>%
  dplyr::summarise(
    total_n =
      dplyr::n(),

    available_n = sum(
      !is.na(
        value
      ) &
        is.finite(
          value
        )
    ),

    missing_n = sum(
      is.na(
        value
      )
    ),

    nonfinite_n = sum(
      !is.na(
        value
      ) &
        !is.finite(
          value
        )
    ),

    distinct_available_n =
      dplyr::n_distinct(
        value[
          !is.na(
            value
          ) &
            is.finite(
              value
            )
        ]
      ),

    .groups =
      "drop"
  )


if (
  any(
    biomarker_availability$
      available_n !=
      biomarker_availability$
        total_n
  )
) {
  stop(
    "At least one study biomarker is unavailable or nonfinite.",
    call. = FALSE
  )
}


biomarker_distribution_summary <- biomarker_long %>%
  dplyr::mutate(
    group = dplyr::case_when(
      treatment_sepsis == 1L ~
        "Sepsis",

      analysis_id == "A" ~
        "Control",

      TRUE ~
        "Infection without sepsis"
    )
  ) %>%
  dplyr::group_by(
    design_id,
    analysis_id,
    group,
    biomarker_id,
    biomarker_label,
    marker_class
  ) %>%
  dplyr::summarise(
    n =
      dplyr::n(),

    mean =
      mean(
        value
      ),

    standard_deviation =
      stats::sd(
        value
      ),

    median =
      stats::median(
        value
      ),

    quartile_1 =
      stats::quantile(
        value,
        0.25,
        names = FALSE
      ),

    quartile_3 =
      stats::quantile(
        value,
        0.75,
        names = FALSE
      ),

    minimum =
      min(
        value
      ),

    maximum =
      max(
        value
      ),

    .groups =
      "drop"
  )


# ==============================================================================
# 13. SOFA AND COAGULATION DESCRIPTIVE RESULTS
# ==============================================================================

sofa_dataset_index <- analysis_dataset_index %>%
  dplyr::filter(
    design_id %in% c(
      "matched",
      "unmatched"
    )
  )


sofa_summary <- purrr::pmap_dfr(
  sofa_dataset_index,

  function(
    design_id,
    analysis_id,
    data
  ) {

    data %>%
      dplyr::mutate(
        group = dplyr::case_when(
          treatment_sepsis == 1L ~
            "Sepsis",

          analysis_id == "A" ~
            "Control",

          TRUE ~
            "Infection without sepsis"
        )
      ) %>%
      dplyr::select(
        group,
        sofa_total_revised_observed_0_24h,
        sofa_noncoag_observed_0_24h,
        sofa_coagulation_revised_0_24h
      ) %>%
      tidyr::pivot_longer(
        cols = -group,
        names_to = "sofa_measure",
        values_to = "value"
      ) %>%
      dplyr::group_by(
        group,
        sofa_measure
      ) %>%
      dplyr::summarise(
        total_n =
          dplyr::n(),

        available_n = sum(
          !is.na(
            value
          )
        ),

        mean =
          mean(
            value,
            na.rm = TRUE
          ),

        standard_deviation =
          stats::sd(
            value,
            na.rm = TRUE
          ),

        median =
          stats::median(
            value,
            na.rm = TRUE
          ),

        quartile_1 =
          stats::quantile(
            value,
            0.25,
            na.rm = TRUE,
            names = FALSE
          ),

        quartile_3 =
          stats::quantile(
            value,
            0.75,
            na.rm = TRUE,
            names = FALSE
          ),

        .groups =
          "drop"
      ) %>%
      dplyr::mutate(
        design_id =
          design_id,

        analysis_id =
          analysis_id,

        .before =
          1L
      )
  }
)


coagulation_dependent_summary <- purrr::pmap_dfr(
  sofa_dataset_index,

  function(
    design_id,
    analysis_id,
    data
  ) {

    sepsis_data <-
      data %>%
      dplyr::filter(
        treatment_sepsis == 1L
      )


    tibble::tibble(
      design_id =
        design_id,

      analysis_id =
        analysis_id,

      sepsis_n =
        nrow(
          sepsis_data
        ),

      coagulation_dependent_sepsis_n =
        sum(
          sepsis_data$
            sepsis_coagulation_dependent_flag ==
            1L
        ),

      coagulation_dependent_sepsis_percent =
        100 *
          coagulation_dependent_sepsis_n /
          sepsis_n
    )
  }
)


# ==============================================================================
# 14. SINGLE-MARKER AUROC ANALYSIS
# ==============================================================================

ROC_CONFIDENCE_LEVEL <- 0.95

roc_objects <- list()

roc_result_rows <- list()

roc_row_index <- 1L


for (
  dataset_index in
  seq_len(
    nrow(
      analysis_dataset_index
    )
  )
) {

  design_id <-
    analysis_dataset_index$
      design_id[
      dataset_index
    ]


  analysis_id <-
    analysis_dataset_index$
      analysis_id[
      dataset_index
    ]


  analysis_data <-
    analysis_dataset_index$
      data[[
      dataset_index
    ]]


  for (
    biomarker_index in
    seq_len(
      nrow(
        biomarker_registry
      )
    )
  ) {

    marker <-
      biomarker_registry[
        biomarker_index,
      ]


    marker_value <-
      analysis_data[[
        marker$
          value_variable
      ]]


    marker_score <-
      marker_value *
        marker$
          score_multiplier


    roc_object <- pROC::roc(
      response =
        analysis_data$
          treatment_sepsis,

      predictor =
        marker_score,

      levels = c(
        0,
        1
      ),

      direction =
        "<",

      auc =
        TRUE,

      quiet =
        TRUE
    )


    auc_value <- as.numeric(
      pROC::auc(
        roc_object
      )
    )


    auc_interval <- as.numeric(
      pROC::ci.auc(
        roc_object,
        conf.level =
          ROC_CONFIDENCE_LEVEL,
        method =
          "delong"
      )
    )


    object_key <- paste(
      design_id,
      analysis_id,
      marker$
        biomarker_id,
      sep = "__"
    )


    roc_objects[[
      object_key
    ]] <- roc_object


    roc_result_rows[[
      roc_row_index
    ]] <- tibble::tibble(
      design_id =
        design_id,

      analysis_id =
        analysis_id,

      biomarker_id =
        marker$
          biomarker_id,

      biomarker_label =
        marker$
          biomarker_label,

      marker_class =
        marker$
          marker_class,

      clinical_direction =
        marker$
          clinical_direction,

      total_n =
        nrow(
          analysis_data
        ),

      comparator_n =
        sum(
          analysis_data$
            treatment_sepsis ==
            0L
        ),

      sepsis_n =
        sum(
          analysis_data$
            treatment_sepsis ==
            1L
        ),

      auc =
        auc_value,

      auc_ci95_lower =
        auc_interval[
          1L
        ],

      auc_ci95_upper =
        auc_interval[
          3L
        ],

      auc_ci95 =
        format_ci(
          auc_value,
          auc_interval[
            1L
          ],
          auc_interval[
            3L
          ]
        )
    )


    roc_row_index <-
      roc_row_index +
      1L
  }
}


roc_auc_results <- dplyr::bind_rows(
  roc_result_rows
) %>%
  dplyr::group_by(
    design_id,
    analysis_id
  ) %>%
  dplyr::arrange(
    dplyr::desc(
      auc
    ),
    .by_group = TRUE
  ) %>%
  dplyr::mutate(
    auc_rank =
      dplyr::row_number()
  ) %>%
  dplyr::ungroup()


# ==============================================================================
# 15. PAIRED DELONG COMPARISONS
# ==============================================================================

marker_pairs <- utils::combn(
  biomarker_registry$
    biomarker_id,
  2L,
  simplify = FALSE
)


pairwise_result_rows <- list()

pairwise_row_index <- 1L


for (
  dataset_index in
  seq_len(
    nrow(
      analysis_dataset_index
    )
  )
) {

  design_id <-
    analysis_dataset_index$
      design_id[
      dataset_index
    ]


  analysis_id <-
    analysis_dataset_index$
      analysis_id[
      dataset_index
    ]


  for (
    marker_pair in
    marker_pairs
  ) {

    biomarker_1 <-
      marker_pair[
        1L
      ]


    biomarker_2 <-
      marker_pair[
        2L
      ]


    roc_1 <- roc_objects[[
      paste(
        design_id,
        analysis_id,
        biomarker_1,
        sep = "__"
      )
    ]]


    roc_2 <- roc_objects[[
      paste(
        design_id,
        analysis_id,
        biomarker_2,
        sep = "__"
      )
    ]]


    delong_test <- pROC::roc.test(
      roc_1,
      roc_2,
      paired = TRUE,
      method = "delong",
      alternative = "two.sided",
      conf.level =
        ROC_CONFIDENCE_LEVEL
    )


    auc_1 <- as.numeric(
      pROC::auc(
        roc_1
      )
    )


    auc_2 <- as.numeric(
      pROC::auc(
        roc_2
      )
    )


    auc_difference <-
      auc_1 -
      auc_2


    variance_1 <- as.numeric(
      pROC::var(
        roc_1,
        method = "delong"
      )
    )


    variance_2 <- as.numeric(
      pROC::var(
        roc_2,
        method = "delong"
      )
    )


    covariance_12 <- as.numeric(
      pROC::cov(
        roc_1,
        roc_2,
        method = "delong"
      )
    )


    difference_variance <-
      variance_1 +
      variance_2 -
      2 *
        covariance_12


    if (
      difference_variance < 0 &&
        abs(
          difference_variance
        ) <=
          1e-12
    ) {
      difference_variance <- 0
    }


    if (
      difference_variance < 0
    ) {
      stop(
        paste0(
          "Negative DeLong variance for ",
          design_id,
          ", analysis ",
          analysis_id,
          ", ",
          biomarker_1,
          " versus ",
          biomarker_2,
          "."
        ),
        call. = FALSE
      )
    }


    difference_standard_error <- sqrt(
      difference_variance
    )


    critical_value <- stats::qnorm(
      1 -
        (
          1 -
            ROC_CONFIDENCE_LEVEL
        ) /
          2
    )


    difference_lower <-
      auc_difference -
      critical_value *
        difference_standard_error


    difference_upper <-
      auc_difference +
      critical_value *
        difference_standard_error


    label_1 <-
      biomarker_registry$
        biomarker_label[
        match(
          biomarker_1,
          biomarker_registry$
            biomarker_id
        )
      ]


    label_2 <-
      biomarker_registry$
        biomarker_label[
        match(
          biomarker_2,
          biomarker_registry$
            biomarker_id
        )
      ]


    pairwise_result_rows[[
      pairwise_row_index
    ]] <- tibble::tibble(
      design_id =
        design_id,

      analysis_id =
        analysis_id,

      biomarker_1_id =
        biomarker_1,

      biomarker_1_label =
        label_1,

      biomarker_2_id =
        biomarker_2,

      biomarker_2_label =
        label_2,

      auc_1 =
        auc_1,

      auc_2 =
        auc_2,

      auc_difference =
        auc_difference,

      auc_difference_ci95_lower =
        difference_lower,

      auc_difference_ci95_upper =
        difference_upper,

      auc_difference_ci95 =
        format_ci(
          auc_difference,
          difference_lower,
          difference_upper
        ),

      p_value =
        as.numeric(
          delong_test$
            p.value
        )
    )


    pairwise_row_index <-
      pairwise_row_index +
      1L
  }
}


roc_pairwise_delong_results <- dplyr::bind_rows(
  pairwise_result_rows
) %>%
  dplyr::group_by(
    design_id,
    analysis_id
  ) %>%
  dplyr::mutate(
    p_value_holm =
      stats::p.adjust(
        p_value,
        method = "holm"
      )
  ) %>%
  dplyr::ungroup()


roc_pairwise_vs_crp <- roc_pairwise_delong_results %>%
  dplyr::filter(
    biomarker_1_id == "crp" |
      biomarker_2_id == "crp"
  ) %>%
  dplyr::mutate(
    other_biomarker_id =
      dplyr::if_else(
        biomarker_1_id == "crp",
        biomarker_2_id,
        biomarker_1_id
      ),

    other_biomarker_label =
      dplyr::if_else(
        biomarker_1_id == "crp",
        biomarker_2_label,
        biomarker_1_label
      ),

    crp_auc =
      dplyr::if_else(
        biomarker_1_id == "crp",
        auc_1,
        auc_2
      ),

    other_auc =
      dplyr::if_else(
        biomarker_1_id == "crp",
        auc_2,
        auc_1
      ),

    crp_minus_other_auc =
      crp_auc -
        other_auc
  )



# ==============================================================================
# 15.5. CROSS-SCENARIO AUROC DIFFERENCES
# PARTIALLY PAIRED U-STATISTIC
# ==============================================================================
#
# Estimand:
#
#   Delta AUROC = AUROC_A - AUROC_B
#
# Analysis A:
#   Sepsis vs matched noninfectious comparator.
#
# Analysis B:
#   Sepsis vs matched infection-without-sepsis comparator.
#
# Dependence structure:
#   - The same sepsis episodes are used in A and B.
#   - Comparator samples are distinct.
#
# Primary cross-scenario inference:
#   Partially paired U-statistic / structural-component variance.
#
# Multiplicity:
#   Holm family-wise error rate adjustment across the 11 biomarkers.
#
# Matching is treated as fixed. No rematching is performed.
# ==============================================================================


CROSS_SCENARIO_EXPECTED_SEPSIS_N <- 525L
CROSS_SCENARIO_EXPECTED_COMPARATOR_A_N <- 214L
CROSS_SCENARIO_EXPECTED_COMPARATOR_B_N <- 315L

CROSS_SCENARIO_AUC_TOLERANCE <- 1e-12
CROSS_SCENARIO_SCORE_TOLERANCE <- 1e-12

# ------------------------------------------------------------------------------
# Prepare the three samples
# ------------------------------------------------------------------------------


cross_scenario_sepsis_A <- analysis_A_final_data %>%
  dplyr::filter(
    treatment_sepsis ==
      1L
  ) %>%
  dplyr::mutate(
    episode_id_key =
      as.character(
        episode_id
      )
  ) %>%
  dplyr::arrange(
    episode_id_key
  )


cross_scenario_sepsis_B <- analysis_B_final_data %>%
  dplyr::filter(
    treatment_sepsis ==
      1L
  ) %>%
  dplyr::mutate(
    episode_id_key =
      as.character(
        episode_id
      )
  ) %>%
  dplyr::arrange(
    episode_id_key
  )


cross_scenario_comparator_A <- analysis_A_final_data %>%
  dplyr::filter(
    treatment_sepsis ==
      0L
  ) %>%
  dplyr::mutate(
    episode_id_key =
      as.character(
        episode_id
      )
  )


cross_scenario_comparator_B <- analysis_B_final_data %>%
  dplyr::filter(
    treatment_sepsis ==
      0L
  ) %>%
  dplyr::mutate(
    episode_id_key =
      as.character(
        episode_id
      )
  )


if (
  nrow(
    cross_scenario_sepsis_A
  ) !=
  CROSS_SCENARIO_EXPECTED_SEPSIS_N ||
  nrow(
    cross_scenario_sepsis_B
  ) !=
  CROSS_SCENARIO_EXPECTED_SEPSIS_N
) {
  stop(
    "Unexpected sepsis sample size in the cross-scenario analysis.",
    call. = FALSE
  )
}


if (
  nrow(
    cross_scenario_comparator_A
  ) !=
  CROSS_SCENARIO_EXPECTED_COMPARATOR_A_N
) {
  stop(
    "Unexpected Analysis A comparator sample size in the cross-scenario analysis.",
    call. = FALSE
  )
}


if (
  nrow(
    cross_scenario_comparator_B
  ) !=
  CROSS_SCENARIO_EXPECTED_COMPARATOR_B_N
) {
  stop(
    "Unexpected Analysis B comparator sample size in the cross-scenario analysis.",
    call. = FALSE
  )
}


if (
  !identical(
    cross_scenario_sepsis_A$
    episode_id_key,
    
    cross_scenario_sepsis_B$
    episode_id_key
  )
) {
  stop(
    paste(
      "The cross-scenario analysis requires exactly the same",
      "ordered sepsis episode set in Analyses A and B."
    ),
    call. = FALSE
  )
}


cross_scenario_comparator_overlap <-
  intersect(
    cross_scenario_comparator_A$
      episode_id_key,
    
    cross_scenario_comparator_B$
      episode_id_key
  )


if (
  length(
    cross_scenario_comparator_overlap
  ) >
  0L
) {
  stop(
    paste(
      "Unexpected comparator overlap was detected between",
      "Analyses A and B."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# Empirical AUROC kernel
# ------------------------------------------------------------------------------


cross_scenario_auc_kernel <- function(
    sepsis_score,
    comparator_score
) {
  
  if (
    anyNA(
      sepsis_score
    ) ||
    anyNA(
      comparator_score
    ) ||
    any(
      !is.finite(
        sepsis_score
      )
    ) ||
    any(
      !is.finite(
        comparator_score
      )
    )
  ) {
    stop(
      "Missing or non-finite values were passed to the AUROC kernel.",
      call. = FALSE
    )
  }
  
  
  outer(
    sepsis_score,
    comparator_score,
    
    FUN = function(
    x,
    y
    ) {
      
      (x > y) +
        0.5 *
        (x == y)
    }
  )
}


# ------------------------------------------------------------------------------
# Partially paired U-statistic
# ------------------------------------------------------------------------------


calculate_cross_scenario_ustat <- function(
    kernel_A,
    kernel_B
) {
  
  sepsis_n <-
    nrow(
      kernel_A
    )
  
  comparator_A_n <-
    ncol(
      kernel_A
    )
  
  comparator_B_n <-
    ncol(
      kernel_B
    )
  
  
  if (
    nrow(
      kernel_B
    ) !=
    sepsis_n
  ) {
    stop(
      "The two cross-scenario kernels do not share the same sepsis sample size.",
      call. = FALSE
    )
  }
  
  
  # Structural components for the shared sepsis episodes.
  
  V_A <-
    rowMeans(
      kernel_A
    )
  
  V_B <-
    rowMeans(
      kernel_B
    )
  
  
  # Structural components for the distinct comparator samples.
  
  W_A <-
    colMeans(
      kernel_A
    )
  
  W_B <-
    colMeans(
      kernel_B
    )
  
  
  auc_A <-
    mean(
      V_A
    )
  
  auc_B <-
    mean(
      V_B
    )
  
  delta_auc <-
    auc_A -
    auc_B
  
  
  variance_auc_A <-
    stats::var(
      V_A
    ) /
    sepsis_n +
    stats::var(
      W_A
    ) /
    comparator_A_n
  
  
  variance_auc_B <-
    stats::var(
      V_B
    ) /
    sepsis_n +
    stats::var(
      W_B
    ) /
    comparator_B_n
  
  
  # Cross-scenario covariance is contributed by the common sepsis sample.
  # The comparator samples are distinct.
  
  covariance_auc_A_B <-
    stats::cov(
      V_A,
      V_B
    ) /
    sepsis_n
  
  
  variance_delta <-
    variance_auc_A +
    variance_auc_B -
    2 *
    covariance_auc_A_B
  
  
  # Independent algebraic implementation check.
  
  variance_delta_alternative <-
    stats::var(
      V_A -
        V_B
    ) /
    sepsis_n +
    stats::var(
      W_A
    ) /
    comparator_A_n +
    stats::var(
      W_B
    ) /
    comparator_B_n
  
  
  if (
    !isTRUE(
      all.equal(
        variance_delta,
        variance_delta_alternative,
        tolerance = 1e-12
      )
    )
  ) {
    stop(
      "The two U-statistic variance formulations do not agree.",
      call. = FALSE
    )
  }
  
  
  if (
    !is.finite(
      variance_delta
    ) ||
    variance_delta <=
    0
  ) {
    stop(
      "Invalid cross-scenario Delta AUROC variance.",
      call. = FALSE
    )
  }
  
  
  se_delta <-
    sqrt(
      variance_delta
    )
  
  
  z_value <-
    delta_auc /
    se_delta
  
  
  p_value <-
    2 *
    stats::pnorm(
      abs(
        z_value
      ),
      lower.tail = FALSE
    )
  
  
  alpha <-
    1 -
    ROC_CONFIDENCE_LEVEL
  
  
  z_critical <-
    stats::qnorm(
      1 -
        alpha /
        2
    )
  
  
  ci95_lower <-
    delta_auc -
    z_critical *
    se_delta
  
  
  ci95_upper <-
    delta_auc +
    z_critical *
    se_delta
  
  
  list(
    auc_A =
      auc_A,
    
    auc_B =
      auc_B,
    
    delta_auc =
      delta_auc,
    
    variance_auc_A =
      variance_auc_A,
    
    variance_auc_B =
      variance_auc_B,
    
    covariance_auc_A_B =
      covariance_auc_A_B,
    
    variance_delta =
      variance_delta,
    
    se_delta =
      se_delta,
    
    z_value =
      z_value,
    
    p_value =
      p_value,
    
    ci95_lower =
      ci95_lower,
    
    ci95_upper =
      ci95_upper
  )
}


# ------------------------------------------------------------------------------
# Calculate the 11 cross-scenario comparisons
# ------------------------------------------------------------------------------


cross_scenario_kernel_A <- vector(
  "list",
  nrow(
    biomarker_registry
  )
)

cross_scenario_kernel_B <- vector(
  "list",
  nrow(
    biomarker_registry
  )
)


names(
  cross_scenario_kernel_A
) <-
  biomarker_registry$
  biomarker_id


names(
  cross_scenario_kernel_B
) <-
  biomarker_registry$
  biomarker_id


cross_scenario_result_rows <- vector(
  "list",
  nrow(
    biomarker_registry
  )
)


for (
  biomarker_index in
  seq_len(
    nrow(
      biomarker_registry
    )
  )
) {
  
  marker <-
    biomarker_registry[
      biomarker_index,
    ]
  
  
  biomarker_id <-
    marker$
    biomarker_id[[
      1L
    ]]
  
  
  value_variable <-
    marker$
    value_variable[[
      1L
    ]]
  
  
  score_multiplier <-
    as.numeric(
      marker$
        score_multiplier[[
          1L
        ]]
    )
  
  
  if (
    !value_variable %in%
    names(
      analysis_A_final_data
    ) ||
    !value_variable %in%
    names(
      analysis_B_final_data
    )
  ) {
    stop(
      paste0(
        "Missing cross-scenario biomarker variable: ",
        value_variable,
        "."
      ),
      call. = FALSE
    )
  }
  
  
  sepsis_score_A <-
    as.numeric(
      cross_scenario_sepsis_A[[
        value_variable
      ]]
    ) *
    score_multiplier
  
  
  sepsis_score_B <-
    as.numeric(
      cross_scenario_sepsis_B[[
        value_variable
      ]]
    ) *
    score_multiplier
  
  
  comparator_score_A <-
    as.numeric(
      cross_scenario_comparator_A[[
        value_variable
      ]]
    ) *
    score_multiplier
  
  
  comparator_score_B <-
    as.numeric(
      cross_scenario_comparator_B[[
        value_variable
      ]]
    ) *
    score_multiplier
  
  
  if (
    anyNA(
      c(
        sepsis_score_A,
        sepsis_score_B,
        comparator_score_A,
        comparator_score_B
      )
    ) ||
    any(
      !is.finite(
        c(
          sepsis_score_A,
          sepsis_score_B,
          comparator_score_A,
          comparator_score_B
        )
      )
    )
  ) {
    stop(
      paste0(
        "Missing or non-finite cross-scenario biomarker values for ",
        biomarker_id,
        "."
      ),
      call. = FALSE
    )
  }
  
  
  if (
    !isTRUE(
      all.equal(
        sepsis_score_A,
        sepsis_score_B,
        tolerance =
        CROSS_SCENARIO_SCORE_TOLERANCE,
        check.attributes = FALSE
      )
    )
  ) {
    stop(
      paste0(
        "Shared sepsis scores differ between A and B for ",
        biomarker_id,
        "."
      ),
      call. = FALSE
    )
  }
  
  
  kernel_A <-
    cross_scenario_auc_kernel(
      sepsis_score =
        sepsis_score_A,
      
      comparator_score =
        comparator_score_A
    )
  
  
  kernel_B <-
    cross_scenario_auc_kernel(
      sepsis_score =
        sepsis_score_A,
      
      comparator_score =
        comparator_score_B
    )
  
  
  cross_scenario_kernel_A[[
    biomarker_id
  ]] <-
    kernel_A
  
  
  cross_scenario_kernel_B[[
    biomarker_id
  ]] <-
    kernel_B
  
  
  estimate <-
    calculate_cross_scenario_ustat(
      kernel_A =
        kernel_A,
      
      kernel_B =
        kernel_B
    )
  
  
  primary_reference_A <-
    roc_auc_results %>%
    dplyr::filter(
      .data$design_id == "matched",
      .data$analysis_id == "A",
      .data$biomarker_id == .env$biomarker_id
    )
  
  
  primary_reference_B <-
    roc_auc_results %>%
    dplyr::filter(
      .data$design_id == "matched",
      .data$analysis_id == "B",
      .data$biomarker_id == .env$biomarker_id
    )
  
  
  if (
    nrow(
      primary_reference_A
    ) !=
    1L ||
    nrow(
      primary_reference_B
    ) !=
    1L
  ) {
    stop(
      paste0(
        "Primary matched AUROC reference was not uniquely identified for ",
        biomarker_id,
        "."
      ),
      call. = FALSE
    )
  }
  
  
  primary_auc_A_reproduced <-
    abs(
      estimate$
        auc_A -
        primary_reference_A$
        auc
    ) <=
    CROSS_SCENARIO_AUC_TOLERANCE
  
  
  primary_auc_B_reproduced <-
    abs(
      estimate$
        auc_B -
        primary_reference_B$
        auc
    ) <=
    CROSS_SCENARIO_AUC_TOLERANCE
  
  
  if (
    !primary_auc_A_reproduced ||
    !primary_auc_B_reproduced
  ) {
    stop(
      paste0(
        "Cross-scenario U-statistic does not reproduce the validated primary AUROC for ",
        biomarker_id,
        "."
      ),
      call. = FALSE
    )
  }
  
  
  cross_scenario_result_rows[[
    biomarker_index
  ]] <-
    tibble::tibble(
      biomarker_id =
        biomarker_id,
      
      biomarker_label =
        marker$
        biomarker_label[[
          1L
        ]],
      
      marker_class =
        marker$
        marker_class[[
          1L
        ]],
      
      clinical_direction =
        marker$
        clinical_direction[[
          1L
        ]],
      
      sepsis_n =
        nrow(
          cross_scenario_sepsis_A
        ),
      
      comparator_A_n =
        nrow(
          cross_scenario_comparator_A
        ),
      
      comparator_B_n =
        nrow(
          cross_scenario_comparator_B
        ),
      
      auc_A =
        estimate$
        auc_A,
      
      auc_B =
        estimate$
        auc_B,
      
      delta_auc =
        estimate$
        delta_auc,
      
      se_delta_auc =
        estimate$
        se_delta,
      
      delta_auc_ci95_lower =
        estimate$
        ci95_lower,
      
      delta_auc_ci95_upper =
        estimate$
        ci95_upper,
      
      delta_auc_ci95 =
        sprintf(
          "%.3f [%.3f, %.3f]",
          estimate$
            delta_auc,
          estimate$
            ci95_lower,
          estimate$
            ci95_upper
        ),
      
      z_value =
        estimate$
        z_value,
      
      p_value =
        estimate$
        p_value,
      
      variance_delta =
        estimate$
        variance_delta,
      
      covariance_auc_A_B =
        estimate$
        covariance_auc_A_B,
      
      primary_auc_A_reproduced =
        primary_auc_A_reproduced,
      
      primary_auc_B_reproduced =
        primary_auc_B_reproduced
    )
}


cross_scenario_auroc_results <-
  dplyr::bind_rows(
    cross_scenario_result_rows
  ) %>%
  dplyr::mutate(
    p_value_holm =
      stats::p.adjust(
        p_value,
        method = "holm"
      ),
    
    holm_fwer_reject_0_05 =
      p_value_holm <
      0.05
  )


# Publication-facing result object.
# Bootstrap information is intentionally excluded.

cross_scenario_auroc_results_export <-
  cross_scenario_auroc_results %>%
  dplyr::select(
    biomarker_id,
    biomarker_label,
    marker_class,
    clinical_direction,
    sepsis_n,
    comparator_A_n,
    comparator_B_n,
    auc_A,
    auc_B,
    delta_auc,
    se_delta_auc,
    delta_auc_ci95_lower,
    delta_auc_ci95_upper,
    delta_auc_ci95,
    z_value,
    p_value,
    p_value_holm,
    holm_fwer_reject_0_05
  )


# ==============================================================================
# CROSS-SCENARIO VALIDATION CHECKS
# ==============================================================================


cross_scenario_validation_checks <- tibble::tibble(
  check = c(
    "Exactly 11 cross-scenario biomarker comparisons",
    "Expected shared sepsis sample and identical episode set",
    "Expected Analysis A and B comparator sample sizes",
    "No overlap between the two comparator samples",
    "All Analysis A primary AUROCs reproduced",
    "All Analysis B primary AUROCs reproduced",
    "All Delta AUROC variances are finite and positive",
    "All raw and Holm-adjusted P values are valid",
    "Holm-adjusted P values are not smaller than raw P values"
  ),
  
  pass = c(
    nrow(
      cross_scenario_auroc_results
    ) ==
      11L,
    
    nrow(
      cross_scenario_sepsis_A
    ) ==
      CROSS_SCENARIO_EXPECTED_SEPSIS_N &&
      identical(
        cross_scenario_sepsis_A$
          episode_id_key,
        cross_scenario_sepsis_B$
          episode_id_key
      ),
    
    nrow(
      cross_scenario_comparator_A
    ) ==
      CROSS_SCENARIO_EXPECTED_COMPARATOR_A_N &&
      nrow(
        cross_scenario_comparator_B
      ) ==
      CROSS_SCENARIO_EXPECTED_COMPARATOR_B_N,
    
    length(
      cross_scenario_comparator_overlap
    ) ==
      0L,
    
    all(
      cross_scenario_auroc_results$
        primary_auc_A_reproduced
    ),
    
    all(
      cross_scenario_auroc_results$
        primary_auc_B_reproduced
    ),
    
    all(
      is.finite(
        cross_scenario_auroc_results$
          variance_delta
      ) &
        cross_scenario_auroc_results$
        variance_delta >
        0
    ),
    
    all(
      is.finite(
        cross_scenario_auroc_results$
          p_value
      ) &
        cross_scenario_auroc_results$
        p_value >=
        0 &
        cross_scenario_auroc_results$
        p_value <=
        1
    ) &&
      all(
        is.finite(
          cross_scenario_auroc_results$
            p_value_holm
        ) &
          cross_scenario_auroc_results$
          p_value_holm >=
          0 &
          cross_scenario_auroc_results$
          p_value_holm <=
          1
      ),
    
    all(
      cross_scenario_auroc_results$
        p_value_holm +
        1e-15 >=
        cross_scenario_auroc_results$
        p_value
    )
  )
)


cat(
  "\nCROSS-SCENARIO AUROC VALIDATION CHECKS\n"
)


print(
  cross_scenario_validation_checks,
  n = Inf,
  width = Inf
)


if (
  anyNA(
    cross_scenario_validation_checks$
    pass
  ) ||
  any(
    !cross_scenario_validation_checks$
    pass
  )
) {
  
  stop(
    "At least one cross-scenario AUROC validation check failed.",
    call. = FALSE
  )
}


cat(
  "\nCROSS-SCENARIO AUROC DIFFERENCES — PARTIALLY PAIRED U-STATISTIC\n"
)


print(
  cross_scenario_auroc_results_export %>%
    dplyr::arrange(
      dplyr::desc(
        delta_auc
      )
    ),
  n = Inf,
  width = Inf
)

# ==============================================================================
# 16. DESCRIPTIVE COMPARISON OF PRIMARY AND SENSITIVITY AUROCS
# ==============================================================================

matched_vs_unmatched_auc <- roc_auc_results %>%
  dplyr::filter(
    design_id %in% c(
      "matched",
      "unmatched"
    )
  ) %>%
  dplyr::select(
    design_id,
    analysis_id,
    biomarker_id,
    biomarker_label,
    auc
  ) %>%
  tidyr::pivot_wider(
    names_from =
      design_id,

    values_from =
      auc
  ) %>%
  dplyr::mutate(
    unmatched_minus_matched_auc =
      unmatched -
        matched
  )


matched_vs_coagulation_sensitivity_auc <-
  roc_auc_results %>%
  dplyr::filter(
    design_id %in% c(
      "matched",
      "coagulation_sensitivity"
    )
  ) %>%
  dplyr::select(
    design_id,
    analysis_id,
    biomarker_id,
    biomarker_label,
    auc
  ) %>%
  tidyr::pivot_wider(
    names_from =
      design_id,

    values_from =
      auc
  ) %>%
  dplyr::mutate(
    sensitivity_minus_matched_auc =
      coagulation_sensitivity -
        matched
  )


# ==============================================================================
# 19. PARTICIPANT-FLOW COUNTS
# ==============================================================================

participant_flow <- dplyr::bind_rows(
  analysis_final_base %>%
    dplyr::count(
      primary_phenotype_group_revised,
      name = "episodes"
    ) %>%
    dplyr::transmute(
      stage =
        "analytical_cohort",

      analysis_id =
        NA_character_,

      group =
        as.character(
          primary_phenotype_group_revised
        ),

      episodes =
        episodes
    ),

  matching_sample_sizes %>%
    dplyr::transmute(
      stage =
        design,

      analysis_id =
        analysis_id,

      group =
        group,

      episodes =
        episodes
    ),

  analysis_A_coagulation_sensitivity %>%
    dplyr::count(
      treatment_sepsis,
      name = "episodes"
    ) %>%
    dplyr::transmute(
      stage =
        "coagulation_sensitivity",

      analysis_id =
        "A",

      group =
        dplyr::if_else(
          treatment_sepsis ==
            1L,
          "Sepsis",
          "Control"
        ),

      episodes =
        episodes
    ),

  analysis_B_coagulation_sensitivity %>%
    dplyr::count(
      treatment_sepsis,
      name = "episodes"
    ) %>%
    dplyr::transmute(
      stage =
        "coagulation_sensitivity",

      analysis_id =
        "B",

      group =
        dplyr::if_else(
          treatment_sepsis ==
            1L,
          "Sepsis",
          "Infection without sepsis"
        ),

      episodes =
        episodes
    )
)


# ==============================================================================
# 20. FINAL INTEGRITY CHECKS
# ==============================================================================
final_integrity_check <- tibble::tribble(
  ~check_name,
  ~validation_pass,

  "unique_episode_id",
  anyDuplicated(
    analysis_final_base$
      episode_id
  ) == 0L,

  "unique_subject_id",
  anyDuplicated(
    analysis_final_base$
      subject_id
  ) == 0L,

  "same_sepsis_source_A_B",
  setequal(
    sepsis_episode_ids_A,
    sepsis_episode_ids_B
  ),

  "same_sepsis_matched_A_B",
  setequal(
    analysis_A_final_data$
      episode_id[
      analysis_A_final_data$
        treatment_sepsis ==
        1L
    ],

    analysis_B_final_data$
      episode_id[
      analysis_B_final_data$
        treatment_sepsis ==
        1L
    ]
  ),

  "analysis_A_balance_below_0_10",
  nrow(
    balance_issues_A
  ) == 0L,

  "analysis_B_balance_below_0_10",
  nrow(
    balance_issues_B
  ) == 0L,

  "eleven_biomarkers_registered",
  nrow(
    biomarker_registry
  ) == 11L,

  "NMR_registered",
  "nmr" %in%
    biomarker_registry$
      biomarker_id,

  "all_biomarkers_available",
  all(
    biomarker_availability$
      available_n ==
      biomarker_availability$
        total_n
  ),

  "composite_formulas_agree",
  all(
    biomarker_formula_check$
      disagreement_n ==
      0L
  ),

  "matched_roc_curve_count",
  nrow(
    roc_auc_results %>%
      dplyr::filter(
        design_id ==
          "matched"
      )
  ) == 22L,

  "unmatched_roc_curve_count",
  nrow(
    roc_auc_results %>%
      dplyr::filter(
        design_id ==
          "unmatched"
      )
  ) == 22L,

  "coagulation_sensitivity_roc_curve_count",
  nrow(
    roc_auc_results %>%
      dplyr::filter(
        design_id ==
          "coagulation_sensitivity"
      )
  ) == 22L,

  "matched_pairwise_comparison_count",
  nrow(
    roc_pairwise_delong_results %>%
      dplyr::filter(
        design_id ==
          "matched"
      )
  ) == 110L,

  "unmatched_pairwise_comparison_count",
  nrow(
    roc_pairwise_delong_results %>%
      dplyr::filter(
        design_id ==
          "unmatched"
      )
  ) == 110L,

  "coagulation_sensitivity_pairwise_count",
  nrow(
    roc_pairwise_delong_results %>%
      dplyr::filter(
        design_id ==
          "coagulation_sensitivity"
      )
  ) == 110L,

  "matched_CRP_comparison_count",
  nrow(
    roc_pairwise_vs_crp %>%
      dplyr::filter(
        design_id ==
          "matched"
      )
  ) == 20L
)


print(
  final_integrity_check,
  n = Inf,
  width = Inf
)


if (
  any(
    !final_integrity_check$
      validation_pass
  )
) {
  stop(
    "The final analysis failed one or more integrity checks.",
    call. = FALSE
  )
}

# ==============================================================================
# 20.1. MICROBIOLOGY DESCRIPTIVE ANALYSIS
# ==============================================================================


## Required variables


if (
  !exists(
    "analysis_B_final_data",
    inherits = TRUE
  )
) {
  stop(
    "Object 'analysis_B_final_data' was not found.",
    call. = FALSE
  )
}


required_microbiology_variables <- c(
  "episode_id",
  "treatment_sepsis",
  "infection_microbiology_qualifying_specimen_n",
  "infection_microbiology_distinct_study_n",
  "infection_microbiology_any_positive_flag",
  "infection_microbiology_positive_results_compact",
  "blood_culture_obtained_flag",
  "blood_culture_valid_positive_flag"
)


missing_microbiology_variables <- setdiff(
  required_microbiology_variables,
  names(
    analysis_B_final_data
  )
)


if (
  length(
    missing_microbiology_variables
  ) > 0L
) {
  stop(
    paste0(
      "Missing microbiology variable(s): ",
      paste(
        missing_microbiology_variables,
        collapse = ", "
      ),
      "."
    ),
    call. = FALSE
  )
}


## Matched population


microbiology_matched_data <- analysis_B_final_data %>%
  dplyr::mutate(
    episode_id =
      as.character(
        episode_id
      ),

    treatment_sepsis_numeric =
      suppressWarnings(
        as.integer(
          as.character(
            treatment_sepsis
          )
        )
      ),

    microbiology_group =
      dplyr::case_when(
        treatment_sepsis_numeric == 0L ~
          "Infection without sepsis",

        treatment_sepsis_numeric == 1L ~
          "Sepsis",

        TRUE ~
          NA_character_
      ),

    microbiology_group =
      factor(
        microbiology_group,
        levels = c(
          "Infection without sepsis",
          "Sepsis"
        )
      )
  )


if (
  anyNA(
    microbiology_matched_data$
      microbiology_group
  )
) {
  stop(
    "At least one episode could not be assigned to a microbiology group.",
    call. = FALSE
  )
}


if (
  anyDuplicated(
    microbiology_matched_data$
      episode_id
  ) > 0L
) {
  stop(
    "Duplicated episode identifiers were found in 'analysis_B_final_data'.",
    call. = FALSE
  )
}


## Parse reported microbiology findings


positive_result_tokens <- microbiology_matched_data %>%
  dplyr::filter(
    infection_microbiology_any_positive_flag == 1L,
    !is.na(
      infection_microbiology_positive_results_compact
    ),
    stringr::str_squish(
      infection_microbiology_positive_results_compact
    ) != ""
  ) %>%
  dplyr::transmute(
    episode_id,
    microbiology_group,

    positive_result_label =
      stringr::str_split(
        infection_microbiology_positive_results_compact,
        stringr::fixed(
          " || "
        )
      )
  ) %>%
  tidyr::unnest_longer(
    positive_result_label
  ) %>%
  dplyr::mutate(
    positive_result_label =
      stringr::str_squish(
        positive_result_label
      ),

    organism_name_source =
      stringr::str_extract(
        positive_result_label,
        "(?<= => ).+$"
      ),

    study_part =
      stringr::str_remove(
        positive_result_label,
        " => .+$"
      ),

    specimen_type =
      stringr::str_split_fixed(
        study_part,
        stringr::fixed(
          " :: "
        ),
        2
      )[, 1],

    test_name =
      stringr::str_split_fixed(
        study_part,
        stringr::fixed(
          " :: "
        ),
        2
      )[, 2],

    organism_name_source =
      stringr::str_squish(
        organism_name_source
      ),

    specimen_type =
      stringr::str_squish(
        specimen_type
      ),

    test_name =
      stringr::str_squish(
        test_name
      ),

    organism_name_normalized =
      organism_name_source %>%
        stringr::str_squish() %>%
        stringr::str_to_upper()
  ) %>%
  dplyr::distinct(
    episode_id,
    microbiology_group,
    specimen_type,
    test_name,
    organism_name_source,
    organism_name_normalized
  )


positive_result_parsing_check <- positive_result_tokens %>%
  dplyr::summarise(
    parsed_rows =
      dplyr::n(),

    missing_specimen_type =
      sum(
        is.na(
          specimen_type
        ) |
          specimen_type == ""
      ),

    missing_test_name =
      sum(
        is.na(
          test_name
        ) |
          test_name == ""
      ),

    missing_finding_name =
      sum(
        is.na(
          organism_name_source
        ) |
          organism_name_source == ""
      )
  )


if (
  any(
    positive_result_parsing_check[
      c(
        "missing_specimen_type",
        "missing_test_name",
        "missing_finding_name"
      )
    ] > 0L
  )
) {
  stop(
    "At least one microbiology result could not be parsed.",
    call. = FALSE
  )
}


## Administrative labels


administrative_microbiology_labels <- c(
  "CANCELLED"
)


validated_positive_result_tokens <- positive_result_tokens %>%
  dplyr::filter(
    !organism_name_normalized %in%
      administrative_microbiology_labels
  )


## Finding categories


microbiology_finding_dictionary <- tibble::tribble(
  ~finding_key,
  ~finding_display_label,

  "STAPH AUREUS COAG +",
  "Staphylococcus aureus",

  "MIXED BACTERIAL FLORA",
  "Mixed bacterial microbiota",

  "ESCHERICHIA COLI",
  "Escherichia coli",

  "COAGULASE_NEGATIVE_STAPHYLOCOCCI",
  "Coagulase-negative staphylococci",

  "STAPHYLOCOCCUS_LUGDUNENSIS",
  "Staphylococcus lugdunensis",

  "ENTEROCOCCUS_SPP",
  "Enterococcus spp.",

  "YEAST",
  "Yeast",

  "BETA STREPTOCOCCUS GROUP B",
  "Group B Streptococcus",

  "CORYNEBACTERIUM SPECIES (DIPHTHEROIDS)",
  "Corynebacterium spp. (diphtheroids)",

  "PSEUDOMONAS AERUGINOSA",
  "Pseudomonas aeruginosa",

  "CLOSTRIDIUM DIFFICILE",
  "Clostridioides difficile",

  "PROTEUS MIRABILIS",
  "Proteus mirabilis",

  "KLEBSIELLA PNEUMONIAE",
  "Klebsiella pneumoniae",

  "VIRIDANS STREPTOCOCCI",
  "Viridans group streptococci",

  "BACTEROIDES FRAGILIS GROUP",
  "Bacteroides fragilis group",

  "BETA STREPTOCOCCUS GROUP A",
  "Group A Streptococcus",

  "BETA STREPTOCOCCUS GROUP G",
  "Group G Streptococcus",

  "GRAM POSITIVE BACTERIA",
  "Gram-positive bacteria",

  "GRAM_POSITIVE_COCCI",
  "Gram-positive cocci",

  "ANAEROBIC_GRAM_POSITIVE_COCCI",
  "Anaerobic Gram-positive cocci",

  "GRAM_NEGATIVE_RODS",
  "Gram-negative rods",

  "STREPTOCOCCUS ANGINOSUS (MILLERI) GROUP",
  "Streptococcus anginosus group",

  "ENTEROBACTER CLOACAE COMPLEX",
  "Enterobacter cloacae complex",

  "SERRATIA MARCESCENS",
  "Serratia marcescens",

  "POSITIVE FOR INFLUENZA A VIRAL ANTIGEN",
  "Influenza A viral antigen detected"
)


microbiology_finding_tokens <- validated_positive_result_tokens %>%
  dplyr::mutate(
    finding_key =
      dplyr::case_when(
        organism_name_normalized %in% c(
          "STAPHYLOCOCCUS, COAGULASE NEGATIVE",
          "STAPHYLOCOCCUS EPIDERMIDIS"
        ) ~
          "COAGULASE_NEGATIVE_STAPHYLOCOCCI",

        organism_name_normalized ==
          "STAPHYLOCOCCUS LUGDUNENSIS" ~
          "STAPHYLOCOCCUS_LUGDUNENSIS",

        stringr::str_starts(
          organism_name_normalized,
          "ENTEROCOCCUS"
        ) ~
          "ENTEROCOCCUS_SPP",

        organism_name_normalized %in% c(
          "GRAM NEGATIVE ROD(S)",
          "GRAM NEGATIVE ROD #2",
          "GRAM NEGATIVE ROD #3"
        ) ~
          "GRAM_NEGATIVE_RODS",

        organism_name_normalized ==
          "GRAM POSITIVE COCCUS(COCCI)" ~
          "GRAM_POSITIVE_COCCI",

        organism_name_normalized ==
          "ANAEROBIC GRAM POSITIVE COCCUS(I)" ~
          "ANAEROBIC_GRAM_POSITIVE_COCCI",

        TRUE ~
          organism_name_normalized
      )
  ) %>%
  dplyr::left_join(
    microbiology_finding_dictionary,
    by = "finding_key"
  ) %>%
  dplyr::mutate(
    finding_display_label =
      dplyr::coalesce(
        finding_display_label,
        organism_name_source
      )
  )


## Episode-level finding data


microbiology_finding_episode <- microbiology_finding_tokens %>%
  dplyr::distinct(
    episode_id,
    microbiology_group,
    finding_key,
    finding_display_label
  )


microbiology_episode_finding_counts <-
  microbiology_finding_episode %>%
  dplyr::count(
    episode_id,
    microbiology_group,
    name = "distinct_finding_n"
  )


microbiology_episode_level <- microbiology_matched_data %>%
  dplyr::left_join(
    microbiology_episode_finding_counts,
    by = c(
      "episode_id",
      "microbiology_group"
    )
  ) %>%
  dplyr::mutate(
    distinct_finding_n =
      dplyr::coalesce(
        distinct_finding_n,
        0L
      ),

    any_reported_microbiology_finding =
      distinct_finding_n > 0L
  )


## Formatting functions


safe_quantile <- function(
  x,
  probability
) {
  x <- x[
    !is.na(
      x
    )
  ]

  if (
    length(
      x
    ) == 0L
  ) {
    return(
      NA_real_
    )
  }

  as.numeric(
    stats::quantile(
      x,
      probs = probability,
      names = FALSE,
      na.rm = TRUE,
      type = 7
    )
  )
}


format_number_clean <- function(
  x
) {
  dplyr::case_when(
    is.na(
      x
    ) ~
      "NA",

    abs(
      x -
        round(
          x
        )
    ) < 1e-10 ~
      formatC(
        x,
        format = "f",
        digits = 0
      ),

    TRUE ~
      formatC(
        x,
        format = "f",
        digits = 1
      )
  )
}


format_median_iqr <- function(
  median_value,
  q1_value,
  q3_value
) {
  paste0(
    format_number_clean(
      median_value
    ),
    " (",
    format_number_clean(
      q1_value
    ),
    "–",
    format_number_clean(
      q3_value
    ),
    ")"
  )
}


format_n_percent <- function(
  numerator,
  denominator
) {
  dplyr::if_else(
    denominator > 0L,
    sprintf(
      "%d (%.1f%%)",
      as.integer(
        numerator
      ),
      100 *
        numerator /
        denominator
    ),
    paste0(
      as.integer(
        numerator
      ),
      " (NA)"
    )
  )
}


## Episode-level summary


microbiology_episode_summary_numeric <-
  microbiology_episode_level %>%
  dplyr::group_by(
    microbiology_group
  ) %>%
  dplyr::summarise(
    episode_n =
      dplyr::n(),

    qualifying_specimen_median =
      median(
        infection_microbiology_qualifying_specimen_n,
        na.rm = TRUE
      ),

    qualifying_specimen_q1 =
      safe_quantile(
        infection_microbiology_qualifying_specimen_n,
        0.25
      ),

    qualifying_specimen_q3 =
      safe_quantile(
        infection_microbiology_qualifying_specimen_n,
        0.75
      ),

    distinct_study_median =
      median(
        infection_microbiology_distinct_study_n,
        na.rm = TRUE
      ),

    distinct_study_q1 =
      safe_quantile(
        infection_microbiology_distinct_study_n,
        0.25
      ),

    distinct_study_q3 =
      safe_quantile(
        infection_microbiology_distinct_study_n,
        0.75
      ),

    finding_positive_n =
      sum(
        any_reported_microbiology_finding
      ),

    finding_positive_percent =
      100 *
        finding_positive_n /
        episode_n,

    distinct_finding_median =
      median(
        distinct_finding_n[
          any_reported_microbiology_finding
        ],
        na.rm = TRUE
      ),

    distinct_finding_q1 =
      safe_quantile(
        distinct_finding_n[
          any_reported_microbiology_finding
        ],
        0.25
      ),

    distinct_finding_q3 =
      safe_quantile(
        distinct_finding_n[
          any_reported_microbiology_finding
        ],
        0.75
      ),

    multiple_finding_n =
      sum(
        distinct_finding_n > 1L
      ),

    multiple_finding_percent_positive =
      dplyr::if_else(
        finding_positive_n > 0L,
        100 *
          multiple_finding_n /
          finding_positive_n,
        NA_real_
      ),

    blood_culture_obtained_n =
      sum(blood_culture_obtained_flag == 1L, na.rm = TRUE),

    blood_culture_valid_positive_n =
      sum(blood_culture_valid_positive_flag == 1L, na.rm = TRUE),

    blood_culture_positivity_percent_cultured =
      dplyr::if_else(
        blood_culture_obtained_n > 0L,
        100 * blood_culture_valid_positive_n /
          blood_culture_obtained_n,
        NA_real_
      ),

    .groups = "drop"
  )


## Most frequent reported findings


microbiology_finding_overall <- microbiology_finding_episode %>%
  dplyr::distinct(
    episode_id,
    microbiology_group,
    finding_key,
    finding_display_label
  ) %>%
  dplyr::count(
    finding_key,
    finding_display_label,
    name = "overall_episodes",
    sort = TRUE
  )


microbiology_top_findings <- microbiology_finding_overall %>%
  dplyr::slice_max(
    order_by = overall_episodes,
    n = 10L,
    with_ties = TRUE
  ) %>%
  dplyr::arrange(
    dplyr::desc(
      overall_episodes
    ),
    finding_display_label
  ) %>%
  dplyr::mutate(
    finding_order =
      dplyr::row_number()
  )


microbiology_group_denominators <-
  microbiology_episode_summary_numeric %>%
  dplyr::select(
    microbiology_group,
    episode_n,
    finding_positive_n
  )


microbiology_finding_counts_by_group <-
  microbiology_finding_episode %>%
  dplyr::semi_join(
    microbiology_top_findings,
    by = "finding_key"
  ) %>%
  dplyr::count(
    microbiology_group,
    finding_key,
    name = "episodes"
  )


microbiology_finding_summary_numeric <-
  tidyr::crossing(
    microbiology_group =
      factor(
        c(
          "Infection without sepsis",
          "Sepsis"
        ),
        levels = c(
          "Infection without sepsis",
          "Sepsis"
        )
      ),

    finding_key =
      microbiology_top_findings$
        finding_key
  ) %>%
  dplyr::left_join(
    microbiology_top_findings,
    by = "finding_key"
  ) %>%
  dplyr::left_join(
    microbiology_finding_counts_by_group,
    by = c(
      "microbiology_group",
      "finding_key"
    )
  ) %>%
  dplyr::left_join(
    microbiology_group_denominators,
    by = "microbiology_group"
  ) %>%
  dplyr::mutate(
    episodes =
      dplyr::coalesce(
        episodes,
        0L
      ),

    percent_of_all_matched_episodes =
      dplyr::if_else(
        episode_n > 0L,
        100 *
          episodes /
          episode_n,
        NA_real_
      )
  ) %>%
  dplyr::arrange(
    finding_order,
    microbiology_group
  )


## Formatted table


microbiology_summary_rows_long <- dplyr::bind_rows(
  microbiology_episode_summary_numeric %>%
    dplyr::transmute(
      microbiology_group,
      section =
        "Microbiologic sampling and yield",
      row_order =
        1L,
      characteristic =
        "Qualifying microbiology specimens per episode, median (IQR)",
      display_value =
        format_median_iqr(
          qualifying_specimen_median,
          qualifying_specimen_q1,
          qualifying_specimen_q3
        )
    ),

  microbiology_episode_summary_numeric %>%
    dplyr::transmute(
      microbiology_group,
      section =
        "Microbiologic sampling and yield",
      row_order =
        2L,
      characteristic =
        "Distinct microbiology studies per episode, median (IQR)",
      display_value =
        format_median_iqr(
          distinct_study_median,
          distinct_study_q1,
          distinct_study_q3
        )
    ),

  microbiology_episode_summary_numeric %>%
    dplyr::transmute(
      microbiology_group,
      section =
        "Microbiologic sampling and yield",
      row_order =
        3L,
      characteristic =
        "At least one valid reported microbiologic finding, n (%)",
      display_value =
        format_n_percent(
          finding_positive_n,
          episode_n
        )
    ),

  microbiology_episode_summary_numeric %>%
    dplyr::transmute(
      microbiology_group,
      section =
        "Microbiologic sampling and yield",
      row_order =
        4L,
      characteristic =
        paste0(
          "Distinct microbiologic findings per microbiology-positive ",
          "episode, median (IQR)"
        ),
      display_value =
        format_median_iqr(
          distinct_finding_median,
          distinct_finding_q1,
          distinct_finding_q3
        )
    ),

  microbiology_episode_summary_numeric %>%
    dplyr::transmute(
      microbiology_group,
      section =
        "Microbiologic sampling and yield",
      row_order =
        5L,
      characteristic =
        paste0(
          "Multiple distinct microbiologic findings, ",
          "n (% of microbiology-positive episodes)"
        ),
      display_value =
        format_n_percent(
          multiple_finding_n,
          finding_positive_n
        )
    ),

  microbiology_episode_summary_numeric %>%
    dplyr::transmute(
      microbiology_group,
      section = "Blood cultures",
      row_order = 6L,
      characteristic = "At least one blood culture obtained, n (%)",
      display_value = format_n_percent(
        blood_culture_obtained_n,
        episode_n
      )
    ),

  microbiology_episode_summary_numeric %>%
    dplyr::transmute(
      microbiology_group,
      section = "Blood cultures",
      row_order = 7L,
      characteristic = "At least one valid positive blood culture, n (%)",
      display_value = format_n_percent(
        blood_culture_valid_positive_n,
        episode_n
      )
    ),

  microbiology_episode_summary_numeric %>%
    dplyr::transmute(
      microbiology_group,
      section = "Blood cultures",
      row_order = 8L,
      characteristic = "Blood-culture positivity among cultured episodes, n/N (%)",
      display_value = paste0(
        blood_culture_valid_positive_n,
        "/",
        blood_culture_obtained_n,
        " (",
        sprintf("%.1f", blood_culture_positivity_percent_cultured),
        "%)"
      )
    )
)


microbiology_finding_rows_long <-
  microbiology_finding_summary_numeric %>%
  dplyr::transmute(
    microbiology_group,
    section =
      "Most frequent reported microbiologic findings",

    row_order =
      100L +
        finding_order,

    characteristic =
      finding_display_label,

    display_value =
      format_n_percent(
        episodes,
        episode_n
      )
  )


microbiology_table_long <- dplyr::bind_rows(
  microbiology_summary_rows_long,
  microbiology_finding_rows_long
) %>%
  dplyr::mutate(
    group_column =
      dplyr::case_when(
        as.character(
          microbiology_group
        ) ==
          "Infection without sepsis" ~
          "infection_without_sepsis",

        as.character(
          microbiology_group
        ) ==
          "Sepsis" ~
          "sepsis",

        TRUE ~
          NA_character_
      )
  )


microbiology_table_descriptive <-
  microbiology_table_long %>%
  dplyr::select(
    section,
    row_order,
    characteristic,
    group_column,
    display_value
  ) %>%
  tidyr::pivot_wider(
    names_from =
      group_column,
    values_from =
      display_value
  ) %>%
  dplyr::arrange(
    row_order
  ) %>%
  dplyr::select(
    -row_order
  )


infection_without_sepsis_n <-
  microbiology_episode_summary_numeric %>%
  dplyr::filter(
    microbiology_group ==
      "Infection without sepsis"
  ) %>%
  dplyr::pull(
    episode_n
  )


sepsis_n <-
  microbiology_episode_summary_numeric %>%
  dplyr::filter(
    microbiology_group ==
      "Sepsis"
  ) %>%
  dplyr::pull(
    episode_n
  )


names(
  microbiology_table_descriptive
)[
  names(
    microbiology_table_descriptive
  ) ==
    "infection_without_sepsis"
] <- paste0(
  "Infection without sepsis, n = ",
  infection_without_sepsis_n
)


names(
  microbiology_table_descriptive
)[
  names(
    microbiology_table_descriptive
  ) ==
    "sepsis"
] <- paste0(
  "Sepsis, n = ",
  sepsis_n
)


## Validation checks


microbiology_validation_checks <- tibble::tibble(
  check = c(
    "All matched episodes assigned to a group",
    "No duplicated matched episode identifiers",
    "No missing finding names after parsing",
    "Administrative labels excluded by exact matching",
    "Finding-positive episodes do not exceed total episodes",
    "Finding frequencies do not exceed finding-positive denominators",
    "No duplicated episode and normalized finding combinations"
  ),

  pass = c(
    !anyNA(
      microbiology_matched_data$
        microbiology_group
    ),

    anyDuplicated(
      microbiology_matched_data$
        episode_id
    ) == 0L,

    positive_result_parsing_check$
      missing_finding_name == 0L,

    !any(
      microbiology_finding_tokens$
        organism_name_normalized %in%
        administrative_microbiology_labels
    ),

    all(
      microbiology_episode_summary_numeric$
        finding_positive_n <=
        microbiology_episode_summary_numeric$
          episode_n
    ),

    all(
      microbiology_finding_summary_numeric$
        episodes <=
        microbiology_finding_summary_numeric$
          finding_positive_n
    ),

    anyDuplicated(
      microbiology_finding_episode[
        c(
          "episode_id",
          "microbiology_group",
          "finding_key"
        )
      ]
    ) == 0L
  )
)


if (
  any(
    !microbiology_validation_checks$
      pass
  )
) {
  print(
    microbiology_validation_checks,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one microbiology validation check failed.",
    call. = FALSE
  )
}


## MICROBIOLOGY DEFINITIONS


# Microbiology-positive episodes had at least one valid finding after excluding
# the exact administrative label CANCELLED.

# Finding percentages use all matched episodes in each group as denominators
# and are not mutually exclusive.

# Coagulase-negative staphylococci include generic results and S. epidermidis;
# S. lugdunensis remains separate.

# All Enterococcus identifications are grouped as Enterococcus spp.

# MIXED BACTERIAL FLORA is displayed as Mixed bacterial microbiota.

# Findings are ranked in the complete matched population; cutoff ties are kept.

# ==============================================================================
# 21. MANUSCRIPT AND SUPPLEMENT TABLE INPUTS
# ==============================================================================

print_section <- function(title, object) {
  cat(
    "\n\n",
    paste(rep("=", 88L), collapse = ""),
    "\n",
    title,
    "\n",
    paste(rep("=", 88L), collapse = ""),
    "\n",
    sep = ""
  )

  if (is.data.frame(object)) {
    print(tibble::as_tibble(object), n = Inf, width = Inf)
  } else {
    print(object)
  }

  invisible(object)
}


matched_group_data <- function(data, analysis_id) {
  data %>%
    dplyr::mutate(
      analysis_id = analysis_id,
      comparison_group = dplyr::if_else(
        treatment_sepsis == 1L,
        "Sepsis",
        dplyr::if_else(
          analysis_id == "A",
          "Controls without suspected infection",
          "Suspected infection without sepsis"
        )
      )
    )
}


analysis_A_descriptive <- matched_group_data(
  analysis_A_final_data,
  "A"
)

analysis_B_descriptive <- matched_group_data(
  analysis_B_final_data,
  "B"
)


summarize_continuous_variables <- function(data, variables) {
  data %>%
    dplyr::select(
      analysis_id,
      comparison_group,
      dplyr::all_of(variables)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(variables),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::group_by(analysis_id, comparison_group, variable) %>%
    dplyr::summarise(
      available_n = sum(!is.na(value)),
      total_n = dplyr::n(),
      mean = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
      sd = if (sum(!is.na(value)) < 2L) NA_real_ else stats::sd(value, na.rm = TRUE),
      median = if (all(is.na(value))) {
        NA_real_
      } else {
        stats::median(value, na.rm = TRUE)
      },
      q1 = if (all(is.na(value))) {
        NA_real_
      } else {
        stats::quantile(value, 0.25, na.rm = TRUE, names = FALSE)
      },
      q3 = if (all(is.na(value))) {
        NA_real_
      } else {
        stats::quantile(value, 0.75, na.rm = TRUE, names = FALSE)
      },
      .groups = "drop"
    )
}


summarize_binary_variables <- function(data, variables) {
  data %>%
    dplyr::select(
      analysis_id,
      comparison_group,
      dplyr::all_of(variables)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(variables),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::group_by(analysis_id, comparison_group, variable) %>%
    dplyr::summarise(
      available_n = sum(!is.na(value)),
      total_n = dplyr::n(),
      event_n = sum(value == 1L, na.rm = TRUE),
      event_percent = if (all(is.na(value))) {
        NA_real_
      } else {
        100 * mean(value == 1L, na.rm = TRUE)
      },
      .groups = "drop"
    )
}


summarize_categorical_variables <- function(data, variables) {
  data %>%
    dplyr::select(
      analysis_id,
      comparison_group,
      dplyr::all_of(variables)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(variables),
      names_to = "variable",
      values_to = "level"
    ) %>%
    dplyr::mutate(
      level = dplyr::coalesce(as.character(level), "[missing]")
    ) %>%
    dplyr::count(
      analysis_id,
      comparison_group,
      variable,
      level,
      name = "episodes"
    ) %>%
    dplyr::group_by(analysis_id, comparison_group, variable) %>%
    dplyr::mutate(
      percent = 100 * episodes / sum(episodes)
    ) %>%
    dplyr::ungroup()
}


absolute_smd_numeric <- function(data, variable) {
  treated <- data[[variable]][data$treatment_sepsis == 1L]
  comparator <- data[[variable]][data$treatment_sepsis == 0L]

  denominator <- stats::sd(treated, na.rm = TRUE)

  if (!is.finite(denominator) || denominator == 0) {
    return(NA_real_)
  }

  abs(
    mean(treated, na.rm = TRUE) -
      mean(comparator, na.rm = TRUE)
  ) / denominator
}


numeric_smd_table <- function(data, analysis_id, variables) {
  tibble::tibble(
    analysis_id = analysis_id,
    variable = variables,
    absolute_smd = vapply(
      variables,
      function(variable) absolute_smd_numeric(data, variable),
      numeric(1L)
    )
  )
}


table1_continuous_variables <- c(
  "age",
  "sofa_noncoag_unavailable_component_n"
)

table1_binary_variables <- c(
  "chronic_pulmonary_disease",
  "any_liver_disease",
  "renal_disease",
  "any_diabetes"
)

table1_categorical_variables_A <- c(
  "gender_match",
  "race_match_group",
  "insurance_match_group",
  "sofa_noncoag_observed_category"
)

table1_categorical_variables_B <- c(
  "gender_match",
  "race_match_group",
  "insurance_match_group",
  "infection_site_match_group"
)


manuscript_table1_continuous <- dplyr::bind_rows(
  summarize_continuous_variables(
    analysis_A_descriptive,
    table1_continuous_variables
  ),
  summarize_continuous_variables(
    analysis_B_descriptive,
    "age"
  )
)


manuscript_table1_binary <- dplyr::bind_rows(
  summarize_binary_variables(
    analysis_A_descriptive,
    table1_binary_variables
  ),
  summarize_binary_variables(
    analysis_B_descriptive,
    table1_binary_variables
  )
)


manuscript_table1_categorical <- dplyr::bind_rows(
  summarize_categorical_variables(
    analysis_A_descriptive,
    table1_categorical_variables_A
  ),
  summarize_categorical_variables(
    analysis_B_descriptive,
    table1_categorical_variables_B
  )
)


supplementary_table_s1_variables <- c(
  "sofa_total_revised_observed_0_24h",
  "sofa_noncoag_observed_0_24h",
  "sofa_respiratory_revised_0_24h",
  "sofa_coagulation_revised_0_24h",
  "sofa_hepatic_revised_0_24h",
  "sofa_cardiovascular_revised_0_24h",
  "sofa_neurologic_revised_0_24h",
  "sofa_renal_revised_0_24h",
  "ed_arrival_temperature_c",
  "ed_arrival_heart_rate",
  "ed_arrival_respiratory_rate",
  "ed_arrival_map_mmhg",
  "ed_arrival_spo2_percent",
  "creatinine_first_value_0_24h",
  "bilirubin_first_value_0_24h"
)


supplementary_table_s1_descriptive <- dplyr::bind_rows(
  summarize_continuous_variables(
    analysis_A_descriptive,
    supplementary_table_s1_variables
  ),
  summarize_continuous_variables(
    analysis_B_descriptive,
    supplementary_table_s1_variables
  )
)


supplementary_table_s1_smd <- dplyr::bind_rows(
  numeric_smd_table(
    analysis_A_final_data,
    "A",
    supplementary_table_s1_variables
  ),
  numeric_smd_table(
    analysis_B_final_data,
    "B",
    supplementary_table_s1_variables
  )
) %>%
  # The manuscript intentionally reports these structurally incomplete
  # component/laboratory SMDs as NA rather than as available-case contrasts.
  dplyr::mutate(
    absolute_smd = dplyr::if_else(
      variable %in% c(
        "sofa_respiratory_revised_0_24h",
        "sofa_hepatic_revised_0_24h",
        "sofa_neurologic_revised_0_24h",
        "bilirubin_first_value_0_24h"
      ),
      NA_real_,
      absolute_smd
    )
  )


supplementary_table_s2_variables <- c(
  "cbc_index_hemoglobin",
  "cbc_index_platelet_count",
  "cbc_index_wbc_for_calculation",
  "cbc_index_neutrophil_count",
  "cbc_index_lymphocyte_count",
  "cbc_index_monocyte_count",
  "crp_index_value_mg_l",
  "nlr",
  "plr",
  "mlr",
  "nmr",
  "siri",
  "sii"
)


supplementary_table_s2_descriptive <- dplyr::bind_rows(
  summarize_continuous_variables(
    analysis_A_descriptive,
    supplementary_table_s2_variables
  ),
  summarize_continuous_variables(
    analysis_B_descriptive,
    supplementary_table_s2_variables
  )
)


supplementary_table_s2_smd <- dplyr::bind_rows(
  numeric_smd_table(
    analysis_A_final_data,
    "A",
    supplementary_table_s2_variables
  ),
  numeric_smd_table(
    analysis_B_final_data,
    "B",
    supplementary_table_s2_variables
  )
)


# ==============================================================================
# 22. POST HOC NLR COMPONENT ANALYSIS (SUPPLEMENTARY TABLE S6 / FIGURE S5)
# ==============================================================================

component_model_auc <- function(model, data) {
  prediction <- stats::predict(model, type = "response")

  roc_object <- pROC::roc(
    response = data$sepsis,
    predictor = prediction,
    levels = c(0L, 1L),
    direction = "<",
    quiet = TRUE
  )

  as.numeric(pROC::auc(roc_object))
}


extract_component_lrt <- function(reduced_model, full_model) {
  test <- stats::anova(reduced_model, full_model, test = "LRT")

  tibble::tibble(
    lrt_chisq = as.numeric(test$Deviance[[2L]]),
    lrt_df = as.integer(test$Df[[2L]]),
    p_value = as.numeric(test$`Pr(>Chi)`[[2L]])
  )
}


prepare_nlr_component_data <- function(data, analysis_id) {
  output <- data %>%
    dplyr::transmute(
      analysis_id = analysis_id,
      episode_id = as.character(episode_id),
      sepsis = as.integer(treatment_sepsis),
      matching_weight = as.numeric(matching_weight),
      lymphocytes = as.numeric(cbc_index_lymphocyte_count),
      neutrophils = as.numeric(cbc_index_neutrophil_count)
    )

  if (
    anyDuplicated(output$episode_id) > 0L ||
    any(!output$sepsis %in% c(0L, 1L)) ||
    any(!is.finite(output$lymphocytes)) ||
    any(!is.finite(output$neutrophils)) ||
    any(!is.finite(output$matching_weight)) ||
    any(abs(output$matching_weight - 1) > 1e-12)
  ) {
    stop(
      paste0("Invalid NLR component data for analysis ", analysis_id, "."),
      call. = FALSE
    )
  }

  output
}


nlr_component_data <- list(
  A = prepare_nlr_component_data(analysis_A_final_data, "A"),
  B = prepare_nlr_component_data(analysis_B_final_data, "B")
)


nlr_component_models <- list()
nlr_component_result_rows <- list()
nlr_component_result_index <- 0L


for (spline_df in c(3L, 4L)) {
  for (analysis_id in c("A", "B")) {
    component_data <- nlr_component_data[[analysis_id]]

    model_lymphocyte <- stats::glm(
      sepsis ~ splines::ns(lymphocytes, df = spline_df),
      family = stats::binomial(),
      data = component_data
    )

    model_neutrophil <- stats::glm(
      sepsis ~ splines::ns(neutrophils, df = spline_df),
      family = stats::binomial(),
      data = component_data
    )

    model_both <- stats::glm(
      sepsis ~
        splines::ns(lymphocytes, df = spline_df) +
        splines::ns(neutrophils, df = spline_df),
      family = stats::binomial(),
      data = component_data
    )

    model_key <- paste(analysis_id, spline_df, sep = "_df")
    nlr_component_models[[model_key]] <- model_both

    lymphocyte_lrt <- extract_component_lrt(
      model_neutrophil,
      model_both
    )

    neutrophil_lrt <- extract_component_lrt(
      model_lymphocyte,
      model_both
    )

    auc_lymphocyte <- component_model_auc(
      model_lymphocyte,
      component_data
    )
    auc_neutrophil <- component_model_auc(
      model_neutrophil,
      component_data
    )
    auc_both <- component_model_auc(
      model_both,
      component_data
    )

    nlr_component_result_index <- nlr_component_result_index + 1L
    nlr_component_result_rows[[nlr_component_result_index]] <-
      lymphocyte_lrt %>%
      dplyr::mutate(
        spline_df = spline_df,
        analysis_id = analysis_id,
        added_component =
          "Lymphocyte count after accounting for neutrophil count",
        reduced_model = "neutrophil_only",
        delta_aic = stats::AIC(model_both) -
          stats::AIC(model_neutrophil),
        delta_bic = stats::BIC(model_both) -
          stats::BIC(model_neutrophil),
        apparent_auc_reduced = auc_neutrophil,
        apparent_auc_combined = auc_both,
        delta_apparent_auc = auc_both - auc_neutrophil
      )

    nlr_component_result_index <- nlr_component_result_index + 1L
    nlr_component_result_rows[[nlr_component_result_index]] <-
      neutrophil_lrt %>%
      dplyr::mutate(
        spline_df = spline_df,
        analysis_id = analysis_id,
        added_component =
          "Neutrophil count after accounting for lymphocyte count",
        reduced_model = "lymphocyte_only",
        delta_aic = stats::AIC(model_both) -
          stats::AIC(model_lymphocyte),
        delta_bic = stats::BIC(model_both) -
          stats::BIC(model_lymphocyte),
        apparent_auc_reduced = auc_lymphocyte,
        apparent_auc_combined = auc_both,
        delta_apparent_auc = auc_both - auc_lymphocyte
      )
  }
}


supplementary_table_s6 <- dplyr::bind_rows(
  nlr_component_result_rows
) %>%
  dplyr::group_by(spline_df) %>%
  dplyr::mutate(
    p_value_holm = stats::p.adjust(p_value, method = "holm")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(spline_df, added_component, analysis_id) %>%
  dplyr::select(
    spline_df,
    added_component,
    analysis_id,
    lrt_chisq,
    lrt_df,
    p_value,
    p_value_holm,
    delta_aic,
    delta_bic,
    apparent_auc_reduced,
    apparent_auc_combined,
    delta_apparent_auc
  )


centered_spline_curve <- function(
    data,
    model,
    analysis_id,
    spline_df,
    component
) {
  focal_variable <- if (component == "lymphocyte") {
    "lymphocytes"
  } else {
    "neutrophils"
  }

  other_variable <- if (component == "lymphocyte") {
    "neutrophils"
  } else {
    "lymphocytes"
  }

  display_limits <- stats::quantile(
    data[[focal_variable]],
    probs = c(0.025, 0.975),
    na.rm = TRUE,
    names = FALSE
  )

  grid_values <- seq(
    display_limits[[1L]],
    display_limits[[2L]],
    length.out = 160L
  )

  new_data <- data.frame(
    lymphocytes = stats::median(data$lymphocytes),
    neutrophils = stats::median(data$neutrophils)
  )[rep(1L, length(grid_values)), , drop = FALSE]

  new_data[[focal_variable]] <- grid_values

  reference_data <- data.frame(
    lymphocytes = stats::median(data$lymphocytes),
    neutrophils = stats::median(data$neutrophils)
  )

  model_terms <- stats::delete.response(stats::terms(model))
  design_grid <- stats::model.matrix(
    model_terms,
    new_data,
    contrasts.arg = model$contrasts,
    xlev = model$xlevels
  )
  design_reference <- stats::model.matrix(
    model_terms,
    reference_data,
    contrasts.arg = model$contrasts,
    xlev = model$xlevels
  )

  contrast_matrix <- sweep(
    design_grid,
    2L,
    design_reference[1L, ],
    FUN = "-"
  )

  centered_log_odds <- as.numeric(
    contrast_matrix %*% stats::coef(model)
  )

  centered_se <- sqrt(
    rowSums((contrast_matrix %*% stats::vcov(model)) * contrast_matrix)
  )

  tibble::tibble(
    analysis_id = analysis_id,
    spline_df = spline_df,
    component = component,
    component_value = grid_values,
    centered_log_odds = centered_log_odds,
    lower_95 = centered_log_odds - 1.96 * centered_se,
    upper_95 = centered_log_odds + 1.96 * centered_se,
    reference_value = stats::median(data[[focal_variable]]),
    other_component_reference = stats::median(data[[other_variable]])
  )
}


nlr_curve_specifications <- tidyr::crossing(
  analysis_id = c("A", "B"),
  spline_df = c(3L, 4L),
  component = c("lymphocyte", "neutrophil")
)


supplementary_figure_s5_curve_data <- purrr::map_dfr(
  split(
    nlr_curve_specifications,
    seq_len(nrow(nlr_curve_specifications))
  ),
  function(specification) {
    analysis_id <- specification$analysis_id[[1L]]
    spline_df <- specification$spline_df[[1L]]
    component <- specification$component[[1L]]

    centered_spline_curve(
      data = nlr_component_data[[analysis_id]],
      model = nlr_component_models[[paste(analysis_id, spline_df, sep = "_df")]],
      analysis_id = analysis_id,
      spline_df = spline_df,
      component = component
    )
  }
)


supplementary_figure_s5 <- ggplot2::ggplot(
  supplementary_figure_s5_curve_data,
  ggplot2::aes(
    x = component_value,
    y = centered_log_odds,
    color = factor(spline_df),
    fill = factor(spline_df),
    linetype = factor(spline_df),
    group = factor(spline_df)
  )
) +
  ggplot2::geom_hline(yintercept = 0, color = "grey65", linewidth = 0.4) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = lower_95, ymax = upper_95),
    alpha = 0.12,
    color = NA
  ) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::facet_grid(
    analysis_id ~ component,
    scales = "free_x",
    labeller = ggplot2::labeller(
      analysis_id = c(
        A = "Sepsis vs controls",
        B = "Sepsis vs infection without sepsis"
      ),
      component = c(
        lymphocyte = "Absolute lymphocyte count",
        neutrophil = "Absolute neutrophil count"
      )
    )
  ) +
  ggplot2::scale_color_manual(
    values = c(`3` = "#0072B2", `4` = "#D55E00"),
    name = "Spline df"
  ) +
  ggplot2::scale_fill_manual(
    values = c(`3` = "#0072B2", `4` = "#D55E00"),
    name = "Spline df"
  ) +
  ggplot2::scale_linetype_manual(
    values = c(`3` = "solid", `4` = "dashed"),
    name = "Spline df"
  ) +
  ggplot2::labs(
    x = "Cell count (x10^9/L)",
    y = "Centered log-odds of sepsis"
  ) +
  ggplot2::theme_classic(base_size = 10) +
  ggplot2::theme(legend.position = "bottom")


# ==============================================================================
# 23. SECOND-REVIEWER ANATOMICAL-SITE VALIDATION
# ==============================================================================

normalize_site_label <- function(values) {
  values %>%
    as.character() %>%
    stringr::str_squish() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[_-]+", " ") %>%
    stringr::str_squish() %>%
    dplyr::na_if("")
}


cohen_kappa_unweighted <- function(original, adjudicated) {
  complete <- !is.na(original) & !is.na(adjudicated)
  original <- original[complete]
  adjudicated <- adjudicated[complete]

  categories <- sort(union(unique(original), unique(adjudicated)))
  confusion <- table(
    factor(original, levels = categories),
    factor(adjudicated, levels = categories)
  )

  total_n <- sum(confusion)
  observed <- sum(diag(confusion)) / total_n
  expected <- sum(rowSums(confusion) * colSums(confusion)) / total_n^2

  if (!is.finite(expected) || expected >= 1) {
    return(NA_real_)
  }

  (observed - expected) / (1 - expected)
}


find_validation_batch_column <- function(data) {
  preferred <- c(
    "validation_batch",
    "validation_site_batch",
    "batch",
    "batch_id",
    "validation_batch_id"
  )
  exact <- intersect(preferred, names(data))

  if (length(exact) > 0L) {
    return(exact[[1L]])
  }

  candidates <- names(data)[
    stringr::str_detect(stringr::str_to_lower(names(data)), "batch")
  ]

  if (length(candidates) != 1L) {
    stop(
      "A unique anatomical-site validation batch column was not identified.",
      call. = FALSE
    )
  }

  candidates[[1L]]
}


site_validation_inputs_available <- all(
  file.exists(
    c(
      SITE_VALIDATION_KEY_FILE,
      SITE_VALIDATION_ADJUDICATED_FILE
    )
  )
)


if (site_validation_inputs_available) {
  site_key <- readr::read_csv(
    SITE_VALIDATION_KEY_FILE,
    show_col_types = FALSE,
    progress = FALSE
  )

  site_adjudicated <- readr::read_csv(
    SITE_VALIDATION_ADJUDICATED_FILE,
    show_col_types = FALSE,
    progress = FALSE
  )

  if (
    "validation_site_case_id" %in% names(site_key) &&
    "validation_site_case_id" %in% names(site_adjudicated)
  ) {
    site_key <- site_key %>%
      dplyr::mutate(
        .join_id = as.character(validation_site_case_id)
      )
    site_adjudicated <- site_adjudicated %>%
      dplyr::mutate(
        .join_id = as.character(validation_site_case_id)
      )
  } else {
    batch_column_key <- find_validation_batch_column(site_key)
    batch_column_adjudicated <-
      find_validation_batch_column(site_adjudicated)

    if (
      !"validation_case_within_batch" %in% names(site_key) ||
      !"validation_case_within_batch" %in% names(site_adjudicated)
    ) {
      stop(
        "The anatomical-site validation join columns could not be identified.",
        call. = FALSE
      )
    }

    site_key <- site_key %>%
      dplyr::mutate(
        .join_id = dplyr::if_else(
          is.na(.data[[batch_column_key]]) |
            stringr::str_squish(as.character(.data[[batch_column_key]])) == "" |
            is.na(validation_case_within_batch) |
            stringr::str_squish(as.character(validation_case_within_batch)) == "",
          NA_character_,
          paste(
            .data[[batch_column_key]],
            validation_case_within_batch,
            sep = "___"
          )
        )
      )
    site_adjudicated <- site_adjudicated %>%
      dplyr::mutate(
        .join_id = dplyr::if_else(
          is.na(.data[[batch_column_adjudicated]]) |
            stringr::str_squish(
              as.character(.data[[batch_column_adjudicated]])
            ) == "" |
            is.na(validation_case_within_batch) |
            stringr::str_squish(as.character(validation_case_within_batch)) == "",
          NA_character_,
          paste(
            .data[[batch_column_adjudicated]],
            validation_case_within_batch,
            sep = "___"
          )
        )
      )
  }

  if (
    anyNA(site_key$.join_id) ||
    anyNA(site_adjudicated$.join_id) ||
    any(stringr::str_squish(site_key$.join_id) == "") ||
    any(stringr::str_squish(site_adjudicated$.join_id) == "") ||
    anyDuplicated(site_key$.join_id) > 0L ||
    anyDuplicated(site_adjudicated$.join_id) > 0L
  ) {
    stop(
      "Anatomical-site validation identifiers are incomplete or duplicated.",
      call. = FALSE
    )
  }

  original_site_column <- "infection_site_index_broad_clean"

  adjudicated_site_candidates <- c(
    "infection_site_index_broad_adjudicated",
    "infection_site_index_broad_adjudicado",
    "infection_site_broad_adjudicated",
    "infection_site_broad_adjudicado",
    "infection_site_index_broad_final",
    "infection_site_broad_final",
    "adjudicated_infection_site_broad",
    "infection_site_adjudicated",
    "infection_site_adjudicado",
    "adjudicated_site",
    "sitio_infeccion_adjudicado",
    "sitio_anatomico_adjudicado"
  )

  adjudicated_site_column <- intersect(
    adjudicated_site_candidates,
    names(site_adjudicated)
  )

  if (length(adjudicated_site_column) > 0L) {
    adjudicated_site_column <- adjudicated_site_column[[1L]]
  } else {
    adjudicated_names <- names(site_adjudicated)
    adjudicated_names_lower <- stringr::str_to_lower(adjudicated_names)
    broad_candidates <- adjudicated_names[
      stringr::str_detect(adjudicated_names_lower, "site|sitio") &
        stringr::str_detect(
          adjudicated_names_lower,
          "broad|adjud|adjudic|final"
        ) &
        !stringr::str_detect(
          adjudicated_names_lower,
          "secondary|detail|multiple|comment|note|reason"
        )
    ]

    if (length(broad_candidates) == 1L) {
      adjudicated_site_column <- broad_candidates[[1L]]
    }
  }

  if (
    !original_site_column %in% names(site_key) ||
    length(adjudicated_site_column) != 1L
  ) {
    stop(
      "The original or adjudicated anatomical-site column was not identified.",
      call. = FALSE
    )
  }

  site_validation_data <- site_key %>%
    dplyr::transmute(
      .join_id,
      original_site = normalize_site_label(
        .data[[original_site_column]]
      )
    ) %>%
    dplyr::inner_join(
      site_adjudicated %>%
        dplyr::transmute(
          .join_id,
          adjudicated_site = normalize_site_label(
            .data[[adjudicated_site_column]]
          )
        ),
      by = ".join_id"
    ) %>%
    dplyr::filter(
      !is.na(original_site),
      !is.na(adjudicated_site)
    ) %>%
    dplyr::mutate(
      exact_agreement = original_site == adjudicated_site
    )

  if (nrow(site_validation_data) != 200L) {
    stop(
      paste0(
        "Expected 200 complete anatomical-site validation cases; found ",
        nrow(site_validation_data),
        "."
      ),
      call. = FALSE
    )
  }

  site_agreement_estimate <- mean(site_validation_data$exact_agreement)
  site_kappa_estimate <- cohen_kappa_unweighted(
    site_validation_data$original_site,
    site_validation_data$adjudicated_site
  )

  set.seed(20260814L)

  site_bootstrap <- replicate(
    5000L,
    {
      bootstrap_index <- sample.int(
        nrow(site_validation_data),
        replace = TRUE
      )
      bootstrap_data <- site_validation_data[
        bootstrap_index,
        ,
        drop = FALSE
      ]

      c(
        exact_agreement = mean(bootstrap_data$exact_agreement),
        kappa = cohen_kappa_unweighted(
          bootstrap_data$original_site,
          bootstrap_data$adjudicated_site
        )
      )
    }
  )

  site_validation_summary <- tibble::tibble(
    metric = c("Exact agreement", "Unweighted Cohen kappa"),
    estimate = c(site_agreement_estimate, site_kappa_estimate),
    lower_95 = c(
      stats::quantile(
        site_bootstrap["exact_agreement", ],
        0.025,
        na.rm = TRUE,
        names = FALSE
      ),
      stats::quantile(
        site_bootstrap["kappa", ],
        0.025,
        na.rm = TRUE,
        names = FALSE
      )
    ),
    upper_95 = c(
      stats::quantile(
        site_bootstrap["exact_agreement", ],
        0.975,
        na.rm = TRUE,
        names = FALSE
      ),
      stats::quantile(
        site_bootstrap["kappa", ],
        0.975,
        na.rm = TRUE,
        names = FALSE
      )
    )
  )

  site_disagreement_patterns <- site_validation_data %>%
    dplyr::filter(!exact_agreement) %>%
    dplyr::count(
      original_site,
      adjudicated_site,
      sort = TRUE,
      name = "episodes"
    ) %>%
    dplyr::mutate(
      percent_of_disagreements = 100 * episodes / sum(episodes),
      cumulative_percent = cumsum(percent_of_disagreements)
    )

  site_confusion_matrix <- table(
    Original = site_validation_data$original_site,
    Adjudicated = site_validation_data$adjudicated_site
  )
} else {
  site_validation_summary <- tibble::tibble(
    metric = "Anatomical-site validation",
    estimate = NA_real_,
    lower_95 = NA_real_,
    upper_95 = NA_real_
  )

  site_disagreement_patterns <- tibble::tibble(
    original_site = character(),
    adjudicated_site = character(),
    episodes = integer(),
    percent_of_disagreements = numeric(),
    cumulative_percent = numeric()
  )

  site_confusion_matrix <- NULL

  warning(
    paste0(
      "Anatomical-site validation was skipped because one or both input ",
      "files were absent."
    ),
    call. = FALSE
  )
}


# ==============================================================================
# 24. MICROBIOLOGY SMD INPUTS (SUPPLEMENTARY TABLE S5)
# ==============================================================================

microbiology_episode_smd_variables <- c(
  "infection_microbiology_qualifying_specimen_n",
  "infection_microbiology_distinct_study_n",
  "any_reported_microbiology_finding",
  "blood_culture_obtained_flag",
  "blood_culture_valid_positive_flag"
)


supplementary_table_s5_episode_smd <- numeric_smd_table(
  microbiology_episode_level,
  "B",
  microbiology_episode_smd_variables
)


supplementary_table_s5_finding_smd <- purrr::map_dfr(
  seq_len(nrow(microbiology_top_findings)),
  function(index) {
    finding_key <- microbiology_top_findings$finding_key[[index]]
    finding_label <-
      microbiology_top_findings$finding_display_label[[index]]

    finding_episode_ids <- microbiology_finding_episode %>%
      dplyr::filter(.data$finding_key == finding_key) %>%
      dplyr::pull(episode_id)

    finding_data <- microbiology_matched_data %>%
      dplyr::transmute(
        treatment_sepsis,
        finding_present = as.integer(episode_id %in% finding_episode_ids)
      )

    tibble::tibble(
      analysis_id = "B",
      finding_key = finding_key,
      finding_label = finding_label,
      absolute_smd = absolute_smd_numeric(
        finding_data,
        "finding_present"
      )
    )
  }
)


# ==============================================================================
# 25. SELECTED ROC CURVES (MANUSCRIPT FIGURE 2)
# ==============================================================================

selected_roc_biomarkers <- c(
  "crp",
  "lymphocytes",
  "neutrophils",
  "nlr"
)


selected_roc_specifications <- tidyr::crossing(
  analysis_id = c("A", "B"),
  biomarker_id = selected_roc_biomarkers
)


selected_roc_curve_data <- purrr::map_dfr(
  split(
    selected_roc_specifications,
    seq_len(nrow(selected_roc_specifications))
  ),
  function(specification) {
    analysis_id <- specification$analysis_id[[1L]]
    biomarker_id <- specification$biomarker_id[[1L]]
    object_key <- paste("matched", analysis_id, biomarker_id, sep = "__")
    roc_object <- roc_objects[[object_key]]

    marker_information <- biomarker_registry %>%
      dplyr::filter(.data$biomarker_id == biomarker_id)

    auc_information <- roc_auc_results %>%
      dplyr::filter(
        design_id == "matched",
        .data$analysis_id == analysis_id,
        .data$biomarker_id == biomarker_id
      )

    tibble::tibble(
      analysis_id = analysis_id,
      biomarker_id = biomarker_id,
      biomarker_label = marker_information$biomarker_label[[1L]],
      false_positive_rate = 1 - roc_object$specificities,
      sensitivity = roc_object$sensitivities,
      auc = auc_information$auc[[1L]],
      auc_ci95_lower = auc_information$auc_ci95_lower[[1L]],
      auc_ci95_upper = auc_information$auc_ci95_upper[[1L]]
    )
  }
) %>%
  dplyr::mutate(
    analysis_label = dplyr::recode(
      analysis_id,
      A = "Sepsis vs controls without suspected infection",
      B = "Sepsis vs suspected infection without sepsis"
    ),
    legend_label = paste0(
      biomarker_label,
      " (AUROC ",
      sprintf("%.3f", auc),
      ")"
    )
  )


manuscript_figure_2 <- ggplot2::ggplot(
  selected_roc_curve_data,
  ggplot2::aes(
    x = false_positive_rate,
    y = sensitivity,
    color = legend_label,
    group = legend_label
  )
) +
  ggplot2::geom_abline(
    intercept = 0,
    slope = 1,
    color = "grey65",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::facet_wrap(~ analysis_label, nrow = 1L) +
  ggplot2::coord_equal() +
  ggplot2::scale_x_continuous(limits = c(0, 1)) +
  ggplot2::scale_y_continuous(limits = c(0, 1)) +
  ggplot2::labs(
    x = "1 - specificity",
    y = "Sensitivity",
    color = NULL
  ) +
  ggplot2::theme_classic(base_size = 10) +
  ggplot2::theme(legend.position = "bottom")


if (isTRUE(SAVE_FINAL_FIGURES)) {
  dir.create(
    OUTPUT_DIRECTORY,
    recursive = TRUE,
    showWarnings = FALSE
  )

  ggplot2::ggsave(
    filename = file.path(
      OUTPUT_DIRECTORY,
      "figure_2_selected_ROC_curves.png"
    ),
    plot = manuscript_figure_2,
    width = 8.0,
    height = 4.4,
    units = "in",
    dpi = 300,
    bg = "white"
  )

  ggplot2::ggsave(
    filename = file.path(
      OUTPUT_DIRECTORY,
      "supplementary_figure_S5_NLR_components.png"
    ),
    plot = supplementary_figure_s5,
    width = 8.0,
    height = 6.0,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}


# ==============================================================================
# 26. CURATED CONSOLE OUTPUT
# ==============================================================================

print_section(
  "FIGURE 1 - POST-ADJUDICATION PARTICIPANT FLOW",
  dplyr::bind_rows(
    revised_episode_linkage_summary %>%
      dplyr::mutate(stage = "usable discharge note"),
    cohort_eligibility_summary %>%
      dplyr::mutate(stage = "clinical exclusions")
  )
)

print_section(
  "FIGURE 1 - NON-MUTUALLY-EXCLUSIVE CLINICAL EXCLUSION CATEGORIES",
  clinical_exclusion_category_summary
)

print_section(
  "FIGURE 1 - RAW CLINICAL EXCLUSION COMBINATIONS",
  clinical_exclusion_cause_combination_summary
)

print_section(
  "FIGURE 1 - FINAL PHENOTYPE AND MATCHED SAMPLE SIZES",
  participant_flow
)

print_section(
  "TABLE 1 - CONTINUOUS DESCRIPTIVE INPUTS",
  manuscript_table1_continuous
)

print_section(
  "TABLE 1 - BINARY DESCRIPTIVE INPUTS",
  manuscript_table1_binary
)

print_section(
  "TABLE 1 - CATEGORICAL DESCRIPTIVE INPUTS",
  manuscript_table1_categorical
)

print_section(
  "TABLE 1 / SUPPLEMENTARY FIGURE S1 - ANALYSIS A BALANCE",
  balance_A$table %>%
    dplyr::arrange(dplyr::desc(abs(Diff.Adj)))
)

print_section(
  "TABLE 1 / SUPPLEMENTARY FIGURE S1 - ANALYSIS B BALANCE",
  balance_B$table %>%
    dplyr::arrange(dplyr::desc(abs(Diff.Adj)))
)

print_section(
  "TABLE 2 - PRIMARY MATCHED AUROCS",
  roc_auc_results %>%
    dplyr::filter(design_id == "matched") %>%
    dplyr::arrange(analysis_id, auc_rank)
)

print_section(
  "TABLE 3 - CROSS-SETTING AUROC DIFFERENCES",
  cross_scenario_auroc_results_export %>%
    dplyr::arrange(p_value_holm, p_value)
)

print_section(
  "SUPPLEMENTARY TABLE S1 - DESCRIPTIVE INPUTS",
  supplementary_table_s1_descriptive
)

print_section(
  "SUPPLEMENTARY TABLE S1 - ABSOLUTE SMD INPUTS",
  supplementary_table_s1_smd
)

print_section(
  "SUPPLEMENTARY TABLE S2 - DESCRIPTIVE INPUTS",
  supplementary_table_s2_descriptive
)

print_section(
  "SUPPLEMENTARY TABLE S2 - ABSOLUTE SMD INPUTS",
  supplementary_table_s2_smd
)

print_section(
  "SUPPLEMENTARY TABLE S3 - UNMATCHED AUROCS",
  roc_auc_results %>%
    dplyr::filter(design_id == "unmatched") %>%
    dplyr::arrange(analysis_id, auc_rank)
)

print_section(
  "SUPPLEMENTARY TABLE S4 - COAGULATION SENSITIVITY AUROCS",
  roc_auc_results %>%
    dplyr::filter(design_id == "coagulation_sensitivity") %>%
    dplyr::arrange(analysis_id, auc_rank)
)

print_section(
  "SUPPLEMENTARY TABLE S5 - MICROBIOLOGY DESCRIPTIVE INPUTS",
  microbiology_table_descriptive
)

print_section(
  "SUPPLEMENTARY TABLE S5 - EPISODE-LEVEL ABSOLUTE SMD INPUTS",
  supplementary_table_s5_episode_smd
)

print_section(
  "SUPPLEMENTARY TABLE S5 - FINDING-SPECIFIC ABSOLUTE SMD INPUTS",
  supplementary_table_s5_finding_smd
)

print_section(
  "SUPPLEMENTARY TABLE S6 - NLR COMPONENT SPLINE ANALYSIS",
  supplementary_table_s6
)

print_section(
  "SUPPLEMENTARY FIGURES S2-S4 - ALL PAIRED DELONG INPUTS",
  roc_pairwise_delong_results %>%
    dplyr::arrange(design_id, analysis_id, p_value_holm, p_value)
)

print_section(
  "COAGULATION SENSITIVITY - MATCHING SAMPLE SIZES",
  coagulation_sensitivity_matching_sample_sizes
)

print_section(
  "COAGULATION SENSITIVITY - ANALYSIS A BALANCE",
  balance_A_coagulation_sensitivity$table
)

print_section(
  "COAGULATION SENSITIVITY - ANALYSIS B BALANCE",
  balance_B_coagulation_sensitivity$table
)

print_section(
  "ANATOMICAL-SITE VALIDATION - PRIMARY AGREEMENT",
  site_validation_summary
)

print_section(
  "ANATOMICAL-SITE VALIDATION - DISAGREEMENT PATTERNS",
  site_disagreement_patterns
)

if (!is.null(site_confusion_matrix)) {
  print_section(
    "ANATOMICAL-SITE VALIDATION - CONFUSION MATRIX",
    site_confusion_matrix
  )
}

print_section(
  "FINAL INTEGRITY CHECKS",
  dplyr::bind_rows(
    final_integrity_check,
    microbiology_validation_checks %>%
      dplyr::transmute(
        check_name = paste0("microbiology: ", check),
        validation_pass = pass
      ),
    cross_scenario_validation_checks %>%
      dplyr::transmute(
        check_name = paste0("cross-setting: ", check),
        validation_pass = pass
      ),
    coagulation_sensitivity_rematch_validation_checks %>%
      dplyr::transmute(
        check_name = paste0("coagulation sensitivity: ", check),
        validation_pass = pass
      )
  )
)


cat(
  "\nAnalysis completed. Tabular inputs were printed to the console.\n",
  if (isTRUE(SAVE_FINAL_FIGURES)) {
    paste0("Final figures were written to: ", OUTPUT_DIRECTORY, "\n")
  } else {
    "Final figure objects remain available in memory and were not written.\n"
  },
  sep = ""
)
