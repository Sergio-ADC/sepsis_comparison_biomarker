# Comparator-dependent sepsis biomarker discrimination

R code supporting the study:

**Clinical comparator dependence of inflammatory and hematologic biomarker discrimination for sepsis: a matched retrospective study using MIMIC-IV emergency department data**

## Overview

This repository contains the R code used for cohort construction, data processing, and statistical analysis in a retrospective observational study evaluating the discrimination of routine inflammatory and hematologic biomarkers for sepsis across two clinical comparator populations.

The study evaluates two primary clinical comparisons:

1. Sepsis versus controls without suspected infection.
2. Sepsis versus suspected infection without sepsis.

The same sepsis cohort is retained across both primary matched comparisons.

Eleven biomarkers are evaluated:

- C-reactive protein (CRP)
- White blood cell count (WBC)
- Absolute neutrophil count (ANC)
- Absolute lymphocyte count (ALC)
- Absolute monocyte count (AMC)
- Neutrophil-to-lymphocyte ratio (NLR)
- Platelet-to-lymphocyte ratio (PLR)
- Monocyte-to-lymphocyte ratio (MLR)
- Neutrophil-to-monocyte ratio (NMR)
- Systemic Inflammation Response Index (SIRI)
- Systemic Immune-Inflammation Index (SII)

## Repository contents

```text
.
├── data.R
├── analysis.R
├── README.md
├── .gitignore
└── LICENSE
```

This repository contains **code and documentation only**.

No MIMIC source data, patient-level derived data, clinical notes, clinical-review files, or study datasets are distributed with this repository.

## Scripts

### `data.R`

`data.R` constructs the study cohort from locally available MIMIC source exports.

Major tasks include:

- construction of the adult emergency department episode cohort;
- linkage of MIMIC-IV, MIMIC-IV-ED, and MIMIC-IV-Note data;
- selection of the index complete blood count within 0–24 hours of emergency department arrival;
- integration of complete blood count and differential measurements;
- calculation of hematologic ratios;
- identification and temporal selection of C-reactive protein measurements;
- reconstruction of suspected infection;
- derivation of structured SOFA components;
- construction of the preliminary study phenotype;
- preparation of data for clinical review;
- construction of the primary analytical cohort;
- microbiology processing;
- internal data-quality and consistency checks.

The script uses DuckDB for processing large local source files.

At the end of the workflow, `data.R` generates local analysis and clinical-review products, including:

```text
analytic_cohort_primary_unblinded.csv
clinical_adjudication_worklist_canonical.rds
clinical_adjudication_worklist_excel.xlsx
clinical_adjudication_worklist_excel_flat.csv
```

These files are generated locally and are **not included in this repository**.

### `analysis.R`

`analysis.R` performs the final statistical analyses after completion of the clinical-review step.

Major tasks include:

- integration of revised clinical-review information;
- reconstruction of revised observed SOFA components;
- final phenotype classification;
- verification and derivation of the 11 study biomarkers;
- comparison-specific profile matching;
- covariate-balance assessment;
- empirical AUROC estimation;
- DeLong confidence intervals;
- pairwise biomarker AUROC comparisons;
- Holm adjustment for multiple comparisons;
- formal cross-setting AUROC comparisons accounting for the shared sepsis cohort;
- unmatched sensitivity analyses;
- coagulation-focused sensitivity analyses with rematching;
- descriptive microbiologic analyses;
- reproducibility analysis of anatomical infection-site classification;
- post hoc analysis of NLR components using natural cubic splines;
- generation of manuscript and supplementary figure outputs;
- internal consistency and quality-control checks.

The complete workflow includes an intermediate manual clinical-review step between `data.R` and `analysis.R`. The resulting patient-level review files are used locally and are not distributed with this repository.

## Data sources

The study uses:

- MIMIC-IV version 3.1
- MIMIC-IV-ED version 2.2
- MIMIC-IV-Note version 2.2

These resources are available through PhysioNet subject to the applicable credentialing, training, and Data Use Agreement requirements.

This repository does not redistribute MIMIC data.

Users wishing to reproduce the study must obtain their own authorized access to the required MIMIC resources and create the local source exports expected by `data.R`.

Required source filenames and their corresponding processing steps are documented directly in `data.R`.

## Software

The manuscript analyses were performed using R 4.4.3.

Key software used in the analysis included:

- DuckDB 1.5.0
- pROC 1.19.0.1
- MatchIt 4.7.2
- cobalt 4.6.3
- HiGHS / highs 1.12.0-3

### Packages used by `data.R`

The principal packages used by `data.R` are:

```r
c(
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "lubridate",
  "tibble",
  "DBI",
  "duckdb",
  "openxlsx"
)
```

`openxlsx` is required for generation of the Excel clinical-review workbook.

### Packages used by `analysis.R`

```r
c(
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "purrr",
  "tibble",
  "MatchIt",
  "highs",
  "cobalt",
  "pROC",
  "ggplot2"
)
```

Base R functionality is additionally used for statistical modeling, spline functions, bootstrap procedures, and other calculations.


## Reproducibility and quality-control checks

Both scripts contain explicit checks intended to prevent silent propagation of inconsistent data.

Examples include checks for:

- missing required input files;
- missing required variables;
- duplicated episode or patient identifiers;
- invalid identifier relationships;
- invalid numeric conversions;
- unexpected phenotype categories;
- row-changing or inconsistent joins;
- inconsistent cohort sizes;
- invalid SOFA component values;
- missing matching variables;
- covariate-balance requirements;
- biomarker calculation inconsistencies;
- consistency of the shared sepsis sample across comparison settings;
- invalid anatomical-site validation inputs.

Execution stops when predefined structural assumptions are violated.

## Data privacy

No patient-level data are included in this repository.

The repository contains only R code, documentation, and repository configuration files.

Controlled-access MIMIC data, clinical-note text, patient-level derived datasets, and clinical-review files should remain in authorized local environments and should not be committed to a public repository.

The repository `.gitignore` is configured to reduce the risk of accidentally committing common data-file formats.


## License

The source code in this repository is distributed under the MIT License.

The repository license applies only to the code and documentation provided here. It does not alter the access, credentialing, licensing, or Data Use Agreement requirements governing MIMIC data.

## Contact

For questions regarding the analysis code or reproducibility workflow, please contact:

**Sergio Ayala-De la Cruz**  
Departamento de Patología Clínica  
Facultad de Medicina y Hospital Universitario “Dr. José Eleuterio González”  
Universidad Autónoma de Nuevo León  
Monterrey, Nuevo León, Mexico  

Email: sergio.ayalad@uanl.edu.mx
