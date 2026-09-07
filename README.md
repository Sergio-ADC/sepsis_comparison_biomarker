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
└── README.md
