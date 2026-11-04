# Psychometric Analysis of an Agency Instrument for Sustainable Development

## Project Overview

This repository documents the R scripts developed for the statistical and psychometric analysis of an assessment instrument designed to measure agency as a component of sustainable development competence among secondary school students.

The analyses were conducted as part of my undergraduate thesis in Psychology at the National Autonomous University of Mexico (UNAM), completed in 2025:

*Agency as a Construct of Sustainable Development Competence: Test Development and Analysis.*

The study involved a sample of 502 secondary school students and focused on the development and evaluation of an instrument intended to assess agency across different sustainability-related contexts.

## Theoretical Framework

The conceptualization of agency underlying this instrument draws on the OECD Learning Compass 2030 framework and scientific literature on agency and related psychological constructs.

Based on these theoretical foundations, I developed an original conceptual model of agency organized into seven subconstructs:

- Co-agency
- Identity and sense of belonging
- Motivation, self-determination, autonomy, and control
- Hope
- Self-efficacy, self-regulation, anticipation, self-reflection, and control
- Growth mindset
- Sense of purpose

These dimensions informed the development of the instrument and the organization of its items across four sustainability-related scenarios.

## Analytical Approach

The statistical analyses were conducted in R and included:

- Descriptive statistics and frequency distributions.
- Analysis of total scores across the four test cases.
- Classical Test Theory analyses.
- Exploratory Item Response Theory (IRT) modeling.
- Estimation of Generalized Partial Credit Models (GPCM).
- Exploration of alternative multidimensional factor structures.
- Examination of model fit, parameter estimates, and latent trait scores.

The `mirt` package was used for IRT model specification and estimation.

## Repository Structure

The repository is organized into separate R scripts documenting different stages of the analytical process.

```text
sustainability-agency-psychometric-analysis/
│
├── README.md
├── .gitignore
│
└── R/
    ├── 01_descriptive_analysis.R
    ├── 02_CTT_Analysis.R
    ├── 03_unidimensional_analysis.R
    ├── 04_exploratory_irt_model_case_A.R
    └── 05_exploratory_multidimensional_irt_all_cases.R
    └──...
```

Additional scripts and materials may be added as the repository develops.

## Data Availability and Reproducibility

The original dataset is not included in this repository because it contains individual responses from student participants and is subject to confidentiality considerations.

Consequently, the scripts document the analytical procedures but cannot be executed independently without authorized access to the original dataset.

## Scope and Limitations

This repository documents the statistical and psychometric work conducted during an undergraduate research project.

The exploratory models represent different stages of the analytical process and should not be interpreted as equivalent to the final model reported in the thesis. The scripts are shared to document the analytical procedures, model specifications, and exploratory work undertaken during the project.

## Author and Supervision

**Author:** Daniela García  
**Institution:** Faculty of Psychology, National Autonomous University of Mexico (UNAM)  
**Degree:** Bachelor's Degree in Psychology  
**Thesis Completion:** 2025  
**Thesis Advisor:** Dr. Iwin Leenen
