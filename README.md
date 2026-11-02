------------------------------------------------------------------------

# Before Reproducibility and Replication comes Reflection: A Re-examination of the Relationship between Soccer Referee and Player Physical Match Performances

Repository for the data and code for the above-named project.

### Authors

- **Matthew Weston**
- Institute of Sport, Manchester Metropolitan University, Manchester, UK.
- **ORCID:** <https://orcid.org/0000-0002-9531-3004>

- **Seb Bate**
- Centre for Digital Data Research, Manchester Metropolitan University, Manchester, UK.
- **ORCID: ** <https://orcid.org/0000-0002-2218-503X>

E-mail: M.Weston@mmu.ac.uk
------------------------------------------------------------------------

## Project Description

This project contains data and analysis scripts for a reflection project whereby I assessed the validity of the conclusions presented by Weston et al. (2011) that concluded referees' physical match performances were inter-related with those of the players on the same match.

### Included Files:

- `scripts/` and `R/`directories with all code
- `osf/data.syn.csv`: Synthetic dataset used for the analysis.

------------------------------------------------------------------------

## Software and Platform

- **Language:** R (Version 4.6.0)
- **Data & Plotting Packages:** `tidyverse`, `ggeasy`, `visdat`, `lemon`, `sjPlot`, `scales`, `patchwork`
- **Statistical Packages:** `lme4`, `lmertest`, `mgcv`, `MuMIn`, `gratia`, `broom.mixed`, `emmeans`, `parameters`
- **Data Import & Synthesis Packages:** `readxl`, `synthpop`, `MASS`, `broom`
- **Parallel Processing & Progress Packages:** `future`, `future.apply`, `progressr`

------------------------------------------------------------------------

## How to Run the Synthetic Analysis

To replicate the findings and generate the figures, follow these steps:

1.  **Download the Files:** Download and extract all files into the same local folder on your computer. Ensure that you preserve the structure.
2.  **Open the Script:** Open `scripts/synthetic_test.R`.
3.  **Set Working Directory:** Ensure your RStudio working directory is set to the folder containing both the script and the `data.syn.csv` file.
4.  **Install Packages:** Ensure all packages listed in the "Software and Platform" section above are installed.
5.  **Run the test file:** you can set the replicate number from 1 to 10 in the header of the file. This will allow you to verify the outputs run cleanly on the main path.
6. **Pool estimates:** `scripts/synthetic_test_parallel.R` will allow you to run the models over all 10 rpelicates to get the pooled estimates. **This is very slow and will take 3+ hours to run**.

## Notes on the structure

The main analysis code is executes from `scripts/models_original.R` to build the models and then `scripts/analysis_original.R` to analyse it. The corresponding `..._synthetic.R` files run the same pipelines but for the replicates. They all depend on the five files in `R/` for the shared functions. `scripts/synthetic_test.R` and `scripts/synthetic_test_pooled.R` act as wrappers to run the analysis through osf using the synthetic data only. Hence in the vanilla `scripts/synthetic_test.R`, you can pick any replicate number (1-10) and that it runs the code through the `..._origninal.R` pipeline.