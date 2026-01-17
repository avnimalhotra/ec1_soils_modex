# Regional variability in total soil carbon across coastal terrestrial–aquatic interfaces

This repository contains code and supporting materials for analyses associated with the paper:

> **Regional variability in total soil carbon across coastal terrestrial–aquatic interfaces**  
> (working title)

The project focuses on understanding the magnitude, variability, and predictors of total soil carbon (TC) across coastal terrestrial–aquatic interface (TAI) gradients (wetland–transition–upland), and on evaluating how well these patterns are represented in the E3SM land model (ELM).

Observed data come from the EXCHANGE v1 campaign  
(https://doi.org/10.1038/s41597-023-02548-7), which includes upland, transition, and wetland soils from 52 locations in the Great Lakes and Mid‑Atlantic regions of the continental United States.

---

## Project goals

1. **Quantify TC variability** across regions and along coastal TAI gradients (wetland, transition, upland).
2. **Identify predictors of TC**, focusing on region, TAI position, pH, bulk density, and related variables.
3. **Compare observations to ELM simulations**, assessing:
   - Whether ELM reproduces the magnitude and variability of observed TC.
   - Which model components (e.g., hydrology, vegetation) drive TC responses.
   - Priorities for future model development for coastal TAIs.

At present, this repository primarily contains the **statistical analysis** components. ELM configuration and post‑processing will be added in collaboration under the `elm/` directory.

---

## Repository structure

Current directory layout:

```text
./

├─ data/
│  ├─ raw/
│  │  └─ ec1_soil_v3/       # Raw soil data for this project (taken directly from ESS-DIVE)
│  └─ processed/            # Cleaned / merged datasets used directly in analyses (merges ESS-DIVE data into one file)
├─ elm/
│  ├─ configs/              # ELM configuration/namelist files (to be added)
│  ├─ input_data/           # Forcing, boundary conditions, parameter inputs (to be added)
│  ├─ postprocessing/       # Scripts to process ELM outputs (to be added)
│  └─ runs/                 # Run scripts, job submissions, logs (to be added)
├─ manuscript/              # Manuscript text, supplement, and notes (to be populated)
├─ results/
│  ├─ figures/
│  │  ├─ exploratory/       # Exploratory and diagnostic plots
│  │  └─ paper/             # Final figures used in the manuscript
│  └─ tables/               # Tables for the manuscript (usually csv outputs of stats)
└─ scripts/                 # Analysis scripts (data prep, figures and stats)