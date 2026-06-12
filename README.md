# SLSD: Stroboscopic Light Stimulation for Depression

Repository accompanying:

**Nacker, D., Seth, A. K., Schwartzman, D. J., et al. (2026).**
**Stroboscopic Light Stimulation for Depression: Safety, Tolerability, and Feasibility in a Staged Early-Phase Study**

_manuscript in preparation_

_all music by Gavin Lawson_ (https://audysseyave.com/about/)

---

## Overview

This repository contains the cleaned analysis environment, anonymised study data, and reproducible workflows used to generate the figures, tables, statistical summaries, and supplementary materials reported in the manuscript.

The project investigated the feasibility, safety, tolerability, and exploratory clinical outcomes associated with repeated sessions of stroboscopic light stimulation (SLS) in individuals experiencing depressive symptoms.

All participant data included in this repository have been de-identified and cleaned for public release. No direct participant identifiers are contained within the repository.

---

## Study Summary

Stroboscopic light stimulation (SLS) produces altered visual experiences through rhythmic photic stimulation of the visual system. While SLS has historically been investigated in the context of consciousness research, perceptual phenomenology, and neural entrainment, its potential clinical applications remain relatively underexplored.

This study evaluated the feasibility, safety, tolerability, and exploratory clinical outcomes of repeated SLS exposure in individuals experiencing depressive symptoms. Participants completed multiple stimulation sessions alongside assessments of depressive symptoms, anxiety, wellbeing, subjective experience, adverse effects, and treatment expectancy.

The analyses contained within this repository support the results reported in the accompanying manuscript and supplementary materials.

---

## Trial Registration

The staged SLSD/MRC programme was prospectively registered through ISRCTN for the relevant study stages:

| Study Stage | Trial Registration |
|---|---|
| WP1 Parameter-Testing Stage | [ISRCTN82430224](https://www.isrctn.com/ISRCTN82430224) |
| WP2 Randomised Feasibility Study | [ISRCTN13880276](https://www.isrctn.com/ISRCTN13880276) |

The registration records provide the formal trial-registry entries for the corresponding study stages. The present repository contains the cleaned public-facing analysis code, anonymised data, music stimulus materials, exported figures, and supplementary outputs used to support the accompanying manuscript.

---

## Repository Branches

This repository is organised across separate branches for analysis code, anonymised public data, music stimulus materials, and exported figure files.

| Branch     | Purpose                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| ---------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `analyses` | Cleaned R scripts, supplementary workbook, and supporting notebooks used to generate manuscript figures, supplementary tables, narrative summaries, and audit outputs.                                                                                                                                                                                                                                                                       |
| `data`     | De-identified and cleaned public study datasets required by the analysis scripts.                                                                                                                                                                                                                                                                                                                                                            |
| `music`    | Audio stimulus materials used as musical accompaniment during the interim bridge and WP2 study sessions. This branch contains `mrc_interim.mp3` and `mrc_intervention.mp3`, created by Gavin Lawson. These files are included for stimulus transparency and should be treated as authored musical works rather than participant data or analysis code.                                                                                       |
| `figures`  | Exported PNG versions of the six manuscript figures and the plots shown throughout the supplementary material narrative. These files are provided for visual inspection, review, and submission convenience; the reproducible source of truth remains the `analyses` branch together with the de-identified data in the `data` branch. Supplementary workbook tabs, source tables, audit CSVs, and intermediate outputs are not stored here. |

---

## Stimulus Materials and Hardware Context

### Music and isochronic stimulation

The `music` branch contains the audio files used as musical accompaniment during the MRC/SLSD study sessions:

```text
mrc_interim.mp3
mrc_intervention.mp3
```

These tracks were created by **Gavin Lawson** (https://audysseyave.com/about/) and are included for transparency around the auditory context of the study. The music was used alongside programmed stroboscopic light stimulation rather than as a standalone intervention. In the study sequences, rhythmic/isochronic stimulation was implemented through the light stimulation parameters, with the music providing a structured affective and temporal context for the session.

The audio files should be treated as authored musical stimulus materials, not participant data, analysis code, or derived numerical outputs. Reuse, redistribution, remixing, or adaptation outside the context of reviewing or reproducing this study may require permission from the relevant rights holder(s), unless an explicit licence is provided elsewhere in the repository.

### RX1 stimulation device and sequence documentation

The study sessions were delivered using a **Roxiva RX1** stroboscopic light stimulation device. This repository does **not** include RX1 device sequence files, device-control code, firmware, or instructions for programming or modifying the RX1. Instead, the repository provides study-facing descriptions and derived summaries of the stimulation sequences sufficient to document the intended experimental conditions.

The shared sequence information should therefore be interpreted as **scientific stimulus documentation** rather than a complete hardware-control package. It describes the timing, condition structure, and relevant stimulation parameters used in the study, but it is not intended to reproduce device-specific implementation details or to certify physical equivalence across RX1 units or other stroboscopic devices.

Exact delivered light output may depend on device firmware, hardware configuration, LED behaviour, brightness calibration, participant setup, and the device’s internal handling of stimulation instructions. For this reason, the repository supports transparency around the experimental stimulus design while avoiding redistribution of proprietary, device-specific, or non-public implementation materials.

---

## Repository Contents

The repository is organised around the final outputs reported in the manuscript and supplementary materials.

### Main Manuscript

| File                               | Purpose                                                                                                               |
| ---------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| `manuscript_plots_cleaned.r`             | Generates all figures reported in the main manuscript.                                                                |
| `manuscript_narrative_results_cleaned.r` | Generates manuscript-facing statistical summaries, model outputs, confidence intervals, and narrative reporting text. |

### Supplementary Material

| File                       | Purpose                                                                                                                                      |
| -------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------- |
| `SM_workbook_plots_cleaned.r`    | Generates all supplementary figures, supplementary tables, workbook outputs, and source datasets used throughout the supplementary material. |
| `SM_narrative_results_cleaned.r` | Generates supplementary statistical summaries and narrative reporting text.                                                                  |

### Qualitative Analyses

| File                        | Purpose                                                                                                                            |
| --------------------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| `interim_word_clouds.ipynb` | Exploratory qualitative analysis of participant experience reports using thematic keyword extraction and word-cloud visualisation. |

### Additional Study Components

| File                   | Purpose                                                                                                                                                               |
| ---------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `wp1_session_11.ipynb` | Analysis and visualisation of the WP1 Session 11 parameter-exploration study.                                                                                         |
| `wp2_EMDRATS.ipynb`    | Operational monitoring, adherence tracking, wellbeing monitoring, and stratification workflows used during WP2. Included for transparency and documentation purposes. |

---

## Included Data

This repository includes the cleaned and anonymised datasets required to reproduce the analyses reported in the manuscript and supplementary materials.

The repository therefore contains the data and code required to reproduce:

* All main manuscript figures
* All supplementary figures
* All supplementary tables
* Manuscript statistical summaries
* Supplementary statistical summaries
* Qualitative word-cloud visualisations
* WP1 Session 11 analyses

No direct participant identifiers are included.

---

## Reproducing the Study Outputs

### Main Manuscript

Run:

```r
source("manuscript_narrative_results_cleaned.r")
source("manuscript_plots_cleaned.r")
```

This reproduces all statistical outputs and figures reported in the main manuscript.

### Supplementary Material

Run:

```r
source("SM_narrative_results_cleaned.r")
source("SM_workbook_plots_cleaned.r")
```

This reproduces all supplementary figures, tables, workbook outputs, and supplementary narrative summaries.

## Figure Exports

The `figures` branch contains exported PNG figure files corresponding to the manuscript and supplementary material. This includes:

* the six main manuscript figures;
* the plots shown throughout the supplementary material narrative.

These exported figures are included as convenience outputs for review, submission checking, and visual inspection. They are not the primary reproducibility layer. The scripts used to generate the figures are stored in the `analyses` branch, and the de-identified datasets used by those scripts are stored in the `data` branch.

The `figures` branch intentionally does not contain supplementary workbook tabs, source tables, audit CSVs, or intermediate analysis outputs.

---

### Important Dependency

Figure 6 of the manuscript uses workbook outputs generated by:

```r
source("SM_workbook_plots_cleaned.r")
```

Accordingly, `SM_workbook_plots_cleaned.r` should be run before generating Figure 6 using `manuscript_plots_cleaned.r`.

All other manuscript figures are generated directly by `manuscript_plots_cleaned.r`.

---

## Software Requirements

### R

The primary analysis environment was developed in R.

Core packages used throughout the project include:

* tidyverse
* ggplot2
* dplyr
* tidyr
* readr
* lme4
* lmerTest
* emmeans
* openxlsx
* patchwork
* scales
* broom
* broom.mixed

Additional packages may be loaded within individual scripts as required.

### Python

The Jupyter notebooks require a standard scientific Python environment.

Typical dependencies include:

* pandas
* numpy
* scipy
* matplotlib
* seaborn
* wordcloud
* jupyter

---

## Repository Philosophy

The original project analysis environment evolved over multiple work packages, pilot studies, manuscript revisions, and supplementary analyses. The files included in this repository represent cleaned analysis pathways extracted from the larger project environment to improve transparency, reproducibility, and ease of navigation.

Where possible, superseded analyses, abandoned figure-generation routes, intermediate debugging code, and redundant outputs have been removed.

The goal of this repository is to provide a clear and reproducible pathway from anonymised study data to the final figures, tables, and statistical outputs reported in the manuscript.

---

## Repository Notes and Limitations

### Data Cleaning

All datasets included in this repository have been cleaned and anonymised for public release. Direct identifiers, metadata fields, and other information that could reasonably contribute to participant identification have been removed prior to publication.

### Historical Variable Names

Several source datasets contain typographical inconsistencies in variable names originating from the original data-collection instruments and survey platforms.

Examples include variables such as:

* `fisber_*` (rather than `fibser_*`)
* `n32apr20` (rather than `Interim_Session_Feedback_XXX`)
* other legacy naming conventions retained from the original study databases

Where possible, analysis scripts account for these inconsistencies directly. Variable names were generally retained to preserve compatibility with the original analysis environment and to minimise the risk of introducing errors during retrospective data cleaning.

### Calendar-Based Analyses

A small number of workflows in the original project relied on internal calendar and scheduling records to calculate attendance, booking events, scheduling metrics, and operational study-management outcomes.

These calendar files are not included in the public repository because they contain information that cannot be fully anonymised without compromising participant confidentiality.

Consequently, analyses requiring the original calendar records cannot be reproduced directly from the public repository. All manuscript figures, supplementary figures, supplementary tables, and reported statistical analyses are reproducible without access to these files.

These files were used for operational study management rather than primary outcome analyses.

### Fonts

Some figures were originally generated using Palatino Linotype. Because redistribution rights for this font may vary across systems and software installations, font files are not included in this repository.

Figures should reproduce correctly using available system fonts, although minor differences in typography may occur across operating systems.

---

## Citation

If you use this repository, please cite the accompanying manuscript:

> Nacker, D., Seth, A. K., Schwartzman, D. J., et al. (2026). *Stroboscopic Light Stimulation for Depression: Safety, Tolerability, and Feasibility in a Staged Early-Phase Study.*

---

## Contact

For questions regarding the repository, analysis code, or study methodology, please contact the corresponding author.
