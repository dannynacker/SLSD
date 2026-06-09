# SLSD: Stroboscopic Light Stimulation for Depression

Repository accompanying:

**Nacker, D., Schwartzman, D. J., Seth, A. K., et al.**
**Stroboscopic Light Stimulation for Depression: Safety, Tolerability, and Feasibility in a Staged Early-Phase Study**

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

## Repository Contents

The repository is organised around the final outputs reported in the manuscript and supplementary materials.

### Main Manuscript

| File                               | Purpose                                                                                                               |
| ---------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| `manuscript_plots.txt`             | Generates all figures reported in the main manuscript.                                                                |
| `manuscript_narrative_results.txt` | Generates manuscript-facing statistical summaries, model outputs, confidence intervals, and narrative reporting text. |

### Supplementary Material

| File                       | Purpose                                                                                                                                      |
| -------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------- |
| `SM_workbook_plots.txt`    | Generates all supplementary figures, supplementary tables, workbook outputs, and source datasets used throughout the supplementary material. |
| `SM_narrative_results.txt` | Generates supplementary statistical summaries and narrative reporting text.                                                                  |

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
source("manuscript_narrative_results.txt")
source("manuscript_plots.txt")
```

This reproduces all statistical outputs and figures reported in the main manuscript.

### Supplementary Material

Run:

```r
source("SM_narrative_results.txt")
source("SM_workbook_plots.txt")
```

This reproduces all supplementary figures, tables, workbook outputs, and supplementary narrative summaries.

### Important Dependency

Figure 6 of the manuscript uses workbook outputs generated by:

```r
source("SM_workbook_plots.txt")
```

Accordingly, `SM_workbook_plots.txt` should be run before generating Figure 6 using `manuscript_plots.txt`.

All other manuscript figures are generated directly by `manuscript_plots.txt`.

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

> Nacker, D., Schwartzman, D. J., Seth, A. K., et al. *Stroboscopic Light Stimulation for Depression: Safety, Tolerability, and Feasibility in a Staged Early-Phase Study.*

---

## Contact

For questions regarding the repository, analysis code, or study methodology, please contact the corresponding author.
