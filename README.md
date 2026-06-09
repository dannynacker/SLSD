# SLSD: Stroboscopic Light Stimulation for Depression

Repository accompanying:

**Nacker, D., Schwartzman, D. J., Seth, A. K., et al.**
*Stroboscopic Light Stimulation for Depression: Safety, Tolerability, and Feasibility in a Staged Early-Phase Study.*

---

## Overview

This repository contains the cleaned analysis environment, anonymised study data, and reproducible workflows used to generate the figures, tables, supplementary materials, and exploratory qualitative analyses reported in the manuscript.

The project examined the feasibility, safety, tolerability, and exploratory clinical outcomes associated with repeated sessions of stroboscopic light stimulation (SLS) in individuals experiencing depressive symptoms.

All participant data included in this repository have been de-identified and cleaned for public release.

---

## Repository Structure

### Manuscript Outputs

| File                               | Description                                                             |
| ---------------------------------- | ----------------------------------------------------------------------- |
| `manuscript_plots.txt`             | Generates all manuscript figures.                                       |
| `manuscript_narrative_results.txt` | Produces manuscript-facing statistical summaries and narrative outputs. |

### Supplementary Materials

| File                       | Description                                                                  |
| -------------------------- | ---------------------------------------------------------------------------- |
| `SM_workbook_plots.txt`    | Generates supplementary tables, workbook outputs, and supplementary figures. |
| `SM_narrative_results.txt` | Produces supplementary narrative summaries and reporting outputs.            |

### Qualitative Analyses

| File                        | Description                                                                                 |
| --------------------------- | ------------------------------------------------------------------------------------------- |
| `interim_word_clouds.ipynb` | Exploratory thematic visualisation of interim participant reports using word-cloud methods. |

### Additional Study Components

| File                   | Description                                                                                                                                     |
| ---------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| `wp1_session_11.ipynb` | Analysis and visualisation of Session 11 parameter exploration data from WP1.                                                                   |
| `wp2_EMDRATS.ipynb`    | Participant monitoring, wellbeing tracking, and stratification workflows used during WP2. Included for transparency and documentation purposes. |

### Data

The repository includes cleaned and anonymised datasets required to reproduce the manuscript and supplementary analyses.

Data files contain no direct participant identifiers.

---

## Reproducing the Manuscript

### Main Manuscript Figures

Run:

```r
source("manuscript_plots.txt")
```

This script generates all figures reported in the main manuscript.

### Manuscript Statistical Outputs

Run:

```r
source("manuscript_narrative_results.txt")
```

This script generates the statistical summaries used throughout the manuscript.

### Supplementary Materials

Run:

```r
source("SM_workbook_plots.txt")
source("SM_narrative_results.txt")
```

These scripts generate supplementary figures, tables, workbook outputs, and narrative summaries.

---

## Software Requirements

### R

The analysis scripts primarily use:

* tidyverse
* lme4
* lmerTest
* emmeans
* ggplot2
* patchwork
* scales
* openxlsx

Additional packages may be loaded within individual scripts.

### Python

The Jupyter notebooks require a standard scientific Python environment including:

* pandas
* numpy
* matplotlib
* scipy
* wordcloud

---

## Study Components Represented in this Repository

The repository contains outputs relating to:

* Feasibility and recruitment
* Attendance and adherence
* Safety and tolerability
* Clinical outcome measures
* Acute subjective experience measures
* Exploratory qualitative analyses
* WP1 stimulation-parameter exploration

---

## Notes

The files in this repository represent cleaned analysis pathways extracted from the full project environment. Intermediate development code, abandoned analyses, and superseded figure-generation routes have been removed where possible to improve clarity and reproducibility.

Some scripts generate publication-ready figures directly. Others generate workbook outputs that serve as source material for supplementary reporting.

---

## Contact

For questions regarding the repository or study methodology, please contact the corresponding author.
