# SLSD: Stroboscopic Light Stimulation for Depression

Repository accompanying:

**Nacker, D., Schwartzman, D. J., Seth, A. K., et al. *Stroboscopic Light Stimulation for Depression: Safety, Tolerability, and Feasibility in a Staged Early-Phase Study.***

Manuscript in preparation / under submission.

---

## Overview

This repository contains the cleaned public-facing data and analysis workflows used to support the manuscript and supplementary material for a staged early-phase programme evaluating **stroboscopic light stimulation (SLS)** for adults reporting depressive symptoms.

The project examined whether supervised SLS could be delivered safely, tolerably, and feasibly across three linked stages:

1. **Work Package 1 (WP1):** single-session parameter testing in adults reporting depressive symptoms.
2. **Interim bridge study:** control-condition calibration and experiential separation testing.
3. **Work Package 2 (WP2):** a randomised feasibility study of four weekly supervised SLS sessions comparing an intervention sequence with a low-phenomenology control sequence.

The analyses are feasibility-first and estimation-focused. Clinical outcomes are exploratory and hypothesis-generating rather than confirmatory tests of efficacy.

The repository is organised around the final manuscript, supplementary material, supplementary workbook, and supporting audit outputs.

---

## Repository Branches

This repository uses separate branches for analysis code and public data.

| Branch     | Purpose                                                                                                                                                                |
| ---------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `analyses` | Cleaned R scripts, supplementary workbook, and supporting notebooks used to generate manuscript figures, supplementary tables, narrative summaries, and audit outputs. |
| `data`     | De-identified and cleaned public data files required by the analysis scripts.                                                                                          |

Because GitHub branches are separate snapshots of the repository, the easiest way to reproduce outputs is to download or clone both branches into separate folders, then point the analysis scripts to the local data folder using `MRC_DATA_DIR`.

Example:

```bash
git clone -b analyses https://github.com/dannynacker/SLSD.git SLSD_analyses
git clone -b data https://github.com/dannynacker/SLSD.git SLSD_data
```

Then in R:

```r
Sys.setenv(MRC_DATA_DIR = "path/to/SLSD_data")
setwd("path/to/SLSD_analyses")
```

On Windows, for example:

```r
Sys.setenv(MRC_DATA_DIR = "C:/Users/yourname/Desktop/SLSD_data")
setwd("C:/Users/yourname/Desktop/SLSD_analyses")
```

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

The study sessions were delivered using a **Roxiva RX1** stroboscopic light stimulation device (https://roxiva.com/about/). This repository does **not** include RX1 device sequence files, device-control code, firmware, or instructions for programming or modifying the RX1. Instead, the repository provides study-facing descriptions and derived summaries of the stimulation sequences sufficient to document the intended experimental conditions.

The shared sequence information should therefore be interpreted as **scientific stimulus documentation** rather than a complete hardware-control package. It describes the timing, condition structure, and relevant stimulation parameters used in the study, but it is not intended to reproduce device-specific implementation details or to certify physical equivalence across RX1 units or other stroboscopic devices.

Exact delivered light output may depend on device firmware, hardware configuration, LED behaviour, brightness calibration, participant setup, and the device’s internal handling of stimulation instructions. For this reason, the repository supports transparency around the experimental stimulus design while avoiding redistribution of proprietary, device-specific, or non-public implementation materials.

---

## Main Analysis Files

The main public analysis workflows are in the `analyses` branch.

| File                                  | Main role                                                                                                                                                                             | Main outputs                                                                                                      |
| ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------- |
| `MRC_manuscript_plots_cleaned.r`      | Generates the six main manuscript figures.                                                                                                                                            | Figure PNG/PDF files in a manuscript-plot output folder.                                                          |
| `MRC_manuscript_narrative_cleaned.r`  | Reruns/reuses the manuscript figure calculation environment and exports manuscript-facing results prose and audit values.                                                             | `Results_Narrative_AutoGenerated.txt`; `Results_Narrative_Audit_Values.csv`.                                      |
| `MRC_SM_workbook_and_plots_cleaned.r` | Generates supplementary workbook source tables, supplementary-only plots, section-specific workbooks, audit files, and supporting outputs used throughout the supplementary material. | Supplementary plots, CSV audit files, section workbooks, consolidated supplementary workbook outputs.             |
| `MRC_SM_narrative_cleaned.r`          | Reruns or collects supplementary narrative outputs generated by the supplementary calculation script.                                                                                 | `SM_Narrative_AutoGenerated.txt`; `SM_Narrative_Source_Audit.csv`.                                                |
| `MRC_SM v1.5.xlsx`                    | Static submitted supplementary workbook containing extended tables, notes, source values, and model outputs.                                                                          | Human-readable workbook used alongside the supplementary material.                                                |
| `interim_word_clouds.ipynb`           | Exploratory qualitative word-cloud notebook for interim participant experience reports.                                                                                               | Word-cloud plots / qualitative visual summaries.                                                                  |
| `wp1_session_11.ipynb`                | Additional notebook for WP1 Session 11 parameter-exploration analyses.                                                                                                                | Session-specific exploratory plots and checks.                                                                    |
| `wp2_EMDRATS.ipynb`                   | Operational WP2 notebook used during trial conduct for stratification, allocation monitoring, adherence tracking, wellbeing monitoring, and safety/adherence workflows.               | Operational monitoring outputs; included for transparency, not as the primary manuscript figure-generation route. |

---

## Public Data Files

The `data` branch contains de-identified source files used by the analysis workflows. Direct participant identifiers, contact details, calendar metadata, and ID-to-contact mappings are not included.

### WP1 files

| File                                               | Description                                                                                                                                                                     | Used for                                                                                                           |
| -------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| `WP1 Sign-up_February 17, 2026_09.54.csv`          | WP1 screening/sign-up dataset. Includes eligibility, demographic, and baseline screening information for the WP1 stage.                                                         | WP1 recruitment flow, screened/tested/analysed denominators, demographic summaries, screening-stage audit outputs. |
| `WP1 Testing Start_February 17, 2026_09.54.csv`    | WP1 pre-session / testing-start dataset. Includes baseline measures collected immediately before in-person WP1 testing.                                                         | WP1 baseline clinical summaries, pre/post mood analyses, participant reconciliation.                               |
| `WP1 Session Feedback_February 17, 2026_09.54.csv` | WP1 post-session/session-feedback dataset. Includes discomfort/tolerability ratings, visual effects, symptom checks, and session-level responses across WP1 SLS parameter sets. | Main manuscript Figure 2, WP1 safety/tolerability summaries, Table S3.2-style supplementary tolerability outputs.  |
| `WP1+Testing+End_March+3,+2025_02.36.csv`          | WP1 testing-end dataset. Contains post-testing symptom or mood-related measures where available.                                                                                | WP1 pre/post mood and symptom-change checks, supplementary WP1 clinical/mood summaries.                            |

### Interim bridge-study files

| File                                       | Description                                                                                                                         | Used for                                                                                                                                                                            |
| ------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Interim Sign-Up_April 22, 2026_08.31.csv` | Interim bridge-study sign-up / pre-screening dataset.                                                                               | Interim recruitment funnel, eligibility, demographic summaries, denominator audits.                                                                                                 |
| `n32apr20.csv`                             | Interim analysed dataset. The filename is a legacy internal name retained for compatibility with the original analysis environment. | Main manuscript Figure 3, 6D-VHQ analyses, immediate experiential ratings, ranking analyses, interim control-calibration summaries, qualitative word-cloud inputs where applicable. |

### WP2 files

| File                                            | Description                                                                                                                                                                                              | Used for                                                                                                                                                         |
| ----------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `wp2_pre_screen_March 6, 2026_09.54.csv`        | WP2 screening and eligibility dataset. Includes depressive-symptom eligibility, demographic/safety-screening variables, medication-stratification inputs, and screening-stage information.               | WP2 recruitment flow, demographics, stratification variables, screening and eligibility audit outputs.                                                           |
| `wp2_assignments.csv`                           | WP2 randomisation/allocation dataset. Includes participant IDs, allocation condition, strata, and blinded sequence names.                                                                                | WP2 arm assignment, feasibility summaries, attendance/adherence summaries, main manuscript Figures 1, 4, 5, and 6.                                               |
| `wp2_parameters.xlsx`                           | WP2 stimulation-parameter / sequence-support file.                                                                                                                                                       | Documentation and reproducibility of intervention/control sequence structure and parameter-level information where needed.                                       |
| `wp2_pre_session_1_March 6, 2026_09.54.csv`     | WP2 baseline/pre-Session 1 dataset. Includes baseline clinical measures, expectancy/credibility items, and Session 1 pre-stimulation information.                                                        | Baseline PHQ-9, BDI-II, BAI, MADRS-S, expectancy analyses, candidate-predictor models, exploratory clinical summaries.                                           |
| `wp2_pre_sessions_2-4_March 6, 2026_09.54.csv`  | WP2 pre-session dataset for Sessions 2–4. Includes repeated pre-session clinical/wellbeing measures and follow-up side-effect/FIBSER/FISBER-style fields from prior sessions.                            | Repeated PHQ-9/M3VAS/SPANE trajectories, side-effect follow-up summaries, FIBSER/FISBER tables, adherence and missingness summaries.                             |
| `wp2_post_sessions_1-3_March 6, 2026_09.55.csv` | WP2 post-session dataset for Sessions 1–3. Includes post-session discomfort/tolerability, acute subjective-experience measures, visual phenomenology, side-effect fields, and related immediate ratings. | Main manuscript Figure 4, WP2 tolerability summaries, acute-experience predictors, 6D-VHQ/11-ASC/EXP4-style summaries, safety/tolerability supplementary tables. |
| `wp2_post_session_4_March 6, 2026_09.55.csv`    | WP2 post-Session 4 / endpoint dataset. Includes endpoint symptom measures and final post-treatment assessments.                                                                                          | Main manuscript Figures 4, 5, and 6; PHQ-9 and BDI-II endpoint summaries; exploratory clinical outcomes; response/remission and candidate-predictor analyses.    |
| `wp2_sms_day1,3,5_March 6, 2026_09.54.csv`      | WP2 SMS follow-up dataset collected 1, 3, and 5 days after Sessions 1–3. Includes short-form wellbeing/symptom monitoring variables.                                                                     | SMS adherence summaries, repeated-measures PHQ-9/M3VAS/SPANE trajectories, safety/wellbeing monitoring summaries.                                                |
| `wp2_sms_post_March 6, 2026_09.54.csv`          | WP2 final SMS / one-week follow-up dataset after Session 4.                                                                                                                                              | Final follow-up symptom/wellbeing summaries, retention and missingness summaries, repeated-measures sensitivity analyses.                                        |
| `Drop out questions_March 6, 2026_09.55.csv`    | Dropout/withdrawal questionnaire dataset. Includes available participant-reported reasons for discontinuation or missed completion.                                                                      | WP2 dropout-reason summaries, feasibility/adherence supplementary tables, retention narrative.                                                                   |

---

## What Each Main Script Generates

### 1. `MRC_manuscript_plots_cleaned.r`

This script generates the main figures reported in the manuscript.

Expected manuscript figures:

| Figure   | Focus                                                                                         | Main data sources                                                                                                                                |
| -------- | --------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| Figure 1 | Programme-level flow and WP2 allocation/follow-up.                                            | WP1 sign-up/testing/session feedback; interim sign-up/analysed files; WP2 pre-screen, assignment, pre/post-session files.                        |
| Figure 2 | WP1 safety and tolerability across parameter-testing sessions.                                | `WP1 Session Feedback...csv`.                                                                                                                    |
| Figure 3 | Interim bridge-study experiential separation and control-calibration analyses.                | `n32apr20.csv`; interim sign-up file where needed for denominators.                                                                              |
| Figure 4 | WP2 feasibility, retention, session completion, and repeated-session discomfort/tolerability. | `wp2_assignments.csv`, WP2 pre-session files, WP2 post-session files.                                                                            |
| Figure 5 | Exploratory depressive-symptom outcomes over WP2.                                             | WP2 baseline, repeated pre-session, post-session, and follow-up files.                                                                           |
| Figure 6 | Exploratory arm-specific BDI-II trajectories by total 11-ASC acute-experience tertile.        | WP2 baseline expectancy, post-session acute-experience measures, endpoint BDI-II; also supported by supplementary workbook/source-table outputs. |

The script saves manuscript figure outputs as PNG and PDF files. The exact output folder is set inside the script, normally under:

```text
<MRC_DATA_DIR>/manuscript_plots/
```

or the script-defined manuscript plot folder.

---

### 2. `MRC_manuscript_narrative_cleaned.r`

This script reruns or sources the manuscript plotting/calculation script, then exports manuscript-facing narrative summaries and audit values.

Main purpose:

* regenerate the numerical objects used by the manuscript figures;
* collect the numerical values needed for Results text;
* produce an audit CSV of reported values;
* generate a manuscript-facing narrative text file.

Expected outputs include:

```text
MRC_public_outputs/manuscript_narrative/Results_Narrative_AutoGenerated.txt
MRC_public_outputs/manuscript_narrative/Results_Narrative_Audit_Values.csv
```

This script is useful for checking that manuscript prose, figure captions, and reported numerical values remain aligned with the current analysis outputs.

---

### 3. `MRC_SM_workbook_and_plots_cleaned.r`

This is the broadest supplementary analysis script. It stages and exports supplementary workbook tables, supplementary figures, section-specific audit files, and some narrative source files.

It is not intended to generate the six main manuscript figures directly, although some outputs support main manuscript reconciliation, especially Figure 6.

Major sections include:

| Supplementary area                         | What the script does                                                                                                                                                      |
| ------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| WP1 demographics                           | Reconciles WP1 screened/tested/analysed denominators; resolves duplicate and corrected participant IDs; exports demographic tables and denominator audits.                |
| WP1 tolerability                           | Generates cleaned WP1 tolerability tables, participant/session audit files, symptom-category summaries, and Table S3.2-style outputs.                                     |
| WP1 pre/post mood checks                   | Summarises WP1 pre/post PHQ-9, M3VAS, BDI-II, and related mood/symptom measures where available.                                                                          |
| Interim demographics and denominator audit | Summarises interim bridge-study recruitment, eligibility, demographic, and denominator information.                                                                       |
| Interim experiential analyses              | Summarises 6D-VHQ, immediate ratings, ranking outcomes, and control-calibration outputs.                                                                                  |
| WP2 demographics                           | Summarises WP2 screening, baseline, analysed-sample demographics, PHQ-9 severity strata, and medication strata.                                                           |
| WP2 feasibility/adherence/missingness      | Generates attendance, retention, questionnaire-completion, and expected-vs-observed data-collection event summaries.                                                      |
| WP2 safety/tolerability                    | Cleans and summarises side-effect endorsements, FIBSER/FISBER-style follow-ups, VDQ/discomfort ratings, and participant/session-level tolerability tables.                |
| WP2 clinical outcomes                      | Generates exploratory PHQ-9, BDI-II, MADRS-S, BAI, M3VAS, SPANE, response/remission, MID benchmark, and baseline-adjusted endpoint summaries.                             |
| Expectancy and candidate predictors        | Summarises baseline expectancy and exploratory candidate-predictor analyses using 6D-VHQ, 11-ASC, and related acute-experience measures.                                  |
| Figure 6 ASC predictors                    | Adds or exports the source table for manuscript Figure 6, including ASC tertiles, BDI-II improvement, plotted baseline-aligned values, and continuous model coefficients. |
| Consolidated workbook builder              | Collects selected tables into supplementary workbook-style outputs for inspection and submission support.                                                                 |

Typical output roots:

```text
<MRC_DATA_DIR>/MRC_public_outputs/SM_workbook/
<MRC_DATA_DIR>/MRC_public_outputs/SM_plots/
<MRC_DATA_DIR>/MRC_public_outputs/audit_SM_workbook/
```

Many section-specific outputs are also preserved under section-specific folders for traceability.

---

### 4. `MRC_SM_narrative_cleaned.r`

This script collects the supplementary narrative text outputs generated by the supplementary analysis environment.

Main purpose:

* rerun the supplementary workbook/plot script if requested;
* search for expected narrative text outputs;
* combine them into a single supplementary narrative text file;
* export a source audit showing which narrative files were found.

Expected outputs include:

```text
MRC_public_outputs/SM_narrative/SM_Narrative_AutoGenerated.txt
MRC_public_outputs/SM_narrative/SM_Narrative_Source_Audit.csv
```

The source audit is useful because it shows which supplementary sections have successfully generated narrative files and which sections may need to be regenerated or checked manually.

---

## Suggested Reproduction Order

The safest reproduction order is:

```r
# 1. Point scripts to the public data folder
Sys.setenv(MRC_DATA_DIR = "path/to/SLSD_data")

# 2. Work from the analyses folder
setwd("path/to/SLSD_analyses")

# 3. Generate supplementary workbook, supplementary plots, and Figure 6 support tables
source("MRC_SM_workbook_and_plots_cleaned.r")

# 4. Generate / collect supplementary narrative outputs
source("MRC_SM_narrative_cleaned.r")

# 5. Generate main manuscript figures
source("MRC_manuscript_plots_cleaned.r")

# 6. Generate manuscript narrative/audit outputs
source("MRC_manuscript_narrative_cleaned.r")
```

Running the supplementary workbook script first is recommended because the final Figure 6 support table is generated as part of the supplementary workbook pathway.

---

## Expected Output Structure

Depending on the local configuration, the scripts will create output folders similar to:

```text
MRC_public_outputs/
  manuscript_narrative/
    Results_Narrative_AutoGenerated.txt
    Results_Narrative_Audit_Values.csv

  SM_workbook/
    [supplementary workbook tables and section exports]

  SM_plots/
    [supplementary figure outputs]

  SM_narrative/
    SM_Narrative_AutoGenerated.txt
    SM_Narrative_Source_Audit.csv

  audit_SM_workbook/
    [audit and reconciliation files]
```

The manuscript plotting script also writes manuscript figure files to a manuscript-plot output folder, typically:

```text
manuscript_plots/
  Figure1_ProgrammeFlow_CONSORT_synthesised_full.png
  Figure1_ProgrammeFlow_CONSORT_synthesised_full.pdf
  [Figures 2-6 as PNG/PDF outputs]
```

Some legacy or section-specific output folders may also be created because the public scripts preserve the original section-level output names for traceability.

---

## Supplementary Workbook

The file:

```text
MRC_SM v1.5.xlsx
```

is the static supplementary workbook corresponding to the submitted supplementary material. It contains extended tables, notes, source summaries, and model outputs that support the supplementary text.

The workbook is intended for human inspection and manuscript review. The R scripts also generate CSV/XLSX source tables and workbook-style outputs used to reconstruct or audit many of the workbook tabs.

Important tabs include, but are not limited to:

* demographics and denominator summaries;
* WP1 tolerability and mood-check summaries;
* interim bridge-study summaries;
* WP2 feasibility, adherence, and missingness summaries;
* WP2 safety and tolerability tables;
* exploratory clinical outcome summaries;
* expectancy and candidate-predictor outputs;
* Figure 6 ASC predictor source values;
* scale definitions and notes.

---

## Software Requirements

### R

The main reproducible analysis environment is R.

Core packages used across the cleaned scripts include:

```r
tidyverse
readr
dplyr
tidyr
stringr
lubridate
ggplot2
patchwork
scales
showtext
sysfonts
grid
gt
webshot2
gridExtra
ragg
writexl
openxlsx
glue
jsonlite
gtable
magick
wordcloud
tm
RColorBrewer
```

Some sections may use additional packages depending on local output settings and table-rendering options.

Recommended practice before running:

```r
packages <- c(
  "tidyverse", "readr", "dplyr", "tidyr", "stringr", "lubridate",
  "ggplot2", "patchwork", "scales", "showtext", "sysfonts", "grid",
  "gt", "webshot2", "gridExtra", "ragg", "writexl", "openxlsx",
  "glue", "jsonlite", "gtable", "magick", "wordcloud", "tm",
  "RColorBrewer"
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)
```

For exact reproducibility, use the session information deposited alongside the final analysis scripts where available.

### Python / Jupyter

The notebooks require a standard scientific Python environment.

Typical packages include:

```text
pandas
numpy
scipy
matplotlib
seaborn
wordcloud
jupyter
openpyxl
```

The Python notebooks are included for transparency around development, stratification, monitoring, and exploratory qualitative/sequence analyses. The final manuscript-facing figures and main statistical summaries are generated primarily through the R scripts unless otherwise specified.

---

## Data Cleaning and Anonymisation

All public datasets have been cleaned and de-identified prior to release.

The public repository does **not** include:

* names;
* email addresses;
* phone numbers;
* free-text contact details;
* raw calendar booking files;
* participant ID-to-contact mappings;
* directly identifying scheduling metadata;
* internal operational files that could reasonably contribute to re-identification.

Some qualitative/free-text fields may have been cleaned, summarised, removed, or transformed depending on identifiability risk.

Where participant-level data could not be shared safely, derived or aggregate outputs are provided where ethically permissible.

---

## Historical Variable Names and Typographical Inconsistencies

Some source files retain historical variable names from Qualtrics exports, earlier notebooks, or internal data-collection forms.

Examples include:

* `fisber_*` fields, corresponding to FIBSER-style follow-up information;
* legacy file name `n32apr20.csv` for the interim analysed dataset;
* mixed naming conventions for participant identifiers, session numbers, discomfort ratings, and scale items across work packages.

The analysis scripts generally account for these inconsistencies through column-detection helpers, cleaning functions, or explicit mapping logic. Variable names were not fully renamed across all files because preserving original names reduces the risk of introducing retrospective coding errors and keeps the public scripts closer to the original analysis environment.

---

## Calendar-Based and Operational Files Not Shared

Some original internal workflows used calendar files to reconcile booking events, attendance, SMS follow-ups, scheduling metadata, and operational monitoring.

Those raw calendar files are not included because they contain scheduling information and metadata that cannot be fully anonymised without compromising participant confidentiality.

As a result:

* internal calendar-reconciliation workflows are not fully reproducible from the public repository;
* raw booking-level audit files are not shared;
* participant-contact mapping files are not shared;
* operational monitoring records used during live study management are only represented through de-identified or derived summaries where appropriate.

The manuscript-facing figures, supplementary tables, and statistical summaries are intended to be reproducible from the public de-identified datasets and deposited analysis scripts. Where a particular internal audit output depended on withheld calendar metadata, that limitation should be interpreted as a confidentiality constraint rather than an absence of the underlying operational check.

---

## Fonts and Figure Appearance

Some submitted figures were prepared using Palatino Linotype. The font file is not included in this repository because redistribution rights may vary across operating systems and installations.

The scripts fall back to a generic serif font when Palatino Linotype is unavailable. Figures should reproduce analytically, although minor typography and spacing differences may occur across systems.

---

## Interpretation Notes

This repository supports a feasibility-first, early-phase study.

The outputs should be interpreted with the same caution as the manuscript:

* WP1 tested safety and tolerability across SLS parameters.
* The interim bridge study calibrated and evaluated a low-phenomenology control condition.
* WP2 tested feasibility, retention, safety, tolerability, and exploratory clinical outcomes in a randomised repeated-session design.
* Clinical outcome analyses were exploratory and estimation-focused.
* Candidate-predictor and acute-experience analyses were exploratory and should not be interpreted as evidence of mechanism, mediation, or moderation.
* The study was not powered to establish therapeutic efficacy.

---

## Quick File-to-Output Map

| Goal                                           | Run this                              | Main inputs                                                    | Main outputs                                                            |
| ---------------------------------------------- | ------------------------------------- | -------------------------------------------------------------- | ----------------------------------------------------------------------- |
| Generate all main manuscript figures           | `MRC_manuscript_plots_cleaned.r`      | WP1, interim, and WP2 public data files                        | Figures 1–6 as PNG/PDF                                                  |
| Generate manuscript Results prose/audit        | `MRC_manuscript_narrative_cleaned.r`  | Objects created by manuscript plot/calculation workflow        | Auto-generated manuscript narrative and audit CSV                       |
| Generate supplementary tables/workbook outputs | `MRC_SM_workbook_and_plots_cleaned.r` | WP1, interim, WP2 public data files                            | Supplementary CSV/XLSX tables, plots, audit files                       |
| Generate supplementary narrative outputs       | `MRC_SM_narrative_cleaned.r`          | Narrative text outputs from supplementary calculation blocks   | Combined SM narrative file and source audit                             |
| Inspect submitted supplementary workbook       | `MRC_SM v1.5.xlsx`                    | Static workbook file                                           | Human-readable supplementary tables and notes                           |
| Reproduce interim word clouds                  | `interim_word_clouds.ipynb`           | Interim analysed / text-derived data where available           | Word-cloud figures                                                      |
| Inspect WP1 Session 11 analyses                | `wp1_session_11.ipynb`                | WP1 Session 11 data/outputs where available                    | Exploratory session-specific plots                                      |
| Inspect WP2 operational monitoring             | `wp2_EMDRATS.ipynb`                   | WP2 screening, assignment, session, SMS, and monitoring inputs | Stratification, allocation, adherence, and wellbeing-monitoring outputs |

---

## Citation

Please cite the accompanying manuscript when using this repository:

> Nacker, D., Schwartzman, D. J., Seth, A. K., et al. *Stroboscopic Light Stimulation for Depression: Safety, Tolerability, and Feasibility in a Staged Early-Phase Study.*

---

## Contact

For questions regarding the repository, analysis scripts, or study methodology, please contact the corresponding author listed in the accompanying manuscript.
