# ============================================================
# MRC MANUSCRIPT PLOTS — CLEAN RUN SCRIPT
# ============================================================
#
# Purpose:
#   Generate the six manuscript figures only.
#
# Outputs:
#   PNG and PDF copies are saved to:
#     file.path(DATA_DIR, "manuscript_plots")
#
# Notes:
#   - This script was cleaned from the selected manuscript-figure
#     blocks supplied in Pasted text(38).txt.
#   - Older/superseded plot pathways were removed:
#       * old Figure 1 standardised flow export
#       * earlier Figure 4 version using non-final discomfort summaries
#       * earlier Figure 5 version using SEM rather than 95% CIs
#   - Shared package loading and the manuscript output folder are
#     centralised here.
#   - DATA_DIR can be edited below if the project folder moves.
#
# ============================================================

# ---------------------------
# 0. USER SETTINGS
# ---------------------------

DATA_DIR <- "C:/Users/dn284/Desktop/MRC_omni/data"
SEARCH_DIRS <- c(DATA_DIR)

MANUSCRIPT_PLOT_DIR <- file.path(DATA_DIR, "manuscript_plots")
if (!dir.exists(MANUSCRIPT_PLOT_DIR)) {
  dir.create(MANUSCRIPT_PLOT_DIR, recursive = TRUE, showWarnings = FALSE)
}

# ---------------------------
# 1. PACKAGES
# ---------------------------

required_packages <- c(
  "tidyverse",
  "lubridate",
  "showtext",
  "sysfonts",
  "patchwork",
  "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them once with:\ninstall.packages(c(",
    paste(sprintf('\"%s\"', missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(showtext)
  library(sysfonts)
  library(patchwork)
  library(scales)
  library(grid)
})

message("Manuscript plot outputs will be saved to: ", MANUSCRIPT_PLOT_DIR)

# ============================================================
# BEGIN MANUSCRIPT FIGURE CODE
# ============================================================

# FIGURE 1 #

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

# ===================================================
# FIGURE 1 — FULL PROGRAMME FLOW
# Standardised labels across arms:
# Screened → Excluded → Tested → Analysed
#
# Panel A: WP1, Interim, WP2 standardised flow
# Panel B: WP2 allocation and follow-up
# ===================================================
# ===================================================
# 1. SETUP
# ===================================================

DATA_DIR <- "C:/Users/dn284/Desktop/MRC_omni/data"
SEARCH_DIRS <- c(DATA_DIR)
MANUSCRIPT_PLOT_DIR <- file.path(DATA_DIR, "manuscript_plots")
if (!dir.exists(MANUSCRIPT_PLOT_DIR)) dir.create(MANUSCRIPT_PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

FONT_PATH <- file.path(DATA_DIR, "palatinolinotype_roman.ttf")

if (file.exists(FONT_PATH)) {
  font_add("PalatinoLinotype", regular = FONT_PATH)
  showtext_auto()
  PALATINO_NAME <- "PalatinoLinotype"
  message("Loaded font: ", PALATINO_NAME)
} else {
  PALATINO_NAME <- "serif"
  message("Palatino Linotype not found; using serif fallback.")
}

# ===================================================
# FIGURE 1 STYLE CONTROLS
# ===================================================

COLORS <- list(
  wp1 = "#5A5FD6",
  interim = "#B6781E",
  wp2 = "#4D77BC",
  control = "#4B9B73",
  intervention = "#4D77BC",
  text = "#111827",
  text_soft = "#4B5563",
  neutral_mid = "#6B7280",
  box_fill = "#F8FBFF",
  excluded_fill = "#F9E8E5",
  tested_fill = "#E8F4EC",
  analysed_fill = "#E8F4EC",
  wp2_fill = "#EEF4FC",
  arm_fill = "#F3F7F6"
)

FONT_SIZES_F1 <- list(
  figure_title = 52,
  panel_title  = 32,
  arm_title    = 28,
  box_label    = 23,
  box_value    = 21,
  note_text    = 20,
  audit_text   = 18,
  panel_tag    = 30
)

LINE_SIZES_F1 <- list(
  box_outline = 0.85,
  arrow       = 0.65
)

ARROW_SIZES_F1 <- list(
  length = 0.07
)

BOX_SIZES_F1 <- list(
  panel_a_width     = 0.25,
  panel_a_height    = 0.135,
  panel_b_top_width = 0.42,
  panel_b_arm_width = 0.38,
  panel_b_height    = 0.135
)

FIGURE_SIZE_F1 <- list(
  width  = 6.8,
  height = 5.8,
  dpi    = 300
)

pt_to_gg <- function(x) x / .pt

theme_set(theme_void(base_family = PALATINO_NAME))

# ===================================================
# 2. HELPERS
# ===================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

newest_match <- function(patterns, search_dirs = SEARCH_DIRS, required = TRUE) {
  hits <- character(0)
  
  for (d in search_dirs) {
    if (!dir.exists(d)) next
    
    all_files <- list.files(d, recursive = TRUE, full.names = TRUE)
    file_names <- basename(all_files)
    
    for (pat in patterns) {
      pat_regex <- pat
      pat_regex <- gsub("\\.", "\\\\.", pat_regex)
      pat_regex <- gsub("\\*", ".*", pat_regex)
      
      matched <- all_files[str_detect(file_names, regex(pat_regex, ignore_case = TRUE))]
      hits <- c(hits, matched)
    }
  }
  
  hits <- unique(hits[file.exists(hits)])
  
  if (length(hits) == 0) {
    if (required) stop("No files found for patterns: ", paste(patterns, collapse = ", "))
    return(NULL)
  }
  
  info <- file.info(hits)
  hits[order(info$mtime, decreasing = TRUE)][1]
}

read_qualtrics_real <- function(path, skiprows = NULL) {
  df <- read_csv(
    path,
    col_types = cols(.default = col_character()),
    skip = skiprows %||% 0
  )
  
  names(df) <- str_trim(names(df))
  
  if ("ResponseId" %in% names(df)) {
    df <- df %>%
      filter(str_starts(as.character(ResponseId), "R_"))
  }
  
  df
}

clean_id <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    na_if("") %>%
    na_if("nan") %>%
    na_if("NaN") %>%
    na_if("None") %>%
    str_extract("\\d{3,6}")
}

latest_per_pid <- function(
    df,
    pid_col = "part_id",
    dt_candidates = c("RecordedDate", "EndDate", "StartDate")
) {
  out <- df %>%
    filter(!is.na(.data[[pid_col]]))
  
  dt_col <- dt_candidates[dt_candidates %in% names(out)][1]
  
  if (is.na(dt_col)) {
    return(out %>% distinct(.data[[pid_col]], .keep_all = TRUE))
  }
  
  out %>%
    mutate(
      .dt = suppressWarnings(
        parse_date_time(
          .data[[dt_col]],
          orders = c(
            "ymd HMS", "ymd HM",
            "dmy HMS", "dmy HM",
            "mdy HMS", "mdy HM"
          )
        )
      )
    ) %>%
    arrange(.dt) %>%
    distinct(.data[[pid_col]], .keep_all = TRUE) %>%
    select(-.dt)
}

find_col <- function(df, candidates) {
  nms <- names(df)
  lower_map <- setNames(nms, tolower(nms))
  
  for (cand in candidates) {
    if (tolower(cand) %in% names(lower_map)) {
      return(lower_map[[tolower(cand)]])
    }
  }
  
  for (cand in candidates) {
    hits <- nms[str_detect(tolower(nms), fixed(tolower(cand)))]
    if (length(hits) > 0) return(hits[1])
  }
  
  NULL
}

standardise_condition <- function(x) {
  x_chr <- str_to_lower(str_trim(as.character(x)))
  
  case_when(
    str_detect(x_chr, "control|placebo|sham") ~ "Control",
    str_detect(x_chr, "intervention|active|treatment|sls") ~ "Intervention",
    x_chr %in% c("0", "c", "ctrl") ~ "Control",
    x_chr %in% c("1", "i", "int", "active") ~ "Intervention",
    TRUE ~ NA_character_
  )
}

count_ids <- function(x) {
  length(unique(na.omit(x)))
}

# ===================================================
# 3. LOCATE FILES
# ===================================================

WP1_SIGNUP_PATH   <- newest_match(c("*WP1*Sign-up*.csv"))
WP1_TEST_PATH     <- newest_match(c("*WP1*Testing*Start*.csv"))
WP1_FEEDBACK_PATH <- newest_match(c("*WP1*Session*Feedback*.csv"))

INTERIM_SIGNUP_PATH   <- newest_match(c("*Interim*Sign-Up*.csv", "*Interim*Sign-up*.csv"))
INTERIM_ANALYSED_PATH <- newest_match(c("*n32apr20*.csv"))

WP2_PRE_PATH    <- newest_match(c("*wp2_pre_screen*.csv"))
WP2_ASSIGN_PATH <- newest_match(c("*wp2_assignments*.csv"))
WP2_PRE1_PATH   <- newest_match(c("*wp2_pre_session_1*.csv"), required = FALSE)
WP2_POST4_PATH  <- newest_match(c("*wp2_post_session_4*.csv"), required = FALSE)

message("WP1 sign-up:       ", basename(WP1_SIGNUP_PATH))
message("WP1 testing:       ", basename(WP1_TEST_PATH))
message("WP1 feedback:      ", basename(WP1_FEEDBACK_PATH))
message("Interim sign-up:   ", basename(INTERIM_SIGNUP_PATH))
message("Interim analysed:  ", basename(INTERIM_ANALYSED_PATH))
message("WP2 pre-screen:    ", basename(WP2_PRE_PATH))
message("WP2 assignments:   ", basename(WP2_ASSIGN_PATH))
message("WP2 pre-session 1: ", ifelse(is.null(WP2_PRE1_PATH), "NULL", basename(WP2_PRE1_PATH)))
message("WP2 post-session 4:", ifelse(is.null(WP2_POST4_PATH), "NULL", basename(WP2_POST4_PATH)))

# ===================================================
# 4. LOAD FILES
# ===================================================

wp1_signup <- read_qualtrics_real(WP1_SIGNUP_PATH)
wp1_test   <- read_qualtrics_real(WP1_TEST_PATH)
wp1_fb     <- read_qualtrics_real(WP1_FEEDBACK_PATH)

interim_signup   <- read_qualtrics_real(INTERIM_SIGNUP_PATH)
interim_analysed <- read_qualtrics_real(INTERIM_ANALYSED_PATH)

wp2_pre    <- read_qualtrics_real(WP2_PRE_PATH)
wp2_assign <- read_qualtrics_real(WP2_ASSIGN_PATH)
wp2_pre1   <- if (!is.null(WP2_PRE1_PATH)) read_qualtrics_real(WP2_PRE1_PATH) else NULL
wp2_post4  <- if (!is.null(WP2_POST4_PATH)) read_qualtrics_real(WP2_POST4_PATH) else NULL

# ===================================================
# 5. HARMONISE IDS
# ===================================================

wp1_signup <- wp1_signup %>%
  mutate(part_id = clean_id(part_id))

wp1_test <- wp1_test %>%
  mutate(part_id = clean_id(part_id))

wp1_fb <- wp1_fb %>%
  mutate(part_id = clean_id(participant_id))

interim_signup <- interim_signup %>%
  mutate(part_id = clean_id(part_id))

interim_analysed <- interim_analysed %>%
  mutate(part_id = clean_id(part_id))

wp2_pre <- wp2_pre %>%
  mutate(part_id = clean_id(part_id))

wp2_assign <- wp2_assign %>%
  mutate(part_id = clean_id(part_id))

if (!is.null(wp2_pre1)) {
  wp2_pre1 <- wp2_pre1 %>%
    mutate(part_id = clean_id(part_id))
}

if (!is.null(wp2_post4)) {
  wp2_post4 <- wp2_post4 %>%
    mutate(part_id = clean_id(part_id))
}

# ===================================================
# 6. DEDUPLICATE SCREENING FILES
# ===================================================

wp1_signup_u     <- latest_per_pid(wp1_signup, "part_id")
interim_signup_u <- latest_per_pid(interim_signup, "part_id")
wp2_pre_u        <- latest_per_pid(wp2_pre, "part_id")

# ===================================================
# 7. COMPUTE STANDARDISED COUNTS
# ===================================================

# ===================================================
# WP1
# Screened = unique WP1 sign-up IDs
# Excluded = excluded == TRUE
# Tested   = unique WP1 testing-start IDs
# Analysed = participants with >=3 valid VDQ session rows
# ===================================================

wp1_screened <- count_ids(wp1_signup_u$part_id)

wp1_signup_u <- wp1_signup_u %>%
  mutate(excluded = str_to_upper(str_trim(excluded)))

wp1_excluded <- wp1_signup_u %>%
  filter(excluded == "TRUE") %>%
  summarise(n = n()) %>%
  pull(n)

wp1_passed <- wp1_signup_u %>%
  filter(excluded == "FALSE") %>%
  summarise(n = n()) %>%
  pull(n)

wp1_tested_ids <- wp1_test %>%
  filter(!is.na(part_id)) %>%
  pull(part_id) %>%
  unique()

wp1_tested <- length(wp1_tested_ids)

wp1_fb_valid <- wp1_fb %>%
  mutate(
    session_n_num = suppressWarnings(as.numeric(session_n)),
    vdq = suppressWarnings(as.numeric(discomfortScore))
  ) %>%
  filter(
    !is.na(part_id),
    between(session_n_num, 1, 11),
    !is.na(vdq)
  )

wp1_valid_counts <- wp1_fb_valid %>%
  count(part_id, name = "n_valid")

wp1_analysed_ids <- wp1_valid_counts %>%
  filter(n_valid >= 3) %>%
  pull(part_id)

wp1_analysed <- length(wp1_analysed_ids)

# ===================================================
# Interim
# Screened = unique Interim sign-up IDs
# Excluded = screened - eligible
# Tested   = attended Interim session / present in analysed file
# Analysed = unique IDs in analysed dataset
# ===================================================

interim_screened <- count_ids(interim_signup_u$part_id)

interim_signup_u <- interim_signup_u %>%
  mutate(excluded = str_to_upper(str_trim(excluded)))

interim_eligible_ids <- interim_signup_u %>%
  filter(excluded == "FALSE", !is.na(part_id)) %>%
  pull(part_id) %>%
  unique()

interim_eligible <- length(interim_eligible_ids)

interim_excluded <- interim_screened - interim_eligible

interim_tested_ids <- interim_analysed %>%
  filter(!is.na(part_id)) %>%
  pull(part_id) %>%
  unique()

interim_tested <- length(interim_tested_ids)

interim_analysed_ids <- interim_analysed %>%
  filter(!is.na(part_id)) %>%
  pull(part_id) %>%
  unique()

interim_analysed_n <- length(interim_analysed_ids)

# Optional retained for audit only
interim_booked <- NA_integer_
if ("sona" %in% names(interim_signup_u)) {
  interim_booked_ids <- interim_signup_u %>%
    filter(excluded == "FALSE", !is.na(sona), !is.na(part_id)) %>%
    pull(part_id) %>%
    unique()
  
  interim_booked <- length(interim_booked_ids)
}

# ===================================================
# WP2
# Screened = unique WP2 pre-screen IDs
# Excluded = true exclusions from WP2 pre-screening file
# Tested   = randomised/started
# Analysed = completed/analysed post-session-4 available cases
# ===================================================

wp2_screened <- count_ids(wp2_pre_u$part_id)

wp2_pre_u <- wp2_pre_u %>%
  mutate(
    excluded_clean = str_to_lower(str_trim(as.character(excluded))),
    excluded_clean = na_if(excluded_clean, ""),
    excluded_clean = na_if(excluded_clean, "nan"),
    excluded_clean = na_if(excluded_clean, "none")
  )

# True WP2 screening exclusions:
# In this file, exclusions are coded by exclusion stage/reason rather than TRUE.
wp2_excluded_ids <- wp2_pre_u %>%
  filter(
    !is.na(part_id),
    !is.na(excluded_clean),
    excluded_clean != "false"
  ) %>%
  pull(part_id) %>%
  unique()

wp2_excluded <- length(wp2_excluded_ids)

# Eligible / passed pre-screening, retained for audit only
wp2_passed_ids <- wp2_pre_u %>%
  filter(
    !is.na(part_id),
    excluded_clean == "false"
  ) %>%
  pull(part_id) %>%
  unique()

wp2_randomised_ids <- wp2_assign %>%
  filter(!is.na(part_id)) %>%
  pull(part_id) %>%
  unique()

wp2_randomised <- length(wp2_randomised_ids)

# We define Tested as all successfully randomised/started WP2 participants.
wp2_tested_ids <- wp2_randomised_ids
wp2_tested <- wp2_randomised

if (is.null(WP2_POST4_PATH)) {
  stop("Could not find wp2_post_session_4*.csv; needed for WP2 analysed counts.")
}

wp2_post4_vdq_col <- find_col(
  wp2_post4,
  c("tol_score", "discomfortScore", "vdq", "vdq_score", "vdq_total")
)

if (!is.null(wp2_post4_vdq_col)) {
  wp2_post4_valid <- wp2_post4 %>%
    mutate(.vdq_num = suppressWarnings(as.numeric(.data[[wp2_post4_vdq_col]]))) %>%
    filter(!is.na(part_id), !is.na(.vdq_num))
} else {
  wp2_post4_valid <- wp2_post4 %>%
    filter(!is.na(part_id))
}

wp2_analysed_ids <- intersect(
  unique(wp2_post4_valid$part_id),
  wp2_randomised_ids
)

wp2_analysed <- length(wp2_analysed_ids)

# ===================================================
# 8. WP2 ARM ALLOCATION FOR PANEL B
# ===================================================

condition_col <- find_col(
  wp2_assign,
  c(
    "condition",
    "allocation",
    "group",
    "arm",
    "assigned_condition",
    "randomised_condition",
    "randomized_condition",
    "treatment"
  )
)

if (is.null(condition_col)) {
  stop(
    "Could not identify WP2 condition/allocation column in wp2_assignments.csv. ",
    "Please check the assignment file column names. Expected something like ",
    "'condition', 'allocation', 'group', or 'arm'."
  )
}

message("Using WP2 allocation column: ", condition_col)

wp2_assign_clean <- wp2_assign %>%
  mutate(
    condition_raw = .data[[condition_col]],
    condition = standardise_condition(condition_raw)
  ) %>%
  filter(!is.na(part_id), !is.na(condition)) %>%
  distinct(part_id, .keep_all = TRUE)

wp2_arm_counts <- wp2_assign_clean %>%
  mutate(
    tested = part_id %in% wp2_tested_ids,
    analysed = part_id %in% wp2_analysed_ids
  ) %>%
  group_by(condition) %>%
  summarise(
    tested = sum(tested),
    analysed = sum(analysed),
    .groups = "drop"
  ) %>%
  complete(
    condition = c("Control", "Intervention"),
    fill = list(
      tested = 0,
      analysed = 0
    )
  )

# ===================================================
# 9. ASSEMBLE COUNT OBJECT
# ===================================================

FLOW_COUNTS <- list(
  WP1 = list(
    screened = wp1_screened,
    excluded = wp1_excluded,
    tested = wp1_tested,
    analysed = wp1_analysed
  ),
  Interim = list(
    screened = interim_screened,
    excluded = interim_excluded,
    tested = interim_tested,
    analysed = interim_analysed_n
  ),
  WP2 = list(
    screened = wp2_screened,
    excluded = wp2_excluded,
    tested = wp2_tested,
    analysed = wp2_analysed
  )
)

# ===================================================
# 10. AUDIT TABLES
# ===================================================

audit_df <- tribble(
  ~Arm,      ~Stage,     ~Count,
  "WP1",     "Screened", FLOW_COUNTS$WP1$screened,
  "WP1",     "Excluded", FLOW_COUNTS$WP1$excluded,
  "WP1",     "Tested",   FLOW_COUNTS$WP1$tested,
  "WP1",     "Analysed", FLOW_COUNTS$WP1$analysed,
  
  "Interim", "Screened", FLOW_COUNTS$Interim$screened,
  "Interim", "Excluded", FLOW_COUNTS$Interim$excluded,
  "Interim", "Tested",   FLOW_COUNTS$Interim$tested,
  "Interim", "Analysed", FLOW_COUNTS$Interim$analysed,
  
  "WP2",     "Screened", FLOW_COUNTS$WP2$screened,
  "WP2",     "Excluded", FLOW_COUNTS$WP2$excluded,
  "WP2",     "Tested",   FLOW_COUNTS$WP2$tested,
  "WP2",     "Analysed", FLOW_COUNTS$WP2$analysed
)

cat("\nProgramme-flow audit table\n")
cat("==========================\n")
print(audit_df)

cat("\nWP2 arm-level audit table\n")
cat("=========================\n")
print(wp2_arm_counts)

cat("\nSanity checks\n")
cat("-------------\n")
cat(sprintf(
  "WP1: screened=%s, excluded=%s, passed=%s, tested=%s, analysed=%s\n",
  wp1_screened, wp1_excluded, wp1_passed, wp1_tested, wp1_analysed
))
cat(sprintf(
  "Interim: screened=%s, eligible=%s, excluded=%s, booked=%s, tested/attended=%s, analysed=%s\n",
  interim_screened, interim_eligible, interim_excluded,
  ifelse(is.na(interim_booked), "NA", interim_booked),
  interim_tested, interim_analysed_n
))
wp2_exclusion_breakdown <- wp2_pre_u %>%
  filter(
    !is.na(part_id),
    !is.na(excluded_clean),
    excluded_clean != "false"
  ) %>%
  distinct(part_id, excluded_clean) %>%
  count(excluded_clean, name = "n")

cat("\nWP2 exclusion breakdown\n")
cat("=======================\n")
print(wp2_exclusion_breakdown)

cat(sprintf(
  "WP2: screened=%s, true_excluded=%s, passed_screen=%s, tested/randomised-started=%s, analysed=%s\n",
  wp2_screened, wp2_excluded, length(wp2_passed_ids), wp2_tested, wp2_analysed
))

cat(sprintf(
  "WP2 screened but neither true-excluded nor tested/randomised-started: %s\n",
  wp2_screened - wp2_excluded - wp2_tested
))

if (!is.null(wp2_pre1)) {
  wp2_pre1_ids <- wp2_pre1 %>%
    filter(!is.na(part_id)) %>%
    pull(part_id) %>%
    unique()
  
  cat(sprintf(
    "WP2 pre-session-1 forms found for %s randomised participants; figure defines Tested as randomised/started.\n",
    length(intersect(wp2_pre1_ids, wp2_randomised_ids))
  ))
}

# ===================================================
# 11–14. FIGURE 1 — SYNTHESISED CONSORT-STYLE FLOW
# Older aesthetic + reduced panel whitespace + richer WP2 CONSORT detail
# ===================================================

# ===================================================
# FIGURE 1 STYLE CONTROLS
# ===================================================

FONT_SIZES_F1 <- list(
  figure_title   = 52,  # retained for compatibility; not used
  panel_title    = 29,
  arm_title      = 29,
  box_label      = 21.5,
  box_value      = 19.5,
  note_text      = 19,
  audit_text     = 17,
  panel_tag      = 29,
  side_label     = 17.5,
  excluded_title = 14.5,
  excluded_body  = 12.5
)

LINE_SIZES_F1 <- list(
  box_outline = 0.90,
  arrow       = 0.70
)

ARROW_SIZES_F1 <- list(
  length = 0.060
)

BOX_SIZES_F1 <- list(
  panel_a_width      = 0.25,
  panel_a_height     = 0.118,
  panel_b_top_width  = 0.39,
  panel_b_arm_width  = 0.36,
  panel_b_height     = 0.112,
  excluded_width     = 0.245,
  excluded_height    = 0.160
)

FIGURE_SIZE_F1 <- list(
  width  = 7.2,
  height = 7.75,
  dpi    = 300
)

pt_to_gg <- function(x) x / .pt

theme_set(theme_void(base_family = PALATINO_NAME))

# ===================================================
# PREP PANEL-B EXCLUSION REASONS
# ===================================================

wp2_prescreen_audit <- wp2_pre_u %>%
  mutate(
    excluded_clean = str_to_lower(str_trim(as.character(excluded))),
    excluded_clean = na_if(excluded_clean, ""),
    excluded_clean = na_if(excluded_clean, "nan"),
    excluded_clean = na_if(excluded_clean, "none"),
    exclusion_status = case_when(
      is.na(excluded_clean) ~ "unclear_or_missing",
      excluded_clean == "false" ~ "eligible",
      excluded_clean %in% c("safety_screening", "safety", "safety screening") ~ "excluded_safety",
      excluded_clean %in% c("demographics", "demographic") ~ "excluded_demographics",
      excluded_clean %in% c("phq_9", "phq9", "phq-9") ~ "excluded_phq_9",
      TRUE ~ paste0("excluded_other_", excluded_clean)
    )
  ) %>%
  filter(!is.na(part_id)) %>%
  distinct(part_id, .keep_all = TRUE)

wp2_exclusion_reason_counts <- wp2_prescreen_audit %>%
  filter(str_starts(exclusion_status, "excluded_")) %>%
  mutate(
    reason = case_when(
      exclusion_status == "excluded_safety" ~ "Safety screening",
      exclusion_status == "excluded_demographics" ~ "Demographics",
      exclusion_status == "excluded_phq_9" ~ "PHQ-9",
      TRUE ~ str_remove(exclusion_status, "^excluded_other_")
    )
  ) %>%
  count(reason, name = "n") %>%
  arrange(desc(n))

get_reason_n <- function(reason_name) {
  out <- wp2_exclusion_reason_counts %>%
    filter(reason == reason_name) %>%
    pull(n)
  
  if (length(out) == 0) 0 else out[1]
}

# ===================================================
# PREP WP2 SMS / POST-SESSION-4 ASSESSMENT COUNTS
# ===================================================

WP2_SMS_PATH <- newest_match(
  c("*wp2_sms_post*.csv", "*sms_post*.csv", "*WP2*SMS*.csv"),
  required = FALSE
)

wp2_sms <- if (!is.null(WP2_SMS_PATH)) {
  message("WP2 SMS post file: ", basename(WP2_SMS_PATH))
  read_qualtrics_real(WP2_SMS_PATH) %>%
    mutate(part_id = clean_id(part_id))
} else {
  message("No WP2 SMS post file found. SMS counts will be 0 unless wp2_consort_arm_counts already exists.")
  NULL
}

if (!exists("wp2_post4_valid")) {
  
  wp2_post4_vdq_col <- find_col(
    wp2_post4,
    c("tol_score", "discomfortScore", "vdq", "vdq_score", "vdq_total")
  )
  
  if (!is.null(wp2_post4_vdq_col)) {
    wp2_post4_valid <- wp2_post4 %>%
      mutate(.vdq_num = suppressWarnings(as.numeric(.data[[wp2_post4_vdq_col]]))) %>%
      filter(!is.na(part_id), !is.na(.vdq_num))
  } else {
    wp2_post4_valid <- wp2_post4 %>%
      filter(!is.na(part_id))
  }
}

wp2_post4_ids <- wp2_post4_valid %>%
  filter(!is.na(part_id)) %>%
  pull(part_id) %>%
  unique()

wp2_sms_ids <- if (!is.null(wp2_sms)) {
  wp2_sms %>%
    filter(!is.na(part_id)) %>%
    pull(part_id) %>%
    unique()
} else {
  character(0)
}

if (!exists("wp2_consort_arm_counts")) {
  wp2_consort_arm_counts <- wp2_assign_clean %>%
    filter(!is.na(part_id), !is.na(condition)) %>%
    distinct(part_id, condition) %>%
    mutate(
      allocated = TRUE,
      provided_post4 = part_id %in% wp2_post4_ids,
      provided_sms   = part_id %in% wp2_sms_ids,
      provided_both  = provided_post4 & provided_sms
    ) %>%
    group_by(condition) %>%
    summarise(
      allocated_n = n_distinct(part_id),
      post4_n = sum(provided_post4, na.rm = TRUE),
      no_post4_n = allocated_n - post4_n,
      sms_n = sum(provided_sms, na.rm = TRUE),
      both_post4_sms_n = sum(provided_both, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    complete(
      condition = c("Control", "Intervention"),
      fill = list(
        allocated_n = 0,
        post4_n = 0,
        no_post4_n = 0,
        sms_n = 0,
        both_post4_sms_n = 0
      )
    )
}

cat("\nWP2 CONSORT-style arm-level follow-up / assessment counts\n")
cat("==========================================================\n")
print(wp2_consort_arm_counts)

# ===================================================
# MANUAL WP2 POST-SESSION-4 NON-COMPLETION REASONS
# ===================================================

WP2_DROPOUT_REASONS <- list(
  Control = list(
    tolerability = 3
  ),
  Intervention = list(
    tolerability = 1
  )
)

# ===================================================
# 11. PANEL A — PARTICIPANT FLOW BY PROGRAMME STAGE
# ===================================================

make_panel_a <- function() {
  
  flow_plot_df <- bind_rows(
    tibble(
      arm = "WP1",
      arm_display = "WP1",
      x = 0.13,
      y = c(0.665, 0.495, 0.325, 0.155),
      label = c("Screened", "Excluded at screening", "Tested", "Analysed"),
      value = c(
        FLOW_COUNTS$WP1$screened,
        FLOW_COUNTS$WP1$excluded,
        FLOW_COUNTS$WP1$tested,
        FLOW_COUNTS$WP1$analysed
      ),
      edgecolor = COLORS$wp1
    ),
    tibble(
      arm = "Interim",
      arm_display = "Interim bridge",
      x = 0.50,
      y = c(0.665, 0.495, 0.325, 0.155),
      label = c("Screened", "Excluded at screening", "Tested", "Analysed"),
      value = c(
        FLOW_COUNTS$Interim$screened,
        FLOW_COUNTS$Interim$excluded,
        FLOW_COUNTS$Interim$tested,
        FLOW_COUNTS$Interim$analysed
      ),
      edgecolor = COLORS$interim
    ),
    tibble(
      arm = "WP2",
      arm_display = "WP2",
      x = 0.87,
      y = c(0.665, 0.495, 0.325, 0.155),
      label = c("Screened", "Excluded at screening", "Randomised", "Analysed post-session 4"),
      value = c(
        FLOW_COUNTS$WP2$screened,
        FLOW_COUNTS$WP2$excluded,
        FLOW_COUNTS$WP2$tested,
        FLOW_COUNTS$WP2$analysed
      ),
      edgecolor = COLORS$wp2
    )
  ) %>%
    mutate(
      width  = BOX_SIZES_F1$panel_a_width,
      height = BOX_SIZES_F1$panel_a_height,
      xmin = x - width / 2,
      xmax = x + width / 2,
      ymin = y,
      ymax = y + height,
      fill = case_when(
        str_detect(label, "Excluded") ~ COLORS$excluded_fill,
        str_detect(label, "Screened") ~ COLORS$box_fill,
        label %in% c("Tested", "Randomised") ~ COLORS$tested_fill,
        str_detect(label, "Analysed") ~ COLORS$analysed_fill,
        TRUE ~ COLORS$box_fill
      ),
      label_y = y + height * 0.62,
      value_y = y + height * 0.24,
      value_text = paste0("n = ", as.integer(value))
    )
  
  arrow_df <- flow_plot_df %>%
    group_by(arm) %>%
    arrange(desc(y)) %>%
    mutate(next_y = lead(y)) %>%
    filter(!is.na(next_y)) %>%
    transmute(
      x = x,
      xend = x,
      y = ymin,
      yend = next_y + height[1],
      col = edgecolor
    )
  
  title_df <- tibble(
    arm_display = c("WP1", "Interim bridge", "WP2"),
    x = c(0.13, 0.50, 0.87),
    y = 0.850,
    col = c(COLORS$wp1, COLORS$interim, COLORS$wp2)
  )
  
  ggplot() +
    annotate(
      "text",
      x = 0.035,
      y = 0.955,
      label = "A.",
      family = PALATINO_NAME,
      fontface = "bold",
      colour = COLORS$text,
      size = pt_to_gg(FONT_SIZES_F1$panel_tag),
      hjust = 0
    ) +
    annotate(
      "text",
      x = 0.50,
      y = 0.955,
      label = "Participant flow by programme stage",
      family = PALATINO_NAME,
      fontface = "bold",
      colour = COLORS$text,
      size = pt_to_gg(FONT_SIZES_F1$panel_title),
      hjust = 0.5
    ) +
    geom_text(
      data = title_df,
      aes(x = x, y = y, label = arm_display, colour = col),
      family = PALATINO_NAME,
      fontface = "bold",
      size = pt_to_gg(FONT_SIZES_F1$arm_title)
    ) +
    geom_segment(
      data = arrow_df,
      aes(x = x, y = y, xend = xend, yend = yend, colour = col),
      linewidth = LINE_SIZES_F1$arrow,
      arrow = arrow(
        type = "closed",
        length = unit(ARROW_SIZES_F1$length, "inches")
      ),
      lineend = "round"
    ) +
    geom_rect(
      data = flow_plot_df,
      aes(
        xmin = xmin, xmax = xmax,
        ymin = ymin, ymax = ymax,
        fill = fill,
        colour = edgecolor
      ),
      linewidth = LINE_SIZES_F1$box_outline
    ) +
    geom_text(
      data = flow_plot_df,
      aes(x = x, y = label_y, label = label),
      family = PALATINO_NAME,
      fontface = "bold",
      colour = COLORS$text,
      size = pt_to_gg(FONT_SIZES_F1$box_label),
      lineheight = 0.92
    ) +
    geom_text(
      data = flow_plot_df,
      aes(x = x, y = value_y, label = value_text),
      family = PALATINO_NAME,
      colour = COLORS$text,
      size = pt_to_gg(FONT_SIZES_F1$box_value)
    ) +
    scale_fill_identity() +
    scale_colour_identity() +
    coord_cartesian(
      xlim = c(0.02, 0.98),
      ylim = c(0.080, 0.995),
      clip = "off"
    ) +
    theme_void(base_family = PALATINO_NAME) +
    theme(
      plot.margin = margin(0, 8, -16, 8)
    )
}

# ===================================================
# PANEL B STYLE OVERRIDES — FINAL POLISH VERSION
# ===================================================

PALETTE <- c(
  "Control" = "#4F76BC",
  "Intervention" = "#F2A100"
)

COLORS$control      <- PALETTE[["Control"]]
COLORS$intervention <- PALETTE[["Intervention"]]

FONT_SIZES_F1$panel_b_label <- 17.4
FONT_SIZES_F1$panel_b_value <- 14.8
FONT_SIZES_F1$side_label    <- 16.0

FONT_SIZES_F1$excluded_title <- 13.2
FONT_SIZES_F1$excluded_body  <- 11.4

BOX_SIZES_F1$panel_b_top_width <- 0.40
BOX_SIZES_F1$panel_b_arm_width <- 0.37
BOX_SIZES_F1$panel_b_height    <- 0.104
BOX_SIZES_F1$excluded_width    <- 0.240
BOX_SIZES_F1$excluded_height   <- 0.142

# ===================================================
# 12. PANEL B — CLEANED CONSORT-STYLE FLOW
# Final polish: centred exclusion connector + arm colours + clean loss box
# ===================================================

make_panel_b <- function() {
  
  control_counts <- wp2_consort_arm_counts %>%
    filter(condition == "Control")
  
  intervention_counts <- wp2_consort_arm_counts %>%
    filter(condition == "Intervention")
  
  control_tol_dropout <- WP2_DROPOUT_REASONS$Control$tolerability
  int_tol_dropout     <- WP2_DROPOUT_REASONS$Intervention$tolerability
  
  control_sched_dropout <- control_counts$no_post4_n - control_tol_dropout
  int_sched_dropout     <- intervention_counts$no_post4_n - int_tol_dropout
  
  n_demo   <- get_reason_n("Demographics")
  n_safety <- get_reason_n("Safety screening")
  n_phq    <- get_reason_n("PHQ-9")
  
  panel_b_boxes <- bind_rows(
    tibble(
      id = "screened",
      x = 0.47,
      y = 0.820,
      width = BOX_SIZES_F1$panel_b_top_width,
      height = BOX_SIZES_F1$panel_b_height,
      label = "Screened",
      value = paste0("n = ", FLOW_COUNTS$WP2$screened),
      fill = COLORS$box_fill,
      edge = COLORS$wp2,
      label_mult = 0.64,
      value_mult = 0.26
    ),
    tibble(
      id = "randomised",
      x = 0.47,
      y = 0.670,
      width = BOX_SIZES_F1$panel_b_top_width,
      height = BOX_SIZES_F1$panel_b_height,
      label = "Randomised / started",
      value = paste0("n = ", FLOW_COUNTS$WP2$tested),
      fill = COLORS$wp2_fill,
      edge = COLORS$wp2,
      label_mult = 0.64,
      value_mult = 0.26
    ),
    tibble(
      id = "control_alloc",
      x = 0.31,
      y = 0.510,
      width = BOX_SIZES_F1$panel_b_arm_width,
      height = BOX_SIZES_F1$panel_b_height,
      label = "Allocated to matched control",
      value = paste0("n = ", control_counts$allocated_n),
      fill = COLORS$arm_fill,
      edge = COLORS$control,
      label_mult = 0.64,
      value_mult = 0.26
    ),
    tibble(
      id = "intervention_alloc",
      x = 0.73,
      y = 0.510,
      width = BOX_SIZES_F1$panel_b_arm_width,
      height = BOX_SIZES_F1$panel_b_height,
      label = "Allocated to SLS intervention",
      value = paste0("n = ", intervention_counts$allocated_n),
      fill = COLORS$arm_fill,
      edge = COLORS$intervention,
      label_mult = 0.64,
      value_mult = 0.26
    ),
    tibble(
      id = "control_follow",
      x = 0.31,
      y = 0.355,
      width = BOX_SIZES_F1$panel_b_arm_width,
      height = BOX_SIZES_F1$panel_b_height * 1.12,
      label = "Completed post-session-4 follow-up",
      value = paste0("n = ", control_counts$post4_n),
      fill = COLORS$analysed_fill,
      edge = COLORS$control,
      label_mult = 0.62,
      value_mult = 0.27
    ),
    tibble(
      id = "intervention_follow",
      x = 0.73,
      y = 0.355,
      width = BOX_SIZES_F1$panel_b_arm_width,
      height = BOX_SIZES_F1$panel_b_height * 1.12,
      label = "Completed post-session-4 follow-up",
      value = paste0("n = ", intervention_counts$post4_n),
      fill = COLORS$analysed_fill,
      edge = COLORS$intervention,
      label_mult = 0.62,
      value_mult = 0.27
    ),
    tibble(
      id = "control_loss",
      x = 0.31,
      y = 0.190,
      width = BOX_SIZES_F1$panel_b_arm_width,
      height = BOX_SIZES_F1$panel_b_height * 1.32,
      label = "Did not provide post-session-4 data",
      value = paste0(
        "n = ", control_counts$no_post4_n, "\n",
        "Tolerability: n = ", control_tol_dropout,
        "; Scheduling: n = ", control_sched_dropout
      ),
      fill = COLORS$excluded_fill,
      edge = COLORS$control,
      label_mult = 0.72,
      value_mult = 0.34
    ),
    tibble(
      id = "intervention_loss",
      x = 0.73,
      y = 0.190,
      width = BOX_SIZES_F1$panel_b_arm_width,
      height = BOX_SIZES_F1$panel_b_height * 1.32,
      label = "Did not provide post-session-4 data",
      value = paste0(
        "n = ", intervention_counts$no_post4_n, "\n",
        "Tolerability: n = ", int_tol_dropout,
        "; Scheduling: n = ", int_sched_dropout
      ),
      fill = COLORS$excluded_fill,
      edge = COLORS$intervention,
      label_mult = 0.72,
      value_mult = 0.34
    ),
    tibble(
      id = "control_assessed",
      x = 0.31,
      y = 0.028,
      width = BOX_SIZES_F1$panel_b_arm_width,
      height = BOX_SIZES_F1$panel_b_height * 1.34,
      label = "Assessment data available",
      value = paste0(
        "Post-session 4: n = ", control_counts$post4_n, "\n",
        "SMS follow-up: n = ", control_counts$sms_n
      ),
      fill = COLORS$analysed_fill,
      edge = COLORS$control,
      label_mult = 0.76,
      value_mult = 0.33
    ),
    tibble(
      id = "intervention_assessed",
      x = 0.73,
      y = 0.028,
      width = BOX_SIZES_F1$panel_b_arm_width,
      height = BOX_SIZES_F1$panel_b_height * 1.34,
      label = "Assessment data available",
      value = paste0(
        "Post-session 4: n = ", intervention_counts$post4_n, "\n",
        "SMS follow-up: n = ", intervention_counts$sms_n
      ),
      fill = COLORS$analysed_fill,
      edge = COLORS$intervention,
      label_mult = 0.76,
      value_mult = 0.33
    )
  ) %>%
    mutate(
      xmin = x - width / 2,
      xmax = x + width / 2,
      ymin = y,
      ymax = y + height,
      label_y = y + height * label_mult,
      value_y = y + height * value_mult
    )
  
  get_box <- function(id_name) {
    panel_b_boxes %>% filter(id == id_name)
  }
  
  screened_box <- get_box("screened")
  screened_mid_y <- screened_box$y + screened_box$height / 2
  
  excluded_box <- tibble(
    x = 0.805,
    width = BOX_SIZES_F1$excluded_width,
    height = BOX_SIZES_F1$excluded_height,
    y = screened_mid_y - height / 2,
    xmin = x - width / 2,
    xmax = x + width / 2,
    ymin = y,
    ymax = y + height,
    line1 = paste0("Excluded: n = ", FLOW_COUNTS$WP2$excluded),
    line2 = paste0("Demographics: n = ", n_demo),
    line3 = paste0("Safety: n = ", n_safety),
    line4 = paste0("PHQ-9: n = ", n_phq),
    line1_y = y + height * 0.78,
    line2_y = y + height * 0.56,
    line3_y = y + height * 0.35,
    line4_y = y + height * 0.15
  )
  
  screened_bottom   <- get_box("screened")$ymin
  randomised_top    <- get_box("randomised")$ymax
  randomised_bottom <- get_box("randomised")$ymin
  
  control_alloc_top <- get_box("control_alloc")$ymax
  int_alloc_top     <- get_box("intervention_alloc")$ymax
  
  control_follow_top <- get_box("control_follow")$ymax
  int_follow_top     <- get_box("intervention_follow")$ymax
  
  control_loss_top <- get_box("control_loss")$ymax
  int_loss_top     <- get_box("intervention_loss")$ymax
  
  control_assess_top <- get_box("control_assessed")$ymax
  int_assess_top     <- get_box("intervention_assessed")$ymax
  
  branch_y <- 0.625
  
  panel_b_lines <- tribble(
    ~x,    ~y,                 ~xend, ~yend,     ~col,
    0.47,  randomised_bottom,   0.47,  branch_y,  COLORS$wp2,
    0.47,  branch_y,           0.31,  branch_y,  COLORS$wp2,
    0.47,  branch_y,           0.73,  branch_y,  COLORS$wp2
  )
  
  panel_b_arrows <- tribble(
    ~x,    ~y,              ~xend, ~yend,              ~col,
    0.47,  screened_bottom, 0.47,  randomised_top,     COLORS$wp2,
    0.31,  branch_y,       0.31,  control_alloc_top,   COLORS$control,
    0.73,  branch_y,       0.73,  int_alloc_top,       COLORS$intervention,
    0.31,  0.510,          0.31,  control_follow_top,  COLORS$control,
    0.73,  0.510,          0.73,  int_follow_top,      COLORS$intervention,
    0.31,  0.355,          0.31,  control_loss_top,    COLORS$control,
    0.73,  0.355,          0.73,  int_loss_top,        COLORS$intervention,
    0.31,  0.190,          0.31,  control_assess_top,  COLORS$control,
    0.73,  0.190,          0.73,  int_assess_top,      COLORS$intervention
  )
  
  excluded_connector <- tibble(
    x    = screened_box$x + screened_box$width / 2,
    y    = screened_mid_y,
    xend = excluded_box$xmin,
    yend = screened_mid_y,
    col  = COLORS$neutral_mid
  )
  
  section_labels <- tibble(
    lab = c("Screening", "Enrolment", "Allocation", "Follow-up", "Loss", "Assessment"),
    y = c(
      get_box("screened")$y + get_box("screened")$height / 2,
      get_box("randomised")$y + get_box("randomised")$height / 2,
      get_box("control_alloc")$y + get_box("control_alloc")$height / 2,
      get_box("control_follow")$y + get_box("control_follow")$height / 2,
      get_box("control_loss")$y + get_box("control_loss")$height / 2,
      get_box("control_assessed")$y + get_box("control_assessed")$height / 2
    )
  )
  
  ggplot() +
    annotate(
      "text",
      x = 0.035,
      y = 1.005,
      label = "B.",
      family = PALATINO_NAME,
      fontface = "bold",
      colour = COLORS$text,
      size = pt_to_gg(FONT_SIZES_F1$panel_tag),
      hjust = 0
    ) +
    annotate(
      "text",
      x = 0.50,
      y = 1.005,
      label = "WP2 allocation, follow-up and assessment data",
      family = PALATINO_NAME,
      fontface = "bold",
      colour = COLORS$text,
      size = pt_to_gg(FONT_SIZES_F1$panel_title),
      hjust = 0.5
    ) +
    geom_segment(
      data = panel_b_lines,
      aes(x = x, y = y, xend = xend, yend = yend, colour = col),
      linewidth = LINE_SIZES_F1$arrow,
      lineend = "round"
    ) +
    geom_segment(
      data = excluded_connector,
      aes(x = x, y = y, xend = xend, yend = yend, colour = col),
      linewidth = LINE_SIZES_F1$arrow,
      lineend = "round"
    ) +
    geom_segment(
      data = panel_b_arrows,
      aes(x = x, y = y, xend = xend, yend = yend, colour = col),
      linewidth = LINE_SIZES_F1$arrow,
      arrow = arrow(
        type = "closed",
        length = unit(ARROW_SIZES_F1$length, "inches")
      ),
      lineend = "round"
    ) +
    geom_rect(
      data = panel_b_boxes,
      aes(
        xmin = xmin, xmax = xmax,
        ymin = ymin, ymax = ymax,
        fill = fill,
        colour = edge
      ),
      linewidth = LINE_SIZES_F1$box_outline
    ) +
    geom_rect(
      data = excluded_box,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = COLORS$excluded_fill,
      colour = COLORS$neutral_mid,
      linewidth = LINE_SIZES_F1$box_outline
    ) +
    geom_text(
      data = panel_b_boxes,
      aes(x = x, y = label_y, label = label),
      family = PALATINO_NAME,
      fontface = "bold",
      colour = COLORS$text,
      size = pt_to_gg(FONT_SIZES_F1$panel_b_label),
      lineheight = 0.95
    ) +
    geom_text(
      data = panel_b_boxes,
      aes(x = x, y = value_y, label = value),
      family = PALATINO_NAME,
      colour = COLORS$text,
      size = pt_to_gg(FONT_SIZES_F1$panel_b_value),
      lineheight = 1.05
    ) +
    geom_text(
      data = excluded_box,
      aes(x = x, y = line1_y, label = line1),
      family = PALATINO_NAME,
      fontface = "bold",
      colour = COLORS$text,
      size = pt_to_gg(FONT_SIZES_F1$excluded_title)
    ) +
    geom_text(
      data = excluded_box,
      aes(x = x, y = line2_y, label = line2),
      family = PALATINO_NAME,
      colour = COLORS$text,
      size = pt_to_gg(FONT_SIZES_F1$excluded_body)
    ) +
    geom_text(
      data = excluded_box,
      aes(x = x, y = line3_y, label = line3),
      family = PALATINO_NAME,
      colour = COLORS$text,
      size = pt_to_gg(FONT_SIZES_F1$excluded_body)
    ) +
    geom_text(
      data = excluded_box,
      aes(x = x, y = line4_y, label = line4),
      family = PALATINO_NAME,
      colour = COLORS$text,
      size = pt_to_gg(FONT_SIZES_F1$excluded_body)
    ) +
    geom_text(
      data = section_labels,
      aes(x = 0.118, y = y, label = lab),
      family = PALATINO_NAME,
      fontface = "bold",
      colour = COLORS$text_soft,
      size = pt_to_gg(FONT_SIZES_F1$side_label),
      hjust = 1
    ) +
    scale_fill_identity() +
    scale_colour_identity() +
    coord_cartesian(
      xlim = c(0.02, 0.98),
      ylim = c(0.018, 1.018),
      clip = "off"
    ) +
    theme_void(base_family = PALATINO_NAME) +
    theme(
      plot.margin = margin(-20, 8, 2, 8)
    )
}

# ===================================================
# 13. COMBINE PANELS — NO OVERALL FIGURE TITLE
# Reduced whitespace between panels
# ===================================================

panel_a <- make_panel_a()
panel_b <- make_panel_b()

figure_1 <- panel_a / panel_b +
  plot_layout(
    heights = c(0.72, 1.28)
  ) +
  plot_annotation(
    theme = theme(
      plot.margin = margin(0, 4, 2, 4)
    )
  )

print(figure_1)

# ===================================================
# 14. EXPORT
# ===================================================

ggsave(
  filename = file.path(MANUSCRIPT_PLOT_DIR, "Figure1_ProgrammeFlow_CONSORT_synthesised_full.png"),
  plot = figure_1,
  width = FIGURE_SIZE_F1$width,
  height = FIGURE_SIZE_F1$height,
  dpi = FIGURE_SIZE_F1$dpi,
  bg = "white"
)

ggsave(
  filename = file.path(MANUSCRIPT_PLOT_DIR, "Figure1_ProgrammeFlow_CONSORT_synthesised_full.pdf"),
  plot = figure_1,
  width = FIGURE_SIZE_F1$width,
  height = FIGURE_SIZE_F1$height,
  dpi = FIGURE_SIZE_F1$dpi,
  bg = "white"
)

# ===================================================

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

# FIGURE 2

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

# ===================================================
# FIGURE 2 — WP1 SAFETY / TOLERABILITY
# Compact two-panel version:
#   A. Session-wise discomfort zoomed to observed range
#   B. Threshold context against 7/10 discontinuation criterion
#
# Updated:
#   - 80% UCL remains plotted as a dashed line
#   - UCL triangle markers removed from Panel A
#   - UCL triangle markers removed from legend
# ===================================================

# install.packages(c("tidyverse", "patchwork", "showtext", "sysfonts", "scales"))
# ===================================================
# SETUP
# ===================================================

DATA_DIR <- "C:/Users/dn284/Desktop/MRC_omni/data"
SEARCH_DIRS <- c(DATA_DIR)
MANUSCRIPT_PLOT_DIR <- file.path(DATA_DIR, "manuscript_plots")
if (!dir.exists(MANUSCRIPT_PLOT_DIR)) dir.create(MANUSCRIPT_PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

FONT_PATH <- file.path(DATA_DIR, "palatinolinotype_roman.ttf")

if (file.exists(FONT_PATH)) {
  font_add("PalatinoLinotype", regular = FONT_PATH)
  showtext_auto()
  PALATINO_NAME <- "PalatinoLinotype"
  message("Loaded font: ", PALATINO_NAME)
} else {
  PALATINO_NAME <- "serif"
  message("Palatino Linotype not found; using serif fallback.")
}

# ===================================================
# FIGURE STYLE CONTROLS
# ===================================================

FONT_SIZES_F2 <- list(
  panel_title  = 28,
  axis_title   = 24,
  axis_text    = 24,
  legend_text  = 24,
  annotation   = 20,
  bar_label    = 24
)

LINE_SIZES_F2 <- list(
  mean_line    = 1.15,
  ucl_line     = 1.05,
  error_stem   = 0.65,
  axis_line    = 0.40,
  grid_line    = 0.30,
  threshold    = 0.85,
  context_bar  = 10.5
)

POINT_SIZES_F2 <- list(
  mean_point = 3.0
)

FIGURE_SIZE_F2 <- list(
  width  = 6.8,
  height = 3.4,
  dpi    = 300
)

COLORS_F2 <- list(
  mean       = "#1D6140",
  ucl        = "#2F8F8A",
  threshold  = "#DC2626",
  band_none  = "#E8F3E6",
  band_mild  = "#F4F1D0",
  context_bg = "#D9D9D9",
  text       = "#111827",
  text_soft  = "#4B5563",
  grid       = "grey70"
)

THRESHOLD <- 7.0
Z80 <- 0.841621233

theme_set(
  theme_minimal(base_family = PALATINO_NAME) +
    theme(
      plot.title = element_text(
        face = "plain",
        size = FONT_SIZES_F2$panel_title,
        hjust = 0
      ),
      axis.title = element_text(size = FONT_SIZES_F2$axis_title),
      axis.text = element_text(size = FONT_SIZES_F2$axis_text),
      legend.text = element_text(size = FONT_SIZES_F2$legend_text)
    )
)

# ===================================================
# HELPERS
# ===================================================

newest_match <- function(patterns, search_dirs = SEARCH_DIRS, required = TRUE) {
  hits <- character(0)
  
  for (d in search_dirs) {
    if (!dir.exists(d)) next
    
    all_files  <- list.files(d, recursive = TRUE, full.names = TRUE)
    file_names <- basename(all_files)
    
    for (pat in patterns) {
      pat_regex <- pat
      pat_regex <- gsub("\\.", "\\\\.", pat_regex)
      pat_regex <- gsub("\\*", ".*", pat_regex)
      
      matched <- all_files[stringr::str_detect(
        file_names,
        stringr::regex(pat_regex, ignore_case = TRUE)
      )]
      
      hits <- c(hits, matched)
    }
  }
  
  hits <- unique(hits[file.exists(hits)])
  
  if (length(hits) == 0) {
    if (required) stop("No files found for patterns: ", paste(patterns, collapse = ", "))
    return(NULL)
  }
  
  info <- file.info(hits)
  hits[order(info$mtime, decreasing = TRUE)][1]
}

read_qualtrics_real <- function(path, skiprows = NULL) {
  df <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    skip = ifelse(is.null(skiprows), 0, skiprows)
  )
  
  names(df) <- stringr::str_trim(names(df))
  
  if ("ResponseId" %in% names(df)) {
    df <- df %>%
      filter(stringr::str_starts(as.character(ResponseId), "R_"))
  }
  
  df
}

clean_id <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    dplyr::na_if("") %>%
    dplyr::na_if("nan") %>%
    dplyr::na_if("NaN") %>%
    dplyr::na_if("None") %>%
    dplyr::na_if('{"ImportId":"part_id"}') %>%
    stringr::str_extract("\\d{3,6}")
}

find_col <- function(df, candidates) {
  nms <- names(df)
  lower_map <- setNames(nms, tolower(nms))
  
  for (cand in candidates) {
    if (tolower(cand) %in% names(lower_map)) {
      return(lower_map[[tolower(cand)]])
    }
  }
  
  for (cand in candidates) {
    hits <- nms[stringr::str_detect(tolower(nms), fixed(tolower(cand)))]
    if (length(hits) > 0) return(hits[1])
  }
  
  NULL
}

to_num <- function(x) suppressWarnings(as.numeric(x))

# ===================================================
# LOAD + CLEAN WP1 SESSION FEEDBACK
# ===================================================

WP1_FEEDBACK_PATH <- newest_match(c("*WP1*Session*Feedback*.csv"))
message("Using WP1 feedback file: ", basename(WP1_FEEDBACK_PATH))

wp1 <- read_qualtrics_real(WP1_FEEDBACK_PATH)

pid_col <- find_col(wp1, c("participant_id", "part_id"))
sess_col <- find_col(wp1, c("session_n", "session", "session_number"))
vdq_col <- find_col(wp1, c("discomfortScore", "vdq", "vdq_score", "tol_score"))
sidefx_col <- find_col(wp1, c("tol_sum", "any_side_effect", "side_effect"))

if (is.null(pid_col))  stop("Could not find participant ID column.")
if (is.null(sess_col)) stop("Could not find session-number column.")
if (is.null(vdq_col))  stop("Could not find WP1 discomfort / VDQ column.")

message("Participant column: ", pid_col)
message("Session column:     ", sess_col)
message("VDQ column:         ", vdq_col)
message("Side-effect column: ", ifelse(is.null(sidefx_col), "NULL", sidefx_col))

wp1_long <- tibble(
  part_id = clean_id(wp1[[pid_col]]),
  session_n = to_num(wp1[[sess_col]]),
  discomfort = to_num(wp1[[vdq_col]])
)

if (!is.null(sidefx_col)) {
  raw_sidefx <- wp1[[sidefx_col]] %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower()
  
  wp1_long <- wp1_long %>%
    mutate(
      any_side_effect = case_when(
        raw_sidefx %in% c("yes", "true", "1") ~ 1,
        raw_sidefx %in% c("no", "false", "0") ~ 0,
        TRUE ~ NA_real_
      )
    )
} else {
  wp1_long <- wp1_long %>%
    mutate(
      any_side_effect = case_when(
        !is.na(discomfort) ~ as.numeric(discomfort > 0),
        TRUE ~ NA_real_
      )
    )
}

wp1_long <- wp1_long %>%
  filter(
    !is.na(part_id),
    !is.na(session_n),
    !is.na(discomfort)
  ) %>%
  mutate(session_n = as.integer(session_n)) %>%
  filter(session_n >= 1, session_n <= 11)

# ===================================================
# SESSION-WISE SUMMARY
# ===================================================

wp1_session <- wp1_long %>%
  group_by(session_n) %>%
  summarise(
    n = sum(!is.na(discomfort)),
    mean_discomfort = mean(discomfort, na.rm = TRUE),
    sd_discomfort = sd(discomfort, na.rm = TRUE),
    se_discomfort = sd_discomfort / sqrt(n),
    pct_any_side_effect = 100 * mean(any_side_effect, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(session_n) %>%
  mutate(
    sd_discomfort = replace_na(sd_discomfort, 0),
    se_discomfort = replace_na(se_discomfort, 0),
    ucl80 = mean_discomfort + Z80 * se_discomfort
  )

print(wp1_session)

unique_participants <- n_distinct(wp1_long$part_id)
total_valid_rows <- nrow(wp1_long)
highest_mean <- max(wp1_session$mean_discomfort, na.rm = TRUE)
highest_ucl80 <- max(wp1_session$ucl80, na.rm = TRUE)
highest_ucl80_session <- wp1_session$session_n[which.max(wp1_session$ucl80)]

cat("\n")
cat("Unique WP1 participants: ", unique_participants, "\n", sep = "")
cat("Total valid WP1 session rows: ", total_valid_rows, "\n", sep = "")
cat("Highest mean discomfort: ", round(highest_mean, 2), "/10\n", sep = "")
cat("Highest one-sided 80% UCL: ", round(highest_ucl80, 2), "/10 at session ", highest_ucl80_session, "\n", sep = "")
cat("All one-sided 80% UCLs below threshold 7.0: ", all(wp1_session$ucl80 < THRESHOLD), "\n", sep = "")

# ===================================================
# PLOT DATA + AXIS LIMITS
# ===================================================

observed_upper <- max(wp1_session$ucl80, wp1_session$mean_discomfort, na.rm = TRUE)

# Keeps Panel A zoomed, but gives clean headroom.
panelA_ymax <- max(1.6, ceiling((observed_upper + 0.10) * 10) / 10)
panelA_ymax <- min(panelA_ymax, 2.0)

context_df <- tibble(
  y = 1,
  xmin = 0,
  xmax_threshold = THRESHOLD,
  xmax_ucl = highest_ucl80,
  label = paste0(
    "Highest upper 80%\nconfidence limit = ",
    sprintf("%.2f", highest_ucl80)
  )
)

# ===================================================
# COMMON PANEL THEME
# ===================================================

paper_panel_theme_f2 <- theme_minimal(base_family = PALATINO_NAME) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      colour = scales::alpha(COLORS_F2$grid, 0.22),
      linewidth = LINE_SIZES_F2$grid_line
    ),
    axis.line.x = element_line(
      colour = "grey30",
      linewidth = LINE_SIZES_F2$axis_line
    ),
    axis.line.y = element_line(
      colour = "grey30",
      linewidth = LINE_SIZES_F2$axis_line
    ),
    plot.title = element_text(
      family = PALATINO_NAME,
      face = "plain",
      size = FONT_SIZES_F2$panel_title,
      hjust = 0,
      colour = COLORS_F2$text,
      margin = margin(b = 6)
    ),
    axis.title = element_text(
      family = PALATINO_NAME,
      size = FONT_SIZES_F2$axis_title,
      colour = COLORS_F2$text
    ),
    axis.text = element_text(
      family = PALATINO_NAME,
      size = FONT_SIZES_F2$axis_text,
      colour = "grey25"
    ),
    legend.title = element_blank(),
    legend.text = element_text(
      family = PALATINO_NAME,
      size = FONT_SIZES_F2$legend_text
    ),
    legend.background = element_rect(
      fill = "white",
      colour = "#D1D5DB",
      linewidth = 0.35
    ),
    legend.key = element_rect(fill = "white", colour = NA),
    plot.margin = margin(4, 6, 4, 5)
  )

# ===================================================
# PANEL A — SESSION-WISE DISCOMFORT, ZOOMED
# ===================================================

pA <- ggplot(wp1_session, aes(x = session_n)) +
  
  # Compact interpretive context:
  # 0–1 is none/minimal discomfort; 1+ begins mild range.
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = 0,
    ymax = 1,
    fill = COLORS_F2$band_none,
    alpha = 1
  ) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = 1,
    ymax = Inf,
    fill = COLORS_F2$band_mild,
    alpha = 0.45
  ) +
  
  # Mean-to-UCL stems
  geom_segment(
    aes(
      xend = session_n,
      y = mean_discomfort,
      yend = ucl80
    ),
    colour = scales::alpha(COLORS_F2$ucl, 0.55),
    linewidth = LINE_SIZES_F2$error_stem
  ) +
  
  # UCL line only — no triangle point markers
  geom_line(
    aes(
      y = ucl80,
      colour = "One-sided 80% UCL",
      linetype = "One-sided 80% UCL"
    ),
    linewidth = LINE_SIZES_F2$ucl_line,
    alpha = 0.85
  ) +
  
  # Mean discomfort rating
  geom_line(
    aes(
      y = mean_discomfort,
      colour = "Mean discomfort rating",
      linetype = "Mean discomfort rating"
    ),
    linewidth = LINE_SIZES_F2$mean_line
  ) +
  geom_point(
    aes(
      y = mean_discomfort,
      colour = "Mean discomfort rating",
      shape = "Mean discomfort rating"
    ),
    size = POINT_SIZES_F2$mean_point
  ) +
  
  scale_colour_manual(
    values = c(
      "Mean discomfort rating" = COLORS_F2$mean,
      "One-sided 80% UCL" = COLORS_F2$ucl
    ),
    breaks = c(
      "Mean discomfort rating",
      "One-sided 80% UCL"
    ),
    name = NULL
  ) +
  scale_linetype_manual(
    values = c(
      "Mean discomfort rating" = "solid",
      "One-sided 80% UCL" = "dashed"
    ),
    breaks = c(
      "Mean discomfort rating",
      "One-sided 80% UCL"
    ),
    name = NULL
  ) +
  scale_shape_manual(
    values = c(
      "Mean discomfort rating" = 16
    ),
    breaks = c(
      "Mean discomfort rating"
    ),
    name = NULL
  ) +
  guides(
    colour = guide_legend(
      override.aes = list(
        shape = c(16, NA),
        linetype = c("solid", "dashed"),
        linewidth = c(LINE_SIZES_F2$mean_line, LINE_SIZES_F2$ucl_line)
      )
    ),
    linetype = "none",
    shape = "none"
  ) +
  scale_x_continuous(
    breaks = 1:11,
    limits = c(0.7, 11.3),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    limits = c(0, panelA_ymax),
    breaks = seq(0, panelA_ymax, by = 0.5),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "A. Session-wise discomfort ratings",
    x = "WP1 session",
    y = "Discomfort rating\n(0 = none, 10 = extreme)"
  ) +
  paper_panel_theme_f2 +
  theme(
    axis.title.y = element_text(
      lineheight = 0.78
    ),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.direction = "vertical",
    legend.background = element_rect(
      fill = "white",
      colour = "#D1D5DB",
      linewidth = 0.35
    ),
    legend.key = element_rect(
      fill = "white",
      colour = NA
    ),
    legend.key.size = unit(0.42, "cm"),
    legend.margin = margin(3, 5, 3, 5)
  )

# ===================================================
# PANEL B — THRESHOLD CONTEXT
# ===================================================

pB <- ggplot(context_df) +
  
  # Grey threshold bar from 0 to 7
  geom_segment(
    aes(
      x = 0,
      xend = THRESHOLD,
      y = y,
      yend = y
    ),
    colour = COLORS_F2$context_bg,
    linewidth = LINE_SIZES_F2$context_bar,
    lineend = "butt"
  ) +
  
  # Observed highest 80% UCL
  geom_segment(
    aes(
      x = 0,
      xend = xmax_ucl,
      y = y,
      yend = y
    ),
    colour = COLORS_F2$ucl,
    linewidth = LINE_SIZES_F2$context_bar,
    lineend = "butt"
  ) +
  
  # Threshold line
  geom_vline(
    xintercept = THRESHOLD,
    colour = COLORS_F2$threshold,
    linetype = "dashed",
    linewidth = LINE_SIZES_F2$threshold,
    alpha = 0.85
  ) +
  
  # Label for highest UCL
  geom_text(
    aes(
      x = 1.65,
      y = 1.07,
      label = label
    ),
    family = PALATINO_NAME,
    size = FONT_SIZES_F2$annotation / .pt,
    colour = COLORS_F2$text_soft,
    hjust = 0,
    vjust = 0.5,
    lineheight = 0.78
  ) +
  
  # Threshold label
  annotate(
    "text",
    x = THRESHOLD - 0.20,
    y = 0.66,
    label = "Discontinuation\nthreshold = 7/10",
    family = PALATINO_NAME,
    size = FONT_SIZES_F2$annotation / .pt,
    colour = COLORS_F2$threshold,
    hjust = 1,
    vjust = 0.5,
    lineheight = 0.78
  ) +
  
  scale_x_continuous(
    limits = c(0, THRESHOLD),
    breaks = c(0, 1, 3, 5, 7),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    limits = c(0.45, 1.45),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "B. Discontinuation-\nthreshold context",
    x = "Discomfort rating",
    y = NULL
  ) +
  paper_panel_theme_f2 +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = scales::alpha(COLORS_F2$grid, 0.22),
      linewidth = LINE_SIZES_F2$grid_line
    ),
    plot.title = element_text(
      family = PALATINO_NAME,
      face = "plain",
      size = FONT_SIZES_F2$panel_title,
      hjust = 0,
      lineheight = 0.28,
      colour = COLORS_F2$text,
      margin = margin(b = 4)
    ),
    legend.position = "none",
    plot.margin = margin(4, 12, 4, 8)
  )

# ===================================================
# COMBINE
# ===================================================

figure_2 <- pA + pB +
  plot_layout(widths = c(3.05, 1.55))

print(figure_2)

# ===================================================
# OPTIONAL EXPORT
# ===================================================

ggsave(
  filename = file.path(MANUSCRIPT_PLOT_DIR, "Figure2_WP1_DiscomfortRatings_ThresholdContext.png"),
  plot = figure_2,
  width = FIGURE_SIZE_F2$width,
  height = FIGURE_SIZE_F2$height,
  dpi = FIGURE_SIZE_F2$dpi,
  bg = "white"
)

ggsave(
  filename = file.path(MANUSCRIPT_PLOT_DIR, "Figure2_WP1_DiscomfortRatings_ThresholdContext.pdf"),
  plot = figure_2,
  width = FIGURE_SIZE_F2$width,
  height = FIGURE_SIZE_F2$height,
  dpi = FIGURE_SIZE_F2$dpi,
  bg = "white"
)

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

# ===================================================

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

# FIGURE 3

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

# ===================================================
# FIGURE 3 — COMBINED PANEL A + PANEL B
# Interim visual phenomenology + binary ranking
# ===================================================

# Install if needed:
# ===================================================
# SETUP
# ===================================================

DATA_DIR <- "C:/Users/dn284/Desktop/MRC_omni/data"
FONT_PATH <- file.path(DATA_DIR, "palatinolinotype_roman.ttf")

if (file.exists(FONT_PATH)) {
  font_add("PalatinoLinotype", regular = FONT_PATH)
  showtext_auto()
  PALATINO_NAME <- "PalatinoLinotype"
  message("Loaded font: ", PALATINO_NAME)
} else {
  PALATINO_NAME <- "serif"
  message("Palatino Linotype not found; using serif fallback.")
}

# Intervention first controls:
#   1. Legend order in Panel A
#   2. Bar order in Panel A
#   3. X-axis order in Panel B
CONDITION_ORDER <- c("Intervention", "Control")

PALETTE <- c(
  "Intervention" = "#F2A100",
  "Control"      = "#4F76BC"
)

# General bar opacity control
BAR_ALPHA <- 0.68

# ===================================================
# FIGURE 3 STYLE CONTROLS
# Matched approximately to Figure 2 text hierarchy
# ===================================================

FONT_SIZES_F3 <- list(
  panel_title  = 28,
  axis_title   = 24,
  axis_text    = 24,
  legend_text  = 24,
  bar_label    = 24,
  sig_label    = 24
)

LINE_SIZES_F3 <- list(
  bar_outline = 0.45,
  errorbar    = 0.70,
  bracket     = 0.55,
  axis_line   = 0.40,
  grid_line   = 0.30
)

POINT_SIZES_F3 <- list(
  individual = 1.45
)

FIGURE_SIZE_F3 <- list(
  width  = 6.8,
  height = 3.4,
  dpi    = 300
)

BAR_WIDTH <- 0.68
ERRORBAR_WIDTH <- 0.12

theme_set(
  theme_minimal(base_family = PALATINO_NAME) +
    theme(
      plot.title = element_text(
        face = "plain",
        size = FONT_SIZES_F3$panel_title,
        hjust = 0
      ),
      axis.title = element_text(size = FONT_SIZES_F3$axis_title),
      axis.text = element_text(size = FONT_SIZES_F3$axis_text),
      legend.text = element_text(size = FONT_SIZES_F3$legend_text)
    )
)

PRIMARY_DIMS <- c()
SECONDARY_DIMS <- c("Geometric Content", "Detail", "Vividness", "Focality", "Semantic Content", "Entropy")
DIM_ORDER <- c(PRIMARY_DIMS, SECONDARY_DIMS)

VHQ_DIMENSIONS <- list(
  "Geometric Content" = c("vhq_1", "vhq_2", "vhq_3"),
  "Semantic Content"  = c("vhq_4", "vhq_5", "vhq_6"),
  "Detail"            = c("vhq_7", "vhq_8", "vhq_9"),
  "Vividness"         = c("vhq_10", "vhq_11", "vhq_12"),
  "Entropy"           = c("vhq_13", "vhq_14", "vhq_15"),
  "Focality"          = c("vhq_16", "vhq_17", "vhq_18")
)

REVERSE_VHQ_ITEMS <- c("vhq_10", "vhq_11", "vhq_12", "vhq_17")

# ===================================================
# HELPERS
# ===================================================

SEARCH_DIRS <- c(DATA_DIR)

newest_match <- function(patterns, search_dirs = SEARCH_DIRS, required = TRUE) {
  hits <- character(0)
  
  for (d in search_dirs) {
    if (!dir.exists(d)) next
    
    all_files <- list.files(d, recursive = TRUE, full.names = TRUE)
    file_names <- basename(all_files)
    
    for (pat in patterns) {
      pat_regex <- pat
      pat_regex <- gsub("\\.", "\\\\.", pat_regex)
      pat_regex <- gsub("\\*", ".*", pat_regex)
      
      matched <- all_files[stringr::str_detect(file_names, stringr::regex(pat_regex, ignore_case = TRUE))]
      hits <- c(hits, matched)
    }
  }
  
  hits <- unique(hits[file.exists(hits)])
  
  if (length(hits) == 0) {
    if (required) stop("No files found for patterns: ", paste(patterns, collapse = ", "))
    return(NULL)
  }
  
  info <- file.info(hits)
  hits[order(info$mtime, decreasing = TRUE)][1]
}

clean_id <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    dplyr::na_if("") %>%
    dplyr::na_if("nan") %>%
    dplyr::na_if("NaN") %>%
    dplyr::na_if("None") %>%
    stringr::str_extract("\\d{3,6}")
}

sem <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(NA_real_)
  stats::sd(x) / sqrt(length(x))
}

# ===================================================
# LOAD RAW FILE
# ===================================================

CSV_PATH <- newest_match(c("*n32apr20*.csv"))
message("Using: ", basename(CSV_PATH))

df <- readr::read_csv(CSV_PATH, col_types = readr::cols(.default = readr::col_character()))
names(df) <- stringr::str_trim(names(df))

required_cols <- c("part_id", "session_n")
missing <- setdiff(required_cols, names(df))
if (length(missing) > 0) {
  stop("Missing required columns: ", paste(missing, collapse = ", "))
}

df <- df %>%
  mutate(
    part_id = clean_id(part_id),
    session_n = suppressWarnings(as.numeric(session_n))
  )

# ===================================================
# PANEL A DATA — VHQ BY CONDITION
# ===================================================

vhq_cols <- paste0("vhq_", 1:18)
missing_vhq <- setdiff(vhq_cols, names(df))
if (length(missing_vhq) > 0) {
  stop("Missing VHQ columns: ", paste(missing_vhq, collapse = ", "))
}

use <- df %>%
  filter(session_n %in% 1:4) %>%
  mutate(across(all_of(vhq_cols), ~ suppressWarnings(as.numeric(.x))))

for (col in REVERSE_VHQ_ITEMS) {
  if (col %in% names(use)) {
    use[[col]] <- 6 - use[[col]]
  }
}

use <- use %>%
  mutate(
    condition = case_when(
      session_n %in% c(1, 2) ~ "Control",
      session_n %in% c(3, 4) ~ "Intervention",
      TRUE ~ NA_character_
    ),
    condition = factor(condition, levels = CONDITION_ORDER)
  ) %>%
  filter(!is.na(condition))

# Row-level dimension means
for (dim in names(VHQ_DIMENSIONS)) {
  vals <- rowMeans(use[, VHQ_DIMENSIONS[[dim]]], na.rm = TRUE)
  vals[is.nan(vals)] <- NA_real_
  use[[dim]] <- vals
}

df_vhq <- use %>%
  select(part_id, session_n, condition, all_of(names(VHQ_DIMENSIONS))) %>%
  pivot_longer(
    cols = all_of(names(VHQ_DIMENSIONS)),
    names_to = "dimension",
    values_to = "mean_score"
  ) %>%
  filter(!is.na(mean_score))

pt_condition <- df_vhq %>%
  group_by(part_id, condition, dimension) %>%
  summarise(mean_score = mean(mean_score, na.rm = TRUE), .groups = "drop")

summary_df <- pt_condition %>%
  group_by(dimension, condition) %>%
  summarise(
    mean = mean(mean_score, na.rm = TRUE),
    se = sem(mean_score),
    sd = sd(mean_score, na.rm = TRUE),
    n = sum(!is.na(mean_score)),
    .groups = "drop"
  ) %>%
  mutate(
    dimension = factor(dimension, levels = DIM_ORDER),
    condition = factor(condition, levels = CONDITION_ORDER)
  ) %>%
  arrange(dimension, condition)

pt_condition <- pt_condition %>%
  mutate(
    dimension = factor(dimension, levels = DIM_ORDER),
    condition = factor(condition, levels = CONDITION_ORDER)
  ) %>%
  arrange(dimension, condition)

# ===================================================
# PANEL A STATS — paired Control vs Intervention by dimension
# ===================================================

p_to_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  ""
}

stats_df <- map_dfr(DIM_ORDER, function(dim_name) {
  sub <- pt_condition %>%
    filter(dimension == dim_name) %>%
    select(part_id, condition, mean_score) %>%
    pivot_wider(names_from = condition, values_from = mean_score)
  
  if (all(c("Control", "Intervention") %in% names(sub))) {
    paired <- sub %>% filter(!is.na(Control), !is.na(Intervention))
    n <- nrow(paired)
    
    if (n >= 2) {
      tt <- t.test(paired$Intervention, paired$Control, paired = TRUE)
      diff <- paired$Intervention - paired$Control
      d_z <- if (sd(diff, na.rm = TRUE) > 0) mean(diff, na.rm = TRUE) / sd(diff, na.rm = TRUE) else NA_real_
      p_val <- tt$p.value
      t_stat <- unname(tt$statistic)
    } else {
      n <- NA_real_
      p_val <- NA_real_
      t_stat <- NA_real_
      d_z <- NA_real_
    }
  } else {
    n <- NA_real_
    p_val <- NA_real_
    t_stat <- NA_real_
    d_z <- NA_real_
  }
  
  tibble(
    dimension = dim_name,
    n_paired = n,
    t_stat = t_stat,
    p_value = p_val,
    cohens_dz = d_z,
    stars = p_to_stars(p_val)
  )
})

print(stats_df)

# ===================================================
# PANEL A SIGNIFICANCE BRACKET POSITIONS
# ===================================================

sig_df <- summary_df %>%
  select(dimension, condition, mean, se) %>%
  mutate(se = replace_na(se, 0)) %>%
  left_join(stats_df %>% select(dimension, stars, p_value), by = "dimension") %>%
  group_by(dimension) %>%
  summarise(
    y_base = max(mean + se, na.rm = TRUE),
    stars = first(stars),
    p_value = first(p_value),
    .groups = "drop"
  ) %>%
  mutate(
    x = factor(dimension, levels = DIM_ORDER),
    y = y_base + 0.22
  ) %>%
  filter(stars != "")

# ===================================================
# PANEL B DATA — BINARY RANKING
# Uses row order within participant × block, because the raw
# stage column is inconsistently coded for some participants.
# ===================================================

rank_required <- c("part_id", "block", "session_n", "exp_dim_session_1")
missing_rank <- setdiff(rank_required, names(df))
if (length(missing_rank) > 0) {
  stop("Missing ranking columns: ", paste(missing_rank, collapse = ", "))
}

df_rank <- df %>%
  mutate(
    block = suppressWarnings(as.numeric(block)),
    session_n = suppressWarnings(as.numeric(session_n)),
    exp_dim_session_1 = suppressWarnings(as.numeric(exp_dim_session_1))
  ) %>%
  filter(!is.na(part_id), !is.na(block), !is.na(session_n)) %>%
  arrange(part_id, block) %>%
  group_by(part_id, block) %>%
  mutate(stage_fixed = row_number()) %>%
  ungroup() %>%
  filter(stage_fixed %in% c(1, 2))

stage_pivot <- df_rank %>%
  select(part_id, block, stage_fixed, session_n) %>%
  pivot_wider(
    id_cols = c(part_id, block),
    names_from = stage_fixed,
    values_from = session_n,
    names_prefix = "session_"
  )

if (!all(c("session_1", "session_2") %in% names(stage_pivot))) {
  stop("Could not reconstruct stage 1 and stage 2 sessions from row order.")
}

rank_info <- df_rank %>%
  filter(!is.na(exp_dim_session_1)) %>%
  select(part_id, block, exp_dim_session_1) %>%
  distinct()

stage_pivot <- stage_pivot %>%
  left_join(rank_info, by = c("part_id", "block")) %>%
  filter(!is.na(exp_dim_session_1)) %>%
  mutate(
    higher_session_n = case_when(
      exp_dim_session_1 == 1 ~ session_1,
      exp_dim_session_1 == 2 ~ session_2,
      TRUE ~ NA_real_
    ),
    higher_condition = case_when(
      higher_session_n %in% c(1, 2) ~ "Control",
      higher_session_n %in% c(3, 4) ~ "Intervention",
      TRUE ~ NA_character_
    )
  )

message("Total exp_dim_session_1 rankings: ", nrow(rank_info))
message("Successfully classified rankings: ", sum(!is.na(stage_pivot$higher_condition)))

rank_summary <- stage_pivot %>%
  filter(!is.na(higher_condition)) %>%
  count(higher_condition, name = "count") %>%
  rename(condition = higher_condition) %>%
  complete(condition = CONDITION_ORDER, fill = list(count = 0)) %>%
  mutate(condition = factor(condition, levels = CONDITION_ORDER))

control_n <- rank_summary %>% filter(condition == "Control") %>% pull(count)
intervention_n <- rank_summary %>% filter(condition == "Intervention") %>% pull(count)
total_n <- control_n + intervention_n

binom_p <- if (total_n > 0) {
  binom.test(intervention_n, total_n, p = 0.5, alternative = "greater")$p.value
} else {
  NA_real_
}

panel_b_label <- case_when(
  is.na(binom_p) ~ "",
  binom_p < 0.001 ~ "***",
  binom_p < 0.01  ~ "**",
  binom_p < 0.05  ~ "*",
  TRUE ~ paste0("p = ", format(round(binom_p, 3), nsmall = 3))
)

# ===================================================
# COMMON PANEL THEME — FIGURE 3
# ===================================================

paper_panel_theme_f3 <- theme_minimal(base_family = PALATINO_NAME) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      colour = scales::alpha("grey70", 0.22),
      linewidth = LINE_SIZES_F3$grid_line
    ),
    axis.line.x = element_line(
      colour = "grey30",
      linewidth = LINE_SIZES_F3$axis_line
    ),
    axis.line.y = element_line(
      colour = "grey30",
      linewidth = LINE_SIZES_F3$axis_line
    ),
    plot.title = element_text(
      family = PALATINO_NAME,
      face = "plain",
      size = FONT_SIZES_F3$panel_title,
      hjust = 0,
      colour = "#111827",
      margin = margin(b = 6)
    ),
    axis.title = element_text(
      family = PALATINO_NAME,
      size = FONT_SIZES_F3$axis_title,
      colour = "#111827"
    ),
    axis.text = element_text(
      family = PALATINO_NAME,
      size = FONT_SIZES_F3$axis_text,
      colour = "grey25"
    ),
    legend.title = element_blank(),
    legend.text = element_text(
      family = PALATINO_NAME,
      size = FONT_SIZES_F3$legend_text
    ),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.margin = margin(5, 8, 5, 5)
  )

# ===================================================
# PANEL A PLOT
# ===================================================

panelA_ymax <- max(summary_df$mean + replace_na(summary_df$se, 0), na.rm = TRUE)
panelA_upper <- max(5.25, panelA_ymax + 0.65)

pA <- ggplot(summary_df, aes(x = dimension, y = mean)) +
  
  # Bars
  geom_col(
    aes(fill = condition),
    position = position_dodge(width = 0.75),
    width = BAR_WIDTH,
    linewidth = LINE_SIZES_F3$bar_outline,
    colour = "grey25",
    alpha = BAR_ALPHA
  ) +
  
  # Individual participant means
  geom_point(
    data = pt_condition,
    aes(
      x = dimension,
      y = mean_score,
      colour = condition
    ),
    position = position_jitterdodge(
      jitter.width = 0.10,
      dodge.width = 0.75,
      seed = 42
    ),
    size = POINT_SIZES_F3$individual,
    alpha = 0.28,
    stroke = 0
  ) +
  
  # SEM bars drawn last so they remain opaque and visible
  geom_errorbar(
    aes(
      ymin = mean - se,
      ymax = mean + se,
      colour = condition
    ),
    position = position_dodge(width = 0.75),
    width = ERRORBAR_WIDTH,
    linewidth = LINE_SIZES_F3$errorbar,
    alpha = 1
  ) +
  
  # Significance brackets
  geom_segment(
    data = sig_df,
    aes(
      x = as.numeric(x) - 0.17,
      xend = as.numeric(x) - 0.17,
      y = y - 0.03,
      yend = y
    ),
    inherit.aes = FALSE,
    linewidth = LINE_SIZES_F3$bracket
  ) +
  geom_segment(
    data = sig_df,
    aes(
      x = as.numeric(x) - 0.17,
      xend = as.numeric(x) + 0.17,
      y = y,
      yend = y
    ),
    inherit.aes = FALSE,
    linewidth = LINE_SIZES_F3$bracket
  ) +
  geom_segment(
    data = sig_df,
    aes(
      x = as.numeric(x) + 0.17,
      xend = as.numeric(x) + 0.17,
      y = y,
      yend = y - 0.03
    ),
    inherit.aes = FALSE,
    linewidth = LINE_SIZES_F3$bracket
  ) +
  geom_text(
    data = sig_df,
    aes(x = x, y = y + 0.05, label = stars),
    inherit.aes = FALSE,
    size = FONT_SIZES_F3$sig_label / .pt,
    family = PALATINO_NAME
  ) +
  
  scale_fill_manual(
    values = PALETTE,
    breaks = CONDITION_ORDER,
    name = NULL
  ) +
  scale_colour_manual(
    values = PALETTE,
    breaks = CONDITION_ORDER,
    guide = "none"
  ) +
  scale_y_continuous(
    breaks = 1:5,
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(ylim = c(0, panelA_upper), clip = "off") +
  labs(
    title = "A. Visual phenomenology",
    x = NULL,
    y = "Mean 6D-VHQ score (1–5)"
  ) +
  paper_panel_theme_f3 +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      size = FONT_SIZES_F3$axis_text
    ),
    legend.position = c(0.975, 1.125),
    legend.justification = c(1, 1),
    legend.direction = "vertical",
    legend.background = element_rect(
      fill = "white",
      colour = "#D1D5DB",
      linewidth = 0.35
    ),
    legend.key = element_rect(
      fill = "white",
      colour = NA
    ),
    legend.key.size = unit(0.42, "cm"),
    legend.margin = margin(3, 5, 3, 5),
    plot.margin = margin(5, 8, 5, 5)
  )

# ===================================================
# PANEL B PLOT
# ===================================================

rank_summary <- rank_summary %>%
  mutate(
    label = paste0(count, "/", total_n)
  )

ymax2 <- max(rank_summary$count, na.rm = TRUE)

bracket_y <- ymax2 + 6
bracket_tick <- 1.2
label_y <- bracket_y + 2.0
ymax2_upper <- label_y + 3.5

pB <- ggplot(rank_summary, aes(x = condition, y = count, fill = condition)) +
  geom_col(
    width = 0.62,
    alpha = BAR_ALPHA,
    linewidth = LINE_SIZES_F3$bar_outline,
    colour = "grey25"
  ) +
  geom_text(
    aes(label = label),
    vjust = -0.35,
    size = FONT_SIZES_F3$bar_label / .pt,
    family = PALATINO_NAME
  ) +
  
  annotate(
    "segment",
    x = 1,
    xend = 1,
    y = bracket_y - bracket_tick,
    yend = bracket_y,
    linewidth = LINE_SIZES_F3$bracket
  ) +
  annotate(
    "segment",
    x = 1,
    xend = 2,
    y = bracket_y,
    yend = bracket_y,
    linewidth = LINE_SIZES_F3$bracket
  ) +
  annotate(
    "segment",
    x = 2,
    xend = 2,
    y = bracket_y,
    yend = bracket_y - bracket_tick,
    linewidth = LINE_SIZES_F3$bracket
  ) +
  annotate(
    "text",
    x = 1.5,
    y = label_y,
    label = panel_b_label,
    size = FONT_SIZES_F3$sig_label / .pt,
    family = PALATINO_NAME
  ) +
  
  scale_fill_manual(
    values = PALETTE,
    breaks = CONDITION_ORDER,
    guide = "none"
  ) +
  scale_y_continuous(
    limits = c(0, ymax2_upper),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "B. Greater visual\nexperience ranking",
    x = NULL,
    y = "Count"
  ) +
  paper_panel_theme_f3 +
  theme(
    axis.text.x = element_text(size = FONT_SIZES_F3$axis_text),
    plot.title = element_text(
      family = PALATINO_NAME,
      face = "plain",
      size = FONT_SIZES_F3$panel_title,
      hjust = 0,
      lineheight = 0.28,
      colour = "#111827",
      margin = margin(b = 4)
    ),
    plot.margin = margin(5, 5, 5, 10)
  )

# ===================================================
# COMBINE
# ===================================================

combined_plot <- pA + pB +
  plot_layout(widths = c(4.1, 1.25))

print(combined_plot)

# ===================================================
# EXPORT
# ===================================================

ggsave(
  filename = file.path(MANUSCRIPT_PLOT_DIR, "Figure3_Combined_AB.png"),
  plot = combined_plot,
  width = FIGURE_SIZE_F3$width,
  height = FIGURE_SIZE_F3$height,
  dpi = FIGURE_SIZE_F3$dpi,
  bg = "white"
)

ggsave(
  filename = file.path(MANUSCRIPT_PLOT_DIR, "Figure3_Combined_AB.pdf"),
  plot = combined_plot,
  width = FIGURE_SIZE_F3$width,
  height = FIGURE_SIZE_F3$height,
  dpi = FIGURE_SIZE_F3$dpi,
  bg = "white"
)

# ===================================================

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

# FIGURE 4

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

# ===================================================
# FIGURE 4 — WP2 FEASIBILITY AND REPEATED-SESSION TOLERABILITY
# ===================================================

# install.packages(c("tidyverse", "patchwork", "showtext", "sysfonts", "scales"))
# ===================================================
# SETUP
# ===================================================

DATA_DIR <- "C:/Users/dn284/Desktop/MRC_omni/data"
SEARCH_DIRS <- c(DATA_DIR)
MANUSCRIPT_PLOT_DIR <- file.path(DATA_DIR, "manuscript_plots")
if (!dir.exists(MANUSCRIPT_PLOT_DIR)) dir.create(MANUSCRIPT_PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

FONT_PATH <- file.path(DATA_DIR, "palatinolinotype_roman.ttf")

if (file.exists(FONT_PATH)) {
  font_add("PalatinoLinotype", regular = FONT_PATH)
  showtext_auto()
  PALATINO_NAME <- "PalatinoLinotype"
  message("Loaded font: ", PALATINO_NAME)
} else {
  PALATINO_NAME <- "serif"
  message("Palatino Linotype not found; using serif fallback.")
}

# ===================================================
# FIGURE 4 STYLE CONTROLS
# Matched to Figure 3 visual style
# ===================================================

# Intervention first controls legend order and plotting order
CONDITION_ORDER <- c("Intervention", "Control")

PALETTE <- c(
  "Intervention" = "#F2A100",
  "Control"      = "#4F76BC"
)

COLORS <- c(
  "Control" = PALETTE["Control"],
  "Intervention" = PALETTE["Intervention"],
  "neutral" = "#64748B",
  "threshold" = "#DC2626",
  "grid" = "grey70",
  "text" = "#111827",
  "text_soft" = "#4B5563"
)

FONT_SIZES_F4 <- list(
  panel_title  = 28,
  axis_title   = 24,
  axis_text    = 24,
  legend_text  = 24,
  annotation   = 20,
  point_label  = 20
)

LINE_SIZES_F4 <- list(
  main_line = 1.15,
  errorbar  = 0.75,
  axis_line = 0.40,
  grid_line = 0.30,
  reference = 0.60
)

POINT_SIZES_F4 <- list(
  main_point = 3.0
)

FIGURE_SIZE_F4 <- list(
  width  = 6.8,
  height = 3.4,
  dpi    = 300
)

theme_set(
  theme_minimal(base_family = PALATINO_NAME) +
    theme(
      plot.title = element_text(
        face = "plain",
        size = FONT_SIZES_F4$panel_title,
        hjust = 0
      ),
      axis.title = element_text(size = FONT_SIZES_F4$axis_title),
      axis.text = element_text(size = FONT_SIZES_F4$axis_text),
      legend.text = element_text(size = FONT_SIZES_F4$legend_text)
    )
)

# ===================================================
# HELPERS
# ===================================================

newest_match <- function(patterns, search_dirs = SEARCH_DIRS, required = TRUE) {
  hits <- character(0)
  
  for (d in search_dirs) {
    if (!dir.exists(d)) next
    
    all_files <- list.files(d, recursive = TRUE, full.names = TRUE)
    file_names <- basename(all_files)
    
    for (pat in patterns) {
      pat_regex <- pat
      pat_regex <- gsub("\\.", "\\\\.", pat_regex)
      pat_regex <- gsub("\\*", ".*", pat_regex)
      
      matched <- all_files[stringr::str_detect(
        file_names,
        stringr::regex(pat_regex, ignore_case = TRUE)
      )]
      
      hits <- c(hits, matched)
    }
  }
  
  hits <- unique(hits[file.exists(hits)])
  
  if (length(hits) == 0) {
    if (required) stop("No files found for patterns: ", paste(patterns, collapse = ", "))
    return(NULL)
  }
  
  info <- file.info(hits)
  hits[order(info$mtime, decreasing = TRUE)][1]
}

read_qualtrics_real <- function(path, skiprows = NULL) {
  df <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    skip = ifelse(is.null(skiprows), 0, skiprows)
  )
  
  names(df) <- stringr::str_trim(names(df))
  
  if ("ResponseId" %in% names(df)) {
    df <- df %>%
      filter(stringr::str_starts(as.character(ResponseId), "R_"))
  }
  
  df
}

clean_id <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    dplyr::na_if("") %>%
    dplyr::na_if("nan") %>%
    dplyr::na_if("NaN") %>%
    dplyr::na_if("None") %>%
    dplyr::na_if('{"ImportId":"part_id"}') %>%
    stringr::str_extract("\\d{3,6}")
}

find_col <- function(df, candidates) {
  nms <- names(df)
  lower_map <- setNames(nms, tolower(nms))
  
  for (cand in candidates) {
    if (tolower(cand) %in% names(lower_map)) {
      return(lower_map[[tolower(cand)]])
    }
  }
  
  for (nm in nms) {
    nm_lower <- tolower(nm)
    if (any(stringr::str_detect(nm_lower, fixed(tolower(candidates))))) {
      return(nm)
    }
  }
  
  NULL
}

to_num <- function(x) suppressWarnings(as.numeric(x))

sem <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(NA_real_)
  sd(x) / sqrt(length(x))
}

compute_tol_score_if_needed <- function(df) {
  if ("tol_score" %in% names(df)) {
    return(pmin(pmax(to_num(df$tol_score), 0), 10))
  }
  
  tol_cols <- names(df)[stringr::str_detect(tolower(names(df)), "^tol_follow")]
  
  if (length(tol_cols) == 0) {
    return(rep(NA_real_, nrow(df)))
  }
  
  tmp <- df %>%
    select(all_of(tol_cols)) %>%
    mutate(across(everything(), to_num)) %>%
    replace(is.na(.), 0)
  
  pmin(pmax(do.call(pmax, c(tmp, na.rm = TRUE)), 0), 10)
}

# ===================================================
# LOCATE FILES
# ===================================================

WP2_ASSIGN_PATH <- newest_match(c("*wp2_assignments*.csv"))
WP2_PRE1_PATH   <- newest_match(c("*wp2_pre_session_1*.csv"))

WP2_POST13_PATH <- newest_match(
  c(
    "*wp2_post_session_1_3*.csv",
    "*wp2_post_session_13*.csv",
    "*wp2_post*session*1*3*.csv"
  ),
  required = FALSE
)

WP2_POST4_PATH <- newest_match(c("*wp2_post_session_4*.csv"))

message("Assignments: ", basename(WP2_ASSIGN_PATH))
message("Pre session 1: ", basename(WP2_PRE1_PATH))
message("Post session 1-3: ", ifelse(is.null(WP2_POST13_PATH), "NULL", basename(WP2_POST13_PATH)))
message("Post session 4: ", basename(WP2_POST4_PATH))

# ===================================================
# LOAD
# ===================================================

assign_df <- read_qualtrics_real(WP2_ASSIGN_PATH)
pre1 <- read_qualtrics_real(WP2_PRE1_PATH)
post4 <- read_qualtrics_real(WP2_POST4_PATH)
post13 <- if (!is.null(WP2_POST13_PATH)) {
  read_qualtrics_real(WP2_POST13_PATH)
} else {
  tibble()
}

# ===================================================
# CLEAN CORE IDS / SESSION NUMBERS
# ===================================================

assign_df <- assign_df %>%
  mutate(part_id = clean_id(part_id))

arm_col <- find_col(assign_df, c("condition", "arm", "group", "allocation", "assigned_condition"))

if (is.null(arm_col)) {
  stop("Could not find WP2 assignment arm/condition column in assignments file.")
}

assign_df <- assign_df %>%
  mutate(
    arm_raw = stringr::str_trim(.data[[arm_col]]),
    condition = case_when(
      stringr::str_detect(stringr::str_to_lower(arm_raw), "control") ~ "Control",
      stringr::str_detect(stringr::str_to_lower(arm_raw), "intervention") ~ "Intervention",
      TRUE ~ NA_character_
    ),
    condition = factor(condition, levels = CONDITION_ORDER)
  ) %>%
  filter(!is.na(part_id), !is.na(condition))

randomised_ids <- assign_df %>%
  pull(part_id) %>%
  unique()

arm_map <- assign_df %>%
  mutate(condition = as.character(condition)) %>%
  distinct(part_id, condition) %>%
  deframe()

add_session_num <- function(df) {
  if (nrow(df) == 0) return(df)
  
  sess_col <- find_col(df, c("session_n", "session", "session_number"))
  
  if (is.null(sess_col)) {
    df$session_n_num <- NA_real_
  } else {
    df$session_n_num <- to_num(df[[sess_col]])
  }
  
  df %>%
    mutate(part_id = clean_id(part_id))
}

pre1 <- add_session_num(pre1)
post4 <- add_session_num(post4)
post13 <- add_session_num(post13)

if (all(is.na(pre1$session_n_num))) {
  pre1 <- pre1 %>%
    mutate(session_n_num = 1)
}

if (all(is.na(post4$session_n_num))) {
  post4 <- post4 %>%
    mutate(session_n_num = 4)
}

# ===================================================
# PANEL A DATA — RETENTION / FEASIBILITY
# ===================================================

sess1_ids <- pre1 %>%
  filter(!is.na(part_id), part_id %in% randomised_ids) %>%
  pull(part_id) %>%
  unique()

sess2_ids <- if (nrow(post13) > 0) {
  post13 %>%
    filter(session_n_num == 2, !is.na(part_id), part_id %in% randomised_ids) %>%
    pull(part_id) %>%
    unique()
} else {
  character(0)
}

sess3_ids <- if (nrow(post13) > 0) {
  post13 %>%
    filter(session_n_num == 3, !is.na(part_id), part_id %in% randomised_ids) %>%
    pull(part_id) %>%
    unique()
} else {
  character(0)
}

sess4_ids <- post4 %>%
  filter(!is.na(part_id), part_id %in% randomised_ids) %>%
  pull(part_id) %>%
  unique()

session_id_list <- list(
  `1` = sess1_ids,
  `2` = sess2_ids,
  `3` = sess3_ids,
  `4` = sess4_ids
)

retention_df <- map_dfr(1:4, function(s) {
  ids <- session_id_list[[as.character(s)]]
  conds <- arm_map[ids]
  
  tibble(
    session_n = s,
    session_label = paste("Session", s),
    Control = sum(conds == "Control", na.rm = TRUE),
    Intervention = sum(conds == "Intervention", na.rm = TRUE)
  )
}) %>%
  mutate(All = Control + Intervention)

print(retention_df)

# ===================================================
# TOLERABILITY-RELATED DROPOUTS
# ===================================================

post13_tol <- if (nrow(post13) > 0) compute_tol_score_if_needed(post13) else numeric(0)
post4_tol  <- compute_tol_score_if_needed(post4)

if (nrow(post13) > 0) post13$tol_score_num <- post13_tol
post4$tol_score_num <- post4_tol

dropout_ids <- setdiff(sess1_ids, sess4_ids)
tol_dropout_ids <- c()

if (nrow(post13) > 0) {
  tol_dropout_ids <- c(
    tol_dropout_ids,
    post13 %>%
      filter(part_id %in% dropout_ids, !is.na(tol_score_num), tol_score_num > 0) %>%
      pull(part_id)
  )
}

tol_dropout_ids <- c(
  tol_dropout_ids,
  post4 %>%
    filter(part_id %in% dropout_ids, !is.na(tol_score_num), tol_score_num > 0) %>%
    pull(part_id)
)

tolerability_dropouts <- length(unique(tol_dropout_ids))

# ===================================================
# PANEL B DATA — SESSION-WISE DISCOMFORT BY ARM
# ===================================================

if (nrow(post13) > 0) {
  post13$tol_score_num <- compute_tol_score_if_needed(post13)
}

post4$tol_score_num <- compute_tol_score_if_needed(post4)

tol13 <- if (nrow(post13) > 0) {
  post13 %>%
    filter(
      session_n_num %in% c(1, 2, 3),
      !is.na(tol_score_num),
      part_id %in% randomised_ids
    ) %>%
    select(part_id, session_n_num, tol_score_num)
} else {
  tibble(
    part_id = character(),
    session_n_num = numeric(),
    tol_score_num = numeric()
  )
}

tol4 <- post4 %>%
  filter(
    !is.na(tol_score_num),
    part_id %in% randomised_ids
  ) %>%
  select(part_id, session_n_num, tol_score_num)

tol_all <- bind_rows(tol13, tol4) %>%
  rename(
    session_n = session_n_num,
    tol_score = tol_score_num
  ) %>%
  mutate(
    condition = unname(arm_map[part_id]),
    condition = factor(condition, levels = CONDITION_ORDER)
  ) %>%
  filter(!is.na(condition))

summary_tol <- tol_all %>%
  group_by(session_n, condition) %>%
  summarise(
    mean = mean(tol_score, na.rm = TRUE),
    se = sem(tol_score),
    sd = sd(tol_score, na.rm = TRUE),
    n = sum(!is.na(tol_score)),
    .groups = "drop"
  ) %>%
  mutate(condition = factor(condition, levels = CONDITION_ORDER)) %>%
  arrange(session_n, condition)

summary_tol_pooled <- tol_all %>%
  group_by(session_n) %>%
  summarise(
    mean = mean(tol_score, na.rm = TRUE),
    se = sem(tol_score),
    sd = sd(tol_score, na.rm = TRUE),
    n = sum(!is.na(tol_score)),
    .groups = "drop"
  ) %>%
  arrange(session_n)

print(summary_tol)
print(summary_tol_pooled)

# ===================================================
# ===================================================
# TOLERABILITY SUMMARIES WITH 95% CONFIDENCE INTERVALS
# ===================================================

ci95_summary <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  
  n <- length(x)
  m <- if (n > 0) mean(x) else NA_real_
  s <- if (n > 1) sd(x) else NA_real_
  se <- if (n > 1) s / sqrt(n) else NA_real_
  
  if (n > 1 && !is.na(se)) {
    crit <- stats::qt(0.975, df = n - 1)
    ci_low <- m - crit * se
    ci_high <- m + crit * se
  } else {
    ci_low <- NA_real_
    ci_high <- NA_real_
  }
  
  tibble(
    mean = m,
    sd = s,
    se = se,
    n = n,
    ci_low = ci_low,
    ci_high = ci_high
  )
}

summary_tol <- tol_all %>%
  group_by(session_n, condition) %>%
  summarise(
    ci95_summary(tol_score),
    .groups = "drop"
  ) %>%
  mutate(
    condition = factor(condition, levels = c("Control", "Intervention")),
    ci_low_plot = pmax(ci_low, 0),
    ci_high_plot = ci_high
  ) %>%
  arrange(session_n, condition)

summary_tol_pooled <- tol_all %>%
  group_by(session_n) %>%
  summarise(
    ci95_summary(tol_score),
    .groups = "drop"
  ) %>%
  mutate(
    ci_low_plot = pmax(ci_low, 0),
    ci_high_plot = ci_high
  ) %>%
  arrange(session_n)

print(summary_tol)
print(summary_tol_pooled)

# ===================================================
# COMMON PANEL THEME — FIGURE 4
# ===================================================

paper_panel_theme_f4 <- theme_minimal(base_family = PALATINO_NAME) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      colour = scales::alpha(COLORS["grid"], 0.22),
      linewidth = LINE_SIZES_F4$grid_line
    ),
    axis.line.x = element_line(
      colour = "grey30",
      linewidth = LINE_SIZES_F4$axis_line
    ),
    axis.line.y = element_line(
      colour = "grey30",
      linewidth = LINE_SIZES_F4$axis_line
    ),
    plot.title = element_text(
      family = PALATINO_NAME,
      face = "plain",
      size = FONT_SIZES_F4$panel_title,
      hjust = 0,
      lineheight = 0.90,
      colour = COLORS["text"],
      margin = margin(b = 6)
    ),
    axis.title = element_text(
      family = PALATINO_NAME,
      size = FONT_SIZES_F4$axis_title,
      lineheight = 0.28,
      colour = COLORS["text"]
    ),
    axis.text = element_text(
      family = PALATINO_NAME,
      size = FONT_SIZES_F4$axis_text,
      colour = "grey25"
    ),
    legend.title = element_blank(),
    legend.text = element_text(
      family = PALATINO_NAME,
      size = FONT_SIZES_F4$legend_text
    ),
    legend.background = element_rect(
      fill = "white",
      colour = "#D1D5DB",
      linewidth = 0.35
    ),
    legend.key = element_rect(fill = "white", colour = NA),
    plot.margin = margin(5, 8, 5, 5)
  )

# ===================================================
# PANEL A PLOT — SESSION COMPLETION (%)
# ===================================================

ret_long <- retention_df %>%
  select(session_n, session_label, Control, Intervention) %>%
  pivot_longer(
    cols = c(Control, Intervention),
    names_to = "condition",
    values_to = "count"
  ) %>%
  mutate(
    condition = factor(condition, levels = c("Control", "Intervention"))
  )

denoms <- assign_df %>%
  count(condition, name = "n_randomised") %>%
  mutate(
    condition = factor(condition, levels = c("Control", "Intervention"))
  )

ret_percent <- ret_long %>%
  left_join(denoms, by = "condition") %>%
  mutate(
    percent_complete = 100 * count / n_randomised,
    count_label = paste0(count, "/", n_randomised),
    label_y = case_when(
      condition == "Control" ~ percent_complete - 2.0,
      condition == "Intervention" ~ percent_complete + 1.7,
      TRUE ~ percent_complete
    )
  )

pA <- ggplot(
  ret_percent,
  aes(
    x = session_n,
    y = percent_complete,
    colour = condition,
    group = condition
  )
) +
  geom_line(linewidth = LINE_SIZES_F4$main_line) +
  geom_point(size = POINT_SIZES_F4$main_point) +
  geom_text(
    aes(
      y = label_y,
      label = count_label
    ),
    family = PALATINO_NAME,
    size = FONT_SIZES_F4$point_label / .pt,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_colour_manual(values = PALETTE, name = NULL) +
  scale_x_continuous(
    breaks = 1:4,
    labels = paste("Session", 1:4),
    expand = expansion(mult = c(0.04, 0.06))
  ) +
  scale_y_continuous(
    limits = c(70, 105),
    breaks = seq(70, 100, 5),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "A. Session completion",
    x = "Session",
    y = "Session completion (%)"
  ) +
  paper_panel_theme_f4 +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.direction = "vertical",
    legend.background = element_rect(
      fill = "white",
      colour = "#D1D5DB",
      linewidth = 0.35
    ),
    legend.key = element_rect(
      fill = "white",
      colour = NA
    ),
    legend.key.size = unit(0.34, "cm"),
    legend.margin = margin(2, 4, 2, 4),
    legend.text = element_text(
      family = PALATINO_NAME,
      size = FONT_SIZES_F4$legend_text
    )
  )

# ===================================================
# PANEL B PLOT — MEAN DISCOMFORT BY SESSION, 95% CI
# ===================================================

pB_data <- summary_tol %>%
  mutate(
    condition = factor(condition, levels = c("Control", "Intervention"))
  )

ymaxB <- max(
  pB_data$ci_high_plot,
  pB_data$mean,
  na.rm = TRUE
)

upperB <- max(1.05, ymaxB + 0.12)

pB <- ggplot(
  pB_data,
  aes(
    x = session_n,
    y = mean,
    colour = condition,
    group = condition
  )
) +
  annotate(
    "segment",
    x = 1,
    xend = 4,
    y = 1,
    yend = 1,
    colour = scales::alpha(COLORS["neutral"], 0.40),
    linetype = "dotted",
    linewidth = LINE_SIZES_F4$reference
  ) +
  annotate(
    "text",
    x = 1.05,
    y = 1.04,
    label = "Zoomed to observed range",
    family = PALATINO_NAME,
    size = FONT_SIZES_F4$annotation / .pt,
    colour = COLORS["text_soft"],
    hjust = 0
  ) +
  annotate(
    "text",
    x = 3.95,
    y = 1.04,
    label = "1/10",
    family = PALATINO_NAME,
    size = FONT_SIZES_F4$annotation / .pt,
    colour = COLORS["text_soft"],
    hjust = 1
  ) +
  geom_line(linewidth = LINE_SIZES_F4$main_line) +
  geom_point(size = POINT_SIZES_F4$main_point) +
  geom_errorbar(
    aes(
      ymin = ci_low_plot,
      ymax = ci_high_plot
    ),
    width = 0.10,
    linewidth = LINE_SIZES_F4$errorbar,
    alpha = 1
  ) +
  scale_colour_manual(values = PALETTE, name = NULL) +
  scale_x_continuous(
    breaks = 1:4,
    labels = paste("Session", 1:4),
    expand = expansion(mult = c(0.04, 0.06))
  ) +
  scale_y_continuous(
    limits = c(0, upperB),
    breaks = seq(0, upperB, by = 0.2),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "B. Mean discomfort by session",
    x = "Session",
    y = "Discomfort rating\n(mean and 95% CI)\n0 = none, 10 = extreme"
  ) +
  paper_panel_theme_f4 +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 8)
  )

# ===================================================
# COMBINE
# ===================================================

combined_plot <- pA + pB +
  plot_layout(widths = c(1.08, 1.00))

print(combined_plot)

# ===================================================
# EXPORT
# ===================================================

ggsave(
  filename = file.path(MANUSCRIPT_PLOT_DIR, "Figure4_WP2_Feasibility_Tolerability.png"),
  plot = combined_plot,
  width = FIGURE_SIZE_F4$width,
  height = FIGURE_SIZE_F4$height,
  dpi = FIGURE_SIZE_F4$dpi,
  bg = "white"
)

ggsave(
  filename = file.path(MANUSCRIPT_PLOT_DIR, "Figure4_WP2_Feasibility_Tolerability.pdf"),
  plot = combined_plot,
  width = FIGURE_SIZE_F4$width,
  height = FIGURE_SIZE_F4$height,
  dpi = FIGURE_SIZE_F4$dpi,
  bg = "white"
)

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

# ===================================================

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

# FIGURE 5

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

# ===================================================
# FIGURE 5 — WP2 EXPLORATORY CLINICAL OUTCOMES
# PHQ-9 and BDI-II change over WP2
# ===================================================

# Required packages:
# library(tidyverse)
# library(patchwork)
# library(showtext)
# library(sysfonts)
# library(scales)

# ===================================================
# SETUP
# ===================================================

DATA_DIR <- "C:/Users/dn284/Desktop/MRC_omni/data"
SEARCH_DIRS <- c(DATA_DIR)
MANUSCRIPT_PLOT_DIR <- file.path(DATA_DIR, "manuscript_plots")
if (!dir.exists(MANUSCRIPT_PLOT_DIR)) dir.create(MANUSCRIPT_PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

FONT_PATH <- file.path(DATA_DIR, "palatinolinotype_roman.ttf")

if (file.exists(FONT_PATH)) {
  font_add("PalatinoLinotype", regular = FONT_PATH)
  showtext_auto()
  PALATINO_NAME <- "PalatinoLinotype"
  message("Loaded font: ", PALATINO_NAME)
} else {
  PALATINO_NAME <- "serif"
  message("Palatino Linotype not found; using serif fallback.")
}

# ===================================================
# FIGURE 5 STYLE CONTROLS
# Matched to Figures 3–4 visual style
# ===================================================

# Intervention first controls legend order and plotting order
CONDITION_ORDER <- c("Intervention", "Control")

PALETTE <- c(
  "Intervention" = "#F2A100",
  "Control"      = "#4F76BC"
)

COLORS <- c(
  "Control" = PALETTE["Control"],
  "Intervention" = PALETTE["Intervention"],
  "neutral" = "#64748B",
  "grid" = "grey70",
  "text" = "#111827",
  "text_soft" = "#4B5563"
)

FONT_SIZES_F5 <- list(
  panel_title    = 28,
  axis_title     = 24,
  axis_text      = 24,
  legend_text    = 24,
  annotation     = 20,
  severity_label = 22
)

LINE_SIZES_F5 <- list(
  main_line = 1.15,
  errorbar  = 0.75,
  axis_line = 0.40,
  grid_line = 0.30,
  band_line = 0.30
)

POINT_SIZES_F5 <- list(
  main_point = 3.0
)

FIGURE_SIZE_F5 <- list(
  width  = 7.4,
  height = 3.9,
  dpi    = 300
)

theme_set(
  theme_minimal(base_family = PALATINO_NAME) +
    theme(
      plot.title = element_text(
        face = "plain",
        size = FONT_SIZES_F5$panel_title,
        hjust = 0
      ),
      axis.title = element_text(size = FONT_SIZES_F5$axis_title),
      axis.text = element_text(size = FONT_SIZES_F5$axis_text),
      legend.text = element_text(size = FONT_SIZES_F5$legend_text)
    )
)

# ===================================================
# HELPERS
# ===================================================

newest_match <- function(patterns, search_dirs = SEARCH_DIRS, required = TRUE) {
  hits <- character(0)
  
  for (d in search_dirs) {
    if (!dir.exists(d)) next
    
    all_files <- list.files(d, recursive = TRUE, full.names = TRUE)
    file_names <- basename(all_files)
    
    for (pat in patterns) {
      pat_regex <- pat
      pat_regex <- gsub("\\.", "\\\\.", pat_regex)
      pat_regex <- gsub("\\*", ".*", pat_regex)
      
      matched <- all_files[stringr::str_detect(
        file_names,
        stringr::regex(pat_regex, ignore_case = TRUE)
      )]
      
      hits <- c(hits, matched)
    }
  }
  
  hits <- unique(hits[file.exists(hits)])
  
  if (length(hits) == 0) {
    if (required) stop("No files found for patterns: ", paste(patterns, collapse = ", "))
    return(NULL)
  }
  
  info <- file.info(hits)
  hits[order(info$mtime, decreasing = TRUE)][1]
}

read_qualtrics_real <- function(path, skiprows = NULL) {
  df <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    skip = ifelse(is.null(skiprows), 0, skiprows)
  )
  
  names(df) <- names(df) %>%
    stringr::str_trim() %>%
    tolower()
  
  if ("responseid" %in% names(df)) {
    df <- df %>%
      filter(stringr::str_starts(as.character(responseid), "R_"))
  }
  
  df
}

clean_id <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    dplyr::na_if("") %>%
    dplyr::na_if("nan") %>%
    dplyr::na_if("none") %>%
    dplyr::na_if('{"importid":"part_id"}') %>%
    stringr::str_extract("\\d{3,6}")
}

sem <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(NA_real_)
  sd(x) / sqrt(length(x))
}

find_items <- function(cols, prefix, n_items) {
  out <- list()
  
  for (k in seq_len(n_items)) {
    patterns <- c(
      paste0("^", prefix, "[_\\- ]?0?", k, "$"),
      paste0("^", prefix, "0?", k, "$")
    )
    
    found <- NULL
    
    for (c in cols) {
      cl <- tolower(trimws(c))
      
      if (any(stringr::str_detect(cl, stringr::regex(patterns)))) {
        found <- c
        break
      }
    }
    
    if (!is.null(found)) {
      out[[as.character(k)]] <- found
    }
  }
  
  unname(unlist(out))
}

qnum <- function(x) {
  if (is.na(x)) return(NA_real_)
  
  s <- trimws(as.character(x))
  
  if (s == "" || tolower(s) %in% c("nan", "none")) {
    return(NA_real_)
  }
  
  direct <- suppressWarnings(as.numeric(s))
  
  if (!is.na(direct)) {
    return(direct)
  }
  
  m <- stringr::str_match(s, "^\\s*([0-9]+)")
  
  if (!is.na(m[1, 2])) {
    return(as.numeric(m[1, 2]))
  }
  
  NA_real_
}

ensure_part_id <- function(df) {
  if (!"part_id" %in% names(df)) {
    for (alt in c("participant_id", "participantid", "externalreference", "id")) {
      if (alt %in% names(df)) {
        names(df)[names(df) == alt] <- "part_id"
        break
      }
    }
  }
  
  if (!"part_id" %in% names(df)) {
    stop("Could not find participant ID column.")
  }
  
  df %>%
    mutate(part_id = clean_id(part_id))
}

add_condition <- function(df, assign_df) {
  df %>%
    left_join(assign_df %>% select(part_id, condition), by = "part_id") %>%
    mutate(
      condition = trimws(as.character(condition)),
      condition = case_when(
        tolower(condition) == "control" ~ "Control",
        tolower(condition) == "intervention" ~ "Intervention",
        TRUE ~ condition
      ),
      condition = factor(condition, levels = CONDITION_ORDER)
    )
}

compute_phq_sum <- function(d) {
  if ("phq9_sum" %in% names(d)) {
    out <- suppressWarnings(as.numeric(d$phq9_sum))
    if (sum(!is.na(out)) > 0) return(out)
  }
  
  phq_cols <- find_items(names(d), "phq9", 9)
  
  if (length(phq_cols) < 7) {
    phq_cols <- find_items(names(d), "phq", 9)
  }
  
  if (length(phq_cols) > 0) {
    tmp <- d[, phq_cols, drop = FALSE] %>%
      mutate(across(everything(), ~ vapply(.x, qnum, numeric(1))))
    
    return(rowSums(tmp, na.rm = FALSE))
  }
  
  rep(NA_real_, nrow(d))
}

compute_bdi_sum <- function(d) {
  for (cand in c("bdi_sum", "bdi_sum_calc", "bdi_total", "bdi_ii_total")) {
    if (cand %in% names(d)) {
      out <- suppressWarnings(as.numeric(d[[cand]]))
      if (sum(!is.na(out)) > 0) return(out)
    }
  }
  
  item_cols <- names(d)[grepl("^bdi_\\d{1,2}$", names(d), ignore.case = TRUE)]
  
  if (length(item_cols) > 0) {
    item_nums <- as.numeric(sub("^bdi_(\\d{1,2})$", "\\1", item_cols, ignore.case = TRUE))
    item_cols <- item_cols[order(item_nums)]
    
    tmp <- d[, item_cols, drop = FALSE]
    tmp[] <- lapply(tmp, function(x) vapply(x, qnum, numeric(1)))
    
    out <- rowSums(tmp, na.rm = FALSE)
    out[is.nan(out)] <- NA_real_
    
    return(out)
  }
  
  rep(NA_real_, nrow(d))
}

prepare_file <- function(path, kind, force_session_n = NULL) {
  if (is.null(path)) return(NULL)
  
  d <- read_qualtrics_real(path)
  d <- ensure_part_id(d)
  
  if (!is.null(force_session_n)) {
    d$session_n <- force_session_n
  } else if ("session_n" %in% names(d)) {
    d$session_n <- suppressWarnings(as.numeric(d$session_n))
  } else {
    d$session_n <- NA_real_
  }
  
  if (kind == "pre") {
    d$timepoint <- ifelse(
      !is.na(d$session_n),
      paste0("session", as.integer(d$session_n), "_pre"),
      NA
    )
  } else if (kind == "post") {
    d$timepoint <- ifelse(
      !is.na(d$session_n),
      paste0("session", as.integer(d$session_n), "_post"),
      NA
    )
  } else if (kind == "final_post") {
    d$timepoint <- "final_post"
  }
  
  d$phq9_sum <- compute_phq_sum(d)
  d$bdi_sum <- compute_bdi_sum(d)
  
  message(
    basename(path), ": ",
    "timepoints=", paste(sort(unique(na.omit(d$timepoint))), collapse = ", "),
    ", PHQ non-missing=", sum(!is.na(d$phq9_sum)),
    ", BDI non-missing=", sum(!is.na(d$bdi_sum))
  )
  
  d %>%
    select(part_id, session_n, timepoint, phq9_sum, bdi_sum)
}

# ===================================================
# LOCATE FILES
# ===================================================

ASSIGN_PATH   <- newest_match(c("*wp2_assignments*.csv"))
PRE1_PATH     <- newest_match(c("*wp2_pre_session_1*.csv"))
PRE24_PATH    <- newest_match(c("*wp2_pre_sessions_2-4*.csv"), required = FALSE)
POST13_PATH   <- newest_match(c("*wp2_post_sessions_1-3*.csv"), required = FALSE)
POST4_PATH    <- newest_match(c("*wp2_post_session_4*.csv"), required = FALSE)
SMS_POST_PATH <- newest_match(c("*wp2_sms_post*.csv", "*sms_post*.csv"), required = FALSE)

message("Assignments: ", basename(ASSIGN_PATH))
message("Pre-S1: ", basename(PRE1_PATH))
message("Pre-S2-4: ", ifelse(is.null(PRE24_PATH), "NULL", basename(PRE24_PATH)))
message("Post-S1-3: ", ifelse(is.null(POST13_PATH), "NULL", basename(POST13_PATH)))
message("Post-S4: ", ifelse(is.null(POST4_PATH), "NULL", basename(POST4_PATH)))
message("SMS post: ", ifelse(is.null(SMS_POST_PATH), "NULL", basename(SMS_POST_PATH)))

# ===================================================
# LOAD ASSIGNMENTS
# ===================================================

assign <- read_qualtrics_real(ASSIGN_PATH)
assign <- ensure_part_id(assign)

if (!"condition" %in% names(assign)) {
  stop("Assignments file must contain a 'condition' column.")
}

assign <- assign %>%
  mutate(
    condition = trimws(as.character(condition)),
    condition = tolower(condition),
    condition = case_when(
      condition == "control" ~ "Control",
      condition == "intervention" ~ "Intervention",
      TRUE ~ condition
    ),
    condition = factor(condition, levels = CONDITION_ORDER)
  )

# ===================================================
# BUILD LONG DATA
# ===================================================

frames <- list(
  prepare_file(PRE1_PATH, "pre", force_session_n = 1),
  prepare_file(PRE24_PATH, "pre"),
  prepare_file(POST13_PATH, "post"),
  prepare_file(POST4_PATH, "post", force_session_n = 4),
  prepare_file(SMS_POST_PATH, "final_post")
)

df_long <- bind_rows(frames[!vapply(frames, is.null, logical(1))]) %>%
  filter(!is.na(part_id), !is.na(timepoint)) %>%
  add_condition(assign) %>%
  filter(condition %in% CONDITION_ORDER) %>%
  mutate(condition = factor(condition, levels = CONDITION_ORDER)) %>%
  group_by(part_id, condition, timepoint) %>%
  summarise(
    phq9_sum = if (all(is.na(phq9_sum))) NA_real_ else mean(phq9_sum, na.rm = TRUE),
    bdi_sum  = if (all(is.na(bdi_sum)))  NA_real_ else mean(bdi_sum, na.rm = TRUE),
    .groups = "drop"
  )

print(head(df_long, 10))
message("Available timepoints: ", paste(sort(unique(df_long$timepoint)), collapse = ", "))
message("Non-missing PHQ rows: ", sum(!is.na(df_long$phq9_sum)))
message("Non-missing BDI rows: ", sum(!is.na(df_long$bdi_sum)))

# ===================================================
# TIMEPOINT SELECTION
# ===================================================

phq_keep <- c(
  "session1_pre",
  "session2_pre",
  "session3_pre",
  "session4_pre",
  "session4_post"
)

include_final_post <- FALSE

if ("final_post" %in% df_long$timepoint) {
  final_counts <- df_long %>%
    filter(timepoint == "final_post") %>%
    count(condition, name = "n_final")
  
  base_counts <- df_long %>%
    filter(timepoint == "session1_pre") %>%
    count(condition, name = "n_base")
  
  coverage_df <- final_counts %>%
    inner_join(base_counts, by = "condition") %>%
    mutate(coverage = n_final / n_base)
  
  if (nrow(coverage_df) > 0) {
    coverage <- min(coverage_df$coverage, na.rm = TRUE)
    include_final_post <- !is.na(coverage) && coverage >= 0.60
  }
}

if (include_final_post) {
  phq_keep <- c(phq_keep, "final_post")
}

bdi_keep <- c(
  "session1_pre",
  "session4_post"
)

phq_lab_map <- c(
  "session1_pre"  = "Baseline",
  "session2_pre"  = "Pre-S2",
  "session3_pre"  = "Pre-S3",
  "session4_pre"  = "Pre-S4",
  "session4_post" = "Post-S4",
  "final_post"    = "1-week FU"
)

bdi_lab_map <- c(
  "session1_pre"  = "Baseline",
  "session4_post" = "Post-S4"
)

# ===================================================
# SEVERITY BANDS — PHQ-9 AND BDI-II
# ===================================================

PHQ_BANDS <- tibble::tribble(
  ~ymin, ~ymax, ~fill,      ~label_y, ~label,
  0,     4,     "#DCEAF7",  2.0,      "0–4\nMinimal",
  5,     9,     "#DDF2E3",  7.0,      "5–9\nMild",
  10,    14,    "#F6E6CF",  12.0,     "10–14\nModerate",
  15,    19,    "#F2D7D7",  17.0,     "15–19\nModerately\nSevere",
  20,    27,    "#E8C4C4",  23.5,     "20–27\nSevere"
)

BDI_BANDS <- tibble::tribble(
  ~ymin, ~ymax, ~fill,      ~label_y, ~label,
  0,     13,    "#DCEAF7",  6.5,      "0–13\nMinimal",
  14,    19,    "#DDF2E3",  16.5,     "14–19\nMild",
  20,    28,    "#F6E6CF",  24.0,     "20–28\nModerate",
  29,    63,    "#F2D7D7",  31.8,     "29+\nSevere"
)

SEVERITY_LABEL_SIZE <- FONT_SIZES_F5$severity_label / .pt

# ===================================================
# SUMMARIES
# ===================================================

phq <- df_long %>%
  filter(timepoint %in% phq_keep, !is.na(phq9_sum)) %>%
  mutate(
    tp = factor(timepoint, levels = phq_keep, ordered = TRUE),
    condition = factor(condition, levels = CONDITION_ORDER)
  )

phq_summary <- phq %>%
  group_by(condition, tp) %>%
  summarise(
    mean = mean(phq9_sum, na.rm = TRUE),
    sd = sd(phq9_sum, na.rm = TRUE),
    n = sum(!is.na(phq9_sum)),
    .groups = "drop"
  ) %>%
  mutate(
    sem = sd / sqrt(pmax(n, 1)),
    condition = factor(condition, levels = CONDITION_ORDER)
  ) %>%
  arrange(condition, tp)

bdi <- df_long %>%
  filter(timepoint %in% bdi_keep, !is.na(bdi_sum)) %>%
  mutate(
    tp = factor(timepoint, levels = bdi_keep, ordered = TRUE),
    condition = factor(condition, levels = CONDITION_ORDER)
  )

bdi_summary <- bdi %>%
  group_by(condition, tp) %>%
  summarise(
    mean = mean(bdi_sum, na.rm = TRUE),
    sd = sd(bdi_sum, na.rm = TRUE),
    n = sum(!is.na(bdi_sum)),
    .groups = "drop"
  ) %>%
  mutate(
    sem = sd / sqrt(pmax(n, 1)),
    condition = factor(condition, levels = CONDITION_ORDER)
  ) %>%
  arrange(condition, tp)

print(phq_summary)
print(bdi_summary)

message("PHQ timepoints: ", paste(phq_keep, collapse = ", "))
message("BDI timepoints: ", paste(bdi_keep, collapse = ", "))

# ===================================================
# SUMMARIES, FIGURE 5 PLOT, AND CLINICAL OUTCOME NARRATIVE
# Reviewer-facing update:
#   - Plot error bars = 95% confidence intervals
#   - Clinical change estimates include 95% CIs
#   - Intervention-minus-Control improvement differences include 95% CIs
# ===================================================

# ===================================================
# SUMMARY HELPERS
# ===================================================

ci95_summary <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  
  n <- length(x)
  m <- if (n > 0) mean(x) else NA_real_
  s <- if (n > 1) sd(x) else NA_real_
  se <- if (n > 1) s / sqrt(n) else NA_real_
  
  if (n > 1 && !is.na(se)) {
    crit <- stats::qt(0.975, df = n - 1)
    ci_low <- m - crit * se
    ci_high <- m + crit * se
  } else {
    ci_low <- NA_real_
    ci_high <- NA_real_
  }
  
  tibble::tibble(
    mean = m,
    sd = s,
    se = se,
    n = n,
    ci_low = ci_low,
    ci_high = ci_high
  )
}

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

fmt_ci <- function(lo, hi, digits = 2) {
  if (is.na(lo) || is.na(hi)) return("[NA, NA]")
  paste0("[", fmt_num(lo, digits), ", ", fmt_num(hi, digits), "]")
}

fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< .001")
  paste0("= ", sprintf("%.3f", p))
}

# ===================================================
# PHQ-9 LONG DATA AND SUMMARY WITH 95% CIs
# ===================================================

phq <- df_long %>%
  filter(timepoint %in% phq_keep, !is.na(phq9_sum)) %>%
  mutate(
    tp = factor(timepoint, levels = phq_keep, ordered = TRUE),
    condition = factor(condition, levels = c("Control", "Intervention"))
  )

phq_summary <- phq %>%
  group_by(condition, tp) %>%
  summarise(
    ci95_summary(phq9_sum),
    .groups = "drop"
  ) %>%
  mutate(
    ci_low_plot = pmax(ci_low, 0),
    ci_high_plot = ci_high
  )

# ===================================================
# BDI-II LONG DATA AND SUMMARY WITH 95% CIs
# ===================================================

bdi <- df_long %>%
  filter(timepoint %in% bdi_keep, !is.na(bdi_sum)) %>%
  mutate(
    tp = factor(timepoint, levels = bdi_keep, ordered = TRUE),
    condition = factor(condition, levels = c("Control", "Intervention"))
  )

bdi_summary <- bdi %>%
  group_by(condition, tp) %>%
  summarise(
    ci95_summary(bdi_sum),
    .groups = "drop"
  ) %>%
  mutate(
    ci_low_plot = pmax(ci_low, 0),
    ci_high_plot = ci_high
  )

print(phq_summary)
print(bdi_summary)

message("PHQ timepoints: ", paste(phq_keep, collapse = ", "))
message("BDI timepoints: ", paste(bdi_keep, collapse = ", "))

# ===================================================
# COMMON PANEL THEME — FIGURE 5
# ===================================================

paper_panel_theme_f5 <- theme_minimal(base_family = PALATINO_NAME) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      colour = scales::alpha(COLORS["grid"], 0.22),
      linewidth = LINE_SIZES_F5$grid_line
    ),
    axis.line.x = element_line(
      colour = "grey30",
      linewidth = LINE_SIZES_F5$axis_line
    ),
    axis.line.y = element_line(
      colour = "grey30",
      linewidth = LINE_SIZES_F5$axis_line
    ),
    plot.title = element_text(
      family = PALATINO_NAME,
      face = "plain",
      size = FONT_SIZES_F5$panel_title,
      hjust = 0,
      lineheight = 0.90,
      colour = COLORS["text"],
      margin = margin(b = 6)
    ),
    axis.title = element_text(
      family = PALATINO_NAME,
      size = FONT_SIZES_F5$axis_title,
      colour = COLORS["text"]
    ),
    axis.text = element_text(
      family = PALATINO_NAME,
      size = FONT_SIZES_F5$axis_text,
      colour = "grey25"
    ),
    legend.title = element_blank(),
    legend.text = element_text(
      family = PALATINO_NAME,
      size = FONT_SIZES_F5$legend_text
    ),
    legend.background = element_rect(
      fill = "white",
      colour = "#D1D5DB",
      linewidth = 0.35
    ),
    legend.key = element_rect(fill = "white", colour = NA),
    legend.key.height = unit(0.50, "cm"),
    legend.key.width  = unit(1.00, "cm"),
    legend.margin = margin(4, 6, 4, 6),
    legend.box.margin = margin(0, 0, 0, 0),
    plot.margin = margin(5, 8, 5, 5)
  )

# ===================================================
# PANEL A — PHQ-9 WITH SEVERITY BANDS AND 95% CIs
# ===================================================

phq_upper <- max(
  phq_summary$ci_high_plot,
  phq_summary$mean,
  na.rm = TRUE
)

phq_upper <- max(20, ceiling(phq_upper + 1))

PHQ_BANDS_PLOT <- PHQ_BANDS %>%
  mutate(
    ymax_plot = pmin(ymax, phq_upper),
    tp_anchor = factor(phq_keep[length(phq_keep)], levels = phq_keep)
  ) %>%
  filter(ymin < phq_upper)

p1 <- ggplot(
  phq_summary,
  aes(
    x = tp,
    y = mean,
    colour = condition,
    group = condition
  )
) +
  geom_rect(
    data = PHQ_BANDS_PLOT,
    aes(
      xmin = -Inf,
      xmax = Inf,
      ymin = ymin,
      ymax = ymax_plot,
      fill = fill
    ),
    inherit.aes = FALSE,
    alpha = 0.30,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  geom_hline(
    yintercept = c(4, 9, 14, 19),
    linewidth = LINE_SIZES_F5$band_line,
    colour = scales::alpha("black", 0.12)
  ) +
  geom_line(linewidth = LINE_SIZES_F5$main_line) +
  geom_point(size = POINT_SIZES_F5$main_point) +
  geom_errorbar(
    aes(
      ymin = ci_low_plot,
      ymax = ci_high_plot
    ),
    width = 0.10,
    linewidth = LINE_SIZES_F5$errorbar
  ) +
  geom_text(
    data = PHQ_BANDS_PLOT,
    aes(
      x = tp_anchor,
      y = pmin(label_y, phq_upper - 0.6),
      label = label
    ),
    inherit.aes = FALSE,
    nudge_x = 0.52,
    hjust = 0,
    size = SEVERITY_LABEL_SIZE,
    family = PALATINO_NAME,
    colour = scales::alpha("black", 0.62),
    lineheight = 0.80
  ) +
  scale_colour_manual(values = PALETTE, name = NULL) +
  guides(
    colour = guide_legend(
      override.aes = list(
        linewidth = 1.1,
        size = 2.8
      )
    )
  ) +
  scale_x_discrete(
    labels = phq_lab_map[phq_keep],
    expand = expansion(add = c(0.18, 1.15))
  ) +
  scale_y_continuous(
    limits = c(0, phq_upper),
    breaks = seq(0, phq_upper, by = 5),
    expand = expansion(mult = c(0, 0.03))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "A. PHQ-9 scores",
    x = "Study timepoint",
    y = "PHQ-9 score (0-27)"
  ) +
  paper_panel_theme_f5 +
  theme(
    legend.position = c(0.03, 0.96),
    legend.justification = c(0, 1),
    plot.margin = margin(5, 10, 5, 5),
    axis.text.x = element_text(
      lineheight = 0.88,
      margin = margin(t = 4),
      hjust = 0.5,
      vjust = 0.9
    )
  )

# ===================================================
# PANEL B — BDI-II WITH SEVERITY BANDS AND 95% CIs
# ===================================================

bdi_upper <- max(
  bdi_summary$ci_high_plot,
  bdi_summary$mean,
  na.rm = TRUE
)

bdi_upper <- max(33, ceiling(bdi_upper + 1))

BDI_BANDS_PLOT <- BDI_BANDS %>%
  mutate(
    ymax_plot = pmin(ymax, bdi_upper),
    tp_anchor = factor(bdi_keep[length(bdi_keep)], levels = bdi_keep)
  ) %>%
  filter(ymin < bdi_upper)

p2 <- ggplot(
  bdi_summary,
  aes(
    x = tp,
    y = mean,
    colour = condition,
    group = condition
  )
) +
  geom_rect(
    data = BDI_BANDS_PLOT,
    aes(
      xmin = -Inf,
      xmax = Inf,
      ymin = ymin,
      ymax = ymax_plot,
      fill = fill
    ),
    inherit.aes = FALSE,
    alpha = 0.30,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  geom_hline(
    yintercept = c(13, 19, 28),
    linewidth = LINE_SIZES_F5$band_line,
    colour = scales::alpha("black", 0.12)
  ) +
  geom_line(linewidth = LINE_SIZES_F5$main_line) +
  geom_point(size = POINT_SIZES_F5$main_point) +
  geom_errorbar(
    aes(
      ymin = ci_low_plot,
      ymax = ci_high_plot
    ),
    width = 0.08,
    linewidth = LINE_SIZES_F5$errorbar
  ) +
  geom_text(
    data = BDI_BANDS_PLOT,
    aes(
      x = tp_anchor,
      y = pmin(label_y, bdi_upper - 0.9),
      label = label
    ),
    inherit.aes = FALSE,
    nudge_x = 0.36,
    hjust = 0,
    size = SEVERITY_LABEL_SIZE,
    family = PALATINO_NAME,
    colour = scales::alpha("black", 0.62),
    lineheight = 0.80
  ) +
  scale_colour_manual(values = PALETTE, name = NULL) +
  scale_x_discrete(
    labels = bdi_lab_map[bdi_keep],
    expand = expansion(add = c(0.18, 0.70))
  ) +
  scale_y_continuous(
    limits = c(0, bdi_upper),
    breaks = seq(0, bdi_upper, by = 5),
    expand = expansion(mult = c(0, 0.03))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "B. BDI-II scores",
    x = "Study timepoint",
    y = "BDI-II score (0-63)"
  ) +
  paper_panel_theme_f5 +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 10, 5, 5),
    axis.text.x = element_text(
      lineheight = 0.88,
      margin = margin(t = 4),
      hjust = 0.5,
      vjust = 0.8
    )
  )

# ===================================================
# COMBINE
# ===================================================

combined_plot <- p1 + p2 +
  plot_layout(widths = c(1.48, 0.80))

print(combined_plot)

# ===================================================
# EXPORT FIGURE
# ===================================================

ggsave(
  filename = file.path(MANUSCRIPT_PLOT_DIR, "Figure5_WP2_Clinical_Trajectories.png"),
  plot = combined_plot,
  width = FIGURE_SIZE_F5$width,
  height = FIGURE_SIZE_F5$height,
  dpi = FIGURE_SIZE_F5$dpi,
  bg = "white"
)

ggsave(
  filename = file.path(MANUSCRIPT_PLOT_DIR, "Figure5_WP2_Clinical_Trajectories.pdf"),
  plot = combined_plot,
  width = FIGURE_SIZE_F5$width,
  height = FIGURE_SIZE_F5$height,
  dpi = FIGURE_SIZE_F5$dpi,
  bg = "white"
)

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

# ===================================================

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

#####################################################
#####################################################
#####################################################
#####################################################

#####################################################
#####################################################
#####################################################
#####################################################

# ===================================================
# FIGURE 6 — LARGE-FONT ASC-ONLY BASELINE-ALIGNED
# ARM-SPECIFIC BDI-II TRAJECTORIES
#
# Standalone drop-in:
#   - Loads assignments, pre-session 1, post-sessions 1–3,
#     and post-session 4.
#   - Computes:
#       * baseline BDI-II
#       * post-session-4 BDI-II
#       * baseline expectancy, for model adjustment only
#       * total ASC acute experience, NO VHQ
#       * low / medium / high ASC tertiles
#   - Plots two panels:
#       A. Intervention arm
#       B. Control arm
#   - Baseline-aligns trajectories within each arm:
#       aligned baseline = shared arm-specific mean baseline
#       aligned post = shared baseline - observed improvement
#   - Uses large fonts and BDI-II severity bands with score ranges.
#   - Exports plot, CSV audits, statistical model table,
#     narrative, and caption.
# ===================================================
# ===================================================
# SETUP
# ===================================================

DATA_DIR <- "C:/Users/dn284/Desktop/MRC_omni/data"
SEARCH_DIRS <- c(DATA_DIR)
MANUSCRIPT_PLOT_DIR <- file.path(DATA_DIR, "manuscript_plots")
if (!dir.exists(MANUSCRIPT_PLOT_DIR)) dir.create(MANUSCRIPT_PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

FONT_PATH <- file.path(DATA_DIR, "palatinolinotype_roman.ttf")

if (file.exists(FONT_PATH)) {
  font_add("PalatinoLinotype", regular = FONT_PATH)
  showtext_auto()
  PALATINO_NAME <- "PalatinoLinotype"
  message("Loaded font: ", PALATINO_NAME)
} else {
  PALATINO_NAME <- "serif"
  message("Palatino Linotype not found; using serif fallback.")
}

# ===================================================
# USER SETTINGS
# ===================================================

# "s1"    = session 1 ASC only
# "s1_s3" = mean total ASC across sessions 1–3
# "s1_s4" = mean total ASC across sessions 1–4, if available
ACUTE_MODE <- "s1_s3"

TERTILE_ORDER <- c("High", "Medium", "Low")
ARM_ORDER <- c("Intervention", "Control")

TERTILE_PALETTE <- c(
  "High"   = "#157A6E",
  "Medium" = "#C49A3A",
  "Low"    = "#8E949E"
)

TERTILE_LABELS <- c(
  "High"   = "High ASC",
  "Medium" = "Medium ASC",
  "Low"    = "Low ASC"
)

COLORS <- c(
  "grid"      = "grey70",
  "text"      = "#111827",
  "text_soft" = "#4B5563"
)

# ===================================================
# LARGE-FONT FIGURE STYLE
# ===================================================

FONT_SIZES_F6 <- list(
  panel_title    = 28,
  axis_title     = 24,
  axis_text      = 23,
  legend_text    = 22,
  severity_label = 18,
  annotation     = 20
)

LINE_SIZES_F6 <- list(
  main_line = 1.30,
  errorbar  = 0.85,
  axis_line = 0.45,
  grid_line = 0.34,
  band_line = 0.34
)

POINT_SIZES_F6 <- list(
  main_point = 3.45
)

FIGURE_SIZE_F6 <- list(
  width  = 7.2,
  height = 4.05,
  dpi    = 300
)

# ===================================================
# HELPERS
# ===================================================

find_csv_python_style <- function(substring, search_dirs = SEARCH_DIRS, required = TRUE) {
  hits <- character(0)
  
  for (d in search_dirs) {
    if (!dir.exists(d)) next
    
    all_files <- list.files(
      d,
      pattern = "\\.csv$",
      recursive = TRUE,
      full.names = TRUE
    )
    
    matched <- all_files[stringr::str_detect(
      basename(all_files),
      stringr::regex(substring, ignore_case = TRUE)
    )]
    
    hits <- c(hits, matched)
  }
  
  hits <- unique(hits[file.exists(hits)])
  
  if (length(hits) == 0) {
    if (required) stop("No CSV found matching substring: ", substring)
    return(NULL)
  }
  
  info <- file.info(hits)
  
  tibble(path = hits) %>%
    mutate(
      fname = basename(path),
      name_len = nchar(fname),
      mtime = info[path, "mtime", drop = TRUE]
    ) %>%
    arrange(name_len, desc(mtime)) %>%
    slice(1) %>%
    pull(path)
}

find_csv_any <- function(substrings, search_dirs = SEARCH_DIRS, required = TRUE) {
  for (s in substrings) {
    p <- find_csv_python_style(s, search_dirs = search_dirs, required = FALSE)
    if (!is.null(p)) return(p)
  }
  
  if (required) {
    stop("No CSV found matching any of: ", paste(substrings, collapse = ", "))
  }
  
  NULL
}

read_qualtrics_real <- function(path, skiprows = NULL) {
  df <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    skip = ifelse(is.null(skiprows), 0, skiprows),
    show_col_types = FALSE
  )
  
  names(df) <- names(df) %>%
    stringr::str_trim() %>%
    stringr::str_to_lower()
  
  if ("responseid" %in% names(df)) {
    df <- df %>%
      filter(stringr::str_starts(as.character(responseid), "R_"))
  }
  
  if (!"part_id" %in% names(df)) {
    for (alt in c("participantid", "participant_id", "externalreference", "responseid", "id")) {
      if (alt %in% names(df)) {
        names(df)[names(df) == alt] <- "part_id"
        break
      }
    }
  }
  
  df
}

clean_id <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    dplyr::na_if("") %>%
    dplyr::na_if("nan") %>%
    dplyr::na_if("none") %>%
    dplyr::na_if('{"importid":"part_id"}') %>%
    stringr::str_extract("\\d{3,6}")
}

qnum <- function(x) {
  if (is.na(x)) return(NA_real_)
  
  s <- trimws(as.character(x))
  
  if (s == "" || tolower(s) %in% c("nan", "none", "na")) return(NA_real_)
  
  direct <- suppressWarnings(as.numeric(s))
  if (!is.na(direct)) return(direct)
  
  m <- stringr::str_match(s, "^\\s*([-+]?[0-9]+\\.?[0-9]*)")
  if (!is.na(m[1, 2])) return(as.numeric(m[1, 2]))
  
  NA_real_
}

rowmean_py <- function(df_cols) {
  if (ncol(df_cols) == 0) return(rep(NA_real_, nrow(df_cols)))
  
  out <- rowMeans(df_cols, na.rm = TRUE)
  out[rowSums(!is.na(df_cols)) == 0] <- NA_real_
  out
}

rowsum_py <- function(df_cols) {
  if (ncol(df_cols) == 0) return(rep(NA_real_, nrow(df_cols)))
  
  out <- rowSums(df_cols, na.rm = TRUE)
  out[rowSums(!is.na(df_cols)) == 0] <- NA_real_
  out
}

zscore <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

sum_scale_py <- function(df, prefix, n_items) {
  summary_col <- paste0(prefix, "_sum")
  
  if (summary_col %in% names(df)) {
    out <- vapply(df[[summary_col]], qnum, numeric(1))
    out[is.nan(out)] <- NA_real_
    return(out)
  }
  
  cols <- paste0(prefix, "_", seq_len(n_items))
  cols <- cols[cols %in% names(df)]
  
  if (length(cols) == 0) return(rep(NA_real_, nrow(df)))
  
  tmp <- df[, cols, drop = FALSE] %>%
    mutate(across(everything(), ~ vapply(.x, qnum, numeric(1))))
  
  rowsum_py(tmp)
}

find_first_col <- function(df, patterns) {
  nms <- names(df)
  
  for (p in patterns) {
    hit <- nms[str_detect(nms, regex(p, ignore_case = TRUE))]
    if (length(hit) > 0) return(hit[1])
  }
  
  NA_character_
}

normalise_condition <- function(x) {
  x <- str_to_lower(str_trim(as.character(x)))
  
  case_when(
    str_detect(x, "inter") ~ "Intervention",
    str_detect(x, "control") ~ "Control",
    TRUE ~ NA_character_
  )
}

ci95_summary <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  
  n <- length(x)
  m <- if (n > 0) mean(x) else NA_real_
  s <- if (n > 1) sd(x) else NA_real_
  se <- if (n > 1) s / sqrt(n) else NA_real_
  
  if (n > 1 && !is.na(se)) {
    crit <- qt(0.975, df = n - 1)
    ci_low <- m - crit * se
    ci_high <- m + crit * se
  } else {
    ci_low <- NA_real_
    ci_high <- NA_real_
  }
  
  tibble(
    n = n,
    mean = m,
    sd = s,
    se = se,
    ci_low = ci_low,
    ci_high = ci_high
  )
}

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

fmt_p <- function(p) {
  case_when(
    is.na(p) ~ "NA",
    p < .001 ~ "< .001",
    TRUE ~ sprintf("%.3f", p)
  )
}

fmt_ci <- function(lo, hi, digits = 2) {
  paste0("[", fmt_num(lo, digits), ", ", fmt_num(hi, digits), "]")
}

make_tertiles <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  q <- quantile(x, probs = c(1 / 3, 2 / 3), na.rm = TRUE, type = 7)
  
  case_when(
    is.na(x) ~ NA_character_,
    x <= q[1] ~ "Low",
    x <= q[2] ~ "Medium",
    TRUE ~ "High"
  )
}

collapse_formula <- function(form) {
  paste(deparse(form), collapse = " ")
}

# ===================================================
# LOCATE FILES
# ===================================================

ASSIGN_PATH <- find_csv_any(c(
  "wp2_assignments",
  "assignments"
))

PRE1_PATH <- find_csv_any(c(
  "wp2_pre_session_1",
  "pre_session_1"
))

POST13_PATH <- find_csv_any(c(
  "wp2_post_sessions_1-3",
  "post_sessions_1-3",
  "post_sessions_1_3",
  "post_sessions_1"
))

POST4_PATH <- find_csv_any(c(
  "wp2_post_session_4",
  "post_session_4"
))

message("[load] assignments:       ", basename(ASSIGN_PATH))
message("[load] pre_session_1:     ", basename(PRE1_PATH))
message("[load] post_sessions_1-3: ", basename(POST13_PATH))
message("[load] post_session_4:    ", basename(POST4_PATH))

# ===================================================
# LOAD DATA
# ===================================================

assign <- read_qualtrics_real(ASSIGN_PATH) %>%
  mutate(part_id = clean_id(part_id))

pre1 <- read_qualtrics_real(PRE1_PATH) %>%
  mutate(part_id = clean_id(part_id))

post13 <- read_qualtrics_real(POST13_PATH) %>%
  mutate(part_id = clean_id(part_id))

post4 <- read_qualtrics_real(POST4_PATH) %>%
  mutate(part_id = clean_id(part_id))

# ===================================================
# ARM
# ===================================================

condition_col <- find_first_col(assign, c(
  "^condition$",
  "^arm$",
  "allocation",
  "assigned_condition",
  "randomised_condition",
  "randomized_condition",
  "treatment"
))

if (is.na(condition_col)) {
  stop("Could not find condition / arm column in assignments file.")
}

assign_clean <- assign %>%
  transmute(
    part_id,
    condition = normalise_condition(.data[[condition_col]])
  ) %>%
  distinct(part_id, .keep_all = TRUE)

# ===================================================
# EXPECTANCY
# Only used for adjustment in continuous models.
# It is NOT plotted.
# ===================================================

exp_cols <- paste0("treat_expct_", 1:15)
exp_cols <- exp_cols[exp_cols %in% names(pre1)]

if (length(exp_cols) == 0) {
  warning("No treat_expct_* expectancy columns found. Models will omit expectancy.")
  pre1$expect_sum <- NA_real_
  pre1$expect_mean <- NA_real_
  pre1$expect_z <- NA_real_
} else {
  pre1 <- pre1 %>%
    mutate(across(all_of(exp_cols), ~ vapply(.x, qnum, numeric(1))))
  
  reverse_cols <- paste0("treat_expct_", 7:11)
  reverse_cols <- reverse_cols[reverse_cols %in% names(pre1)]
  
  pre1 <- pre1 %>%
    mutate(across(all_of(reverse_cols), ~ 10 - .x))
  
  exp_mat <- pre1 %>%
    select(all_of(exp_cols))
  
  pre1$expect_sum <- rowsum_py(exp_mat)
  pre1$expect_mean <- rowmean_py(exp_mat)
  pre1$expect_z <- zscore(pre1$expect_sum)
}

# ===================================================
# BDI-II
# ===================================================

pre1$bdi_pre <- sum_scale_py(pre1, "bdi", 21)
post4$bdi_post <- sum_scale_py(post4, "bdi", 21)

# ===================================================
# TOTAL ASC ACUTE EXPERIENCE — NO VHQ
# ===================================================

numericise_asc <- function(df) {
  asc_cols <- names(df)[str_detect(names(df), "^asc_[0-9]+(_[0-9]+)?$")]
  
  if (length(asc_cols) == 0) {
    stop("No ASC columns found. Expected columns like asc_1, asc_2, asc_1_1, asc_1_2, etc.")
  }
  
  df %>%
    mutate(across(all_of(asc_cols), ~ vapply(.x, qnum, numeric(1))))
}

extract_post13_asc_totals <- function(post13) {
  post13 <- numericise_asc(post13)
  
  session_col <- find_first_col(post13, c(
    "^session_n$",
    "^session$",
    "session_number",
    "sessionnum"
  ))
  
  asc_cols <- names(post13)[str_detect(names(post13), "^asc_[0-9]+(_[0-9]+)?$")]
  
  if (!is.na(session_col)) {
    return(
      post13 %>%
        mutate(
          session_num = vapply(.data[[session_col]], qnum, numeric(1)),
          asc_total = rowsum_py(select(., all_of(asc_cols)))
        ) %>%
        filter(session_num %in% c(1, 2, 3)) %>%
        group_by(part_id, session_num) %>%
        summarise(
          asc_total = mean(asc_total, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          asc_total = ifelse(is.nan(asc_total), NA_real_, asc_total)
        ) %>%
        pivot_wider(
          names_from = session_num,
          values_from = asc_total,
          names_prefix = "asc_total_s"
        )
    )
  }
  
  out <- post13 %>%
    select(part_id) %>%
    distinct()
  
  for (s in 1:3) {
    s_cols <- names(post13)[str_detect(names(post13), paste0("^asc_[0-9]+_", s, "$"))]
    
    if (length(s_cols) > 0) {
      tmp <- post13 %>%
        transmute(
          part_id,
          !!paste0("asc_total_s", s) := rowsum_py(select(., all_of(s_cols)))
        ) %>%
        group_by(part_id) %>%
        summarise(
          !!paste0("asc_total_s", s) := mean(.data[[paste0("asc_total_s", s)]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          !!paste0("asc_total_s", s) := ifelse(
            is.nan(.data[[paste0("asc_total_s", s)]]),
            NA_real_,
            .data[[paste0("asc_total_s", s)]]
          )
        )
      
      out <- out %>%
        left_join(tmp, by = "part_id")
    }
  }
  
  out
}

extract_post4_asc_total <- function(post4) {
  post4 <- numericise_asc(post4)
  
  s4_cols <- names(post4)[str_detect(names(post4), "^asc_[0-9]+_4$")]
  
  if (length(s4_cols) == 0) {
    s4_cols <- names(post4)[str_detect(names(post4), "^asc_[0-9]+$")]
  }
  
  post4 %>%
    transmute(
      part_id,
      asc_total_s4 = rowsum_py(select(., all_of(s4_cols)))
    ) %>%
    group_by(part_id) %>%
    summarise(
      asc_total_s4 = mean(asc_total_s4, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      asc_total_s4 = ifelse(is.nan(asc_total_s4), NA_real_, asc_total_s4)
    )
}

asc13 <- extract_post13_asc_totals(post13)
asc4 <- extract_post4_asc_total(post4)

# ===================================================
# PARTICIPANT-LEVEL TABLE
# ===================================================

pt <- pre1 %>%
  select(
    part_id,
    bdi_pre,
    expect_sum,
    expect_mean,
    expect_z
  ) %>%
  inner_join(
    post4 %>% select(part_id, bdi_post),
    by = "part_id"
  ) %>%
  left_join(asc13, by = "part_id") %>%
  left_join(asc4, by = "part_id") %>%
  left_join(assign_clean, by = "part_id") %>%
  mutate(
    condition = factor(condition, levels = ARM_ORDER),
    bdi_improve = bdi_pre - bdi_post,
    asc_total_s1_s3 = rowmean_py(select(., any_of(c("asc_total_s1", "asc_total_s2", "asc_total_s3")))),
    asc_total_s1_s4 = rowmean_py(select(., any_of(c("asc_total_s1", "asc_total_s2", "asc_total_s3", "asc_total_s4")))),
    asc_total_primary = case_when(
      ACUTE_MODE == "s1" ~ asc_total_s1,
      ACUTE_MODE == "s1_s3" ~ asc_total_s1_s3,
      ACUTE_MODE == "s1_s4" ~ asc_total_s1_s4,
      TRUE ~ asc_total_s1_s3
    ),
    asc_total_z = zscore(asc_total_primary),
    asc_tertile = factor(make_tertiles(asc_total_z), levels = TERTILE_ORDER)
  )

cat("\n============================================================\n")
cat("ASC-ONLY FIGURE 6 DATA AUDIT\n")
cat("============================================================\n")
cat("Participant rows in pt:", nrow(pt), "\n\n")

cat("Arm counts:\n")
print(pt %>% count(condition))

cat("\nTotal ASC tertile counts by arm:\n")
print(pt %>% count(condition, asc_tertile))

cat("\nASC availability:\n")
print(
  pt %>%
    summarise(
      n_s1 = sum(!is.na(asc_total_s1)),
      n_s2 = sum(!is.na(asc_total_s2)),
      n_s3 = sum(!is.na(asc_total_s3)),
      n_s4 = sum(!is.na(asc_total_s4)),
      n_primary = sum(!is.na(asc_total_primary))
    )
)

cat("============================================================\n\n")

# ===================================================
# BASELINE-ALIGNED ASC-ONLY LONG DATA
# ===================================================

plot_participant_long <- pt %>%
  transmute(
    part_id,
    condition,
    tertile = asc_tertile,
    asc_total_z,
    expect_z,
    bdi_pre,
    bdi_post,
    bdi_improve
  ) %>%
  drop_na(condition, tertile, bdi_pre, bdi_post) %>%
  mutate(
    tertile = factor(tertile, levels = TERTILE_ORDER)
  )

plot_participant_aligned <- plot_participant_long %>%
  group_by(condition) %>%
  mutate(
    shared_baseline = mean(bdi_pre, na.rm = TRUE),
    bdi_aligned_pre = shared_baseline,
    bdi_aligned_post = shared_baseline - bdi_improve
  ) %>%
  ungroup()

plot_long <- plot_participant_aligned %>%
  select(
    part_id,
    condition,
    tertile,
    bdi_aligned_pre,
    bdi_aligned_post
  ) %>%
  pivot_longer(
    cols = c(bdi_aligned_pre, bdi_aligned_post),
    names_to = "timepoint",
    values_to = "bdi_score"
  ) %>%
  mutate(
    timepoint = recode(
      timepoint,
      "bdi_aligned_pre" = "Baseline-aligned",
      "bdi_aligned_post" = "Post-treatment"
    ),
    timepoint = factor(timepoint, levels = c("Baseline-aligned", "Post-treatment")),
    x_plot = as.numeric(timepoint)
  )

plot_summary <- plot_long %>%
  group_by(condition, tertile, timepoint, x_plot) %>%
  summarise(
    ci95_summary(bdi_score),
    .groups = "drop"
  ) %>%
  mutate(
    ci_low_plot = ifelse(is.na(ci_low), mean, pmax(ci_low, 0)),
    ci_high_plot = ifelse(is.na(ci_high), mean, ci_high),
    group_id = interaction(tertile, condition, drop = TRUE)
  )

improvement_summary <- plot_participant_long %>%
  group_by(condition, tertile) %>%
  summarise(
    ci95_summary(bdi_improve),
    .groups = "drop"
  ) %>%
  mutate(
    improvement_direction = ifelse(mean > 0, "improvement", "worsening")
  )

# ===================================================
# EXPLORATORY MODELS
# ===================================================

extract_coef <- function(fit, term, label, n) {
  coef_mat <- summary(fit)$coefficients
  ci <- suppressMessages(confint(fit))
  
  if (!term %in% rownames(coef_mat)) {
    return(tibble(
      model = label,
      term = term,
      n = n,
      estimate = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      p_value = NA_real_
    ))
  }
  
  tibble(
    model = label,
    term = term,
    n = n,
    estimate = coef_mat[term, "Estimate"],
    ci_low = ci[term, 1],
    ci_high = ci[term, 2],
    p_value = coef_mat[term, "Pr(>|t|)"]
  )
}

run_within_arm_model <- function(df, arm_name) {
  d <- df %>%
    filter(condition == arm_name) %>%
    select(part_id, bdi_post, bdi_pre, asc_total_z, expect_z) %>%
    drop_na(bdi_post, bdi_pre, asc_total_z)
  
  include_expectancy <- sum(!is.na(d$expect_z)) >= 10 && sd(d$expect_z, na.rm = TRUE) > 0
  
  if (include_expectancy) {
    d <- d %>% drop_na(expect_z)
    form <- bdi_post ~ bdi_pre + asc_total_z + expect_z
  } else {
    form <- bdi_post ~ bdi_pre + asc_total_z
  }
  
  if (nrow(d) < 15) {
    return(tibble(
      model = paste0(arm_name, " within-arm continuous model"),
      term = c("asc_total_z", if (include_expectancy) "expect_z" else NULL),
      n = nrow(d),
      estimate = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      p_value = NA_real_,
      model_formula = NA_character_
    ))
  }
  
  fit <- lm(form, data = d)
  
  bind_rows(
    extract_coef(fit, "asc_total_z", paste0(arm_name, " within-arm continuous model"), nrow(d)),
    if (include_expectancy) extract_coef(fit, "expect_z", paste0(arm_name, " within-arm continuous model"), nrow(d)) else NULL
  ) %>%
    mutate(model_formula = collapse_formula(form))
}

within_arm_models <- bind_rows(
  run_within_arm_model(pt, "Intervention"),
  run_within_arm_model(pt, "Control")
)

full_model_df <- pt %>%
  select(part_id, condition, bdi_post, bdi_pre, asc_total_z, expect_z) %>%
  drop_na(bdi_post, bdi_pre, asc_total_z, condition) %>%
  mutate(condition = factor(condition, levels = ARM_ORDER))

if (sum(!is.na(full_model_df$expect_z)) >= 25 && sd(full_model_df$expect_z, na.rm = TRUE) > 0) {
  full_model_df <- full_model_df %>% drop_na(expect_z)
  full_form <- bdi_post ~ bdi_pre + condition * asc_total_z + expect_z
} else {
  full_form <- bdi_post ~ bdi_pre + condition * asc_total_z
}

extract_full_terms <- function(fit, n, form) {
  coef_mat <- summary(fit)$coefficients
  ci <- suppressMessages(confint(fit))
  
  tibble(
    model = "Full continuous interaction model",
    term = rownames(coef_mat),
    n = n,
    estimate = coef_mat[, "Estimate"],
    ci_low = ci[, 1],
    ci_high = ci[, 2],
    p_value = coef_mat[, "Pr(>|t|)"],
    model_formula = collapse_formula(form)
  )
}

if (nrow(full_model_df) >= 25 && n_distinct(full_model_df$condition) == 2) {
  full_fit <- lm(full_form, data = full_model_df)
  full_model_terms <- extract_full_terms(full_fit, nrow(full_model_df), full_form)
} else {
  full_model_terms <- tibble(
    model = "Full continuous interaction model",
    term = NA_character_,
    n = nrow(full_model_df),
    estimate = NA_real_,
    ci_low = NA_real_,
    ci_high = NA_real_,
    p_value = NA_real_,
    model_formula = NA_character_
  )
}

model_export <- bind_rows(
  within_arm_models,
  full_model_terms
) %>%
  mutate(
    estimate_round = round(estimate, 3),
    ci_low_round = round(ci_low, 3),
    ci_high_round = round(ci_high, 3),
    p_value_formatted = fmt_p(p_value)
  )

run_tertile_ancova <- function(df, arm_name) {
  d <- df %>%
    filter(condition == arm_name) %>%
    select(part_id, bdi_post, bdi_pre, tertile = asc_tertile) %>%
    drop_na() %>%
    mutate(tertile = factor(tertile, levels = TERTILE_ORDER))
  
  if (nrow(d) < 15 || n_distinct(d$tertile) < 2) {
    return(tibble(
      condition = arm_name,
      predictor = "Total ASC",
      n = nrow(d),
      test = "tertile ANCOVA",
      f_value = NA_real_,
      p_value = NA_real_
    ))
  }
  
  fit <- lm(bdi_post ~ bdi_pre + tertile, data = d)
  a <- drop1(fit, test = "F")
  
  if (!"tertile" %in% rownames(a)) {
    return(tibble(
      condition = arm_name,
      predictor = "Total ASC",
      n = nrow(d),
      test = "tertile ANCOVA",
      f_value = NA_real_,
      p_value = NA_real_
    ))
  }
  
  tibble(
    condition = arm_name,
    predictor = "Total ASC",
    n = nrow(d),
    test = "tertile ANCOVA",
    f_value = a["tertile", "F value"],
    p_value = a["tertile", "Pr(>F)"]
  )
}

tertile_model_export <- bind_rows(
  run_tertile_ancova(pt, "Intervention"),
  run_tertile_ancova(pt, "Control")
) %>%
  mutate(
    f_value_round = round(f_value, 3),
    p_value_formatted = fmt_p(p_value)
  )

# ===================================================
# BDI-II SEVERITY BANDS WITH SCORE RANGE LABELS
# ===================================================

ymax_data <- max(plot_summary$ci_high_plot, plot_summary$mean, na.rm = TRUE)

BDI_YMIN <- 0
BDI_YMAX <- max(33, ceiling(ymax_data + 2))

BDI_BANDS <- tibble::tribble(
  ~ymin, ~ymax, ~fill,      ~label_y, ~label,
  0,     13,    "#DCEAF7",  6.5,      "0–13\nMinimal",
  14,    19,    "#DDF2E3",  16.5,     "14–19\nMild",
  20,    28,    "#F6E6CF",  24.0,     "20–28\nModerate",
  29,    63,    "#F2D7D7",  31.8,     "29+\nSevere"
) %>%
  mutate(
    ymin_plot = pmax(ymin, BDI_YMIN),
    ymax_plot = pmin(ymax, BDI_YMAX),
    label_y_plot = pmin(label_y, BDI_YMAX - 0.9),
    label_x = 2.18
  ) %>%
  filter(ymax_plot > BDI_YMIN, ymin_plot < BDI_YMAX)

SEVERITY_LABEL_SIZE <- FONT_SIZES_F6$severity_label / .pt

# ===================================================
# PANEL LABELS
# ===================================================

plot_summary_panel <- plot_summary %>%
  mutate(
    condition_panel = case_when(
      condition == "Intervention" ~ "A. Intervention arm",
      condition == "Control" ~ "B. Control arm",
      TRUE ~ as.character(condition)
    ),
    condition_panel = factor(
      condition_panel,
      levels = c("A. Intervention arm", "B. Control arm")
    ),
    tertile = factor(tertile, levels = TERTILE_ORDER)
  )

BDI_BANDS_PANEL <- bind_rows(
  BDI_BANDS %>%
    mutate(condition_panel = factor("A. Intervention arm", levels = levels(plot_summary_panel$condition_panel))),
  BDI_BANDS %>%
    mutate(condition_panel = factor("B. Control arm", levels = levels(plot_summary_panel$condition_panel)))
)

# ===================================================
# LARGE-FONT THEME
# ===================================================

paper_panel_theme_f6 <- theme_minimal(base_family = PALATINO_NAME) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      colour = scales::alpha(COLORS["grid"], 0.22),
      linewidth = LINE_SIZES_F6$grid_line
    ),
    axis.line.x = element_line(
      colour = "grey30",
      linewidth = LINE_SIZES_F6$axis_line
    ),
    axis.line.y = element_line(
      colour = "grey30",
      linewidth = LINE_SIZES_F6$axis_line
    ),
    axis.ticks = element_line(
      colour = "grey30",
      linewidth = LINE_SIZES_F6$axis_line
    ),
    strip.text = element_text(
      family = PALATINO_NAME,
      face = "plain",
      size = FONT_SIZES_F6$panel_title,
      hjust = 0,
      lineheight = 0.92,
      colour = COLORS["text"],
      margin = margin(b = 7)
    ),
    axis.title = element_text(
      family = PALATINO_NAME,
      size = FONT_SIZES_F6$axis_title,
      colour = COLORS["text"]
    ),
    axis.text = element_text(
      family = PALATINO_NAME,
      size = FONT_SIZES_F6$axis_text,
      colour = "grey25"
    ),
    axis.text.x = element_text(
      margin = margin(t = 5),
      hjust = 0.5,
      vjust = 0.5
    ),
    legend.title = element_blank(),
    legend.text = element_text(
      family = PALATINO_NAME,
      size = FONT_SIZES_F6$legend_text,
      colour = COLORS["text"]
    ),
    legend.background = element_rect(
      fill = "white",
      colour = "#D1D5DB",
      linewidth = 0.35
    ),
    legend.key = element_rect(fill = "white", colour = NA),
    legend.key.height = unit(0.50, "cm"),
    legend.key.width  = unit(1.05, "cm"),
    legend.margin = margin(4, 7, 4, 7),
    legend.position = c(0.03, 0.04),
    legend.justification = c(0, 0),
    plot.margin = margin(6, 14, 6, 6)
  )

# ===================================================
# PLOT
# ===================================================

p <- ggplot(
  plot_summary_panel,
  aes(
    x = x_plot,
    y = mean,
    colour = tertile,
    group = group_id
  )
) +
  geom_rect(
    data = BDI_BANDS_PANEL,
    aes(
      xmin = -Inf,
      xmax = Inf,
      ymin = ymin_plot,
      ymax = ymax_plot,
      fill = fill
    ),
    inherit.aes = FALSE,
    alpha = 0.30,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  geom_hline(
    yintercept = c(13, 19, 28),
    linewidth = LINE_SIZES_F6$band_line,
    colour = scales::alpha("black", 0.12)
  ) +
  geom_line(
    linewidth = LINE_SIZES_F6$main_line,
    alpha = 0.96
  ) +
  geom_point(
    size = POINT_SIZES_F6$main_point,
    alpha = 0.96
  ) +
  geom_errorbar(
    aes(
      ymin = ci_low_plot,
      ymax = ci_high_plot
    ),
    width = 0.075,
    linewidth = LINE_SIZES_F6$errorbar,
    alpha = 0.92
  ) +
  geom_text(
    data = BDI_BANDS_PANEL,
    aes(
      x = label_x,
      y = label_y_plot,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 0,
    size = SEVERITY_LABEL_SIZE,
    family = PALATINO_NAME,
    colour = scales::alpha("black", 0.58),
    lineheight = 0.80
  ) +
  facet_wrap(~ condition_panel, nrow = 1) +
  scale_colour_manual(
    values = TERTILE_PALETTE,
    breaks = TERTILE_ORDER,
    labels = TERTILE_LABELS,
    name = NULL
  ) +
  scale_x_continuous(
    limits = c(0.83, 2.43),
    breaks = c(1, 2),
    labels = c("Baseline-\naligned", "Post-\ntreatment"),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    limits = c(BDI_YMIN, BDI_YMAX),
    breaks = sort(unique(c(0, 13, 19, 28, BDI_YMAX))),
    labels = function(x) {
      case_when(
        x == 0 ~ "0",
        x == 13 ~ "13",
        x == 19 ~ "19",
        x == 28 ~ "28",
        TRUE ~ as.character(x)
      )
    },
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Study timepoint",
    y = "Arm-specific baseline-aligned\nBDI-II score (0–63)"
  ) +
  paper_panel_theme_f6 +
  guides(
    colour = guide_legend(
      nrow = 1,
      override.aes = list(
        linewidth = 1.2,
        size = 3.0
      )
    )
  )

print(p)

# ===================================================
# EXPORT LARGE-FONT FIGURE
# ===================================================

ggsave(
  filename = file.path(MANUSCRIPT_PLOT_DIR, "Figure6_ASCOnly_BaselineAligned_LargeFont_ByArm.png"),
  plot = p,
  width = FIGURE_SIZE_F6$width,
  height = FIGURE_SIZE_F6$height,
  dpi = FIGURE_SIZE_F6$dpi,
  bg = "white"
)

ggsave(
  filename = file.path(MANUSCRIPT_PLOT_DIR, "Figure6_ASCOnly_BaselineAligned_LargeFont_ByArm.pdf"),
  plot = p,
  width = FIGURE_SIZE_F6$width,
  height = FIGURE_SIZE_F6$height,
  dpi = FIGURE_SIZE_F6$dpi,
  bg = "white"
)


# ============================================================
# END MANUSCRIPT FIGURE CODE
# ============================================================
