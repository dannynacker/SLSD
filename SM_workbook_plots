# ============================================================
# MRC SUPPLEMENTARY WORKBOOK + SM PLOTS CLEANED STAGING SCRIPT
# ============================================================
#
# Generated from the user-curated supplementary material code blocks.
#
# Scope:
#   - SM workbook source tables / section workbooks
#   - SM-only plots and table PNG/PDF exports
#   - Not intended for the six main manuscript figures
#   - Narrative outputs are retained only where the original block requires
#     them for audit/provenance; they can be ignored for plotting/workbook review.
#
# How to use:
#   1. Put all raw CSV/XLSX/font/source files in DATA_DIR.
#   2. Optional: override the data directory before running:
#        Sys.setenv(MRC_DATA_DIR = "C:/Users/dn284/Desktop/MRC_omni/data")
#   3. Run this file from a fresh R session.
#
# Output convention:
#   The original section-specific output names are preserved for traceability.
#   A final optional consolidated workbook builder is appended at the end.
#
# ============================================================

DATA_DIR <- Sys.getenv("MRC_DATA_DIR", unset = "C:/Users/dn284/Desktop/MRC_omni/data")
SEARCH_DIRS <- c(DATA_DIR)

MRC_PUBLIC_OUT <- file.path(DATA_DIR, "MRC_public_outputs")
SM_WORKBOOK_OUT <- file.path(MRC_PUBLIC_OUT, "SM_workbook")
SM_PLOTS_OUT <- file.path(MRC_PUBLIC_OUT, "SM_plots")
SM_AUDIT_OUT <- file.path(MRC_PUBLIC_OUT, "audit_SM_workbook")

dir.create(MRC_PUBLIC_OUT, recursive = TRUE, showWarnings = FALSE)
dir.create(SM_WORKBOOK_OUT, recursive = TRUE, showWarnings = FALSE)
dir.create(SM_PLOTS_OUT, recursive = TRUE, showWarnings = FALSE)
dir.create(SM_AUDIT_OUT, recursive = TRUE, showWarnings = FALSE)

# Packages used across the curated blocks.
# The original section-level library() calls are retained below because they make
# each block easier to lift out independently.
required_pkgs <- c(
  "tidyverse", "lubridate", "gt", "webshot2", "gridExtra", "grid",
  "showtext", "sysfonts", "ragg", "writexl", "openxlsx",
  "readr", "stringr", "patchwork", "scales", "ggplot2",
  "glue", "reticulate", "jsonlite", "gtable", "magick",
  "wordcloud", "tm", "RColorBrewer"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  message(
    "Missing packages detected. Install before running if needed: ",
    paste(missing_pkgs, collapse = ", ")
  )
}

# Small global helper used by the appended final workbook builder.
mrc_as_sheet_df <- function(x, source_name = NA_character_) {
  if (is.null(x)) {
    return(tibble::tibble())
  }
  if (inherits(x, "data.frame")) {
    out <- tibble::as_tibble(x)
  } else if (is.atomic(x) || is.character(x)) {
    out <- tibble::tibble(value = as.character(x))
  } else if (is.list(x)) {
    out <- tryCatch(
      tibble::as_tibble(x),
      error = function(e) tibble::tibble(value = capture.output(str(x)))
    )
  } else {
    out <- tibble::tibble(value = capture.output(print(x)))
  }
  if (!is.na(source_name)) {
    out <- dplyr::mutate(out, .source_object = source_name, .before = 1)
  }
  out
}

# ============================================================
# BEGIN USER-CURATED SUPPLEMENTARY CODE BLOCKS
# ============================================================

# ------------------------------------------------------------
# SECTION: wp1 demographics
# ------------------------------------------------------------

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

# ===================================================
# WP1 DEMOGRAPHICS TABLES
# FINAL RECONCILED VERSION WITH BEST SIGN-UP + BEST TESTING ROWS
# ===================================================
# Manuscript denominators:
#   Screened = 197
#   Tested   = 36
#   Analysed = 31
#
# Outputs:
#   WP1_SUPERVISOR_RECONCILED_Demographics_Table.html/png/pdf
#   WP1_SUPERVISOR_RECONCILED_MissingDenominators_Table.html/png/pdf
#   WP1_SUPERVISOR_RECONCILED_Demographics_StageSpecific.csv
#   WP1_SUPERVISOR_RECONCILED_MissingDenominators.csv
#   WP1_SUPERVISOR_RECONCILED_ID_Audit.csv
#   WP1_SUPERVISOR_RECONCILED_Denominator_Audit.csv
#   WP1_SUPERVISOR_RECONCILED_Calendar_Audit.csv
#   WP1_SUPERVISOR_RECONCILED_Demographics_ManuscriptText.txt
#
# Key logic:
#   - Screened / passed screening are derived from raw WP1 Sign-up IDs.
#   - Testing Start and Session Feedback IDs are reconciled manually.
#   - Analysed = corrected Testing Start IDs ∩ corrected valid Session Feedback IDs.
#   - Duplicate sign-up rows are resolved by keeping the most complete demographic row.
#   - Duplicate Testing Start rows are resolved by keeping the most complete clinical row.
# ===================================================

# install.packages(c("tidyverse", "lubridate", "gt", "webshot2"))

library(tidyverse)
library(lubridate)
library(gt)

# ===================================================
# 1. SETTINGS
# ===================================================

DATA_DIR <- Sys.getenv("MRC_DATA_DIR", unset = "C:/Users/dn284/Desktop/MRC_omni/data")
SEARCH_DIRS <- c(DATA_DIR)

OUT_PREFIX <- "WP1_SUPERVISOR_RECONCILED"

EXPECTED_SCREENED_N <- 197
EXPECTED_TESTED_N   <- 36
EXPECTED_ANALYSED_N <- 31

# Corrections are applied ONLY to Testing Start and Session Feedback.
# Do not apply these to Sign-up, because screened n should remain raw.
ID_CORRECTIONS_TEST_FB <- tribble(
  ~bad_id, ~correct_id, ~reason,
  "476", "467", "Testing/feedback ID appears mistyped as 476; calendar/sign-up ID is 467",
  "555", "551", "Session feedback ID appears mistyped as 555; Testing Start/calendar ID is 551"
)

# Usually should remain empty.
MANUAL_ANALYSED_EXCLUDE_IDS <- c()

OUT_SUMMARY_CSV <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_Demographics_StageSpecific.csv"))
OUT_MISSING_CSV <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_MissingDenominators.csv"))
OUT_ID_AUDIT_CSV <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_ID_Audit.csv"))
OUT_DENOM_AUDIT_CSV <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_Denominator_Audit.csv"))
OUT_CALENDAR_AUDIT_CSV <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_Calendar_Audit.csv"))
OUT_TEXT <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_Demographics_ManuscriptText.txt"))

OUT_TABLE_HTML <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_Demographics_Table.html"))
OUT_TABLE_PNG  <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_Demographics_Table.png"))
OUT_TABLE_PDF  <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_Demographics_Table.pdf"))

OUT_MISSING_HTML <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_MissingDenominators_Table.html"))
OUT_MISSING_PNG  <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_MissingDenominators_Table.png"))
OUT_MISSING_PDF  <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_MissingDenominators_Table.pdf"))

# ===================================================
# 2. HELPERS
# ===================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

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
  
  hits[order(file.info(hits)$mtime, decreasing = TRUE)][1]
}

read_qualtrics_real <- function(path) {
  df <- readr::read_csv(
    path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )
  
  names(df) <- str_trim(names(df))
  
  if ("ResponseId" %in% names(df)) {
    df <- df %>%
      filter(str_starts(as.character(ResponseId), "R_"))
  }
  
  df <- df %>%
    filter(
      !if_any(
        everything(),
        ~ str_detect(coalesce(as.character(.x), ""), fixed("ImportId"))
      )
    )
  
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
    str_extract("\\d{1,6}")
}

apply_id_corrections <- function(x, corrections = ID_CORRECTIONS_TEST_FB) {
  x <- clean_id(x)
  
  if (nrow(corrections) == 0) return(x)
  
  corrections <- corrections %>%
    mutate(
      bad_id = clean_id(bad_id),
      correct_id = clean_id(correct_id)
    ) %>%
    filter(!is.na(bad_id), !is.na(correct_id))
  
  correction_map <- setNames(corrections$correct_id, corrections$bad_id)
  
  out <- ifelse(
    !is.na(x) & x %in% names(correction_map),
    correction_map[x],
    x
  )
  
  as.character(out)
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
    arrange(desc(.dt)) %>%
    distinct(.data[[pid_col]], .keep_all = TRUE) %>%
    select(-.dt)
}

best_signup_per_pid <- function(df, pid_col = "part_id") {
  
  if (!pid_col %in% names(df)) {
    stop("PID column not found: ", pid_col)
  }
  
  demo_cols <- c(
    "incl_dem_age",
    "incl_dem_sex",
    "incl_dem_gender",
    "incl_dem_med",
    paste0("phq9_", 1:9),
    "phq9_sum"
  )
  
  demo_cols <- demo_cols[demo_cols %in% names(df)]
  
  dt_col <- c("RecordedDate", "EndDate", "StartDate")
  dt_col <- dt_col[dt_col %in% names(df)][1]
  
  df %>%
    filter(!is.na(.data[[pid_col]])) %>%
    mutate(
      .row_order = row_number(),
      .completeness = if (length(demo_cols) > 0) {
        rowSums(
          across(
            all_of(demo_cols),
            ~ !is.na(.x) & str_squish(as.character(.x)) != ""
          ),
          na.rm = TRUE
        )
      } else {
        0
      },
      .dt = if (!is.na(dt_col)) {
        suppressWarnings(
          parse_date_time(
            .data[[dt_col]],
            orders = c(
              "ymd HMS", "ymd HM",
              "dmy HMS", "dmy HM",
              "mdy HMS", "mdy HM"
            )
          )
        )
      } else {
        as.POSIXct(NA)
      }
    ) %>%
    arrange(
      .data[[pid_col]],
      desc(.completeness),
      desc(.dt),
      desc(.row_order)
    ) %>%
    distinct(.data[[pid_col]], .keep_all = TRUE) %>%
    select(-.row_order, -.completeness, -.dt)
}

best_testing_start_per_pid <- function(df, pid_col = "part_id") {
  
  if (!pid_col %in% names(df)) {
    stop("PID column not found: ", pid_col)
  }
  
  phq_cols <- c(paste0("phq9_", 1:9), "phq9_sum")
  phq_cols <- phq_cols[phq_cols %in% names(df)]
  
  bdi_cols <- c(
    paste0("bdi_", 1:21),
    "bdi_sum",
    "bdi_total",
    "bdi_sum_calc",
    "bdi_ii_total"
  )
  bdi_cols <- bdi_cols[bdi_cols %in% names(df)]
  
  clinical_cols <- unique(c(phq_cols, bdi_cols))
  
  dt_col <- c("RecordedDate", "EndDate", "StartDate")
  dt_col <- dt_col[dt_col %in% names(df)][1]
  
  df %>%
    filter(!is.na(.data[[pid_col]])) %>%
    mutate(
      .row_order = row_number(),
      
      .phq_completeness = if (length(phq_cols) > 0) {
        rowSums(
          across(
            all_of(phq_cols),
            ~ !is.na(.x) & str_squish(as.character(.x)) != ""
          ),
          na.rm = TRUE
        )
      } else {
        0
      },
      
      .bdi_completeness = if (length(bdi_cols) > 0) {
        rowSums(
          across(
            all_of(bdi_cols),
            ~ !is.na(.x) & str_squish(as.character(.x)) != ""
          ),
          na.rm = TRUE
        )
      } else {
        0
      },
      
      .clinical_completeness = if (length(clinical_cols) > 0) {
        rowSums(
          across(
            all_of(clinical_cols),
            ~ !is.na(.x) & str_squish(as.character(.x)) != ""
          ),
          na.rm = TRUE
        )
      } else {
        0
      },
      
      .dt = if (!is.na(dt_col)) {
        suppressWarnings(
          parse_date_time(
            .data[[dt_col]],
            orders = c(
              "ymd HMS", "ymd HM",
              "dmy HMS", "dmy HM",
              "mdy HMS", "mdy HM"
            )
          )
        )
      } else {
        as.POSIXct(NA)
      }
    ) %>%
    arrange(
      .data[[pid_col]],
      desc(.bdi_completeness),
      desc(.phq_completeness),
      desc(.clinical_completeness),
      desc(.dt),
      desc(.row_order)
    ) %>%
    distinct(.data[[pid_col]], .keep_all = TRUE) %>%
    select(
      -.row_order,
      -.phq_completeness,
      -.bdi_completeness,
      -.clinical_completeness,
      -.dt
    )
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

score_phq_item <- function(x) {
  x_chr <- str_to_lower(str_squish(as.character(x)))
  
  case_when(
    is.na(x_chr) | x_chr == "" ~ NA_real_,
    str_detect(x_chr, "not at all") ~ 0,
    str_detect(x_chr, "several days") ~ 1,
    str_detect(x_chr, "more than half") ~ 2,
    str_detect(x_chr, "nearly every day") ~ 3,
    str_detect(x_chr, "^0$") ~ 0,
    str_detect(x_chr, "^1$") ~ 1,
    str_detect(x_chr, "^2$") ~ 2,
    str_detect(x_chr, "^3$") ~ 3,
    TRUE ~ suppressWarnings(as.numeric(x_chr))
  )
}

score_bdi_item <- function(x) {
  x_chr <- str_squish(as.character(x))
  
  case_when(
    is.na(x_chr) | x_chr == "" ~ NA_real_,
    str_detect(x_chr, "^0") ~ 0,
    str_detect(x_chr, "^1") ~ 1,
    str_detect(x_chr, "^2") ~ 2,
    str_detect(x_chr, "^3") ~ 3,
    TRUE ~ suppressWarnings(as.numeric(x_chr))
  )
}

clean_cat <- function(x) {
  x_chr <- str_squish(as.character(x))
  x_chr <- if_else(is.na(x) | x_chr == "", NA_character_, x_chr)
  
  case_when(
    str_to_lower(x_chr) %in% c("true", "yes", "y", "1") ~ "Yes",
    str_to_lower(x_chr) %in% c("false", "no", "n", "0") ~ "No",
    TRUE ~ x_chr
  )
}

summarise_numeric <- function(df, value_col, stage, label) {
  x <- suppressWarnings(as.numeric(df[[value_col]]))
  stage_denom <- nrow(df)
  nonmissing_n <- sum(!is.na(x))
  missing_n <- sum(is.na(x))
  
  tibble(
    Stage = stage,
    Variable = label,
    Level = NA_character_,
    Stage_denominator = stage_denom,
    Denominator = nonmissing_n,
    Non_missing_n = nonmissing_n,
    Missing_n = missing_n,
    Count = NA_integer_,
    Percent = NA_real_,
    Mean = ifelse(nonmissing_n > 0, mean(x, na.rm = TRUE), NA_real_),
    SD = ifelse(nonmissing_n > 1, sd(x, na.rm = TRUE), NA_real_),
    Min = ifelse(nonmissing_n > 0, min(x, na.rm = TRUE), NA_real_),
    Max = ifelse(nonmissing_n > 0, max(x, na.rm = TRUE), NA_real_)
  )
}

summarise_categorical <- function(df, value_col, stage, label) {
  vals <- clean_cat(df[[value_col]])
  stage_denom <- nrow(df)
  
  nonmissing <- !is.na(vals)
  cat_denom <- sum(nonmissing)
  missing_n <- sum(!nonmissing)
  
  nonmissing_tbl <- tibble(value = vals[nonmissing]) %>%
    count(value, name = "Count") %>%
    mutate(
      Stage = stage,
      Variable = label,
      Level = value,
      Stage_denominator = stage_denom,
      Denominator = cat_denom,
      Non_missing_n = cat_denom,
      Missing_n = missing_n,
      Percent = ifelse(cat_denom > 0, 100 * Count / cat_denom, NA_real_),
      Mean = NA_real_,
      SD = NA_real_,
      Min = NA_real_,
      Max = NA_real_
    ) %>%
    select(
      Stage, Variable, Level, Stage_denominator, Denominator,
      Non_missing_n, Missing_n, Count, Percent, Mean, SD, Min, Max
    )
  
  missing_tbl <- tibble(
    Stage = stage,
    Variable = label,
    Level = "Unavailable / missing",
    Stage_denominator = stage_denom,
    Denominator = stage_denom,
    Non_missing_n = cat_denom,
    Missing_n = missing_n,
    Count = missing_n,
    Percent = ifelse(stage_denom > 0, 100 * missing_n / stage_denom, NA_real_),
    Mean = NA_real_,
    SD = NA_real_,
    Min = NA_real_,
    Max = NA_real_
  )
  
  bind_rows(nonmissing_tbl, missing_tbl)
}

missing_audit <- function(df, value_col, stage, label) {
  vals <- df[[value_col]]
  is_missing <- is.na(vals) | str_squish(as.character(vals)) == ""
  
  missing_ids <- df %>%
    filter(is_missing) %>%
    pull(part_id) %>%
    unique() %>%
    na.omit()
  
  tibble(
    Stage = stage,
    Variable = label,
    Stage_denominator = nrow(df),
    Non_missing_n = nrow(df) - length(missing_ids),
    Missing_n = length(missing_ids),
    Missing_percent = ifelse(nrow(df) > 0, 100 * length(missing_ids) / nrow(df), NA_real_),
    Missing_part_ids = paste(missing_ids, collapse = "; ")
  )
}

parse_testing_calendar_ids <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(tibble())
  }
  
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  
  unfolded <- character(0)
  for (ln in lines) {
    if (str_starts(ln, " ") && length(unfolded) > 0) {
      unfolded[length(unfolded)] <- paste0(unfolded[length(unfolded)], str_sub(ln, 2))
    } else {
      unfolded <- c(unfolded, ln)
    }
  }
  
  txt <- paste(unfolded, collapse = "\n")
  events <- str_split(txt, "BEGIN:VEVENT", simplify = FALSE)[[1]]
  events <- events[events != ""]
  
  map_dfr(events, function(ev) {
    summary <- str_match(ev, "\nSUMMARY:(.*)")[, 2]
    dtstart <- str_match(ev, "\nDTSTART[^:]*:(.*)")[, 2]
    description <- str_match(
      ev,
      "\nDESCRIPTION:(.*?)(\nLAST-MODIFIED|\nLOCATION|\nSEQUENCE|\nSTATUS|\nEND:VEVENT)"
    )[, 2]
    
    if (is.na(description)) description <- ""
    
    description_clean <- description %>%
      str_replace_all("\\\\n", "\n") %>%
      str_replace_all("<br>", "\n") %>%
      str_replace_all("<[^>]+>", "")
    
    ids <- str_match_all(
      description_clean,
      regex("part[_ ]?id\\s*:?\\s*([0-9]{1,6})", ignore_case = TRUE)
    )[[1]]
    
    pid <- if (nrow(ids) > 0) ids[1, 2] else NA_character_
    
    tibble(
      calendar_dtstart = dtstart %||% NA_character_,
      calendar_summary = summary %||% NA_character_,
      calendar_part_id = clean_id(pid),
      calendar_is_testing_session =
        str_detect(coalesce(summary, ""), regex("Testing Session", ignore_case = TRUE)) |
        str_detect(description_clean, regex("Testing Start", ignore_case = TRUE)),
      calendar_is_cancelled_or_noshow =
        str_detect(coalesce(summary, ""), regex("CANCELLED|NO SHOW|RESCHEDULE", ignore_case = TRUE))
    )
  }) %>%
    filter(calendar_is_testing_session, !is.na(calendar_part_id)) %>%
    distinct(calendar_part_id, .keep_all = TRUE)
}

# ===================================================
# 3. LOCATE AND LOAD FILES
# ===================================================

WP1_SIGNUP_PATH <- newest_match(c("*WP1*Sign-up*.csv"))
WP1_TEST_PATH   <- newest_match(c("*WP1*Testing*Start*.csv"))
WP1_FB_PATH     <- newest_match(c("*WP1*Session*Feedback*.csv"))
CALENDAR_PATH   <- newest_match(c("*Strobe Bookings*.ics"), required = FALSE)

message("WP1 sign-up:          ", basename(WP1_SIGNUP_PATH))
message("WP1 testing start:    ", basename(WP1_TEST_PATH))
message("WP1 session feedback: ", basename(WP1_FB_PATH))
message("Calendar:             ", ifelse(is.null(CALENDAR_PATH), "not found", basename(CALENDAR_PATH)))

wp1_signup_raw <- read_qualtrics_real(WP1_SIGNUP_PATH)
wp1_test_raw   <- read_qualtrics_real(WP1_TEST_PATH)
wp1_fb_raw     <- read_qualtrics_real(WP1_FB_PATH)

calendar_ids <- parse_testing_calendar_ids(CALENDAR_PATH)

# ===================================================
# 4. CLEAN IDS
# ===================================================

if (!"part_id" %in% names(wp1_signup_raw)) {
  stop("No part_id column found in WP1 Sign-up.")
}

if (!"part_id" %in% names(wp1_test_raw)) {
  stop("No part_id column found in WP1 Testing Start.")
}

# Sign-up IDs are NOT corrected, preserving screened n = 197.
wp1_signup <- wp1_signup_raw %>%
  mutate(
    part_id_original = clean_id(part_id),
    part_id = clean_id(part_id)
  ) %>%
  filter(!is.na(part_id))

# Testing Start IDs ARE corrected.
wp1_test <- wp1_test_raw %>%
  mutate(
    part_id_original = clean_id(part_id),
    part_id = apply_id_corrections(part_id)
  ) %>%
  filter(!is.na(part_id))

fb_id_col <- find_col(wp1_fb_raw, c("participant_id", "part_id"))
fb_session_col <- find_col(wp1_fb_raw, c("session_n", "session", "session_number"))
fb_discomfort_col <- find_col(wp1_fb_raw, c("discomfortScore", "tol_score", "vdq", "vdq_score", "vdq_total"))

if (is.null(fb_id_col)) stop("Could not find participant_id / part_id in WP1 Session Feedback.")
if (is.null(fb_session_col)) stop("Could not find session_n in WP1 Session Feedback.")
if (is.null(fb_discomfort_col)) stop("Could not find discomfortScore / tolerability score in WP1 Session Feedback.")

# Session Feedback IDs ARE corrected.
wp1_fb <- wp1_fb_raw %>%
  mutate(
    part_id_original = clean_id(.data[[fb_id_col]]),
    part_id = apply_id_corrections(.data[[fb_id_col]]),
    session_n_num = suppressWarnings(as.numeric(.data[[fb_session_col]])),
    discomfort_num = suppressWarnings(as.numeric(.data[[fb_discomfort_col]])),
    status_clean = if ("Status" %in% names(.)) str_to_lower(str_squish(as.character(Status))) else NA_character_,
    distribution_clean = if ("DistributionChannel" %in% names(.)) str_to_lower(str_squish(as.character(DistributionChannel))) else NA_character_
  ) %>%
  filter(!is.na(part_id))

# Best complete sign-up row per participant; fixes duplicate blank sign-up rows.
wp1_signup_u <- best_signup_per_pid(wp1_signup, "part_id")

# Best complete Testing Start row per participant; fixes duplicate blank/incomplete BDI-II rows.
wp1_test_u <- best_testing_start_per_pid(wp1_test, "part_id")

# ===================================================
# 5. SCORE BASELINE PHQ-9 AND BDI-II
# ===================================================

phq_items <- paste0("phq9_", 1:9)

if ("phq9_sum" %in% names(wp1_signup_u)) {
  wp1_signup_u <- wp1_signup_u %>%
    mutate(phq9_signup_total = suppressWarnings(as.numeric(phq9_sum)))
} else if (all(phq_items %in% names(wp1_signup_u))) {
  wp1_signup_u <- wp1_signup_u %>%
    mutate(
      across(all_of(phq_items), score_phq_item, .names = "{.col}_score"),
      phq9_signup_total = rowSums(across(all_of(paste0(phq_items, "_score"))), na.rm = FALSE)
    )
} else {
  wp1_signup_u$phq9_signup_total <- NA_real_
}

if ("phq9_sum" %in% names(wp1_test_u)) {
  wp1_test_u <- wp1_test_u %>%
    mutate(phq9_test_total = suppressWarnings(as.numeric(phq9_sum)))
} else if (all(phq_items %in% names(wp1_test_u))) {
  wp1_test_u <- wp1_test_u %>%
    mutate(
      across(all_of(phq_items), score_phq_item, .names = "{.col}_score"),
      phq9_test_total = rowSums(across(all_of(paste0(phq_items, "_score"))), na.rm = FALSE)
    )
} else {
  wp1_test_u$phq9_test_total <- NA_real_
}

bdi_items <- paste0("bdi_", 1:21)

if ("bdi_sum" %in% names(wp1_test_u)) {
  wp1_test_u <- wp1_test_u %>%
    mutate(bdi_total = suppressWarnings(as.numeric(bdi_sum)))
} else if (all(bdi_items %in% names(wp1_test_u))) {
  wp1_test_u <- wp1_test_u %>%
    mutate(
      across(all_of(bdi_items), score_bdi_item, .names = "{.col}_score"),
      bdi_total = rowSums(across(all_of(paste0(bdi_items, "_score"))), na.rm = FALSE)
    )
} else {
  wp1_test_u$bdi_total <- NA_real_
}

wp1_test_scores <- wp1_test_u %>%
  select(part_id, phq9_test_total, bdi_total)

cat("\n=== Testing Start clinical availability after best-row selection ===\n")
cat("Testing Start unique IDs:", n_distinct(wp1_test_u$part_id), "\n")
cat("PHQ-9 available:", sum(!is.na(wp1_test_u$phq9_test_total)), "\n")
cat("BDI-II available:", sum(!is.na(wp1_test_u$bdi_total)), "\n")

cat("\nTesting Start IDs missing BDI-II after best-row selection:\n")
print(
  wp1_test_u %>%
    filter(is.na(bdi_total)) %>%
    pull(part_id) %>%
    sort()
)

# ===================================================
# 6. DEFINE DENOMINATORS
# ===================================================

screened_ids <- wp1_signup_u %>%
  pull(part_id) %>%
  unique()

passed_ids <- wp1_signup_u %>%
  mutate(excluded_clean = str_to_upper(str_trim(as.character(excluded)))) %>%
  filter(excluded_clean == "FALSE") %>%
  pull(part_id) %>%
  unique()

tested_ids <- wp1_test_u %>%
  pull(part_id) %>%
  unique()

valid_feedback <- wp1_fb %>%
  filter(
    !is.na(part_id),
    part_id != "0",
    !is.na(session_n_num),
    session_n_num >= 1,
    session_n_num <= 11,
    !is.na(discomfort_num),
    is.na(status_clean) | status_clean != "survey preview",
    is.na(distribution_clean) | distribution_clean != "preview"
  )

valid_feedback_ids <- valid_feedback %>%
  pull(part_id) %>%
  unique()

analysed_auto_ids <- intersect(tested_ids, valid_feedback_ids)
analysed_ids <- setdiff(analysed_auto_ids, MANUAL_ANALYSED_EXCLUDE_IDS)

stage_ids <- list(
  "Screened" = screened_ids,
  "Passed screening" = passed_ids,
  "Tested" = tested_ids,
  "Analysed" = analysed_ids
)

stage_n_table <- tibble(
  Stage = names(stage_ids),
  N = map_int(stage_ids, ~ length(unique(na.omit(.x))))
)

denominator_audit <- tibble(
  Quantity = c(
    "Screened IDs in sign-up",
    "Passed-screening IDs in sign-up",
    "Testing Start IDs after correction",
    "Valid Session Feedback IDs after correction",
    "Testing Start AND valid Session Feedback IDs, before manual exclusions",
    "Manual analysed exclusions",
    "Final manuscript analysed IDs",
    "Testing Start IDs without valid feedback",
    "Valid feedback IDs without Testing Start"
  ),
  N = c(
    length(screened_ids),
    length(passed_ids),
    length(tested_ids),
    length(valid_feedback_ids),
    length(analysed_auto_ids),
    length(MANUAL_ANALYSED_EXCLUDE_IDS),
    length(analysed_ids),
    length(setdiff(tested_ids, valid_feedback_ids)),
    length(setdiff(valid_feedback_ids, tested_ids))
  ),
  IDs = c(
    paste(sort(screened_ids), collapse = "; "),
    paste(sort(passed_ids), collapse = "; "),
    paste(sort(tested_ids), collapse = "; "),
    paste(sort(valid_feedback_ids), collapse = "; "),
    paste(sort(analysed_auto_ids), collapse = "; "),
    paste(sort(MANUAL_ANALYSED_EXCLUDE_IDS), collapse = "; "),
    paste(sort(analysed_ids), collapse = "; "),
    paste(sort(setdiff(tested_ids, valid_feedback_ids)), collapse = "; "),
    paste(sort(setdiff(valid_feedback_ids, tested_ids)), collapse = "; ")
  )
)

cat("\n=== Denominator audit ===\n")
print(denominator_audit %>% select(Quantity, N), n = Inf)

cat("\n=== Final stage denominators ===\n")
print(stage_n_table)

cat("\nAnalysed IDs missing BDI-II after best-row selection:\n")
print(
  wp1_test_scores %>%
    filter(part_id %in% analysed_ids, is.na(bdi_total)) %>%
    pull(part_id) %>%
    sort()
)

# Hard checks
if (length(screened_ids) != EXPECTED_SCREENED_N) {
  stop("Screened n mismatch: expected ", EXPECTED_SCREENED_N, ", got ", length(screened_ids))
}

if (length(tested_ids) != EXPECTED_TESTED_N) {
  stop("Tested n mismatch: expected ", EXPECTED_TESTED_N, ", got ", length(tested_ids))
}

if (length(analysed_ids) != EXPECTED_ANALYSED_N) {
  cat("\nAnalysed IDs before manual exclusions:\n")
  print(sort(analysed_auto_ids))
  
  cat("\nTesting Start IDs without valid feedback:\n")
  print(sort(setdiff(tested_ids, valid_feedback_ids)))
  
  cat("\nValid feedback IDs without Testing Start:\n")
  print(sort(setdiff(valid_feedback_ids, tested_ids)))
  
  stop("Analysed n mismatch: expected ", EXPECTED_ANALYSED_N, ", got ", length(analysed_ids))
}

# ===================================================
# 7. MASTER TABLE
# ===================================================

all_stage_ids <- unique(unlist(stage_ids))

wp1_master <- tibble(part_id = all_stage_ids) %>%
  left_join(
    wp1_signup_u %>%
      select(
        part_id,
        any_of(c(
          "incl_dem_age",
          "incl_dem_sex",
          "incl_dem_gender",
          "incl_dem_med",
          "phq9_signup_total"
        ))
      ),
    by = "part_id"
  ) %>%
  left_join(wp1_test_scores, by = "part_id") %>%
  mutate(
    baseline_phq9 = coalesce(phq9_test_total, phq9_signup_total)
  )

needed_cols <- c(
  "incl_dem_age",
  "incl_dem_sex",
  "incl_dem_gender",
  "incl_dem_med",
  "baseline_phq9",
  "bdi_total"
)

for (col in needed_cols) {
  if (!col %in% names(wp1_master)) wp1_master[[col]] <- NA
}

analysed_missing_demo <- wp1_master %>%
  filter(part_id %in% analysed_ids) %>%
  mutate(
    has_age = !is.na(suppressWarnings(as.numeric(incl_dem_age))),
    has_sex = !is.na(clean_cat(incl_dem_sex)),
    has_gender = !is.na(clean_cat(incl_dem_gender)),
    has_med = !is.na(clean_cat(incl_dem_med)),
    has_any_demo = has_age | has_sex | has_gender | has_med
  ) %>%
  filter(!has_any_demo) %>%
  pull(part_id)

cat("\n=== Analysed IDs missing all sign-up demographics after reconciliation ===\n")
print(sort(analysed_missing_demo))

if (length(analysed_missing_demo) > 0) {
  warning(
    "Some analysed IDs still lack all demographics. Inspect ID audit and consider additional reconciliation."
  )
}

feedback_counts <- valid_feedback %>%
  count(part_id, name = "n_valid_feedback_rows")

id_audit <- tibble(part_id = unique(c(screened_ids, tested_ids, valid_feedback_ids))) %>%
  mutate(
    In_signup = part_id %in% screened_ids,
    Passed_screening = part_id %in% passed_ids,
    In_testing_start_after_correction = part_id %in% tested_ids,
    In_valid_feedback_after_correction = part_id %in% valid_feedback_ids,
    In_auto_analysed_before_manual_exclusion = part_id %in% analysed_auto_ids,
    Manually_excluded_from_analysed = part_id %in% MANUAL_ANALYSED_EXCLUDE_IDS,
    Analysed_main = part_id %in% analysed_ids,
    Has_signup_demographics = part_id %in% wp1_signup_u$part_id,
    Has_testing_start_scores = part_id %in% wp1_test_u$part_id,
    In_calendar_testing_session = if (nrow(calendar_ids) > 0) part_id %in% calendar_ids$calendar_part_id else NA
  ) %>%
  left_join(feedback_counts, by = "part_id") %>%
  mutate(n_valid_feedback_rows = replace_na(n_valid_feedback_rows, 0L)) %>%
  arrange(desc(Analysed_main), part_id)

calendar_audit <- calendar_ids %>%
  mutate(
    In_testing_start_after_correction = calendar_part_id %in% tested_ids,
    In_valid_feedback_after_correction = calendar_part_id %in% valid_feedback_ids,
    In_analysed = calendar_part_id %in% analysed_ids,
    In_signup = calendar_part_id %in% screened_ids
  )

# ===================================================
# 8. SUMMARISE TABLE VALUES
# ===================================================

summary_rows <- list()
missing_rows <- list()

for (stage_name in names(stage_ids)) {
  ids <- stage_ids[[stage_name]]
  
  df_stage <- wp1_master %>%
    filter(part_id %in% ids) %>%
    distinct(part_id, .keep_all = TRUE)
  
  summary_rows[[length(summary_rows) + 1]] <- summarise_numeric(df_stage, "incl_dem_age", stage_name, "Age")
  summary_rows[[length(summary_rows) + 1]] <- summarise_categorical(df_stage, "incl_dem_sex", stage_name, "Sex")
  summary_rows[[length(summary_rows) + 1]] <- summarise_categorical(df_stage, "incl_dem_gender", stage_name, "Gender")
  summary_rows[[length(summary_rows) + 1]] <- summarise_categorical(df_stage, "incl_dem_med", stage_name, "Medication status")
  summary_rows[[length(summary_rows) + 1]] <- summarise_numeric(df_stage, "baseline_phq9", stage_name, "Baseline PHQ-9")
  summary_rows[[length(summary_rows) + 1]] <- summarise_numeric(df_stage, "bdi_total", stage_name, "Baseline BDI-II")
  
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_stage, "incl_dem_age", stage_name, "Age")
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_stage, "incl_dem_sex", stage_name, "Sex")
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_stage, "incl_dem_gender", stage_name, "Gender")
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_stage, "incl_dem_med", stage_name, "Medication status")
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_stage, "baseline_phq9", stage_name, "Baseline PHQ-9")
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_stage, "bdi_total", stage_name, "Baseline BDI-II")
}

wp1_demo_summary <- bind_rows(summary_rows) %>%
  mutate(
    Percent = round(Percent, 1),
    Mean = round(Mean, 2),
    SD = round(SD, 2),
    Min = round(Min, 2),
    Max = round(Max, 2)
  )

wp1_missing_denominators <- bind_rows(missing_rows) %>%
  mutate(Missing_percent = round(Missing_percent, 1))

# ===================================================
# 9. FORMAT MAIN TABLE
# ===================================================

format_numeric_summary <- function(mean, sd, min, max, n, miss) {
  ifelse(
    is.na(mean),
    "",
    paste0(
      sprintf("%.2f", mean),
      " ± ",
      sprintf("%.2f", sd),
      " [",
      sprintf("%.2f", min),
      "–",
      sprintf("%.2f", max),
      "]",
      " (n = ", n,
      ifelse(miss > 0, paste0("; unavailable = ", miss), ""),
      ")"
    )
  )
}

demo_numeric <- wp1_demo_summary %>%
  filter(is.na(Level)) %>%
  mutate(
    Display = format_numeric_summary(Mean, SD, Min, Max, Non_missing_n, Missing_n)
  ) %>%
  select(Stage, Variable, Display)

demo_categorical <- wp1_demo_summary %>%
  filter(
    !is.na(Level),
    Level != "Unavailable / missing",
    Count > 0
  ) %>%
  mutate(
    Display = paste0(
      Count,
      "/",
      Denominator,
      " (",
      sprintf("%.1f", 100 * Count / Denominator),
      "%)"
    )
  ) %>%
  select(Stage, Variable, Level, Display) %>%
  mutate(Variable = paste0(Variable, ": ", Level)) %>%
  select(Stage, Variable, Display)

demo_unavailable <- wp1_demo_summary %>%
  filter(
    !is.na(Level),
    Level == "Unavailable / missing",
    Count > 0
  ) %>%
  mutate(
    Display = paste0(Count, "/", Stage_denominator, " unavailable")
  ) %>%
  select(Stage, Variable, Level, Display) %>%
  mutate(Variable = paste0(Variable, ": Unavailable / missing")) %>%
  select(Stage, Variable, Display)

demo_table_wide <- bind_rows(
  demo_numeric,
  demo_categorical,
  demo_unavailable
) %>%
  pivot_wider(
    names_from = Stage,
    values_from = Display
  ) %>%
  mutate(across(everything(), ~ replace_na(.x, ""))) %>%
  rename(Characteristic = Variable)

preferred_order <- c(
  "Age",
  "Sex: Female",
  "Sex: Male",
  "Sex: Other",
  "Sex: Prefer not to say",
  "Sex: Unavailable / missing",
  "Gender: Female",
  "Gender: Male",
  "Gender: Other",
  "Gender: Non-binary",
  "Gender: Prefer not to say",
  "Gender: Unavailable / missing",
  "Medication status: Yes",
  "Medication status: No",
  "Medication status: Unavailable / missing",
  "Baseline PHQ-9",
  "Baseline BDI-II"
)

demo_table_wide <- demo_table_wide %>%
  mutate(
    .order = match(Characteristic, preferred_order),
    .order = ifelse(is.na(.order), 999, .order)
  ) %>%
  arrange(.order, Characteristic) %>%
  select(-.order)

# ===================================================
# 10. EXPORT MAIN TABLE
# ===================================================

demo_gt <- demo_table_wide %>%
  gt() %>%
  tab_header(
    title = md("**WP1 Stage-Specific Demographic and Baseline Clinical Characteristics**"),
    subtitle = md(
      "Continuous variables are mean ± SD [range]. Categorical variables are n / non-missing denominator (%). Unavailable records are shown separately."
    )
  ) %>%
  cols_align(align = "left", columns = Characteristic) %>%
  cols_align(align = "center", columns = -Characteristic) %>%
  tab_options(
    table.font.names = "Palatino Linotype",
    table.font.size = px(13),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(12),
    column_labels.font.weight = "bold",
    data_row.padding = px(4),
    source_notes.font.size = px(10),
    table.border.top.width = px(1),
    table.border.bottom.width = px(1)
  ) %>%
  tab_source_note(
    source_note = md(
      paste0(
        "Screened (n = ", EXPECTED_SCREENED_N,
        ") and passed-screening denominators are derived from WP1 Sign-up without ID collapsing. ",
        "Tested (n = ", EXPECTED_TESTED_N,
        ") is derived from Testing Start after ID reconciliation and best-row selection. ",
        "Analysed (n = ", EXPECTED_ANALYSED_N,
        ") is Testing Start ∩ valid Session Feedback after ID reconciliation. ",
        "Corrections applied to Testing Start / Session Feedback only: ",
        paste0(ID_CORRECTIONS_TEST_FB$bad_id, "→", ID_CORRECTIONS_TEST_FB$correct_id, collapse = "; "),
        "."
      )
    )
  )

gtsave(demo_gt, OUT_TABLE_HTML)
gtsave(demo_gt, OUT_TABLE_PNG)
gtsave(demo_gt, OUT_TABLE_PDF)

# ===================================================
# 11. EXPORT MISSING-DENOMINATOR TABLE
# ===================================================

missing_table_export <- wp1_missing_denominators %>%
  mutate(
    Missing = paste0(
      Missing_n,
      "/",
      Stage_denominator,
      " (",
      sprintf("%.1f", Missing_percent),
      "%)"
    )
  ) %>%
  select(
    Stage,
    Variable,
    Stage_denominator,
    Non_missing_n,
    Missing,
    Missing_part_ids
  ) %>%
  arrange(
    factor(Stage, levels = c("Screened", "Passed screening", "Tested", "Analysed")),
    Variable
  )

missing_gt <- missing_table_export %>%
  gt() %>%
  tab_header(
    title = md("**WP1 Missing-Denominator Audit**"),
    subtitle = md("Missingness is shown as missing n / stage denominator (%), with participant IDs listed where available.")
  ) %>%
  cols_label(
    Stage = "Stage",
    Variable = "Variable",
    Stage_denominator = "Stage denominator",
    Non_missing_n = "Non-missing n",
    Missing = "Missing / unavailable",
    Missing_part_ids = "Missing participant IDs"
  ) %>%
  cols_align(
    align = "center",
    columns = c(Stage, Variable, Stage_denominator, Non_missing_n, Missing)
  ) %>%
  cols_align(align = "left", columns = Missing_part_ids) %>%
  tab_options(
    table.font.names = "Palatino Linotype",
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(12),
    column_labels.font.weight = "bold",
    data_row.padding = px(4),
    source_notes.font.size = px(10)
  )

gtsave(missing_gt, OUT_MISSING_HTML)
gtsave(missing_gt, OUT_MISSING_PNG)
gtsave(missing_gt, OUT_MISSING_PDF)

# ===================================================
# 12. SAVE AUDITS
# ===================================================

readr::write_csv(wp1_demo_summary, OUT_SUMMARY_CSV)
readr::write_csv(wp1_missing_denominators, OUT_MISSING_CSV)
readr::write_csv(id_audit, OUT_ID_AUDIT_CSV)
readr::write_csv(denominator_audit, OUT_DENOM_AUDIT_CSV)
readr::write_csv(calendar_audit, OUT_CALENDAR_AUDIT_CSV)

text_lines <- c(
  "WP1 reconciled demographics table denominator audit",
  "===================================================",
  "",
  paste0("Sign-up file: ", basename(WP1_SIGNUP_PATH)),
  paste0("Testing Start file: ", basename(WP1_TEST_PATH)),
  paste0("Session Feedback file: ", basename(WP1_FB_PATH)),
  paste0("Calendar file: ", ifelse(is.null(CALENDAR_PATH), "not found", basename(CALENDAR_PATH))),
  "",
  "Manuscript denominators enforced:",
  paste0("Screened: n = ", EXPECTED_SCREENED_N),
  paste0("Tested: n = ", EXPECTED_TESTED_N),
  paste0("Analysed: n = ", EXPECTED_ANALYSED_N),
  "",
  "ID corrections applied to Testing Start and Session Feedback only:",
  paste0(ID_CORRECTIONS_TEST_FB$bad_id, " -> ", ID_CORRECTIONS_TEST_FB$correct_id, " (", ID_CORRECTIONS_TEST_FB$reason, ")"),
  "",
  "Stage denominators used:",
  paste0(stage_n_table$Stage, ": n = ", stage_n_table$N),
  "",
  "Testing Start clinical availability after best-row selection:",
  paste0("PHQ-9 available: ", sum(!is.na(wp1_test_u$phq9_test_total))),
  paste0("BDI-II available: ", sum(!is.na(wp1_test_u$bdi_total))),
  "",
  "Testing Start IDs missing BDI-II after best-row selection:",
  paste(
    wp1_test_u %>%
      filter(is.na(bdi_total)) %>%
      pull(part_id) %>%
      sort(),
    collapse = ", "
  ),
  "",
  "Analysed IDs missing BDI-II after best-row selection:",
  paste(
    wp1_test_scores %>%
      filter(part_id %in% analysed_ids, is.na(bdi_total)) %>%
      pull(part_id) %>%
      sort(),
    collapse = ", "
  ),
  "",
  "Analysed IDs missing all sign-up demographics after reconciliation:",
  if (length(analysed_missing_demo) == 0) "None" else paste(sort(analysed_missing_demo), collapse = ", "),
  "",
  "Denominator audit:",
  paste0(denominator_audit$Quantity, ": n = ", denominator_audit$N)
)

writeLines(text_lines, OUT_TEXT)

cat("\n=== Exported files ===\n")
cat("Main table HTML:      ", OUT_TABLE_HTML, "\n")
cat("Main table PNG:       ", OUT_TABLE_PNG, "\n")
cat("Main table PDF:       ", OUT_TABLE_PDF, "\n")
cat("Missing table HTML:   ", OUT_MISSING_HTML, "\n")
cat("Missing table PNG:    ", OUT_MISSING_PNG, "\n")
cat("Missing table PDF:    ", OUT_MISSING_PDF, "\n")
cat("Summary CSV:          ", OUT_SUMMARY_CSV, "\n")
cat("Missing CSV:          ", OUT_MISSING_CSV, "\n")
cat("ID audit CSV:         ", OUT_ID_AUDIT_CSV, "\n")
cat("Denominator audit:    ", OUT_DENOM_AUDIT_CSV, "\n")
cat("Calendar audit:       ", OUT_CALENDAR_AUDIT_CSV, "\n")
cat("Text audit:           ", OUT_TEXT, "\n")


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


# ------------------------------------------------------------
# SECTION: wp1 parameters
# ------------------------------------------------------------

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

# ============================================================
# WP1 Parameter Schedule Table — Tight PNG for Google Docs
# ============================================================

# install.packages(c("tidyverse", "gridExtra", "grid", "showtext", "sysfonts", "ragg"))

library(tidyverse)
library(gridExtra)
library(grid)
library(showtext)
library(sysfonts)
library(ragg)

# ------------------------------------------------------------
# Setup
# ------------------------------------------------------------

DATA_DIR <- Sys.getenv("MRC_DATA_DIR", unset = "C:/Users/dn284/Desktop/MRC_omni/data")
FONT_PATH <- file.path(DATA_DIR, "palatinolinotype_roman.ttf")

if (file.exists(FONT_PATH)) {
  font_add("PalatinoLinotype", regular = FONT_PATH)
  showtext_auto()
  TABLE_FONT <- "PalatinoLinotype"
} else {
  TABLE_FONT <- "serif"
}

# ------------------------------------------------------------
# Table data
# ------------------------------------------------------------

wp1_param_table <- tibble::tribble(
  ~`Session`, ~`Tested parameter`,              ~`Split`,       ~`Steps`,
  "1",        "Luminance",                      "4 × 30 sec",   "10, 30, 50, 70",
  "2",        "Luminance",                      "4 × 30 sec",   "70, 80, 90, 100",
  "3",        "Frequency",                      "4 × 30 sec",   "3, 5, 7, 9 Hz",
  "4",        "Frequency",                      "4 × 30 sec",   "9, 11, 13, 15 Hz",
  "5",        "Frequency shift — small",        "4 × 30 sec",   "3, 3.75, 4.69, 4.82 Hz\n1.25× multiplicative ratio",
  "6",        "Frequency shift — moderate",     "4 × 30 sec",   "3, 4.5, 6.75, 10.13 Hz\n1.5× multiplicative ratio",
  "7",        "Frequency shift — large",        "4 × 30 sec",   "3, 6.6, 14.52, 6.6 Hz\n2.2× multiplicative ratio",
  "8",        "Duty-cycle shift — small",       "4 × 30 sec",   "50, 55, 60, 65%",
  "9",        "Duty-cycle shift — moderate",    "4 × 30 sec",   "40, 50, 60, 70%",
  "10",       "Duty-cycle shift — large",       "4 × 30 sec",   "25, 50, 75, 50%",
  "11",       "Dynamic sequence",               "120 sec",      "Combination of the above parameters"
)

# ------------------------------------------------------------
# Table theme
# ------------------------------------------------------------

table_theme <- ttheme_minimal(
  base_family = TABLE_FONT,
  base_size = 12,
  padding = unit(c(5, 6), "pt"),
  core = list(
    fg_params = list(
      fontsize = 11,
      fontfamily = TABLE_FONT,
      hjust = 0,
      x = 0.035,
      lineheight = 0.88
    ),
    bg_params = list(
      fill = rep(c("#FFFFFF", "#F5F5F5"), length.out = nrow(wp1_param_table)),
      col = "#D6D6D6",
      lwd = 0.35
    )
  ),
  colhead = list(
    fg_params = list(
      fontsize = 11.5,
      fontface = "bold",
      fontfamily = TABLE_FONT,
      col = "white",
      hjust = 0.5,
      x = 0.5
    ),
    bg_params = list(
      fill = "#2F3A4A",
      col = "#2F3A4A",
      lwd = 0.5
    )
  )
)

# ------------------------------------------------------------
# Build table
# ------------------------------------------------------------

tg <- tableGrob(
  wp1_param_table,
  rows = NULL,
  theme = table_theme
)

# Better proportions for Google Docs page width
tg$widths <- unit(c(0.55, 2.45, 1.15, 3.1), "null")

# Slight compression
tg$heights <- tg$heights * 0.92

# ------------------------------------------------------------
# Export with tight dimensions
# ------------------------------------------------------------

out_png <- file.path(DATA_DIR, "WP1_parameter_schedule_table_TIGHT.png")

# Draw once to calculate true size
tmp <- tempfile(fileext = ".png")
agg_png(tmp, width = 8, height = 4, units = "in", res = 300, background = "white")
grid.newpage()
grid.draw(tg)
w <- convertWidth(sum(tg$widths), "in", valueOnly = TRUE)
h <- convertHeight(sum(tg$heights), "in", valueOnly = TRUE)
dev.off()

# Add tiny margin only
margin_in <- 0.04

agg_png(
  filename = out_png,
  width = w + margin_in,
  height = h + margin_in,
  units = "in",
  res = 300,
  background = "white"
)

grid.newpage()
pushViewport(viewport(
  width = unit(1, "npc") - unit(0.02, "in"),
  height = unit(1, "npc") - unit(0.02, "in")
))
grid.draw(tg)
popViewport()

dev.off()

message("Saved tight Google Docs-ready PNG to: ", out_png)

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




# ------------------------------------------------------------
# SECTION: wp1 session tolerability
# ------------------------------------------------------------

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

# ============================================================
# SUPPLEMENTARY TABLE S3.2 — WP1 SESSION-LEVEL TOLERABILITY
# CLEANED SIDE-EFFECT LOGIC + EXCEL EXPORT
# ============================================================
#
# Purpose:
#   Recreates WP1 Table S3.2 for the SM Excel workbook.
#
# Key logic:
#   - discomfortScore remains the main tolerability outcome.
#   - tol_sum is preserved as raw "any side effect?" audit flag.
#   - cleaned side-effect endorsement is based on specific VDQ
#     symptom items > "Not at all".
#   - A participant-session is counted ONCE if any symptom is endorsed,
#     instead of counting each mild symptom as a separate side-effect event.
#   - Discordant records are exported for hand-checking.
#
# Outputs:
#   supplementary_S3_2_wp1_tolerability/
#     - Table_S3_2_WP1_session_tolerability_cleaned.xlsx
#     - Table_S3_2_WP1_session_tolerability_cleaned.csv
#     - Table_S3_2_WP1_discordant_records_for_handcheck.csv
#     - Table_S3_2_WP1_symptom_category_summary.csv
#     - Table_S3_2_WP1_column_audit.csv
#
# ============================================================

# install.packages(c("tidyverse", "lubridate"))

library(tidyverse)
library(lubridate)

# ============================================================
# 0. USER SETTINGS
# ============================================================

DATA_DIR <- Sys.getenv("MRC_DATA_DIR", unset = "C:/Users/dn284/Desktop/MRC_omni/data")
SEARCH_DIRS <- c(DATA_DIR)

OUT_DIR <- file.path(DATA_DIR, "supplementary_S3_2_wp1_tolerability")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

Z80 <- 0.8416212335729143
TOLERABILITY_THRESHOLD <- 7

# ============================================================
# 1. HELPERS
# ============================================================

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
      
      matched <- all_files[
        stringr::str_detect(file_names, regex(pat_regex, ignore_case = TRUE))
      ]
      
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
    col_types = cols(.default = col_character()),
    skip = skiprows %||% 0,
    show_col_types = FALSE
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

to_num <- function(x) suppressWarnings(as.numeric(x))

clean_response_text <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_to_lower() %>%
    str_replace_all("\\s+", " ")
}

parse_yes_no <- function(x) {
  xx <- clean_response_text(x)
  
  case_when(
    xx %in% c("yes", "y", "true", "1", "side effects", "i did") ~ TRUE,
    xx %in% c("no", "n", "false", "0", "none", "not at all", "no side effects") ~ FALSE,
    is.na(xx) | xx == "" | xx %in% c("na", "n/a", "nan") ~ NA,
    TRUE ~ NA
  )
}

# Severity coding for VDQ-style symptom frequency/severity responses.
# "A little", "Moderate", and "A lot" are all symptom endorsements,
# but the participant-session is counted only once in the summary table.
parse_symptom_severity <- function(x) {
  xx <- clean_response_text(x)
  
  case_when(
    is.na(xx) | xx == "" | xx %in% c("na", "n/a", "nan") ~ NA_real_,
    
    xx %in% c(
      "not at all",
      "none",
      "none of the time",
      "no",
      "false",
      "0",
      "never"
    ) ~ 0,
    
    xx %in% c(
      "a little",
      "little",
      "slightly",
      "mild",
      "mildly",
      "somewhat",
      "1",
      "rarely"
    ) ~ 1,
    
    xx %in% c(
      "moderate",
      "moderately",
      "2",
      "sometimes"
    ) ~ 2,
    
    xx %in% c(
      "a lot",
      "lot",
      "severe",
      "severely",
      "very much",
      "3",
      "often",
      "always"
    ) ~ 3,
    
    # Numeric fallback in case Qualtrics stores coded values.
    suppressWarnings(!is.na(as.numeric(xx))) ~ suppressWarnings(as.numeric(xx)),
    
    TRUE ~ NA_real_
  )
}

fmt_mean_sd <- function(mean, sd) {
  ifelse(
    is.na(mean),
    NA_character_,
    paste0(sprintf("%.2f", mean), " ± ", sprintf("%.2f", sd))
  )
}

# ============================================================
# 2. LOAD WP1 SESSION FEEDBACK
# ============================================================

WP1_FEEDBACK_PATH <- newest_match(c("*WP1*Session*Feedback*.csv"))

message("Using WP1 feedback file: ", basename(WP1_FEEDBACK_PATH))

wp1_raw <- read_qualtrics_real(WP1_FEEDBACK_PATH)

pid_col <- find_col(wp1_raw, c("participant_id", "part_id", "pid"))
sess_col <- find_col(wp1_raw, c("session_n", "session", "session_number"))
discomfort_col <- find_col(wp1_raw, c("discomfortScore", "discomfort_score", "vdq", "vdq_score", "tol_score"))
tol_sum_col <- find_col(wp1_raw, c("tol_sum", "any_side_effect", "side_effect", "side_effects"))

if (is.null(pid_col)) stop("Could not find participant ID column.")
if (is.null(sess_col)) stop("Could not find session-number column.")
if (is.null(discomfort_col)) stop("Could not find discomfortScore / VDQ / tol_score column.")

message("Participant column: ", pid_col)
message("Session column:     ", sess_col)
message("Discomfort column:  ", discomfort_col)
message("tol_sum column:     ", tol_sum_col %||% "None detected")

# ============================================================
# 3. DETECT VDQ SYMPTOM COLUMNS
# ============================================================

# These columns are not symptom items even if they contain yes/no or text.
never_symptom_cols <- c(
  pid_col,
  sess_col,
  discomfort_col,
  tol_sum_col,
  "ResponseId",
  "RecordedDate",
  "StartDate",
  "EndDate",
  "Duration",
  "Finished",
  "Progress",
  "DistributionChannel",
  "UserLanguage",
  "participant_id",
  "part_id",
  "session_n",
  "session",
  "session_number"
)

# Base-R-safe cleanup.
# This avoids purrr::discard(), which is causing:
# Error in range[1] : object of type 'builtin' is not subsettable
never_symptom_cols <- unique(never_symptom_cols)
never_symptom_cols <- never_symptom_cols[!is.na(never_symptom_cols)]
never_symptom_cols <- never_symptom_cols[nzchar(never_symptom_cols)]
candidate_cols <- setdiff(names(wp1_raw), never_symptom_cols)

# Identify symptom columns by values containing the expected VDQ levels.
symptom_signature <- c(
  "not at all",
  "a little",
  "moderate",
  "a lot",
  "none of the time",
  "mild",
  "mildly",
  "severe",
  "severely"
)

looks_like_symptom_col <- function(x) {
  vals <- clean_response_text(x)
  vals <- vals[!is.na(vals) & vals != ""]
  if (length(vals) == 0) return(FALSE)
  
  prop_matching <- mean(vals %in% symptom_signature | vals %in% c("0", "1", "2", "3"))
  any(str_detect(vals, "not at all|a little|moderate|a lot")) || prop_matching >= 0.50
}

symptom_cols_auto <- candidate_cols[
  purrr::map_lgl(candidate_cols, ~ looks_like_symptom_col(wp1_raw[[.x]]))
]

# Remove columns that are probably global ratings rather than individual symptoms.
symptom_cols <- symptom_cols_auto[
  !str_detect(
    tolower(symptom_cols_auto),
    "discomfort|score|total|sum|overall|engagement|pleasure|arousal|sleep|drows|experience|comment|text|other_text"
  )
]

message("Detected VDQ/symptom columns:")
message(ifelse(length(symptom_cols) > 0, paste(symptom_cols, collapse = "; "), "None detected"))

if (length(symptom_cols) == 0) {
  warning(
    "No VDQ symptom columns were auto-detected. The cleaned side-effect flag will fall back to tol_sum only. ",
    "Please inspect the column audit sheet and, if needed, manually set symptom_cols."
  )
}

# ============================================================
# 4. SESSION PARAMETER LOOKUP FROM TABLE S3.1
# ============================================================

session_lookup <- tribble(
  ~session_n, ~parameter_focus, ~split_steps,
  1,  "Luminance",                  "4 × 30 sec; 10, 30, 50, 70",
  2,  "Luminance",                  "4 × 30 sec; 70, 80, 90, 100",
  3,  "Frequency",                  "4 × 30 sec; 3, 5, 7, 9 Hz",
  4,  "Frequency",                  "4 × 30 sec; 9, 11, 13, 15 Hz",
  5,  "Frequency shift (small)",    "4 × 30 sec; 3, 3.75, 4.69, 4.82 Hz",
  6,  "Frequency shift (moderate)", "4 × 30 sec; 3, 4.5, 6.75, 10.13 Hz",
  7,  "Frequency shift (large)",    "4 × 30 sec; 3, 6.6, 14.52, 6.6 Hz",
  8,  "Duty-cycle shift (small)",   "4 × 30 sec; 50, 55, 60, 65%",
  9,  "Duty-cycle shift (moderate)","4 × 30 sec; 40, 50, 60, 70%",
  10, "Duty-cycle shift (large)",   "4 × 30 sec; 25, 50, 75, 50%",
  11, "Dynamic sequence",           "120 sec; combined parameter sequence"
)

# ============================================================
# 5. BUILD PARTICIPANT-SESSION AUDIT DATASET
# ============================================================

wp1_base <- wp1_raw %>%
  mutate(
    part_id = clean_id(.data[[pid_col]]),
    session_n = to_num(.data[[sess_col]]),
    discomfortScore = to_num(.data[[discomfort_col]]),
    raw_tol_sum_text = if (!is.null(tol_sum_col)) as.character(.data[[tol_sum_col]]) else NA_character_,
    raw_tol_sum_yes = parse_yes_no(raw_tol_sum_text)
  ) %>%
  filter(
    !is.na(part_id),
    !is.na(session_n),
    session_n >= 1,
    session_n <= 11
  ) %>%
  mutate(session_n = as.integer(session_n))

if (length(symptom_cols) > 0) {
  symptom_long <- wp1_base %>%
    select(part_id, session_n, all_of(symptom_cols)) %>%
    pivot_longer(
      cols = all_of(symptom_cols),
      names_to = "symptom_column",
      values_to = "symptom_raw"
    ) %>%
    mutate(
      symptom_clean = clean_response_text(symptom_raw),
      symptom_severity = parse_symptom_severity(symptom_raw),
      symptom_endorsed = !is.na(symptom_severity) & symptom_severity > 0
    )
  
  symptom_by_session <- symptom_long %>%
    group_by(part_id, session_n) %>%
    summarise(
      n_symptom_items_detected = n(),
      n_symptom_items_answered = sum(!is.na(symptom_severity)),
      n_symptom_categories_endorsed = sum(symptom_endorsed, na.rm = TRUE),
      max_symptom_severity = suppressWarnings(max(symptom_severity, na.rm = TRUE)),
      cleaned_any_vdq_symptom = any(symptom_endorsed, na.rm = TRUE),
      endorsed_symptom_columns = paste(symptom_column[symptom_endorsed], collapse = "; "),
      endorsed_symptom_raw_values = paste(
        paste0(symptom_column[symptom_endorsed], "=", symptom_raw[symptom_endorsed]),
        collapse = "; "
      ),
      .groups = "drop"
    ) %>%
    mutate(
      max_symptom_severity = ifelse(is.infinite(max_symptom_severity), NA_real_, max_symptom_severity)
    )
} else {
  symptom_long <- tibble(
    part_id = character(),
    session_n = integer(),
    symptom_column = character(),
    symptom_raw = character(),
    symptom_clean = character(),
    symptom_severity = numeric(),
    symptom_endorsed = logical()
  )
  
  symptom_by_session <- wp1_base %>%
    distinct(part_id, session_n) %>%
    mutate(
      n_symptom_items_detected = 0L,
      n_symptom_items_answered = 0L,
      n_symptom_categories_endorsed = NA_integer_,
      max_symptom_severity = NA_real_,
      cleaned_any_vdq_symptom = NA,
      endorsed_symptom_columns = NA_character_,
      endorsed_symptom_raw_values = NA_character_
    )
}

participant_session_audit <- wp1_base %>%
  left_join(symptom_by_session, by = c("part_id", "session_n")) %>%
  mutate(
    # Manuscript-facing cleaned side-effect endorsement:
    # Prefer specific VDQ symptom items when available.
    cleaned_side_effect_participant_session = case_when(
      length(symptom_cols) > 0 ~ cleaned_any_vdq_symptom,
      is.na(raw_tol_sum_yes) ~ NA,
      TRUE ~ raw_tol_sum_yes
    ),
    
    raw_tol_sum_yes_num = as.integer(raw_tol_sum_yes %in% TRUE),
    cleaned_side_effect_num = as.integer(cleaned_side_effect_participant_session %in% TRUE),
    
    discordance_flag = case_when(
      raw_tol_sum_yes %in% TRUE &
        cleaned_side_effect_participant_session %in% FALSE ~
        "tol_sum_yes_but_all_specific_symptoms_not_at_all",
      
      raw_tol_sum_yes %in% FALSE &
        cleaned_side_effect_participant_session %in% TRUE ~
        "tol_sum_no_but_specific_symptom_endorsed",
      
      discomfortScore > 0 &
        cleaned_side_effect_participant_session %in% FALSE ~
        "discomfort_above_zero_but_no_cleaned_specific_symptom",
      
      discomfortScore == 0 &
        cleaned_side_effect_participant_session %in% TRUE ~
        "cleaned_specific_symptom_but_discomfort_zero",
      
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(session_lookup, by = "session_n") %>%
  arrange(session_n, part_id)

discordant_records_for_handcheck <- participant_session_audit %>%
  filter(!is.na(discordance_flag)) %>%
  select(
    part_id,
    session_n,
    parameter_focus,
    split_steps,
    discordance_flag,
    raw_tol_sum_text,
    raw_tol_sum_yes,
    discomfortScore,
    cleaned_side_effect_participant_session,
    n_symptom_categories_endorsed,
    max_symptom_severity,
    endorsed_symptom_columns,
    endorsed_symptom_raw_values
  ) %>%
  arrange(session_n, discordance_flag, part_id)

# ============================================================
# 6. TABLE S3.2 SESSION-LEVEL SUMMARY
# ============================================================

table_s3_2 <- participant_session_audit %>%
  group_by(session_n, parameter_focus, split_steps) %>%
  summarise(
    n_evaluable = sum(!is.na(discomfortScore)),
    mean_discomfort = mean(discomfortScore, na.rm = TRUE),
    sd_discomfort = sd(discomfortScore, na.rm = TRUE),
    se_discomfort = sd_discomfort / sqrt(n_evaluable),
    upper_one_sided_80_cl = mean_discomfort + Z80 * se_discomfort,
    
    raw_tol_sum_yes_n = sum(raw_tol_sum_yes %in% TRUE, na.rm = TRUE),
    
    cleaned_side_effect_participant_sessions_n =
      sum(cleaned_side_effect_participant_session %in% TRUE, na.rm = TRUE),
    
    cleaned_side_effect_participant_sessions_pct =
      100 * mean(cleaned_side_effect_participant_session %in% TRUE, na.rm = TRUE),
    
    total_symptom_category_endorsements =
      sum(n_symptom_categories_endorsed, na.rm = TRUE),
    
    median_symptom_categories_among_cleaned_endorsers =
      ifelse(
        sum(cleaned_side_effect_participant_session %in% TRUE, na.rm = TRUE) > 0,
        median(
          n_symptom_categories_endorsed[cleaned_side_effect_participant_session %in% TRUE],
          na.rm = TRUE
        ),
        NA_real_
      ),
    
    max_symptom_severity_observed =
      suppressWarnings(max(max_symptom_severity, na.rm = TRUE)),
    
    discordant_records_n = sum(!is.na(discordance_flag)),
    discontinuation_count = sum(discomfortScore > TOLERABILITY_THRESHOLD, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    sd_discomfort = replace_na(sd_discomfort, 0),
    se_discomfort = replace_na(se_discomfort, 0),
    upper_one_sided_80_cl = mean_discomfort + Z80 * se_discomfort,
    criterion_threshold = TOLERABILITY_THRESHOLD,
    criterion_met = upper_one_sided_80_cl < TOLERABILITY_THRESHOLD,
    
    mean_sd_discomfort = fmt_mean_sd(mean_discomfort, sd_discomfort),
    
    cleaned_side_effect_display = paste0(
      cleaned_side_effect_participant_sessions_n,
      "/",
      n_evaluable,
      " (",
      sprintf("%.1f", cleaned_side_effect_participant_sessions_pct),
      "%)"
    )
  ) %>%
  select(
    session_n,
    parameter_focus,
    split_steps,
    n_evaluable,
    mean_discomfort,
    sd_discomfort,
    mean_sd_discomfort,
    se_discomfort,
    upper_one_sided_80_cl,
    criterion_threshold,
    criterion_met,
    raw_tol_sum_yes_n,
    cleaned_side_effect_participant_sessions_n,
    cleaned_side_effect_participant_sessions_pct,
    cleaned_side_effect_display,
    total_symptom_category_endorsements,
    median_symptom_categories_among_cleaned_endorsers,
    max_symptom_severity_observed,
    discordant_records_n,
    discontinuation_count
  ) %>%
  arrange(session_n)

# Version with friendly column names for Excel/SM.
table_s3_2_excel <- table_s3_2 %>%
  transmute(
    `Session` = session_n,
    `Parameter focus` = parameter_focus,
    `Split / steps` = split_steps,
    `Evaluable n` = n_evaluable,
    `Mean discomfort` = round(mean_discomfort, 2),
    `SD` = round(sd_discomfort, 2),
    `Mean ± SD` = mean_sd_discomfort,
    `SE` = round(se_discomfort, 3),
    `Upper one-sided 80% CL` = round(upper_one_sided_80_cl, 2),
    `Criterion threshold` = criterion_threshold,
    `Criterion met` = if_else(criterion_met, "Yes", "No"),
    `Raw tol_sum yes, n` = raw_tol_sum_yes_n,
    `Cleaned participant-sessions with any VDQ symptom, n` =
      cleaned_side_effect_participant_sessions_n,
    `Cleaned participant-sessions with any VDQ symptom, %` =
      round(cleaned_side_effect_participant_sessions_pct, 1),
    `Cleaned side-effect display` = cleaned_side_effect_display,
    `Total symptom-category endorsements, audit only` =
      total_symptom_category_endorsements,
    `Median symptom categories among cleaned endorsers` =
      median_symptom_categories_among_cleaned_endorsers,
    `Maximum symptom severity observed` = max_symptom_severity_observed,
    `Discordant records needing hand-check, n` = discordant_records_n,
    `Discontinuations / >7 discomfort, n` = discontinuation_count
  )

# ============================================================
# 7. SYMPTOM CATEGORY SUMMARY
# ============================================================

symptom_category_summary <- if (nrow(symptom_long) > 0) {
  symptom_long %>%
    filter(!is.na(symptom_severity)) %>%
    group_by(symptom_column) %>%
    summarise(
      n_answered = n(),
      n_not_at_all = sum(symptom_severity == 0, na.rm = TRUE),
      n_a_little = sum(symptom_severity == 1, na.rm = TRUE),
      n_moderate = sum(symptom_severity == 2, na.rm = TRUE),
      n_a_lot = sum(symptom_severity >= 3, na.rm = TRUE),
      n_any_above_not_at_all = sum(symptom_severity > 0, na.rm = TRUE),
      pct_any_above_not_at_all = 100 * n_any_above_not_at_all / n_answered,
      .groups = "drop"
    ) %>%
    arrange(desc(n_any_above_not_at_all), symptom_column)
} else {
  tibble(
    symptom_column = character(),
    n_answered = integer(),
    n_not_at_all = integer(),
    n_a_little = integer(),
    n_moderate = integer(),
    n_a_lot = integer(),
    n_any_above_not_at_all = integer(),
    pct_any_above_not_at_all = numeric()
  )
}

# ============================================================
# 8. COLUMN AUDIT
# ============================================================

column_audit <- tibble(
  column = names(wp1_raw),
  detected_role = case_when(
    column == pid_col ~ "participant_id",
    column == sess_col ~ "session_number",
    column == discomfort_col ~ "discomfort_score",
    !is.null(tol_sum_col) & column == tol_sum_col ~ "raw_tol_sum",
    column %in% symptom_cols ~ "specific_vdq_symptom_item",
    column %in% symptom_cols_auto ~ "auto_detected_symptom_like_but_excluded_by_name_filter",
    TRUE ~ "not_used"
  )
) %>%
  arrange(factor(
    detected_role,
    levels = c(
      "participant_id",
      "session_number",
      "discomfort_score",
      "raw_tol_sum",
      "specific_vdq_symptom_item",
      "auto_detected_symptom_like_but_excluded_by_name_filter",
      "not_used"
    )
  ), column)

# ============================================================
# 9. NARRATIVE NOTES FOR EXCEL
# ============================================================

narrative_notes <- tibble(
  text = c(
    "Table S3.2 WP1 session-level tolerability summary.",
    "",
    "discomfortScore is the primary tolerability outcome and is summarised using mean, SD, SE, and upper one-sided 80% confidence limits.",
    paste0("The tolerability criterion is met where upper one-sided 80% CL < ", TOLERABILITY_THRESHOLD, "/10."),
    "",
    "Side-effect handling:",
    "tol_sum is retained as a raw audit variable because some participants endorsed the global yes/no side-effect item but then marked every specific symptom as 'Not at all'.",
    "The manuscript-facing cleaned side-effect count is therefore based on participant-sessions with at least one specific VDQ symptom item above 'Not at all'.",
    "Multiple mild symptoms in the same participant-session are counted as one cleaned participant-session endorsement in the main table.",
    "The total number of symptom-category endorsements is retained as an audit-only descriptive field and should not be presented as the number of side-effect events.",
    "",
    "Records needing hand-check are exported in the discordant-records sheet."
  )
)

# ============================================================
# 10. EXPORT CSV + EXCEL
# ultra-safe version: avoids openxlsx entirely
# ============================================================

# Install once if needed:
# install.packages("writexl")

library(writexl)

OUT_XLSX <- file.path(OUT_DIR, "Table_S3_2_WP1_session_tolerability_cleaned.xlsx")
OUT_CSV <- file.path(OUT_DIR, "Table_S3_2_WP1_session_tolerability_cleaned.csv")
OUT_DISCORDANT <- file.path(OUT_DIR, "Table_S3_2_WP1_discordant_records_for_handcheck.csv")
OUT_SYMPTOMS <- file.path(OUT_DIR, "Table_S3_2_WP1_symptom_category_summary.csv")
OUT_AUDIT <- file.path(OUT_DIR, "Table_S3_2_WP1_column_audit.csv")

# CSV exports
readr::write_csv(table_s3_2_excel, OUT_CSV)
readr::write_csv(discordant_records_for_handcheck, OUT_DISCORDANT)
readr::write_csv(symptom_category_summary, OUT_SYMPTOMS)
readr::write_csv(column_audit, OUT_AUDIT)

# Excel sheet names must be <=31 chars for Excel
excel_sheets <- list(
  "S3.2 cleaned" = table_s3_2_excel,
  "Participant audit" = participant_session_audit,
  "Discordant handcheck" = discordant_records_for_handcheck,
  "Symptom categories" = symptom_category_summary,
  "Column audit" = column_audit,
  "Notes" = narrative_notes
)

# Make sure all sheets are plain data frames/tibbles
excel_sheets <- lapply(excel_sheets, function(x) {
  x <- as.data.frame(x)
  x[] <- lapply(x, function(col) {
    if (is.list(col)) as.character(col) else col
  })
  x
})

writexl::write_xlsx(excel_sheets, path = OUT_XLSX)

cat("\nSaved Excel workbook:\n")
cat("  ", OUT_XLSX, "\n")

# ============================================================
# 11. CONSOLE SUMMARY
# ============================================================

cat("\n============================================================\n")
cat("TABLE S3.2 WP1 TOLERABILITY EXPORT COMPLETE\n")
cat("============================================================\n\n")

cat("Input file:\n")
cat("  ", WP1_FEEDBACK_PATH, "\n\n")

cat("Detected columns:\n")
cat("  Participant ID: ", pid_col, "\n")
cat("  Session:        ", sess_col, "\n")
cat("  Discomfort:     ", discomfort_col, "\n")
cat("  tol_sum:        ", tol_sum_col %||% "None detected", "\n")
cat("  VDQ symptoms:   ", ifelse(length(symptom_cols) > 0, paste(symptom_cols, collapse = "; "), "None detected"), "\n\n")

cat("Outputs:\n")
cat("  ", OUT_XLSX, "\n")
cat("  ", OUT_CSV, "\n")
cat("  ", OUT_DISCORDANT, "\n")
cat("  ", OUT_SYMPTOMS, "\n")
cat("  ", OUT_AUDIT, "\n\n")

cat("Main Table S3.2 preview:\n")
print(table_s3_2_excel, n = Inf, width = Inf)

cat("\nDiscordant records needing hand-check: ", nrow(discordant_records_for_handcheck), "\n")
if (nrow(discordant_records_for_handcheck) > 0) {
  print(discordant_records_for_handcheck, n = Inf, width = Inf)
}

# ===================================================

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

# ===================================================




# ------------------------------------------------------------
# SECTION: wp1 pre post mood comparisons
# ------------------------------------------------------------

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

# ============================================================
# WP1 PRE/POST STIMULATION MOOD CHECKS — FIXED DEBUG DROP-IN
# PHQ-9, M3VAS mood/pleasure/suicidality, BDI-II
# ============================================================

# Main fix:
# Previous versions filtered out any row containing "IP Address" anywhere.
# In these Qualtrics files, real participant rows can have Status == "IP Address",
# so all participant rows were accidentally removed.
#
# This version removes Qualtrics metadata rows ONLY by inspecting the first column,
# which should remove:
#   row 1: "Start Date"
#   row 2: '{"ImportId": ...}'
# while keeping all real participant rows.

# ----------------------------
# 0. Packages
# ----------------------------

pkgs <- c("tidyverse", "readr", "janitor", "openxlsx", "glue", "lubridate")

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(tidyverse)
library(readr)
library(janitor)
library(openxlsx)
library(glue)
library(lubridate)

# ----------------------------
# 1. User settings
# ----------------------------

DATA_DIR <- Sys.getenv("MRC_DATA_DIR", unset = "C:/Users/dn284/Desktop/MRC_omni/data")

START_FILE <- file.path(
  DATA_DIR,
  "WP1 Testing Start_February 17, 2026_09.54.csv"
)

END_FILE <- file.path(
  DATA_DIR,
  "WP1+Testing+End_March+3,+2025_02.36.csv"
)

OUT_XLSX <- file.path(DATA_DIR, "WP1_PrePost_Mood_Checks_FIXED.xlsx")
OUT_DESC_CSV <- file.path(DATA_DIR, "WP1_PrePost_Mood_Checks_Descriptives_FIXED.csv")
OUT_CHANGE_CSV <- file.path(DATA_DIR, "WP1_PrePost_Mood_Checks_Paired_Change_FIXED.csv")
OUT_NARRATIVE_TXT <- file.path(DATA_DIR, "WP1_PrePost_Mood_Checks_Narrative_FIXED.txt")

# ----------------------------
# 2. Helpers
# ----------------------------

find_file_if_missing <- function(dir, must_contain, exclude = character()) {
  all_csvs <- list.files(dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  
  hits <- all_csvs
  
  for (term in must_contain) {
    hits <- hits[
      str_detect(str_to_lower(basename(hits)), fixed(str_to_lower(term)))
    ]
  }
  
  for (term in exclude) {
    hits <- hits[
      !str_detect(str_to_lower(basename(hits)), fixed(str_to_lower(term)))
    ]
  }
  
  if (length(hits) == 0) return(NA_character_)
  
  hits[which.max(file.info(hits)$mtime)]
}

read_qualtrics_clean <- function(path) {
  raw <- read_csv(path, show_col_types = FALSE, guess_max = 10000) %>%
    clean_names()
  
  first_col <- names(raw)[1]
  
  cleaned <- raw %>%
    filter(
      !(
        as.character(.data[[first_col]]) %in% c("Start Date", "End Date") |
          str_detect(as.character(.data[[first_col]]), fixed("{\"ImportId\"")) |
          str_detect(as.character(.data[[first_col]]), fixed("ImportId"))
      )
    )
  
  cleaned
}

find_col <- function(df, candidates, required = TRUE, label = "column") {
  nm <- names(df)
  
  exact <- candidates[candidates %in% nm]
  if (length(exact) > 0) return(exact[1])
  
  for (cand in candidates) {
    hits <- nm[str_detect(nm, fixed(cand))]
    if (length(hits) > 0) return(hits[1])
  }
  
  if (required) {
    stop(
      "Could not find ", label, ". Tried: ",
      paste(candidates, collapse = ", "),
      "\n\nAvailable columns:\n",
      paste(nm, collapse = "\n")
    )
  }
  
  NA_character_
}

clean_id <- function(x) {
  x <- str_trim(as.character(x))
  out <- str_extract(x, "\\d+")
  ifelse(is.na(out), x, out)
}

to_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

parse_date_flex <- function(x) {
  suppressWarnings(
    parse_date_time(
      x,
      orders = c(
        "ymd HMS",
        "ymd HM",
        "mdy HMS",
        "mdy HM",
        "dmy HMS",
        "dmy HM"
      ),
      tz = "Europe/London"
    )
  )
}

phq_to_num <- function(x) {
  x_chr <- str_to_lower(str_trim(as.character(x)))
  
  case_when(
    is.na(x_chr) | x_chr == "" ~ NA_real_,
    str_detect(x_chr, "^0$|not at all") ~ 0,
    str_detect(x_chr, "^1$|several days") ~ 1,
    str_detect(x_chr, "^2$|more than half") ~ 2,
    str_detect(x_chr, "^3$|nearly every day") ~ 3,
    TRUE ~ suppressWarnings(as.numeric(x_chr))
  )
}

bdi_to_num <- function(x) {
  x_chr <- str_trim(as.character(x))
  out <- suppressWarnings(as.numeric(x_chr))
  
  needs_parse <- is.na(out) & !is.na(x_chr) & x_chr != ""
  
  out[needs_parse] <- suppressWarnings(
    as.numeric(str_extract(x_chr[needs_parse], "^[0-3]"))
  )
  
  out
}

score_sum_from_df <- function(df, expected_cols, scorer, label = "scale") {
  present_cols <- intersect(expected_cols, names(df))
  missing_cols <- setdiff(expected_cols, names(df))
  
  if (length(missing_cols) > 0) {
    warning(
      "Missing ", label, " columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (length(present_cols) == 0) {
    return(rep(NA_real_, nrow(df)))
  }
  
  tmp <- df %>%
    select(all_of(present_cols)) %>%
    mutate(across(everything(), scorer))
  
  rowSums(tmp, na.rm = FALSE)
}

get_numeric_col <- function(df, colname) {
  if (is.na(colname) || !colname %in% names(df)) {
    return(rep(NA_real_, nrow(df)))
  }
  
  to_num(df[[colname]])
}

ci95 <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  
  if (n == 0) {
    return(tibble(
      n = 0,
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      max = NA_real_,
      ci95_low = NA_real_,
      ci95_high = NA_real_
    ))
  }
  
  m <- mean(x)
  s <- ifelse(n > 1, sd(x), NA_real_)
  se <- ifelse(n > 1, s / sqrt(n), NA_real_)
  crit <- ifelse(n > 1, qt(0.975, df = n - 1), NA_real_)
  
  tibble(
    n = n,
    mean = m,
    sd = s,
    min = min(x),
    max = max(x),
    ci95_low = ifelse(n > 1, m - crit * se, NA_real_),
    ci95_high = ifelse(n > 1, m + crit * se, NA_real_)
  )
}

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), NA_character_, sprintf(paste0("%.", digits, "f"), x))
}

fmt_ci <- function(lo, hi, digits = 2) {
  ifelse(
    is.na(lo) | is.na(hi),
    NA_character_,
    paste0("[", fmt_num(lo, digits), ", ", fmt_num(hi, digits), "]")
  )
}

paired_summary <- function(df, pre_col, post_col, label) {
  pre_col <- rlang::ensym(pre_col)
  post_col <- rlang::ensym(post_col)
  
  tmp <- df %>%
    transmute(
      part_id,
      pre = !!pre_col,
      post = !!post_col
    ) %>%
    filter(!is.na(pre), !is.na(post)) %>%
    mutate(change = post - pre)
  
  n <- nrow(tmp)
  
  if (n == 0) {
    return(tibble(
      measure = label,
      paired_n = 0,
      pre_mean = NA_real_,
      post_mean = NA_real_,
      mean_change = NA_real_,
      sd_change = NA_real_,
      change_min = NA_real_,
      change_max = NA_real_,
      change_ci95_low = NA_real_,
      change_ci95_high = NA_real_,
      paired_t = NA_real_,
      paired_p = NA_real_
    ))
  }
  
  ch <- tmp$change
  m <- mean(ch, na.rm = TRUE)
  s <- ifelse(n > 1, sd(ch, na.rm = TRUE), NA_real_)
  se <- ifelse(n > 1, s / sqrt(n), NA_real_)
  crit <- ifelse(n > 1, qt(0.975, df = n - 1), NA_real_)
  
  tt <- if (n > 1 && sd(ch, na.rm = TRUE) > 0) {
    t.test(tmp$post, tmp$pre, paired = TRUE)
  } else {
    NULL
  }
  
  tibble(
    measure = label,
    paired_n = n,
    pre_mean = mean(tmp$pre, na.rm = TRUE),
    post_mean = mean(tmp$post, na.rm = TRUE),
    mean_change = m,
    sd_change = s,
    change_min = min(ch, na.rm = TRUE),
    change_max = max(ch, na.rm = TRUE),
    change_ci95_low = ifelse(n > 1, m - crit * se, NA_real_),
    change_ci95_high = ifelse(n > 1, m + crit * se, NA_real_),
    paired_t = ifelse(is.null(tt), NA_real_, unname(tt$statistic)),
    paired_p = ifelse(is.null(tt), NA_real_, tt$p.value)
  )
}

# ----------------------------
# 3. Resolve files
# ----------------------------

if (!file.exists(START_FILE)) {
  message("Hard-coded START_FILE not found. Searching for raw WP1 Testing Start file...")
  
  START_FILE_FOUND <- find_file_if_missing(
    DATA_DIR,
    must_contain = c("wp1", "testing", "start"),
    exclude = c(
      "audit",
      "summary",
      "prepost",
      "descriptives",
      "paired",
      "narrative",
      "debugged",
      "fixed"
    )
  )
  
  if (is.na(START_FILE_FOUND)) {
    stop(
      "Could not find raw WP1 Testing Start file.\n",
      "Expected hard-coded path:\n",
      START_FILE,
      "\n\nFiles in DATA_DIR:\n",
      paste(list.files(DATA_DIR), collapse = "\n")
    )
  }
  
  START_FILE <- START_FILE_FOUND
}

if (!file.exists(END_FILE)) {
  message("Hard-coded END_FILE not found. Searching for raw WP1 Testing End file...")
  
  END_FILE_FOUND <- find_file_if_missing(
    DATA_DIR,
    must_contain = c("wp1", "testing", "end"),
    exclude = c(
      "audit",
      "summary",
      "prepost",
      "descriptives",
      "paired",
      "narrative",
      "debugged",
      "fixed"
    )
  )
  
  if (is.na(END_FILE_FOUND)) {
    stop(
      "Could not find raw WP1 Testing End file.\n",
      "Expected hard-coded path:\n",
      END_FILE,
      "\n\nFiles in DATA_DIR:\n",
      paste(list.files(DATA_DIR), collapse = "\n")
    )
  }
  
  END_FILE <- END_FILE_FOUND
}

message("Using START file: ", START_FILE)
message("Using END file:   ", END_FILE)

# ----------------------------
# 4. Load files
# ----------------------------

start_raw_unclean <- read_csv(START_FILE, show_col_types = FALSE, guess_max = 10000) %>%
  clean_names()

end_raw_unclean <- read_csv(END_FILE, show_col_types = FALSE, guess_max = 10000) %>%
  clean_names()

start_raw <- read_qualtrics_clean(START_FILE)
end_raw <- read_qualtrics_clean(END_FILE)

message("START raw rows before metadata removal: ", nrow(start_raw_unclean))
message("START rows after metadata removal:      ", nrow(start_raw))
message("END raw rows before metadata removal:   ", nrow(end_raw_unclean))
message("END rows after metadata removal:        ", nrow(end_raw))

# ----------------------------
# 5. Detect columns
# ----------------------------

id_candidates <- c(
  "part_id",
  "participant_id",
  "participant",
  "participant_number",
  "participant_no",
  "participant_num",
  "subject_id",
  "subject",
  "sub_id",
  "pid",
  "id",
  "response_id",
  "responseid"
)

date_candidates <- c(
  "recorded_date",
  "recordeddate",
  "end_date",
  "enddate",
  "start_date",
  "startdate",
  "date"
)

start_id_col <- find_col(
  start_raw,
  id_candidates,
  required = TRUE,
  label = "participant ID column in START file"
)

end_id_col <- find_col(
  end_raw,
  id_candidates,
  required = TRUE,
  label = "participant ID column in END file"
)

start_date_col <- find_col(
  start_raw,
  date_candidates,
  required = FALSE,
  label = "date column in START file"
)

end_date_col <- find_col(
  end_raw,
  date_candidates,
  required = FALSE,
  label = "date column in END file"
)

m3_mood_pre_col <- find_col(
  start_raw,
  c("m3vas_mood_1", "m3vas_mood", "mood_1", "mood"),
  required = FALSE,
  label = "pre M3VAS mood column"
)

m3_pleasure_pre_col <- find_col(
  start_raw,
  c("m3vas_pleasure_1", "m3vas_pleasure", "pleasure_1", "pleasure"),
  required = FALSE,
  label = "pre M3VAS pleasure column"
)

m3_suicidal_pre_col <- find_col(
  start_raw,
  c(
    "m3vas_suicidal_1",
    "m3vas_suicidality_1",
    "m3vas_suicidal",
    "m3vas_suicidality",
    "suicidal_1",
    "suicidality_1",
    "suicidal",
    "suicidality"
  ),
  required = FALSE,
  label = "pre M3VAS suicidality column"
)

m3_mood_change_col <- find_col(
  end_raw,
  c(
    "m3vas_ch_mood_1",
    "m3vas_change_mood_1",
    "m3vas_ch_mood",
    "m3vas_change_mood",
    "mood_change",
    "change_mood"
  ),
  required = FALSE,
  label = "post M3VAS mood-change column"
)

m3_pleasure_change_col <- find_col(
  end_raw,
  c(
    "m3vas_ch_pleasure_1",
    "m3vas_change_pleasure_1",
    "m3vas_ch_pleasure",
    "m3vas_change_pleasure",
    "pleasure_change",
    "change_pleasure"
  ),
  required = FALSE,
  label = "post M3VAS pleasure-change column"
)

m3_suicidal_change_col <- find_col(
  end_raw,
  c(
    "m3vas_ch_suicidal_1",
    "m3vas_ch_suicidality_1",
    "m3vas_change_suicidal_1",
    "m3vas_change_suicidality_1",
    "m3vas_ch_suicidal",
    "m3vas_ch_suicidality",
    "suicidal_change",
    "suicidality_change",
    "change_suicidal",
    "change_suicidality"
  ),
  required = FALSE,
  label = "post M3VAS suicidality-change column"
)

message("")
message("Detected columns:")
message("  START ID:              ", start_id_col)
message("  END ID:                ", end_id_col)
message("  START date:            ", ifelse(is.na(start_date_col), "NOT FOUND", start_date_col))
message("  END date:              ", ifelse(is.na(end_date_col), "NOT FOUND", end_date_col))
message("  Pre mood:              ", ifelse(is.na(m3_mood_pre_col), "NOT FOUND", m3_mood_pre_col))
message("  Pre pleasure:          ", ifelse(is.na(m3_pleasure_pre_col), "NOT FOUND", m3_pleasure_pre_col))
message("  Pre suicidality:       ", ifelse(is.na(m3_suicidal_pre_col), "NOT FOUND", m3_suicidal_pre_col))
message("  Change mood:           ", ifelse(is.na(m3_mood_change_col), "NOT FOUND", m3_mood_change_col))
message("  Change pleasure:       ", ifelse(is.na(m3_pleasure_change_col), "NOT FOUND", m3_pleasure_change_col))
message("  Change suicidality:    ", ifelse(is.na(m3_suicidal_change_col), "NOT FOUND", m3_suicidal_change_col))
message("")

# ----------------------------
# 6. Score start/pre data
# ----------------------------

phq_cols_start <- paste0("phq9_", 1:9)
bdi_cols_start <- paste0("bdi_", 1:21)

start_scored <- tibble(
  part_id = clean_id(start_raw[[start_id_col]]),
  recorded_date_internal = if (!is.na(start_date_col)) {
    parse_date_flex(start_raw[[start_date_col]])
  } else {
    as.POSIXct(rep(NA, nrow(start_raw)), origin = "1970-01-01", tz = "Europe/London")
  },
  phq9_pre = score_sum_from_df(start_raw, phq_cols_start, phq_to_num, "PHQ-9 start"),
  m3vas_mood_pre = get_numeric_col(start_raw, m3_mood_pre_col),
  m3vas_pleasure_pre = get_numeric_col(start_raw, m3_pleasure_pre_col),
  m3vas_suicidality_pre = get_numeric_col(start_raw, m3_suicidal_pre_col),
  bdi2_pre = score_sum_from_df(start_raw, bdi_cols_start, bdi_to_num, "BDI-II start")
) %>%
  filter(!is.na(part_id), part_id != "") %>%
  arrange(part_id, desc(recorded_date_internal)) %>%
  group_by(part_id) %>%
  slice(1) %>%
  ungroup()

# ----------------------------
# 7. Score end/post data
# ----------------------------

phq_cols_end <- paste0("phq9_", 1:9)
bdi_cols_end <- paste0("bdi_", 1:21)

end_has_bdi <- all(bdi_cols_end %in% names(end_raw))

end_scored <- tibble(
  part_id = clean_id(end_raw[[end_id_col]]),
  recorded_date_internal = if (!is.na(end_date_col)) {
    parse_date_flex(end_raw[[end_date_col]])
  } else {
    as.POSIXct(rep(NA, nrow(end_raw)), origin = "1970-01-01", tz = "Europe/London")
  },
  phq9_post = score_sum_from_df(end_raw, phq_cols_end, phq_to_num, "PHQ-9 end"),
  m3vas_mood_change = get_numeric_col(end_raw, m3_mood_change_col),
  m3vas_pleasure_change = get_numeric_col(end_raw, m3_pleasure_change_col),
  m3vas_suicidality_change = get_numeric_col(end_raw, m3_suicidal_change_col),
  bdi2_post = if (end_has_bdi) {
    score_sum_from_df(end_raw, bdi_cols_end, bdi_to_num, "BDI-II end")
  } else {
    rep(NA_real_, nrow(end_raw))
  }
) %>%
  filter(!is.na(part_id), part_id != "") %>%
  arrange(part_id, desc(recorded_date_internal)) %>%
  group_by(part_id) %>%
  slice(1) %>%
  ungroup()

# ----------------------------
# 8. Merge pre/post and compute estimated M3VAS post
# ----------------------------

paired <- start_scored %>%
  full_join(end_scored, by = "part_id") %>%
  mutate(
    m3vas_mood_post_est = ifelse(
      !is.na(m3vas_mood_pre) & !is.na(m3vas_mood_change),
      m3vas_mood_pre - m3vas_mood_change,
      NA_real_
    ),
    m3vas_pleasure_post_est = ifelse(
      !is.na(m3vas_pleasure_pre) & !is.na(m3vas_pleasure_change),
      m3vas_pleasure_pre - m3vas_pleasure_change,
      NA_real_
    ),
    m3vas_suicidality_post_est = ifelse(
      !is.na(m3vas_suicidality_pre) & !is.na(m3vas_suicidality_change),
      m3vas_suicidality_pre - m3vas_suicidality_change,
      NA_real_
    )
  )

id_overlap <- tibble(
  category = c(
    "Start unique IDs",
    "End unique IDs",
    "Overlapping IDs",
    "Start-only IDs",
    "End-only IDs"
  ),
  n = c(
    n_distinct(start_scored$part_id),
    n_distinct(end_scored$part_id),
    length(intersect(start_scored$part_id, end_scored$part_id)),
    length(setdiff(start_scored$part_id, end_scored$part_id)),
    length(setdiff(end_scored$part_id, start_scored$part_id))
  )
)

message("")
message("Participant counts:")
message("  Start scored rows: ", nrow(start_scored))
message("  End scored rows:   ", nrow(end_scored))
message("  Merged rows:       ", nrow(paired))
message("  Overlapping IDs:   ", length(intersect(start_scored$part_id, end_scored$part_id)))
message("")

# ----------------------------
# 9. Descriptive table
# ----------------------------

desc_long <- paired %>%
  select(
    part_id,
    
    phq9_pre,
    phq9_post,
    
    m3vas_mood_pre,
    m3vas_mood_change,
    m3vas_mood_post_est,
    
    m3vas_pleasure_pre,
    m3vas_pleasure_change,
    m3vas_pleasure_post_est,
    
    m3vas_suicidality_pre,
    m3vas_suicidality_change,
    m3vas_suicidality_post_est,
    
    bdi2_pre,
    bdi2_post
  ) %>%
  pivot_longer(
    cols = -part_id,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    measure = case_when(
      str_detect(variable, "^phq9") ~ "PHQ-9",
      str_detect(variable, "^m3vas_mood") ~ "M3VAS mood/depression",
      str_detect(variable, "^m3vas_pleasure") ~ "M3VAS pleasure/anhedonia",
      str_detect(variable, "^m3vas_suicidality") ~ "M3VAS suicidality",
      str_detect(variable, "^bdi2") ~ "BDI-II",
      TRUE ~ variable
    ),
    timepoint = case_when(
      str_detect(variable, "pre$") ~ "Pre",
      str_detect(variable, "post$|post_est$") ~ "Post / estimated post",
      str_detect(variable, "change$") ~ "Post-stim change rating",
      TRUE ~ variable
    ),
    note = case_when(
      str_detect(variable, "post_est$") ~ "Estimated post score from pre minus change rating",
      str_detect(variable, "change$") ~ "Raw end-of-session change rating",
      variable == "bdi2_post" & all(is.na(value)) ~ "No post BDI-II columns detected in end file",
      TRUE ~ ""
    )
  )

desc_table <- desc_long %>%
  group_by(measure, timepoint, note) %>%
  summarise(ci95(value), .groups = "drop") %>%
  mutate(
    mean_sd = paste0(fmt_num(mean), " ± ", fmt_num(sd)),
    range = paste0(fmt_num(min), " to ", fmt_num(max)),
    ci95 = fmt_ci(ci95_low, ci95_high)
  ) %>%
  select(
    measure,
    timepoint,
    note,
    n,
    mean,
    sd,
    min,
    max,
    ci95_low,
    ci95_high,
    mean_sd,
    range,
    ci95
  ) %>%
  arrange(
    factor(
      measure,
      levels = c(
        "PHQ-9",
        "M3VAS mood/depression",
        "M3VAS pleasure/anhedonia",
        "M3VAS suicidality",
        "BDI-II"
      )
    ),
    factor(
      timepoint,
      levels = c(
        "Pre",
        "Post / estimated post",
        "Post-stim change rating"
      )
    )
  )

# ----------------------------
# 10. Paired pre/post change table
# ----------------------------

change_table <- bind_rows(
  paired_summary(
    paired,
    phq9_pre,
    phq9_post,
    "PHQ-9"
  ),
  paired_summary(
    paired,
    m3vas_mood_pre,
    m3vas_mood_post_est,
    "M3VAS mood/depression estimated post"
  ),
  paired_summary(
    paired,
    m3vas_pleasure_pre,
    m3vas_pleasure_post_est,
    "M3VAS pleasure/anhedonia estimated post"
  ),
  paired_summary(
    paired,
    m3vas_suicidality_pre,
    m3vas_suicidality_post_est,
    "M3VAS suicidality estimated post"
  ),
  paired_summary(
    paired,
    bdi2_pre,
    bdi2_post,
    "BDI-II"
  )
) %>%
  mutate(
    change_ci95 = fmt_ci(change_ci95_low, change_ci95_high),
    pre_mean = round(pre_mean, 2),
    post_mean = round(post_mean, 2),
    mean_change = round(mean_change, 2),
    sd_change = round(sd_change, 2),
    change_min = round(change_min, 2),
    change_max = round(change_max, 2),
    paired_t = round(paired_t, 3),
    paired_p = ifelse(is.na(paired_p), NA_character_, sprintf("%.4f", paired_p))
  )

# ----------------------------
# 11. Debug tables
# ----------------------------

detected_cols <- tibble(
  item = c(
    "Start file",
    "End file",
    "Start rows before metadata removal",
    "Start rows after metadata removal",
    "End rows before metadata removal",
    "End rows after metadata removal",
    "Start ID column",
    "End ID column",
    "Start date column",
    "End date column",
    "Pre M3VAS mood",
    "Pre M3VAS pleasure",
    "Pre M3VAS suicidality",
    "End M3VAS mood change",
    "End M3VAS pleasure change",
    "End M3VAS suicidality change",
    "End has BDI-II columns"
  ),
  value = c(
    basename(START_FILE),
    basename(END_FILE),
    as.character(nrow(start_raw_unclean)),
    as.character(nrow(start_raw)),
    as.character(nrow(end_raw_unclean)),
    as.character(nrow(end_raw)),
    start_id_col,
    end_id_col,
    ifelse(is.na(start_date_col), "Not found", start_date_col),
    ifelse(is.na(end_date_col), "Not found", end_date_col),
    ifelse(is.na(m3_mood_pre_col), "Not found", m3_mood_pre_col),
    ifelse(is.na(m3_pleasure_pre_col), "Not found", m3_pleasure_pre_col),
    ifelse(is.na(m3_suicidal_pre_col), "Not found", m3_suicidal_pre_col),
    ifelse(is.na(m3_mood_change_col), "Not found", m3_mood_change_col),
    ifelse(is.na(m3_pleasure_change_col), "Not found", m3_pleasure_change_col),
    ifelse(is.na(m3_suicidal_change_col), "Not found", m3_suicidal_change_col),
    as.character(end_has_bdi)
  )
)

id_debug <- bind_rows(
  start_scored %>%
    transmute(
      source = "Start",
      part_id,
      phq9 = phq9_pre,
      m3vas_mood = m3vas_mood_pre,
      bdi2 = bdi2_pre
    ) %>%
    slice_head(n = 20),
  end_scored %>%
    transmute(
      source = "End",
      part_id,
      phq9 = phq9_post,
      m3vas_mood = m3vas_mood_change,
      bdi2 = bdi2_post
    ) %>%
    slice_head(n = 20)
)

id_mismatch <- tibble(
  part_id = c(
    setdiff(start_scored$part_id, end_scored$part_id),
    setdiff(end_scored$part_id, start_scored$part_id)
  ),
  status = c(
    rep("Start only", length(setdiff(start_scored$part_id, end_scored$part_id))),
    rep("End only", length(setdiff(end_scored$part_id, start_scored$part_id)))
  )
)

column_debug <- bind_rows(
  tibble(source = "Start", column = names(start_raw)),
  tibble(source = "End", column = names(end_raw))
)

metadata_debug <- bind_rows(
  start_raw_unclean %>%
    slice_head(n = 5) %>%
    mutate(source = "Start unclean") %>%
    select(source, everything()),
  end_raw_unclean %>%
    slice_head(n = 5) %>%
    mutate(source = "End unclean") %>%
    select(source, everything())
)

# ----------------------------
# 12. Exports
# ----------------------------

write_csv(desc_table, OUT_DESC_CSV)
write_csv(change_table, OUT_CHANGE_CSV)

wb <- createWorkbook()

addWorksheet(wb, "Descriptives")
writeData(wb, "Descriptives", desc_table)

addWorksheet(wb, "Paired change")
writeData(wb, "Paired change", change_table)

addWorksheet(wb, "Participant paired data")
writeData(wb, "Participant paired data", paired)

addWorksheet(wb, "Start scored")
writeData(wb, "Start scored", start_scored)

addWorksheet(wb, "End scored")
writeData(wb, "End scored", end_scored)

addWorksheet(wb, "ID overlap")
writeData(wb, "ID overlap", id_overlap)

addWorksheet(wb, "ID mismatch")
writeData(wb, "ID mismatch", id_mismatch)

addWorksheet(wb, "ID debug first rows")
writeData(wb, "ID debug first rows", id_debug)

addWorksheet(wb, "Detected columns")
writeData(wb, "Detected columns", detected_cols)

addWorksheet(wb, "Column names")
writeData(wb, "Column names", column_debug)

addWorksheet(wb, "Metadata debug first rows")
writeData(wb, "Metadata debug first rows", metadata_debug)

addWorksheet(wb, "Readme")
readme <- tibble(
  item = c(
    "PHQ-9 scoring",
    "M3VAS post handling",
    "BDI-II post handling",
    "Participant ID handling",
    "Critical bug fixed"
  ),
  value = c(
    "PHQ-9 items scored 0-3 and summed.",
    "The WP1 Testing End file contains M3VAS change ratings rather than direct raw post VAS ratings. Estimated post scores are computed as pre minus change, assuming positive change indicates improvement on symptom-burden scales.",
    ifelse(
      end_has_bdi,
      "Post BDI-II columns were detected and scored.",
      "No post BDI-II columns were detected in the end file, so BDI-II is summarised as pre only and paired BDI-II change is unavailable."
    ),
    "The script reads part_id from both Start and End files and reports overlap/mismatch in separate sheets.",
    "Previous scripts removed rows if any cell contained 'IP Address'. Real participant rows had Status == 'IP Address', so all participants were deleted. This version removes metadata rows only by inspecting the first column."
  )
)
writeData(wb, "Readme", readme)

for (s in names(wb)) {
  setColWidths(wb, s, cols = 1:120, widths = "auto")
  freezePane(wb, s, firstRow = TRUE)
}

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)

message("Wrote Excel workbook: ", OUT_XLSX)
message("Wrote descriptives CSV: ", OUT_DESC_CSV)
message("Wrote paired change CSV: ", OUT_CHANGE_CSV)

# ----------------------------
# 13. Narrative output
# ----------------------------

safe_first_row <- function(tbl, measure_name) {
  out <- tbl %>% filter(measure == measure_name)
  
  if (nrow(out) == 0) {
    return(tibble(
      measure = measure_name,
      paired_n = 0,
      pre_mean = NA_real_,
      post_mean = NA_real_,
      mean_change = NA_real_,
      change_ci95 = NA_character_,
      paired_t = NA_real_,
      paired_p = NA_character_
    ))
  }
  
  out %>% slice(1)
}

phq_row <- safe_first_row(change_table, "PHQ-9")
mood_row <- safe_first_row(change_table, "M3VAS mood/depression estimated post")
pleasure_row <- safe_first_row(change_table, "M3VAS pleasure/anhedonia estimated post")
suic_row <- safe_first_row(change_table, "M3VAS suicidality estimated post")
bdi_row <- safe_first_row(change_table, "BDI-II")

narrative <- c(
  "WP1 pre/post stimulation mood-check summaries",
  "",
  glue(
    "After removing Qualtrics metadata rows, the Testing Start file contained {nrow(start_raw)} participant rows and the Testing End file contained {nrow(end_raw)} participant rows. ",
    "There were {length(intersect(start_scored$part_id, end_scored$part_id))} overlapping participant IDs."
  ),
  "",
  glue(
    "PHQ-9 scores were available for {phq_row$paired_n} paired participants. ",
    "Mean PHQ-9 changed from {phq_row$pre_mean} pre-stimulation to {phq_row$post_mean} post-stimulation, ",
    "corresponding to a mean paired change of {phq_row$mean_change} points ",
    "(95% CI {phq_row$change_ci95}; paired t = {phq_row$paired_t}, p = {phq_row$paired_p})."
  ),
  glue(
    "For M3VAS mood/depression, estimated post-stimulation scores were derived from the end-session change rating. ",
    "Among {mood_row$paired_n} paired participants, the mean estimated change was {mood_row$mean_change} ",
    "(95% CI {mood_row$change_ci95})."
  ),
  glue(
    "For M3VAS pleasure/anhedonia, estimated post-stimulation scores were derived from the end-session change rating. ",
    "Among {pleasure_row$paired_n} paired participants, the mean estimated change was {pleasure_row$mean_change} ",
    "(95% CI {pleasure_row$change_ci95})."
  ),
  glue(
    "For M3VAS suicidality, estimated post-stimulation scores were derived from the end-session change rating. ",
    "Among {suic_row$paired_n} paired participants, the mean estimated change was {suic_row$mean_change} ",
    "(95% CI {suic_row$change_ci95})."
  ),
  if (is.na(bdi_row$paired_n) || bdi_row$paired_n == 0) {
    "BDI-II was available at pre-stimulation only in the detected files; no post-stimulation BDI-II columns were detected in the WP1 Testing End file, so paired BDI-II change was not computed."
  } else {
    glue(
      "BDI-II scores were available for {bdi_row$paired_n} paired participants. ",
      "Mean BDI-II changed from {bdi_row$pre_mean} pre-stimulation to {bdi_row$post_mean} post-stimulation, ",
      "corresponding to a mean paired change of {bdi_row$mean_change} points ",
      "(95% CI {bdi_row$change_ci95}; paired t = {bdi_row$paired_t}, p = {bdi_row$paired_p})."
    )
  },
  "",
  "Note: The WP1 Testing End file contains M3VAS change-rating fields rather than raw post-stimulation VAS fields. Therefore, the script reports both raw change ratings and estimated post-stimulation M3VAS scores computed as pre minus change, assuming positive change indicates improvement."
)

writeLines(narrative, OUT_NARRATIVE_TXT)

message("Wrote narrative: ", OUT_NARRATIVE_TXT)

# ----------------------------
# 14. Console output
# ----------------------------

message("")
message("Matched files used:")
message("Start: ", START_FILE)
message("End:   ", END_FILE)
message("")
message("Rows after metadata removal:")
message("Start rows: ", nrow(start_raw))
message("End rows:   ", nrow(end_raw))
message("")
message("Participant overlap:")
print(id_overlap)
message("")

desc_table
change_table

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

# ===================================================

# ============================================================
# TABLE S3.3 — WP1 BASELINE PHQ-9 AND M3VAS DESCRIPTIVES
# + PRE/POST MOOD CHECK DEBUG TABLES
# ============================================================
#
# Coding rules:
#
# PHQ-9:
#   Higher = worse.
#   post - pre < 0 means improvement.
#
# M3VAS baseline:
#   0 = bad, 100 = good.
#
# M3VAS_CHANGE:
#   -50 = worse, +50 = better.
#
# Therefore:
#   M3VAS estimated post = baseline + change_rating
#   post - pre > 0 means improvement.
#
# Outputs:
#   1. Excel workbook:
#        Table_S3_3_WP1_Baseline_and_PrePost_Mood_Debug.xlsx
#   2. Baseline Table S3.3 CSV
#   3. Pre/post descriptive CSV
#   4. Paired-change CSV
#   5. Narrative text file
#
# ============================================================

# install.packages(c("tidyverse", "lubridate", "writexl"))

library(tidyverse)
library(lubridate)
library(writexl)

# ============================================================
# 0. USER SETTINGS
# ============================================================

DATA_DIR <- Sys.getenv("MRC_DATA_DIR", unset = "C:/Users/dn284/Desktop/MRC_omni/data")

OUT_DIR <- file.path(DATA_DIR, "supplementary_S3_3_wp1_baseline_mood")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

OUT_XLSX <- file.path(
  OUT_DIR,
  "Table_S3_3_WP1_Baseline_and_PrePost_Mood_Debug.xlsx"
)

OUT_BASELINE_CSV <- file.path(
  OUT_DIR,
  "Table_S3_3_WP1_Baseline_PHQ9_M3VAS.csv"
)

OUT_PREPOST_CSV <- file.path(
  OUT_DIR,
  "WP1_PrePost_PHQ9_M3VAS_Comparison.csv"
)

OUT_PAIRED_CSV <- file.path(
  OUT_DIR,
  "WP1_PrePost_PHQ9_M3VAS_Paired_Change.csv"
)

OUT_NARRATIVE <- file.path(
  OUT_DIR,
  "WP1_Table_S3_3_and_PrePost_Narrative.txt"
)

MIN_VALID_WP1_SESSIONS_FOR_ANALYSED <- 3

# ============================================================
# 1. HELPER FUNCTIONS
# ============================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

clean_names_simple <- function(x) {
  x %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace_all("^_|_$", "") %>%
    stringr::str_to_lower()
}

standardise_names <- function(df) {
  old <- names(df)
  new <- clean_names_simple(old)
  names(df) <- make.unique(new, sep = "_dup")
  attr(df, "name_map") <- tibble(
    original_name = old,
    clean_name = names(df)
  )
  df
}

clean_id <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    na_if("") %>%
    na_if("nan") %>%
    na_if("NaN") %>%
    na_if("None") %>%
    stringr::str_extract("\\d{3,6}")
}

num_clean <- function(x) {
  suppressWarnings(
    as.numeric(
      stringr::str_replace_all(as.character(x), "[^0-9\\.\\-]", "")
    )
  )
}

read_qualtrics_clean <- function(path) {
  raw <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
  
  names(raw) <- stringr::str_trim(names(raw))
  
  # Drop Qualtrics label/import metadata rows.
  # These are usually first two rows:
  #   row 1: "Start Date", "End Date", ...
  #   row 2: {"ImportId":...}
  if (nrow(raw) > 0) {
    first_col <- as.character(raw[[1]])
    
    metadata_like <- stringr::str_detect(
      first_col,
      stringr::regex(
        "^Start Date$|^End Date$|^\\{\"ImportId\"",
        ignore_case = TRUE
      )
    )
    
    raw <- raw[!metadata_like, , drop = FALSE]
  }
  
  raw %>% standardise_names()
}

find_existing_col <- function(df, candidates, label = "column", required = TRUE) {
  candidates_clean <- clean_names_simple(candidates)
  
  # exact match
  hit <- candidates_clean[candidates_clean %in% names(df)]
  if (length(hit) > 0) return(hit[1])
  
  # partial match
  for (cand in candidates_clean) {
    hit2 <- names(df)[stringr::str_detect(names(df), fixed(cand))]
    if (length(hit2) > 0) return(hit2[1])
  }
  
  if (required) {
    cat("\nAvailable columns for ", label, ":\n", sep = "")
    print(names(df))
    stop(
      "Could not find ", label, ". Tried: ",
      paste(candidates_clean, collapse = ", ")
    )
  }
  
  NA_character_
}

parse_date_flex <- function(x) {
  suppressWarnings(
    lubridate::parse_date_time(
      x,
      orders = c(
        "ymd HMS", "ymd HM", "ymd",
        "dmy HMS", "dmy HM", "dmy",
        "mdy HMS", "mdy HM", "mdy"
      ),
      tz = "Europe/London"
    )
  )
}

phq_to_num <- function(x) {
  xx <- stringr::str_to_lower(stringr::str_trim(as.character(x)))
  
  dplyr::case_when(
    xx %in% c("0", "not at all") ~ 0,
    xx %in% c("1", "several days") ~ 1,
    xx %in% c("2", "more than half the days") ~ 2,
    xx %in% c("3", "nearly every day") ~ 3,
    TRUE ~ suppressWarnings(as.numeric(xx))
  )
}

score_scale_sum <- function(df, cols, parser, label = "scale", require_all = TRUE) {
  missing_cols <- setdiff(cols, names(df))
  
  if (length(missing_cols) > 0) {
    warning(
      label,
      ": missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
    return(rep(NA_real_, nrow(df)))
  }
  
  mat <- df %>%
    select(all_of(cols)) %>%
    mutate(across(everything(), parser)) %>%
    as.data.frame()
  
  n_answered <- rowSums(!is.na(mat))
  total <- rowSums(mat, na.rm = TRUE)
  
  if (require_all) {
    total[n_answered < length(cols)] <- NA_real_
  } else {
    total[n_answered == 0] <- NA_real_
  }
  
  total
}

ci_summary <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  
  if (n == 0) {
    return(tibble(
      n = 0,
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      max = NA_real_,
      ci95_low = NA_real_,
      ci95_high = NA_real_
    ))
  }
  
  m <- mean(x)
  s <- if (n > 1) sd(x) else NA_real_
  se <- if (n > 1) s / sqrt(n) else NA_real_
  tcrit <- if (n > 1) qt(0.975, df = n - 1) else NA_real_
  
  tibble(
    n = n,
    mean = m,
    sd = s,
    min = min(x),
    max = max(x),
    ci95_low = if (n > 1) m - tcrit * se else NA_real_,
    ci95_high = if (n > 1) m + tcrit * se else NA_real_
  )
}

fmt_num <- function(x, digits = 2) {
  ifelse(
    is.na(x),
    NA_character_,
    sprintf(paste0("%.", digits, "f"), x)
  )
}

fmt_ci <- function(lo, hi, digits = 2) {
  ifelse(
    is.na(lo) | is.na(hi),
    NA_character_,
    paste0("[", fmt_num(lo, digits), ", ", fmt_num(hi, digits), "]")
  )
}

fmt_p <- function(p) {
  ifelse(
    is.na(p),
    NA_character_,
    ifelse(p < 0.001, "<.001", sprintf("%.4f", p))
  )
}

make_plain_df <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  x[] <- lapply(x, function(col) {
    if (is.list(col)) as.character(col) else col
  })
  x
}

paired_summary <- function(df, pre_col, post_col, outcome, direction) {
  if (!(pre_col %in% names(df)) || !(post_col %in% names(df))) {
    return(tibble(
      outcome = outcome,
      direction = direction,
      paired_n = 0,
      pre_mean = NA_real_,
      post_mean = NA_real_,
      mean_change_post_minus_pre = NA_real_,
      sd_change = NA_real_,
      change_ci95_low = NA_real_,
      change_ci95_high = NA_real_,
      paired_t = NA_real_,
      paired_p = NA_real_
    ))
  }
  
  dat <- df %>%
    select(part_id, pre = all_of(pre_col), post = all_of(post_col)) %>%
    filter(!is.na(pre), !is.na(post)) %>%
    mutate(change = post - pre)
  
  n <- nrow(dat)
  
  if (n == 0) {
    return(tibble(
      outcome = outcome,
      direction = direction,
      paired_n = 0,
      pre_mean = NA_real_,
      post_mean = NA_real_,
      mean_change_post_minus_pre = NA_real_,
      sd_change = NA_real_,
      change_ci95_low = NA_real_,
      change_ci95_high = NA_real_,
      paired_t = NA_real_,
      paired_p = NA_real_
    ))
  }
  
  ch <- ci_summary(dat$change)
  
  tt <- if (n > 1) {
    tryCatch(
      t.test(dat$post, dat$pre, paired = TRUE),
      error = function(e) NULL
    )
  } else {
    NULL
  }
  
  tibble(
    outcome = outcome,
    direction = direction,
    paired_n = n,
    pre_mean = mean(dat$pre, na.rm = TRUE),
    post_mean = mean(dat$post, na.rm = TRUE),
    mean_change_post_minus_pre = ch$mean,
    sd_change = ch$sd,
    change_ci95_low = ch$ci95_low,
    change_ci95_high = ch$ci95_high,
    paired_t = if (!is.null(tt)) unname(tt$statistic) else NA_real_,
    paired_p = if (!is.null(tt)) tt$p.value else NA_real_
  )
}

resolve_file <- function(primary, fallback_regex = NULL) {
  if (file.exists(primary)) return(primary)
  
  if (!is.null(fallback_regex)) {
    hits <- list.files(
      DATA_DIR,
      pattern = fallback_regex,
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    hits <- hits[
      !stringr::str_detect(
        basename(hits),
        regex(
          "Audit|Denominator|Table_S3|supplementary|Narrative|PrePost|Baseline|Debug|output",
          ignore_case = TRUE
        )
      )
    ]
    
    if (length(hits) > 0) {
      info <- file.info(hits)
      return(hits[order(info$mtime, decreasing = TRUE)][1])
    }
  }
  
  stop("Could not find file: ", primary)
}

# ============================================================
# 2. FORCE CORRECT RAW FILES
# ============================================================

START_FILE <- resolve_file(
  file.path(DATA_DIR, "WP1 Testing Start_February 17, 2026_09.54.csv"),
  "^WP1 Testing Start_February 17, 2026_09\\.54.*\\.csv$"
)

END_FILE <- resolve_file(
  file.path(DATA_DIR, "WP1+Testing+End_March+3,+2025_02.36.csv"),
  "^WP1\\+Testing\\+End_March\\+3,\\+2025_02\\.36.*\\.csv$"
)

FEEDBACK_FILE <- resolve_file(
  file.path(DATA_DIR, "WP1 Session Feedback_February 17, 2026_09.54.csv"),
  "^WP1 Session Feedback_February 17, 2026_09\\.54.*\\.csv$"
)

message("Start file:    ", basename(START_FILE))
message("End file:      ", basename(END_FILE))
message("Feedback file: ", basename(FEEDBACK_FILE))

# Fresh load: do not reuse old objects in the R environment.
start_raw_unclean <- readr::read_csv(
  START_FILE,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

end_raw_unclean <- readr::read_csv(
  END_FILE,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

feedback_raw_unclean <- readr::read_csv(
  FEEDBACK_FILE,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

start_raw <- read_qualtrics_clean(START_FILE)
end_raw <- read_qualtrics_clean(END_FILE)
feedback_raw <- read_qualtrics_clean(FEEDBACK_FILE)

cat("\nTesting Start columns after fresh load:\n")
print(names(start_raw))

cat("\nTesting End columns after fresh load:\n")
print(names(end_raw))

cat("\nSession Feedback columns after fresh load:\n")
print(names(feedback_raw))

# ============================================================
# 3. DETECT KNOWN COLUMNS
# ============================================================

start_id_col <- find_existing_col(
  start_raw,
  c("part_id", "participant_id", "pid", "participant"),
  label = "Testing Start participant ID"
)

end_id_col <- find_existing_col(
  end_raw,
  c("part_id", "participant_id", "pid", "participant"),
  label = "Testing End participant ID"
)

feedback_id_col <- find_existing_col(
  feedback_raw,
  c("participant_id", "part_id", "pid", "participant"),
  label = "Session Feedback participant ID"
)

start_date_col <- find_existing_col(
  start_raw,
  c("recorded_date", "recordeddate", "end_date", "start_date"),
  label = "Testing Start date",
  required = FALSE
)

end_date_col <- find_existing_col(
  end_raw,
  c("recorded_date", "recordeddate", "end_date", "start_date"),
  label = "Testing End date",
  required = FALSE
)

feedback_session_col <- find_existing_col(
  feedback_raw,
  c("session_n", "session", "session_number"),
  label = "Session Feedback session number"
)

feedback_discomfort_col <- find_existing_col(
  feedback_raw,
  c("discomfortscore", "discomfort_score", "tol_score", "vdq", "vdq_score"),
  label = "Session Feedback discomfort score"
)

# PHQ-9 columns
phq_cols_start <- paste0("phq9_", 1:9)
if (!all(phq_cols_start %in% names(start_raw))) {
  phq_cols_start_alt <- paste0("phq_", 1:9)
  if (all(phq_cols_start_alt %in% names(start_raw))) {
    phq_cols_start <- phq_cols_start_alt
  }
}

phq_cols_end <- paste0("phq9_", 1:9)
if (!all(phq_cols_end %in% names(end_raw))) {
  phq_cols_end_alt <- paste0("phq_", 1:9)
  if (all(phq_cols_end_alt %in% names(end_raw))) {
    phq_cols_end <- phq_cols_end_alt
  }
}

# M3VAS pre columns
m3_mood_pre_col <- find_existing_col(
  start_raw,
  c("m3vas_mood_1", "m3vas_mood", "mood"),
  label = "pre M3VAS mood/depression",
  required = FALSE
)

m3_pleasure_pre_col <- find_existing_col(
  start_raw,
  c("m3vas_pleasure_1", "m3vas_pleasure", "pleasure"),
  label = "pre M3VAS pleasure/anhedonia",
  required = FALSE
)

m3_suicidality_pre_col <- find_existing_col(
  start_raw,
  c(
    "m3vas_suicidal_1",
    "m3vas_suicidality_1",
    "m3vas_suicidal",
    "m3vas_suicidality",
    "suicidal",
    "suicidality"
  ),
  label = "pre M3VAS suicidality",
  required = FALSE
)

# M3VAS change columns
m3_mood_change_col <- find_existing_col(
  end_raw,
  c("m3vas_ch_mood_1", "m3vas_change_mood_1", "m3vas_ch_mood", "mood_change"),
  label = "end M3VAS mood/depression change",
  required = FALSE
)

m3_pleasure_change_col <- find_existing_col(
  end_raw,
  c("m3vas_ch_pleasure_1", "m3vas_change_pleasure_1", "m3vas_ch_pleasure", "pleasure_change"),
  label = "end M3VAS pleasure/anhedonia change",
  required = FALSE
)

m3_suicidality_change_col <- find_existing_col(
  end_raw,
  c(
    "m3vas_ch_suicidal_1",
    "m3vas_ch_suicidality_1",
    "m3vas_change_suicidal_1",
    "m3vas_change_suicidality_1",
    "m3vas_ch_suicidal",
    "m3vas_ch_suicidality",
    "suicidal_change",
    "suicidality_change"
  ),
  label = "end M3VAS suicidality change",
  required = FALSE
)

detected_columns <- tibble(
  item = c(
    "Start file",
    "End file",
    "Feedback file",
    "Start ID column",
    "End ID column",
    "Feedback ID column",
    "Feedback session column",
    "Feedback discomfort column",
    "Start date column",
    "End date column",
    "PHQ-9 start columns",
    "PHQ-9 end columns",
    "Pre M3VAS mood/depression",
    "Pre M3VAS pleasure/anhedonia",
    "Pre M3VAS suicidality",
    "End M3VAS mood/depression change",
    "End M3VAS pleasure/anhedonia change",
    "End M3VAS suicidality change"
  ),
  value = c(
    basename(START_FILE),
    basename(END_FILE),
    basename(FEEDBACK_FILE),
    start_id_col,
    end_id_col,
    feedback_id_col,
    feedback_session_col,
    feedback_discomfort_col,
    ifelse(is.na(start_date_col), "Not found", start_date_col),
    ifelse(is.na(end_date_col), "Not found", end_date_col),
    paste(phq_cols_start, collapse = ", "),
    paste(phq_cols_end, collapse = ", "),
    ifelse(is.na(m3_mood_pre_col), "Not found", m3_mood_pre_col),
    ifelse(is.na(m3_pleasure_pre_col), "Not found", m3_pleasure_pre_col),
    ifelse(is.na(m3_suicidality_pre_col), "Not found", m3_suicidality_pre_col),
    ifelse(is.na(m3_mood_change_col), "Not found", m3_mood_change_col),
    ifelse(is.na(m3_pleasure_change_col), "Not found", m3_pleasure_change_col),
    ifelse(is.na(m3_suicidality_change_col), "Not found", m3_suicidality_change_col)
  )
)

cat("\nDetected columns:\n")
print(detected_columns, n = Inf)

# ============================================================
# 4. DEFINE WP1 ANALYSED SAMPLE FROM SESSION FEEDBACK
# ============================================================

analysed_ids <- feedback_raw %>%
  transmute(
    part_id = clean_id(.data[[feedback_id_col]]),
    session_n = num_clean(.data[[feedback_session_col]]),
    discomfort = num_clean(.data[[feedback_discomfort_col]])
  ) %>%
  filter(
    !is.na(part_id),
    !is.na(session_n),
    session_n >= 1,
    session_n <= 11,
    !is.na(discomfort)
  ) %>%
  count(part_id, name = "n_valid_sessions") %>%
  filter(n_valid_sessions >= MIN_VALID_WP1_SESSIONS_FOR_ANALYSED) %>%
  pull(part_id)

analysed_id_source <- paste0(
  "WP1 Session Feedback analysed sample: >= ",
  MIN_VALID_WP1_SESSIONS_FOR_ANALYSED,
  " valid session-level discomfort rows"
)

expected_n <- length(unique(analysed_ids))

message("WP1 analysed IDs detected from Session Feedback: ", expected_n)

# ============================================================
# 5. SCORE START / BASELINE DATA
# ============================================================

start_scored_all <- tibble(
  part_id = clean_id(start_raw[[start_id_col]]),
  recorded_date_internal = if (!is.na(start_date_col)) {
    parse_date_flex(start_raw[[start_date_col]])
  } else {
    as.POSIXct(rep(NA, nrow(start_raw)), origin = "1970-01-01", tz = "Europe/London")
  },
  phq9_pre = score_scale_sum(
    start_raw,
    phq_cols_start,
    phq_to_num,
    "PHQ-9 start",
    require_all = TRUE
  ),
  m3vas_mood_pre = if (!is.na(m3_mood_pre_col)) {
    num_clean(start_raw[[m3_mood_pre_col]])
  } else {
    NA_real_
  },
  m3vas_pleasure_pre = if (!is.na(m3_pleasure_pre_col)) {
    num_clean(start_raw[[m3_pleasure_pre_col]])
  } else {
    NA_real_
  },
  m3vas_suicidality_pre = if (!is.na(m3_suicidality_pre_col)) {
    num_clean(start_raw[[m3_suicidality_pre_col]])
  } else {
    NA_real_
  }
) %>%
  filter(!is.na(part_id), part_id != "") %>%
  arrange(part_id, desc(recorded_date_internal)) %>%
  group_by(part_id) %>%
  slice(1) %>%
  ungroup()

start_scored <- start_scored_all %>%
  filter(part_id %in% analysed_ids)

# ============================================================
# 6. SCORE END / POST DATA
# ============================================================

end_scored_all <- tibble(
  part_id = clean_id(end_raw[[end_id_col]]),
  recorded_date_internal = if (!is.na(end_date_col)) {
    parse_date_flex(end_raw[[end_date_col]])
  } else {
    as.POSIXct(rep(NA, nrow(end_raw)), origin = "1970-01-01", tz = "Europe/London")
  },
  phq9_post = score_scale_sum(
    end_raw,
    phq_cols_end,
    phq_to_num,
    "PHQ-9 end",
    require_all = TRUE
  ),
  m3vas_mood_change = if (!is.na(m3_mood_change_col)) {
    num_clean(end_raw[[m3_mood_change_col]])
  } else {
    NA_real_
  },
  m3vas_pleasure_change = if (!is.na(m3_pleasure_change_col)) {
    num_clean(end_raw[[m3_pleasure_change_col]])
  } else {
    NA_real_
  },
  m3vas_suicidality_change = if (!is.na(m3_suicidality_change_col)) {
    num_clean(end_raw[[m3_suicidality_change_col]])
  } else {
    NA_real_
  }
) %>%
  filter(!is.na(part_id), part_id != "") %>%
  arrange(part_id, desc(recorded_date_internal)) %>%
  group_by(part_id) %>%
  slice(1) %>%
  ungroup()

end_scored <- end_scored_all %>%
  filter(part_id %in% analysed_ids)

# ============================================================
# 7. MERGE AND COMPUTE CORRECT M3VAS ESTIMATED POST SCORES
# ============================================================

paired <- start_scored %>%
  left_join(end_scored, by = "part_id") %>%
  mutate(
    # Correct direction:
    # baseline M3VAS: 0 bad -> 100 good
    # change rating: -50 worse -> +50 better
    # therefore estimated post = pre + change
    m3vas_mood_post_est = m3vas_mood_pre + m3vas_mood_change,
    m3vas_pleasure_post_est = m3vas_pleasure_pre + m3vas_pleasure_change,
    m3vas_suicidality_post_est = m3vas_suicidality_pre + m3vas_suicidality_change,
    
    m3vas_mood_post_est_clamped_0_100 = pmin(pmax(m3vas_mood_post_est, 0), 100),
    m3vas_pleasure_post_est_clamped_0_100 = pmin(pmax(m3vas_pleasure_post_est, 0), 100),
    m3vas_suicidality_post_est_clamped_0_100 = pmin(pmax(m3vas_suicidality_post_est, 0), 100),
    
    mood_est_outside_0_100 = !is.na(m3vas_mood_post_est) &
      (m3vas_mood_post_est < 0 | m3vas_mood_post_est > 100),
    
    pleasure_est_outside_0_100 = !is.na(m3vas_pleasure_post_est) &
      (m3vas_pleasure_post_est < 0 | m3vas_pleasure_post_est > 100),
    
    suicidality_est_outside_0_100 = !is.na(m3vas_suicidality_post_est) &
      (m3vas_suicidality_post_est < 0 | m3vas_suicidality_post_est > 100)
  )

# ============================================================
# 8. TABLE S3.3 BASELINE DESCRIPTIVES
# ============================================================

baseline_long <- start_scored %>%
  select(
    part_id,
    phq9_pre,
    m3vas_mood_pre,
    m3vas_pleasure_pre,
    m3vas_suicidality_pre
  ) %>%
  pivot_longer(
    cols = -part_id,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    outcome = case_when(
      variable == "phq9_pre" ~ "PHQ-9",
      variable == "m3vas_mood_pre" ~ "M3VAS mood/depression",
      variable == "m3vas_pleasure_pre" ~ "M3VAS pleasure/anhedonia",
      variable == "m3vas_suicidality_pre" ~ "M3VAS suicidality",
      TRUE ~ variable
    ),
    scale_direction = case_when(
      outcome == "PHQ-9" ~ "Higher = more depressive symptoms",
      str_starts(outcome, "M3VAS") ~ "0 = bad, 100 = good",
      TRUE ~ NA_character_
    )
  )

table_s3_3_baseline <- baseline_long %>%
  group_by(outcome, scale_direction) %>%
  summarise(ci_summary(value), .groups = "drop") %>%
  mutate(
    expected_n = expected_n,
    analysed_n = n,
    missing_n = expected_n - analysed_n,
    `Mean ± SD` = paste0(fmt_num(mean), " ± ", fmt_num(sd)),
    `95% CI` = fmt_ci(ci95_low, ci95_high),
    value_range = paste0(fmt_num(min), " to ", fmt_num(max))
  ) %>%
  select(
    outcome,
    scale_direction,
    expected_n,
    analysed_n,
    missing_n,
    `Mean ± SD`,
    `95% CI`,
    mean,
    sd,
    min,
    max,
    ci95_low,
    ci95_high,
    value_range
  ) %>%
  arrange(
    factor(
      outcome,
      levels = c(
        "PHQ-9",
        "M3VAS mood/depression",
        "M3VAS pleasure/anhedonia",
        "M3VAS suicidality"
      )
    )
  )

# ============================================================
# 9. PRE/POST DESCRIPTIVE TABLE
# ============================================================

prepost_long <- paired %>%
  select(
    part_id,
    phq9_pre,
    phq9_post,
    m3vas_mood_pre,
    m3vas_mood_post_est,
    m3vas_mood_post_est_clamped_0_100,
    m3vas_mood_change,
    m3vas_pleasure_pre,
    m3vas_pleasure_post_est,
    m3vas_pleasure_post_est_clamped_0_100,
    m3vas_pleasure_change,
    m3vas_suicidality_pre,
    m3vas_suicidality_post_est,
    m3vas_suicidality_post_est_clamped_0_100,
    m3vas_suicidality_change
  ) %>%
  pivot_longer(
    cols = -part_id,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    outcome = case_when(
      str_detect(variable, "^phq9") ~ "PHQ-9",
      str_detect(variable, "^m3vas_mood") ~ "M3VAS mood/depression",
      str_detect(variable, "^m3vas_pleasure") ~ "M3VAS pleasure/anhedonia",
      str_detect(variable, "^m3vas_suicidality") ~ "M3VAS suicidality",
      TRUE ~ variable
    ),
    timepoint = case_when(
      str_detect(variable, "_pre$") ~ "Pre",
      str_detect(variable, "_post_est_clamped") ~ "Estimated post, clamped 0-100 audit",
      str_detect(variable, "_post_est$") ~ "Estimated post, raw",
      str_detect(variable, "_post$") ~ "Post",
      str_detect(variable, "_change$") ~ "Raw end-session change rating",
      TRUE ~ variable
    ),
    scale_direction = case_when(
      outcome == "PHQ-9" ~
        "Higher = worse; negative post-minus-pre change = improvement",
      str_starts(outcome, "M3VAS") & timepoint == "Raw end-session change rating" ~
        "-50 = worse, +50 = better; positive change = improvement",
      str_starts(outcome, "M3VAS") ~
        "0 = bad, 100 = good; estimated post = pre + change",
      TRUE ~ NA_character_
    )
  )

prepost_descriptives <- prepost_long %>%
  group_by(outcome, timepoint, scale_direction) %>%
  summarise(ci_summary(value), .groups = "drop") %>%
  mutate(
    expected_n = expected_n,
    analysed_n = n,
    missing_n = expected_n - analysed_n,
    `Mean ± SD` = paste0(fmt_num(mean), " ± ", fmt_num(sd)),
    `95% CI` = fmt_ci(ci95_low, ci95_high),
    value_range = paste0(fmt_num(min), " to ", fmt_num(max))
  ) %>%
  select(
    outcome,
    timepoint,
    scale_direction,
    expected_n,
    analysed_n,
    missing_n,
    `Mean ± SD`,
    `95% CI`,
    mean,
    sd,
    min,
    max,
    ci95_low,
    ci95_high,
    value_range
  ) %>%
  arrange(
    factor(
      outcome,
      levels = c(
        "PHQ-9",
        "M3VAS mood/depression",
        "M3VAS pleasure/anhedonia",
        "M3VAS suicidality"
      )
    ),
    factor(
      timepoint,
      levels = c(
        "Pre",
        "Post",
        "Estimated post, raw",
        "Estimated post, clamped 0-100 audit",
        "Raw end-session change rating"
      )
    )
  )

# ============================================================
# 10. PAIRED CHANGE TABLE
# ============================================================

paired_change <- bind_rows(
  paired_summary(
    paired,
    "phq9_pre",
    "phq9_post",
    "PHQ-9",
    "Higher = worse; negative post-minus-pre change = improvement"
  ),
  paired_summary(
    paired,
    "m3vas_mood_pre",
    "m3vas_mood_post_est",
    "M3VAS mood/depression estimated post",
    "0 = bad, 100 = good; positive post-minus-pre change = improvement"
  ),
  paired_summary(
    paired,
    "m3vas_pleasure_pre",
    "m3vas_pleasure_post_est",
    "M3VAS pleasure/anhedonia estimated post",
    "0 = bad, 100 = good; positive post-minus-pre change = improvement"
  ),
  paired_summary(
    paired,
    "m3vas_suicidality_pre",
    "m3vas_suicidality_post_est",
    "M3VAS suicidality estimated post",
    "0 = bad, 100 = good; positive post-minus-pre change = improvement"
  )
) %>%
  mutate(
    change_ci95 = fmt_ci(change_ci95_low, change_ci95_high),
    pre_mean = round(pre_mean, 2),
    post_mean = round(post_mean, 2),
    mean_change_post_minus_pre = round(mean_change_post_minus_pre, 2),
    sd_change = round(sd_change, 2),
    paired_t = round(paired_t, 3),
    paired_p = fmt_p(paired_p)
  )

# ============================================================
# 11. AUDIT TABLES
# ============================================================

m3vas_coding_audit <- paired %>%
  select(
    part_id,
    m3vas_mood_pre,
    m3vas_mood_change,
    m3vas_mood_post_est,
    m3vas_mood_post_est_clamped_0_100,
    mood_est_outside_0_100,
    m3vas_pleasure_pre,
    m3vas_pleasure_change,
    m3vas_pleasure_post_est,
    m3vas_pleasure_post_est_clamped_0_100,
    pleasure_est_outside_0_100,
    m3vas_suicidality_pre,
    m3vas_suicidality_change,
    m3vas_suicidality_post_est,
    m3vas_suicidality_post_est_clamped_0_100,
    suicidality_est_outside_0_100
  ) %>%
  arrange(part_id)

m3vas_direction_check <- tibble(
  check = c(
    "Baseline M3VAS coding",
    "M3VAS_CHANGE coding",
    "Estimated post formula",
    "Positive M3VAS change",
    "Negative M3VAS change",
    "Main M3VAS summary",
    "Clamped score handling"
  ),
  value = c(
    "0 = bad, 100 = good",
    "-50 = worse, +50 = better",
    "estimated_post = pre + change_rating",
    "Increases estimated post score; interpreted as improvement",
    "Decreases estimated post score; interpreted as worsening",
    "Uses raw estimated post values",
    "Clamped 0-100 estimated values are exported only as an audit/sensitivity column"
  )
)

id_overlap <- tibble(
  metric = c(
    "Analysed IDs from Session Feedback",
    "Start scored participants in analysed sample",
    "End scored participants in analysed sample",
    "Overlapping start/end participants",
    "PHQ-9 paired complete cases",
    "M3VAS mood paired complete cases",
    "M3VAS pleasure paired complete cases",
    "M3VAS suicidality paired complete cases"
  ),
  n = c(
    expected_n,
    n_distinct(start_scored$part_id),
    n_distinct(end_scored$part_id),
    length(intersect(start_scored$part_id, end_scored$part_id)),
    sum(!is.na(paired$phq9_pre) & !is.na(paired$phq9_post)),
    sum(!is.na(paired$m3vas_mood_pre) & !is.na(paired$m3vas_mood_post_est)),
    sum(!is.na(paired$m3vas_pleasure_pre) & !is.na(paired$m3vas_pleasure_post_est)),
    sum(!is.na(paired$m3vas_suicidality_pre) & !is.na(paired$m3vas_suicidality_post_est))
  )
)

raw_column_names <- bind_rows(
  tibble(file = "Testing Start", column = names(start_raw)),
  tibble(file = "Testing End", column = names(end_raw)),
  tibble(file = "Session Feedback", column = names(feedback_raw))
)

# ============================================================
# 12. NARRATIVE GENERATION
# ============================================================

get_base_row <- function(outcome_name) {
  table_s3_3_baseline %>%
    filter(outcome == outcome_name) %>%
    slice(1)
}

get_pair_row <- function(outcome_name) {
  paired_change %>%
    filter(outcome == outcome_name) %>%
    slice(1)
}

phq_base <- get_base_row("PHQ-9")
mood_base <- get_base_row("M3VAS mood/depression")
pleasure_base <- get_base_row("M3VAS pleasure/anhedonia")
suic_base <- get_base_row("M3VAS suicidality")

phq_pair <- get_pair_row("PHQ-9")
mood_pair <- get_pair_row("M3VAS mood/depression estimated post")
pleasure_pair <- get_pair_row("M3VAS pleasure/anhedonia estimated post")
suic_pair <- get_pair_row("M3VAS suicidality estimated post")

narrative <- c(
  "Table S3.3. WP1 Baseline PHQ-9 and M3VAS Descriptive Summaries.",
  "",
  paste0(
    "Table S3.3 reports descriptive pre-stimulation PHQ-9 and M3VAS values for the WP1 analysed sample. ",
    "The expected denominator was defined as ", expected_n, " participants using the rule: ",
    analysed_id_source, ". ",
    "PHQ-9 was used to characterise depressive-symptom severity at entry, while M3VAS items indexed mood/depression, pleasure/anhedonia, and suicidality before exposure. ",
    "These values are baseline descriptors."
  ),
  "",
  paste0(
    "At baseline, PHQ-9 scores were available for ", phq_base$analysed_n,
    "/", phq_base$expected_n, " participants. The mean PHQ-9 score was ",
    phq_base$`Mean ± SD`, ", with a 95% CI of ", phq_base$`95% CI`,
    " and a range of ", phq_base$value_range, "."
  ),
  paste0(
    "M3VAS mood/depression scores were available for ", mood_base$analysed_n,
    "/", mood_base$expected_n, " participants. The mean score was ",
    mood_base$`Mean ± SD`, ", with a 95% CI of ", mood_base$`95% CI`,
    " and a range of ", mood_base$value_range, "."
  ),
  paste0(
    "M3VAS pleasure/anhedonia scores were available for ", pleasure_base$analysed_n,
    "/", pleasure_base$expected_n, " participants. The mean score was ",
    pleasure_base$`Mean ± SD`, ", with a 95% CI of ", pleasure_base$`95% CI`,
    " and a range of ", pleasure_base$value_range, "."
  ),
  paste0(
    "M3VAS suicidality scores were available for ", suic_base$analysed_n,
    "/", suic_base$expected_n, " participants. The mean score was ",
    suic_base$`Mean ± SD`, ", with a 95% CI of ", suic_base$`95% CI`,
    " and a range of ", suic_base$value_range, "."
  ),
  "",
  "Pre/post mood-check coding audit.",
  "",
  paste0(
    "For PHQ-9, lower scores indicate improvement. Among ", phq_pair$paired_n,
    " paired participants, mean PHQ-9 changed from ", phq_pair$pre_mean,
    " pre-stimulation to ", phq_pair$post_mean,
    " post-stimulation, corresponding to a mean post-minus-pre change of ",
    phq_pair$mean_change_post_minus_pre, " points (95% CI ",
    phq_pair$change_ci95, "; paired t = ", phq_pair$paired_t,
    ", p = ", phq_pair$paired_p, ")."
  ),
  paste0(
    "For M3VAS outcomes, baseline scores were coded 0 = bad and 100 = good, while M3VAS_CHANGE scores were coded -50 = worse and +50 = better. ",
    "Estimated post-stimulation scores were therefore computed as baseline + change rating."
  ),
  paste0(
    "For M3VAS mood/depression, among ", mood_pair$paired_n,
    " paired participants, the mean estimated post-minus-pre change was ",
    mood_pair$mean_change_post_minus_pre, " points (95% CI ",
    mood_pair$change_ci95, "). Positive values indicate improvement."
  ),
  paste0(
    "For M3VAS pleasure/anhedonia, among ", pleasure_pair$paired_n,
    " paired participants, the mean estimated post-minus-pre change was ",
    pleasure_pair$mean_change_post_minus_pre, " points (95% CI ",
    pleasure_pair$change_ci95, "). Positive values indicate improvement."
  ),
  paste0(
    "For M3VAS suicidality, among ", suic_pair$paired_n,
    " paired participants, the mean estimated post-minus-pre change was ",
    suic_pair$mean_change_post_minus_pre, " points (95% CI ",
    suic_pair$change_ci95, "). Positive values indicate improvement under the stated M3VAS coding."
  ),
  "",
  paste0(
    "These pre/post summaries should be interpreted cautiously because M3VAS post-stimulation values were estimated from change ratings rather than directly observed raw post-stimulation VAS scores, ",
    "and sample sizes differed across measures and timepoints. Clamped 0-100 estimated M3VAS scores are exported in the audit sheet but are not used as the primary descriptive estimate unless explicitly stated."
  )
)

writeLines(narrative, OUT_NARRATIVE)

# ============================================================
# 13. EXPORT OUTPUTS
# ============================================================

readr::write_csv(table_s3_3_baseline, OUT_BASELINE_CSV)
readr::write_csv(prepost_descriptives, OUT_PREPOST_CSV)
readr::write_csv(paired_change, OUT_PAIRED_CSV)

excel_sheets <- list(
  "Table S3.3 baseline" = make_plain_df(table_s3_3_baseline),
  "Prepost descriptives" = make_plain_df(prepost_descriptives),
  "Paired change" = make_plain_df(paired_change),
  "M3VAS coding audit" = make_plain_df(m3vas_coding_audit),
  "M3VAS direction check" = make_plain_df(m3vas_direction_check),
  "Detected columns" = make_plain_df(detected_columns),
  "ID overlap" = make_plain_df(id_overlap),
  "Raw column names" = make_plain_df(raw_column_names),
  "Start scored" = make_plain_df(start_scored),
  "End scored" = make_plain_df(end_scored),
  "Merged paired data" = make_plain_df(paired)
)

writexl::write_xlsx(excel_sheets, OUT_XLSX)

# ============================================================
# 14. CONSOLE OUTPUT
# ============================================================

cat("\n============================================================\n")
cat("WP1 TABLE S3.3 + PRE/POST MOOD DEBUG EXPORT COMPLETE\n")
cat("============================================================\n\n")

cat("Files used:\n")
cat("  Start:    ", START_FILE, "\n")
cat("  End:      ", END_FILE, "\n")
cat("  Feedback: ", FEEDBACK_FILE, "\n\n")

cat("Expected n source:\n")
cat("  ", analysed_id_source, "\n")
cat("  expected_n = ", expected_n, "\n\n", sep = "")

cat("Outputs:\n")
cat("  ", OUT_XLSX, "\n")
cat("  ", OUT_BASELINE_CSV, "\n")
cat("  ", OUT_PREPOST_CSV, "\n")
cat("  ", OUT_PAIRED_CSV, "\n")
cat("  ", OUT_NARRATIVE, "\n\n")

cat("Detected columns:\n")
print(detected_columns, n = Inf)

cat("\nID overlap:\n")
print(id_overlap, n = Inf)

cat("\nTable S3.3 baseline preview:\n")
print(table_s3_3_baseline, n = Inf, width = Inf)

cat("\nPaired change preview:\n")
print(paired_change, n = Inf, width = Inf)

cat("\nNarrative written to:\n")
cat("  ", OUT_NARRATIVE, "\n")

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################









# ------------------------------------------------------------
# SECTION: interim demographics
# ------------------------------------------------------------

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

# ===================================================
# INTERIM DEMOGRAPHICS
# ===================================================

# ===================================================
# INTERIM DEMOGRAPHICS TABLES
# ===================================================
# Inputs:
#   *Interim*Sign-Up*.csv or *Interim*Sign-up*.csv
#   *n32apr20*.csv
#
# Outputs:
#   INTERIM_SUPERVISOR_Demographics_Table.html/png/pdf
#   INTERIM_SUPERVISOR_MissingDenominators_Table.html/png/pdf
#   INTERIM_SUPERVISOR_Demographics_StageSpecific.csv
#   INTERIM_SUPERVISOR_MissingDenominators.csv
#   INTERIM_SUPERVISOR_ID_Audit.csv
#   INTERIM_SUPERVISOR_Demographics_ManuscriptText.txt
#
# Stages:
#   Screened         = unique valid IDs in Interim Sign-Up
#   Passed screening = unique valid IDs in Interim Sign-Up with excluded == FALSE
#   Tested           = unique valid IDs in n32apr20.csv
#   Analysed         = unique valid IDs in n32apr20.csv
#
# Notes:
#   - In the interim study, the analysed file appears to be the tested/analysed dataset.
#   - Baseline clinical variable defaults to trait anxiety / anx_total.
#   - PHQ-9 / BDI-II are included only if detectable in the files.
# ===================================================

# install.packages(c("tidyverse", "lubridate", "gt", "webshot2"))

library(tidyverse)
library(lubridate)
library(gt)

# ===================================================
# 1. SETTINGS
# ===================================================

DATA_DIR <- Sys.getenv("MRC_DATA_DIR", unset = "C:/Users/dn284/Desktop/MRC_omni/data")
SEARCH_DIRS <- c(DATA_DIR)

OUT_PREFIX <- "INTERIM_SUPERVISOR"

# Set expected values if you want hard stops.
# Based on the uploaded files, these are likely:
EXPECTED_SCREENED_N <- 109
EXPECTED_ANALYSED_N <- 32

USE_HARD_DENOMINATOR_CHECKS <- TRUE

OUT_SUMMARY_CSV <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_Demographics_StageSpecific.csv"))
OUT_MISSING_CSV <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_MissingDenominators.csv"))
OUT_ID_AUDIT_CSV <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_ID_Audit.csv"))
OUT_TEXT <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_Demographics_ManuscriptText.txt"))

OUT_TABLE_HTML <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_Demographics_Table.html"))
OUT_TABLE_PNG  <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_Demographics_Table.png"))
OUT_TABLE_PDF  <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_Demographics_Table.pdf"))

OUT_MISSING_HTML <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_MissingDenominators_Table.html"))
OUT_MISSING_PNG  <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_MissingDenominators_Table.png"))
OUT_MISSING_PDF  <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_MissingDenominators_Table.pdf"))

# ===================================================
# 2. HELPERS
# ===================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

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
  
  hits[order(file.info(hits)$mtime, decreasing = TRUE)][1]
}

read_qualtrics_real <- function(path) {
  df <- readr::read_csv(
    path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )
  
  names(df) <- str_trim(names(df))
  
  if ("ResponseId" %in% names(df)) {
    df <- df %>%
      filter(str_starts(as.character(ResponseId), "R_"))
  }
  
  if ("Response ID" %in% names(df)) {
    df <- df %>%
      filter(str_starts(as.character(`Response ID`), "R_"))
  }
  
  df <- df %>%
    filter(
      !if_any(
        everything(),
        ~ str_detect(coalesce(as.character(.x), ""), fixed("ImportId"))
      )
    )
  
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
    str_extract("\\d{1,6}")
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

best_signup_per_pid <- function(df, pid_col = "part_id") {
  
  if (!pid_col %in% names(df)) {
    stop("PID column not found: ", pid_col)
  }
  
  completeness_cols <- c(
    "incl_dem_age",
    "incl_dem_sex",
    "incl_dem_gender",
    "incl_dem_med",
    "anx_total",
    paste0("trait_anx_", 1:20),
    paste0("phq9_", 1:9),
    "phq9_sum",
    paste0("bdi_", 1:21),
    "bdi_sum",
    "bdi_total"
  )
  
  completeness_cols <- completeness_cols[completeness_cols %in% names(df)]
  
  dt_col <- c("RecordedDate", "EndDate", "StartDate")
  dt_col <- dt_col[dt_col %in% names(df)][1]
  
  df %>%
    filter(!is.na(.data[[pid_col]])) %>%
    mutate(
      .row_order = row_number(),
      .completeness = if (length(completeness_cols) > 0) {
        rowSums(
          across(
            all_of(completeness_cols),
            ~ !is.na(.x) & str_squish(as.character(.x)) != ""
          ),
          na.rm = TRUE
        )
      } else {
        0
      },
      .dt = if (!is.na(dt_col)) {
        suppressWarnings(
          parse_date_time(
            .data[[dt_col]],
            orders = c(
              "ymd HMS", "ymd HM",
              "dmy HMS", "dmy HM",
              "mdy HMS", "mdy HM"
            )
          )
        )
      } else {
        as.POSIXct(NA)
      }
    ) %>%
    arrange(
      .data[[pid_col]],
      desc(.completeness),
      desc(.dt),
      desc(.row_order)
    ) %>%
    distinct(.data[[pid_col]], .keep_all = TRUE) %>%
    select(-.row_order, -.completeness, -.dt)
}

clean_cat <- function(x) {
  x_chr <- str_squish(as.character(x))
  x_chr <- if_else(is.na(x) | x_chr == "", NA_character_, x_chr)
  
  case_when(
    str_to_lower(x_chr) %in% c("true", "yes", "y", "1") ~ "Yes",
    str_to_lower(x_chr) %in% c("false", "no", "n", "0") ~ "No",
    TRUE ~ x_chr
  )
}

score_phq_item <- function(x) {
  x_chr <- str_to_lower(str_squish(as.character(x)))
  
  case_when(
    is.na(x_chr) | x_chr == "" ~ NA_real_,
    str_detect(x_chr, "not at all") ~ 0,
    str_detect(x_chr, "several days") ~ 1,
    str_detect(x_chr, "more than half") ~ 2,
    str_detect(x_chr, "nearly every day") ~ 3,
    str_detect(x_chr, "^0$") ~ 0,
    str_detect(x_chr, "^1$") ~ 1,
    str_detect(x_chr, "^2$") ~ 2,
    str_detect(x_chr, "^3$") ~ 3,
    TRUE ~ suppressWarnings(as.numeric(x_chr))
  )
}

score_bdi_item <- function(x) {
  x_chr <- str_squish(as.character(x))
  
  case_when(
    is.na(x_chr) | x_chr == "" ~ NA_real_,
    str_detect(x_chr, "^0") ~ 0,
    str_detect(x_chr, "^1") ~ 1,
    str_detect(x_chr, "^2") ~ 2,
    str_detect(x_chr, "^3") ~ 3,
    TRUE ~ suppressWarnings(as.numeric(x_chr))
  )
}

score_trait_anx_item <- function(x) {
  x_chr <- str_to_lower(str_squish(as.character(x)))
  
  case_when(
    is.na(x_chr) | x_chr == "" ~ NA_real_,
    str_detect(x_chr, "^1$") ~ 1,
    str_detect(x_chr, "^2$") ~ 2,
    str_detect(x_chr, "^3$") ~ 3,
    str_detect(x_chr, "^4$") ~ 4,
    str_detect(x_chr, "almost never") ~ 1,
    str_detect(x_chr, "sometimes") ~ 2,
    str_detect(x_chr, "often") ~ 3,
    str_detect(x_chr, "almost always") ~ 4,
    TRUE ~ suppressWarnings(as.numeric(x_chr))
  )
}

summarise_numeric <- function(df, value_col, stage, label) {
  x <- suppressWarnings(as.numeric(df[[value_col]]))
  stage_denom <- nrow(df)
  nonmissing_n <- sum(!is.na(x))
  missing_n <- sum(is.na(x))
  
  tibble(
    Stage = stage,
    Variable = label,
    Level = NA_character_,
    Stage_denominator = stage_denom,
    Denominator = nonmissing_n,
    Non_missing_n = nonmissing_n,
    Missing_n = missing_n,
    Count = NA_integer_,
    Percent = NA_real_,
    Mean = ifelse(nonmissing_n > 0, mean(x, na.rm = TRUE), NA_real_),
    SD = ifelse(nonmissing_n > 1, sd(x, na.rm = TRUE), NA_real_),
    Min = ifelse(nonmissing_n > 0, min(x, na.rm = TRUE), NA_real_),
    Max = ifelse(nonmissing_n > 0, max(x, na.rm = TRUE), NA_real_)
  )
}

summarise_categorical <- function(df, value_col, stage, label) {
  vals <- clean_cat(df[[value_col]])
  stage_denom <- nrow(df)
  
  nonmissing <- !is.na(vals)
  cat_denom <- sum(nonmissing)
  missing_n <- sum(!nonmissing)
  
  nonmissing_tbl <- tibble(value = vals[nonmissing]) %>%
    count(value, name = "Count") %>%
    mutate(
      Stage = stage,
      Variable = label,
      Level = value,
      Stage_denominator = stage_denom,
      Denominator = cat_denom,
      Non_missing_n = cat_denom,
      Missing_n = missing_n,
      Percent = ifelse(cat_denom > 0, 100 * Count / cat_denom, NA_real_),
      Mean = NA_real_,
      SD = NA_real_,
      Min = NA_real_,
      Max = NA_real_
    ) %>%
    select(
      Stage, Variable, Level, Stage_denominator, Denominator,
      Non_missing_n, Missing_n, Count, Percent, Mean, SD, Min, Max
    )
  
  missing_tbl <- tibble(
    Stage = stage,
    Variable = label,
    Level = "Unavailable / missing",
    Stage_denominator = stage_denom,
    Denominator = stage_denom,
    Non_missing_n = cat_denom,
    Missing_n = missing_n,
    Count = missing_n,
    Percent = ifelse(stage_denom > 0, 100 * missing_n / stage_denom, NA_real_),
    Mean = NA_real_,
    SD = NA_real_,
    Min = NA_real_,
    Max = NA_real_
  )
  
  bind_rows(nonmissing_tbl, missing_tbl)
}

missing_audit <- function(df, value_col, stage, label) {
  vals <- df[[value_col]]
  is_missing <- is.na(vals) | str_squish(as.character(vals)) == ""
  
  missing_ids <- df %>%
    filter(is_missing) %>%
    pull(part_id) %>%
    unique() %>%
    na.omit()
  
  tibble(
    Stage = stage,
    Variable = label,
    Stage_denominator = nrow(df),
    Non_missing_n = nrow(df) - length(missing_ids),
    Missing_n = length(missing_ids),
    Missing_percent = ifelse(nrow(df) > 0, 100 * length(missing_ids) / nrow(df), NA_real_),
    Missing_part_ids = paste(missing_ids, collapse = "; ")
  )
}

# ===================================================
# 3. LOCATE AND LOAD FILES
# ===================================================

INTERIM_SIGNUP_PATH <- newest_match(c("*Interim*Sign-Up*.csv", "*Interim*Sign-up*.csv"))
INTERIM_ANALYSED_PATH <- newest_match(c("*n32apr20*.csv"))

message("Interim sign-up:  ", basename(INTERIM_SIGNUP_PATH))
message("Interim analysed: ", basename(INTERIM_ANALYSED_PATH))

interim_signup_raw <- read_qualtrics_real(INTERIM_SIGNUP_PATH)
interim_analysed_raw <- readr::read_csv(
  INTERIM_ANALYSED_PATH,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

names(interim_signup_raw) <- str_trim(names(interim_signup_raw))
names(interim_analysed_raw) <- str_trim(names(interim_analysed_raw))

# ===================================================
# 4. CLEAN IDS
# ===================================================

if (!"part_id" %in% names(interim_signup_raw)) {
  stop("No part_id column found in Interim Sign-Up.")
}

if (!"part_id" %in% names(interim_analysed_raw)) {
  stop("No part_id column found in n32apr20 analysed file.")
}

interim_signup <- interim_signup_raw %>%
  mutate(
    part_id_original = clean_id(part_id),
    part_id = clean_id(part_id)
  ) %>%
  filter(!is.na(part_id))

interim_analysed <- interim_analysed_raw %>%
  mutate(
    part_id_original = clean_id(part_id),
    part_id = clean_id(part_id)
  ) %>%
  filter(!is.na(part_id))

interim_signup_u <- best_signup_per_pid(interim_signup, "part_id")

# ===================================================
# 5. SCORE AVAILABLE BASELINE CLINICAL VARIABLES
# ===================================================

# Trait anxiety
trait_items <- paste0("trait_anx_", 1:20)

if ("anx_total" %in% names(interim_signup_u)) {
  interim_signup_u <- interim_signup_u %>%
    mutate(baseline_anxiety = suppressWarnings(as.numeric(anx_total)))
} else if (all(trait_items %in% names(interim_signup_u))) {
  interim_signup_u <- interim_signup_u %>%
    mutate(
      across(all_of(trait_items), score_trait_anx_item, .names = "{.col}_score"),
      baseline_anxiety = rowSums(across(all_of(paste0(trait_items, "_score"))), na.rm = FALSE)
    )
} else {
  interim_signup_u$baseline_anxiety <- NA_real_
}

# Optional PHQ-9 if present
phq_items <- paste0("phq9_", 1:9)

if ("phq9_sum" %in% names(interim_signup_u)) {
  interim_signup_u <- interim_signup_u %>%
    mutate(baseline_phq9 = suppressWarnings(as.numeric(phq9_sum)))
} else if (all(phq_items %in% names(interim_signup_u))) {
  interim_signup_u <- interim_signup_u %>%
    mutate(
      across(all_of(phq_items), score_phq_item, .names = "{.col}_score"),
      baseline_phq9 = rowSums(across(all_of(paste0(phq_items, "_score"))), na.rm = FALSE)
    )
} else {
  interim_signup_u$baseline_phq9 <- NA_real_
}

# Optional BDI-II if present
bdi_items <- paste0("bdi_", 1:21)

if ("bdi_sum" %in% names(interim_signup_u)) {
  interim_signup_u <- interim_signup_u %>%
    mutate(baseline_bdi = suppressWarnings(as.numeric(bdi_sum)))
} else if ("bdi_total" %in% names(interim_signup_u)) {
  interim_signup_u <- interim_signup_u %>%
    mutate(baseline_bdi = suppressWarnings(as.numeric(bdi_total)))
} else if (all(bdi_items %in% names(interim_signup_u))) {
  interim_signup_u <- interim_signup_u %>%
    mutate(
      across(all_of(bdi_items), score_bdi_item, .names = "{.col}_score"),
      baseline_bdi = rowSums(across(all_of(paste0(bdi_items, "_score"))), na.rm = FALSE)
    )
} else {
  interim_signup_u$baseline_bdi <- NA_real_
}

# ===================================================
# 6. DEFINE STAGE IDS
# ===================================================

screened_ids <- interim_signup_u %>%
  pull(part_id) %>%
  unique()

passed_ids <- interim_signup_u %>%
  mutate(excluded_clean = str_to_upper(str_trim(as.character(excluded)))) %>%
  filter(excluded_clean == "FALSE") %>%
  pull(part_id) %>%
  unique()

tested_ids <- interim_analysed %>%
  pull(part_id) %>%
  unique()

analysed_ids <- tested_ids

stage_ids <- list(
  "Screened" = screened_ids,
  "Passed screening" = passed_ids,
  "Tested" = tested_ids,
  "Analysed" = analysed_ids
)

stage_n_table <- tibble(
  Stage = names(stage_ids),
  N = map_int(stage_ids, ~ length(unique(na.omit(.x))))
)

denominator_audit <- tibble(
  Quantity = c(
    "Screened IDs in Interim Sign-Up",
    "Passed-screening IDs in Interim Sign-Up",
    "Tested IDs in analysed file",
    "Analysed IDs in analysed file",
    "Analysed IDs missing from Sign-Up"
  ),
  N = c(
    length(screened_ids),
    length(passed_ids),
    length(tested_ids),
    length(analysed_ids),
    length(setdiff(analysed_ids, screened_ids))
  ),
  IDs = c(
    paste(sort(screened_ids), collapse = "; "),
    paste(sort(passed_ids), collapse = "; "),
    paste(sort(tested_ids), collapse = "; "),
    paste(sort(analysed_ids), collapse = "; "),
    paste(sort(setdiff(analysed_ids, screened_ids)), collapse = "; ")
  )
)

cat("\n=== Interim denominator audit ===\n")
print(denominator_audit %>% select(Quantity, N), n = Inf)

cat("\n=== Interim final stage denominators ===\n")
print(stage_n_table)

if (USE_HARD_DENOMINATOR_CHECKS) {
  if (length(screened_ids) != EXPECTED_SCREENED_N) {
    stop("Screened n mismatch: expected ", EXPECTED_SCREENED_N, ", got ", length(screened_ids))
  }
  
  if (length(analysed_ids) != EXPECTED_ANALYSED_N) {
    stop("Analysed n mismatch: expected ", EXPECTED_ANALYSED_N, ", got ", length(analysed_ids))
  }
}

# ===================================================
# 7. MASTER TABLE
# ===================================================

all_stage_ids <- unique(unlist(stage_ids))

interim_master <- tibble(part_id = all_stage_ids) %>%
  left_join(
    interim_signup_u %>%
      select(
        part_id,
        any_of(c(
          "incl_dem_age",
          "incl_dem_sex",
          "incl_dem_gender",
          "incl_dem_med",
          "baseline_anxiety",
          "baseline_phq9",
          "baseline_bdi"
        ))
      ),
    by = "part_id"
  )

needed_cols <- c(
  "incl_dem_age",
  "incl_dem_sex",
  "incl_dem_gender",
  "incl_dem_med",
  "baseline_anxiety",
  "baseline_phq9",
  "baseline_bdi"
)

for (col in needed_cols) {
  if (!col %in% names(interim_master)) interim_master[[col]] <- NA
}

analysed_missing_demo <- interim_master %>%
  filter(part_id %in% analysed_ids) %>%
  mutate(
    has_age = !is.na(suppressWarnings(as.numeric(incl_dem_age))),
    has_sex = !is.na(clean_cat(incl_dem_sex)),
    has_gender = !is.na(clean_cat(incl_dem_gender)),
    has_med = !is.na(clean_cat(incl_dem_med)),
    has_any_demo = has_age | has_sex | has_gender | has_med
  ) %>%
  filter(!has_any_demo) %>%
  pull(part_id)

cat("\n=== Interim analysed IDs missing all sign-up demographics ===\n")
print(sort(analysed_missing_demo))

id_audit <- tibble(part_id = unique(c(screened_ids, analysed_ids))) %>%
  mutate(
    In_signup = part_id %in% screened_ids,
    Passed_screening = part_id %in% passed_ids,
    In_analysed_file = part_id %in% analysed_ids,
    Has_signup_demographics = part_id %in% interim_signup_u$part_id
  ) %>%
  arrange(desc(In_analysed_file), part_id)

# ===================================================
# 8. SUMMARISE TABLE VALUES
# ===================================================

summary_rows <- list()
missing_rows <- list()

include_phq <- any(!is.na(interim_master$baseline_phq9))
include_bdi <- any(!is.na(interim_master$baseline_bdi))
include_anx <- any(!is.na(interim_master$baseline_anxiety))

for (stage_name in names(stage_ids)) {
  ids <- stage_ids[[stage_name]]
  
  df_stage <- interim_master %>%
    filter(part_id %in% ids) %>%
    distinct(part_id, .keep_all = TRUE)
  
  summary_rows[[length(summary_rows) + 1]] <- summarise_numeric(df_stage, "incl_dem_age", stage_name, "Age")
  summary_rows[[length(summary_rows) + 1]] <- summarise_categorical(df_stage, "incl_dem_sex", stage_name, "Sex")
  summary_rows[[length(summary_rows) + 1]] <- summarise_categorical(df_stage, "incl_dem_gender", stage_name, "Gender")
  summary_rows[[length(summary_rows) + 1]] <- summarise_categorical(df_stage, "incl_dem_med", stage_name, "Medication status")
  
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_stage, "incl_dem_age", stage_name, "Age")
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_stage, "incl_dem_sex", stage_name, "Sex")
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_stage, "incl_dem_gender", stage_name, "Gender")
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_stage, "incl_dem_med", stage_name, "Medication status")
  
  if (include_anx) {
    summary_rows[[length(summary_rows) + 1]] <- summarise_numeric(df_stage, "baseline_anxiety", stage_name, "Baseline trait anxiety")
    missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_stage, "baseline_anxiety", stage_name, "Baseline trait anxiety")
  }
  
  if (include_phq) {
    summary_rows[[length(summary_rows) + 1]] <- summarise_numeric(df_stage, "baseline_phq9", stage_name, "Baseline PHQ-9")
    missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_stage, "baseline_phq9", stage_name, "Baseline PHQ-9")
  }
  
  if (include_bdi) {
    summary_rows[[length(summary_rows) + 1]] <- summarise_numeric(df_stage, "baseline_bdi", stage_name, "Baseline BDI-II")
    missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_stage, "baseline_bdi", stage_name, "Baseline BDI-II")
  }
}

interim_demo_summary <- bind_rows(summary_rows) %>%
  mutate(
    Percent = round(Percent, 1),
    Mean = round(Mean, 2),
    SD = round(SD, 2),
    Min = round(Min, 2),
    Max = round(Max, 2)
  )

interim_missing_denominators <- bind_rows(missing_rows) %>%
  mutate(Missing_percent = round(Missing_percent, 1))

# ===================================================
# 9. FORMAT MAIN TABLE
# ===================================================

format_numeric_summary <- function(mean, sd, min, max, n, miss) {
  ifelse(
    is.na(mean),
    "",
    paste0(
      sprintf("%.2f", mean),
      " ± ",
      sprintf("%.2f", sd),
      " [",
      sprintf("%.2f", min),
      "–",
      sprintf("%.2f", max),
      "]",
      " (n = ", n,
      ifelse(miss > 0, paste0("; unavailable = ", miss), ""),
      ")"
    )
  )
}

demo_numeric <- interim_demo_summary %>%
  filter(is.na(Level)) %>%
  mutate(
    Display = format_numeric_summary(Mean, SD, Min, Max, Non_missing_n, Missing_n)
  ) %>%
  select(Stage, Variable, Display)

demo_categorical <- interim_demo_summary %>%
  filter(
    !is.na(Level),
    Level != "Unavailable / missing",
    Count > 0
  ) %>%
  mutate(
    Display = paste0(
      Count,
      "/",
      Denominator,
      " (",
      sprintf("%.1f", 100 * Count / Denominator),
      "%)"
    )
  ) %>%
  select(Stage, Variable, Level, Display) %>%
  mutate(Variable = paste0(Variable, ": ", Level)) %>%
  select(Stage, Variable, Display)

demo_unavailable <- interim_demo_summary %>%
  filter(
    !is.na(Level),
    Level == "Unavailable / missing",
    Count > 0
  ) %>%
  mutate(
    Display = paste0(Count, "/", Stage_denominator, " unavailable")
  ) %>%
  select(Stage, Variable, Level, Display) %>%
  mutate(Variable = paste0(Variable, ": Unavailable / missing")) %>%
  select(Stage, Variable, Display)

demo_table_wide <- bind_rows(
  demo_numeric,
  demo_categorical,
  demo_unavailable
) %>%
  pivot_wider(
    names_from = Stage,
    values_from = Display
  ) %>%
  mutate(across(everything(), ~ replace_na(.x, ""))) %>%
  rename(Characteristic = Variable)

preferred_order <- c(
  "Age",
  "Sex: Female",
  "Sex: Male",
  "Sex: Other",
  "Sex: Prefer not to say",
  "Sex: Unavailable / missing",
  "Gender: Female",
  "Gender: Male",
  "Gender: Other",
  "Gender: Non-binary",
  "Gender: Prefer not to say",
  "Gender: Unavailable / missing",
  "Medication status: Yes",
  "Medication status: No",
  "Medication status: Unavailable / missing",
  "Baseline trait anxiety",
  "Baseline PHQ-9",
  "Baseline BDI-II"
)

demo_table_wide <- demo_table_wide %>%
  mutate(
    .order = match(Characteristic, preferred_order),
    .order = ifelse(is.na(.order), 999, .order)
  ) %>%
  arrange(.order, Characteristic) %>%
  select(-.order)

# ===================================================
# 10. EXPORT MAIN TABLE
# ===================================================

demo_gt <- demo_table_wide %>%
  gt() %>%
  tab_header(
    title = md("**Interim Study Stage-Specific Demographic and Baseline Characteristics**"),
    subtitle = md(
      "Continuous variables are mean ± SD [range]. Categorical variables are n / non-missing denominator (%). Unavailable records are shown separately."
    )
  ) %>%
  cols_align(align = "left", columns = Characteristic) %>%
  cols_align(align = "center", columns = -Characteristic) %>%
  tab_options(
    table.font.names = "Palatino Linotype",
    table.font.size = px(13),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(12),
    column_labels.font.weight = "bold",
    data_row.padding = px(4),
    source_notes.font.size = px(10),
    table.border.top.width = px(1),
    table.border.bottom.width = px(1)
  ) %>%
  tab_source_note(
    source_note = md(
      paste0(
        "Screened and passed-screening denominators are derived from the Interim Sign-Up file. ",
        "Tested and analysed denominators are derived from the analysed interim dataset (",
        basename(INTERIM_ANALYSED_PATH),
        "). Baseline trait anxiety is derived from Sign-Up where available."
      )
    )
  )

gtsave(demo_gt, OUT_TABLE_HTML)
gtsave(demo_gt, OUT_TABLE_PNG)
gtsave(demo_gt, OUT_TABLE_PDF)

# ===================================================
# 11. EXPORT MISSING-DENOMINATOR TABLE
# ===================================================

missing_table_export <- interim_missing_denominators %>%
  mutate(
    Missing = paste0(
      Missing_n,
      "/",
      Stage_denominator,
      " (",
      sprintf("%.1f", Missing_percent),
      "%)"
    )
  ) %>%
  select(
    Stage,
    Variable,
    Stage_denominator,
    Non_missing_n,
    Missing,
    Missing_part_ids
  ) %>%
  arrange(
    factor(Stage, levels = c("Screened", "Passed screening", "Tested", "Analysed")),
    Variable
  )

missing_gt <- missing_table_export %>%
  gt() %>%
  tab_header(
    title = md("**Interim Study Missing-Denominator Audit**"),
    subtitle = md("Missingness is shown as missing n / stage denominator (%), with participant IDs listed where available.")
  ) %>%
  cols_label(
    Stage = "Stage",
    Variable = "Variable",
    Stage_denominator = "Stage denominator",
    Non_missing_n = "Non-missing n",
    Missing = "Missing / unavailable",
    Missing_part_ids = "Missing participant IDs"
  ) %>%
  cols_align(
    align = "center",
    columns = c(Stage, Variable, Stage_denominator, Non_missing_n, Missing)
  ) %>%
  cols_align(align = "left", columns = Missing_part_ids) %>%
  tab_options(
    table.font.names = "Palatino Linotype",
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(12),
    column_labels.font.weight = "bold",
    data_row.padding = px(4),
    source_notes.font.size = px(10)
  )

gtsave(missing_gt, OUT_MISSING_HTML)
gtsave(missing_gt, OUT_MISSING_PNG)
gtsave(missing_gt, OUT_MISSING_PDF)

# ===================================================
# 12. SAVE AUDITS
# ===================================================

readr::write_csv(interim_demo_summary, OUT_SUMMARY_CSV)
readr::write_csv(interim_missing_denominators, OUT_MISSING_CSV)
readr::write_csv(id_audit, OUT_ID_AUDIT_CSV)
readr::write_csv(denominator_audit, file.path(DATA_DIR, paste0(OUT_PREFIX, "_Denominator_Audit.csv")))

text_lines <- c(
  "Interim study demographics table denominator audit",
  "==================================================",
  "",
  paste0("Sign-Up file: ", basename(INTERIM_SIGNUP_PATH)),
  paste0("Analysed file: ", basename(INTERIM_ANALYSED_PATH)),
  "",
  "Stage denominators used:",
  paste0(stage_n_table$Stage, ": n = ", stage_n_table$N),
  "",
  "Denominator audit:",
  paste0(denominator_audit$Quantity, ": n = ", denominator_audit$N),
  "",
  "Analysed IDs missing all sign-up demographics:",
  if (length(analysed_missing_demo) == 0) "None" else paste(sort(analysed_missing_demo), collapse = ", "),
  "",
  "Clinical baseline variables included:",
  paste(
    c(
      if (include_anx) "Baseline trait anxiety",
      if (include_phq) "Baseline PHQ-9",
      if (include_bdi) "Baseline BDI-II"
    ),
    collapse = ", "
  )
)

writeLines(text_lines, OUT_TEXT)

cat("\n=== Exported Interim files ===\n")
cat("Main table HTML:      ", OUT_TABLE_HTML, "\n")
cat("Main table PNG:       ", OUT_TABLE_PNG, "\n")
cat("Main table PDF:       ", OUT_TABLE_PDF, "\n")
cat("Missing table HTML:   ", OUT_MISSING_HTML, "\n")
cat("Missing table PNG:    ", OUT_MISSING_PNG, "\n")
cat("Missing table PDF:    ", OUT_MISSING_PDF, "\n")
cat("Summary CSV:          ", OUT_SUMMARY_CSV, "\n")
cat("Missing CSV:          ", OUT_MISSING_CSV, "\n")
cat("ID audit CSV:         ", OUT_ID_AUDIT_CSV, "\n")
cat("Text audit:           ", OUT_TEXT, "\n")

# ===================================================

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

 ===================================================

# ============================================================
# INTERIM DEMOGRAPHICS — STAGE-SPECIFIC EXCEL EXPORT
# Corrected medication + trait anxiety columns:
#   medication status = incl_dem_med
#   baseline trait anxiety = anx_total
# ============================================================
#
# Output:
#   supplementary_interim_demographics/
#     Interim_Demographics_StageSpecific_Corrected.xlsx
#     Interim_Demographics_StageSpecific_Corrected.csv
#
# Logic:
#   - Screened / Passed screening from Interim Sign-Up.
#   - Tested / Analysed IDs from analysed interim dataset.
#   - Tested / Analysed demographics linked back to Sign-Up by part_id.
#   - Medication status explicitly from incl_dem_med.
#   - Baseline trait anxiety explicitly from anx_total.
#   - Categorical percentages use non-missing denominator.
#
# ============================================================

# install.packages(c("tidyverse", "lubridate", "writexl"))

library(tidyverse)
library(lubridate)
library(writexl)

# ============================================================
# 0. USER SETTINGS
# ============================================================

DATA_DIR <- Sys.getenv("MRC_DATA_DIR", unset = "C:/Users/dn284/Desktop/MRC_omni/data")

OUT_DIR <- file.path(DATA_DIR, "supplementary_interim_demographics")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

OUT_XLSX <- file.path(
  OUT_DIR,
  "Interim_Demographics_StageSpecific_Corrected.xlsx"
)

OUT_CSV <- file.path(
  OUT_DIR,
  "Interim_Demographics_StageSpecific_Corrected.csv"
)

# ============================================================
# 1. HELPERS
# ============================================================

clean_names_simple <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "") %>%
    str_to_lower()
}

standardise_names <- function(df) {
  old <- names(df)
  new <- clean_names_simple(old)
  names(df) <- make.unique(new, sep = "_dup")
  attr(df, "name_map") <- tibble(
    original_name = old,
    clean_name = names(df)
  )
  df
}

read_qualtrics_clean <- function(path) {
  raw <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
  
  names(raw) <- str_trim(names(raw))
  
  if (nrow(raw) > 0) {
    first_col <- as.character(raw[[1]])
    
    metadata_like <- str_detect(
      first_col,
      regex("^Start Date$|^End Date$|^\\{\"ImportId\"", ignore_case = TRUE)
    )
    
    raw <- raw[!metadata_like, , drop = FALSE]
  }
  
  raw %>% standardise_names()
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

num_clean <- function(x) {
  suppressWarnings(
    as.numeric(
      str_replace_all(as.character(x), "[^0-9\\.\\-]", "")
    )
  )
}

norm_text <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_to_lower()
}

find_existing_col <- function(df, candidates, label = "column", required = TRUE) {
  candidates_clean <- clean_names_simple(candidates)
  
  hit <- candidates_clean[candidates_clean %in% names(df)]
  if (length(hit) > 0) return(hit[1])
  
  for (cand in candidates_clean) {
    hit2 <- names(df)[str_detect(names(df), fixed(cand))]
    if (length(hit2) > 0) return(hit2[1])
  }
  
  if (required) {
    cat("\nAvailable columns for ", label, ":\n", sep = "")
    print(names(df))
    stop("Could not find ", label, ". Tried: ", paste(candidates_clean, collapse = ", "))
  }
  
  NA_character_
}

newest_match <- function(patterns, data_dir = DATA_DIR, required = TRUE) {
  all_files <- list.files(data_dir, recursive = TRUE, full.names = TRUE)
  file_names <- basename(all_files)
  
  generated <- str_detect(
    file_names,
    regex(
      "Audit|Denominator|Table_S|supplementary|Narrative|PrePost|Baseline|Debug|output|Excel|xlsx|Corrected",
      ignore_case = TRUE
    )
  )
  
  all_files <- all_files[!generated]
  file_names <- file_names[!generated]
  
  hits <- character(0)
  
  for (pat in patterns) {
    pat_regex <- pat
    pat_regex <- gsub("\\.", "\\\\.", pat_regex)
    pat_regex <- gsub("\\*", ".*", pat_regex)
    
    matched <- all_files[
      str_detect(file_names, regex(pat_regex, ignore_case = TRUE))
    ]
    
    hits <- c(hits, matched)
  }
  
  hits <- unique(hits[file.exists(hits)])
  
  if (length(hits) == 0) {
    if (required) stop("No files found for patterns: ", paste(patterns, collapse = ", "))
    return(NULL)
  }
  
  info <- file.info(hits)
  hits[order(info$mtime, decreasing = TRUE)][1]
}

latest_per_pid <- function(df, pid_col = "part_id") {
  date_col <- find_existing_col(
    df,
    c("recorded_date", "recordeddate", "end_date", "start_date"),
    label = "date column",
    required = FALSE
  )
  
  out <- df %>%
    filter(!is.na(.data[[pid_col]]), .data[[pid_col]] != "")
  
  if (is.na(date_col)) {
    return(out %>% distinct(.data[[pid_col]], .keep_all = TRUE))
  }
  
  out %>%
    mutate(
      .dt = suppressWarnings(
        parse_date_time(
          .data[[date_col]],
          orders = c(
            "ymd HMS", "ymd HM", "ymd",
            "dmy HMS", "dmy HM", "dmy",
            "mdy HMS", "mdy HM", "mdy"
          )
        )
      )
    ) %>%
    arrange(.data[[pid_col]], desc(.dt)) %>%
    distinct(.data[[pid_col]], .keep_all = TRUE) %>%
    select(-.dt)
}

recode_sex <- function(x) {
  xx <- norm_text(x)
  
  case_when(
    xx %in% c("female", "f", "woman", "2") ~ "Female",
    xx %in% c("male", "m", "man", "1") ~ "Male",
    xx %in% c("", "na", "n/a", "nan", "none", "prefer not to say") | is.na(xx) ~ NA_character_,
    TRUE ~ str_to_title(xx)
  )
}

recode_gender <- function(x) {
  xx <- norm_text(x)
  
  case_when(
    xx %in% c("female", "woman", "f", "2") ~ "Female",
    xx %in% c("male", "man", "m", "1") ~ "Male",
    str_detect(xx, "other|non|trans|fluid|queer|prefer|self") ~ "Other",
    xx %in% c("", "na", "n/a", "nan", "none") | is.na(xx) ~ NA_character_,
    TRUE ~ str_to_title(xx)
  )
}

recode_medication <- function(x) {
  xx <- norm_text(x)
  
  case_when(
    xx %in% c("yes", "y", "true", "1") ~ "Yes",
    xx %in% c("no", "n", "false", "0") ~ "No",
    str_detect(xx, "^yes") ~ "Yes",
    str_detect(xx, "^no") ~ "No",
    xx %in% c("", "na", "n/a", "nan", "none", "prefer not to say") | is.na(xx) ~ NA_character_,
    TRUE ~ NA_character_
  )
}

fmt_cont <- function(x, stage_n) {
  x <- x[!is.na(x)]
  n <- length(x)
  unavailable <- stage_n - n
  
  if (n == 0) {
    return(paste0("-\n(n = 0; unavailable = ", unavailable, ")"))
  }
  
  paste0(
    sprintf("%.2f", mean(x)),
    " ± ",
    sprintf("%.2f", sd(x)),
    " [",
    sprintf("%.2f", min(x)),
    "–",
    sprintf("%.2f", max(x)),
    "]\n(n = ",
    n,
    ifelse(unavailable > 0, paste0("; unavailable = ", unavailable), ""),
    ")"
  )
}

fmt_cat <- function(count, denom) {
  if (is.na(denom) || denom == 0) return("-")
  
  paste0(
    count,
    "/",
    denom,
    " (",
    sprintf("%.1f", 100 * count / denom),
    "%)"
  )
}

fmt_missing <- function(missing_n, stage_n) {
  if (missing_n == 0) return("-")
  paste0(missing_n, "/", stage_n, " unavailable")
}

# ============================================================
# 2. LOAD FILES
# ============================================================

INTERIM_SIGNUP_FILE <- newest_match(
  c("*Interim*Sign-Up*.csv", "*Interim*Sign-up*.csv", "*Interim*Signup*.csv")
)

INTERIM_ANALYSED_FILE <- newest_match(
  c("*n32apr20*.csv")
)

message("Interim sign-up file:  ", basename(INTERIM_SIGNUP_FILE))
message("Interim analysed file: ", basename(INTERIM_ANALYSED_FILE))

signup_raw <- read_qualtrics_clean(INTERIM_SIGNUP_FILE)
analysed_raw <- read_qualtrics_clean(INTERIM_ANALYSED_FILE)

# ============================================================
# 3. DETECT / SET COLUMNS
# ============================================================

signup_id_col <- find_existing_col(
  signup_raw,
  c("part_id", "participant_id", "pid", "participant"),
  label = "interim sign-up participant ID"
)

analysed_id_col <- find_existing_col(
  analysed_raw,
  c("part_id", "participant_id", "pid", "participant"),
  label = "interim analysed participant ID"
)

excluded_col <- find_existing_col(
  signup_raw,
  c("excluded", "exclude", "exclusion"),
  label = "interim sign-up excluded column",
  required = FALSE
)

# Explicit age column fix
age_col <- dplyr::case_when(
  "incl_dem_age" %in% names(signup_raw) ~ "incl_dem_age",
  "dem_age" %in% names(signup_raw) ~ "dem_age",
  "age" %in% names(signup_raw) ~ "age",
  TRUE ~ NA_character_
)

if (is.na(age_col)) {
  cat("\nAvailable Interim Sign-Up columns:\n")
  print(names(signup_raw))
  stop("Could not find age column. Expected one of: incl_dem_age, dem_age, age.")
}

sex_col <- find_existing_col(
  signup_raw,
  c("sex", "biological_sex"),
  label = "sex",
  required = FALSE
)

gender_col <- find_existing_col(
  signup_raw,
  c("gender"),
  label = "gender",
  required = FALSE
)

# Explicit fixes requested:
med_col <- "incl_dem_med"
trait_col <- "anx_total"

if (!med_col %in% names(signup_raw)) {
  cat("\nAvailable Interim Sign-Up columns:\n")
  print(names(signup_raw))
  stop("Expected medication column 'incl_dem_med' was not found after name cleaning.")
}

if (!trait_col %in% names(signup_raw)) {
  cat("\nAvailable Interim Sign-Up columns:\n")
  print(names(signup_raw))
  stop("Expected trait anxiety column 'anx_total' was not found after name cleaning.")
}

# ============================================================
# 4. HARMONISE SIGN-UP DEMOGRAPHICS
# ============================================================

signup <- signup_raw %>%
  mutate(
    part_id = clean_id(.data[[signup_id_col]]),
    
    excluded_raw = if (!is.na(excluded_col)) as.character(.data[[excluded_col]]) else NA_character_,
    excluded_clean = norm_text(excluded_raw),
    
    passed_screening = case_when(
      excluded_clean %in% c("false", "f", "0", "no", "n") ~ TRUE,
      excluded_clean %in% c("true", "t", "1", "yes", "y") ~ FALSE,
      is.na(excluded_clean) | excluded_clean == "" ~ NA,
      TRUE ~ FALSE
    ),
    
    age = if (!is.na(age_col)) num_clean(.data[[age_col]]) else NA_real_,
    sex_clean = if (!is.na(sex_col)) recode_sex(.data[[sex_col]]) else NA_character_,
    gender_clean = if (!is.na(gender_col)) recode_gender(.data[[gender_col]]) else NA_character_,
    
    # Explicit columns
    med_clean = recode_medication(.data[[med_col]]),
    trait_anxiety = num_clean(.data[[trait_col]])
  ) %>%
  filter(!is.na(part_id), part_id != "") %>%
  latest_per_pid("part_id")

analysed_ids <- analysed_raw %>%
  transmute(part_id = clean_id(.data[[analysed_id_col]])) %>%
  filter(!is.na(part_id), part_id != "") %>%
  distinct(part_id) %>%
  pull(part_id)

tested_ids <- analysed_ids

# Link tested/analysed IDs back to sign-up demographics.
participant_audit <- signup %>%
  mutate(
    screened = TRUE,
    passed_screening = passed_screening %in% TRUE,
    tested = part_id %in% tested_ids,
    analysed = part_id %in% analysed_ids
  ) %>%
  select(
    part_id,
    screened,
    passed_screening,
    tested,
    analysed,
    age,
    sex_clean,
    gender_clean,
    med_clean,
    trait_anxiety,
    excluded_raw,
    excluded_clean
  ) %>%
  arrange(part_id)

missing_from_signup <- tibble(
  part_id = setdiff(unique(c(tested_ids, analysed_ids)), participant_audit$part_id)
)

if (nrow(missing_from_signup) > 0) {
  participant_audit <- bind_rows(
    participant_audit,
    missing_from_signup %>%
      mutate(
        screened = FALSE,
        passed_screening = NA,
        tested = part_id %in% tested_ids,
        analysed = part_id %in% analysed_ids,
        age = NA_real_,
        sex_clean = NA_character_,
        gender_clean = NA_character_,
        med_clean = NA_character_,
        trait_anxiety = NA_real_,
        excluded_raw = NA_character_,
        excluded_clean = NA_character_
      )
  ) %>%
    arrange(part_id)
}

# ============================================================
# 5. BUILD STAGE DATASETS
# ============================================================

screened_df <- participant_audit %>% filter(screened %in% TRUE)
passed_df   <- participant_audit %>% filter(passed_screening %in% TRUE)
tested_df   <- participant_audit %>% filter(tested %in% TRUE)
analysed_df <- participant_audit %>% filter(analysed %in% TRUE)

# ============================================================
# 6. SUMMARY FUNCTIONS
# ============================================================

make_stage_table <- function(stage_df, stage_name) {
  stage_n <- nrow(stage_df)
  
  sex_known <- stage_df %>% filter(!is.na(sex_clean))
  gender_known <- stage_df %>% filter(!is.na(gender_clean))
  med_known <- stage_df %>% filter(!is.na(med_clean))
  
  tibble(
    characteristic = c(
      "Age",
      "Sex: Female",
      "Sex: Male",
      "Sex: Unavailable / missing",
      "Gender: Female",
      "Gender: Male",
      "Gender: Other",
      "Gender: Unavailable / missing",
      "Medication status: Yes",
      "Medication status: No",
      "Medication status: Unavailable / missing",
      "Baseline trait anxiety"
    ),
    value = c(
      fmt_cont(stage_df$age, stage_n),
      
      fmt_cat(sum(sex_known$sex_clean == "Female"), nrow(sex_known)),
      fmt_cat(sum(sex_known$sex_clean == "Male"), nrow(sex_known)),
      fmt_missing(stage_n - nrow(sex_known), stage_n),
      
      fmt_cat(sum(gender_known$gender_clean == "Female"), nrow(gender_known)),
      fmt_cat(sum(gender_known$gender_clean == "Male"), nrow(gender_known)),
      fmt_cat(sum(gender_known$gender_clean == "Other"), nrow(gender_known)),
      fmt_missing(stage_n - nrow(gender_known), stage_n),
      
      fmt_cat(sum(med_known$med_clean == "Yes"), nrow(med_known)),
      fmt_cat(sum(med_known$med_clean == "No"), nrow(med_known)),
      fmt_missing(stage_n - nrow(med_known), stage_n),
      
      fmt_cont(stage_df$trait_anxiety, stage_n)
    )
  ) %>%
    rename(!!stage_name := value)
}

make_stage_long <- function(stage_df, stage_name) {
  stage_n <- nrow(stage_df)
  
  cont_row <- function(variable_label, x) {
    x2 <- x[!is.na(x)]
    
    tibble(
      Stage = stage_name,
      Variable = variable_label,
      Level = NA_character_,
      Stage_denominator = stage_n,
      Denominator = length(x2),
      Non_missing_n = length(x2),
      Missing_n = stage_n - length(x2),
      Count = NA_integer_,
      Percent = NA_real_,
      Mean = ifelse(length(x2) > 0, mean(x2), NA_real_),
      SD = ifelse(length(x2) > 1, sd(x2), NA_real_),
      Min = ifelse(length(x2) > 0, min(x2), NA_real_),
      Max = ifelse(length(x2) > 0, max(x2), NA_real_)
    )
  }
  
  cat_rows <- function(variable_label, vec, levels_keep) {
    known <- vec[!is.na(vec)]
    denom <- length(known)
    
    bind_rows(
      map_dfr(levels_keep, function(lvl) {
        cnt <- sum(known == lvl)
        
        tibble(
          Stage = stage_name,
          Variable = variable_label,
          Level = lvl,
          Stage_denominator = stage_n,
          Denominator = denom,
          Non_missing_n = denom,
          Missing_n = stage_n - denom,
          Count = cnt,
          Percent = ifelse(denom > 0, 100 * cnt / denom, NA_real_),
          Mean = NA_real_,
          SD = NA_real_,
          Min = NA_real_,
          Max = NA_real_
        )
      }),
      tibble(
        Stage = stage_name,
        Variable = variable_label,
        Level = "Unavailable / missing",
        Stage_denominator = stage_n,
        Denominator = stage_n,
        Non_missing_n = denom,
        Missing_n = stage_n - denom,
        Count = stage_n - denom,
        Percent = ifelse(stage_n > 0, 100 * (stage_n - denom) / stage_n, NA_real_),
        Mean = NA_real_,
        SD = NA_real_,
        Min = NA_real_,
        Max = NA_real_
      )
    )
  }
  
  bind_rows(
    cont_row("Age", stage_df$age),
    cat_rows("Sex", stage_df$sex_clean, c("Female", "Male")),
    cat_rows("Gender", stage_df$gender_clean, c("Female", "Male", "Other")),
    cat_rows("Medication status", stage_df$med_clean, c("Yes", "No")),
    cont_row("Baseline trait anxiety", stage_df$trait_anxiety)
  )
}

# ============================================================
# 7. CREATE FINAL TABLES
# ============================================================

screened_table <- make_stage_table(screened_df, "Screened")
passed_table   <- make_stage_table(passed_df, "Passed screening")
tested_table   <- make_stage_table(tested_df, "Tested")
analysed_table <- make_stage_table(analysed_df, "Analysed")

screening_wide <- screened_table %>%
  left_join(passed_table, by = "characteristic") %>%
  mutate(section = "Screening stages") %>%
  select(section, characteristic, Screened, `Passed screening`)

testing_wide <- tested_table %>%
  left_join(analysed_table, by = "characteristic") %>%
  mutate(section = "Testing and analysed stages") %>%
  select(section, characteristic, Tested, Analysed)

interim_demographics_table <- bind_rows(screening_wide, testing_wide)

interim_demographics_long <- bind_rows(
  make_stage_long(screened_df, "Screened"),
  make_stage_long(passed_df, "Passed screening"),
  make_stage_long(tested_df, "Tested"),
  make_stage_long(analysed_df, "Analysed")
)

stage_counts <- tibble(
  stage = c("Screened", "Passed screening", "Tested", "Analysed"),
  n = c(nrow(screened_df), nrow(passed_df), nrow(tested_df), nrow(analysed_df))
)

detected_columns <- tibble(
  item = c(
    "Interim sign-up file",
    "Interim analysed file",
    "Sign-up ID column",
    "Analysed ID column",
    "Excluded column",
    "Age column",
    "Sex column",
    "Gender column",
    "Medication column",
    "Trait anxiety column"
  ),
  value = c(
    basename(INTERIM_SIGNUP_FILE),
    basename(INTERIM_ANALYSED_FILE),
    signup_id_col,
    analysed_id_col,
    ifelse(is.na(excluded_col), "Not found", excluded_col),
    ifelse(is.na(age_col), "Not found", age_col),
    ifelse(is.na(sex_col), "Not found", sex_col),
    ifelse(is.na(gender_col), "Not found", gender_col),
    med_col,
    trait_col
  )
)

notes <- tibble(
  note = c(
    "Interim Study Stage-Specific Demographic and Baseline Characteristics.",
    "Continuous variables are mean ± SD [range].",
    "Categorical variables are n / non-missing denominator (%).",
    "Unavailable records are shown separately.",
    "Screened and passed-screening denominators are derived from the Interim Sign-Up file.",
    "Tested and analysed denominators are derived from the analysed interim dataset, with demographics linked back to Interim Sign-Up by participant ID.",
    "Medication status is derived from incl_dem_med.",
    "Baseline trait anxiety is derived from anx_total."
  )
)

# ============================================================
# 8. EXPORT
# ============================================================

readr::write_csv(interim_demographics_table, OUT_CSV)

excel_sheets <- list(
  "Interim demographics table" = as.data.frame(interim_demographics_table),
  "Long numeric summary" = as.data.frame(interim_demographics_long),
  "Stage counts" = as.data.frame(stage_counts),
  "Participant audit" = as.data.frame(participant_audit),
  "Detected columns" = as.data.frame(detected_columns),
  "Notes" = as.data.frame(notes)
)

writexl::write_xlsx(excel_sheets, OUT_XLSX)

# ============================================================
# 9. CONSOLE OUTPUT
# ============================================================

cat("\n============================================================\n")
cat("INTERIM DEMOGRAPHICS EXPORT COMPLETE\n")
cat("============================================================\n\n")

cat("Stage counts:\n")
print(stage_counts, n = Inf)

cat("\nDetected columns:\n")
print(detected_columns, n = Inf)

cat("\nFinal table preview:\n")
print(interim_demographics_table, n = Inf, width = Inf)

cat("\nLong numeric preview:\n")
print(interim_demographics_long, n = Inf, width = Inf)

cat("\nOutputs:\n")
cat("  ", OUT_XLSX, "\n")
cat("  ", OUT_CSV, "\n")

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



parameter bins, sonata form structure, sequence condensation, interim validation, fidelity summary, fidelity per segment

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

# ============================================================
# SUPPLEMENTARY ALGORITHM TABLES + AUTO-NARRATIVE
# Sonata-form SLS design and Interim sequence condensation
# ============================================================
#
# Outputs:
#   1. S5a_algorithm_sonata_to_sls.html/png/pdf
#   2. S5b_algorithm_condensation.html/png/pdf
#   3. S5c_algorithm_validation_metrics.html/png/pdf
#   4. S5d_parameter_bins.html/png/pdf
#   5. S5e_interim_fidelity_summary.html/png/pdf
#   6. S5f_segment_level_fidelity.html/png/pdf
#   7. Supplementary_Algorithm_Narrative_AutoGenerated.txt
#
# Notes:
# - PNG/PDF export requires webshot2/chromium availability.
# - HTML export should always work.
# - Edit DATA_DIR and FONT_PATH as needed.
# ============================================================

# ============================================================
# 0. Packages
# ============================================================

needed <- c(
  "tidyverse",
  "gt",
  "glue",
  "scales",
  "showtext",
  "sysfonts",
  "webshot2"
)

for (pkg in needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(tidyverse)
library(gt)
library(glue)
library(scales)
library(showtext)
library(sysfonts)

# ============================================================
# 1. Settings
# ============================================================

DATA_DIR <- Sys.getenv("MRC_DATA_DIR", unset = "C:/Users/dn284/Desktop/MRC_omni/data")
OUT_DIR  <- file.path(DATA_DIR, "supplementary_algorithm_tables")

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

FONT_PATH <- file.path(DATA_DIR, "palatinolinotype_roman.ttf")

if (file.exists(FONT_PATH)) {
  font_add("PalatinoLinotype", regular = FONT_PATH)
  showtext_auto()
  TABLE_FONT <- "PalatinoLinotype"
  message("Loaded Palatino Linotype.")
} else {
  TABLE_FONT <- "serif"
  message("Palatino Linotype not found; using serif fallback.")
}

# ------------------------------------------------------------
# IMPORTANT EDITABLE CONSISTENCY SETTINGS
# ------------------------------------------------------------
# Your pasted SM text says 10 Hz / 100 ms.
# The older sonata-form PDF says 1000 Hz / 1 ms.
# Set this to the value you want reported in the final supplement.

SEQUENCE_RENDERING_RATE <- "10 Hz"
SEQUENCE_TEMPORAL_RESOLUTION <- "100 ms"

# General styling
TABLE_WIDTH_PX <- 1500
TABLE_FONT_SIZE <- 15
TABLE_HEADER_SIZE <- 18
TABLE_TITLE_SIZE <- 22

# ============================================================
# 2. Helper functions
# ============================================================

save_gt_all <- function(gt_obj, filename_stub, out_dir = OUT_DIR) {
  html_path <- file.path(out_dir, paste0(filename_stub, ".html"))
  png_path  <- file.path(out_dir, paste0(filename_stub, ".png"))
  pdf_path  <- file.path(out_dir, paste0(filename_stub, ".pdf"))
  
  gt::gtsave(gt_obj, html_path)
  message("Saved HTML: ", html_path)
  
  tryCatch({
    gt::gtsave(gt_obj, png_path, vwidth = TABLE_WIDTH_PX, expand = 10)
    message("Saved PNG:  ", png_path)
  }, error = function(e) {
    message("PNG export skipped: ", e$message)
  })
  
  tryCatch({
    gt::gtsave(gt_obj, pdf_path, vwidth = TABLE_WIDTH_PX)
    message("Saved PDF:  ", pdf_path)
  }, error = function(e) {
    message("PDF export skipped: ", e$message)
  })
  
  invisible(list(html = html_path, png = png_path, pdf = pdf_path))
}

style_algorithm_gt <- function(gt_tbl, title, subtitle = NULL) {
  gt_tbl %>%
    tab_header(
      title = md(glue("**{title}**")),
      subtitle = if (!is.null(subtitle)) md(subtitle) else NULL
    ) %>%
    tab_options(
      table.font.names = TABLE_FONT,
      table.font.size = px(TABLE_FONT_SIZE),
      heading.title.font.size = px(TABLE_TITLE_SIZE),
      heading.subtitle.font.size = px(TABLE_HEADER_SIZE),
      column_labels.font.weight = "bold",
      column_labels.font.size = px(TABLE_HEADER_SIZE),
      table.border.top.width = px(0),
      table.border.bottom.width = px(0),
      column_labels.border.top.width = px(1),
      column_labels.border.bottom.width = px(1),
      row.striping.include_table_body = TRUE,
      row.striping.background_color = "#F7F7F7",
      data_row.padding = px(7),
      table.width = pct(100)
    ) %>%
    opt_table_outline(style = "solid", width = px(0.5), color = "#B8B8B8") %>%
    opt_row_striping() %>%
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style = cell_text(align = "left"),
      locations = cells_body()
    )
}

fmt_numeric_clean <- function(x, digits = 2) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

# ============================================================
# 3. Table S5a: Sonata-to-SLS algorithm
# ============================================================

sonata_algorithm <- tribble(
  ~Step, ~Component, ~Operation, ~Equation_or_rule, ~Purpose,
  "1", "Musical source selection",
  "Five sonatas were used as structural templates.",
  "Mozart K.545; Schubert D.960; Beethoven Op.13, Op.53, Op.27 No.2",
  "To generate varied but musically structured SLS envelopes.",
  
  "2", "Feature extraction",
  "MIDI/MusicXML files were parsed to estimate dynamic intensity, note density, harmonic tension, and rhythmic complexity.",
  "D(t), N(t), H(t), R(t)",
  "To convert musical structure into time-varying stimulation intensity.",
  
  "3", "Dynamic mapping",
  "Nominal dynamic markings were mapped to scalar values.",
  "p = 0.20; mp = 0.35; mf = 0.50; f = 0.80; ff = 1.00",
  "To provide an interpretable musical intensity anchor.",
  
  "4", "Master intensity curve",
  "Extracted musical features were combined into a smoothed intensity trajectory.",
  "I(t) = 0.4D(t) + 0.3N(t) + 0.2H(t) + 0.1R(t)",
  "To produce a single continuous control signal.",
  
  "5", "Frequency mapping",
  "Intensity was linearly mapped onto the active SLS frequency range.",
  "f(t) = 3 + 12I(t); range = 3–15 Hz",
  "To keep active stimulation within the WP1-acceptable frequency range.",
  
  "6", "Duty-cycle mapping",
  "Intensity was mapped to pulse width.",
  "d(t) = 25 + 50I(t); range = 25–75% ON",
  "To increase pulse salience during higher-intensity passages.",
  
  "7", "Luminance mapping",
  "Intensity was mapped to brightness.",
  "L(t) = 10 + 90I(t); range = 10–100%",
  "To increase perceived intensity while preserving a bounded luminance range.",
  
  "8", "Sonata-form envelope",
  "Each sequence was shaped into exposition, development, and resolution phases.",
  "0–105 s; 105–240 s; 240–300 s",
  "To preserve a recognisable opening, peak, and tapering structure.",
  
  "9", "Special phases",
  "Wash-light and full-darkness intervals were inserted where appropriate.",
  "Wash = all oscillators at 60 Hz; darkness = luminance 0",
  "To provide perceptual contrast and transitional structure.",
  
  "10", "Rendering and export",
  "Final arrays were rendered and exported for RX1 deployment.",
  glue("Rendering = {SEQUENCE_RENDERING_RATE} ({SEQUENCE_TEMPORAL_RESOLUTION}); output = [f(t), d(t), L(t)] .txt/STP arrays"),
  "To create device-ready SLS sequences."
)

tbl_s5a <- sonata_algorithm %>%
  gt() %>%
  cols_label(
    Step = "Step",
    Component = "Component",
    Operation = "Algorithmic operation",
    Equation_or_rule = "Equation / rule",
    Purpose = "Rationale"
  ) %>%
  fmt_markdown(columns = everything()) %>%
  cols_width(
    Step ~ px(55),
    Component ~ px(190),
    Operation ~ px(360),
    Equation_or_rule ~ px(330),
    Purpose ~ px(360)
  ) %>%
  style_algorithm_gt(
    title = "Table S5a. Sonata-form algorithm for generating stroboscopic light sequences",
    subtitle = "Musical information was converted into time-varying frequency, duty-cycle, and luminance parameters."
  )

save_gt_all(tbl_s5a, "S5a_algorithm_sonata_to_sls")

# ============================================================
# 4. Table S5b: Condensation algorithm
# ============================================================

condensation_algorithm <- tribble(
  ~Step, ~Criterion, ~Operational_definition, ~Equation_or_rule, ~Interpretation,
  "1", "Compression factor",
  "A 30-minute candidate sequence was condensed to a shorter Interim Study sequence.",
  "alpha = T_condensed / T_original",
  "Defines the target temporal reduction.",
  
  "2", "Segment proportionality",
  "Each original segment contributed proportionally to the condensed sequence.",
  "T'_S = alpha × T_S",
  "Prevents over- or under-representation of any major structural phase.",
  
  "3", "Cumulative duration function",
  "Candidate windows were located using cumulative step durations.",
  "C_k = sum_{i=1}^{k} d_i",
  "Allows contiguous windows to be selected from the original sequence.",
  
  "4", "Candidate window selection",
  "Candidate windows were required to approximate the target compressed duration.",
  "C_end − C_start ≈ T'_S",
  "Preserves temporal density while avoiding arbitrary rescaling.",
  
  "5", "Frequency centroid",
  "The amplitude-weighted frequency centroid was calculated for each segment.",
  "f_c = sum(f_i A_i) / sum(A_i)",
  "Summarises the dominant frequency content of the segment.",
  
  "6", "Luminance centroid",
  "The duration-weighted luminance centroid was calculated for each segment.",
  "L_c = sum(L_i T_i) / sum(T_i)",
  "Summarises the dominant brightness level of the segment.",
  
  "7", "Within-window deviation",
  "Candidate windows were scored by frequency and luminance deviation from their centroids.",
  "E(W_k) = sigma^2_freq(W_k) + sigma^2_lum(W_k)",
  "Lower values indicate better parameter representativeness.",
  
  "8", "Transition penalty",
  "Candidate windows were penalised for abrupt boundary discontinuities.",
  "E_total(W_k) = E(W_k) + lambda(Delta_freq + Delta_lum)",
  "Favours smooth transitions between retained snippets.",
  
  "9", "Duty-cycle contrast scaling",
  "Duty cycles were adjusted to preserve relative contrast structure.",
  "D' = D_min + (D − D_min)(C_new / C_original)",
  "Keeps pulse-width variation proportional after condensation.",
  
  "10", "Perceptual luminance scaling",
  "A logarithmic brightness transform was used to reduce oversaturation at high intensities.",
  "L' = L_ref × log2(1 + L / L_base)",
  "Maintains perceptual gradation across low and high brightness periods.",
  
  "11", "Final snippet selection",
  "The candidate with lowest combined deviation and acceptable continuity was selected.",
  "minimise: sigma^2_freq + sigma^2_lum + transition penalty",
  "Produces a condensed sequence that remains representative of the original."
)

tbl_s5b <- condensation_algorithm %>%
  gt() %>%
  cols_label(
    Step = "Step",
    Criterion = "Criterion",
    Operational_definition = "Operational definition",
    Equation_or_rule = "Equation / rule",
    Interpretation = "Interpretation"
  ) %>%
  fmt_markdown(columns = everything()) %>%
  cols_width(
    Step ~ px(55),
    Criterion ~ px(210),
    Operational_definition ~ px(360),
    Equation_or_rule ~ px(340),
    Interpretation ~ px(360)
  ) %>%
  style_algorithm_gt(
    title = "Table S5b. Algorithm for condensing full-length SLS sequences",
    subtitle = "Representative subsections were selected by preserving segment duration, parameter centroids, within-segment variability, and transition smoothness."
  )

save_gt_all(tbl_s5b, "S5b_algorithm_condensation")

# ============================================================
# 5. Table S5c: Validation metric definitions
# ============================================================

validation_algorithm <- tribble(
  ~Metric, ~Definition, ~Equation_or_rule, ~Direction_of_interpretation,
  "Mean absolute difference",
  "Average absolute parameter deviation between full and condensed sequences.",
  "MAD = mean(|A_i − B_i|)",
  "Lower values indicate closer absolute similarity.",
  
  "Relative difference",
  "Mean absolute difference expressed relative to the full-sequence mean.",
  "Relative difference = MAD / mean(A)",
  "Allows comparison across parameters with different units.",
  
  "Pairwise distance matrix",
  "Absolute pointwise difference between full and condensed sequences.",
  "D(i,j) = |A_i − B_j|",
  "Forms the input matrix for Dynamic Time Warping.",
  
  "Cumulative DTW cost",
  "Minimum accumulated alignment cost across allowable temporal paths.",
  "C(i,j) = D(i,j) + min{C(i−1,j), C(i,j−1), C(i−1,j−1)}",
  "Lower values indicate closer temporally warped alignment.",
  
  "Final DTW distance",
  "Cumulative cost at the terminal alignment point.",
  "DTW(A,B) = C(N,M)",
  "Represents total sequence deviation after temporal alignment.",
  
  "Normalised DTW",
  "DTW distance divided by the full-sequence segment length.",
  "Normalised DTW_S = DTW_S / length(S_full)",
  "Expresses deviation per original-sequence step.",
  
  "Length-weighted total DTW",
  "Segment DTW values averaged after weighting by original segment length.",
  "Total = sum(DTW_S) / sum(length(S_full))",
  "Prevents shorter or more compressed segments from dominating the summary.",
  
  "Exclusion of wash/dark phases",
  "Wash-ramp and full-darkness phases were excluded from fidelity comparisons.",
  "Validation computed on active stimulation segments only.",
  "Ensures fidelity estimates reflect active SLS parameter structure."
)

tbl_s5c <- validation_algorithm %>%
  gt() %>%
  cols_label(
    Metric = "Validation metric",
    Definition = "Definition",
    Equation_or_rule = "Equation / rule",
    Direction_of_interpretation = "Interpretation"
  ) %>%
  fmt_markdown(columns = everything()) %>%
  cols_width(
    Metric ~ px(260),
    Definition ~ px(390),
    Equation_or_rule ~ px(420),
    Direction_of_interpretation ~ px(300)
  ) %>%
  style_algorithm_gt(
    title = "Table S5c. Sequence-fidelity validation metrics",
    subtitle = "Condensed sequences were compared with the full candidate sequence using absolute, relative, and temporally aligned deviation metrics."
  )

save_gt_all(tbl_s5c, "S5c_algorithm_validation_metrics")

# ============================================================
# 6. Table S5d: Parameter bins
# ============================================================

parameter_bins <- tribble(
  ~Parameter, ~Low, ~Medium, ~High, ~Special_phase,
  "Frequency", "3–7 Hz", "7–11 Hz", "11–15 Hz", "Wash phase = 60 Hz",
  "Duty cycle", "25–40% ON", "40–60% ON", "60–75% ON", "N/A",
  "Luminance", "10–30%", "30–70%", "70–100%", "Full darkness = 0%"
)

tbl_s5d <- parameter_bins %>%
  gt() %>%
  cols_label(
    Parameter = "Parameter",
    Low = "Low",
    Medium = "Medium",
    High = "High",
    Special_phase = "Special phase"
  ) %>%
  fmt_markdown(columns = everything()) %>%
  cols_width(
    Parameter ~ px(210),
    Low ~ px(210),
    Medium ~ px(210),
    High ~ px(210),
    Special_phase ~ px(310)
  ) %>%
  style_algorithm_gt(
    title = "Table S5d. Stroboscopic parameter bins used for sequence construction",
    subtitle = "Frequency, duty-cycle, and luminance values were grouped into low, medium, and high experiential-intensity bands."
  )

save_gt_all(tbl_s5d, "S5d_parameter_bins")

# ============================================================
# 7. Table S5e: Interim fidelity summary
# ============================================================

fidelity_summary <- tribble(
  ~Parameter, ~Mean_absolute_difference, ~Relative_difference_percent,
  ~Accumulated_DTW, ~Per_step_DTW,
  "Frequency", 1.12, 12.43, 1.92, 0.32,
  "Duty cycle", 1.68, 2.89, 0.47, 0.078,
  "Luminance", 4.23, 14.02, 2.44, 0.47
)

tbl_s5e <- fidelity_summary %>%
  gt() %>%
  cols_label(
    Parameter = "Parameter",
    Mean_absolute_difference = "Mean absolute difference",
    Relative_difference_percent = "Relative difference (%)",
    Accumulated_DTW = "Accumulated DTW",
    Per_step_DTW = "Per-step DTW"
  ) %>%
  fmt_number(
    columns = c(Mean_absolute_difference, Relative_difference_percent, Accumulated_DTW),
    decimals = 2
  ) %>%
  fmt_number(
    columns = Per_step_DTW,
    decimals = 3
  ) %>%
  tab_spanner(
    label = "Direct parameter deviation",
    columns = c(Mean_absolute_difference, Relative_difference_percent)
  ) %>%
  tab_spanner(
    label = "Dynamic Time Warping deviation",
    columns = c(Accumulated_DTW, Per_step_DTW)
  ) %>%
  tab_footnote(
    footnote = "Units are Hz for frequency, percentage points for duty cycle, and percentage brightness for luminance.",
    locations = cells_column_labels(columns = Mean_absolute_difference)
  ) %>%
  tab_footnote(
    footnote = "Per-step DTW values express accumulated alignment deviation after adjustment for full-sequence segment length.",
    locations = cells_column_labels(columns = Per_step_DTW)
  ) %>%
  style_algorithm_gt(
    title = "Table S5e. Overall fidelity between full and condensed Interim Study SLS sequences",
    subtitle = "Summary deviation metrics indicate close preservation of active stimulation parameters after temporal condensation."
  )

save_gt_all(tbl_s5e, "S5e_interim_fidelity_summary")

# ============================================================
# 8. Table S5f: Segment-level fidelity
# ============================================================

segment_fidelity <- tribble(
  ~Segment, ~Parameter, ~Full_mean, ~Condensed_mean, ~Mean_absolute_difference,
  ~Relative_difference_percent, ~Accumulated_DTW, ~Per_step_DTW,
  
  "A", "Frequency", 8.20, 6.54, 1.66, 20.10, 1.72, 0.287,
  "A", "Duty cycle", 55.36, 54.00, 1.36, 2.46, 0.45, 0.075,
  "A", "Luminance", 25.71, 21.00, 4.71, 18.30, 2.42, 0.403,
  
  "B", "Frequency", 7.06, 7.10, 0.86, 10.75, 2.05, 0.342,
  "B", "Duty cycle", 54.79, 53.20, 1.59, 2.89, 0.51, 0.085,
  "B", "Luminance", 25.65, 20.00, 5.65, 22.04, 3.02, 0.503,
  
  "C", "Frequency", 7.81, 6.23, 1.58, 19.98, 1.86, 0.310,
  "C", "Duty cycle", 56.56, 54.79, 1.77, 3.14, 0.47, 0.783,
  "C", "Luminance", 27.50, 22.50, 5.00, 18.18, 2.89, 0.482,
  
  "Peak", "Frequency", 10.17, 10.16, 0.01, 0.17, 1.97, 0.328,
  "Peak", "Duty cycle", 61.40, 60.80, 0.60, 0.98, 0.49, 0.0812,
  "Peak", "Luminance", 53.00, 47.00, 6.00, 11.32, 3.15, 0.525,
  
  "D", "Frequency", 9.30, 8.43, 0.87, 9.39, 1.75, 0.292,
  "D", "Duty cycle", 59.40, 57.50, 1.90, 3.20, 0.44, 0.073,
  "D", "Luminance", 31.00, 26.00, 5.00, 16.13, 2.71, 0.452,
  
  "E", "Frequency", 8.96, 8.09, 0.87, 9.79, 1.58, 0.263,
  "E", "Duty cycle", 55.29, 52.40, 2.89, 5.22, 0.40, 0.067,
  "E", "Luminance", 28.57, 24.00, 4.57, 16.00, 2.23, 0.372
)

tbl_s5f <- segment_fidelity %>%
  mutate(
    Segment = factor(Segment, levels = c("A", "B", "C", "Peak", "D", "E")),
    Parameter = factor(Parameter, levels = c("Frequency", "Duty cycle", "Luminance"))
  ) %>%
  arrange(Segment, Parameter) %>%
  gt(groupname_col = "Segment") %>%
  cols_label(
    Parameter = "Parameter",
    Full_mean = "Full sequence mean",
    Condensed_mean = "Condensed sequence mean",
    Mean_absolute_difference = "Mean absolute difference",
    Relative_difference_percent = "Relative difference (%)",
    Accumulated_DTW = "Accumulated DTW",
    Per_step_DTW = "Per-step DTW"
  ) %>%
  fmt_number(
    columns = c(
      Full_mean,
      Condensed_mean,
      Mean_absolute_difference,
      Relative_difference_percent,
      Accumulated_DTW
    ),
    decimals = 2
  ) %>%
  fmt_number(
    columns = Per_step_DTW,
    decimals = 3
  ) %>%
  tab_spanner(
    label = "Parameter means",
    columns = c(Full_mean, Condensed_mean)
  ) %>%
  tab_spanner(
    label = "Direct deviation",
    columns = c(Mean_absolute_difference, Relative_difference_percent)
  ) %>%
  tab_spanner(
    label = "DTW deviation",
    columns = c(Accumulated_DTW, Per_step_DTW)
  ) %>%
  style_algorithm_gt(
    title = "Table S5f. Segment-level fidelity between full and condensed SLS sequences",
    subtitle = "Segment-wise comparison of full-sequence and condensed-sequence frequency, duty-cycle, and luminance parameters."
  )

save_gt_all(tbl_s5f, "S5f_segment_level_fidelity")

# ============================================================
# 9. Auto-generated narrative
# ============================================================

total_freq <- fidelity_summary %>% filter(Parameter == "Frequency")
total_duty <- fidelity_summary %>% filter(Parameter == "Duty cycle")
total_lum  <- fidelity_summary %>% filter(Parameter == "Luminance")

max_rel <- segment_fidelity %>%
  arrange(desc(Relative_difference_percent)) %>%
  slice(1)

min_rel <- segment_fidelity %>%
  arrange(Relative_difference_percent) %>%
  slice(1)

narrative <- glue(
  "Supplementary Algorithm Narrative: Sonata-form SLS generation and Interim sequence condensation

Sonata-form sequence generation

The Intervention and Control SLS sequences were generated using a structured sonata-form algorithm. Five classical piano sonatas were used as musical templates: Mozart K.545, Schubert D.960, Beethoven Op.13 (Pathétique), Beethoven Op.53 (Waldstein), and Beethoven Op.27 No.2 (Moonlight). Each source was parsed to extract dynamic markings, note density, harmonic tension, and rhythmic complexity. These features were combined into a smoothed master intensity function:

I(t) = 0.4D(t) + 0.3N(t) + 0.2H(t) + 0.1R(t),

where D(t) denotes mapped dynamic intensity, N(t) denotes local note density, H(t) denotes harmonic tension, and R(t) denotes rhythmic complexity.

The resulting intensity function was mapped onto three device-relevant SLS parameters. Frequency was mapped as f(t) = 3 + 12I(t), giving an active stimulation range of 3–15 Hz. Duty cycle was mapped as d(t) = 25 + 50I(t), producing a pulse-width range of 25–75% ON. Luminance was mapped as L(t) = 10 + 90I(t), producing a brightness range of 10–100%. Each approximately 300-second sequence was shaped into an exposition phase, a development phase, and a resolution phase. The exposition gradually increased the intensity envelope, the development retained or boosted the central stimulation profile, and the resolution tapered the sequence toward a lower-intensity ending. Wash-light phases were defined by setting all oscillators to 60 Hz, whereas full-darkness phases were defined by setting luminance to 0. Sequences were rendered at {SEQUENCE_RENDERING_RATE} ({SEQUENCE_TEMPORAL_RESOLUTION}) and exported as device-ready arrays containing frequency, duty-cycle, and luminance values.

Interim Study condensation algorithm

For the Interim Study, shorter SLS sequences were required to test whether the proposed strobe-to-wash-light amplitude ratio reduced subjective visual experience before this ratio was used in the longer intervention protocol. Because administering the full 30-minute candidate sequence was not feasible for this validation step, a condensed sequence was derived from the full-length candidate while preserving its segmental structure and parameter profile.

The condensation algorithm first computed a compression factor:

alpha = T_condensed / T_original.

This factor was used to define the target duration of each retained segment. Rather than uniformly rescaling all steps, contiguous candidate windows were selected from the original sequence. Candidate windows were required to preserve segment proportionality, approximate the target compressed duration, and retain the original segment's frequency and luminance centroids. The amplitude-weighted frequency centroid was calculated as:

f_c = sum(f_i A_i) / sum(A_i),

and the duration-weighted luminance centroid was calculated as:

L_c = sum(L_i T_i) / sum(T_i).

Candidate windows were scored by their combined frequency and luminance variance from these centroids. Additional transition penalties were applied when candidate windows introduced abrupt shifts at segment boundaries. The selected windows therefore minimised parameter distortion while preserving smooth transitions and the broader temporal structure of the original sequence.

Duty-cycle and luminance adjustments were applied to preserve perceptual structure after temporal compression. Duty cycles were contrast-scaled to retain proportional variation between low- and high-contrast periods. Luminance values were transformed using a logarithmic perceptual-brightness mapping to reduce oversaturation during high-intensity moments while retaining relative brightness differences across the sequence.

Fidelity validation

Fidelity between the full and condensed sequences was assessed using mean absolute differences, relative differences, and Dynamic Time Warping (DTW). DTW was used because the condensed sequence was necessarily shorter than the full candidate sequence, making pointwise comparison alone insufficient.

Pairwise distances were first calculated as:

D(i,j) = |A_i - B_j|,

where A_i denotes a point in the full sequence and B_j denotes a point in the condensed sequence. A cumulative cost matrix was then computed using the standard recurrence:

C(i,j) = D(i,j) + min[C(i-1,j), C(i,j-1), C(i-1,j-1)].

The final DTW distance was defined as C(N,M), and segment-wise DTW values were normalised by the corresponding full-sequence segment length. Wash-ramp and full-darkness phases were excluded from these fidelity analyses so that validation focused on active stimulation structure.

Across all active segments, the condensed sequence showed a mean absolute frequency difference of {fmt_numeric_clean(total_freq$Mean_absolute_difference, 2)} Hz ({fmt_numeric_clean(total_freq$Relative_difference_percent, 2)}%), a duty-cycle difference of {fmt_numeric_clean(total_duty$Mean_absolute_difference, 2)} percentage points ({fmt_numeric_clean(total_duty$Relative_difference_percent, 2)}%), and a luminance difference of {fmt_numeric_clean(total_lum$Mean_absolute_difference, 2)} percentage points ({fmt_numeric_clean(total_lum$Relative_difference_percent, 2)}%). Length-adjusted per-step DTW deviations were {fmt_numeric_clean(total_freq$Per_step_DTW, 3)} Hz for frequency, {fmt_numeric_clean(total_duty$Per_step_DTW, 3)} percentage points for duty cycle, and {fmt_numeric_clean(total_lum$Per_step_DTW, 3)} percentage points for luminance.

Segment-level analyses indicated that the central peak segment was especially well preserved for frequency, with a full-sequence mean of 10.17 Hz and a condensed-sequence mean of 10.16 Hz. The largest relative segment-level deviation was observed for {max_rel$Parameter} in Segment {max_rel$Segment}, with a relative difference of {fmt_numeric_clean(max_rel$Relative_difference_percent, 2)}%. The smallest relative segment-level deviation was observed for {min_rel$Parameter} in Segment {min_rel$Segment}, with a relative difference of {fmt_numeric_clean(min_rel$Relative_difference_percent, 2)}%. Overall, these results indicate that the condensed Interim Study sequence retained the central temporal and parametric features of the full-length candidate sequence while reducing the duration to a feasible validation format.

Recommended supplement placement

Tables S5a–S5d define the sequence-generation and condensation algorithms. Tables S5e–S5f provide the corresponding fidelity checks. Together, these tables allow the supplementary material to present the algorithmic logic and empirical validation separately: first explaining how the sequences were constructed, then demonstrating that the condensed Interim Study sequence remained representative of the full candidate sequence.
"
)

narrative_path <- file.path(OUT_DIR, "Supplementary_Algorithm_Narrative_AutoGenerated.txt")
writeLines(narrative, narrative_path)

message("Saved narrative: ", narrative_path)

# ============================================================
# 10. Optional: write clean CSV versions of all tables
# ============================================================

write_csv(sonata_algorithm, file.path(OUT_DIR, "S5a_algorithm_sonata_to_sls.csv"))
write_csv(condensation_algorithm, file.path(OUT_DIR, "S5b_algorithm_condensation.csv"))
write_csv(validation_algorithm, file.path(OUT_DIR, "S5c_algorithm_validation_metrics.csv"))
write_csv(parameter_bins, file.path(OUT_DIR, "S5d_parameter_bins.csv"))
write_csv(fidelity_summary, file.path(OUT_DIR, "S5e_interim_fidelity_summary.csv"))
write_csv(segment_fidelity, file.path(OUT_DIR, "S5f_segment_level_fidelity.csv"))

message("All supplementary algorithm tables and narrative outputs complete.")
message("Output directory: ", OUT_DIR)

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


# ------------------------------------------------------------
# SECTION: interim parameters
# ------------------------------------------------------------

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

# ============================================================
# INTERIM SLS SUPPLEMENTARY PARAMETER TABLES
# Four-condition unified PNG series:
#   1. Interim Intervention
#   2. Interim Control
#   3. Interim Intervention Other
#   4. Interim Control Other
#
# Designed for readable insertion into Google Docs.
#
# Input:
#   C:/Users/dn284/Desktop/MRC_omni/data/interim_sequence (1).py
#
# Outputs:
#   C:/Users/dn284/Desktop/MRC_omni/data/Interim_SLS_tables_BIGFONT/
#     Interim_all_conditions_BIGFONT_page_01.png
#     Interim_all_conditions_BIGFONT_page_02.png
#     ...
#     csv_backups/
#       Interim_all_conditions_unified_table.csv
#       Interim_all_conditions_unified_table_compact.csv
#       Interim_sequence_narrative.txt
# ============================================================

# ============================================================
# 0. PACKAGES
# ============================================================

packages <- c(
  "tidyverse",
  "reticulate",
  "jsonlite",
  "gridExtra",
  "grid",
  "gtable",
  "magick"
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(tidyverse)
library(reticulate)
library(jsonlite)
library(gridExtra)
library(grid)
library(gtable)
library(magick)

# ============================================================
# 1. USER SETTINGS
# ============================================================

INPUT_PY <- "C:/Users/dn284/Desktop/MRC_omni/data/interim_sequence (1).py"

OUT_DIR <- "C:/Users/dn284/Desktop/MRC_omni/data/Interim_SLS_tables_BIGFONT"

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT_DIR, "csv_backups"), recursive = TRUE, showWarnings = FALSE)

# Google Docs readability settings
ROWS_PER_PAGE <- 4

PNG_WIDTH  <- 4200
PNG_HEIGHT <- 2400
PNG_RES    <- 300

TRIM_BORDER_PX <- 35

BASE_FONT <- "serif"

BASE_FONT_SIZE      <- 15
TITLE_FONT_SIZE     <- 24
SUBTITLE_FONT_SIZE  <- 15
NOTE_FONT_SIZE      <- 13

# ============================================================
# 2. SAFE PYTHON EXTRACTOR
#
# This does not execute the Python file. It parses the file with
# Python AST and extracts:
#   sequence = {...}
# plus the following main() seq_name assignment.
#
# It also supports simple list comprehensions like:
#   [50, 50] for _ in range(50)
# ============================================================

extractor_code <- '
import ast, json
from pathlib import Path

def _safe_eval(node):
    if isinstance(node, ast.Constant):
        return node.value

    if isinstance(node, ast.List):
        return [_safe_eval(x) for x in node.elts]

    if isinstance(node, ast.Tuple):
        return tuple(_safe_eval(x) for x in node.elts)

    if isinstance(node, ast.Dict):
        return {
            _safe_eval(k): _safe_eval(v)
            for k, v in zip(node.keys, node.values)
        }

    if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.USub):
        return -_safe_eval(node.operand)

    if isinstance(node, ast.ListComp):
        # Supports simple form:
        #   [something] for _ in range(N)
        if len(node.generators) != 1:
            raise ValueError("Unsupported list comprehension")

        gen = node.generators[0]

        if gen.ifs:
            raise ValueError("List comprehension with ifs is unsupported")

        iter_node = gen.iter

        if not (
            isinstance(iter_node, ast.Call)
            and isinstance(iter_node.func, ast.Name)
            and iter_node.func.id == "range"
            and len(iter_node.args) == 1
        ):
            raise ValueError("Only range(N) list comprehensions are supported")

        n = _safe_eval(iter_node.args[0])
        return [_safe_eval(node.elt) for _ in range(n)]

    raise ValueError(f"Unsupported AST node: {type(node).__name__}")

def _assigns_to_name(assign_node, name):
    if isinstance(assign_node, ast.Assign):
        for target in assign_node.targets:
            if isinstance(target, ast.Name) and target.id == name:
                return True
    return False

def _get_assignment_value(assign_node):
    if isinstance(assign_node, ast.Assign):
        return assign_node.value
    return None

def _find_seq_name_in_main(func_node):
    for node in ast.walk(func_node):
        if isinstance(node, ast.Assign):
            for target in node.targets:
                if isinstance(target, ast.Name) and target.id == "seq_name":
                    try:
                        value = _safe_eval(node.value)
                        if isinstance(value, str):
                            return value
                    except Exception:
                        pass
    return None

def extract_named_sequences(path):
    path = Path(path)
    text = path.read_text(encoding="utf-8")
    tree = ast.parse(text)

    records = []
    pending_sequence = None

    for node in tree.body:
        if isinstance(node, ast.Assign) and _assigns_to_name(node, "sequence"):
            try:
                pending_sequence = _safe_eval(_get_assignment_value(node))
            except Exception:
                pending_sequence = None

        elif isinstance(node, ast.FunctionDef) and node.name == "main":
            if pending_sequence is not None:
                seq_name = _find_seq_name_in_main(node)
                if seq_name:
                    records.append({
                        "seq_name": seq_name,
                        "sequence": pending_sequence
                    })
                    pending_sequence = None

    return json.dumps(records)
'

reticulate::py_run_string(extractor_code)

seq_json <- reticulate::py$extract_named_sequences(INPUT_PY)
seq_records <- jsonlite::fromJSON(seq_json, simplifyVector = FALSE)

if (length(seq_records) == 0) {
  stop("No named sequences were extracted from INPUT_PY.")
}

available_names <- map_chr(seq_records, "seq_name")

message("Loaded file: ", INPUT_PY)
message("Extracted named sequences:")
message(paste0("  - ", available_names, collapse = "\n"))

get_named_sequence <- function(name) {
  hit <- keep(seq_records, ~ identical(.x$seq_name, name))
  
  if (length(hit) != 1) {
    stop(
      "Could not find exactly one extracted sequence named: ",
      name,
      "\nAvailable names are:\n",
      paste0("  - ", available_names, collapse = "\n")
    )
  }
  
  hit[[1]]$sequence
}

# Target four-condition set.
# These names match the seq_name assignments in the pasted file.
sequence_map <- list(
  "Interim Intervention"       = get_named_sequence("x_interim_intv_new"),
  "Interim Control"            = get_named_sequence("x_interim_cnt_new"),
  "Interim Intervention Other" = get_named_sequence("x_interim_intv_other"),
  "Interim Control Other"      = get_named_sequence("x_interim_cnt_other")
)

# ============================================================
# 3. HELPERS
# ============================================================

fmt_time <- function(seconds) {
  minutes <- floor(seconds / 60)
  secs <- seconds - 60 * minutes
  sprintf("%02d:%04.1f", minutes, secs)
}

fmt_pair <- function(pair, digits = 2) {
  vals <- as.numeric(unlist(pair))
  
  if (length(vals) != 2) {
    return(NA_character_)
  }
  
  if (digits == 0) {
    paste0(round(vals[1]), "->", round(vals[2]))
  } else {
    paste0(
      formatC(vals[1], format = "f", digits = digits),
      "->",
      formatC(vals[2], format = "f", digits = digits)
    )
  }
}

fmt_osc_cell <- function(freq_pair, duty_pair, lum_pair) {
  paste0(
    "F ", fmt_pair(freq_pair, digits = 2), " Hz\n",
    "D ", fmt_pair(duty_pair, digits = 0), "%\n",
    "L ", fmt_pair(lum_pair, digits = 0)
  )
}

classify_phase <- function(f12, l12, f34, l34) {
  f12_vals <- as.numeric(unlist(f12))
  f34_vals <- as.numeric(unlist(f34))
  l12_vals <- as.numeric(unlist(l12))
  l34_vals <- as.numeric(unlist(l34))
  
  f12_wash <- all(f12_vals == 60)
  f34_wash <- all(f34_vals == 60)
  
  full_dark <- all(l12_vals == 0) && all(l34_vals == 0)
  
  osc12_active <- !f12_wash
  osc34_active <- !f34_wash
  
  osc12_lum_present <- max(l12_vals, na.rm = TRUE) > 0
  osc34_lum_present <- max(l34_vals, na.rm = TRUE) > 0
  
  any_lum <- max(c(l12_vals, l34_vals), na.rm = TRUE) > 0
  
  case_when(
    full_dark ~ "Dark",
    osc12_active && osc34_active ~ "Dual SLS",
    osc12_active && osc34_lum_present ~ "OSC12 SLS + wash",
    osc34_active && osc12_lum_present ~ "OSC34 SLS + wash",
    osc12_active ~ "OSC12 SLS",
    osc34_active ~ "OSC34 SLS",
    f12_wash && f34_wash && any_lum ~ "Wash",
    TRUE ~ "Transition"
  )
}

validate_sequence <- function(seq, sequence_name) {
  required_keys <- c(
    "durations",
    "freqs_osc12",
    "duties_osc12",
    "lums_osc12",
    "freqs_osc34",
    "duties_osc34",
    "lums_osc34"
  )
  
  missing_keys <- setdiff(required_keys, names(seq))
  
  if (length(missing_keys) > 0) {
    stop(
      sequence_name,
      " is missing required keys: ",
      paste(missing_keys, collapse = ", ")
    )
  }
  
  key_lengths <- map_int(required_keys, ~ length(seq[[.x]]))
  
  if (length(unique(key_lengths)) != 1) {
    length_report <- paste(
      paste(required_keys, key_lengths, sep = " = "),
      collapse = "; "
    )
    
    stop(
      sequence_name,
      " has unequal vector lengths: ",
      length_report
    )
  }
  
  invisible(TRUE)
}

section_labels <- function(n_steps, order = c("standard", "other")) {
  order <- match.arg(order)
  
  if (order == "standard") {
    blocks <- tribble(
      ~Section, ~n,
      "Onset", 1,
      "Warmup", 8,
      "A", 5,
      "B", 5,
      "C1", 7,
      "Peak", 5,
      "D", 6,
      "E", 5,
      "Cooldown", 8
    )
  } else {
    blocks <- tribble(
      ~Section, ~n,
      "Onset", 1,
      "Warmup", 8,
      "B", 5,
      "A", 5,
      "Peak", 5,
      "C1", 7,
      "E", 5,
      "D", 6,
      "Cooldown", 8
    )
  }
  
  if (sum(blocks$n) != n_steps) {
    warning(
      "Section block total does not match n_steps = ",
      n_steps,
      ". Using generic sequence labels."
    )
    return(rep("Sequence", n_steps))
  }
  
  rep(blocks$Section, blocks$n)
}

trim_png_whitespace <- function(path, border_px = 35) {
  img <- magick::image_read(path)
  img <- magick::image_trim(img)
  img <- magick::image_border(
    img,
    color = "white",
    geometry = paste0(border_px, "x", border_px)
  )
  magick::image_write(img, path = path, format = "png")
}

# ============================================================
# 4. BUILD TWO BIG-FONT TABLE SERIES
#
# Series 1:
#   Interim Intervention 1 + Interim Control 1
#
# Series 2:
#   Interim Intervention 2 + Interim Control 2
#
# This avoids one enormous all-condition table and makes the PNGs
# readable in Google Docs.
# ============================================================

iwalk(sequence_map, ~ validate_sequence(.x, .y))

# Pull out the four extracted sequences
intv_1 <- sequence_map[["Interim Intervention"]]
ctrl_1 <- sequence_map[["Interim Control"]]
intv_2 <- sequence_map[["Interim Intervention Other"]]
ctrl_2 <- sequence_map[["Interim Control Other"]]

# ------------------------------------------------------------
# Build a paired Intervention/Control table
# ------------------------------------------------------------

build_pair_table <- function(
    intv_seq,
    ctrl_seq,
    pair_label = "Interim pair",
    block_order = c("standard", "shuffled")
) {
  block_order <- match.arg(block_order)
  
  validate_sequence(intv_seq, paste0(pair_label, " intervention"))
  validate_sequence(ctrl_seq, paste0(pair_label, " control"))
  
  n_intv <- length(intv_seq$durations)
  n_ctrl <- length(ctrl_seq$durations)
  
  if (n_intv != n_ctrl) {
    stop(
      pair_label,
      " intervention/control sequences have different step counts: ",
      n_intv,
      " vs ",
      n_ctrl
    )
  }
  
  n_steps <- n_intv
  
  dur_intv <- as.numeric(unlist(intv_seq$durations))
  dur_ctrl <- as.numeric(unlist(ctrl_seq$durations))
  
  if (!isTRUE(all.equal(dur_intv, dur_ctrl))) {
    warning(
      pair_label,
      ": Intervention and Control durations are not identical. ",
      "Using Intervention timing as the primary timeline."
    )
  }
  
  start_sec <- c(0, cumsum(dur_intv)[-n_steps])
  end_sec   <- cumsum(dur_intv)
  
  blocks <- section_labels(n_steps, order = ifelse(block_order == "standard", "standard", "other"))
  
  map_dfr(seq_len(n_steps), function(i) {
    tibble(
      Pair = pair_label,
      Step = i,
      Time = paste0(fmt_time(start_sec[i]), "-", fmt_time(end_sec[i])),
      Dur = formatC(dur_intv[i], format = "f", digits = 1),
      Block = blocks[i],
      
      Intv_Phase = classify_phase(
        intv_seq$freqs_osc12[[i]], intv_seq$lums_osc12[[i]],
        intv_seq$freqs_osc34[[i]], intv_seq$lums_osc34[[i]]
      ),
      
      Ctrl_Phase = classify_phase(
        ctrl_seq$freqs_osc12[[i]], ctrl_seq$lums_osc12[[i]],
        ctrl_seq$freqs_osc34[[i]], ctrl_seq$lums_osc34[[i]]
      ),
      
      `Intervention OSC1-2` = fmt_osc_cell(
        intv_seq$freqs_osc12[[i]],
        intv_seq$duties_osc12[[i]],
        intv_seq$lums_osc12[[i]]
      ),
      
      `Intervention OSC3-4` = fmt_osc_cell(
        intv_seq$freqs_osc34[[i]],
        intv_seq$duties_osc34[[i]],
        intv_seq$lums_osc34[[i]]
      ),
      
      `Control OSC1-2` = fmt_osc_cell(
        ctrl_seq$freqs_osc12[[i]],
        ctrl_seq$duties_osc12[[i]],
        ctrl_seq$lums_osc12[[i]]
      ),
      
      `Control OSC3-4` = fmt_osc_cell(
        ctrl_seq$freqs_osc34[[i]],
        ctrl_seq$duties_osc34[[i]],
        ctrl_seq$lums_osc34[[i]]
      )
    )
  })
}

table_pair_1 <- build_pair_table(
  intv_seq = intv_1,
  ctrl_seq = ctrl_1,
  pair_label = "Interim Intervention/Control 1",
  block_order = "standard"
)

table_pair_2 <- build_pair_table(
  intv_seq = intv_2,
  ctrl_seq = ctrl_2,
  pair_label = "Interim Intervention/Control 2",
  block_order = "shuffled"
)

# CSV backups
write_csv(
  table_pair_1,
  file.path(OUT_DIR, "csv_backups", "Interim_intervention_control_1_table.csv")
)

write_csv(
  table_pair_2,
  file.path(OUT_DIR, "csv_backups", "Interim_intervention_control_2_shuffled_table.csv")
)

# Display versions for PNG
display_pair_1 <- table_pair_1 %>%
  select(
    Step,
    Time,
    Dur,
    Block,
    `Intervention OSC1-2`,
    `Intervention OSC3-4`,
    `Control OSC1-2`,
    `Control OSC3-4`
  )

display_pair_2 <- table_pair_2 %>%
  select(
    Step,
    Time,
    Dur,
    Block,
    `Intervention OSC1-2`,
    `Intervention OSC3-4`,
    `Control OSC1-2`,
    `Control OSC3-4`
  )

# ============================================================
# 5. BIG-FONT PNG EXPORT FOR TWO SEPARATE SERIES
# ============================================================

# Bigger font now that each image has fewer columns
ROWS_PER_PAGE <- 6

PNG_WIDTH  <- 3600
PNG_HEIGHT <- 2200
PNG_RES    <- 300

BASE_FONT_SIZE      <- 19
TITLE_FONT_SIZE     <- 25
SUBTITLE_FONT_SIZE  <- 16
NOTE_FONT_SIZE      <- 14

TRIM_BORDER_PX <- 35

trim_png_whitespace <- function(path, border_px = 35) {
  img <- magick::image_read(path)
  img <- magick::image_trim(img)
  img <- magick::image_border(
    img,
    color = "white",
    geometry = paste0(border_px, "x", border_px)
  )
  magick::image_write(img, path = path, format = "png")
}

draw_pair_page <- function(
    df_page,
    page_i,
    n_pages,
    row_start,
    row_end,
    n_total,
    out_file,
    title,
    subtitle_extra = ""
) {
  
  theme_tbl <- gridExtra::ttheme_minimal(
    base_size = BASE_FONT_SIZE,
    base_family = BASE_FONT,
    padding = grid::unit(c(7.8, 5.8), "mm"),
    
    core = list(
      fg_params = list(
        cex = 0.92,
        fontfamily = BASE_FONT,
        col = "#111111",
        lineheight = 0.95
      ),
      bg_params = list(
        fill = rep(c("#FFFFFF", "#F4F4F4"), length.out = nrow(df_page)),
        col = "#D2D2D2",
        lwd = 0.60
      )
    ),
    
    colhead = list(
      fg_params = list(
        cex = 0.88,
        fontface = "bold",
        fontfamily = BASE_FONT,
        col = "#FFFFFF",
        lineheight = 0.94
      ),
      bg_params = list(
        fill = "#2B2B2B",
        col = "#2B2B2B",
        lwd = 0.65
      )
    )
  )
  
  table_grob <- gridExtra::tableGrob(
    df_page,
    rows = NULL,
    theme = theme_tbl
  )
  
  # 8 columns:
  # Step, Time, Dur, Block, four oscillator columns
  table_grob$widths <- grid::unit(
    c(
      0.045, # Step
      0.105, # Time
      0.055, # Dur
      0.075, # Block
      0.180, # Intervention OSC1-2
      0.180, # Intervention OSC3-4
      0.180, # Control OSC1-2
      0.180  # Control OSC3-4
    ),
    "npc"
  )
  
  title_grob <- grid::textGrob(
    title,
    gp = grid::gpar(
      fontsize = TITLE_FONT_SIZE,
      fontface = "bold",
      fontfamily = BASE_FONT,
      col = "#111111"
    ),
    x = 0,
    hjust = 0
  )
  
  subtitle_text <- paste0(
    "Rows ",
    row_start,
    "-",
    row_end,
    " of ",
    n_total,
    " | Page ",
    page_i,
    " of ",
    n_pages,
    " | Cells report frequency, duty cycle, and luminance as start->end values.",
    subtitle_extra
  )
  
  subtitle_grob <- grid::textGrob(
    subtitle_text,
    gp = grid::gpar(
      fontsize = SUBTITLE_FONT_SIZE,
      fontfamily = BASE_FONT,
      col = "#333333"
    ),
    x = 0,
    hjust = 0
  )
  
  note_grob <- grid::textGrob(
    "Abbreviations: F = frequency in Hz; D = duty cycle percentage; L = luminance. OSC1-2 and OSC3-4 are the two oscillator groups encoded in the sequence script.",
    gp = grid::gpar(
      fontsize = NOTE_FONT_SIZE,
      fontfamily = BASE_FONT,
      col = "#444444"
    ),
    x = 0,
    hjust = 0
  )
  
  full_grob <- gridExtra::arrangeGrob(
    title_grob,
    subtitle_grob,
    table_grob,
    note_grob,
    ncol = 1,
    heights = grid::unit.c(
      grid::unit(0.44, "in"),
      grid::unit(0.34, "in"),
      grid::unit(1, "null"),
      grid::unit(0.30, "in")
    )
  )
  
  png(
    filename = out_file,
    width = PNG_WIDTH,
    height = PNG_HEIGHT,
    res = PNG_RES,
    bg = "white"
  )
  
  grid::grid.newpage()
  grid::grid.draw(full_grob)
  dev.off()
  
  trim_png_whitespace(out_file, border_px = TRIM_BORDER_PX)
}

export_pair_png_series <- function(
    df,
    series_name,
    file_prefix,
    title,
    subtitle_extra = ""
) {
  series_dir <- file.path(OUT_DIR, series_name)
  dir.create(series_dir, recursive = TRUE, showWarnings = FALSE)
  
  n_total <- nrow(df)
  n_pages <- ceiling(n_total / ROWS_PER_PAGE)
  
  for (page_i in seq_len(n_pages)) {
    row_start <- ((page_i - 1) * ROWS_PER_PAGE) + 1
    row_end   <- min(page_i * ROWS_PER_PAGE, n_total)
    
    df_page <- df[row_start:row_end, ]
    
    out_file <- file.path(
      series_dir,
      sprintf("%s_page_%02d.png", file_prefix, page_i)
    )
    
    draw_pair_page(
      df_page = df_page,
      page_i = page_i,
      n_pages = n_pages,
      row_start = row_start,
      row_end = row_end,
      n_total = n_total,
      out_file = out_file,
      title = title,
      subtitle_extra = subtitle_extra
    )
    
    message("Exported: ", out_file)
  }
  
  invisible(TRUE)
}

export_pair_png_series(
  df = display_pair_1,
  series_name = "intervention_control_1",
  file_prefix = "Interim_intervention_control_1_BIGFONT",
  title = "Interim SLS parameters: Intervention/Control 1",
  subtitle_extra = " Standard block order."
)

export_pair_png_series(
  df = display_pair_2,
  series_name = "intervention_control_2_shuffled",
  file_prefix = "Interim_intervention_control_2_shuffled_BIGFONT",
  title = "Interim SLS parameters: Intervention/Control 2",
  subtitle_extra = " Shuffled audiovisual block order."
)

# ============================================================
# 6. NARRATIVE OUTPUT
# ============================================================

duration_summary <- bind_rows(
  table_pair_1 %>%
    summarise(
      Series = "Intervention/Control 1",
      Steps = n(),
      Total_duration_s = sum(as.numeric(Dur), na.rm = TRUE),
      Total_duration_min = round(Total_duration_s / 60, 2)
    ),
  table_pair_2 %>%
    summarise(
      Series = "Intervention/Control 2 shuffled",
      Steps = n(),
      Total_duration_s = sum(as.numeric(Dur), na.rm = TRUE),
      Total_duration_min = round(Total_duration_s / 60, 2)
    )
)

write_csv(
  duration_summary,
  file.path(OUT_DIR, "csv_backups", "Interim_two_series_duration_summary.csv")
)

narrative <- paste0(
  "Interim sequence parameterisation and shuffled audiovisual block order\n\n",
  "The interim protocol used two paired Intervention/Control sequence series. ",
  "The first series retained the standard representative block order, progressing from A to B to C1 to Peak to D to E after the onset and warmup periods. ",
  "The second series used the corresponding shuffled order, progressing from B to A to Peak to C1 to E to D before cooldown. ",
  "Both series preserved the same local oscillator parameter families, including the frequency, duty-cycle, and luminance profiles within each representative block, but differed in the temporal ordering of those blocks.\n\n",
  "Within each paired series, the Intervention and Control sequences shared the same temporal scaffold. ",
  "The Intervention sequence placed the higher-luminance, dynamically varying stroboscopic component on the active oscillator group, while the complementary oscillator group served primarily as a lower-luminance wash-light channel. ",
  "The Control sequence retained the same broad timing and oscillator progression but shifted luminance emphasis away from the active stroboscopic component and toward the wash-light component. ",
  "This reduced the expected phenomenological salience of flicker while preserving the general timing, transition structure, and audiovisual context.\n\n",
  "The shuffled second series was included to avoid repeating an identical audiovisual trajectory. ",
  "By rearranging the representative visual blocks while preserving their local parameter profiles, the secondary Intervention/Control pair altered how the isochronic auditory pulse motifs aligned with the visual oscillator trajectory over time. ",
  "Thus, the second series should be understood as an audiovisual-alignment/order variant rather than as a distinct stimulus family.\n\n",
  "Across all table pages, each oscillator cell reports frequency, duty cycle, and luminance as start-to-end values for that time step. ",
  "OSC1-2 and OSC3-4 refer to the two oscillator groups encoded in the source STP-generation script. ",
  "In this coding framework, 60 Hz entries denote the high-rate wash-like component rather than the lower-frequency stroboscopic range used to drive stronger visual phenomenology.\n"
)

writeLines(
  narrative,
  con = file.path(OUT_DIR, "csv_backups", "Interim_two_series_narrative.txt")
)

message("============================================================")
message("EXPORT COMPLETE")
message("Series 1 saved to:")
message(file.path(OUT_DIR, "intervention_control_1"))
message("")
message("Series 2 saved to:")
message(file.path(OUT_DIR, "intervention_control_2_shuffled"))
message("")
message("CSV backups and narrative saved to:")
message(file.path(OUT_DIR, "csv_backups"))
message("============================================================")

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







# ------------------------------------------------------------
# SECTION: wp2 parameters
# ------------------------------------------------------------


# ------------------------------------------------------------
# SECTION: wp2 demographics
# ------------------------------------------------------------

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

# ===================================================
# WP2 DEMOGRAPHICS
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

# ===================================================

# ============================================================
# WP2 SUPERVISOR DEMOGRAPHICS TABLES
# ============================================================
# Comment addressed:
# [MISSING: Add WP2 stage-specific demographics by randomised arm,
# including age, sex, gender, psychoactive medication status,
# baseline PHQ-9 severity stratum, baseline BDI-II/PHQ-9/MADRS-S/BAI
# where available, and missing demographic denominators.
# The main manuscript states WP2 screened 312, randomised/started 84,
# and analysed 70 at post-session 4; confirm these values here.]
#
# Assumptions:
#   - There is NO separate WP2 demographics file.
#   - Demographics come from wp2_pre_screen.
#   - Arm allocation comes from wp2_assignments.
#   - Baseline clinical variables come from wp2_pre_session_1 where available.
#   - PHQ-9 can fall back to wp2_pre_screen if needed.
#   - Analysed = randomised participants with valid post-session-4 data.
#
# Required files:
#   *wp2_pre_screen*.csv
#   *wp2_assignments*.csv
#   *wp2_pre_session_1*.csv
#   *wp2_post_session_4*.csv
#
# Outputs:
#   WP2_SUPERVISOR_Demographics_ByArm_Table.html/png/pdf
#   WP2_SUPERVISOR_MissingDenominators_Table.html/png/pdf
#   WP2_SUPERVISOR_Demographics_ByArm.csv
#   WP2_SUPERVISOR_MissingDenominators.csv
#   WP2_SUPERVISOR_ID_Audit.csv
#   WP2_SUPERVISOR_Denominator_Audit.csv
#   WP2_SUPERVISOR_ManuscriptText.txt
# ============================================================

# install.packages(c("tidyverse", "lubridate", "gt", "webshot2"))

library(tidyverse)
library(lubridate)
library(gt)

# ============================================================
# 1. SETTINGS
# ============================================================

DATA_DIR <- Sys.getenv("MRC_DATA_DIR", unset = "C:/Users/dn284/Desktop/MRC_omni/data")
SEARCH_DIRS <- c(DATA_DIR)

OUT_PREFIX <- "WP2_SUPERVISOR"

EXPECTED_SCREENED_N <- 312
EXPECTED_RANDOMISED_N <- 84
EXPECTED_ANALYSED_N <- 70

USE_HARD_DENOMINATOR_CHECKS <- TRUE

OUT_MAIN_CSV <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_Demographics_ByArm.csv"))
OUT_MISSING_CSV <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_MissingDenominators.csv"))
OUT_ID_AUDIT_CSV <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_ID_Audit.csv"))
OUT_DENOM_AUDIT_CSV <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_Denominator_Audit.csv"))
OUT_TEXT <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_ManuscriptText.txt"))

OUT_MAIN_HTML <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_Demographics_ByArm_Table.html"))
OUT_MAIN_PNG  <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_Demographics_ByArm_Table.png"))
OUT_MAIN_PDF  <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_Demographics_ByArm_Table.pdf"))

OUT_MISSING_HTML <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_MissingDenominators_Table.html"))
OUT_MISSING_PNG  <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_MissingDenominators_Table.png"))
OUT_MISSING_PDF  <- file.path(DATA_DIR, paste0(OUT_PREFIX, "_MissingDenominators_Table.pdf"))

# ============================================================
# 2. HELPERS
# ============================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

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
  
  hits[order(file.info(hits)$mtime, decreasing = TRUE)][1]
}

read_qualtrics_real <- function(path) {
  df <- readr::read_csv(
    path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )
  
  names(df) <- str_trim(names(df))
  
  if ("ResponseId" %in% names(df)) {
    df <- df %>%
      filter(str_starts(as.character(ResponseId), "R_"))
  }
  
  if ("Response ID" %in% names(df)) {
    df <- df %>%
      filter(str_starts(as.character(`Response ID`), "R_"))
  }
  
  df <- df %>%
    filter(
      !if_any(
        everything(),
        ~ str_detect(coalesce(as.character(.x), ""), fixed("ImportId"))
      )
    )
  
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
    str_extract("\\d{1,8}")
}

find_col <- function(df, candidates) {
  if (is.null(df)) return(NULL)
  
  nms <- names(df)
  lower_map <- setNames(nms, tolower(nms))
  
  # exact lower-case match first
  for (cand in candidates) {
    if (tolower(cand) %in% names(lower_map)) {
      return(lower_map[[tolower(cand)]])
    }
  }
  
  # then partial match
  for (cand in candidates) {
    hits <- nms[str_detect(tolower(nms), fixed(tolower(cand)))]
    if (length(hits) > 0) return(hits[1])
  }
  
  NULL
}

standardise_condition <- function(x) {
  x_chr <- str_to_lower(str_squish(as.character(x)))
  
  case_when(
    str_detect(x_chr, "control|placebo|sham|matched") ~ "Control",
    str_detect(x_chr, "intervention|active|treatment|sls") ~ "Intervention",
    x_chr %in% c("0", "c", "ctrl") ~ "Control",
    x_chr %in% c("1", "i", "int", "active") ~ "Intervention",
    TRUE ~ NA_character_
  )
}

best_row_per_pid <- function(df, pid_col = "part_id", priority_cols = character(0)) {
  if (is.null(df)) return(NULL)
  
  if (!pid_col %in% names(df)) {
    stop("PID column not found: ", pid_col)
  }
  
  priority_cols <- priority_cols[priority_cols %in% names(df)]
  
  dt_col <- c("RecordedDate", "EndDate", "StartDate")
  dt_col <- dt_col[dt_col %in% names(df)][1]
  
  df %>%
    filter(!is.na(.data[[pid_col]])) %>%
    mutate(
      .row_order = row_number(),
      .completeness = if (length(priority_cols) > 0) {
        rowSums(
          across(
            all_of(priority_cols),
            ~ !is.na(.x) & str_squish(as.character(.x)) != ""
          ),
          na.rm = TRUE
        )
      } else {
        0
      },
      .dt = if (!is.na(dt_col)) {
        suppressWarnings(
          parse_date_time(
            .data[[dt_col]],
            orders = c(
              "ymd HMS", "ymd HM",
              "dmy HMS", "dmy HM",
              "mdy HMS", "mdy HM",
              "Ymd HMS", "Ymd HM"
            )
          )
        )
      } else {
        as.POSIXct(NA)
      }
    ) %>%
    arrange(
      .data[[pid_col]],
      desc(.completeness),
      desc(.dt),
      desc(.row_order)
    ) %>%
    distinct(.data[[pid_col]], .keep_all = TRUE) %>%
    select(-.row_order, -.completeness, -.dt)
}

clean_cat <- function(x) {
  x_chr <- str_squish(as.character(x))
  x_chr <- if_else(is.na(x) | x_chr == "", NA_character_, x_chr)
  
  case_when(
    str_to_lower(x_chr) %in% c("true", "yes", "y", "1") ~ "Yes",
    str_to_lower(x_chr) %in% c("false", "no", "n", "0") ~ "No",
    TRUE ~ x_chr
  )
}

score_0_3_item <- function(x) {
  x_chr <- str_to_lower(str_squish(as.character(x)))
  
  case_when(
    is.na(x_chr) | x_chr == "" ~ NA_real_,
    str_detect(x_chr, "not at all") ~ 0,
    str_detect(x_chr, "several days") ~ 1,
    str_detect(x_chr, "more than half") ~ 2,
    str_detect(x_chr, "nearly every day") ~ 3,
    str_detect(x_chr, "^0") ~ 0,
    str_detect(x_chr, "^1") ~ 1,
    str_detect(x_chr, "^2") ~ 2,
    str_detect(x_chr, "^3") ~ 3,
    TRUE ~ suppressWarnings(as.numeric(x_chr))
  )
}

score_0_4_item <- function(x) {
  x_chr <- str_to_lower(str_squish(as.character(x)))
  
  case_when(
    is.na(x_chr) | x_chr == "" ~ NA_real_,
    str_detect(x_chr, "^0") ~ 0,
    str_detect(x_chr, "^1") ~ 1,
    str_detect(x_chr, "^2") ~ 2,
    str_detect(x_chr, "^3") ~ 3,
    str_detect(x_chr, "^4") ~ 4,
    TRUE ~ suppressWarnings(as.numeric(x_chr))
  )
}

score_0_6_item <- function(x) {
  x_chr <- str_to_lower(str_squish(as.character(x)))
  
  case_when(
    is.na(x_chr) | x_chr == "" ~ NA_real_,
    str_detect(x_chr, "^0") ~ 0,
    str_detect(x_chr, "^1") ~ 1,
    str_detect(x_chr, "^2") ~ 2,
    str_detect(x_chr, "^3") ~ 3,
    str_detect(x_chr, "^4") ~ 4,
    str_detect(x_chr, "^5") ~ 5,
    str_detect(x_chr, "^6") ~ 6,
    TRUE ~ suppressWarnings(as.numeric(x_chr))
  )
}

get_item_cols <- function(df, prefixes, n_items, allow_suffix = TRUE) {
  if (is.null(df)) return(character(0))
  
  out <- character(0)
  
  for (prefix in prefixes) {
    for (i in seq_len(n_items)) {
      candidates <- c(
        paste0(prefix, "_", i),
        paste0(prefix, i)
      )
      
      if (allow_suffix) {
        candidates <- c(
          candidates,
          paste0(prefix, "_", i, "_1"),
          paste0(prefix, i, "_1")
        )
      }
      
      hit <- candidates[candidates %in% names(df)][1]
      if (!is.na(hit)) out <- c(out, hit)
    }
  }
  
  unique(out)
}

sum_scale <- function(df, total_candidates, item_prefixes, n_items, scorer, min_items = 1) {
  total_col <- total_candidates[total_candidates %in% names(df)][1]
  
  if (!is.na(total_col)) {
    return(suppressWarnings(as.numeric(df[[total_col]])))
  }
  
  item_cols <- get_item_cols(df, item_prefixes, n_items, allow_suffix = TRUE)
  
  if (length(item_cols) == 0) {
    return(rep(NA_real_, nrow(df)))
  }
  
  scored <- df %>%
    mutate(across(all_of(item_cols), scorer, .names = "{.col}__score")) %>%
    select(ends_with("__score"))
  
  n_valid <- rowSums(!is.na(scored))
  total <- rowSums(scored, na.rm = TRUE)
  total[n_valid < min_items] <- NA_real_
  total
}

phq9_severity <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  
  case_when(
    is.na(x) ~ NA_character_,
    x <= 4 ~ "Minimal (0–4)",
    x <= 9 ~ "Mild (5–9)",
    x <= 14 ~ "Moderate (10–14)",
    x <= 19 ~ "Moderately severe (15–19)",
    x <= 27 ~ "Severe (20–27)",
    TRUE ~ NA_character_
  )
}

summarise_numeric <- function(df, value_col, group_label, variable_label) {
  x <- suppressWarnings(as.numeric(df[[value_col]]))
  stage_denom <- nrow(df)
  nonmissing_n <- sum(!is.na(x))
  missing_n <- sum(is.na(x))
  
  tibble(
    Group = group_label,
    Variable = variable_label,
    Level = NA_character_,
    Stage_denominator = stage_denom,
    Denominator = nonmissing_n,
    Non_missing_n = nonmissing_n,
    Missing_n = missing_n,
    Count = NA_integer_,
    Percent = NA_real_,
    Mean = ifelse(nonmissing_n > 0, mean(x, na.rm = TRUE), NA_real_),
    SD = ifelse(nonmissing_n > 1, sd(x, na.rm = TRUE), NA_real_),
    Min = ifelse(nonmissing_n > 0, min(x, na.rm = TRUE), NA_real_),
    Max = ifelse(nonmissing_n > 0, max(x, na.rm = TRUE), NA_real_)
  )
}

summarise_categorical <- function(df, value_col, group_label, variable_label) {
  vals <- clean_cat(df[[value_col]])
  stage_denom <- nrow(df)
  
  nonmissing <- !is.na(vals)
  cat_denom <- sum(nonmissing)
  missing_n <- sum(!nonmissing)
  
  nonmissing_tbl <- tibble(value = vals[nonmissing]) %>%
    count(value, name = "Count") %>%
    mutate(
      Group = group_label,
      Variable = variable_label,
      Level = value,
      Stage_denominator = stage_denom,
      Denominator = cat_denom,
      Non_missing_n = cat_denom,
      Missing_n = missing_n,
      Percent = ifelse(cat_denom > 0, 100 * Count / cat_denom, NA_real_),
      Mean = NA_real_,
      SD = NA_real_,
      Min = NA_real_,
      Max = NA_real_
    ) %>%
    select(
      Group, Variable, Level, Stage_denominator, Denominator,
      Non_missing_n, Missing_n, Count, Percent, Mean, SD, Min, Max
    )
  
  missing_tbl <- tibble(
    Group = group_label,
    Variable = variable_label,
    Level = "Unavailable / missing",
    Stage_denominator = stage_denom,
    Denominator = stage_denom,
    Non_missing_n = cat_denom,
    Missing_n = missing_n,
    Count = missing_n,
    Percent = ifelse(stage_denom > 0, 100 * missing_n / stage_denom, NA_real_),
    Mean = NA_real_,
    SD = NA_real_,
    Min = NA_real_,
    Max = NA_real_
  )
  
  bind_rows(nonmissing_tbl, missing_tbl)
}

missing_audit <- function(df, value_col, group_label, variable_label) {
  vals <- df[[value_col]]
  is_missing <- is.na(vals) | str_squish(as.character(vals)) == ""
  
  missing_ids <- df %>%
    filter(is_missing) %>%
    pull(part_id) %>%
    unique() %>%
    na.omit()
  
  tibble(
    Group = group_label,
    Variable = variable_label,
    Stage_denominator = nrow(df),
    Non_missing_n = nrow(df) - length(missing_ids),
    Missing_n = length(missing_ids),
    Missing_percent = ifelse(nrow(df) > 0, 100 * length(missing_ids) / nrow(df), NA_real_),
    Missing_part_ids = paste(missing_ids, collapse = "; ")
  )
}

format_numeric_summary <- function(mean, sd, min, max, n, miss) {
  ifelse(
    is.na(mean),
    "",
    paste0(
      sprintf("%.2f", mean),
      " ± ",
      sprintf("%.2f", sd),
      " [",
      sprintf("%.2f", min),
      "–",
      sprintf("%.2f", max),
      "]",
      " (n = ", n,
      ifelse(miss > 0, paste0("; unavailable = ", miss), ""),
      ")"
    )
  )
}

# ============================================================
# 3. LOCATE FILES
# ============================================================

WP2_PRE_PATH <- newest_match(c("*wp2_pre_screen*.csv"))
WP2_ASSIGN_PATH <- newest_match(c("*wp2_assignments*.csv"))
WP2_PRE1_PATH <- newest_match(c("*wp2_pre_session_1*.csv"))
WP2_POST4_PATH <- newest_match(c("*wp2_post_session_4*.csv"))
WP2_SMS_POST_PATH <- newest_match(c("*wp2_sms_post*.csv", "*sms_post*.csv"), required = FALSE)

message("WP2 pre-screen:      ", basename(WP2_PRE_PATH))
message("WP2 assignments:     ", basename(WP2_ASSIGN_PATH))
message("WP2 pre-session 1:   ", basename(WP2_PRE1_PATH))
message("WP2 post-session 4:  ", basename(WP2_POST4_PATH))
message("WP2 SMS post:        ", ifelse(is.null(WP2_SMS_POST_PATH), "not found", basename(WP2_SMS_POST_PATH)))

# ============================================================
# 4. LOAD FILES
# ============================================================

wp2_pre_raw <- read_qualtrics_real(WP2_PRE_PATH)
wp2_assign_raw <- read_qualtrics_real(WP2_ASSIGN_PATH)
wp2_pre1_raw <- read_qualtrics_real(WP2_PRE1_PATH)
wp2_post4_raw <- read_qualtrics_real(WP2_POST4_PATH)

wp2_sms_post_raw <- if (!is.null(WP2_SMS_POST_PATH)) {
  read_qualtrics_real(WP2_SMS_POST_PATH)
} else {
  NULL
}

names(wp2_pre_raw) <- str_trim(names(wp2_pre_raw))
names(wp2_assign_raw) <- str_trim(names(wp2_assign_raw))
names(wp2_pre1_raw) <- str_trim(names(wp2_pre1_raw))
names(wp2_post4_raw) <- str_trim(names(wp2_post4_raw))

# ============================================================
# 5. CLEAN IDS
# ============================================================

for (obj_name in c("wp2_pre_raw", "wp2_assign_raw", "wp2_pre1_raw", "wp2_post4_raw")) {
  obj <- get(obj_name)
  if (!"part_id" %in% names(obj)) {
    stop("No part_id column found in ", obj_name)
  }
}

wp2_pre <- wp2_pre_raw %>%
  mutate(part_id = clean_id(part_id)) %>%
  filter(!is.na(part_id))

wp2_assign <- wp2_assign_raw %>%
  mutate(part_id = clean_id(part_id)) %>%
  filter(!is.na(part_id))

wp2_pre1 <- wp2_pre1_raw %>%
  mutate(part_id = clean_id(part_id)) %>%
  filter(!is.na(part_id))

wp2_post4 <- wp2_post4_raw %>%
  mutate(part_id = clean_id(part_id)) %>%
  filter(!is.na(part_id))

wp2_sms_post <- if (!is.null(wp2_sms_post_raw) && "part_id" %in% names(wp2_sms_post_raw)) {
  wp2_sms_post_raw %>%
    mutate(part_id = clean_id(part_id)) %>%
    filter(!is.na(part_id))
} else {
  NULL
}

# ============================================================
# 6. BEST ROWS
# ============================================================

pre_priority_cols <- c(
  "incl_dem_age", "incl_dem_sex", "incl_dem_gender", "incl_dem_med",
  "age", "sex", "gender", "medication", "medication_status",
  "psychoactive", "psychoactive_medication", "psychotropic",
  paste0("phq9_", 1:9), paste0("phq_", 1:9), "phq9_sum", "phq_sum",
  paste0("bdi_", 1:21), "bdi_sum", "bdi_sum_calc", "bdi_total", "bdi_ii_total",
  paste0("bai_", 1:21), "bai_sum", "bai_total",
  paste0("madrs_", 1:9), paste0("madrs_s_", 1:9), paste0("mards_", 1:9),
  "madrs_sum", "madrs_total", "madrs_s_sum", "mards_sum"
)

wp2_pre_u <- best_row_per_pid(wp2_pre, "part_id", priority_cols = pre_priority_cols)
wp2_pre1_u <- best_row_per_pid(wp2_pre1, "part_id", priority_cols = pre_priority_cols)
wp2_post4_u <- best_row_per_pid(
  wp2_post4,
  "part_id",
  priority_cols = c("tol_score", "discomfortScore", "vdq", "vdq_score", "vdq_total")
)

# ============================================================
# 7. ARM ASSIGNMENT
# ============================================================

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
  stop("Could not identify WP2 condition/allocation column in wp2_assignments.")
}

message("Using WP2 allocation column: ", condition_col)

wp2_assign_clean <- wp2_assign %>%
  mutate(
    condition_raw = .data[[condition_col]],
    condition = standardise_condition(condition_raw)
  ) %>%
  filter(!is.na(part_id), !is.na(condition)) %>%
  distinct(part_id, .keep_all = TRUE)

# ============================================================
# 8. SCORE BASELINE CLINICAL VARIABLES
# ============================================================

wp2_pre1_scored <- wp2_pre1_u

wp2_pre1_scored$baseline_phq9 <- sum_scale(
  wp2_pre1_scored,
  total_candidates = c("phq9_sum", "phq_sum", "phq9_total", "phq_total"),
  item_prefixes = c("phq9", "phq"),
  n_items = 9,
  scorer = score_0_3_item,
  min_items = 1
)

wp2_pre1_scored$baseline_bdi <- sum_scale(
  wp2_pre1_scored,
  total_candidates = c("bdi_sum", "bdi_sum_calc", "bdi_total", "bdi_ii_total"),
  item_prefixes = c("bdi"),
  n_items = 21,
  scorer = score_0_3_item,
  min_items = 1
)

wp2_pre1_scored$baseline_bai <- sum_scale(
  wp2_pre1_scored,
  total_candidates = c("bai_sum", "bai_total"),
  item_prefixes = c("bai"),
  n_items = 21,
  scorer = score_0_3_item,
  min_items = 1
)

wp2_pre1_scored$baseline_madrs <- sum_scale(
  wp2_pre1_scored,
  total_candidates = c("madrs_sum", "madrs_total", "madrs_s_sum", "madrs_s_total", "mards_sum", "mards_total"),
  item_prefixes = c("madrs", "madrs_s", "mards"),
  n_items = 9,
  scorer = score_0_6_item,
  min_items = 1
)

wp2_pre1_scored$baseline_phq9_severity <- phq9_severity(wp2_pre1_scored$baseline_phq9)

wp2_pre_scored <- wp2_pre_u

wp2_pre_scored$prescreen_phq9 <- sum_scale(
  wp2_pre_scored,
  total_candidates = c("phq9_sum", "phq_sum", "phq9_total", "phq_total"),
  item_prefixes = c("phq9", "phq"),
  n_items = 9,
  scorer = score_0_3_item,
  min_items = 1
)

# ============================================================
# 9. DEMOGRAPHIC FIELD DETECTION FROM PRE-SCREEN ONLY
# ============================================================

age_col_pre <- find_col(wp2_pre_scored, c("incl_dem_age", "age"))
sex_col_pre <- find_col(wp2_pre_scored, c("incl_dem_sex", "sex"))
gender_col_pre <- find_col(wp2_pre_scored, c("incl_dem_gender", "gender"))
med_col_pre <- find_col(
  wp2_pre_scored,
  c(
    "incl_dem_med",
    "psychoactive_medication_status",
    "psychoactive_medication",
    "psychoactive",
    "psychotropic",
    "medication_status",
    "medication",
    "meds"
  )
)

message("Using WP2 demographic columns from pre-screen:")
message("  age:    ", age_col_pre %||% "not found")
message("  sex:    ", sex_col_pre %||% "not found")
message("  gender: ", gender_col_pre %||% "not found")
message("  meds:   ", med_col_pre %||% "not found")

demo_pre <- wp2_pre_scored %>%
  transmute(
    part_id,
    age_pre = if (!is.null(age_col_pre)) as.character(.data[[age_col_pre]]) else NA_character_,
    sex_pre = if (!is.null(sex_col_pre)) as.character(.data[[sex_col_pre]]) else NA_character_,
    gender_pre = if (!is.null(gender_col_pre)) as.character(.data[[gender_col_pre]]) else NA_character_,
    med_pre = if (!is.null(med_col_pre)) as.character(.data[[med_col_pre]]) else NA_character_
  )

# ============================================================
# 10. DEFINE DENOMINATORS
# ============================================================

screened_ids <- wp2_pre_u %>%
  pull(part_id) %>%
  unique()

randomised_ids <- wp2_assign_clean %>%
  pull(part_id) %>%
  unique()

post4_vdq_col <- find_col(
  wp2_post4_u,
  c("tol_score", "discomfortScore", "vdq", "vdq_score", "vdq_total")
)

if (!is.null(post4_vdq_col)) {
  wp2_post4_valid <- wp2_post4_u %>%
    mutate(.post4_valid_value = suppressWarnings(as.numeric(.data[[post4_vdq_col]]))) %>%
    filter(!is.na(part_id), !is.na(.post4_valid_value))
} else {
  wp2_post4_valid <- wp2_post4_u %>%
    filter(!is.na(part_id))
}

analysed_ids <- intersect(
  wp2_post4_valid %>% pull(part_id) %>% unique(),
  randomised_ids
)

wp2_pre_excl <- wp2_pre_u %>%
  mutate(
    excluded_clean = str_to_lower(str_squish(as.character(excluded))),
    excluded_clean = na_if(excluded_clean, ""),
    excluded_clean = na_if(excluded_clean, "nan"),
    excluded_clean = na_if(excluded_clean, "none")
  )

excluded_ids <- wp2_pre_excl %>%
  filter(!is.na(part_id), !is.na(excluded_clean), excluded_clean != "false") %>%
  pull(part_id) %>%
  unique()

passed_screening_ids <- wp2_pre_excl %>%
  filter(!is.na(part_id), excluded_clean == "false") %>%
  pull(part_id) %>%
  unique()

denominator_audit <- tibble(
  Quantity = c(
    "Screened IDs in WP2 pre-screen",
    "Excluded IDs in WP2 pre-screen",
    "Passed-screening IDs in WP2 pre-screen",
    "Randomised/started IDs in assignments",
    "Analysed IDs with valid post-session-4 data",
    "Randomised IDs without valid post-session-4 data",
    "Post-session-4 IDs not in assignments"
  ),
  N = c(
    length(screened_ids),
    length(excluded_ids),
    length(passed_screening_ids),
    length(randomised_ids),
    length(analysed_ids),
    length(setdiff(randomised_ids, analysed_ids)),
    length(setdiff(wp2_post4_valid$part_id, randomised_ids))
  ),
  IDs = c(
    paste(sort(screened_ids), collapse = "; "),
    paste(sort(excluded_ids), collapse = "; "),
    paste(sort(passed_screening_ids), collapse = "; "),
    paste(sort(randomised_ids), collapse = "; "),
    paste(sort(analysed_ids), collapse = "; "),
    paste(sort(setdiff(randomised_ids, analysed_ids)), collapse = "; "),
    paste(sort(setdiff(wp2_post4_valid$part_id, randomised_ids)), collapse = "; ")
  )
)

cat("\n=== WP2 denominator audit ===\n")
print(denominator_audit %>% select(Quantity, N), n = Inf)

if (USE_HARD_DENOMINATOR_CHECKS) {
  if (length(screened_ids) != EXPECTED_SCREENED_N) {
    stop("Screened n mismatch: expected ", EXPECTED_SCREENED_N, ", got ", length(screened_ids))
  }
  
  if (length(randomised_ids) != EXPECTED_RANDOMISED_N) {
    stop("Randomised/started n mismatch: expected ", EXPECTED_RANDOMISED_N, ", got ", length(randomised_ids))
  }
  
  if (length(analysed_ids) != EXPECTED_ANALYSED_N) {
    stop("Analysed n mismatch: expected ", EXPECTED_ANALYSED_N, ", got ", length(analysed_ids))
  }
}

# ============================================================
# 11. BUILD MASTER TABLE
# ============================================================

all_ids <- unique(c(screened_ids, randomised_ids, analysed_ids))

wp2_master <- tibble(part_id = all_ids) %>%
  left_join(demo_pre, by = "part_id") %>%
  left_join(
    wp2_pre_scored %>%
      select(part_id, prescreen_phq9),
    by = "part_id"
  ) %>%
  left_join(
    wp2_pre1_scored %>%
      select(
        part_id,
        baseline_phq9,
        baseline_bdi,
        baseline_bai,
        baseline_madrs,
        baseline_phq9_severity
      ),
    by = "part_id"
  ) %>%
  left_join(
    wp2_assign_clean %>%
      select(part_id, condition),
    by = "part_id"
  ) %>%
  mutate(
    age = age_pre,
    sex = sex_pre,
    gender = gender_pre,
    psychoactive_medication_status = med_pre,
    baseline_phq9 = coalesce(baseline_phq9, prescreen_phq9),
    baseline_phq9_severity = coalesce(
      baseline_phq9_severity,
      phq9_severity(baseline_phq9)
    ),
    stage_screened = part_id %in% screened_ids,
    stage_randomised = part_id %in% randomised_ids,
    stage_analysed = part_id %in% analysed_ids
  )

needed_cols <- c(
  "age",
  "sex",
  "gender",
  "psychoactive_medication_status",
  "baseline_phq9",
  "baseline_phq9_severity",
  "baseline_bdi",
  "baseline_madrs",
  "baseline_bai",
  "condition"
)

for (col in needed_cols) {
  if (!col %in% names(wp2_master)) wp2_master[[col]] <- NA
}

# ============================================================
# 12. GROUPS FOR TABLE
# ============================================================

group_defs <- list(
  "Screened overall" = wp2_master %>% filter(stage_screened),
  "Randomised overall" = wp2_master %>% filter(stage_randomised),
  "Randomised Control" = wp2_master %>% filter(stage_randomised, condition == "Control"),
  "Randomised Intervention" = wp2_master %>% filter(stage_randomised, condition == "Intervention"),
  "Analysed overall" = wp2_master %>% filter(stage_analysed),
  "Analysed Control" = wp2_master %>% filter(stage_analysed, condition == "Control"),
  "Analysed Intervention" = wp2_master %>% filter(stage_analysed, condition == "Intervention")
)

group_n_table <- tibble(
  Group = names(group_defs),
  N = map_int(group_defs, nrow)
)

cat("\n=== WP2 table group denominators ===\n")
print(group_n_table, n = Inf)

# ============================================================
# 13. SUMMARISE TABLE VALUES
# ============================================================

summary_rows <- list()
missing_rows <- list()

for (group_name in names(group_defs)) {
  df_group <- group_defs[[group_name]]
  
  summary_rows[[length(summary_rows) + 1]] <- summarise_numeric(df_group, "age", group_name, "Age")
  summary_rows[[length(summary_rows) + 1]] <- summarise_categorical(df_group, "sex", group_name, "Sex")
  summary_rows[[length(summary_rows) + 1]] <- summarise_categorical(df_group, "gender", group_name, "Gender")
  summary_rows[[length(summary_rows) + 1]] <- summarise_categorical(df_group, "psychoactive_medication_status", group_name, "Psychoactive medication status")
  summary_rows[[length(summary_rows) + 1]] <- summarise_categorical(df_group, "baseline_phq9_severity", group_name, "Baseline PHQ-9 severity stratum")
  summary_rows[[length(summary_rows) + 1]] <- summarise_numeric(df_group, "baseline_phq9", group_name, "Baseline PHQ-9")
  summary_rows[[length(summary_rows) + 1]] <- summarise_numeric(df_group, "baseline_bdi", group_name, "Baseline BDI-II")
  summary_rows[[length(summary_rows) + 1]] <- summarise_numeric(df_group, "baseline_madrs", group_name, "Baseline MADRS-S")
  summary_rows[[length(summary_rows) + 1]] <- summarise_numeric(df_group, "baseline_bai", group_name, "Baseline BAI")
  
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_group, "age", group_name, "Age")
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_group, "sex", group_name, "Sex")
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_group, "gender", group_name, "Gender")
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_group, "psychoactive_medication_status", group_name, "Psychoactive medication status")
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_group, "baseline_phq9_severity", group_name, "Baseline PHQ-9 severity stratum")
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_group, "baseline_phq9", group_name, "Baseline PHQ-9")
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_group, "baseline_bdi", group_name, "Baseline BDI-II")
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_group, "baseline_madrs", group_name, "Baseline MADRS-S")
  missing_rows[[length(missing_rows) + 1]] <- missing_audit(df_group, "baseline_bai", group_name, "Baseline BAI")
}

wp2_demo_summary <- bind_rows(summary_rows) %>%
  mutate(
    Percent = round(Percent, 1),
    Mean = round(Mean, 2),
    SD = round(SD, 2),
    Min = round(Min, 2),
    Max = round(Max, 2)
  )

wp2_missing_denominators <- bind_rows(missing_rows) %>%
  mutate(Missing_percent = round(Missing_percent, 1))

# ============================================================
# 14. FORMAT MAIN TABLE
# ============================================================

demo_numeric <- wp2_demo_summary %>%
  filter(is.na(Level)) %>%
  mutate(
    Display = format_numeric_summary(Mean, SD, Min, Max, Non_missing_n, Missing_n)
  ) %>%
  select(Group, Variable, Display)

demo_categorical <- wp2_demo_summary %>%
  filter(
    !is.na(Level),
    Level != "Unavailable / missing",
    Count > 0
  ) %>%
  mutate(
    Display = paste0(
      Count,
      "/",
      Denominator,
      " (",
      sprintf("%.1f", 100 * Count / Denominator),
      "%)"
    )
  ) %>%
  select(Group, Variable, Level, Display) %>%
  mutate(Variable = paste0(Variable, ": ", Level)) %>%
  select(Group, Variable, Display)

demo_unavailable <- wp2_demo_summary %>%
  filter(
    !is.na(Level),
    Level == "Unavailable / missing",
    Count > 0
  ) %>%
  mutate(
    Display = paste0(Count, "/", Stage_denominator, " unavailable")
  ) %>%
  select(Group, Variable, Level, Display) %>%
  mutate(Variable = paste0(Variable, ": Unavailable / missing")) %>%
  select(Group, Variable, Display)

demo_table_wide <- bind_rows(
  demo_numeric,
  demo_categorical,
  demo_unavailable
) %>%
  pivot_wider(
    names_from = Group,
    values_from = Display
  ) %>%
  mutate(across(everything(), ~ replace_na(.x, ""))) %>%
  rename(Characteristic = Variable)

preferred_order <- c(
  "Age",
  "Sex: Female",
  "Sex: Male",
  "Sex: Other",
  "Sex: Prefer not to say",
  "Sex: Unavailable / missing",
  "Gender: Female",
  "Gender: Male",
  "Gender: Other",
  "Gender: Non-binary",
  "Gender: Prefer not to say",
  "Gender: Unavailable / missing",
  "Psychoactive medication status: Yes",
  "Psychoactive medication status: No",
  "Psychoactive medication status: Unavailable / missing",
  "Baseline PHQ-9 severity stratum: Minimal (0–4)",
  "Baseline PHQ-9 severity stratum: Mild (5–9)",
  "Baseline PHQ-9 severity stratum: Moderate (10–14)",
  "Baseline PHQ-9 severity stratum: Moderately severe (15–19)",
  "Baseline PHQ-9 severity stratum: Severe (20–27)",
  "Baseline PHQ-9 severity stratum: Unavailable / missing",
  "Baseline PHQ-9",
  "Baseline BDI-II",
  "Baseline MADRS-S",
  "Baseline BAI"
)

demo_table_wide <- demo_table_wide %>%
  mutate(
    .order = match(Characteristic, preferred_order),
    .order = ifelse(is.na(.order), 999, .order)
  ) %>%
  arrange(.order, Characteristic) %>%
  select(-.order)

# ============================================================
# 15. EXPORT MAIN TABLE
# ============================================================

demo_gt <- demo_table_wide %>%
  gt() %>%
  tab_header(
    title = md("**WP2 Stage-Specific Demographic and Baseline Clinical Characteristics**"),
    subtitle = md(
      "Demographics are from WP2 pre-screening and linked to randomised arm through assignments. Continuous variables are mean ± SD [range]. Categorical variables are n / non-missing denominator (%)."
    )
  ) %>%
  tab_spanner(
    label = "Screened",
    columns = any_of(c("Screened overall"))
  ) %>%
  tab_spanner(
    label = "Randomised / started",
    columns = any_of(c("Randomised overall", "Randomised Control", "Randomised Intervention"))
  ) %>%
  tab_spanner(
    label = "Analysed at post-session 4",
    columns = any_of(c("Analysed overall", "Analysed Control", "Analysed Intervention"))
  ) %>%
  cols_align(align = "left", columns = Characteristic) %>%
  cols_align(align = "center", columns = -Characteristic) %>%
  tab_options(
    table.font.names = "Palatino Linotype",
    table.font.size = px(11),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(12),
    column_labels.font.weight = "bold",
    data_row.padding = px(3),
    source_notes.font.size = px(9),
    table.border.top.width = px(1),
    table.border.bottom.width = px(1)
  ) %>%
  tab_source_note(
    source_note = md(
      paste0(
        "Confirmed denominators: screened n = ", length(screened_ids),
        "; randomised/started n = ", length(randomised_ids),
        "; analysed at post-session 4 n = ", length(analysed_ids),
        ". Arm is taken from the WP2 assignments file. Analysed status is defined as randomised participants with valid post-session-4 data."
      )
    )
  )

gtsave(demo_gt, OUT_MAIN_HTML)
gtsave(demo_gt, OUT_MAIN_PNG)
gtsave(demo_gt, OUT_MAIN_PDF)

# ============================================================
# 16. EXPORT MISSING TABLE
# ============================================================

missing_table_export <- wp2_missing_denominators %>%
  mutate(
    Missing = paste0(
      Missing_n,
      "/",
      Stage_denominator,
      " (",
      sprintf("%.1f", Missing_percent),
      "%)"
    )
  ) %>%
  select(
    Group,
    Variable,
    Stage_denominator,
    Non_missing_n,
    Missing,
    Missing_part_ids
  ) %>%
  arrange(Group, Variable)

missing_gt <- missing_table_export %>%
  gt() %>%
  tab_header(
    title = md("**WP2 Missing-Denominator Audit**"),
    subtitle = md("Missingness is shown as missing n / group denominator (%), with participant IDs listed where available.")
  ) %>%
  cols_label(
    Group = "Group",
    Variable = "Variable",
    Stage_denominator = "Group denominator",
    Non_missing_n = "Non-missing n",
    Missing = "Missing / unavailable",
    Missing_part_ids = "Missing participant IDs"
  ) %>%
  cols_align(
    align = "center",
    columns = c(Group, Variable, Stage_denominator, Non_missing_n, Missing)
  ) %>%
  cols_align(align = "left", columns = Missing_part_ids) %>%
  tab_options(
    table.font.names = "Palatino Linotype",
    table.font.size = px(10),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(12),
    column_labels.font.weight = "bold",
    data_row.padding = px(3),
    source_notes.font.size = px(9)
  )

gtsave(missing_gt, OUT_MISSING_HTML)
gtsave(missing_gt, OUT_MISSING_PNG)
gtsave(missing_gt, OUT_MISSING_PDF)

# ============================================================
# 17. AUDITS AND TEXT EXPORTS
# ============================================================

id_audit <- wp2_master %>%
  mutate(
    In_screened = part_id %in% screened_ids,
    In_randomised = part_id %in% randomised_ids,
    In_analysed = part_id %in% analysed_ids,
    Has_assignment = !is.na(condition),
    Has_age = !is.na(suppressWarnings(as.numeric(age))),
    Has_sex = !is.na(clean_cat(sex)),
    Has_gender = !is.na(clean_cat(gender)),
    Has_medication = !is.na(clean_cat(psychoactive_medication_status)),
    Has_baseline_phq9 = !is.na(baseline_phq9),
    Has_baseline_bdi = !is.na(baseline_bdi),
    Has_baseline_madrs = !is.na(baseline_madrs),
    Has_baseline_bai = !is.na(baseline_bai)
  ) %>%
  arrange(desc(In_randomised), desc(In_analysed), condition, part_id)

readr::write_csv(wp2_demo_summary, OUT_MAIN_CSV)
readr::write_csv(wp2_missing_denominators, OUT_MISSING_CSV)
readr::write_csv(id_audit, OUT_ID_AUDIT_CSV)
readr::write_csv(denominator_audit, OUT_DENOM_AUDIT_CSV)

text_lines <- c(
  "WP2 supervisor demographics denominator audit",
  "============================================",
  "",
  paste0("WP2 pre-screen file: ", basename(WP2_PRE_PATH)),
  paste0("WP2 assignments file: ", basename(WP2_ASSIGN_PATH)),
  paste0("WP2 pre-session-1 file: ", basename(WP2_PRE1_PATH)),
  paste0("WP2 post-session-4 file: ", basename(WP2_POST4_PATH)),
  paste0("WP2 SMS post file: ", ifelse(is.null(WP2_SMS_POST_PATH), "not found", basename(WP2_SMS_POST_PATH))),
  "",
  "Confirmed manuscript denominators:",
  paste0("Screened: n = ", length(screened_ids)),
  paste0("Randomised / started: n = ", length(randomised_ids)),
  paste0("Analysed at post-session 4: n = ", length(analysed_ids)),
  "",
  "Group denominators:",
  paste0(group_n_table$Group, ": n = ", group_n_table$N),
  "",
  "Denominator audit:",
  paste0(denominator_audit$Quantity, ": n = ", denominator_audit$N),
  "",
  "Clinical availability among randomised participants:",
  paste0("Baseline PHQ-9: n = ", sum(!is.na(wp2_master$baseline_phq9[wp2_master$stage_randomised]))),
  paste0("Baseline BDI-II: n = ", sum(!is.na(wp2_master$baseline_bdi[wp2_master$stage_randomised]))),
  paste0("Baseline MADRS-S: n = ", sum(!is.na(wp2_master$baseline_madrs[wp2_master$stage_randomised]))),
  paste0("Baseline BAI: n = ", sum(!is.na(wp2_master$baseline_bai[wp2_master$stage_randomised]))),
  "",
  "Clinical availability among analysed participants:",
  paste0("Baseline PHQ-9: n = ", sum(!is.na(wp2_master$baseline_phq9[wp2_master$stage_analysed]))),
  paste0("Baseline BDI-II: n = ", sum(!is.na(wp2_master$baseline_bdi[wp2_master$stage_analysed]))),
  paste0("Baseline MADRS-S: n = ", sum(!is.na(wp2_master$baseline_madrs[wp2_master$stage_analysed]))),
  paste0("Baseline BAI: n = ", sum(!is.na(wp2_master$baseline_bai[wp2_master$stage_analysed])))
)

writeLines(text_lines, OUT_TEXT)

cat("\n=== Exported WP2 supervisor demographics files ===\n")
cat("Main table HTML:      ", OUT_MAIN_HTML, "\n")
cat("Main table PNG:       ", OUT_MAIN_PNG, "\n")
cat("Main table PDF:       ", OUT_MAIN_PDF, "\n")
cat("Missing table HTML:   ", OUT_MISSING_HTML, "\n")
cat("Missing table PNG:    ", OUT_MISSING_PNG, "\n")
cat("Missing table PDF:    ", OUT_MISSING_PDF, "\n")
cat("Main CSV:             ", OUT_MAIN_CSV, "\n")
cat("Missing CSV:          ", OUT_MISSING_CSV, "\n")
cat("ID audit CSV:         ", OUT_ID_AUDIT_CSV, "\n")
cat("Denominator audit:    ", OUT_DENOM_AUDIT_CSV, "\n")
cat("Text audit:           ", OUT_TEXT, "\n")

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




# ------------------------------------------------------------
# SECTION: wp2 assessment schedule
# ------------------------------------------------------------

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

# ============================================================
# SUPPLEMENTARY TABLE — WP2 ASSESSMENT / DATA-COLLECTION SCHEDULE
# CLEAN EXCEL EXPORT, NO WEIRD ENCODING CHARACTERS
# ============================================================
#
# Purpose:
#   Creates a clean Excel version of the WP2 assessment schedule.
#
# Why this version:
#   - Avoids Unicode checkmarks and em dashes that can become âœ“ / â€” in Excel.
#   - Writes directly to .xlsx using writexl.
#   - Also exports an optional UTF-8-BOM CSV for safer Excel opening.
#   - Provides wide, long, and notes tabs.
#
# Outputs:
#   supplementary_wp2_assessment_schedule/
#     WP2_Assessment_Schedule_Clean.xlsx
#     WP2_Assessment_Schedule_Clean_UTF8BOM.csv
#     WP2_Assessment_Schedule_Long.csv
#     WP2_Assessment_Schedule_Narrative.txt
#
# ============================================================

# install.packages(c("tidyverse", "writexl", "readr"))

library(tidyverse)
library(writexl)
library(readr)

# ============================================================
# 0. SETUP
# ============================================================

DATA_DIR <- if (exists("DATA_DIR")) DATA_DIR else "C:/Users/dn284/Desktop/MRC_omni/data"

OUT_DIR <- file.path(DATA_DIR, "supplementary_wp2_assessment_schedule")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

OUT_XLSX <- file.path(OUT_DIR, "WP2_Assessment_Schedule_Clean.xlsx")
OUT_CSV  <- file.path(OUT_DIR, "WP2_Assessment_Schedule_Clean_UTF8BOM.csv")
OUT_LONG <- file.path(OUT_DIR, "WP2_Assessment_Schedule_Long.csv")
OUT_TXT  <- file.path(OUT_DIR, "WP2_Assessment_Schedule_Narrative.txt")

# Excel-safe coding.
# Use plain ASCII text only.
YES <- "Collected"
NO  <- ""

# ============================================================
# 1. DEFINE WP2 ASSESSMENT SCHEDULE
# ============================================================

wp2_schedule <- tribble(
  ~Domain, ~Measure, ~Pre_screen, ~Pre_S1, ~Post_S1, ~SMS_S1, ~Pre_S2, ~Post_S2, ~SMS_S2, ~Pre_S3, ~Post_S3, ~SMS_S3, ~Pre_S4, ~Post_S4, ~Final_SMS,
  
  "Eligibility / allocation",
  "Eligibility screening, safety criteria, contact details",
  YES, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO,
  
  "Eligibility / allocation",
  "Randomisation / arm allocation",
  NO, YES, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO,
  
  "Baseline clinical symptoms",
  "PHQ-9",
  NO, YES, NO, YES, YES, NO, YES, YES, NO, YES, YES, YES, YES,
  
  "Baseline clinical symptoms",
  "BDI-II",
  NO, YES, NO, NO, NO, NO, NO, NO, NO, NO, NO, YES, NO,
  
  "Baseline clinical symptoms",
  "BAI",
  NO, YES, NO, NO, NO, NO, NO, NO, NO, NO, NO, YES, NO,
  
  "Baseline clinical symptoms",
  "MADRS-S",
  NO, YES, NO, NO, NO, NO, NO, NO, NO, NO, NO, YES, NO,
  
  "Affective state / wellbeing",
  "M3VAS",
  NO, YES, NO, YES, YES, NO, YES, YES, NO, YES, YES, YES, YES,
  
  "Affective state / wellbeing",
  "M3VAS-Change",
  NO, NO, YES, YES, NO, YES, YES, NO, YES, YES, NO, YES, YES,
  
  "Affective state / wellbeing",
  "SPANE",
  NO, YES, NO, YES, YES, NO, YES, YES, NO, YES, YES, YES, YES,
  
  "Expectancy / beliefs",
  "Treatment expectancy / credibility",
  NO, YES, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO,
  
  "Expectancy / beliefs",
  "SETS / expectancy-related items",
  NO, YES, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO, NO,
  
  "Acute experience",
  "11-ASC / altered-state phenomenology",
  NO, NO, YES, NO, NO, YES, NO, NO, YES, NO, NO, YES, NO,
  
  "Acute experience",
  "6D-VHQ visual phenomenology",
  NO, NO, YES, NO, NO, YES, NO, NO, YES, NO, NO, YES, NO,
  
  "Acute experience",
  "Engagement, pleasure, discomfort, drowsiness / session VAS ratings",
  NO, NO, YES, NO, NO, YES, NO, NO, YES, NO, NO, YES, NO,
  
  "Safety / tolerability",
  "VDQ / side-effect and tolerability ratings",
  NO, NO, YES, NO, NO, YES, NO, NO, YES, NO, NO, YES, NO,
  
  "Safety / tolerability",
  "Adverse events, discontinuations, safeguarding concerns",
  NO, YES, YES, YES, YES, YES, YES, YES, YES, YES, YES, YES, YES,
  
  "Feasibility / adherence",
  "Attendance / session completion",
  NO, YES, YES, NO, YES, YES, NO, YES, YES, NO, YES, YES, NO,
  
  "Feasibility / adherence",
  "Questionnaire completion / missingness",
  NO, YES, YES, YES, YES, YES, YES, YES, YES, YES, YES, YES, YES,
  
  "Feasibility / adherence",
  "Protocol adherence / missed sessions / withdrawal reasons",
  NO, YES, YES, YES, YES, YES, YES, YES, YES, YES, YES, YES, YES
)

# ============================================================
# 2. FRIENDLY COLUMN NAMES FOR EXCEL
# ============================================================

wp2_schedule_excel <- wp2_schedule %>%
  rename(
    `Pre-screen` = Pre_screen,
    `Pre-S1` = Pre_S1,
    `Post-S1` = Post_S1,
    `SMS after S1` = SMS_S1,
    `Pre-S2` = Pre_S2,
    `Post-S2` = Post_S2,
    `SMS after S2` = SMS_S2,
    `Pre-S3` = Pre_S3,
    `Post-S3` = Post_S3,
    `SMS after S3` = SMS_S3,
    `Pre-S4` = Pre_S4,
    `Post-S4` = Post_S4,
    `SMS post / final follow-up` = Final_SMS
  )

# ============================================================
# 3. LONG FORMAT VERSION
# ============================================================

wp2_schedule_long <- wp2_schedule_excel %>%
  pivot_longer(
    cols = -c(Domain, Measure),
    names_to = "Timepoint",
    values_to = "Collection status"
  ) %>%
  mutate(
    Collected = `Collection status` == YES
  )

# ============================================================
# 4. OPTIONAL COMPACT EXCEL VERSION
# ============================================================
# This version uses "Y" instead of "Collected", which can be easier
# to read in narrow Excel columns.

wp2_schedule_compact <- wp2_schedule_excel %>%
  mutate(
    across(
      -c(Domain, Measure),
      ~ ifelse(.x == YES, "Y", "")
    )
  )

# ============================================================
# 5. NOTES / ABBREVIATIONS
# ============================================================

notes <- tibble(
  Note = c(
    "Supplementary Table S8. WP2 assessment and data-collection schedule.",
    "Cells marked 'Collected' indicate scheduled collection points.",
    "The compact tab uses 'Y' to indicate scheduled collection points.",
    "SMS follow-ups were scheduled after Sessions 1-3, with final post-protocol follow-up after Session 4.",
    "This Excel export intentionally avoids Unicode checkmarks and em dashes to prevent encoding artifacts such as âœ“ or â€”."
  )
)

abbreviations <- tibble(
  Abbreviation = c(
    "PHQ-9",
    "BDI-II",
    "BAI",
    "MADRS-S",
    "M3VAS",
    "SPANE",
    "11-ASC",
    "6D-VHQ",
    "VDQ",
    "SETS",
    "SMS"
  ),
  Definition = c(
    "Patient Health Questionnaire-9",
    "Beck Depression Inventory-II",
    "Beck Anxiety Inventory",
    "Montgomery-Asberg Depression Rating Scale - Self-report",
    "Maudsley 3-item Visual Analogue Scale",
    "Scale of Positive and Negative Experience",
    "11-dimensional Altered States of Consciousness scale",
    "Six-Dimensional Visual Hallucination Questionnaire",
    "Visual Discomfort Questionnaire",
    "Stanford Expectations of Treatment Scale / expectancy-related items",
    "Short message service follow-up"
  )
)

narrative <- c(
  "Supplementary Table S8 summarises the WP2 assessment and data-collection schedule.",
  "",
  "Eligibility and safety screening were completed at pre-screening, followed by baseline clinical, expectancy, and allocation-related measures at the first pre-session assessment. Clinical symptom measures were collected longitudinally across the protocol, with PHQ-9, M3VAS, SPANE, and short-term change ratings repeated around the supervised SLS sessions and SMS follow-ups. Broader clinical scales, including BDI-II, BAI, and MADRS-S, were collected at baseline and endpoint.",
  "",
  "Acute experiential measures, including altered-state phenomenology, visual phenomenology, engagement, pleasure, discomfort, drowsiness, and tolerability ratings, were collected after each supervised stimulation session. Safety, adverse-event, attendance, questionnaire-completion, missingness, and protocol-adherence information was tracked throughout the intervention period and final follow-up."
)

# ============================================================
# 6. EXPORT CLEAN EXCEL + CSV FILES
# ============================================================

excel_sheets <- list(
  "Schedule" = as.data.frame(wp2_schedule_excel),
  "Schedule compact" = as.data.frame(wp2_schedule_compact),
  "Schedule long" = as.data.frame(wp2_schedule_long),
  "Notes" = as.data.frame(notes),
  "Abbreviations" = as.data.frame(abbreviations)
)

writexl::write_xlsx(excel_sheets, OUT_XLSX)

# UTF-8-BOM CSV is safer for direct opening in Excel than plain UTF-8 CSV.
readr::write_excel_csv(wp2_schedule_excel, OUT_CSV)
readr::write_csv(wp2_schedule_long, OUT_LONG)

writeLines(narrative, OUT_TXT)

# ============================================================
# 7. CONSOLE OUTPUT
# ============================================================

cat("\n============================================================\n")
cat("WP2 ASSESSMENT SCHEDULE CLEAN EXCEL EXPORT COMPLETE\n")
cat("============================================================\n\n")

cat("Output directory:\n")
cat("  ", OUT_DIR, "\n\n")

cat("Files written:\n")
cat("  ", OUT_XLSX, "\n")
cat("  ", OUT_CSV, "\n")
cat("  ", OUT_LONG, "\n")
cat("  ", OUT_TXT, "\n\n")

cat("Preview:\n")
print(wp2_schedule_excel, n = Inf, width = Inf)

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


# ------------------------------------------------------------
# SECTION: wp2 data collection details
# ------------------------------------------------------------

# ------------------------------------------------------------
# SECTION: wp2 scale definitions
# ------------------------------------------------------------

# ------------------------------------------------------------
# SECTION: wp2 attendance by arm/session
# ------------------------------------------------------------






# ------------------------------------------------------------
# SECTION: wp2 data-collection adherence
# ------------------------------------------------------------

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

# ==========================================================
# DROP-IN R SCRIPT:
# WP2 Data-Collection Adherence Table
# Pre / Post / SMS1 / SMS2 / SMS3 / Final SMS
# Intervention column first + total observed/expected row
# ==========================================================
#
# Outputs:
#   WP2_DataCollectionAdherence_Detailed_EventLevel.csv
#   WP2_DataCollectionAdherence_Detailed_ByArmEvent.csv
#   WP2_DataCollectionAdherence_Detailed_Wide.csv
#   WP2_DataCollectionAdherence_Detailed_IDEventLevel.csv
#   WP2_DataCollectionAdherence_Detailed_Table.png
#   WP2_DataCollectionAdherence_Detailed_Narrative.txt
#   WP2_DataCollectionAdherence_Detailed_All.xlsx
#
# Core logic:
#   - Condition comes only from wp2_assignments.
#   - Pre/post expected denominator = participants with evidence of attending that session.
#   - SMS 1/2/3 expected denominator = participants with evidence of attending the preceding session.
#   - Final SMS/post-treatment expected denominator = participants with evidence of attending Session 4.
#   - Total row sums observed/expected event-person observations across all listed events.
#
# ==========================================================

# ----------------------------------------------------------
# 0. Packages
# ----------------------------------------------------------

needed_pkgs <- c(
  "tidyverse",
  "stringr",
  "readr",
  "openxlsx",
  "ggplot2",
  "showtext",
  "sysfonts"
)

to_install <- needed_pkgs[!needed_pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(stringr)
library(readr)
library(openxlsx)
library(ggplot2)
library(showtext)
library(sysfonts)

# ----------------------------------------------------------
# 1. Setup
# ----------------------------------------------------------

if (!exists("DATA_DIR")) {
  DATA_DIR <- Sys.getenv("MRC_DATA_DIR", unset = "C:/Users/dn284/Desktop/MRC_omni/data")
}

SEARCH_DIRS <- c(DATA_DIR)

OUT_PREFIX <- file.path(
  DATA_DIR,
  "WP2_DataCollectionAdherence_Detailed"
)

FONT_PATH <- file.path(DATA_DIR, "palatinolinotype_roman.ttf")

if (file.exists(FONT_PATH)) {
  font_add("PalatinoLinotype", regular = FONT_PATH)
  showtext_auto()
  PALATINO_NAME <- "PalatinoLinotype"
} else {
  PALATINO_NAME <- "serif"
}

# ----------------------------------------------------------
# 2. Helpers
# ----------------------------------------------------------

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
      
      matched <- all_files[
        str_detect(file_names, regex(pat_regex, ignore_case = TRUE))
      ]
      
      hits <- c(hits, matched)
    }
  }
  
  hits <- unique(hits[file.exists(hits)])
  
  if (length(hits) == 0) {
    if (required) {
      stop("No files found for patterns: ", paste(patterns, collapse = ", "))
    }
    return(NULL)
  }
  
  info <- file.info(hits)
  hits[order(info$mtime, decreasing = TRUE)][1]
}

read_qualtrics_real <- function(path, skiprows = NULL) {
  if (is.null(path)) return(NULL)
  
  df <- readr::read_csv(
    path,
    col_types = cols(.default = col_character()),
    skip = skiprows %||% 0,
    show_col_types = FALSE
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

find_col <- function(df, candidates) {
  if (is.null(df)) return(NULL)
  
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

standardise_part_id <- function(df, file_label = "unknown file") {
  if (is.null(df)) return(NULL)
  
  id_col <- find_col(
    df,
    c(
      "part_id",
      "participant_id",
      "participant id",
      "participant",
      "participant_number",
      "participant number",
      "pid",
      "subject_id",
      "subject id",
      "subject",
      "id"
    )
  )
  
  if (is.null(id_col)) {
    stop(
      "Could not find participant ID column in ", file_label, ".\n",
      "Available columns are:\n",
      paste(names(df), collapse = ", ")
    )
  }
  
  df %>%
    mutate(part_id = clean_id(.data[[id_col]]))
}

standardise_condition <- function(x) {
  x_chr <- str_to_lower(str_trim(as.character(x)))
  
  case_when(
    str_detect(x_chr, "intervention|active|treatment|sls") ~ "Intervention",
    str_detect(x_chr, "control|placebo|sham") ~ "Control",
    x_chr %in% c("1", "i", "int", "active") ~ "Intervention",
    x_chr %in% c("0", "c", "ctrl") ~ "Control",
    TRUE ~ NA_character_
  )
}

drop_condition_like_cols <- function(df) {
  if (is.null(df)) return(NULL)
  
  condition_like <- names(df)[
    tolower(names(df)) %in% c(
      "condition",
      "condition.x",
      "condition.y",
      "allocation",
      "group",
      "arm",
      "assigned_condition",
      "randomised_condition",
      "randomized_condition",
      "treatment"
    )
  ]
  
  if (length(condition_like) > 0) {
    df <- df %>% select(-all_of(condition_like))
  }
  
  df
}

get_session_col <- function(df) {
  find_col(
    df,
    c(
      "session_n",
      "session",
      "session_number",
      "session number",
      "visit",
      "visit_n",
      "visit_number"
    )
  )
}

get_sms_n_col <- function(df) {
  find_col(
    df,
    c(
      "sms_n",
      "sms",
      "sms_number",
      "sms number",
      "sms_timepoint",
      "followup_n",
      "follow_up_n",
      "day",
      "sms_day",
      "followup_day",
      "follow_up_day"
    )
  )
}

add_session_n <- function(df, default_session = NA_integer_) {
  if (is.null(df)) return(NULL)
  
  session_col <- get_session_col(df)
  
  if (is.null(session_col)) {
    df$session_n <- default_session
  } else {
    parsed <- suppressWarnings(as.integer(readr::parse_number(df[[session_col]])))
    df$session_n <- ifelse(is.na(parsed), default_session, parsed)
  }
  
  df
}

add_sms_n <- function(df, default_sms = NA_integer_) {
  if (is.null(df)) return(NULL)
  
  sms_col <- get_sms_n_col(df)
  
  if (is.null(sms_col)) {
    df$sms_n <- default_sms
  } else {
    parsed <- suppressWarnings(as.integer(readr::parse_number(df[[sms_col]])))
    df$sms_n <- ifelse(is.na(parsed), default_sms, parsed)
  }
  
  df
}

fmt_n_pct_one <- function(n, d) {
  if (is.na(d) || d == 0) return(paste0(n, "/0 (NA%)"))
  paste0(n, "/", d, " (", sprintf("%.1f", 100 * n / d), "%)")
}

fmt_n_pct_vec <- function(n, d) {
  mapply(fmt_n_pct_one, n, d, USE.NAMES = FALSE)
}

pct_vec <- function(n, d) {
  n <- as.numeric(n)
  d <- as.numeric(d)
  ifelse(is.na(d) | d == 0, NA_real_, 100 * n / d)
}

safe_bind <- function(...) {
  bind_rows(...)
}

# ----------------------------------------------------------
# 3. Locate files
# ----------------------------------------------------------

WP2_ASSIGN_PATH <- newest_match(
  c("*wp2_assignments*.csv"),
  required = TRUE
)

WP2_PRE1_PATH <- newest_match(
  c("*wp2_pre_session_1*.csv", "*pre_session_1*.csv"),
  required = TRUE
)

WP2_PRE24_PATH <- newest_match(
  c(
    "*wp2_pre_sessions_2-4*.csv",
    "*pre_sessions_2-4*.csv",
    "*pre_sessions_2_4*.csv"
  ),
  required = FALSE
)

WP2_POST13_PATH <- newest_match(
  c(
    "*wp2_post_sessions_1-3*.csv",
    "*wp2_post_session_1_3*.csv",
    "*wp2_post_session_13*.csv",
    "*wp2_post*session*1*3*.csv",
    "*post_sessions_1-3*.csv"
  ),
  required = FALSE
)

WP2_POST4_PATH <- newest_match(
  c("*wp2_post_session_4*.csv", "*post_session_4*.csv"),
  required = FALSE
)

WP2_SMS_D135_PATH <- newest_match(
  c(
    "*wp2_sms_day1,3,5*.csv",
    "*sms_day1,3,5*.csv",
    "*sms*day1*3*5*.csv"
  ),
  required = FALSE
)

WP2_SMS_POST_PATH <- newest_match(
  c(
    "*wp2_sms_post*.csv",
    "*sms_post*.csv",
    "*WP2*SMS*post*.csv"
  ),
  required = FALSE
)

message("Using assignment file:      ", basename(WP2_ASSIGN_PATH))
message("Using pre-session 1:        ", basename(WP2_PRE1_PATH))
message("Using pre-sessions 2-4:     ", ifelse(is.null(WP2_PRE24_PATH), "NULL", basename(WP2_PRE24_PATH)))
message("Using post-sessions 1-3:    ", ifelse(is.null(WP2_POST13_PATH), "NULL", basename(WP2_POST13_PATH)))
message("Using post-session 4:       ", ifelse(is.null(WP2_POST4_PATH), "NULL", basename(WP2_POST4_PATH)))
message("Using SMS 1/3/5 file:       ", ifelse(is.null(WP2_SMS_D135_PATH), "NULL", basename(WP2_SMS_D135_PATH)))
message("Using SMS post/final file:  ", ifelse(is.null(WP2_SMS_POST_PATH), "NULL", basename(WP2_SMS_POST_PATH)))

# ----------------------------------------------------------
# 4. Load assignments
# ----------------------------------------------------------

wp2_assign <- read_qualtrics_real(WP2_ASSIGN_PATH) %>%
  standardise_part_id(
    file_label = paste0("assignments / ", basename(WP2_ASSIGN_PATH))
  )

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
    "Could not identify condition/allocation column in wp2_assignments.\n",
    "Available columns are:\n",
    paste(names(wp2_assign), collapse = ", ")
  )
}

wp2_assign_clean <- wp2_assign %>%
  mutate(condition = standardise_condition(.data[[condition_col]])) %>%
  filter(!is.na(part_id), !is.na(condition)) %>%
  distinct(part_id, .keep_all = TRUE) %>%
  select(part_id, condition)

arm_denoms <- wp2_assign_clean %>%
  count(condition, name = "randomised_N") %>%
  mutate(condition = factor(condition, levels = c("Intervention", "Control"))) %>%
  arrange(condition)

# ----------------------------------------------------------
# 5. Read pre/post session files
# ----------------------------------------------------------

read_wp2_event_file <- function(path, label, default_session = NA_integer_) {
  if (is.null(path)) return(NULL)
  
  read_qualtrics_real(path) %>%
    standardise_part_id(
      file_label = paste0(label, " / ", basename(path))
    ) %>%
    drop_condition_like_cols() %>%
    add_session_n(default_session = default_session) %>%
    mutate(
      source_file = basename(path),
      source_type = label
    ) %>%
    left_join(wp2_assign_clean, by = "part_id")
}

pre1 <- read_wp2_event_file(WP2_PRE1_PATH, "pre_session", default_session = 1)
pre24 <- read_wp2_event_file(WP2_PRE24_PATH, "pre_session", default_session = NA_integer_)
post13 <- read_wp2_event_file(WP2_POST13_PATH, "post_session", default_session = NA_integer_)
post4 <- read_wp2_event_file(WP2_POST4_PATH, "post_session", default_session = 4)

all_pre_sessions <- safe_bind(pre1, pre24) %>%
  filter(!is.na(part_id), !is.na(condition), session_n %in% 1:4)

all_post_sessions <- safe_bind(post13, post4) %>%
  filter(!is.na(part_id), !is.na(condition), session_n %in% 1:4)

pre_observed <- all_pre_sessions %>%
  distinct(part_id, condition, session_n) %>%
  mutate(pre_observed = TRUE)

post_observed <- all_post_sessions %>%
  distinct(part_id, condition, session_n) %>%
  mutate(post_observed = TRUE)

attendance_by_session <- full_join(
  pre_observed,
  post_observed,
  by = c("part_id", "condition", "session_n")
) %>%
  mutate(
    pre_observed = replace_na(pre_observed, FALSE),
    post_observed = replace_na(post_observed, FALSE),
    attended_session = pre_observed | post_observed
  )

# ----------------------------------------------------------
# 6. Read SMS files with explicit session_n + sms_n
# ----------------------------------------------------------

read_sms_file <- function(path, label, default_session = NA_integer_, default_sms = NA_integer_) {
  if (is.null(path)) return(NULL)
  
  read_qualtrics_real(path) %>%
    standardise_part_id(
      file_label = paste0(label, " / ", basename(path))
    ) %>%
    drop_condition_like_cols() %>%
    add_session_n(default_session = default_session) %>%
    add_sms_n(default_sms = default_sms) %>%
    mutate(
      source_file = basename(path),
      source_type = label
    ) %>%
    left_join(wp2_assign_clean, by = "part_id")
}

sms_d135_raw <- read_sms_file(
  WP2_SMS_D135_PATH,
  "sms_1_2_3_after_sessions_1_2_3",
  default_session = NA_integer_,
  default_sms = NA_integer_
)

sms_post_raw <- read_sms_file(
  WP2_SMS_POST_PATH,
  "sms_post_final",
  default_session = 4,
  default_sms = NA_integer_
)

# SMS 1/2/3 after Sessions 1/2/3
if (!is.null(sms_d135_raw)) {
  sms_123_observed <- sms_d135_raw %>%
    filter(
      !is.na(part_id),
      !is.na(condition),
      session_n %in% 1:3,
      sms_n %in% 1:3
    ) %>%
    distinct(part_id, condition, session_n, sms_n) %>%
    mutate(
      event_family = "SMS",
      event_label = paste0("SMS ", sms_n, " after Session ", session_n),
      event_order = session_n * 10 + sms_n / 10,
      observed = TRUE
    )
} else {
  sms_123_observed <- tibble(
    part_id = character(0),
    condition = character(0),
    session_n = integer(0),
    sms_n = integer(0),
    event_family = character(0),
    event_label = character(0),
    event_order = numeric(0),
    observed = logical(0)
  )
}

# Final SMS / post-treatment follow-up after Session 4
if (!is.null(sms_post_raw)) {
  sms_post_observed <- sms_post_raw %>%
    filter(!is.na(part_id), !is.na(condition)) %>%
    distinct(part_id, condition) %>%
    mutate(
      session_n = 4L,
      sms_n = NA_integer_,
      event_family = "SMS",
      event_label = "Final SMS / post-treatment follow-up",
      event_order = 45,
      observed = TRUE
    )
} else {
  sms_post_observed <- tibble(
    part_id = character(0),
    condition = character(0),
    session_n = integer(0),
    sms_n = integer(0),
    event_family = character(0),
    event_label = character(0),
    event_order = numeric(0),
    observed = logical(0)
  )
}

# ----------------------------------------------------------
# 7. Build expected event-person dataset
# ----------------------------------------------------------

# Pre/post expected if participant attended that session.
pre_expected <- attendance_by_session %>%
  filter(session_n %in% 1:4) %>%
  transmute(
    part_id,
    condition,
    event_order = session_n * 10 - 1,
    event_family = "Pre-session",
    session_n,
    sms_n = NA_integer_,
    event_label = paste0("Pre Session ", session_n),
    expected = attended_session,
    observed = pre_observed
  )

post_expected <- attendance_by_session %>%
  filter(session_n %in% 1:4) %>%
  transmute(
    part_id,
    condition,
    event_order = session_n * 10,
    event_family = "Post-session",
    session_n,
    sms_n = NA_integer_,
    event_label = paste0("Post Session ", session_n),
    expected = attended_session,
    observed = post_observed
  )

# SMS 1/2/3 expected after each attended Session 1/2/3.
sms_123_expected <- expand_grid(
  sms_n = 1:3
) %>%
  crossing(
    attendance_by_session %>%
      filter(session_n %in% 1:3, attended_session) %>%
      distinct(part_id, condition, session_n)
  ) %>%
  mutate(
    event_order = session_n * 10 + sms_n / 10,
    event_family = "SMS",
    event_label = paste0("SMS ", sms_n, " after Session ", session_n),
    expected = TRUE
  ) %>%
  left_join(
    sms_123_observed %>%
      select(part_id, condition, session_n, sms_n, observed),
    by = c("part_id", "condition", "session_n", "sms_n")
  ) %>%
  mutate(observed = replace_na(observed, FALSE))

# Final SMS expected after attended Session 4.
sms_post_expected <- attendance_by_session %>%
  filter(session_n == 4, attended_session) %>%
  distinct(part_id, condition, session_n) %>%
  mutate(
    sms_n = NA_integer_,
    event_order = 45,
    event_family = "SMS",
    event_label = "Final SMS / post-treatment follow-up",
    expected = TRUE
  ) %>%
  left_join(
    sms_post_observed %>%
      select(part_id, condition, observed),
    by = c("part_id", "condition")
  ) %>%
  mutate(observed = replace_na(observed, FALSE))

id_event_level <- bind_rows(
  pre_expected,
  post_expected,
  sms_123_expected,
  sms_post_expected
) %>%
  filter(expected) %>%
  mutate(
    condition = factor(condition, levels = c("Intervention", "Control"))
  ) %>%
  arrange(event_order, condition, part_id)

write_csv(id_event_level, paste0(OUT_PREFIX, "_IDEventLevel.csv"))

# ----------------------------------------------------------
# 8. Summaries
# ----------------------------------------------------------

event_summary <- id_event_level %>%
  group_by(event_order, event_family, event_label) %>%
  summarise(
    expected_n = n_distinct(part_id),
    observed_n = n_distinct(part_id[observed]),
    missing_n = expected_n - observed_n,
    observed_percent = pct_vec(observed_n, expected_n),
    observed_nN_percent = fmt_n_pct_vec(observed_n, expected_n),
    .groups = "drop"
  ) %>%
  arrange(event_order)

by_arm_event <- id_event_level %>%
  group_by(event_order, event_family, event_label, condition) %>%
  summarise(
    expected_n = n_distinct(part_id),
    observed_n = n_distinct(part_id[observed]),
    missing_n = expected_n - observed_n,
    observed_percent = pct_vec(observed_n, expected_n),
    observed_nN_percent = fmt_n_pct_vec(observed_n, expected_n),
    .groups = "drop"
  ) %>%
  mutate(condition = factor(condition, levels = c("Intervention", "Control"))) %>%
  arrange(event_order, condition)

# Total row by arm: event-person observations.
by_arm_total <- id_event_level %>%
  group_by(condition) %>%
  summarise(
    event_order = 999,
    event_family = "Total",
    event_label = "Total observed/expected across events",
    expected_n = n(),
    observed_n = sum(observed, na.rm = TRUE),
    missing_n = expected_n - observed_n,
    observed_percent = pct_vec(observed_n, expected_n),
    observed_nN_percent = fmt_n_pct_vec(observed_n, expected_n),
    .groups = "drop"
  ) %>%
  mutate(condition = factor(condition, levels = c("Intervention", "Control"))) %>%
  select(event_order, event_family, event_label, condition, everything())

overall_total <- id_event_level %>%
  summarise(
    event_order = 999,
    event_family = "Total",
    event_label = "Total observed/expected across events",
    expected_n = n(),
    observed_n = sum(observed, na.rm = TRUE),
    missing_n = expected_n - observed_n,
    observed_percent = pct_vec(observed_n, expected_n),
    observed_nN_percent = fmt_n_pct_vec(observed_n, expected_n),
    .groups = "drop"
  )

event_summary_with_total <- bind_rows(event_summary, overall_total)

by_arm_event_with_total <- bind_rows(by_arm_event, by_arm_total) %>%
  arrange(event_order, condition)

wide_table <- by_arm_event_with_total %>%
  select(event_order, event_label, condition, observed_nN_percent) %>%
  pivot_wider(
    names_from = condition,
    values_from = observed_nN_percent
  ) %>%
  left_join(
    event_summary_with_total %>%
      select(event_order, event_label, Overall = observed_nN_percent),
    by = c("event_order", "event_label")
  ) %>%
  arrange(event_order) %>%
  transmute(
    `Data-collection event` = event_label,
    `Intervention observed/expected, n/N (%)` = Intervention,
    `Control observed/expected, n/N (%)` = Control,
    `Overall observed/expected, n/N (%)` = Overall
  )

write_csv(event_summary_with_total, paste0(OUT_PREFIX, "_EventLevel.csv"))
write_csv(by_arm_event_with_total, paste0(OUT_PREFIX, "_ByArmEvent.csv"))
write_csv(wide_table, paste0(OUT_PREFIX, "_Wide.csv"))

# ----------------------------------------------------------
# 9. Narrative
# ----------------------------------------------------------

total_overall <- overall_total$observed_nN_percent[[1]]

lowest_events <- event_summary %>%
  arrange(observed_percent) %>%
  head(5) %>%
  mutate(
    line = paste0(
      event_label,
      ": ",
      observed_nN_percent,
      ", missing n = ",
      missing_n
    )
  ) %>%
  pull(line)

narrative_lines <- c(
  "WP2 DATA-COLLECTION ADHERENCE",
  "============================",
  "",
  "Data-collection adherence was calculated as observed/expected responses at each scheduled data-collection event. Pre-session and post-session forms were expected for participants with evidence of attending the corresponding session. The three SMS follow-ups after Sessions 1, 2, and 3 were treated as separate expected events using session_n and sms_n. Final SMS/post-treatment follow-up was expected for participants with evidence of attending Session 4.",
  "",
  paste0(
    "Across all expected event-person observations, total data-collection adherence was ",
    total_overall,
    "."
  ),
  "",
  "Lowest-completion events:",
  lowest_events,
  "",
  "Interpretation note:",
  "These are data-collection adherence denominators rather than randomisation denominators. The expected denominator changes across events because post-session and SMS data are only expected from participants who attended the relevant session."
)

writeLines(narrative_lines, paste0(OUT_PREFIX, "_Narrative.txt"))

# ----------------------------------------------------------
# 10. PNG figure table
# ----------------------------------------------------------

wrap_col <- function(x, width) {
  stringr::str_wrap(x, width = width)
}

make_table_png <- function(df, out_file) {
  df_plot <- df %>%
    mutate(across(everything(), as.character))
  
  col_widths <- c(
    `Data-collection event` = 0.34,
    `Intervention observed/expected, n/N (%)` = 0.22,
    `Control observed/expected, n/N (%)` = 0.22,
    `Overall observed/expected, n/N (%)` = 0.22
  )
  
  cols <- names(col_widths)
  
  df_wrapped <- df_plot
  df_wrapped[[1]] <- wrap_col(df_wrapped[[1]], 34)
  
  x_starts <- c(0, cumsum(col_widths)[-length(col_widths)])
  x_ends <- cumsum(col_widths)
  
  col_geom <- tibble(
    col = cols,
    x_start = x_starts,
    x_end = x_ends
  )
  
  n_rows <- nrow(df_wrapped)
  header_height <- 1.00
  row_height <- 0.58
  total_height <- header_height + n_rows * row_height
  
  header_cells <- col_geom %>%
    mutate(
      y_top = total_height,
      y_bottom = total_height - header_height,
      label = col
    )
  
  body_cells <- expand_grid(
    row_id = seq_len(n_rows),
    col = cols
  ) %>%
    left_join(col_geom, by = "col") %>%
    mutate(
      y_top = total_height - header_height - (row_id - 1) * row_height,
      y_bottom = y_top - row_height,
      label = map2_chr(row_id, col, ~ as.character(df_wrapped[[.y]][.x])),
      is_total = df_wrapped[[1]][row_id] == "Total observed/expected across events",
      fill = case_when(
        is_total ~ "#F3F4F6",
        row_id %% 2 == 0 ~ "#FFFFFF",
        TRUE ~ "#FAFAFA"
      ),
      fontface = if_else(is_total, "bold", "plain")
    )
  
  p <- ggplot() +
    geom_rect(
      data = header_cells,
      aes(xmin = x_start, xmax = x_end, ymin = y_bottom, ymax = y_top),
      fill = "#F2F4F7",
      colour = "#D0D5DD",
      linewidth = 0.35
    ) +
    geom_rect(
      data = body_cells,
      aes(xmin = x_start, xmax = x_end, ymin = y_bottom, ymax = y_top, fill = fill),
      colour = "#E5E7EB",
      linewidth = 0.30
    ) +
    scale_fill_identity() +
    geom_text(
      data = header_cells,
      aes(x = (x_start + x_end) / 2, y = (y_top + y_bottom) / 2, label = label),
      family = PALATINO_NAME,
      fontface = "bold",
      size = 4.7,
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 0.90,
      colour = "#111827"
    ) +
    geom_text(
      data = body_cells,
      aes(
        x = (x_start + x_end) / 2,
        y = (y_top + y_bottom) / 2,
        label = label,
        fontface = fontface
      ),
      family = PALATINO_NAME,
      size = 4.35,
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 0.90,
      colour = "#1F2937"
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, total_height), clip = "off") +
    labs(title = "WP2 Data-Collection Adherence by Expected Event") +
    theme_void(base_family = PALATINO_NAME) +
    theme(
      plot.title = element_text(
        family = PALATINO_NAME,
        face = "bold",
        size = 21,
        hjust = 0.5,
        colour = "#111827",
        margin = margin(b = 10)
      ),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  fig_height <- max(7.0, 1.2 + n_rows * 0.31)
  
  ggsave(
    filename = out_file,
    plot = p,
    width = 12.8,
    height = fig_height,
    dpi = 300,
    bg = "white"
  )
  
  print(p)
}

make_table_png(
  wide_table,
  paste0(OUT_PREFIX, "_Table.png")
)

# ----------------------------------------------------------
# 11. Excel workbook
# ----------------------------------------------------------

wb <- createWorkbook()

safe_add_sheet <- function(wb, sheet_name, df) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df)
  
  if (ncol(df) > 0) {
    setColWidths(
      wb,
      sheet = sheet_name,
      cols = seq_len(ncol(df)),
      widths = 24
    )
  }
  
  invisible(wb)
}

safe_add_sheet(wb, "Wide table", wide_table)
safe_add_sheet(wb, "Event summary", event_summary_with_total)
safe_add_sheet(wb, "By arm event", by_arm_event_with_total)
safe_add_sheet(wb, "ID event level", id_event_level)
safe_add_sheet(wb, "Attendance denominator", attendance_by_session)
safe_add_sheet(wb, "Arm denominators", arm_denoms)
safe_add_sheet(wb, "Narrative", tibble(text = narrative_lines))

saveWorkbook(
  wb,
  paste0(OUT_PREFIX, "_All.xlsx"),
  overwrite = TRUE
)

# ----------------------------------------------------------
# 12. Console output
# ----------------------------------------------------------

cat("\nWP2 detailed data-collection adherence outputs saved:\n")
cat("====================================================\n")
cat(paste0(OUT_PREFIX, "_Wide.csv\n"))
cat(paste0(OUT_PREFIX, "_EventLevel.csv\n"))
cat(paste0(OUT_PREFIX, "_ByArmEvent.csv\n"))
cat(paste0(OUT_PREFIX, "_IDEventLevel.csv\n"))
cat(paste0(OUT_PREFIX, "_Narrative.txt\n"))
cat(paste0(OUT_PREFIX, "_Table.png\n"))
cat(paste0(OUT_PREFIX, "_All.xlsx\n"))

cat("\nWide adherence table:\n")
print(wide_table, n = Inf, width = Inf)

cat("\nNarrative:\n")
cat(paste(narrative_lines, collapse = "\n"))
cat("\n")

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

# ===================================================

# ============================================================
# WP2 DATA-COLLECTION ADHERENCE BY EXPECTED EVENT
# Per-arm + overall n/N (%) table for SM Excel
# ============================================================
#
# Output:
#   supplementary_wp2_adherence_by_event/
#     WP2_DataCollection_Adherence_By_Event.xlsx
#     WP2_DataCollection_Adherence_By_Event.csv
#
# This recreates the table:
#   WP2 Data-Collection Adherence by Expected Event
#
# Uses final audited event-level denominators:
#   - Intervention expected/observed
#   - Control expected/observed
#   - Overall expected/observed
#
# ============================================================

# install.packages(c("tidyverse", "writexl", "readr"))

library(tidyverse)
library(writexl)
library(readr)

# ============================================================
# 0. SETUP
# ============================================================

DATA_DIR <- if (exists("DATA_DIR")) DATA_DIR else "C:/Users/dn284/Desktop/MRC_omni/data"

OUT_DIR <- file.path(DATA_DIR, "supplementary_wp2_adherence_by_event")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

OUT_XLSX <- file.path(OUT_DIR, "WP2_DataCollection_Adherence_By_Event.xlsx")
OUT_CSV  <- file.path(OUT_DIR, "WP2_DataCollection_Adherence_By_Event.csv")
OUT_LONG <- file.path(OUT_DIR, "WP2_DataCollection_Adherence_By_Event_Long.csv")
OUT_TXT  <- file.path(OUT_DIR, "WP2_DataCollection_Adherence_By_Event_Narrative.txt")

# ============================================================
# 1. FINAL AUDITED COUNTS
# ============================================================

event_counts <- tribble(
  ~event_order, ~event, ~int_obs, ~int_exp, ~ctrl_obs, ~ctrl_exp,
  
  1,  "Pre Session 1",                         42, 42, 39, 39,
  2,  "Post Session 1",                        40, 42, 39, 39,
  3,  "SMS 1 after Session 1",                 35, 42, 33, 39,
  4,  "SMS 2 after Session 1",                 32, 42, 31, 39,
  5,  "SMS 3 after Session 1",                 33, 42, 31, 39,
  
  6,  "Pre Session 2",                         39, 39, 35, 35,
  7,  "Post Session 2",                        39, 39, 35, 35,
  8,  "SMS 1 after Session 2",                 30, 39, 27, 35,
  9,  "SMS 2 after Session 2",                 28, 39, 22, 35,
  10, "SMS 3 after Session 2",                 34, 39, 29, 35,
  
  11, "Pre Session 3",                         38, 39, 32, 32,
  12, "Post Session 3",                        39, 39, 32, 32,
  13, "SMS 1 after Session 3",                 32, 39, 23, 32,
  14, "SMS 2 after Session 3",                 28, 39, 15, 32,
  15, "SMS 3 after Session 3",                 33, 39, 22, 32,
  
  16, "Pre Session 4",                         39, 39, 31, 31,
  17, "Post Session 4",                        39, 39, 31, 31,
  18, "Final SMS / post-treatment follow-up",  37, 39, 30, 31
)

# ============================================================
# 2. FORMAT HELPERS
# ============================================================

fmt_pct <- function(obs, exp, digits = 1) {
  ifelse(
    is.na(exp) | exp == 0,
    NA_character_,
    sprintf(paste0("%.", digits, "f%%"), 100 * obs / exp)
  )
}

fmt_n_pct <- function(obs, exp) {
  paste0(obs, "/", exp, " (", fmt_pct(obs, exp), ")")
}

# Optional exact binomial CI for audit sheet
binom_ci <- function(obs, exp) {
  if (is.na(exp) || exp == 0) {
    return(c(NA_real_, NA_real_))
  }
  ci <- stats::binom.test(obs, exp)$conf.int
  c(100 * ci[1], 100 * ci[2])
}

# ============================================================
# 3. WIDE TABLE FOR SM EXCEL
# ============================================================

wp2_adherence_wide <- event_counts %>%
  mutate(
    overall_obs = int_obs + ctrl_obs,
    overall_exp = int_exp + ctrl_exp,
    
    `Intervention observed/expected, n/N (%)` = fmt_n_pct(int_obs, int_exp),
    `Control observed/expected, n/N (%)` = fmt_n_pct(ctrl_obs, ctrl_exp),
    `Overall observed/expected, n/N (%)` = fmt_n_pct(overall_obs, overall_exp)
  ) %>%
  transmute(
    `Data-collection event` = event,
    `Intervention observed/expected, n/N (%)`,
    `Control observed/expected, n/N (%)`,
    `Overall observed/expected, n/N (%)`
  )

# Add total row
total_row <- event_counts %>%
  summarise(
    event = "Total observed/expected across events",
    int_obs = sum(int_obs),
    int_exp = sum(int_exp),
    ctrl_obs = sum(ctrl_obs),
    ctrl_exp = sum(ctrl_exp),
    overall_obs = int_obs + ctrl_obs,
    overall_exp = int_exp + ctrl_exp,
    .groups = "drop"
  ) %>%
  transmute(
    `Data-collection event` = event,
    `Intervention observed/expected, n/N (%)` = fmt_n_pct(int_obs, int_exp),
    `Control observed/expected, n/N (%)` = fmt_n_pct(ctrl_obs, ctrl_exp),
    `Overall observed/expected, n/N (%)` = fmt_n_pct(overall_obs, overall_exp)
  )

wp2_adherence_wide <- bind_rows(wp2_adherence_wide, total_row)

# ============================================================
# 4. LONG NUMERIC TABLE FOR AUDIT / MERGING
# ============================================================

wp2_adherence_long <- event_counts %>%
  mutate(
    overall_obs = int_obs + ctrl_obs,
    overall_exp = int_exp + ctrl_exp
  ) %>%
  select(
    event_order,
    event,
    int_obs,
    int_exp,
    ctrl_obs,
    ctrl_exp,
    overall_obs,
    overall_exp
  ) %>%
  pivot_longer(
    cols = c(int_obs, int_exp, ctrl_obs, ctrl_exp, overall_obs, overall_exp),
    names_to = c("arm", ".value"),
    names_pattern = "(int|ctrl|overall)_(obs|exp)"
  ) %>%
  mutate(
    arm = recode(
      arm,
      int = "Intervention",
      ctrl = "Control",
      overall = "Overall"
    ),
    percent = 100 * obs / exp,
    display = fmt_n_pct(obs, exp)
  ) %>%
  rowwise() %>%
  mutate(
    ci_low = binom_ci(obs, exp)[1],
    ci_high = binom_ci(obs, exp)[2],
    ci_display = paste0(
      "[",
      sprintf("%.1f", ci_low),
      "%, ",
      sprintf("%.1f", ci_high),
      "%]"
    )
  ) %>%
  ungroup() %>%
  select(
    event_order,
    event,
    arm,
    observed_n = obs,
    expected_n = exp,
    percent,
    ci_low,
    ci_high,
    display,
    ci_display
  )

# Add long total rows
wp2_adherence_long_total <- event_counts %>%
  summarise(
    event_order = 999,
    event = "Total observed/expected across events",
    int_obs = sum(int_obs),
    int_exp = sum(int_exp),
    ctrl_obs = sum(ctrl_obs),
    ctrl_exp = sum(ctrl_exp),
    overall_obs = int_obs + ctrl_obs,
    overall_exp = int_exp + ctrl_exp,
    .groups = "drop"
  ) %>%
  select(
    event_order,
    event,
    int_obs,
    int_exp,
    ctrl_obs,
    ctrl_exp,
    overall_obs,
    overall_exp
  ) %>%
  pivot_longer(
    cols = c(int_obs, int_exp, ctrl_obs, ctrl_exp, overall_obs, overall_exp),
    names_to = c("arm", ".value"),
    names_pattern = "(int|ctrl|overall)_(obs|exp)"
  ) %>%
  mutate(
    arm = recode(
      arm,
      int = "Intervention",
      ctrl = "Control",
      overall = "Overall"
    ),
    percent = 100 * obs / exp,
    display = fmt_n_pct(obs, exp)
  ) %>%
  rowwise() %>%
  mutate(
    ci_low = binom_ci(obs, exp)[1],
    ci_high = binom_ci(obs, exp)[2],
    ci_display = paste0(
      "[",
      sprintf("%.1f", ci_low),
      "%, ",
      sprintf("%.1f", ci_high),
      "%]"
    )
  ) %>%
  ungroup() %>%
  select(
    event_order,
    event,
    arm,
    observed_n = obs,
    expected_n = exp,
    percent,
    ci_low,
    ci_high,
    display,
    ci_display
  )

wp2_adherence_long <- bind_rows(
  wp2_adherence_long,
  wp2_adherence_long_total
)

# ============================================================
# 5. NOTES / NARRATIVE
# ============================================================

notes <- tibble(
  Note = c(
    "WP2 Data-Collection Adherence by Expected Event.",
    "Values are observed / expected forms or responses, n/N (%).",
    "Expected denominators vary by event because later-session and follow-up events were expected only for participants retained/eligible for that stage.",
    "The total row sums observed and expected events across all listed data-collection events.",
    "This table is designed for the SM Excel workbook and avoids Unicode symbols to prevent encoding artifacts."
  )
)

narrative <- c(
  "WP2 data-collection adherence was summarised by expected assessment event and trial arm.",
  "",
  "Across all expected events, the Intervention arm contributed 637/717 observed responses (88.8%), while the Control arm contributed 537/623 observed responses (86.2%). Overall, 1174/1340 expected data-collection events were observed (87.6%).",
  "",
  "Expected denominators differed by event because later-session and follow-up assessments were expected only for participants retained or eligible for that stage."
)

writeLines(narrative, OUT_TXT)

# ============================================================
# 6. EXPORT
# ============================================================

readr::write_csv(wp2_adherence_wide, OUT_CSV)
readr::write_csv(wp2_adherence_long, OUT_LONG)

excel_sheets <- list(
  "WP2 adherence by event" = as.data.frame(wp2_adherence_wide),
  "Long numeric summary" = as.data.frame(wp2_adherence_long),
  "Notes" = as.data.frame(notes)
)

writexl::write_xlsx(excel_sheets, OUT_XLSX)

# ============================================================
# 7. CONSOLE OUTPUT
# ============================================================

cat("\n============================================================\n")
cat("WP2 DATA-COLLECTION ADHERENCE EXPORT COMPLETE\n")
cat("============================================================\n\n")

cat("Outputs:\n")
cat("  ", OUT_XLSX, "\n")
cat("  ", OUT_CSV, "\n")
cat("  ", OUT_LONG, "\n")
cat("  ", OUT_TXT, "\n\n")

cat("Preview:\n")
print(wp2_adherence_wide, n = Inf, width = Inf)

cat("\nLong total rows:\n")
print(
  wp2_adherence_long %>%
    filter(event == "Total observed/expected across events"),
  n = Inf,
  width = Inf
)

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





# ------------------------------------------------------------
# SECTION: exploratory wp2 clinical and affective outcomes
# ------------------------------------------------------------




# ------------------------------------------------------------
# SECTION: wp2 asc predictors
# ------------------------------------------------------------

needed for manuscript_plots figure 6

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

# ===================================================

# ==========================================================
# ADD FIGURE 6 ASC PREDICTOR SOURCE TABLE TO SM WORKBOOK
# ==========================================================
# Run this AFTER the final Figure 6 ASC-only / arm-specific code.
#
# Creates/replaces workbook sheet:
#   "Figure 6 ASC Predictors"
#
# Includes:
#   - arm
#   - ASC tertile
#   - n
#   - raw baseline BDI-II mean
#   - raw post-treatment BDI-II mean
#   - mean BDI-II improvement
#   - 95% CI for improvement
#   - plotted baseline-aligned baseline
#   - plotted post-treatment value
#   - continuous model beta, SE, CI, p-value
#   - model covariates
#
# Outputs:
#   - Updated workbook copy:
#       MRC_SM_v1.3_with_Figure6_ASC_Predictors.xlsx
#   - CSV audit:
#       Figure6_ASC_Predictors_Workbook_Tab.csv
# ==========================================================

# install.packages(c("tidyverse", "openxlsx", "readr", "stringr"))

library(tidyverse)
library(openxlsx)
library(readr)
library(stringr)

# ==========================================================
# 1. PATHS
# ==========================================================

DATA_DIR <- Sys.getenv("MRC_DATA_DIR", unset = "C:/Users/dn284/Desktop/MRC_omni/data")
SEARCH_DIRS <- c(DATA_DIR)

OUT_SHEET_NAME <- "Figure 6 ASC Predictors"

OUT_CSV <- file.path(DATA_DIR, "Figure6_ASC_Predictors_Workbook_Tab.csv")
OUT_XLSX <- file.path(DATA_DIR, "MRC_SM_v1.3_with_Figure6_ASC_Predictors.xlsx")

# ==========================================================
# 2. HELPERS
# ==========================================================

normalise_filename <- function(x) {
  basename(x) %>%
    str_to_lower() %>%
    str_replace_all("[+_\\-]+", " ") %>%
    str_squish()
}

newest_match_strict <- function(
    include_regex,
    exclude_regex = NULL,
    search_dirs = SEARCH_DIRS,
    required = TRUE,
    label = "file"
) {
  hits <- character(0)
  
  for (d in search_dirs) {
    if (!dir.exists(d)) next
    
    all_files <- list.files(d, recursive = TRUE, full.names = TRUE)
    file_names_norm <- normalise_filename(all_files)
    
    matched <- all_files[
      str_detect(file_names_norm, regex(include_regex, ignore_case = TRUE))
    ]
    
    if (!is.null(exclude_regex)) {
      matched_norm <- normalise_filename(matched)
      matched <- matched[
        !str_detect(matched_norm, regex(exclude_regex, ignore_case = TRUE))
      ]
    }
    
    hits <- c(hits, matched)
  }
  
  hits <- unique(hits[file.exists(hits)])
  
  if (length(hits) == 0) {
    msg <- paste0(
      "No ", label, " found using include_regex = ",
      include_regex,
      if (!is.null(exclude_regex)) paste0(" and exclude_regex = ", exclude_regex) else ""
    )
    if (required) stop(msg)
    return(NULL)
  }
  
  hits[order(file.info(hits)$mtime, decreasing = TRUE)][1]
}

clean_colnames <- function(df) {
  names(df) <- names(df) %>%
    str_replace_all("%", "pct") %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "") %>%
    str_to_lower()
  
  df
}

fmt_p <- function(p) {
  case_when(
    is.na(p) ~ NA_character_,
    p < .001 ~ "< .001",
    TRUE ~ sprintf("%.3f", p)
  )
}

safe_round <- function(x, digits = 2) {
  ifelse(is.na(x), NA_real_, round(as.numeric(x), digits))
}

coalesce_col <- function(df, candidates, default = NA) {
  candidates <- candidates[candidates %in% names(df)]
  if (length(candidates) == 0) return(rep(default, nrow(df)))
  out <- df[[candidates[1]]]
  if (length(candidates) > 1) {
    for (cc in candidates[-1]) out <- dplyr::coalesce(out, df[[cc]])
  }
  out
}

read_csv_if_exists <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return(NULL)
  readr::read_csv(path, show_col_types = FALSE) %>% clean_colnames()
}

# ==========================================================
# 3. LOCATE SM WORKBOOK
# ==========================================================

SM_XLSX_PATH <- newest_match_strict(
  include_regex = "^mrc sm.*\\.xlsx$",
  exclude_regex = "temporary|backup|old|archive|copy|with figure6",
  label = "MRC supplementary workbook"
)

message("Using SM workbook: ", basename(SM_XLSX_PATH))

# ==========================================================
# 4. GET FIGURE 6 OBJECTS OR CSV FALLBACKS
# ==========================================================

# Expected objects from your Figure 6 code:
#   pt
#   summary_export
#   improvement_export
#   model_export

if (exists("pt")) {
  pt_for_fig6 <- pt %>% as_tibble()
} else {
  pt_for_fig6 <- NULL
}

if (exists("summary_export")) {
  summary_tbl <- summary_export %>% as_tibble() %>% clean_colnames()
} else {
  trajectory_csv <- newest_match_strict(
    include_regex = "figure6.*(trajectory|trajectories).*\\.csv$",
    exclude_regex = "old|archive|backup",
    required = FALSE,
    label = "Figure 6 trajectory CSV"
  )
  summary_tbl <- read_csv_if_exists(trajectory_csv)
}

if (exists("improvement_export")) {
  improvement_tbl <- improvement_export %>% as_tibble() %>% clean_colnames()
} else if (exists("improvement_summary")) {
  improvement_tbl <- improvement_summary %>% as_tibble() %>% clean_colnames()
} else {
  improvement_csv <- newest_match_strict(
    include_regex = "figure6.*improvement.*\\.csv$",
    exclude_regex = "old|archive|backup|delta|high.*minus.*low",
    required = FALSE,
    label = "Figure 6 improvement CSV"
  )
  improvement_tbl <- read_csv_if_exists(improvement_csv)
}

if (exists("model_export")) {
  model_tbl <- model_export %>% as_tibble() %>% clean_colnames()
} else {
  model_csv <- newest_match_strict(
    include_regex = "figure6.*(model|models).*\\.csv$",
    exclude_regex = "old|archive|backup|ancova|tertile",
    required = FALSE,
    label = "Figure 6 continuous model CSV"
  )
  model_tbl <- read_csv_if_exists(model_csv)
}

if (is.null(improvement_tbl)) {
  stop(
    "Could not find Figure 6 improvement table. ",
    "Please run the final Figure 6 code first, or make sure an improvement CSV exists in DATA_DIR."
  )
}

if (is.null(model_tbl)) {
  stop(
    "Could not find Figure 6 model table. ",
    "Please run the final Figure 6 code first, or make sure a continuous model CSV exists in DATA_DIR."
  )
}

# ==========================================================
# 5. STANDARDISE IMPROVEMENT TABLE
# ==========================================================

improvement_tbl <- improvement_tbl %>%
  clean_colnames() %>%
  mutate(
    arm = as.character(coalesce_col(., c("arm", "condition"))),
    predictor = as.character(coalesce_col(., c("predictor"))),
    asc_tertile = as.character(coalesce_col(., c("asc_tertile", "tertile", "predictor_tertile"))),
    n = suppressWarnings(as.numeric(coalesce_col(., c("n", "n_participants")))),
    mean_improvement = suppressWarnings(as.numeric(coalesce_col(
      .,
      c("mean_improvement", "mean_bdi_ii_improvement", "mean", "improvement_mean")
    ))),
    improvement_ci_low = suppressWarnings(as.numeric(coalesce_col(
      .,
      c("ci_low", "x95pct_ci_lower", "95_ci_lower", "ci_lower", "improvement_ci_low")
    ))),
    improvement_ci_high = suppressWarnings(as.numeric(coalesce_col(
      .,
      c("ci_high", "x95pct_ci_upper", "95_ci_upper", "ci_upper", "improvement_ci_high")
    )))
  ) %>%
  filter(
    !is.na(arm),
    !is.na(asc_tertile),
    str_to_lower(predictor) %in% c("total asc", "asc", "total_asc", "11-asc", "11 asc") |
      is.na(predictor)
  ) %>%
  mutate(
    arm = case_when(
      str_detect(str_to_lower(arm), "intervention") ~ "Intervention",
      str_detect(str_to_lower(arm), "control") ~ "Control",
      TRUE ~ arm
    ),
    asc_tertile = case_when(
      str_detect(str_to_lower(asc_tertile), "low") ~ "Low",
      str_detect(str_to_lower(asc_tertile), "med|middle") ~ "Medium",
      str_detect(str_to_lower(asc_tertile), "high") ~ "High",
      TRUE ~ asc_tertile
    )
  ) %>%
  select(
    arm,
    asc_tertile,
    n,
    mean_improvement,
    improvement_ci_low,
    improvement_ci_high
  ) %>%
  distinct()

# ==========================================================
# 6. RAW BASELINE/POST BDI MEANS FROM pt, IF AVAILABLE
# ==========================================================

if (!is.null(pt_for_fig6)) {
  
  pt_clean <- pt_for_fig6 %>%
    clean_colnames() %>%
    mutate(
      arm = as.character(coalesce_col(., c("condition", "arm"))),
      asc_tertile = as.character(coalesce_col(., c("asc_tertile", "tertile", "asc_total_tertile"))),
      bdi_pre = suppressWarnings(as.numeric(coalesce_col(., c("bdi_pre", "baseline_bdi", "bdi_baseline")))),
      bdi_post = suppressWarnings(as.numeric(coalesce_col(., c("bdi_post", "post_bdi", "endpoint_bdi"))))
    ) %>%
    mutate(
      arm = case_when(
        str_detect(str_to_lower(arm), "intervention") ~ "Intervention",
        str_detect(str_to_lower(arm), "control") ~ "Control",
        TRUE ~ arm
      ),
      asc_tertile = case_when(
        str_detect(str_to_lower(asc_tertile), "low") ~ "Low",
        str_detect(str_to_lower(asc_tertile), "med|middle") ~ "Medium",
        str_detect(str_to_lower(asc_tertile), "high") ~ "High",
        TRUE ~ asc_tertile
      )
    ) %>%
    filter(!is.na(arm), !is.na(asc_tertile))
  
  raw_bdi_tbl <- pt_clean %>%
    group_by(arm, asc_tertile) %>%
    summarise(
      raw_baseline_bdi_ii_mean = mean(bdi_pre, na.rm = TRUE),
      raw_post_treatment_bdi_ii_mean = mean(bdi_post, na.rm = TRUE),
      raw_n = sum(!is.na(bdi_pre) & !is.na(bdi_post)),
      .groups = "drop"
    ) %>%
    mutate(
      raw_baseline_bdi_ii_mean = ifelse(is.nan(raw_baseline_bdi_ii_mean), NA_real_, raw_baseline_bdi_ii_mean),
      raw_post_treatment_bdi_ii_mean = ifelse(is.nan(raw_post_treatment_bdi_ii_mean), NA_real_, raw_post_treatment_bdi_ii_mean)
    )
  
} else {
  raw_bdi_tbl <- tibble(
    arm = character(),
    asc_tertile = character(),
    raw_baseline_bdi_ii_mean = numeric(),
    raw_post_treatment_bdi_ii_mean = numeric(),
    raw_n = numeric()
  )
}

# ==========================================================
# 7. PLOTTED BASELINE-ALIGNED VALUES
# ==========================================================

if (!is.null(summary_tbl)) {
  
  summary_tbl <- summary_tbl %>% clean_colnames()
  
  trajectory_tbl <- summary_tbl %>%
    mutate(
      arm = as.character(coalesce_col(., c("arm", "condition"))),
      predictor = as.character(coalesce_col(., c("predictor"))),
      asc_tertile = as.character(coalesce_col(., c("asc_tertile", "tertile", "predictor_tertile"))),
      timepoint = as.character(coalesce_col(., c("timepoint", "study_timepoint"))),
      plotted_mean = suppressWarnings(as.numeric(coalesce_col(., c("mean", "plotted_mean", "bdi_mean"))))
    ) %>%
    filter(
      !is.na(arm),
      !is.na(asc_tertile),
      str_to_lower(predictor) %in% c("total asc", "asc", "total_asc", "11-asc", "11 asc") |
        is.na(predictor)
    ) %>%
    mutate(
      arm = case_when(
        str_detect(str_to_lower(arm), "intervention") ~ "Intervention",
        str_detect(str_to_lower(arm), "control") ~ "Control",
        TRUE ~ arm
      ),
      asc_tertile = case_when(
        str_detect(str_to_lower(asc_tertile), "low") ~ "Low",
        str_detect(str_to_lower(asc_tertile), "med|middle") ~ "Medium",
        str_detect(str_to_lower(asc_tertile), "high") ~ "High",
        TRUE ~ asc_tertile
      ),
      timepoint_clean = case_when(
        str_detect(str_to_lower(timepoint), "base") ~ "Baseline",
        str_detect(str_to_lower(timepoint), "post|endpoint|treatment") ~ "Post-treatment",
        TRUE ~ timepoint
      )
    ) %>%
    select(arm, asc_tertile, timepoint_clean, plotted_mean) %>%
    distinct() %>%
    pivot_wider(
      names_from = timepoint_clean,
      values_from = plotted_mean
    ) %>%
    rename(
      plotted_baseline_aligned_baseline = Baseline,
      plotted_post_treatment_value = `Post-treatment`
    )
  
} else {
  trajectory_tbl <- tibble(
    arm = character(),
    asc_tertile = character(),
    plotted_baseline_aligned_baseline = numeric(),
    plotted_post_treatment_value = numeric()
  )
}

# If trajectory table unavailable or incomplete, reconstruct plotted values:
# plotted baseline = arm-specific raw baseline mean
# plotted post = plotted baseline - observed mean improvement
if (nrow(trajectory_tbl) == 0 ||
    !all(c("plotted_baseline_aligned_baseline", "plotted_post_treatment_value") %in% names(trajectory_tbl))) {
  
  trajectory_tbl <- raw_bdi_tbl %>%
    group_by(arm) %>%
    mutate(
      plotted_baseline_aligned_baseline = mean(raw_baseline_bdi_ii_mean, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(arm, asc_tertile, plotted_baseline_aligned_baseline) %>%
    left_join(improvement_tbl, by = c("arm", "asc_tertile")) %>%
    mutate(
      plotted_post_treatment_value = plotted_baseline_aligned_baseline - mean_improvement
    ) %>%
    select(arm, asc_tertile, plotted_baseline_aligned_baseline, plotted_post_treatment_value)
}

# ==========================================================
# 8. CONTINUOUS WITHIN-ARM ASC MODEL TABLE
# ==========================================================

model_tbl <- model_tbl %>%
  clean_colnames() %>%
  mutate(
    model = as.character(coalesce_col(., c("model"))),
    term = as.character(coalesce_col(., c("term"))),
    model_n = suppressWarnings(as.numeric(coalesce_col(., c("n", "model_n")))),
    beta = suppressWarnings(as.numeric(coalesce_col(., c("estimate", "beta")))),
    se = suppressWarnings(as.numeric(coalesce_col(., c("std_error", "se", "std_error_estimate")))),
    ci_low = suppressWarnings(as.numeric(coalesce_col(., c("ci_low", "conf_low", "x95pct_ci_lower", "95_ci_lower")))),
    ci_high = suppressWarnings(as.numeric(coalesce_col(., c("ci_high", "conf_high", "x95pct_ci_upper", "95_ci_upper")))),
    p_value = suppressWarnings(as.numeric(coalesce_col(., c("p_value", "p", "pvalue")))),
    model_formula = as.character(coalesce_col(., c("model_formula", "formula"), default = NA_character_)),
    arm = case_when(
      str_detect(str_to_lower(model), "intervention") ~ "Intervention",
      str_detect(str_to_lower(model), "control") ~ "Control",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(arm),
    term == "asc_total_z"
  ) %>%
  mutate(
    continuous_model_covariates = case_when(
      !is.na(model_formula) & nzchar(model_formula) ~ model_formula,
      TRUE ~ "bdi_post ~ bdi_pre + asc_total_z + expect_z"
    ),
    continuous_model_interpretation = "Beta is the adjusted difference in post-treatment BDI-II per 1-SD higher total ASC; negative values indicate lower post-treatment BDI-II, conditional on covariates."
  ) %>%
  select(
    arm,
    continuous_model_n = model_n,
    continuous_model_beta = beta,
    continuous_model_se = se,
    continuous_model_ci_low = ci_low,
    continuous_model_ci_high = ci_high,
    continuous_model_p_value = p_value,
    continuous_model_p_formatted = p_value,
    continuous_model_covariates,
    continuous_model_interpretation
  ) %>%
  mutate(
    continuous_model_p_formatted = fmt_p(continuous_model_p_formatted)
  )

# If SE is unavailable in model_export, estimate it approximately from 95% CI width.
# This is only for reporting in the workbook; model beta/CI/p remain the actual model outputs.
model_tbl <- model_tbl %>%
  mutate(
    continuous_model_se = case_when(
      !is.na(continuous_model_se) ~ continuous_model_se,
      !is.na(continuous_model_ci_low) & !is.na(continuous_model_ci_high) ~
        (continuous_model_ci_high - continuous_model_ci_low) / (2 * 1.96),
      TRUE ~ NA_real_
    )
  )

# ==========================================================
# 9. COMBINE INTO ONE FIGURE 6 ASC PREDICTOR TABLE
# ==========================================================

arm_order <- c("Intervention", "Control")
tertile_order <- c("Low", "Medium", "High")

fig6_asc_predictors <- improvement_tbl %>%
  left_join(raw_bdi_tbl, by = c("arm", "asc_tertile")) %>%
  left_join(trajectory_tbl, by = c("arm", "asc_tertile")) %>%
  left_join(model_tbl, by = "arm") %>%
  mutate(
    arm = factor(arm, levels = arm_order),
    asc_tertile = factor(asc_tertile, levels = tertile_order),
    
    raw_baseline_bdi_ii_mean = safe_round(raw_baseline_bdi_ii_mean, 2),
    raw_post_treatment_bdi_ii_mean = safe_round(raw_post_treatment_bdi_ii_mean, 2),
    mean_bdi_ii_improvement = safe_round(mean_improvement, 2),
    improvement_95_ci_low = safe_round(improvement_ci_low, 2),
    improvement_95_ci_high = safe_round(improvement_ci_high, 2),
    plotted_baseline_aligned_baseline = safe_round(plotted_baseline_aligned_baseline, 2),
    plotted_post_treatment_value = safe_round(plotted_post_treatment_value, 2),
    
    continuous_model_beta = safe_round(continuous_model_beta, 2),
    continuous_model_se = safe_round(continuous_model_se, 2),
    continuous_model_ci_low = safe_round(continuous_model_ci_low, 2),
    continuous_model_ci_high = safe_round(continuous_model_ci_high, 2)
  ) %>%
  arrange(arm, asc_tertile) %>%
  transmute(
    arm = as.character(arm),
    asc_tertile = as.character(asc_tertile),
    n,
    raw_baseline_bdi_ii_mean,
    raw_post_treatment_bdi_ii_mean,
    mean_bdi_ii_improvement,
    improvement_95_ci_low,
    improvement_95_ci_high,
    plotted_baseline_aligned_baseline,
    plotted_post_treatment_value,
    continuous_model_n,
    continuous_model_beta,
    continuous_model_se,
    continuous_model_ci_low,
    continuous_model_ci_high,
    continuous_model_p_value,
    continuous_model_p_formatted,
    continuous_model_covariates,
    continuous_model_interpretation
  )

write_csv(fig6_asc_predictors, OUT_CSV)

cat("\nFigure 6 ASC predictor table to be written to workbook:\n")
print(fig6_asc_predictors, n = Inf)

# ==========================================================
# 10. OPTIONAL ASSERTIONS AGAINST MANUSCRIPT-REPORTED VALUES
# ==========================================================

# These are not hard-coded into the table, but this prints quick checks.
cat("\nQuick manuscript-value checks, if using final Figure 6 code:\n")
cat("Intervention low / med / high mean improvements should be about 6.73, 15.80, 19.67.\n")
cat("Control low / med / high mean improvements should be about 5.67, 10.57, 10.00.\n")
cat("Continuous ASC beta should be about -2.29 Intervention and -1.74 Control.\n\n")

print(
  fig6_asc_predictors %>%
    select(
      arm,
      asc_tertile,
      n,
      mean_bdi_ii_improvement,
      improvement_95_ci_low,
      improvement_95_ci_high,
      continuous_model_beta,
      continuous_model_ci_low,
      continuous_model_ci_high,
      continuous_model_p_formatted
    ),
  n = Inf
)

# ==========================================================
# 11. ADD / REPLACE WORKBOOK SHEET
# ==========================================================

wb <- loadWorkbook(SM_XLSX_PATH)

if (OUT_SHEET_NAME %in% names(wb)) {
  removeWorksheet(wb, OUT_SHEET_NAME)
}

addWorksheet(wb, OUT_SHEET_NAME)

# Styles
title_style <- createStyle(
  fontSize = 14,
  textDecoration = "bold",
  fgFill = "#D9EAF7",
  halign = "left",
  valign = "center",
  border = "Bottom"
)

subtitle_style <- createStyle(
  fontSize = 11,
  textDecoration = "italic",
  fontColour = "#4B5563",
  wrapText = TRUE,
  valign = "top"
)

header_style <- createStyle(
  fontSize = 11,
  textDecoration = "bold",
  fgFill = "#E5E7EB",
  border = "TopBottom",
  halign = "center",
  valign = "center",
  wrapText = TRUE
)

body_style <- createStyle(
  fontSize = 10,
  border = "Bottom",
  borderColour = "#E5E7EB",
  valign = "top",
  wrapText = TRUE
)

num_style <- createStyle(
  numFmt = "0.00",
  fontSize = 10,
  border = "Bottom",
  borderColour = "#E5E7EB",
  valign = "top"
)

p_style <- createStyle(
  numFmt = "0.000",
  fontSize = 10,
  border = "Bottom",
  borderColour = "#E5E7EB",
  valign = "top"
)

# Sheet metadata
writeData(
  wb,
  OUT_SHEET_NAME,
  "Figure 6 ASC Predictors",
  startRow = 1,
  startCol = 1
)
addStyle(wb, OUT_SHEET_NAME, title_style, rows = 1, cols = 1:19, gridExpand = TRUE)

writeData(
  wb,
  OUT_SHEET_NAME,
  paste0(
    "Source table for manuscript Figure 6. Rows summarise arm-specific total ASC tertiles ",
    "and baseline-aligned BDI-II trajectory values. Continuous model columns summarise ",
    "within-arm linear models predicting post-treatment BDI-II from baseline BDI-II, ",
    "z-scored total ASC, and expectancy. Negative continuous-model beta values indicate ",
    "lower post-treatment BDI-II per 1-SD higher total ASC, conditional on covariates."
  ),
  startRow = 2,
  startCol = 1
)
mergeCells(wb, OUT_SHEET_NAME, cols = 1:19, rows = 2)
addStyle(wb, OUT_SHEET_NAME, subtitle_style, rows = 2, cols = 1:19, gridExpand = TRUE)

writeData(
  wb,
  OUT_SHEET_NAME,
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  startRow = 3,
  startCol = 1
)
mergeCells(wb, OUT_SHEET_NAME, cols = 1:19, rows = 3)
addStyle(wb, OUT_SHEET_NAME, subtitle_style, rows = 3, cols = 1:19, gridExpand = TRUE)

# Data table
start_row <- 5
writeDataTable(
  wb,
  OUT_SHEET_NAME,
  fig6_asc_predictors,
  startRow = start_row,
  startCol = 1,
  tableStyle = "TableStyleMedium2",
  withFilter = TRUE
)

# Style header and body
n_rows <- nrow(fig6_asc_predictors)
n_cols <- ncol(fig6_asc_predictors)

addStyle(
  wb,
  OUT_SHEET_NAME,
  header_style,
  rows = start_row,
  cols = 1:n_cols,
  gridExpand = TRUE,
  stack = TRUE
)

if (n_rows > 0) {
  addStyle(
    wb,
    OUT_SHEET_NAME,
    body_style,
    rows = (start_row + 1):(start_row + n_rows),
    cols = 1:n_cols,
    gridExpand = TRUE,
    stack = TRUE
  )
  
  numeric_cols <- which(names(fig6_asc_predictors) %in% c(
    "raw_baseline_bdi_ii_mean",
    "raw_post_treatment_bdi_ii_mean",
    "mean_bdi_ii_improvement",
    "improvement_95_ci_low",
    "improvement_95_ci_high",
    "plotted_baseline_aligned_baseline",
    "plotted_post_treatment_value",
    "continuous_model_beta",
    "continuous_model_se",
    "continuous_model_ci_low",
    "continuous_model_ci_high"
  ))
  
  p_cols <- which(names(fig6_asc_predictors) %in% c(
    "continuous_model_p_value"
  ))
  
  addStyle(
    wb,
    OUT_SHEET_NAME,
    num_style,
    rows = (start_row + 1):(start_row + n_rows),
    cols = numeric_cols,
    gridExpand = TRUE,
    stack = TRUE
  )
  
  addStyle(
    wb,
    OUT_SHEET_NAME,
    p_style,
    rows = (start_row + 1):(start_row + n_rows),
    cols = p_cols,
    gridExpand = TRUE,
    stack = TRUE
  )
}

# Freeze panes and widths
freezePane(wb, OUT_SHEET_NAME, firstActiveRow = start_row + 1, firstActiveCol = 3)

setColWidths(wb, OUT_SHEET_NAME, cols = 1:n_cols, widths = "auto")
setColWidths(wb, OUT_SHEET_NAME, cols = which(names(fig6_asc_predictors) == "continuous_model_covariates"), widths = 35)
setColWidths(wb, OUT_SHEET_NAME, cols = which(names(fig6_asc_predictors) == "continuous_model_interpretation"), widths = 55)

setRowHeights(wb, OUT_SHEET_NAME, rows = 1, heights = 24)
setRowHeights(wb, OUT_SHEET_NAME, rows = 2:3, heights = 42)
setRowHeights(wb, OUT_SHEET_NAME, rows = start_row, heights = 36)

# Save as a new workbook copy to avoid damaging the original
saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)

cat("\n============================================================\n")
cat("FIGURE 6 ASC PREDICTOR WORKBOOK TAB CREATED\n")
cat("============================================================\n")
cat("Input workbook:  ", SM_XLSX_PATH, "\n", sep = "")
cat("Output workbook: ", OUT_XLSX, "\n", sep = "")
cat("Sheet added:     ", OUT_SHEET_NAME, "\n", sep = "")
cat("CSV audit:       ", OUT_CSV, "\n", sep = "")
cat("Rows written:    ", nrow(fig6_asc_predictors), "\n", sep = "")
cat("Columns written: ", ncol(fig6_asc_predictors), "\n", sep = "")
cat("============================================================\n")

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


wp2 discomfort summary by arm and session, wp2 fisber se summaries by arm/session

# ============================================================
# SUPPLEMENTARY S9 — WP2 SAFETY / TOLERABILITY TABLE PACK
# FINAL VERSION WITH FISBER + FISBER_TEXT SUPPORT
# ============================================================
#
# Handles:
#   - FISBER typo columns:
#       fisber_1 = frequency
#       fisber_2 = severity/intensity
#       fisber_3 = interference/burden
#       fisber_text = open-text side-effect description
#
#   - VDQ/discomfort from post-session tol_score / VDQ / tol_follow*
#   - AE/SAE-like fields if present
#   - tolerability-related discontinuation notes
#   - open-text side-effect summaries, word counts, and optional word cloud
#
# Outputs:
#   S9a adverse event / SAE summary
#   S9b adverse event / SAE listing
#   S9c FISBER summaries by arm/session
#   S9d VDQ/discomfort means, SD, SE, 95% CI by arm/session
#   S9e VDQ side-effect counts by participant/session
#   S9f side-effect symptom counts by arm/session
#   S9g tolerability-related discontinuation details
#   S9h column-detection audit
#   S9i FISBER open-text classification summary
#   S9j FISBER open-text keyword/topic summary
#   S9k FISBER open-text word-frequency table
#   S9 narrative text
# ============================================================

# install.packages(c(
#   "tidyverse", "gt", "tidytext", "ggwordcloud", "webshot2"
# ))

library(tidyverse)
library(gt)

if (!requireNamespace("tidytext", quietly = TRUE)) {
  message("Package 'tidytext' not installed; word-frequency table will use fallback tokenisation.")
}

HAS_GGWORDCLOUD <- requireNamespace("ggwordcloud", quietly = TRUE)

# ============================================================
# 0. SETUP
# ============================================================

DATA_DIR <- if (exists("DATA_DIR")) DATA_DIR else "C:/Users/dn284/Desktop/MRC_omni/data"
SEARCH_DIRS <- if (exists("SEARCH_DIRS")) SEARCH_DIRS else c(DATA_DIR)
PALATINO_NAME <- if (exists("PALATINO_NAME")) PALATINO_NAME else "serif"

OUT_DIR <- file.path(DATA_DIR, "supplementary_S9_wp2_safety_tolerability")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

Z95 <- 1.96

# ============================================================
# 1. HELPERS
# ============================================================

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

all_matches <- function(patterns, search_dirs = SEARCH_DIRS, required = FALSE) {
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
  
  if (length(hits) == 0 && required) {
    stop("No files found for patterns: ", paste(patterns, collapse = ", "))
  }
  
  hits
}

read_qualtrics_real <- function(path) {
  df <- read_csv(
    path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
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
    na_if('{"ImportId":"part_id"}') %>%
    str_extract("\\d{3,6}")
}

to_num <- function(x) suppressWarnings(as.numeric(x))

standardise_condition <- function(x) {
  x_chr <- str_to_lower(str_squish(as.character(x)))
  
  case_when(
    str_detect(x_chr, "control|placebo|sham") ~ "Control",
    str_detect(x_chr, "intervention|active|treatment|sls") ~ "Intervention",
    x_chr %in% c("0", "c", "ctrl") ~ "Control",
    x_chr %in% c("1", "i", "int", "active") ~ "Intervention",
    TRUE ~ NA_character_
  )
}

find_col <- function(df, candidates, required = FALSE) {
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
  
  if (required) {
    stop("Could not find candidate column: ", paste(candidates, collapse = ", "))
  }
  
  NULL
}

find_cols_regex <- function(df, patterns) {
  nms <- names(df)
  hits <- character(0)
  
  for (pat in patterns) {
    hits <- c(hits, nms[str_detect(nms, regex(pat, ignore_case = TRUE))])
  }
  
  unique(hits)
}

yesish <- function(x) {
  x_chr <- str_to_lower(str_squish(as.character(x)))
  
  case_when(
    is.na(x_chr) | x_chr == "" ~ FALSE,
    x_chr %in% c("0", "no", "none", "false", "n", "na", "nan", "not applicable") ~ FALSE,
    str_detect(x_chr, "^0(\\.0+)?$") ~ FALSE,
    TRUE ~ TRUE
  )
}

num_or_yes <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  case_when(
    !is.na(x_num) ~ x_num > 0,
    TRUE ~ yesish(x)
  )
}

extract_trailing_score <- function(x) {
  # Handles strings like:
  #   "None of the time (no side effects)\n0"
  #   "10% of the time1"
  #   "Minimal severity1"
  #   "No interference with activities0"
  x_chr <- as.character(x)
  x_chr <- str_squish(x_chr)
  out <- str_extract(x_chr, "\\d+(?=\\s*$)")
  suppressWarnings(as.numeric(out))
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "NA", paste0(formatC(x, format = "f", digits = digits), "%"))
}

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "NA", formatC(as.numeric(x), format = "f", digits = digits))
}

fmt_ci <- function(lo, hi, digits = 2) {
  paste0("[", fmt_num(lo, digits), ", ", fmt_num(hi, digits), "]")
}

safe_min <- function(x) ifelse(all(is.na(x)), NA_real_, min(x, na.rm = TRUE))
safe_max <- function(x) ifelse(all(is.na(x)), NA_real_, max(x, na.rm = TRUE))

save_gt <- function(gt_obj, stem, png = TRUE, pdf = TRUE) {
  html_path <- file.path(OUT_DIR, paste0(stem, ".html"))
  gtsave(gt_obj, html_path)
  
  if (png) {
    try(
      gtsave(
        gt_obj,
        file.path(OUT_DIR, paste0(stem, ".png")),
        vwidth = 2200,
        vheight = 1400,
        zoom = 2
      ),
      silent = TRUE
    )
  }
  
  if (pdf) {
    try(
      gtsave(
        gt_obj,
        file.path(OUT_DIR, paste0(stem, ".pdf")),
        vwidth = 2200,
        vheight = 1400,
        zoom = 2
      ),
      silent = TRUE
    )
  }
  
  invisible(html_path)
}

style_s9_gt <- function(tbl, title, subtitle = NULL) {
  tbl %>%
    tab_header(
      title = md(paste0("**", title, "**")),
      subtitle = subtitle
    ) %>%
    tab_options(
      table.font.names = PALATINO_NAME,
      table.font.size = px(12),
      heading.title.font.size = px(16),
      heading.subtitle.font.size = px(11),
      column_labels.font.weight = "bold",
      column_labels.background.color = "#F3F6FA",
      table.border.top.color = "#222222",
      table.border.bottom.color = "#222222",
      data_row.padding = px(4),
      source_notes.font.size = px(10)
    ) %>%
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels(columns = everything())
    )
}

empty_safety_long <- function() {
  tibble(
    part_id = character(),
    session_n = integer(),
    source_file = character(),
    event_field = character(),
    event_value = character(),
    event_value_chr = character(),
    event_reported = logical(),
    is_sae_field = logical(),
    is_discontinuation_field = logical()
  )
}

empty_ae_listing <- function() {
  tibble(
    part_id = character(),
    condition = character(),
    session_n = integer(),
    event_class = character(),
    event_field = character(),
    event_value_chr = character(),
    source_file = character()
  )
}

clean_free_text <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\\r|\\n|\\t", " ") %>%
    str_squish()
}

is_no_side_effect_text <- function(x) {
  x0 <- clean_free_text(x)
  x1 <- str_to_lower(x0)
  
  case_when(
    is.na(x1) | x1 == "" ~ TRUE,
    x1 %in% c(
      "na", "n/a", "n.a", "n.a.", "n|a", "none", "no", "n",
      "-", ".", "nil", "not applicable", "not applicable!!!"
    ) ~ TRUE,
    str_detect(x1, "^n/?a[!\\. ]*$") ~ TRUE,
    str_detect(x1, "^no side effects?$") ~ TRUE,
    str_detect(x1, "^no side effect$") ~ TRUE,
    str_detect(x1, "^none experienced$") ~ TRUE,
    str_detect(x1, "don't think i.*side effect") ~ TRUE,
    str_detect(x1, "dont think i.*side effect") ~ TRUE,
    str_detect(x1, "did not feel any side effect") ~ TRUE,
    str_detect(x1, "did not experience.*side effect") ~ TRUE,
    str_detect(x1, "no side effects? i could") ~ TRUE,
    str_detect(x1, "no side effects? that i could") ~ TRUE,
    str_detect(x1, "no side effects? experienced") ~ TRUE,
    str_detect(x1, "none so how can") ~ TRUE,
    str_detect(x1, "^n+o+$") ~ TRUE,
    str_detect(x1, "^n+o+o+$") ~ TRUE,
    TRUE ~ FALSE
  )
}

classify_fisber_text_topic <- function(x) {
  x0 <- clean_free_text(x)
  x1 <- str_to_lower(x0)
  
  case_when(
    is_no_side_effect_text(x1) ~ "No side effects / not applicable",
    str_detect(x1, "sleep|sleepy|slept|asleep|tired|fatigue|drows") ~ "Sleepiness / tiredness",
    str_detect(x1, "headache|head ache|migraine") ~ "Headache",
    str_detect(x1, "dream|vivid dream|patterns|geometric|visual|colour|color|perception") ~ "Visual/perceptual after-effects",
    str_detect(x1, "sad|mood|up and down|personal circumstances|anxious|anxiety") ~ "Mood or emotional state",
    str_detect(x1, "work|study|studying|focus|unfocused|concentrat|engage") ~ "Work/study interference",
    str_detect(x1, "better|good|fine|positive|relax") ~ "Positive / felt better",
    str_detect(x1, "not sure|may not|might not|unsure|unclear") ~ "Uncertain attribution",
    TRUE ~ "Other substantive comment"
  )
}

# ============================================================
# 2. LOCATE FILES
# ============================================================

WP2_ASSIGN_PATH <- newest_match(c("*wp2_assignments*.csv"))

WP2_PRE24_PATH <- newest_match(
  c("*wp2_pre_sessions_2-4*.csv", "*wp2_pre_sessions_2_4*.csv", "*pre_sessions_2-4*.csv"),
  required = FALSE
)

WP2_POST13_PATH <- newest_match(c(
  "*wp2_post_sessions_1-3*.csv",
  "*wp2_post_session_1_3*.csv",
  "*wp2_post_session_13*.csv",
  "*wp2_post*session*1*3*.csv"
), required = FALSE)

WP2_POST4_PATH <- newest_match(c("*wp2_post_session_4*.csv"), required = FALSE)

if (is.null(WP2_POST13_PATH) && is.null(WP2_POST4_PATH)) {
  stop("No WP2 post-session files found. Need post-session 1-3 and/or post-session 4 files.")
}

message("WP2 assignments: ", basename(WP2_ASSIGN_PATH))
message("WP2 pre S2-4:    ", ifelse(is.null(WP2_PRE24_PATH), "not found", basename(WP2_PRE24_PATH)))
message("WP2 post S1-3:   ", ifelse(is.null(WP2_POST13_PATH), "not found", basename(WP2_POST13_PATH)))
message("WP2 post S4:     ", ifelse(is.null(WP2_POST4_PATH), "not found", basename(WP2_POST4_PATH)))

SAFETY_PATHS <- all_matches(c(
  "*wp2*adverse*.csv",
  "*wp2*ae*.csv",
  "*wp2*safety*.csv",
  "*wp2*post*.csv"
), required = FALSE)

SAFETY_PATHS <- unique(c(WP2_POST13_PATH, WP2_POST4_PATH, SAFETY_PATHS))
SAFETY_PATHS <- SAFETY_PATHS[!is.na(SAFETY_PATHS)]
SAFETY_PATHS <- SAFETY_PATHS[file.exists(SAFETY_PATHS)]

SAFETY_PATHS <- SAFETY_PATHS[
  !str_detect(basename(SAFETY_PATHS), regex("^S9|safety_tolerability|column_detection|adverse_event", ignore_case = TRUE))
]

# ============================================================
# 3. LOAD ASSIGNMENTS
# ============================================================

wp2_assign_raw <- read_qualtrics_real(WP2_ASSIGN_PATH)

cond_col <- find_col(
  wp2_assign_raw,
  c(
    "condition", "allocation", "group", "arm", "assigned_condition",
    "randomised_condition", "randomized_condition", "treatment"
  ),
  required = TRUE
)

pid_assign_col <- find_col(wp2_assign_raw, c("part_id", "participant_id", "id"), required = TRUE)

wp2_assign <- wp2_assign_raw %>%
  mutate(
    part_id = clean_id(.data[[pid_assign_col]]),
    condition = standardise_condition(.data[[cond_col]])
  ) %>%
  filter(!is.na(part_id), !is.na(condition)) %>%
  distinct(part_id, .keep_all = TRUE) %>%
  select(part_id, condition)

arm_denoms <- wp2_assign %>%
  count(condition, name = "n_randomised") %>%
  complete(condition = c("Control", "Intervention"), fill = list(n_randomised = 0))

# ============================================================
# 4. LOAD POST-SESSION DATA FOR VDQ/DISCOMFORT
# ============================================================

load_post_file <- function(path, forced_session = NULL) {
  if (is.null(path) || !file.exists(path)) return(tibble())
  
  df <- read_qualtrics_real(path)
  
  pid_col <- find_col(df, c("part_id", "participant_id", "id"), required = TRUE)
  sess_col <- find_col(df, c("session_n", "session", "session_number", "sess"), required = FALSE)
  
  df %>%
    mutate(
      source_file = basename(path),
      part_id = clean_id(.data[[pid_col]]),
      session_n = if (!is.null(forced_session)) {
        forced_session
      } else if (!is.null(sess_col)) {
        to_num(.data[[sess_col]])
      } else {
        NA_real_
      }
    ) %>%
    filter(!is.na(part_id)) %>%
    mutate(session_n = as.integer(session_n))
}

post13 <- load_post_file(WP2_POST13_PATH)
post4  <- load_post_file(WP2_POST4_PATH, forced_session = 4)

wp2_post_raw <- bind_rows(post13, post4) %>%
  filter(!is.na(part_id), session_n %in% 1:4) %>%
  left_join(wp2_assign, by = "part_id")

if (nrow(wp2_post_raw) == 0) {
  stop("Post-session files loaded, but no usable participant/session rows were found.")
}

# ============================================================
# 5. LOAD PRE-SESSIONS 2-4 FOR FISBER
# ============================================================

load_pre24_fisber <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(tibble(
      part_id = character(),
      condition = character(),
      session_n = integer(),
      fisber_frequency = numeric(),
      fisber_severity = numeric(),
      fisber_interference = numeric(),
      fisber_text = character(),
      source_file = character()
    ))
  }
  
  df <- read_qualtrics_real(path)
  
  pid_col <- find_col(df, c("part_id", "participant_id", "id"), required = TRUE)
  sess_col <- find_col(df, c("session_n", "session", "session_number", "sess"), required = FALSE)
  
  f1 <- find_col(df, c("fisber_1", "fibser_1", "fisber1", "fibser1"), required = FALSE)
  f2 <- find_col(df, c("fisber_2", "fibser_2", "fisber2", "fibser2"), required = FALSE)
  f3 <- find_col(df, c("fisber_3", "fibser_3", "fisber3", "fibser3"), required = FALSE)
  ft <- find_col(df, c("fisber_text", "fibser_text", "fisber_text_1", "fibser_text_1"), required = FALSE)
  
  if (is.null(f1) && is.null(f2) && is.null(f3) && is.null(ft)) {
    warning("No fisber/fibser columns found in ", basename(path))
  }
  
  out <- df %>%
    mutate(
      source_file = basename(path),
      part_id = clean_id(.data[[pid_col]]),
      session_n = if (!is.null(sess_col)) to_num(.data[[sess_col]]) else NA_real_,
      fisber_frequency = if (!is.null(f1)) extract_trailing_score(.data[[f1]]) else NA_real_,
      fisber_severity = if (!is.null(f2)) extract_trailing_score(.data[[f2]]) else NA_real_,
      fisber_interference = if (!is.null(f3)) extract_trailing_score(.data[[f3]]) else NA_real_,
      fisber_text = if (!is.null(ft)) clean_free_text(.data[[ft]]) else NA_character_
    ) %>%
    filter(!is.na(part_id), !is.na(session_n)) %>%
    mutate(session_n = as.integer(session_n)) %>%
    filter(session_n %in% 2:4) %>%
    select(
      part_id,
      session_n,
      fisber_frequency,
      fisber_severity,
      fisber_interference,
      fisber_text,
      source_file
    ) %>%
    left_join(wp2_assign, by = "part_id")
  
  out
}

fisber_pre_long <- load_pre24_fisber(WP2_PRE24_PATH)

write_csv(fisber_pre_long, file.path(OUT_DIR, "S9_fisber_pre_sessions_2_4_long.csv"))

# ============================================================
# 6. DETECT CORE VDQ / SAFETY / TOLERABILITY COLUMNS
# ============================================================

tol_score_col <- find_col(
  wp2_post_raw,
  c(
    "tol_score", "discomfortScore", "discomfort_score",
    "vdq", "vdq_score", "vdq_total", "tolerability_score"
  ),
  required = FALSE
)

tol_follow_cols <- find_cols_regex(
  wp2_post_raw,
  c("^tol_follow", "vdq_", "side.?effect", "symptom", "discomfort")
)

tol_follow_cols <- setdiff(
  tol_follow_cols,
  c(
    tol_score_col,
    "discomfortScore", "discomfort_score",
    "vdq", "vdq_score", "vdq_total", "tolerability_score"
  )
)

ae_cols <- find_cols_regex(
  wp2_post_raw,
  c(
    "adverse", "\\bae\\b", "serious", "\\bsae\\b", "safeguard",
    "medical", "hospital", "harm", "risk", "incident"
  )
)

discontinuation_cols <- find_cols_regex(
  wp2_post_raw,
  c("withdraw", "discontinu", "drop.?out", "early.?stop", "stopped", "terminate", "ceased", "could.?not.?continue")
)

# ============================================================
# 7. CREATE VDQ / DISCOMFORT LONG TABLE
# ============================================================

compute_vdq_score_from_df <- function(df) {
  if (!is.null(tol_score_col) && tol_score_col %in% names(df)) {
    return(pmin(pmax(to_num(df[[tol_score_col]]), 0), 10))
  }
  
  if (length(tol_follow_cols) > 0) {
    tmp <- df %>%
      select(all_of(tol_follow_cols)) %>%
      mutate(across(everything(), to_num))
    
    return(pmin(pmax(apply(tmp, 1, function(z) {
      if (all(is.na(z))) NA_real_ else max(z, na.rm = TRUE)
    }), 0), 10))
  }
  
  rep(NA_real_, nrow(df))
}

compute_any_side_effect_from_df <- function(df, vdq_score) {
  if (length(tol_follow_cols) > 0) {
    return(apply(
      df %>% select(all_of(tol_follow_cols)),
      1,
      function(z) any(num_or_yes(z), na.rm = TRUE)
    ))
  }
  
  !is.na(vdq_score) & vdq_score > 0
}

wp2_tol_long <- wp2_post_raw %>%
  mutate(
    vdq_score = compute_vdq_score_from_df(.),
    any_side_effect = compute_any_side_effect_from_df(., vdq_score)
  ) %>%
  filter(!is.na(condition), !is.na(session_n))

write_csv(wp2_tol_long, file.path(OUT_DIR, "S9_wp2_post_session_safety_long.csv"))

# ============================================================
# 8. S9d: DISCOMFORT MEANS / SD / CI BY SESSION AND ARM
# ============================================================

discomfort_summary <- wp2_tol_long %>%
  filter(!is.na(vdq_score)) %>%
  group_by(condition, session_n) %>%
  summarise(
    n = n_distinct(part_id),
    mean = mean(vdq_score, na.rm = TRUE),
    sd = sd(vdq_score, na.rm = TRUE),
    se = sd / sqrt(n),
    ci95_low = mean - Z95 * se,
    ci95_high = mean + Z95 * se,
    median = median(vdq_score, na.rm = TRUE),
    min = min(vdq_score, na.rm = TRUE),
    max = max(vdq_score, na.rm = TRUE),
    n_any_side_effect = n_distinct(part_id[any_side_effect]),
    percent_any_side_effect = 100 * n_any_side_effect / n,
    .groups = "drop"
  ) %>%
  mutate(
    session_label = paste0("Session ", session_n),
    mean_sd = paste0(fmt_num(mean), " ± ", fmt_num(sd)),
    ci95 = fmt_ci(ci95_low, ci95_high),
    any_side_effect_display = paste0(
      n_any_side_effect, "/", n, " (", fmt_pct(percent_any_side_effect), ")"
    )
  ) %>%
  arrange(condition, session_n)

write_csv(discomfort_summary, file.path(OUT_DIR, "S9d_vdq_discomfort_summary_by_arm_session.csv"))

tbl_discomfort <- discomfort_summary %>%
  select(
    Arm = condition,
    Session = session_label,
    n,
    `Mean ± SD` = mean_sd,
    `95% CI` = ci95,
    Median = median,
    Min = min,
    Max = max,
    `Any side-effect endorsement` = any_side_effect_display
  ) %>%
  gt(groupname_col = "Arm") %>%
  style_s9_gt(
    title = "Table S9d. WP2 VDQ/discomfort summary by arm and session",
    subtitle = md("VDQ/discomfort score is taken from `tol_score`/VDQ where available, otherwise from the maximum available `tol_follow*` symptom severity.")
  ) %>%
  fmt_number(columns = c(Median, Min, Max), decimals = 2)

save_gt(tbl_discomfort, "S9d_vdq_discomfort_summary_by_arm_session")

# ============================================================
# 9. S9e: VDQ SIDE-EFFECT COUNTS BY PARTICIPANT / SESSION
# ============================================================

if (length(tol_follow_cols) > 0) {
  side_effect_count_vec <- apply(
    wp2_tol_long %>% select(all_of(tol_follow_cols)),
    1,
    function(z) sum(num_or_yes(z), na.rm = TRUE)
  )
} else {
  side_effect_count_vec <- if_else(
    !is.na(wp2_tol_long$vdq_score) & wp2_tol_long$vdq_score > 0,
    1L,
    0L
  )
}

participant_session_counts <- wp2_tol_long %>%
  mutate(side_effect_count = as.integer(side_effect_count_vec)) %>%
  select(
    part_id,
    condition,
    session_n,
    vdq_score,
    any_side_effect,
    side_effect_count,
    source_file
  ) %>%
  arrange(condition, part_id, session_n)

write_csv(participant_session_counts, file.path(OUT_DIR, "S9e_vdq_side_effect_counts_by_participant_session.csv"))

tbl_participant_counts <- participant_session_counts %>%
  mutate(
    session_label = paste0("S", session_n),
    any_side_effect = if_else(any_side_effect, "Yes", "No")
  ) %>%
  select(
    Arm = condition,
    Participant = part_id,
    Session = session_label,
    `VDQ/discomfort` = vdq_score,
    `Any side effect` = any_side_effect,
    `No. endorsed symptom fields` = side_effect_count
  ) %>%
  gt(groupname_col = "Arm") %>%
  style_s9_gt(
    title = "Table S9e. VDQ side-effect counts by participant and session",
    subtitle = "This participant-level audit table supports missingness and safety/tolerability traceability."
  ) %>%
  fmt_number(columns = `VDQ/discomfort`, decimals = 2)

save_gt(tbl_participant_counts, "S9e_vdq_side_effect_counts_by_participant_session", png = FALSE, pdf = FALSE)

# ============================================================
# 10. S9f: SIDE-EFFECT SYMPTOM COUNTS BY ARM / SESSION
# ============================================================

if (length(tol_follow_cols) > 0) {
  
  symptom_long <- wp2_tol_long %>%
    select(part_id, condition, session_n, all_of(tol_follow_cols)) %>%
    pivot_longer(
      cols = all_of(tol_follow_cols),
      names_to = "symptom_field",
      values_to = "raw_value"
    ) %>%
    mutate(
      endorsed = num_or_yes(raw_value),
      numeric_severity = to_num(raw_value),
      symptom_field_clean = symptom_field %>%
        str_replace_all("_", " ") %>%
        str_replace_all("\\.", " ") %>%
        str_squish()
    )
  
  symptom_summary <- symptom_long %>%
    group_by(condition, session_n, symptom_field_clean) %>%
    summarise(
      n_available = n_distinct(part_id[!is.na(raw_value) & raw_value != ""]),
      n_endorsed = n_distinct(part_id[endorsed]),
      mean_severity_among_available = ifelse(
        all(is.na(numeric_severity)),
        NA_real_,
        mean(numeric_severity, na.rm = TRUE)
      ),
      max_severity = ifelse(
        all(is.na(numeric_severity)),
        NA_real_,
        max(numeric_severity, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    mutate(
      percent_endorsed = ifelse(n_available > 0, 100 * n_endorsed / n_available, NA_real_),
      endorsed_display = paste0(n_endorsed, "/", n_available, " (", fmt_pct(percent_endorsed), ")")
    ) %>%
    arrange(condition, session_n, desc(n_endorsed), symptom_field_clean)
  
} else {
  
  symptom_long <- tibble(
    part_id = character(),
    condition = character(),
    session_n = integer(),
    symptom_field = character(),
    raw_value = character(),
    endorsed = logical(),
    numeric_severity = numeric(),
    symptom_field_clean = character()
  )
  
  symptom_summary <- tibble(
    condition = "No symptom columns detected",
    session_n = NA_integer_,
    symptom_field_clean = "No tol_follow*/VDQ side-effect symptom columns detected",
    n_available = NA_integer_,
    n_endorsed = NA_integer_,
    mean_severity_among_available = NA_real_,
    max_severity = NA_real_,
    percent_endorsed = NA_real_,
    endorsed_display = "NA"
  )
}

write_csv(symptom_long, file.path(OUT_DIR, "S9f_side_effect_symptom_long.csv"))
write_csv(symptom_summary, file.path(OUT_DIR, "S9f_side_effect_symptom_summary_by_arm_session.csv"))

tbl_symptoms <- symptom_summary %>%
  mutate(session_label = if_else(is.na(session_n), NA_character_, paste0("Session ", session_n))) %>%
  select(
    Arm = condition,
    Session = session_label,
    `Side-effect field` = symptom_field_clean,
    `Endorsed / available` = endorsed_display,
    `Mean severity` = mean_severity_among_available,
    `Maximum severity` = max_severity
  ) %>%
  gt(groupname_col = "Arm") %>%
  style_s9_gt(
    title = "Table S9f. VDQ side-effect symptom counts by arm and session",
    subtitle = "Side-effect endorsement is defined as a non-zero numeric response or a non-empty affirmative/free-text response."
  ) %>%
  fmt_number(columns = c(`Mean severity`, `Maximum severity`), decimals = 2)

save_gt(tbl_symptoms, "S9f_side_effect_symptom_summary_by_arm_session", png = FALSE, pdf = FALSE)

# ============================================================
# 11. S9c: FISBER/FIBSER SUMMARIES FROM PRE-SESSIONS 2-4
# ============================================================

fisber_long <- fisber_pre_long %>%
  select(
    part_id,
    condition,
    session_n,
    Frequency = fisber_frequency,
    Severity = fisber_severity,
    Interference = fisber_interference
  ) %>%
  pivot_longer(
    cols = c(Frequency, Severity, Interference),
    names_to = "dimension",
    values_to = "value"
  ) %>%
  filter(!is.na(value))

if (nrow(fisber_long) > 0) {
  fisber_summary <- fisber_long %>%
    group_by(condition, session_n, dimension) %>%
    summarise(
      n = n_distinct(part_id),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      se = sd / sqrt(n),
      ci95_low = mean - Z95 * se,
      ci95_high = mean + Z95 * se,
      median = median(value, na.rm = TRUE),
      min = safe_min(value),
      max = safe_max(value),
      n_nonzero = n_distinct(part_id[value > 0]),
      percent_nonzero = 100 * n_nonzero / n,
      .groups = "drop"
    ) %>%
    mutate(
      session_label = paste0("Pre-Session ", session_n),
      mean_sd = paste0(fmt_num(mean), " ± ", fmt_num(sd)),
      ci95 = fmt_ci(ci95_low, ci95_high),
      nonzero_display = paste0(n_nonzero, "/", n, " (", fmt_pct(percent_nonzero), ")")
    ) %>%
    arrange(condition, session_n, dimension)
} else {
  fisber_summary <- tibble(
    condition = "No FISBER columns detected",
    session_n = NA_integer_,
    dimension = "No FISBER columns detected",
    n = NA_integer_,
    mean = NA_real_,
    sd = NA_real_,
    se = NA_real_,
    ci95_low = NA_real_,
    ci95_high = NA_real_,
    median = NA_real_,
    min = NA_real_,
    max = NA_real_,
    n_nonzero = NA_integer_,
    percent_nonzero = NA_real_,
    session_label = NA_character_,
    mean_sd = NA_character_,
    ci95 = NA_character_,
    nonzero_display = NA_character_
  )
}

write_csv(fisber_long, file.path(OUT_DIR, "S9c_fisber_long.csv"))
write_csv(fisber_summary, file.path(OUT_DIR, "S9c_fisber_summary_by_arm_session.csv"))

tbl_fisber <- fisber_summary %>%
  select(
    Arm = condition,
    Session = session_label,
    Dimension = dimension,
    n,
    `Mean ± SD` = mean_sd,
    `95% CI` = ci95,
    Median = median,
    Min = min,
    Max = max,
    `Non-zero endorsement` = nonzero_display
  ) %>%
  gt(groupname_col = "Arm") %>%
  style_s9_gt(
    title = "Table S9c. FISBER side-effect summaries by arm and pre-session",
    subtitle = "FISBER fields are read from the exported typo-labelled columns fisber_1, fisber_2, and fisber_3 in the pre-sessions 2–4 file."
  ) %>%
  fmt_number(columns = c(Median, Min, Max), decimals = 2)

save_gt(tbl_fisber, "S9c_fisber_summary_by_arm_session")

# ============================================================
# 12. S9i/S9j/S9k: FISBER_TEXT OPEN-TEXT ANALYSES
# ============================================================

fisber_text_long <- fisber_pre_long %>%
  mutate(
    text_clean = clean_free_text(fisber_text),
    text_missing_or_empty = is.na(text_clean) | text_clean == "",
    text_no_side_effect = is_no_side_effect_text(text_clean),
    text_substantive = !text_missing_or_empty & !text_no_side_effect,
    text_topic = classify_fisber_text_topic(text_clean),
    any_numeric_fisber = if_else(
      coalesce(fisber_frequency, 0) > 0 |
        coalesce(fisber_severity, 0) > 0 |
        coalesce(fisber_interference, 0) > 0,
      TRUE,
      FALSE
    ),
    text_numeric_pattern = case_when(
      any_numeric_fisber & text_substantive ~ "Numeric endorsement + substantive text",
      any_numeric_fisber & text_no_side_effect ~ "Numeric endorsement + no-side-effect text",
      any_numeric_fisber & text_missing_or_empty ~ "Numeric endorsement + empty text",
      !any_numeric_fisber & text_substantive ~ "No numeric endorsement + substantive text",
      !any_numeric_fisber & text_no_side_effect ~ "No numeric endorsement + no-side-effect text",
      TRUE ~ "No numeric endorsement + empty text"
    )
  )

write_csv(fisber_text_long, file.path(OUT_DIR, "S9i_fisber_text_long_classified.csv"))

fisber_text_summary <- fisber_text_long %>%
  group_by(condition, session_n) %>%
  summarise(
    n_records = n(),
    n_with_any_text = sum(!text_missing_or_empty, na.rm = TRUE),
    n_no_side_effect_text = sum(text_no_side_effect, na.rm = TRUE),
    n_substantive_text = sum(text_substantive, na.rm = TRUE),
    n_numeric_fisber = sum(any_numeric_fisber, na.rm = TRUE),
    n_numeric_but_no_side_effect_text = sum(any_numeric_fisber & text_no_side_effect, na.rm = TRUE),
    n_substantive_without_numeric = sum(!any_numeric_fisber & text_substantive, na.rm = TRUE),
    percent_no_side_effect_text = 100 * n_no_side_effect_text / n_records,
    percent_substantive_text = 100 * n_substantive_text / n_records,
    .groups = "drop"
  ) %>%
  mutate(
    session_label = paste0("Pre-Session ", session_n),
    any_text_display = paste0(n_with_any_text, "/", n_records),
    no_side_effect_display = paste0(n_no_side_effect_text, "/", n_records, " (", fmt_pct(percent_no_side_effect_text), ")"),
    substantive_display = paste0(n_substantive_text, "/", n_records, " (", fmt_pct(percent_substantive_text), ")"),
    numeric_but_no_side_effect_display = paste0(n_numeric_but_no_side_effect_text, "/", n_records)
  ) %>%
  arrange(condition, session_n)

write_csv(fisber_text_summary, file.path(OUT_DIR, "S9i_fisber_text_classification_summary.csv"))

tbl_text_summary <- fisber_text_summary %>%
  select(
    Arm = condition,
    Session = session_label,
    `Records` = n_records,
    `Any text` = any_text_display,
    `No-side-effect / N/A text` = no_side_effect_display,
    `Substantive text` = substantive_display,
    `Numeric FISBER endorsement` = n_numeric_fisber,
    `Numeric endorsement + no-side-effect text` = numeric_but_no_side_effect_display
  ) %>%
  gt(groupname_col = "Arm") %>%
  style_s9_gt(
    title = "Table S9i. FISBER open-text response classification",
    subtitle = "Open-text responses are classified as no-side-effect/N/A text or substantive side-effect comments before qualitative keyword summaries."
  )

save_gt(tbl_text_summary, "S9i_fisber_text_classification_summary")

fisber_topic_summary <- fisber_text_long %>%
  filter(!text_missing_or_empty) %>%
  group_by(condition, session_n, text_topic) %>%
  summarise(
    n = n(),
    n_numeric_fisber = sum(any_numeric_fisber, na.rm = TRUE),
    example_1 = first(text_clean[text_clean != "" & !is.na(text_clean)]),
    .groups = "drop"
  ) %>%
  group_by(condition, session_n) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(
    session_label = paste0("Pre-Session ", session_n),
    n_percent = paste0(n, " (", fmt_pct(percent), ")")
  ) %>%
  arrange(condition, session_n, desc(n))

write_csv(fisber_topic_summary, file.path(OUT_DIR, "S9j_fisber_text_topic_summary.csv"))

tbl_topic_summary <- fisber_topic_summary %>%
  select(
    Arm = condition,
    Session = session_label,
    `Text category` = text_topic,
    `n (%)` = n_percent,
    `Numeric FISBER endorsements` = n_numeric_fisber,
    `Example response` = example_1
  ) %>%
  gt(groupname_col = "Arm") %>%
  style_s9_gt(
    title = "Table S9j. FISBER open-text keyword/topic summary",
    subtitle = "Categories use transparent keyword rules; examples are shown for auditability rather than formal topic-model inference."
  )

save_gt(tbl_topic_summary, "S9j_fisber_text_topic_summary", png = FALSE, pdf = FALSE)

# Word-frequency table from substantive text only
substantive_text_df <- fisber_text_long %>%
  filter(text_substantive) %>%
  select(part_id, condition, session_n, text_clean)

custom_stopwords <- c(
  "side", "effect", "effects", "strobe", "session", "sessions",
  "feel", "felt", "just", "day", "week", "time", "really",
  "thing", "things", "bit", "much", "also", "may", "maybe",
  "think", "dont", "don't", "could", "would", "did", "didnt",
  "didn't", "ive", "i've", "im", "i'm", "na", "n/a", "none",
  "no", "not", "applicable"
)

if (nrow(substantive_text_df) > 0) {
  
  if (requireNamespace("tidytext", quietly = TRUE)) {
    data("stop_words", package = "tidytext")
    
    fisber_word_counts <- substantive_text_df %>%
      tidytext::unnest_tokens(word, text_clean) %>%
      mutate(word = str_to_lower(word)) %>%
      filter(str_detect(word, "^[a-z]+$")) %>%
      anti_join(tidytext::stop_words, by = "word") %>%
      filter(!word %in% custom_stopwords) %>%
      count(word, sort = TRUE)
  } else {
    fisber_word_counts <- substantive_text_df %>%
      mutate(text_clean = str_to_lower(text_clean)) %>%
      mutate(text_clean = str_replace_all(text_clean, "[^a-z\\s]", " ")) %>%
      separate_rows(text_clean, sep = "\\s+") %>%
      rename(word = text_clean) %>%
      filter(word != "", nchar(word) > 2) %>%
      filter(!word %in% custom_stopwords) %>%
      count(word, sort = TRUE)
  }
  
} else {
  fisber_word_counts <- tibble(word = character(), n = integer())
}

write_csv(fisber_word_counts, file.path(OUT_DIR, "S9k_fisber_text_word_counts.csv"))

tbl_word_counts <- fisber_word_counts %>%
  slice_head(n = 30) %>%
  gt() %>%
  style_s9_gt(
    title = "Table S9k. Most frequent words in substantive FISBER open-text responses",
    subtitle = "Counts exclude no-side-effect/N/A responses and common stopwords."
  )

save_gt(tbl_word_counts, "S9k_fisber_text_word_counts", png = FALSE, pdf = FALSE)

# Optional word cloud
if (HAS_GGWORDCLOUD && nrow(fisber_word_counts) > 0) {
  p_wc <- fisber_word_counts %>%
    slice_max(n, n = 60) %>%
    ggplot(aes(label = word, size = n)) +
    ggwordcloud::geom_text_wordcloud_area() +
    scale_size_area(max_size = 14) +
    theme_minimal(base_family = PALATINO_NAME) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    labs(title = "FISBER open-text word cloud: substantive side-effect comments")
  
  ggsave(
    file.path(OUT_DIR, "S9k_fisber_text_wordcloud.png"),
    p_wc,
    width = 7.2,
    height = 4.8,
    dpi = 300
  )
  
  ggsave(
    file.path(OUT_DIR, "S9k_fisber_text_wordcloud.pdf"),
    p_wc,
    width = 7.2,
    height = 4.8
  )
}

# ============================================================
# 13. S9a/S9b: ADVERSE EVENT / SAE TABLES
# ============================================================

safety_long <- map_dfr(SAFETY_PATHS, function(path) {
  df <- read_qualtrics_real(path)
  
  pid_col <- find_col(df, c("part_id", "participant_id", "id"), required = FALSE)
  if (is.null(pid_col)) return(empty_safety_long())
  
  sess_col <- find_col(df, c("session_n", "session", "session_number", "sess"), required = FALSE)
  
  candidate_cols <- find_cols_regex(
    df,
    c(
      "adverse", "\\bae\\b", "serious", "\\bsae\\b", "safeguard",
      "medical", "hospital", "harm", "risk", "incident",
      "withdraw", "discontinu", "drop.?out", "early.?stop", "stopped"
    )
  )
  
  if (length(candidate_cols) == 0) return(empty_safety_long())
  
  df %>%
    mutate(
      source_file = basename(path),
      part_id = clean_id(.data[[pid_col]]),
      session_n = if (!is.null(sess_col)) as.integer(to_num(.data[[sess_col]])) else NA_integer_
    ) %>%
    filter(!is.na(part_id)) %>%
    select(part_id, session_n, source_file, all_of(candidate_cols)) %>%
    pivot_longer(
      cols = all_of(candidate_cols),
      names_to = "event_field",
      values_to = "event_value"
    ) %>%
    mutate(
      event_value_chr = str_squish(as.character(event_value)),
      event_reported = yesish(event_value_chr),
      is_sae_field = str_detect(event_field, regex("sae|serious|hospital", ignore_case = TRUE)),
      is_discontinuation_field = str_detect(event_field, regex("withdraw|discontinu|drop.?out|early.?stop|stopped", ignore_case = TRUE))
    ) %>%
    filter(event_reported)
})

if (nrow(safety_long) == 0 || !"part_id" %in% names(safety_long)) {
  safety_long <- empty_safety_long()
}

safety_long <- safety_long %>%
  left_join(wp2_assign, by = "part_id") %>%
  mutate(
    condition = replace_na(condition, "Unassigned / not detected"),
    event_class = case_when(
      is_sae_field ~ "Serious adverse event / SAE-like field",
      is_discontinuation_field ~ "Withdrawal / discontinuation-like field",
      TRUE ~ "Adverse event / safety field"
    )
  )

if (nrow(safety_long) == 0) {
  ae_listing <- empty_ae_listing()
  
  ae_summary <- tibble(
    condition = c("Control", "Intervention"),
    event_class = "No adverse-event/SAE fields with positive entries detected",
    n_participants = 0L,
    n_events_or_entries = 0L
  )
} else {
  ae_listing <- safety_long %>%
    select(
      part_id,
      condition,
      session_n,
      event_class,
      event_field,
      event_value_chr,
      source_file
    ) %>%
    arrange(condition, part_id, session_n, event_class)
  
  ae_summary <- safety_long %>%
    group_by(condition, event_class) %>%
    summarise(
      n_participants = n_distinct(part_id),
      n_events_or_entries = n(),
      .groups = "drop"
    ) %>%
    arrange(condition, event_class)
}

write_csv(ae_listing, file.path(OUT_DIR, "S9b_adverse_event_sae_listing.csv"))
write_csv(ae_summary, file.path(OUT_DIR, "S9a_adverse_event_sae_summary.csv"))

tbl_ae_summary <- ae_summary %>%
  select(
    Arm = condition,
    `Event class` = event_class,
    `Participants with entry` = n_participants,
    `Total entries` = n_events_or_entries
  ) %>%
  gt(groupname_col = "Arm") %>%
  style_s9_gt(
    title = "Table S9a. Adverse event and serious adverse event summary",
    subtitle = "Entries are detected from adverse-event, SAE, serious, safeguarding, medical, withdrawal, and discontinuation-like fields."
  )

save_gt(tbl_ae_summary, "S9a_adverse_event_sae_summary")

tbl_ae_listing <- ae_listing %>%
  select(
    Arm = condition,
    Participant = part_id,
    Session = session_n,
    `Event class` = event_class,
    Field = event_field,
    Entry = event_value_chr,
    Source = source_file
  ) %>%
  gt(groupname_col = "Arm") %>%
  style_s9_gt(
    title = "Table S9b. Adverse event and serious adverse event listing",
    subtitle = "Participant-level listing of all detected safety/adverse-event entries."
  )

save_gt(tbl_ae_listing, "S9b_adverse_event_sae_listing", png = FALSE, pdf = FALSE)

# ============================================================
# 14. S9g: TOLERABILITY-RELATED DISCONTINUATION DETAILS
# ============================================================

manual_discontinuations <- tribble(
  ~part_id, ~manual_reason, ~manual_classification,
  "11213", "Dropped out halfway through Session 1; said it was scary", "Tolerability-related / acute acceptability",
  "11186", "Dropped out after Session 1; did not like the experiment", "Acceptability-related",
  "11152", "Too sick to make Session 2; would need to re-sign up once recovered", "Health/life-event related",
  "11309", "Life events after Session 1", "Life-event related",
  "11162", "Life events after Session 2", "Life-event related",
  "11195", "Drop-out", "Unspecified drop-out",
  "11383", "Dropped after Session 3 due to moving", "Logistical/life-event related",
  "21399", "Dropped out after Session 1", "Unspecified drop-out",
  "21156", "Dropped out after Session 2; life events", "Life-event related",
  "11153", "Dropped out after taster session; excluded from Session 1 retention", "Taster withdrawal"
) %>%
  mutate(part_id = clean_id(part_id)) %>%
  left_join(wp2_assign, by = "part_id") %>%
  mutate(condition = replace_na(condition, "Unassigned / not detected"))

if (nrow(ae_listing) == 0 || !"event_class" %in% names(ae_listing)) {
  detected_discontinuations <- tibble(
    part_id = character(),
    condition = character(),
    session_n = integer(),
    detected_field = character(),
    detected_entry = character(),
    source_file = character()
  )
} else {
  detected_discontinuations <- ae_listing %>%
    filter(str_detect(event_class, regex("withdrawal|discontinuation", ignore_case = TRUE))) %>%
    transmute(
      part_id,
      condition,
      session_n,
      detected_field = event_field,
      detected_entry = event_value_chr,
      source_file
    )
}

discontinuation_details <- manual_discontinuations %>%
  full_join(detected_discontinuations, by = c("part_id", "condition")) %>%
  mutate(
    session_n = suppressWarnings(as.integer(session_n)),
    manual_reason = replace_na(manual_reason, ""),
    manual_classification = replace_na(manual_classification, ""),
    detected_field = replace_na(detected_field, ""),
    detected_entry = replace_na(detected_entry, ""),
    source_file = replace_na(source_file, "")
  ) %>%
  arrange(condition, part_id, session_n)

write_csv(discontinuation_details, file.path(OUT_DIR, "S9g_tolerability_related_discontinuation_details.csv"))

tbl_disc <- discontinuation_details %>%
  select(
    Arm = condition,
    Participant = part_id,
    Session = session_n,
    `Manual reason / note` = manual_reason,
    `Manual classification` = manual_classification,
    `Detected field` = detected_field,
    `Detected entry` = detected_entry,
    Source = source_file
  ) %>%
  gt(groupname_col = "Arm") %>%
  style_s9_gt(
    title = "Table S9g. Tolerability-related discontinuation and withdrawal details",
    subtitle = "Manual tracking notes are combined with any detected withdrawal/discontinuation-like questionnaire fields."
  )

save_gt(tbl_disc, "S9g_tolerability_related_discontinuation_details", png = FALSE, pdf = FALSE)

# ============================================================
# 15. S9h: COLUMN-DETECTION AUDIT
# ============================================================

column_audit <- tibble(
  Category = c(
    "VDQ/discomfort score column",
    "VDQ/tol_follow symptom columns",
    "FISBER frequency column",
    "FISBER severity column",
    "FISBER interference column",
    "FISBER open-text column",
    "Adverse event / SAE candidate columns",
    "Discontinuation candidate columns"
  ),
  Columns_detected = c(
    tol_score_col %||% "None detected",
    ifelse(length(tol_follow_cols) > 0, paste(tol_follow_cols, collapse = "; "), "None detected"),
    "fisber_1 / fibser_1 searched in pre-sessions 2–4",
    "fisber_2 / fibser_2 searched in pre-sessions 2–4",
    "fisber_3 / fibser_3 searched in pre-sessions 2–4",
    "fisber_text / fibser_text searched in pre-sessions 2–4",
    ifelse(length(ae_cols) > 0, paste(ae_cols, collapse = "; "), "None detected in post-session files"),
    ifelse(length(discontinuation_cols) > 0, paste(discontinuation_cols, collapse = "; "), "None detected in post-session files")
  )
)

write_csv(column_audit, file.path(OUT_DIR, "S9h_column_detection_audit.csv"))

tbl_audit <- column_audit %>%
  gt() %>%
  style_s9_gt(
    title = "Table S9h. Safety/tolerability column-detection audit",
    subtitle = "Use this audit to verify that the script detected the intended exported Qualtrics fields."
  )

save_gt(tbl_audit, "S9h_column_detection_audit", png = FALSE, pdf = FALSE)

# ============================================================
# 16. NARRATIVE EXPORT
# ============================================================

overall_discomfort <- wp2_tol_long %>%
  filter(!is.na(vdq_score)) %>%
  summarise(
    n_sessions = n(),
    n_participants = n_distinct(part_id),
    mean = mean(vdq_score, na.rm = TRUE),
    sd = sd(vdq_score, na.rm = TRUE),
    max = max(vdq_score, na.rm = TRUE),
    n_any = n_distinct(part_id[any_side_effect]),
    .groups = "drop"
  )

if (nrow(discomfort_summary) > 0) {
  highest_session <- discomfort_summary %>%
    arrange(desc(mean)) %>%
    slice(1)
} else {
  highest_session <- tibble(
    condition = NA_character_,
    session_n = NA_integer_,
    mean = NA_real_,
    sd = NA_real_,
    ci95 = NA_character_
  )
}

ae_total_participants <- ae_listing %>%
  filter(event_class == "Adverse event / safety field") %>%
  summarise(n = n_distinct(part_id)) %>%
  pull(n)

sae_total_participants <- ae_listing %>%
  filter(event_class == "Serious adverse event / SAE-like field") %>%
  summarise(n = n_distinct(part_id)) %>%
  pull(n)

ae_total_participants <- ifelse(length(ae_total_participants) == 0, 0, ae_total_participants)
sae_total_participants <- ifelse(length(sae_total_participants) == 0, 0, sae_total_participants)

fisber_line <- if (nrow(fisber_long) > 0) {
  paste0(
    "FISBER frequency, severity, and interference fields were detected in the typo-labelled pre-session columns ",
    "fisber_1, fisber_2, and fisber_3 and summarised by arm and pre-session."
  )
} else {
  "No FISBER-like columns were detected in the loaded WP2 pre-session files; see the column-detection audit."
}

text_overall <- fisber_text_long %>%
  summarise(
    n_records = n(),
    n_with_any_text = sum(!text_missing_or_empty, na.rm = TRUE),
    n_no_side_effect_text = sum(text_no_side_effect, na.rm = TRUE),
    n_substantive_text = sum(text_substantive, na.rm = TRUE),
    n_numeric_fisber = sum(any_numeric_fisber, na.rm = TRUE),
    n_numeric_but_no_side_effect_text = sum(any_numeric_fisber & text_no_side_effect, na.rm = TRUE),
    n_substantive_without_numeric = sum(!any_numeric_fisber & text_substantive, na.rm = TRUE),
    .groups = "drop"
  )

top_topics <- fisber_topic_summary %>%
  filter(text_topic != "No side effects / not applicable") %>%
  group_by(text_topic) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  arrange(desc(n)) %>%
  slice_head(n = 5) %>%
  mutate(txt = paste0(text_topic, " (n = ", n, ")")) %>%
  pull(txt) %>%
  paste(collapse = "; ")

if (is.na(top_topics) || top_topics == "") {
  top_topics <- "no substantive open-text side-effect themes detected"
}

s9_text <- c(
  "Supplementary Table S9. WP2 safety and tolerability.",
  "====================================================",
  "",
  paste0("WP2 post-session safety/tolerability rows analysed: n = ", nrow(wp2_tol_long), "."),
  paste0("Unique participants with post-session tolerability data: n = ", overall_discomfort$n_participants, "."),
  "",
  "VDQ/discomfort summary:",
  paste0(
    "Across available WP2 post-session records, mean VDQ/discomfort was ",
    fmt_num(overall_discomfort$mean),
    " ± ",
    fmt_num(overall_discomfort$sd),
    "/10, with a maximum observed value of ",
    fmt_num(overall_discomfort$max),
    "/10."
  ),
  paste0(
    "The highest arm/session mean was observed for ",
    highest_session$condition,
    " at Session ",
    highest_session$session_n,
    ": mean = ",
    fmt_num(highest_session$mean),
    " ± ",
    fmt_num(highest_session$sd),
    "/10; 95% CI ",
    highest_session$ci95,
    "."
  ),
  "",
  "FISBER:",
  fisber_line,
  "",
  "FISBER open-text responses:",
  paste0(
    "The open-text side-effect field was treated as an interpretive audit variable rather than a direct adverse-event count. ",
    "Across FISBER records, ",
    text_overall$n_with_any_text, "/", text_overall$n_records,
    " contained some text. However, ",
    text_overall$n_no_side_effect_text, "/", text_overall$n_records,
    " were classified as no-side-effect, N/A, or equivalent responses, while ",
    text_overall$n_substantive_text, "/", text_overall$n_records,
    " contained substantive comments."
  ),
  paste0(
    "This distinction is important because some participants entered text in the side-effect description field while explicitly stating ",
    "that they had no side effects, or entered N/A/none/no-side-effect style responses. Conversely, a smaller number of substantive ",
    "comments described transient sleepiness/tiredness, headache, visual or perceptual after-effects, mood-related uncertainty, or ",
    "temporary work/study interference. Top substantive categories were: ",
    top_topics,
    "."
  ),
  paste0(
    "The script also reports potentially discordant cases, including ",
    text_overall$n_numeric_but_no_side_effect_text,
    " records with non-zero numeric FISBER endorsement but no-side-effect/N/A-style text, and ",
    text_overall$n_substantive_without_numeric,
    " records with substantive text but no non-zero numeric FISBER endorsement."
  ),
  "",
  "Adverse events / serious adverse events:",
  paste0("Participants with detected AE-like entries: n = ", ae_total_participants, "."),
  paste0("Participants with detected SAE-like entries: n = ", sae_total_participants, "."),
  "",
  "Discontinuations:",
  paste0(
    "Tolerability-related and withdrawal/discontinuation details are listed in Table S9g. ",
    "Manual tracking notes are retained separately from questionnaire-detected discontinuation fields to preserve auditability."
  ),
  "",
  "Column-detection audit:",
  "Table S9h lists the exact columns detected for VDQ/discomfort, FISBER, adverse events/SAEs, side-effect symptoms, and discontinuation fields.",
  "",
  paste0("Output directory: ", OUT_DIR)
)

writeLines(s9_text, file.path(OUT_DIR, "S9_WP2_safety_tolerability_narrative.txt"))

# ============================================================
# 17. CONSOLE SUMMARY
# ============================================================

cat("\n============================================================\n")
cat("SUPPLEMENTARY S9 SAFETY/TOLERABILITY EXPORT COMPLETE\n")
cat("============================================================\n")
cat("Output directory:\n", OUT_DIR, "\n\n")

cat("Detected VDQ/discomfort score column:\n  ", tol_score_col %||% "None; using tol_follow* fallback if available", "\n\n")

cat("Detected tol_follow / side-effect columns:\n  ",
    ifelse(length(tol_follow_cols) > 0, paste(tol_follow_cols, collapse = "; "), "None detected"),
    "\n\n")

cat("FISBER rows loaded from pre-sessions 2-4:\n  ", nrow(fisber_pre_long), "\n\n")

cat("FISBER open-text records:\n  ",
    text_overall$n_with_any_text, "/", text_overall$n_records,
    " with any text; ",
    text_overall$n_substantive_text, " substantive; ",
    text_overall$n_no_side_effect_text, " no-side-effect/N/A style.\n\n",
    sep = "")

cat("Detected AE/SAE candidate columns:\n  ",
    ifelse(length(ae_cols) > 0, paste(ae_cols, collapse = "; "), "None detected"),
    "\n\n")

cat("Detected discontinuation candidate columns:\n  ",
    ifelse(length(discontinuation_cols) > 0, paste(discontinuation_cols, collapse = "; "), "None detected"),
    "\n\n")

cat("Key outputs:\n")
cat("  - S9a_adverse_event_sae_summary.html\n")
cat("  - S9b_adverse_event_sae_listing.html\n")
cat("  - S9c_fisber_summary_by_arm_session.html\n")
cat("  - S9d_vdq_discomfort_summary_by_arm_session.html\n")
cat("  - S9e_vdq_side_effect_counts_by_participant_session.html\n")
cat("  - S9f_side_effect_symptom_summary_by_arm_session.html\n")
cat("  - S9g_tolerability_related_discontinuation_details.html\n")
cat("  - S9h_column_detection_audit.html\n")
cat("  - S9i_fisber_text_classification_summary.html\n")
cat("  - S9j_fisber_text_topic_summary.html\n")
cat("  - S9k_fisber_text_word_counts.html\n")
cat("  - S9_WP2_safety_tolerability_narrative.txt\n")

if (HAS_GGWORDCLOUD) {
  cat("  - S9k_fisber_text_wordcloud.png/pdf\n")
} else {
  cat("  - Word cloud skipped because package 'ggwordcloud' is not installed.\n")
}

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

# ===================================================

#####################################################
#####################################################
#####################################################
#####################################################
#####################################################

# ============================================================
# SUPPLEMENTARY S9 — WP2 SAFETY / TOLERABILITY TABLE PACK
# FULL CLEANED SIDE-EFFECT VERSION
# ============================================================
#
# This is a full end-to-end replacement script.
# You do NOT need to paste this into the old script.
#
# Main purpose:
#   - Re-load WP2 assignment, post-session, and pre-session/FISBER files
#   - Correctly clean side-effect endorsements
#   - Preserve raw endorsement audit values
#   - Produce manuscript-facing cleaned negative side-effect tables
#
# Key cleaned variables:
#   1. "Any side-effect field endorsed before text cleaning"
#      = deliberately broad audit field
#
#   2. "Cleaned negative side-effect endorsement"
#      = manuscript-facing side-effect endorsement after cleaning:
#          - excludes "Not at all"
#          - excludes "None of the time"
#          - excludes "no side effects", "none", "N/A"
#          - excludes positive/benefit comments like "happier", "felt better"
#          - includes substantive negative side-effect text
#
# FISBER mapping:
#   pre-session 2 -> Session 1 follow-up
#   pre-session 3 -> Session 2 follow-up
#   pre-session 4 -> Session 3 follow-up
#   Session 4 has no later pre-session FISBER follow-up
#
# Table ordering:
#   Intervention first, Control second
#
# Outputs:
#   supplementary_S9_wp2_safety_tolerability_cleaned/
#     - S9_cleaned_side_effect_summary_by_arm_session.csv/html/png/pdf
#     - S9d_vdq_discomfort_summary_by_arm_session_CLEANED.csv/html/png/pdf
#     - S9c_fisber_summary_by_arm_session_CLEANED.csv/html/png/pdf
#     - S9e_cleaned_side_effect_counts_by_participant_session.csv/html
#     - S9j_fisber_text_topic_summary_CLEANED.csv/html
#     - S9h_cleaned_column_detection_audit.csv/html
#     - S9_cleaned_side_effect_logic_narrative.txt
#
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================

needed_packages <- c("tidyverse", "gt")

for (pkg in needed_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(tidyverse)
library(gt)


# ============================================================
# 1. SETUP
# ============================================================

DATA_DIR <- if (exists("DATA_DIR")) {
  DATA_DIR
} else {
  "C:/Users/dn284/Desktop/MRC_omni/data"
}

SEARCH_DIRS <- if (exists("SEARCH_DIRS")) {
  SEARCH_DIRS
} else {
  c(DATA_DIR)
}

PALATINO_NAME <- if (exists("PALATINO_NAME")) {
  PALATINO_NAME
} else {
  "serif"
}

OUT_DIR <- file.path(DATA_DIR, "supplementary_S9_wp2_safety_tolerability_cleaned")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

Z95 <- 1.96


# ============================================================
# 2. GENERAL HELPERS
# ============================================================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

s9_fmt_num <- function(x, digits = 2) {
  ifelse(
    is.na(x),
    "NA",
    formatC(as.numeric(x), format = "f", digits = digits)
  )
}

s9_fmt_pct <- function(x, digits = 1) {
  ifelse(
    is.na(x),
    "NA",
    paste0(formatC(as.numeric(x), format = "f", digits = digits), "%")
  )
}

s9_fmt_ci <- function(lo, hi, digits = 2) {
  paste0("[", s9_fmt_num(lo, digits), ", ", s9_fmt_num(hi, digits), "]")
}

s9_arm_factor <- function(x) {
  factor(as.character(x), levels = c("Intervention", "Control"))
}

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
      
      matched <- all_files[
        str_detect(file_names, regex(pat_regex, ignore_case = TRUE))
      ]
      
      hits <- c(hits, matched)
    }
  }
  
  hits <- unique(hits[file.exists(hits)])
  
  if (length(hits) == 0) {
    if (required) {
      stop("No files found for patterns: ", paste(patterns, collapse = ", "))
    }
    return(NULL)
  }
  
  info <- file.info(hits)
  hits[order(info$mtime, decreasing = TRUE)][1]
}

read_qualtrics_real <- function(path) {
  df <- read_csv(
    path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
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
    na_if('{"ImportId":"part_id"}') %>%
    str_extract("\\d{3,6}")
}

to_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

standardise_condition <- function(x) {
  x_chr <- str_to_lower(str_squish(as.character(x)))
  
  case_when(
    str_detect(x_chr, "control|placebo|sham") ~ "Control",
    str_detect(x_chr, "intervention|active|treatment|sls") ~ "Intervention",
    x_chr %in% c("0", "c", "ctrl") ~ "Control",
    x_chr %in% c("1", "i", "int", "active") ~ "Intervention",
    TRUE ~ NA_character_
  )
}

find_col <- function(df, candidates, required = FALSE) {
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
  
  if (required) {
    stop("Could not find candidate column: ", paste(candidates, collapse = ", "))
  }
  
  NULL
}

find_cols_regex <- function(df, patterns) {
  nms <- names(df)
  hits <- character(0)
  
  for (pat in patterns) {
    hits <- c(hits, nms[str_detect(nms, regex(pat, ignore_case = TRUE))])
  }
  
  unique(hits)
}

save_gt <- function(gt_obj, stem, png = TRUE, pdf = TRUE) {
  html_path <- file.path(OUT_DIR, paste0(stem, ".html"))
  gtsave(gt_obj, html_path)
  
  if (png) {
    try(
      gtsave(
        gt_obj,
        file.path(OUT_DIR, paste0(stem, ".png")),
        vwidth = 2400,
        vheight = 1500,
        zoom = 2
      ),
      silent = TRUE
    )
  }
  
  if (pdf) {
    try(
      gtsave(
        gt_obj,
        file.path(OUT_DIR, paste0(stem, ".pdf")),
        vwidth = 2400,
        vheight = 1500,
        zoom = 2
      ),
      silent = TRUE
    )
  }
  
  invisible(html_path)
}

style_s9_gt <- function(tbl, title, subtitle = NULL) {
  tbl %>%
    tab_header(
      title = md(paste0("**", title, "**")),
      subtitle = subtitle
    ) %>%
    tab_options(
      table.font.names = PALATINO_NAME,
      table.font.size = px(12),
      heading.title.font.size = px(16),
      heading.subtitle.font.size = px(11),
      column_labels.font.weight = "bold",
      column_labels.background.color = "#F3F6FA",
      table.border.top.color = "#222222",
      table.border.bottom.color = "#222222",
      data_row.padding = px(4),
      source_notes.font.size = px(10)
    ) %>%
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels(columns = everything())
    )
}


# ============================================================
# 3. CLEANED SIDE-EFFECT TEXT / ORDINAL HELPERS
# ============================================================

clean_text2 <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\\r|\\n|\\t", " ") %>%
    str_squish()
}

is_emptyish_text <- function(x) {
  x0 <- clean_text2(x)
  x1 <- str_to_lower(x0)
  
  is.na(x1) |
    x1 == "" |
    x1 %in% c(
      "na", "n/a", "n.a", "n.a.", "none", "no", "n",
      "-", ".", "..", "...", "nil", "nothing", "not applicable",
      "not applicable!!!", "nope", "nan", "none.", "no.", "n/a."
    ) |
    str_detect(x1, "^n\\s*/?\\s*a[!\\. ]*$") |
    str_detect(x1, "^no side effects?$") |
    str_detect(x1, "^no side effect$") |
    str_detect(x1, "^no side-effects?$") |
    str_detect(x1, "^none experienced$") |
    str_detect(x1, "^nothing to report$") |
    str_detect(x1, "^nothing really$") |
    str_detect(x1, "^nothing noticeable$") |
    str_detect(x1, "^not that i noticed$") |
    str_detect(x1, "^not that i can recall$") |
    str_detect(x1, "^not sure$")
}

is_no_side_effect_text2 <- function(x) {
  x0 <- clean_text2(x)
  x1 <- str_to_lower(x0)
  
  is_emptyish_text(x1) |
    str_detect(x1, "no side effects?") |
    str_detect(x1, "no negative side effects?") |
    str_detect(x1, "did not experience.*side effects?") |
    str_detect(x1, "didn't experience.*side effects?") |
    str_detect(x1, "did not feel.*side effects?") |
    str_detect(x1, "didn't feel.*side effects?") |
    str_detect(x1, "do not think.*side effects?") |
    str_detect(x1, "don't think.*side effects?") |
    str_detect(x1, "dont think.*side effects?") |
    str_detect(x1, "not.*side effects?") |
    str_detect(x1, "none.*side effects?") |
    str_detect(x1, "nothing.*side effects?") |
    str_detect(x1, "no.*symptoms?") |
    str_detect(x1, "nothing.*symptoms?")
}

is_positive_or_benefit_text <- function(x) {
  x0 <- clean_text2(x)
  x1 <- str_to_lower(x0)
  
  str_detect(
    x1,
    paste(
      c(
        "happier",
        "happy",
        "felt better",
        "feel better",
        "feeling better",
        "better mood",
        "improved mood",
        "mood improved",
        "more relaxed",
        "relaxed",
        "calmer",
        "calm",
        "good",
        "positive",
        "energised",
        "energized",
        "more energy",
        "slept better",
        "sleep better",
        "less anxious",
        "less depressed",
        "enjoyed",
        "pleasant",
        "benefit",
        "beneficial",
        "fine",
        "felt fine",
        "felt good",
        "great"
      ),
      collapse = "|"
    )
  )
}

is_negative_side_effect_text <- function(x) {
  x0 <- clean_text2(x)
  x1 <- str_to_lower(x0)
  
  !is_no_side_effect_text2(x1) &
    !is_positive_or_benefit_text(x1) &
    str_detect(
      x1,
      paste(
        c(
          "headache",
          "head ache",
          "migraine",
          "nausea",
          "nauseous",
          "sick",
          "dizzy",
          "dizziness",
          "vertigo",
          "tired",
          "fatigue",
          "fatigued",
          "sleepy",
          "drows",
          "insomnia",
          "anxious",
          "anxiety",
          "panic",
          "sad",
          "low mood",
          "irritab",
          "agitat",
          "uncomfortable",
          "discomfort",
          "pain",
          "pressure",
          "chest",
          "heart",
          "palpitation",
          "breathless",
          "ringing",
          "tinnitus",
          "ear",
          "eye",
          "eyes",
          "twitch",
          "flicker",
          "visual",
          "afterimage",
          "after-image",
          "patterns",
          "blur",
          "nervous",
          "unwell",
          "worse",
          "negative",
          "distress",
          "distressed",
          "scary",
          "scared",
          "fear",
          "fright",
          "overwhelming",
          "overstimulated"
        ),
        collapse = "|"
      )
    )
}

vdq_ordinal_to_num <- function(x) {
  x0 <- clean_text2(x)
  x1 <- str_to_lower(x0)
  
  case_when(
    is.na(x1) | x1 == "" ~ NA_real_,
    
    x1 %in% c(
      "not at all", "none", "no", "0", "0.0", "false", "n",
      "na", "n/a", "nan", "not applicable"
    ) ~ 0,
    
    str_detect(x1, "not at all") ~ 0,
    str_detect(x1, "none of the time") ~ 0,
    str_detect(x1, "no side effects?") ~ 0,
    str_detect(x1, "no symptoms?") ~ 0,
    str_detect(x1, "no interference") ~ 0,
    
    str_detect(x1, "a little") ~ 1,
    str_detect(x1, "slight") ~ 1,
    str_detect(x1, "minimal") ~ 1,
    str_detect(x1, "mild") ~ 2,
    str_detect(x1, "moderate") | str_detect(x1, "moderately") ~ 4,
    str_detect(x1, "a lot") | str_detect(x1, "marked") ~ 7,
    str_detect(x1, "extreme") | str_detect(x1, "severe") ~ 10,
    
    !is.na(suppressWarnings(as.numeric(x1))) ~ suppressWarnings(as.numeric(x1)),
    
    TRUE ~ NA_real_
  )
}

fisber_to_num <- function(x) {
  x0 <- clean_text2(x)
  x1 <- str_to_lower(x0)
  
  trailing <- suppressWarnings(as.numeric(str_extract(x1, "\\d+(?=\\s*$)")))
  
  case_when(
    is.na(x1) | x1 == "" ~ NA_real_,
    
    str_detect(x1, "no side effects?") ~ 0,
    str_detect(x1, "none of the time") ~ 0,
    str_detect(x1, "no interference") ~ 0,
    str_detect(x1, "not at all") ~ 0,
    
    !is.na(trailing) ~ trailing,
    !is.na(suppressWarnings(as.numeric(x1))) ~ suppressWarnings(as.numeric(x1)),
    
    TRUE ~ NA_real_
  )
}

raw_any_nonempty_or_yes <- function(df, cols) {
  if (length(cols) == 0) return(rep(FALSE, nrow(df)))
  
  apply(
    df %>% select(all_of(cols)),
    1,
    function(z) {
      z0 <- clean_text2(z)
      z1 <- str_to_lower(z0)
      
      any(
        !is.na(z1) &
          z1 != "" &
          !z1 %in% c(
            "0", "0.0", "no", "none", "false", "n",
            "na", "n/a", "nan", "not applicable"
          ),
        na.rm = TRUE
      )
    }
  )
}


# ============================================================
# 4. LOCATE FILES
# ============================================================

WP2_ASSIGN_PATH <- newest_match(c("*wp2_assignments*.csv"))

WP2_PRE24_PATH <- newest_match(
  c(
    "*wp2_pre_sessions_2-4*.csv",
    "*wp2_pre_sessions_2_4*.csv",
    "*pre_sessions_2-4*.csv"
  ),
  required = FALSE
)

WP2_POST13_PATH <- newest_match(
  c(
    "*wp2_post_sessions_1-3*.csv",
    "*wp2_post_session_1_3*.csv",
    "*wp2_post_session_13*.csv",
    "*wp2_post*session*1*3*.csv"
  ),
  required = FALSE
)

WP2_POST4_PATH <- newest_match(
  c("*wp2_post_session_4*.csv"),
  required = FALSE
)

if (is.null(WP2_POST13_PATH) && is.null(WP2_POST4_PATH)) {
  stop("No WP2 post-session files found. Need post-session 1-3 and/or post-session 4 files.")
}

message("WP2 assignments: ", basename(WP2_ASSIGN_PATH))
message("WP2 pre S2-4:    ", ifelse(is.null(WP2_PRE24_PATH), "not found", basename(WP2_PRE24_PATH)))
message("WP2 post S1-3:   ", ifelse(is.null(WP2_POST13_PATH), "not found", basename(WP2_POST13_PATH)))
message("WP2 post S4:     ", ifelse(is.null(WP2_POST4_PATH), "not found", basename(WP2_POST4_PATH)))


# ============================================================
# 5. LOAD ASSIGNMENTS
# ============================================================

wp2_assign_raw <- read_qualtrics_real(WP2_ASSIGN_PATH)

cond_col <- find_col(
  wp2_assign_raw,
  c(
    "condition",
    "allocation",
    "group",
    "arm",
    "assigned_condition",
    "randomised_condition",
    "randomized_condition",
    "treatment"
  ),
  required = TRUE
)

pid_assign_col <- find_col(
  wp2_assign_raw,
  c("part_id", "participant_id", "id"),
  required = TRUE
)

wp2_assign <- wp2_assign_raw %>%
  mutate(
    part_id = clean_id(.data[[pid_assign_col]]),
    condition = standardise_condition(.data[[cond_col]])
  ) %>%
  filter(!is.na(part_id), !is.na(condition)) %>%
  distinct(part_id, .keep_all = TRUE) %>%
  select(part_id, condition) %>%
  mutate(condition = s9_arm_factor(condition)) %>%
  arrange(condition, part_id)

arm_denoms <- wp2_assign %>%
  count(condition, name = "n_randomised") %>%
  complete(
    condition = factor(c("Intervention", "Control"), levels = c("Intervention", "Control")),
    fill = list(n_randomised = 0)
  ) %>%
  arrange(condition)

write_csv(
  arm_denoms,
  file.path(OUT_DIR, "S9_arm_denominators.csv")
)


# ============================================================
# 6. LOAD POST-SESSION DATA
# ============================================================

load_post_file <- function(path, forced_session = NULL) {
  if (is.null(path) || !file.exists(path)) return(tibble())
  
  df <- read_qualtrics_real(path)
  
  pid_col <- find_col(df, c("part_id", "participant_id", "id"), required = TRUE)
  sess_col <- find_col(df, c("session_n", "session", "session_number", "sess"), required = FALSE)
  
  df %>%
    mutate(
      source_file = basename(path),
      part_id = clean_id(.data[[pid_col]]),
      session_n = if (!is.null(forced_session)) {
        forced_session
      } else if (!is.null(sess_col)) {
        to_num(.data[[sess_col]])
      } else {
        NA_real_
      }
    ) %>%
    filter(!is.na(part_id)) %>%
    mutate(session_n = as.integer(session_n))
}

post13 <- load_post_file(WP2_POST13_PATH)
post4  <- load_post_file(WP2_POST4_PATH, forced_session = 4)

wp2_post_raw <- bind_rows(post13, post4) %>%
  filter(!is.na(part_id), session_n %in% 1:4) %>%
  left_join(
    wp2_assign %>% mutate(condition = as.character(condition)),
    by = "part_id"
  ) %>%
  mutate(condition = s9_arm_factor(condition))

if (nrow(wp2_post_raw) == 0) {
  stop("Post-session files loaded, but no usable participant/session rows were found.")
}


# ============================================================
# 7. LOAD PRE-SESSIONS 2–4 / FISBER
# ============================================================

load_pre24_fisber <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(tibble(
      part_id = character(),
      condition = character(),
      session_n = integer(),
      fisber_frequency = numeric(),
      fisber_severity = numeric(),
      fisber_interference = numeric(),
      fisber_text = character(),
      source_file = character()
    ))
  }
  
  df <- read_qualtrics_real(path)
  
  pid_col <- find_col(df, c("part_id", "participant_id", "id"), required = TRUE)
  sess_col <- find_col(df, c("session_n", "session", "session_number", "sess"), required = FALSE)
  
  f1 <- find_col(df, c("fisber_1", "fibser_1", "fisber1", "fibser1"), required = FALSE)
  f2 <- find_col(df, c("fisber_2", "fibser_2", "fisber2", "fibser2"), required = FALSE)
  f3 <- find_col(df, c("fisber_3", "fibser_3", "fisber3", "fibser3"), required = FALSE)
  ft <- find_col(df, c("fisber_text", "fibser_text", "fisber_text_1", "fibser_text_1"), required = FALSE)
  
  if (is.null(f1) && is.null(f2) && is.null(f3) && is.null(ft)) {
    warning("No fisber/fibser columns found in ", basename(path))
  }
  
  out <- df %>%
    mutate(
      source_file = basename(path),
      part_id = clean_id(.data[[pid_col]]),
      session_n = if (!is.null(sess_col)) to_num(.data[[sess_col]]) else NA_real_,
      fisber_frequency = if (!is.null(f1)) fisber_to_num(.data[[f1]]) else NA_real_,
      fisber_severity = if (!is.null(f2)) fisber_to_num(.data[[f2]]) else NA_real_,
      fisber_interference = if (!is.null(f3)) fisber_to_num(.data[[f3]]) else NA_real_,
      fisber_text = if (!is.null(ft)) clean_text2(.data[[ft]]) else NA_character_
    ) %>%
    filter(!is.na(part_id), !is.na(session_n)) %>%
    mutate(session_n = as.integer(session_n)) %>%
    filter(session_n %in% 2:4) %>%
    select(
      part_id,
      session_n,
      fisber_frequency,
      fisber_severity,
      fisber_interference,
      fisber_text,
      source_file
    ) %>%
    left_join(
      wp2_assign %>% mutate(condition = as.character(condition)),
      by = "part_id"
    ) %>%
    mutate(condition = s9_arm_factor(condition))
  
  out
}

fisber_pre_long <- load_pre24_fisber(WP2_PRE24_PATH)

write_csv(
  fisber_pre_long,
  file.path(OUT_DIR, "S9_fisber_pre_sessions_2_4_long.csv")
)


# ============================================================
# 8. DETECT POST-SESSION SIDE-EFFECT / VDQ COLUMNS
# ============================================================

tol_score_col <- find_col(
  wp2_post_raw,
  c(
    "tol_score",
    "discomfortScore",
    "discomfort_score",
    "vdq",
    "vdq_score",
    "vdq_total",
    "tolerability_score"
  ),
  required = FALSE
)

post_vdq_cols <- names(wp2_post_raw)[
  str_detect(names(wp2_post_raw), regex("^tol_vdq_\\d+$", ignore_case = TRUE))
]

post_vdq_text_cols <- names(wp2_post_raw)[
  str_detect(names(wp2_post_raw), regex("^tol_vdq_.*text$", ignore_case = TRUE))
]

post_follow_cols <- names(wp2_post_raw)[
  str_detect(names(wp2_post_raw), regex("^tol_follow_\\d+$", ignore_case = TRUE))
]

post_follow_text_cols <- names(wp2_post_raw)[
  str_detect(names(wp2_post_raw), regex("^tol_follow_.*text$", ignore_case = TRUE))
]

post_gate_cols <- names(wp2_post_raw)[
  str_detect(names(wp2_post_raw), regex("^tol_sum$|any.*side|side.*effect", ignore_case = TRUE))
]

post_audit_cols <- unique(c(
  post_gate_cols,
  post_vdq_cols,
  post_vdq_text_cols,
  post_follow_cols,
  post_follow_text_cols
))


# ============================================================
# 9. IMMEDIATE POST-SESSION CLEANED ENDORSEMENT
# ============================================================

compute_post_clean <- function(df) {
  if (nrow(df) == 0) return(tibble())
  
  vdq_mat <- if (length(post_vdq_cols) > 0) {
    df %>%
      select(all_of(post_vdq_cols)) %>%
      mutate(across(everything(), vdq_ordinal_to_num))
  } else {
    tibble(.rows = nrow(df))
  }
  
  follow_mat <- if (length(post_follow_cols) > 0) {
    df %>%
      select(all_of(post_follow_cols)) %>%
      mutate(across(everything(), vdq_ordinal_to_num))
  } else {
    tibble(.rows = nrow(df))
  }
  
  text_mat <- if (length(c(post_vdq_text_cols, post_follow_text_cols)) > 0) {
    df %>% select(all_of(c(post_vdq_text_cols, post_follow_text_cols)))
  } else {
    tibble(.rows = nrow(df))
  }
  
  vdq_max <- if (ncol(vdq_mat) > 0) {
    apply(vdq_mat, 1, function(z) {
      if (all(is.na(z))) NA_real_ else max(z, na.rm = TRUE)
    })
  } else {
    rep(NA_real_, nrow(df))
  }
  
  follow_max <- if (ncol(follow_mat) > 0) {
    apply(follow_mat, 1, function(z) {
      if (all(is.na(z))) NA_real_ else max(z, na.rm = TRUE)
    })
  } else {
    rep(NA_real_, nrow(df))
  }
  
  text_negative <- if (ncol(text_mat) > 0) {
    apply(text_mat, 1, function(z) {
      any(is_negative_side_effect_text(z), na.rm = TRUE)
    })
  } else {
    rep(FALSE, nrow(df))
  }
  
  text_no_side_effect <- if (ncol(text_mat) > 0) {
    apply(text_mat, 1, function(z) {
      z0 <- clean_text2(z)
      any(!is.na(z0) & z0 != "" & is_no_side_effect_text2(z0), na.rm = TRUE)
    })
  } else {
    rep(FALSE, nrow(df))
  }
  
  text_positive <- if (ncol(text_mat) > 0) {
    apply(text_mat, 1, function(z) {
      z0 <- clean_text2(z)
      any(!is.na(z0) & z0 != "" & is_positive_or_benefit_text(z0), na.rm = TRUE)
    })
  } else {
    rep(FALSE, nrow(df))
  }
  
  raw_any <- raw_any_nonempty_or_yes(df, post_audit_cols)
  
  explicit_score <- if (!is.null(tol_score_col) && tol_score_col %in% names(df)) {
    vdq_ordinal_to_num(df[[tol_score_col]])
  } else {
    rep(NA_real_, nrow(df))
  }
  
  cleaned_score <- pmax(explicit_score, vdq_max, follow_max, na.rm = TRUE)
  cleaned_score[is.infinite(cleaned_score)] <- NA_real_
  
  if ("tol_sum" %in% names(df)) {
    no_tol_sum <- str_to_lower(clean_text2(df$tol_sum)) %in% c("no", "0", "false", "n")
    cleaned_score[no_tol_sum & is.na(cleaned_score)] <- 0
  }
  
  cleaned_any <- (!is.na(cleaned_score) & cleaned_score > 0) | text_negative
  
  tibble(
    post_raw_any_before_cleaning = raw_any,
    post_cleaned_vdq_score = cleaned_score,
    post_cleaned_negative_side_effect = cleaned_any,
    post_no_side_effect_text_detected = text_no_side_effect,
    post_positive_text_detected = text_positive,
    post_negative_text_detected = text_negative
  )
}

post_cleaned_vars <- compute_post_clean(wp2_post_raw)

wp2_tol_long <- bind_cols(wp2_post_raw, post_cleaned_vars) %>%
  mutate(
    vdq_score = post_cleaned_vdq_score,
    any_side_effect_raw_before_cleaning = post_raw_any_before_cleaning,
    any_side_effect = post_cleaned_negative_side_effect,
    condition = s9_arm_factor(condition)
  ) %>%
  filter(!is.na(condition), !is.na(session_n), session_n %in% 1:4) %>%
  arrange(condition, session_n, part_id)

write_csv(
  wp2_tol_long,
  file.path(OUT_DIR, "S9_wp2_post_session_safety_long_CLEANED.csv")
)


# ============================================================
# 10. FISBER / WEEKLY FOLLOW-UP CLEANED ENDORSEMENT
# ============================================================

fisber_text_long <- fisber_pre_long %>%
  mutate(
    target_session_n = as.integer(session_n) - 1L,
    
    fisber_frequency_clean = fisber_frequency,
    fisber_severity_clean = fisber_severity,
    fisber_interference_clean = fisber_interference,
    
    fisber_text_clean = clean_text2(fisber_text),
    fisber_text_missing_or_empty = is.na(fisber_text_clean) | fisber_text_clean == "",
    fisber_text_no_side_effect = is_no_side_effect_text2(fisber_text_clean),
    fisber_text_positive_or_benefit = is_positive_or_benefit_text(fisber_text_clean),
    fisber_text_negative = is_negative_side_effect_text(fisber_text_clean),
    
    fisber_any_numeric_raw =
      coalesce(fisber_frequency_clean, 0) > 0 |
      coalesce(fisber_severity_clean, 0) > 0 |
      coalesce(fisber_interference_clean, 0) > 0,
    
    fisber_raw_any_before_text_cleaning =
      fisber_any_numeric_raw |
      (!fisber_text_missing_or_empty & !fisber_text_no_side_effect),
    
    fisber_cleaned_negative_side_effect =
      (
        fisber_any_numeric_raw &
          !fisber_text_no_side_effect &
          !fisber_text_positive_or_benefit
      ) |
      fisber_text_negative,
    
    fisber_cleaning_flag = case_when(
      fisber_any_numeric_raw & fisber_text_no_side_effect ~
        "Numeric endorsement overridden by no-side-effect/N/A text",
      fisber_any_numeric_raw & fisber_text_positive_or_benefit ~
        "Numeric endorsement overridden by positive/benefit text",
      !fisber_any_numeric_raw & fisber_text_negative ~
        "Negative substantive text without numeric FISBER endorsement",
      fisber_cleaned_negative_side_effect ~
        "Cleaned negative side-effect endorsement",
      TRUE ~
        "No cleaned negative side-effect endorsement"
    ),
    
    condition = s9_arm_factor(condition)
  ) %>%
  filter(target_session_n %in% 1:3) %>%
  arrange(condition, target_session_n, part_id)

write_csv(
  fisber_text_long,
  file.path(OUT_DIR, "S9_fisber_followup_long_CLEANED.csv")
)


# ============================================================
# 11. SESSION 1–4 PARTICIPANT-SESSION DATASET
# ============================================================

post_session_level <- wp2_tol_long %>%
  group_by(part_id, condition, session_n) %>%
  summarise(
    post_available = TRUE,
    post_raw_any_before_cleaning = any(any_side_effect_raw_before_cleaning, na.rm = TRUE),
    post_cleaned_negative_side_effect = any(post_cleaned_negative_side_effect, na.rm = TRUE),
    post_cleaned_vdq_score = ifelse(
      all(is.na(post_cleaned_vdq_score)),
      NA_real_,
      max(post_cleaned_vdq_score, na.rm = TRUE)
    ),
    post_negative_text_detected = any(post_negative_text_detected, na.rm = TRUE),
    post_no_side_effect_text_detected = any(post_no_side_effect_text_detected, na.rm = TRUE),
    post_positive_text_detected = any(post_positive_text_detected, na.rm = TRUE),
    .groups = "drop"
  )

fisber_session_level <- fisber_text_long %>%
  group_by(part_id, condition, target_session_n) %>%
  summarise(
    followup_available = TRUE,
    followup_raw_any_before_cleaning = any(fisber_raw_any_before_text_cleaning, na.rm = TRUE),
    followup_cleaned_negative_side_effect = any(fisber_cleaned_negative_side_effect, na.rm = TRUE),
    followup_numeric_raw = any(fisber_any_numeric_raw, na.rm = TRUE),
    followup_no_side_effect_text = any(fisber_text_no_side_effect, na.rm = TRUE),
    followup_positive_or_benefit_text = any(fisber_text_positive_or_benefit, na.rm = TRUE),
    followup_negative_text = any(fisber_text_negative, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(session_n = target_session_n)

all_participant_sessions <- wp2_assign %>%
  tidyr::crossing(session_n = 1:4) %>%
  mutate(condition = s9_arm_factor(condition))

session_cleaned_long <- all_participant_sessions %>%
  left_join(post_session_level, by = c("part_id", "condition", "session_n")) %>%
  left_join(fisber_session_level, by = c("part_id", "condition", "session_n")) %>%
  mutate(
    across(
      c(
        post_available,
        post_raw_any_before_cleaning,
        post_cleaned_negative_side_effect,
        post_negative_text_detected,
        post_no_side_effect_text_detected,
        post_positive_text_detected,
        followup_available,
        followup_raw_any_before_cleaning,
        followup_cleaned_negative_side_effect,
        followup_numeric_raw,
        followup_no_side_effect_text,
        followup_positive_or_benefit_text,
        followup_negative_text
      ),
      ~replace_na(.x, FALSE)
    ),
    
    any_side_effect_field_endorsed_before_text_cleaning =
      post_raw_any_before_cleaning | followup_raw_any_before_cleaning,
    
    cleaned_negative_side_effect_endorsement =
      post_cleaned_negative_side_effect | followup_cleaned_negative_side_effect,
    
    cleaning_removed_endorsement =
      any_side_effect_field_endorsed_before_text_cleaning &
      !cleaned_negative_side_effect_endorsement,
    
    condition = s9_arm_factor(condition)
  ) %>%
  arrange(condition, session_n, part_id)

write_csv(
  session_cleaned_long,
  file.path(OUT_DIR, "S9_cleaned_side_effect_participant_session_long.csv")
)


# ============================================================
# 12. MAIN CLEANED ARM × SESSION TABLE
# ============================================================

session_cleaned_summary <- session_cleaned_long %>%
  group_by(condition, session_n) %>%
  summarise(
    n_randomised = n_distinct(part_id),
    n_with_post_data = n_distinct(part_id[post_available]),
    n_with_followup_fisber = n_distinct(part_id[followup_available]),
    
    n_any_field_before_cleaning =
      n_distinct(part_id[any_side_effect_field_endorsed_before_text_cleaning]),
    pct_any_field_before_cleaning =
      100 * n_any_field_before_cleaning / n_randomised,
    
    n_post_cleaned_negative =
      n_distinct(part_id[post_cleaned_negative_side_effect]),
    pct_post_cleaned_negative =
      100 * n_post_cleaned_negative / n_randomised,
    
    n_followup_cleaned_negative =
      n_distinct(part_id[followup_cleaned_negative_side_effect]),
    pct_followup_cleaned_negative =
      100 * n_followup_cleaned_negative / n_randomised,
    
    n_combined_cleaned_negative =
      n_distinct(part_id[cleaned_negative_side_effect_endorsement]),
    pct_combined_cleaned_negative =
      100 * n_combined_cleaned_negative / n_randomised,
    
    n_removed_by_cleaning =
      n_distinct(part_id[cleaning_removed_endorsement]),
    pct_removed_by_cleaning =
      100 * n_removed_by_cleaning / n_randomised,
    
    mean_post_vdq = ifelse(
      all(is.na(post_cleaned_vdq_score[post_available])),
      NA_real_,
      mean(post_cleaned_vdq_score[post_available], na.rm = TRUE)
    ),
    sd_post_vdq = ifelse(
      all(is.na(post_cleaned_vdq_score[post_available])),
      NA_real_,
      sd(post_cleaned_vdq_score[post_available], na.rm = TRUE)
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    condition = s9_arm_factor(condition),
    session_label = paste0("Session ", session_n),
    
    raw_display = paste0(
      n_any_field_before_cleaning, "/", n_randomised,
      " (", s9_fmt_pct(pct_any_field_before_cleaning), ")"
    ),
    
    post_cleaned_display = paste0(
      n_post_cleaned_negative, "/", n_randomised,
      " (", s9_fmt_pct(pct_post_cleaned_negative), ")"
    ),
    
    followup_cleaned_display = if_else(
      session_n == 4,
      "N/A — no later pre-session follow-up",
      paste0(
        n_followup_cleaned_negative, "/", n_randomised,
        " (", s9_fmt_pct(pct_followup_cleaned_negative), ")"
      )
    ),
    
    combined_cleaned_display = paste0(
      n_combined_cleaned_negative, "/", n_randomised,
      " (", s9_fmt_pct(pct_combined_cleaned_negative), ")"
    ),
    
    removed_display = paste0(
      n_removed_by_cleaning, "/", n_randomised,
      " (", s9_fmt_pct(pct_removed_by_cleaning), ")"
    ),
    
    mean_sd_post_vdq = paste0(
      s9_fmt_num(mean_post_vdq),
      " ± ",
      s9_fmt_num(sd_post_vdq)
    )
  ) %>%
  arrange(condition, session_n)

write_csv(
  session_cleaned_summary,
  file.path(OUT_DIR, "S9_cleaned_side_effect_summary_by_arm_session.csv")
)

tbl_cleaned_session_summary <- session_cleaned_summary %>%
  select(
    Arm = condition,
    Session = session_label,
    `Randomised denominator` = n_randomised,
    `Post-session data n` = n_with_post_data,
    `Follow-up FISBER n` = n_with_followup_fisber,
    `Any side-effect field endorsed before text cleaning` = raw_display,
    `Immediate post-session cleaned negative side-effect` = post_cleaned_display,
    `Weekly follow-up cleaned negative side-effect` = followup_cleaned_display,
    `Combined cleaned negative side-effect endorsement` = combined_cleaned_display,
    `Endorsements removed by text/ordinal cleaning` = removed_display,
    `Post-session VDQ/discomfort mean ± SD` = mean_sd_post_vdq
  ) %>%
  gt(groupname_col = "Arm") %>%
  style_s9_gt(
    title = "Table S9. Cleaned WP2 side-effect endorsement by arm and session",
    subtitle = md(
      paste0(
        "Raw endorsement preserves any side-effect-like field before text cleaning. ",
        "Cleaned endorsement requires non-zero VDQ/FISBER severity or substantive negative side-effect text; ",
        "responses such as `Not at all`, `no side effects`, `none`, `N/A`, and positive/benefit text are not counted as negative side effects."
      )
    )
  )

save_gt(
  tbl_cleaned_session_summary,
  "S9_cleaned_side_effect_summary_by_arm_session"
)


# ============================================================
# 13. POST-SESSION VDQ / DISCOMFORT TABLE
# ============================================================

discomfort_summary <- wp2_tol_long %>%
  filter(!is.na(vdq_score)) %>%
  mutate(condition = s9_arm_factor(condition)) %>%
  group_by(condition, session_n) %>%
  summarise(
    n = n_distinct(part_id),
    mean = mean(vdq_score, na.rm = TRUE),
    sd = sd(vdq_score, na.rm = TRUE),
    se = sd / sqrt(n),
    ci95_low = mean - Z95 * se,
    ci95_high = mean + Z95 * se,
    median = median(vdq_score, na.rm = TRUE),
    min = min(vdq_score, na.rm = TRUE),
    max = max(vdq_score, na.rm = TRUE),
    
    n_any_side_effect_raw_before_cleaning =
      n_distinct(part_id[any_side_effect_raw_before_cleaning]),
    percent_any_side_effect_raw_before_cleaning =
      100 * n_any_side_effect_raw_before_cleaning / n,
    
    n_cleaned_negative_side_effect =
      n_distinct(part_id[post_cleaned_negative_side_effect]),
    percent_cleaned_negative_side_effect =
      100 * n_cleaned_negative_side_effect / n,
    
    .groups = "drop"
  ) %>%
  mutate(
    condition = s9_arm_factor(condition),
    session_label = paste0("Session ", session_n),
    mean_sd = paste0(s9_fmt_num(mean), " ± ", s9_fmt_num(sd)),
    ci95 = s9_fmt_ci(ci95_low, ci95_high),
    
    raw_display = paste0(
      n_any_side_effect_raw_before_cleaning, "/", n,
      " (", s9_fmt_pct(percent_any_side_effect_raw_before_cleaning), ")"
    ),
    
    cleaned_display = paste0(
      n_cleaned_negative_side_effect, "/", n,
      " (", s9_fmt_pct(percent_cleaned_negative_side_effect), ")"
    )
  ) %>%
  arrange(condition, session_n)

write_csv(
  discomfort_summary,
  file.path(OUT_DIR, "S9d_vdq_discomfort_summary_by_arm_session_CLEANED.csv")
)

tbl_discomfort <- discomfort_summary %>%
  select(
    Arm = condition,
    Session = session_label,
    n,
    `Mean ± SD` = mean_sd,
    `95% CI` = ci95,
    Median = median,
    Min = min,
    Max = max,
    `Any side-effect field endorsed before text cleaning` = raw_display,
    `Cleaned negative side-effect endorsement` = cleaned_display
  ) %>%
  gt(groupname_col = "Arm") %>%
  style_s9_gt(
    title = "Table S9d. WP2 post-session VDQ/discomfort summary by arm and session",
    subtitle = md(
      "The raw column is retained as an audit field. The cleaned side-effect column excludes `Not at all`, no-side-effect/N/A text, and positive/benefit comments."
    )
  ) %>%
  fmt_number(columns = c(Median, Min, Max), decimals = 2)

save_gt(
  tbl_discomfort,
  "S9d_vdq_discomfort_summary_by_arm_session_CLEANED"
)


# ============================================================
# 14. PARTICIPANT-LEVEL AUDIT TABLE
# ============================================================

participant_session_counts <- session_cleaned_long %>%
  mutate(
    condition = s9_arm_factor(condition),
    session_label = paste0("S", session_n),
    raw_before_cleaning = if_else(
      any_side_effect_field_endorsed_before_text_cleaning,
      "Yes",
      "No"
    ),
    cleaned_negative = if_else(
      cleaned_negative_side_effect_endorsement,
      "Yes",
      "No"
    ),
    removed_by_cleaning = if_else(
      cleaning_removed_endorsement,
      "Yes",
      "No"
    )
  ) %>%
  select(
    part_id,
    condition,
    session_n,
    session_label,
    post_cleaned_vdq_score,
    raw_before_cleaning,
    cleaned_negative,
    removed_by_cleaning,
    post_negative_text_detected,
    followup_negative_text,
    followup_no_side_effect_text,
    followup_positive_or_benefit_text
  ) %>%
  arrange(condition, part_id, session_n)

write_csv(
  participant_session_counts,
  file.path(OUT_DIR, "S9e_cleaned_side_effect_counts_by_participant_session.csv")
)

tbl_participant_counts <- participant_session_counts %>%
  select(
    Arm = condition,
    Participant = part_id,
    Session = session_label,
    `Post-session VDQ/discomfort` = post_cleaned_vdq_score,
    `Any side-effect field endorsed before text cleaning` = raw_before_cleaning,
    `Cleaned negative side-effect endorsement` = cleaned_negative,
    `Removed by cleaning` = removed_by_cleaning
  ) %>%
  gt(groupname_col = "Arm") %>%
  style_s9_gt(
    title = "Table S9e. Participant/session side-effect audit after cleaning",
    subtitle = "Rows show how raw endorsement was retained for auditability while cleaned negative side-effect endorsement was used for the manuscript-facing summary."
  ) %>%
  fmt_number(columns = `Post-session VDQ/discomfort`, decimals = 2)

save_gt(
  tbl_participant_counts,
  "S9e_cleaned_side_effect_counts_by_participant_session",
  png = FALSE,
  pdf = FALSE
)


# ============================================================
# 15. CLEANED FISBER FOLLOW-UP SUMMARY
# ============================================================

fisber_summary_cleaned <- fisber_text_long %>%
  mutate(condition = s9_arm_factor(condition)) %>%
  group_by(condition, target_session_n) %>%
  summarise(
    n = n_distinct(part_id),
    
    n_numeric_raw = n_distinct(part_id[fisber_any_numeric_raw]),
    pct_numeric_raw = 100 * n_numeric_raw / n,
    
    n_no_side_effect_text = n_distinct(part_id[fisber_text_no_side_effect]),
    pct_no_side_effect_text = 100 * n_no_side_effect_text / n,
    
    n_positive_or_benefit_text = n_distinct(part_id[fisber_text_positive_or_benefit]),
    pct_positive_or_benefit_text = 100 * n_positive_or_benefit_text / n,
    
    n_cleaned_negative = n_distinct(part_id[fisber_cleaned_negative_side_effect]),
    pct_cleaned_negative = 100 * n_cleaned_negative / n,
    
    n_overridden_numeric = n_distinct(
      part_id[
        fisber_any_numeric_raw &
          (fisber_text_no_side_effect | fisber_text_positive_or_benefit)
      ]
    ),
    pct_overridden_numeric = 100 * n_overridden_numeric / n,
    
    .groups = "drop"
  ) %>%
  mutate(
    condition = s9_arm_factor(condition),
    session_label = paste0("Session ", target_session_n, " follow-up"),
    
    numeric_raw_display = paste0(
      n_numeric_raw, "/", n,
      " (", s9_fmt_pct(pct_numeric_raw), ")"
    ),
    no_side_effect_text_display = paste0(
      n_no_side_effect_text, "/", n,
      " (", s9_fmt_pct(pct_no_side_effect_text), ")"
    ),
    positive_display = paste0(
      n_positive_or_benefit_text, "/", n,
      " (", s9_fmt_pct(pct_positive_or_benefit_text), ")"
    ),
    cleaned_negative_display = paste0(
      n_cleaned_negative, "/", n,
      " (", s9_fmt_pct(pct_cleaned_negative), ")"
    ),
    overridden_display = paste0(
      n_overridden_numeric, "/", n,
      " (", s9_fmt_pct(pct_overridden_numeric), ")"
    )
  ) %>%
  arrange(condition, target_session_n)

write_csv(
  fisber_summary_cleaned,
  file.path(OUT_DIR, "S9c_fisber_summary_by_arm_session_CLEANED.csv")
)

tbl_fisber <- fisber_summary_cleaned %>%
  select(
    Arm = condition,
    Session = session_label,
    n,
    `Numeric FISBER endorsement before text cleaning` = numeric_raw_display,
    `No-side-effect / N/A text` = no_side_effect_text_display,
    `Positive / benefit text` = positive_display,
    `Numeric endorsements overridden by text cleaning` = overridden_display,
    `Cleaned negative side-effect endorsement` = cleaned_negative_display
  ) %>%
  gt(groupname_col = "Arm") %>%
  style_s9_gt(
    title = "Table S9c. Cleaned FISBER follow-up side-effect summary by arm and session",
    subtitle = md(
      "FISBER follow-up records are mapped back to the preceding treatment session. Numeric endorsements are retained in the audit column but are not counted as negative side effects when paired with no-side-effect/N/A or positive-benefit text."
    )
  )

save_gt(
  tbl_fisber,
  "S9c_fisber_summary_by_arm_session_CLEANED"
)


# ============================================================
# 16. CLEANED FISBER TEXT TOPIC SUMMARY
# ============================================================

classify_cleaned_text_topic <- function(x) {
  x0 <- clean_text2(x)
  x1 <- str_to_lower(x0)
  
  case_when(
    is_no_side_effect_text2(x1) ~ "No side effects / not applicable",
    is_positive_or_benefit_text(x1) ~ "Positive / felt better",
    str_detect(x1, "sleep|sleepy|slept|asleep|tired|fatigue|drows") ~ "Sleepiness / tiredness",
    str_detect(x1, "headache|head ache|migraine") ~ "Headache",
    str_detect(x1, "dizz|vertigo|nausea|sick") ~ "Dizziness / nausea",
    str_detect(x1, "ringing|tinnitus|ear") ~ "Auditory after-effect",
    str_detect(x1, "eye|visual|afterimage|after-image|pattern|blur|flicker|twitch") ~ "Visual/perceptual after-effect",
    str_detect(x1, "sad|mood|anxious|anxiety|panic|irritab|agitat") ~ "Mood or emotional state",
    str_detect(x1, "work|study|studying|focus|unfocused|concentrat|engage") ~ "Work/study interference",
    str_detect(x1, "not sure|may not|might not|unsure|unclear") ~ "Uncertain attribution",
    TRUE ~ "Other substantive comment"
  )
}

fisber_topic_summary <- fisber_text_long %>%
  mutate(
    condition = s9_arm_factor(condition),
    text_topic = classify_cleaned_text_topic(fisber_text_clean)
  ) %>%
  filter(!fisber_text_missing_or_empty) %>%
  group_by(condition, target_session_n, text_topic) %>%
  summarise(
    n = n(),
    n_cleaned_negative = sum(fisber_cleaned_negative_side_effect, na.rm = TRUE),
    example_1 = first(fisber_text_clean[fisber_text_clean != "" & !is.na(fisber_text_clean)]),
    .groups = "drop"
  ) %>%
  group_by(condition, target_session_n) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(
    condition = s9_arm_factor(condition),
    session_label = paste0("Session ", target_session_n, " follow-up"),
    n_percent = paste0(n, " (", s9_fmt_pct(percent), ")")
  ) %>%
  arrange(condition, target_session_n, desc(n))

write_csv(
  fisber_topic_summary,
  file.path(OUT_DIR, "S9j_fisber_text_topic_summary_CLEANED.csv")
)

tbl_topic_summary <- fisber_topic_summary %>%
  select(
    Arm = condition,
    Session = session_label,
    `Text category` = text_topic,
    `n (%)` = n_percent,
    `Cleaned negative endorsements` = n_cleaned_negative,
    `Example response` = example_1
  ) %>%
  gt(groupname_col = "Arm") %>%
  style_s9_gt(
    title = "Table S9j. Cleaned FISBER open-text classification summary",
    subtitle = "No-side-effect/N/A responses and positive/benefit comments are separated from substantive negative side-effect text."
  )

save_gt(
  tbl_topic_summary,
  "S9j_fisber_text_topic_summary_CLEANED",
  png = FALSE,
  pdf = FALSE
)


# ============================================================
# 17. COLUMN-DETECTION AUDIT
# ============================================================

column_audit <- tibble(
  Category = c(
    "Assignments file",
    "Pre-sessions 2–4 file",
    "Post-sessions 1–3 file",
    "Post-session 4 file",
    "VDQ/discomfort score column",
    "Post-session VDQ ordinal columns",
    "Post-session VDQ text columns",
    "Post-session follow-up ordinal columns",
    "Post-session follow-up text columns",
    "Post-session gate/audit columns",
    "FISBER frequency/severity/interference/text",
    "Main output denominator"
  ),
  Detected = c(
    basename(WP2_ASSIGN_PATH),
    ifelse(is.null(WP2_PRE24_PATH), "None detected", basename(WP2_PRE24_PATH)),
    ifelse(is.null(WP2_POST13_PATH), "None detected", basename(WP2_POST13_PATH)),
    ifelse(is.null(WP2_POST4_PATH), "None detected", basename(WP2_POST4_PATH)),
    tol_score_col %||% "None detected; score derived from cleaned VDQ/follow-up columns where possible",
    ifelse(length(post_vdq_cols) > 0, paste(post_vdq_cols, collapse = "; "), "None detected"),
    ifelse(length(post_vdq_text_cols) > 0, paste(post_vdq_text_cols, collapse = "; "), "None detected"),
    ifelse(length(post_follow_cols) > 0, paste(post_follow_cols, collapse = "; "), "None detected"),
    ifelse(length(post_follow_text_cols) > 0, paste(post_follow_text_cols, collapse = "; "), "None detected"),
    ifelse(length(post_gate_cols) > 0, paste(post_gate_cols, collapse = "; "), "None detected"),
    "fisber_1/fibser_1, fisber_2/fibser_2, fisber_3/fibser_3, fisber_text/fibser_text searched",
    "Randomised arm denominator from wp2_assignments"
  )
)

write_csv(
  column_audit,
  file.path(OUT_DIR, "S9h_cleaned_column_detection_audit.csv")
)

tbl_audit <- column_audit %>%
  gt() %>%
  style_s9_gt(
    title = "Table S9h. Cleaned side-effect column-detection audit",
    subtitle = "This table records which files and side-effect columns were detected by the cleaned S9 script."
  )

save_gt(
  tbl_audit,
  "S9h_cleaned_column_detection_audit",
  png = FALSE,
  pdf = FALSE
)


# ============================================================
# 18. CLEANED SIDE-EFFECT NARRATIVE EXPORT
# ============================================================

overall_cleaned <- session_cleaned_long %>%
  summarise(
    n_participants = n_distinct(part_id),
    n_participant_sessions = n(),
    n_raw = sum(any_side_effect_field_endorsed_before_text_cleaning, na.rm = TRUE),
    n_cleaned = sum(cleaned_negative_side_effect_endorsement, na.rm = TRUE),
    n_removed = sum(cleaning_removed_endorsement, na.rm = TRUE),
    pct_raw = 100 * n_raw / n_participant_sessions,
    pct_cleaned = 100 * n_cleaned / n_participant_sessions,
    pct_removed = 100 * n_removed / n_participant_sessions,
    .groups = "drop"
  )

highest_discomfort <- discomfort_summary %>%
  arrange(desc(mean)) %>%
  slice_head(n = 1)

s9_cleaning_text <- c(
  "Supplementary Table S9. Cleaned WP2 safety and tolerability outputs.",
  "====================================================================",
  "",
  paste0("Output directory: ", OUT_DIR),
  "",
  "Cleaned side-effect endorsement logic.",
  "-------------------------------------",
  "",
  paste0(
    "Side-effect summaries distinguished between raw field endorsement and cleaned negative side-effect endorsement. ",
    "The raw audit variable was labelled 'Any side-effect field endorsed before text cleaning' and preserved any side-effect-like field activity prior to interpretive cleaning."
  ),
  "",
  paste0(
    "The cleaned manuscript-facing variable excluded ordinal responses indicating no symptoms, such as 'Not at all' or 'None of the time', ",
    "as well as open-text responses indicating no side effects or non-applicability, such as 'none', 'N/A', or 'no side effects'. ",
    "Positive or benefit-like comments, including feeling happier, calmer, or better, were also not counted as negative side effects."
  ),
  "",
  paste0(
    "Across participant-session records, raw side-effect-like field activity was detected in ",
    overall_cleaned$n_raw, "/", overall_cleaned$n_participant_sessions,
    " records (", s9_fmt_pct(overall_cleaned$pct_raw), "), whereas cleaned negative side-effect endorsement was detected in ",
    overall_cleaned$n_cleaned, "/", overall_cleaned$n_participant_sessions,
    " records (", s9_fmt_pct(overall_cleaned$pct_cleaned), "). ",
    overall_cleaned$n_removed, " records (", s9_fmt_pct(overall_cleaned$pct_removed),
    ") were retained in the audit trail but not counted as cleaned negative side-effect endorsements."
  ),
  "",
  "Post-session discomfort.",
  "-------------------------",
  paste0(
    "The highest observed arm/session mean post-session VDQ/discomfort score was in ",
    as.character(highest_discomfort$condition),
    " at Session ",
    highest_discomfort$session_n,
    ": mean = ",
    s9_fmt_num(highest_discomfort$mean),
    " ± ",
    s9_fmt_num(highest_discomfort$sd),
    "/10, 95% CI ",
    highest_discomfort$ci95,
    "."
  ),
  "",
  "Generated outputs.",
  "------------------",
  "  - S9_cleaned_side_effect_summary_by_arm_session.html/png/pdf/csv",
  "  - S9d_vdq_discomfort_summary_by_arm_session_CLEANED.html/png/pdf/csv",
  "  - S9c_fisber_summary_by_arm_session_CLEANED.html/png/pdf/csv",
  "  - S9e_cleaned_side_effect_counts_by_participant_session.html/csv",
  "  - S9j_fisber_text_topic_summary_CLEANED.html/csv",
  "  - S9h_cleaned_column_detection_audit.html/csv",
  "  - S9_cleaned_side_effect_logic_narrative.txt"
)

writeLines(
  s9_cleaning_text,
  file.path(OUT_DIR, "S9_cleaned_side_effect_logic_narrative.txt")
)


# ============================================================
# 19. CONSOLE SUMMARY
# ============================================================

cat("\n============================================================\n")
cat("SUPPLEMENTARY S9 CLEANED SAFETY/TOLERABILITY EXPORT COMPLETE\n")
cat("============================================================\n")
cat("Output directory:\n", OUT_DIR, "\n\n")

cat("Detected files:\n")
cat("  Assignments:      ", basename(WP2_ASSIGN_PATH), "\n")
cat("  Pre S2-4:         ", ifelse(is.null(WP2_PRE24_PATH), "not found", basename(WP2_PRE24_PATH)), "\n")
cat("  Post S1-3:        ", ifelse(is.null(WP2_POST13_PATH), "not found", basename(WP2_POST13_PATH)), "\n")
cat("  Post S4:          ", ifelse(is.null(WP2_POST4_PATH), "not found", basename(WP2_POST4_PATH)), "\n\n")

cat("Detected post-session columns:\n")
cat("  tol_score column: ", tol_score_col %||% "None detected", "\n")
cat("  VDQ ordinal:      ", ifelse(length(post_vdq_cols) > 0, paste(post_vdq_cols, collapse = "; "), "None"), "\n")
cat("  VDQ text:         ", ifelse(length(post_vdq_text_cols) > 0, paste(post_vdq_text_cols, collapse = "; "), "None"), "\n")
cat("  Follow-up ordinal:", ifelse(length(post_follow_cols) > 0, paste(post_follow_cols, collapse = "; "), "None"), "\n")
cat("  Follow-up text:   ", ifelse(length(post_follow_text_cols) > 0, paste(post_follow_text_cols, collapse = "; "), "None"), "\n\n")

cat("Raw endorsement is retained as:\n")
cat("  Any side-effect field endorsed before text cleaning\n\n")

cat("Manuscript-facing endorsement is:\n")
cat("  Cleaned negative side-effect endorsement\n\n")

cat("Overall participant-session audit:\n")
cat("  Raw side-effect-like field activity: ",
    overall_cleaned$n_raw, "/", overall_cleaned$n_participant_sessions,
    " (", s9_fmt_pct(overall_cleaned$pct_raw), ")\n", sep = "")
cat("  Cleaned negative side-effect endorsement: ",
    overall_cleaned$n_cleaned, "/", overall_cleaned$n_participant_sessions,
    " (", s9_fmt_pct(overall_cleaned$pct_cleaned), ")\n", sep = "")
cat("  Removed by text/ordinal cleaning: ",
    overall_cleaned$n_removed, "/", overall_cleaned$n_participant_sessions,
    " (", s9_fmt_pct(overall_cleaned$pct_removed), ")\n\n", sep = "")

cat("Table ordering:\n")
cat("  Intervention first, then Control\n\n")

cat("Key outputs:\n")
cat("  - S9_cleaned_side_effect_summary_by_arm_session.html\n")
cat("  - S9_cleaned_side_effect_summary_by_arm_session.csv\n")
cat("  - S9d_vdq_discomfort_summary_by_arm_session_CLEANED.html\n")
cat("  - S9c_fisber_summary_by_arm_session_CLEANED.html\n")
cat("  - S9e_cleaned_side_effect_counts_by_participant_session.html\n")
cat("  - S9j_fisber_text_topic_summary_CLEANED.html\n")
cat("  - S9_cleaned_side_effect_logic_narrative.txt\n")

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


# ------------------------------------------------------------
# SECTION: sm expectancy plots s.11
# ------------------------------------------------------------

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
# BASELINE EXPECTANCY BY ARM
# Distribution plot + descriptive table
#####################################################

library(tidyverse)
library(showtext)
library(sysfonts)
library(scales)
library(grid)

# ===================================================
# SETUP
# ===================================================

DATA_DIR <- Sys.getenv("MRC_DATA_DIR", unset = "C:/Users/dn284/Desktop/MRC_omni/data")
SEARCH_DIRS <- c(DATA_DIR)

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

ARM_ORDER <- c("Intervention", "Control")

ARM_PALETTE <- c(
  "Intervention" = "#F2A100",
  "Control"      = "#4F76BC"
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

read_qualtrics_real <- function(path) {
  df <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
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

rowsum_py <- function(df_cols) {
  if (ncol(df_cols) == 0) return(rep(NA_real_, nrow(df_cols)))
  
  out <- rowSums(df_cols, na.rm = TRUE)
  out[rowSums(!is.na(df_cols)) == 0] <- NA_real_
  out
}

rowmean_py <- function(df_cols) {
  if (ncol(df_cols) == 0) return(rep(NA_real_, nrow(df_cols)))
  
  out <- rowMeans(df_cols, na.rm = TRUE)
  out[rowSums(!is.na(df_cols)) == 0] <- NA_real_
  out
}

find_first_col <- function(df, patterns) {
  nms <- names(df)
  
  for (p in patterns) {
    hit <- nms[stringr::str_detect(nms, stringr::regex(p, ignore_case = TRUE))]
    if (length(hit) > 0) return(hit[1])
  }
  
  NA_character_
}

normalise_condition <- function(x) {
  x <- stringr::str_to_lower(stringr::str_trim(as.character(x)))
  
  case_when(
    stringr::str_detect(x, "inter") ~ "Intervention",
    stringr::str_detect(x, "control") ~ "Control",
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
    median = if (n > 0) median(x) else NA_real_,
    iqr = if (n > 0) IQR(x) else NA_real_,
    min = if (n > 0) min(x) else NA_real_,
    max = if (n > 0) max(x) else NA_real_,
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

# ===================================================
# LOAD FILES
# ===================================================

ASSIGN_PATH <- find_csv_any(c(
  "wp2_assignments",
  "assignments"
))

PRE1_PATH <- find_csv_any(c(
  "wp2_pre_session_1",
  "pre_session_1"
))

message("[load] assignments:   ", basename(ASSIGN_PATH))
message("[load] pre_session_1: ", basename(PRE1_PATH))

assign <- read_qualtrics_real(ASSIGN_PATH) %>%
  mutate(part_id = clean_id(part_id))

pre1 <- read_qualtrics_real(PRE1_PATH) %>%
  mutate(part_id = clean_id(part_id))

# ===================================================
# ARM / CONDITION
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
# EXPECTANCY SCORING
# ===================================================

exp_cols <- paste0("treat_expct_", 1:15)
exp_cols <- exp_cols[exp_cols %in% names(pre1)]

if (length(exp_cols) == 0) {
  stop("No treat_expct_* expectancy columns found in pre_session_1.")
}

pre1 <- pre1 %>%
  mutate(across(all_of(exp_cols), ~ vapply(.x, qnum, numeric(1))))

# Reverse-score expectancy items 7–11 as in prior Figure 6 pipeline.
# Assumes 0–10 item scaling.
reverse_cols <- paste0("treat_expct_", 7:11)
reverse_cols <- reverse_cols[reverse_cols %in% names(pre1)]

pre1 <- pre1 %>%
  mutate(across(all_of(reverse_cols), ~ 10 - .x))

exp_mat <- pre1 %>%
  select(all_of(exp_cols))

expectancy_df <- pre1 %>%
  mutate(
    expect_sum = rowsum_py(exp_mat),
    expect_mean = rowmean_py(exp_mat),
    expect_z = as.numeric(scale(expect_sum))
  ) %>%
  select(part_id, expect_sum, expect_mean, expect_z) %>%
  left_join(assign_clean, by = "part_id") %>%
  mutate(
    condition = factor(condition, levels = ARM_ORDER)
  ) %>%
  filter(!is.na(condition), !is.na(expect_sum))

# ===================================================
# DESCRIPTIVE TABLE + ARM COMPARISON
# ===================================================

expectancy_summary <- expectancy_df %>%
  group_by(condition) %>%
  summarise(
    ci95_summary(expect_sum),
    .groups = "drop"
  ) %>%
  transmute(
    Arm = as.character(condition),
    n,
    Mean = round(mean, 2),
    SD = round(sd, 2),
    SE = round(se, 2),
    Median = round(median, 2),
    IQR = round(iqr, 2),
    Min = round(min, 2),
    Max = round(max, 2),
    `95% CI lower` = round(ci_low, 2),
    `95% CI upper` = round(ci_high, 2)
  )

expectancy_test_df <- expectancy_df %>%
  drop_na(condition, expect_sum)

if (n_distinct(expectancy_test_df$condition) == 2) {
  t_expect <- t.test(expect_sum ~ condition, data = expectancy_test_df)
  w_expect <- wilcox.test(expect_sum ~ condition, data = expectancy_test_df, exact = FALSE)
  
  arm_test_summary <- tibble(
    Comparison = "Baseline expectancy sum: Intervention vs Control",
    Test = c("Welch t-test", "Wilcoxon rank-sum"),
    Statistic = c(
      unname(t_expect$statistic),
      unname(w_expect$statistic)
    ),
    p_value = c(
      t_expect$p.value,
      w_expect$p.value
    ),
    p_value_formatted = fmt_p(p_value)
  )
} else {
  arm_test_summary <- tibble(
    Comparison = "Baseline expectancy sum: Intervention vs Control",
    Test = c("Welch t-test", "Wilcoxon rank-sum"),
    Statistic = NA_real_,
    p_value = NA_real_,
    p_value_formatted = NA_character_
  )
}

# ===================================================
# EXPORT TABLES
# ===================================================

readr::write_csv(
  expectancy_df,
  file.path(DATA_DIR, "Baseline_Expectancy_ByArm_ParticipantData.csv")
)

readr::write_csv(
  expectancy_summary,
  file.path(DATA_DIR, "Baseline_Expectancy_ByArm_Summary.csv")
)

readr::write_csv(
  arm_test_summary,
  file.path(DATA_DIR, "Baseline_Expectancy_ByArm_Tests.csv")
)

# ===================================================
# PLOT 1: DENSITY + RUG BY ARM
# ===================================================

theme_expect <- theme_minimal(base_family = PALATINO_NAME) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      colour = alpha("grey70", 0.30),
      linewidth = 0.25
    ),
    axis.line = element_line(colour = "grey25", linewidth = 0.35),
    axis.ticks = element_line(colour = "grey25", linewidth = 0.25),
    plot.title = element_text(
      family = PALATINO_NAME,
      size = 18,
      face = "plain",
      hjust = 0,
      margin = margin(b = 6)
    ),
    axis.title = element_text(
      family = PALATINO_NAME,
      size = 13.5,
      colour = "#111827"
    ),
    axis.text = element_text(
      family = PALATINO_NAME,
      size = 11.5,
      colour = "grey25"
    ),
    strip.text = element_text(
      family = PALATINO_NAME,
      size = 15,
      colour = "#111827",
      margin = margin(b = 4)
    ),
    legend.position = "none",
    plot.margin = margin(6, 8, 6, 6)
  )

p_density <- ggplot(expectancy_df, aes(x = expect_sum, fill = condition, colour = condition)) +
  geom_density(
    alpha = 0.28,
    linewidth = 0.8,
    adjust = 1.05,
    na.rm = TRUE
  ) +
  geom_rug(
    alpha = 0.35,
    linewidth = 0.35,
    sides = "b"
  ) +
  geom_vline(
    data = expectancy_df %>%
      group_by(condition) %>%
      summarise(mean_expectancy = mean(expect_sum, na.rm = TRUE), .groups = "drop"),
    aes(xintercept = mean_expectancy, colour = condition),
    linetype = "dashed",
    linewidth = 0.65
  ) +
  facet_wrap(~ condition, nrow = 1) +
  scale_fill_manual(values = ARM_PALETTE) +
  scale_colour_manual(values = ARM_PALETTE) +
  labs(
    title = "Baseline treatment expectancy by trial arm",
    x = "Baseline expectancy score, summed",
    y = "Density"
  ) +
  theme_expect

print(p_density)

ggsave(
  filename = file.path(DATA_DIR, "Baseline_Expectancy_ByArm_Density.png"),
  plot = p_density,
  width = 6.8,
  height = 3.2,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(DATA_DIR, "Baseline_Expectancy_ByArm_Density.pdf"),
  plot = p_density,
  width = 6.8,
  height = 3.2,
  dpi = 300,
  bg = "white"
)

# ===================================================
# PLOT 2: VIOLIN/BOX/JITTER BY ARM
# ===================================================

set.seed(123)

p_points <- ggplot(expectancy_df, aes(x = condition, y = expect_sum, fill = condition, colour = condition)) +
  geom_violin(
    width = 0.78,
    alpha = 0.20,
    colour = NA,
    trim = FALSE
  ) +
  geom_boxplot(
    width = 0.22,
    alpha = 0.75,
    outlier.shape = NA,
    linewidth = 0.35,
    colour = "grey25"
  ) +
  geom_jitter(
    width = 0.10,
    height = 0,
    alpha = 0.68,
    size = 2.0,
    stroke = 0
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 3.1,
    fill = "white",
    colour = "black",
    stroke = 0.45
  ) +
  scale_fill_manual(values = ARM_PALETTE) +
  scale_colour_manual(values = ARM_PALETTE) +
  labs(
    title = "Baseline treatment expectancy distribution",
    x = NULL,
    y = "Baseline expectancy score, summed"
  ) +
  theme_expect +
  theme(
    legend.position = "none"
  )

print(p_points)

ggsave(
  filename = file.path(DATA_DIR, "Baseline_Expectancy_ByArm_Distribution.png"),
  plot = p_points,
  width = 4.8,
  height = 3.6,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(DATA_DIR, "Baseline_Expectancy_ByArm_Distribution.pdf"),
  plot = p_points,
  width = 4.8,
  height = 3.6,
  dpi = 300,
  bg = "white"
)

# ===================================================
# NARRATIVE OUTPUT
# ===================================================

get_arm_row <- function(tab, arm) {
  tab %>% filter(Arm == arm) %>% slice(1)
}

int_row <- get_arm_row(expectancy_summary, "Intervention")
ctl_row <- get_arm_row(expectancy_summary, "Control")

welch_p <- arm_test_summary %>%
  filter(Test == "Welch t-test") %>%
  pull(p_value) %>%
  .[1]

wilcox_p <- arm_test_summary %>%
  filter(Test == "Wilcoxon rank-sum") %>%
  pull(p_value) %>%
  .[1]

expectancy_narrative <- paste0(
  "Baseline treatment expectancy was summarised by trial arm before candidate-predictor analyses. ",
  "The Intervention arm had a mean summed expectancy score of ",
  fmt_num(int_row$Mean, 2),
  " (SD = ",
  fmt_num(int_row$SD, 2),
  ", n = ",
  int_row$n,
  "), while the Control arm had a mean score of ",
  fmt_num(ctl_row$Mean, 2),
  " (SD = ",
  fmt_num(ctl_row$SD, 2),
  ", n = ",
  ctl_row$n,
  "). ",
  "A Welch two-sample comparison gave p = ",
  fmt_p(welch_p),
  ", and a Wilcoxon rank-sum comparison gave p = ",
  fmt_p(wilcox_p),
  ". ",
  "These descriptive checks were used to assess whether baseline expectancy distributions differed meaningfully between arms before interpreting expectancy as a candidate predictor."
)

expectancy_caption_density <- paste0(
  "Baseline treatment expectancy by trial arm. Density plots show the distribution of summed expectancy scores in the Intervention and Control arms. ",
  "Rug marks show individual participants and dashed vertical lines show arm-specific means."
)

expectancy_caption_points <- paste0(
  "Baseline treatment expectancy distribution by trial arm. Violin plots show the distribution of summed expectancy scores, boxplots show the median and interquartile range, jittered points show individual participants, and white diamonds show arm-specific means."
)

cat("\n")
cat("============================================================\n")
cat("BASELINE EXPECTANCY SUMMARY\n")
cat("============================================================\n\n")
cat(expectancy_narrative)
cat("\n\nDensity figure caption:\n")
cat(expectancy_caption_density)
cat("\n\nDistribution figure caption:\n")
cat(expectancy_caption_points)
cat("\n\nSummary table:\n")
print(expectancy_summary, n = Inf)
cat("\nArm comparison tests:\n")
print(arm_test_summary, n = Inf)
cat("\n============================================================\n\n")

writeLines(
  expectancy_narrative,
  con = file.path(DATA_DIR, "Baseline_Expectancy_ByArm_Narrative.txt")
)

writeLines(
  expectancy_caption_density,
  con = file.path(DATA_DIR, "Baseline_Expectancy_Density_Caption.txt")
)

writeLines(
  expectancy_caption_points,
  con = file.path(DATA_DIR, "Baseline_Expectancy_Distribution_Caption.txt")
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


# ------------------------------------------------------------
# SECTION: MID benchmark and response/remission rate SM plots and narratives
# ------------------------------------------------------------

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
# RESPONSE / REMISSION DESCRIPTIVE SUMMARIES
# PHQ-9 and BDI-II by trial arm
#####################################################

library(tidyverse)
library(showtext)
library(sysfonts)
library(scales)
library(grid)

# ===================================================
# SETUP
# ===================================================

DATA_DIR <- Sys.getenv("MRC_DATA_DIR", unset = "C:/Users/dn284/Desktop/MRC_omni/data")
SEARCH_DIRS <- c(DATA_DIR)

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

ARM_ORDER <- c("Intervention", "Control")

ARM_PALETTE <- c(
  "Intervention" = "#F2A100",
  "Control"      = "#4F76BC"
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

read_qualtrics_real <- function(path) {
  df <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
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

rowsum_py <- function(df_cols) {
  if (ncol(df_cols) == 0) return(rep(NA_real_, nrow(df_cols)))
  
  out <- rowSums(df_cols, na.rm = TRUE)
  out[rowSums(!is.na(df_cols)) == 0] <- NA_real_
  out
}

find_first_col <- function(df, patterns) {
  nms <- names(df)
  
  for (p in patterns) {
    hit <- nms[stringr::str_detect(nms, stringr::regex(p, ignore_case = TRUE))]
    if (length(hit) > 0) return(hit[1])
  }
  
  NA_character_
}

normalise_condition <- function(x) {
  x <- stringr::str_to_lower(stringr::str_trim(as.character(x)))
  
  case_when(
    stringr::str_detect(x, "inter") ~ "Intervention",
    stringr::str_detect(x, "control") ~ "Control",
    TRUE ~ NA_character_
  )
}

score_sum_col_first <- function(df, summary_col, prefixes, n_items) {
  if (summary_col %in% names(df)) {
    out <- vapply(df[[summary_col]], qnum, numeric(1))
    out[is.nan(out)] <- NA_real_
    return(out)
  }
  
  for (prefix in prefixes) {
    cols <- paste0(prefix, "_", seq_len(n_items))
    cols <- cols[cols %in% names(df)]
    
    if (length(cols) > 0) {
      tmp <- df[, cols, drop = FALSE] %>%
        mutate(across(everything(), ~ vapply(.x, qnum, numeric(1))))
      
      out <- rowSums(tmp, na.rm = TRUE)
      out[rowSums(!is.na(tmp)) == 0] <- NA_real_
      return(out)
    }
  }
  
  rep(NA_real_, nrow(df))
}

exact_binom_ci <- function(x, n) {
  if (is.na(x) || is.na(n) || n == 0) {
    return(tibble(
      prop = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_
    ))
  }
  
  bt <- binom.test(x, n)
  
  tibble(
    prop = unname(bt$estimate),
    ci_low = bt$conf.int[1],
    ci_high = bt$conf.int[2]
  )
}

fmt_num <- function(x, digits = 1) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "NA", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}

fmt_ci_pct <- function(lo, hi, digits = 1) {
  paste0("[", fmt_pct(lo, digits), ", ", fmt_pct(hi, digits), "]")
}

# ===================================================
# LOAD FILES
# ===================================================

ASSIGN_PATH <- find_csv_any(c(
  "wp2_assignments",
  "assignments"
))

PRE1_PATH <- find_csv_any(c(
  "wp2_pre_session_1",
  "pre_session_1"
))

POST4_PATH <- find_csv_any(c(
  "wp2_post_session_4",
  "post_session_4"
))

message("[load] assignments:   ", basename(ASSIGN_PATH))
message("[load] pre_session_1: ", basename(PRE1_PATH))
message("[load] post_session_4:", basename(POST4_PATH))

assign <- read_qualtrics_real(ASSIGN_PATH) %>%
  mutate(part_id = clean_id(part_id))

pre1 <- read_qualtrics_real(PRE1_PATH) %>%
  mutate(part_id = clean_id(part_id))

post4 <- read_qualtrics_real(POST4_PATH) %>%
  mutate(part_id = clean_id(part_id))

# ===================================================
# ARM / CONDITION
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
# SCORE PHQ-9 AND BDI-II
# ===================================================

pre_scores <- pre1 %>%
  mutate(
    phq9_pre = score_sum_col_first(
      .,
      summary_col = "phq9_sum",
      prefixes = c("phq9", "phq"),
      n_items = 9
    ),
    bdi_pre = score_sum_col_first(
      .,
      summary_col = "bdi_sum",
      prefixes = c("bdi"),
      n_items = 21
    )
  ) %>%
  select(part_id, phq9_pre, bdi_pre)

post_scores <- post4 %>%
  mutate(
    phq9_post = score_sum_col_first(
      .,
      summary_col = "phq9_sum",
      prefixes = c("phq9", "phq"),
      n_items = 9
    ),
    bdi_post = score_sum_col_first(
      .,
      summary_col = "bdi_sum",
      prefixes = c("bdi"),
      n_items = 21
    )
  ) %>%
  select(part_id, phq9_post, bdi_post)

response_df <- pre_scores %>%
  inner_join(post_scores, by = "part_id") %>%
  left_join(assign_clean, by = "part_id") %>%
  mutate(
    condition = factor(condition, levels = ARM_ORDER),
    
    phq9_change = phq9_pre - phq9_post,
    phq9_pct_reduction = ifelse(!is.na(phq9_pre) & phq9_pre > 0, phq9_change / phq9_pre, NA_real_),
    phq9_response = ifelse(!is.na(phq9_pct_reduction), phq9_pct_reduction >= 0.50, NA),
    phq9_remission = ifelse(!is.na(phq9_post), phq9_post < 5, NA),
    
    bdi_change = bdi_pre - bdi_post,
    bdi_pct_reduction = ifelse(!is.na(bdi_pre) & bdi_pre > 0, bdi_change / bdi_pre, NA_real_),
    bdi_response = ifelse(!is.na(bdi_pct_reduction), bdi_pct_reduction >= 0.50, NA),
    bdi_minimal = ifelse(!is.na(bdi_post), bdi_post <= 13, NA)
  ) %>%
  filter(!is.na(condition))

cat("\n============================================================\n")
cat("RESPONSE / REMISSION DATA AUDIT\n")
cat("============================================================\n")
cat("Rows with arm assignment:", nrow(response_df), "\n\n")
cat("Available PHQ-9 baseline/post pairs by arm:\n")
print(response_df %>% group_by(condition) %>% summarise(n = sum(!is.na(phq9_pre) & !is.na(phq9_post)), .groups = "drop"))
cat("\nAvailable BDI-II baseline/post pairs by arm:\n")
print(response_df %>% group_by(condition) %>% summarise(n = sum(!is.na(bdi_pre) & !is.na(bdi_post)), .groups = "drop"))
cat("============================================================\n\n")

# ===================================================
# SUMMARY TABLE
# ===================================================

summarise_binary <- function(df, scale_name, outcome_name, var_name) {
  df %>%
    group_by(condition) %>%
    summarise(
      N = sum(!is.na(.data[[var_name]])),
      n = sum(.data[[var_name]] == TRUE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      ci = list(exact_binom_ci(n, N))
    ) %>%
    unnest(ci) %>%
    ungroup() %>%
    mutate(
      Scale = scale_name,
      Outcome = outcome_name,
      Percent = 100 * prop,
      `95% CI lower` = 100 * ci_low,
      `95% CI upper` = 100 * ci_high
    ) %>%
    select(
      Scale,
      Outcome,
      Arm = condition,
      n,
      N,
      Percent,
      `95% CI lower`,
      `95% CI upper`
    )
}

response_summary <- bind_rows(
  summarise_binary(
    response_df,
    scale_name = "PHQ-9",
    outcome_name = "Response: ≥50% reduction from baseline",
    var_name = "phq9_response"
  ),
  summarise_binary(
    response_df,
    scale_name = "PHQ-9",
    outcome_name = "Remission: endpoint PHQ-9 < 5",
    var_name = "phq9_remission"
  ),
  summarise_binary(
    response_df,
    scale_name = "BDI-II",
    outcome_name = "Response: ≥50% reduction from baseline",
    var_name = "bdi_response"
  ),
  summarise_binary(
    response_df,
    scale_name = "BDI-II",
    outcome_name = "Minimal symptoms: endpoint BDI-II ≤ 13",
    var_name = "bdi_minimal"
  )
) %>%
  mutate(
    Arm = factor(Arm, levels = ARM_ORDER),
    Scale = factor(Scale, levels = c("PHQ-9", "BDI-II")),
    Outcome_short = case_when(
      stringr::str_detect(Outcome, "Response") ~ "Response",
      stringr::str_detect(Outcome, "Remission") ~ "Remission",
      stringr::str_detect(Outcome, "Minimal") ~ "Minimal symptoms",
      TRUE ~ Outcome
    ),
    Outcome_short = factor(
      Outcome_short,
      levels = c("Response", "Remission", "Minimal symptoms")
    ),
    Result = paste0(
      n,
      "/",
      N,
      " (",
      fmt_num(Percent, 1),
      "%, 95% CI ",
      fmt_num(`95% CI lower`, 1),
      "–",
      fmt_num(`95% CI upper`, 1),
      "%)"
    )
  )

response_summary_export <- response_summary %>%
  transmute(
    Scale = as.character(Scale),
    Outcome,
    Arm = as.character(Arm),
    n,
    N,
    Percent = round(Percent, 1),
    `95% CI lower` = round(`95% CI lower`, 1),
    `95% CI upper` = round(`95% CI upper`, 1),
    Result
  )

readr::write_csv(
  response_df,
  file.path(DATA_DIR, "Response_Remission_ParticipantLevel.csv")
)

readr::write_csv(
  response_summary_export,
  file.path(DATA_DIR, "Response_Remission_Summary_ByArm.csv")
)

# ===================================================
# OPTIONAL ARM DIFFERENCE TESTS
# Fisher exact tests for each scale/outcome
# ===================================================

run_fisher_for_outcome <- function(df, scale_name, outcome_name, var_name) {
  d <- df %>%
    filter(!is.na(condition), !is.na(.data[[var_name]])) %>%
    mutate(outcome_value = .data[[var_name]])
  
  if (n_distinct(d$condition) < 2 || n_distinct(d$outcome_value) < 2) {
    return(tibble(
      Scale = scale_name,
      Outcome = outcome_name,
      Test = "Fisher exact test",
      p_value = NA_real_,
      p_value_formatted = NA_character_
    ))
  }
  
  tab <- table(d$condition, d$outcome_value)
  ft <- fisher.test(tab)
  
  tibble(
    Scale = scale_name,
    Outcome = outcome_name,
    Test = "Fisher exact test",
    p_value = ft$p.value,
    p_value_formatted = case_when(
      p_value < .001 ~ "< .001",
      TRUE ~ sprintf("%.3f", p_value)
    )
  )
}

response_tests <- bind_rows(
  run_fisher_for_outcome(response_df, "PHQ-9", "Response: ≥50% reduction from baseline", "phq9_response"),
  run_fisher_for_outcome(response_df, "PHQ-9", "Remission: endpoint PHQ-9 < 5", "phq9_remission"),
  run_fisher_for_outcome(response_df, "BDI-II", "Response: ≥50% reduction from baseline", "bdi_response"),
  run_fisher_for_outcome(response_df, "BDI-II", "Minimal symptoms: endpoint BDI-II ≤ 13", "bdi_minimal")
)

readr::write_csv(
  response_tests,
  file.path(DATA_DIR, "Response_Remission_FisherTests_ByArm.csv")
)

# ===================================================
# PLOT
# ===================================================

plot_df <- response_summary %>%
  mutate(
    PlotOutcome = case_when(
      Scale == "PHQ-9" & Outcome_short == "Response" ~ "Response\n≥50% reduction",
      Scale == "PHQ-9" & Outcome_short == "Remission" ~ "Remission\nPHQ-9 < 5",
      Scale == "BDI-II" & Outcome_short == "Response" ~ "Response\n≥50% reduction",
      Scale == "BDI-II" & Outcome_short == "Minimal symptoms" ~ "Minimal symptoms\nBDI-II ≤ 13",
      TRUE ~ as.character(Outcome_short)
    ),
    PlotOutcome = factor(
      PlotOutcome,
      levels = c(
        "Response\n≥50% reduction",
        "Remission\nPHQ-9 < 5",
        "Minimal symptoms\nBDI-II ≤ 13"
      )
    )
  )

theme_resp <- theme_minimal(base_family = PALATINO_NAME) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = alpha("grey70", 0.32),
      linewidth = 0.25
    ),
    axis.line = element_line(colour = "grey25", linewidth = 0.35),
    axis.ticks = element_line(colour = "grey25", linewidth = 0.25),
    strip.text = element_text(
      family = PALATINO_NAME,
      size = 15,
      colour = "#111827",
      margin = margin(b = 5)
    ),
    axis.title = element_text(
      family = PALATINO_NAME,
      size = 13.5,
      colour = "#111827"
    ),
    axis.text = element_text(
      family = PALATINO_NAME,
      size = 11.5,
      colour = "grey25"
    ),
    legend.title = element_blank(),
    legend.text = element_text(
      family = PALATINO_NAME,
      size = 10.5
    ),
    legend.position = "bottom",
    plot.margin = margin(6, 8, 6, 6)
  )

p_response <- ggplot(plot_df, aes(x = PlotOutcome, y = Percent, fill = Arm)) +
  geom_col(
    position = position_dodge(width = 0.68),
    width = 0.58,
    alpha = 0.88,
    colour = "white",
    linewidth = 0.25
  ) +
  geom_errorbar(
    aes(
      ymin = `95% CI lower`,
      ymax = `95% CI upper`
    ),
    position = position_dodge(width = 0.68),
    width = 0.16,
    linewidth = 0.55,
    colour = "grey25"
  ) +
  geom_text(
    aes(label = paste0(n, "/", N)),
    position = position_dodge(width = 0.68),
    vjust = -0.45,
    size = 3.2,
    family = PALATINO_NAME,
    colour = "grey20"
  ) +
  facet_wrap(~ Scale, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = ARM_PALETTE, breaks = ARM_ORDER) +
  scale_y_continuous(
    limits = c(0, 105),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "Participants meeting criterion"
  ) +
  theme_resp

print(p_response)

ggsave(
  filename = file.path(DATA_DIR, "Response_Remission_ByArm_BarPlot.png"),
  plot = p_response,
  width = 7.2,
  height = 3.8,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(DATA_DIR, "Response_Remission_ByArm_BarPlot.pdf"),
  plot = p_response,
  width = 7.2,
  height = 3.8,
  dpi = 300,
  bg = "white"
)

# ===================================================
# OPTIONAL: PUBLICATION-STYLE TABLE PLOT
# ===================================================

response_table_wide <- response_summary_export %>%
  select(Scale, Outcome, Arm, Result) %>%
  pivot_wider(
    names_from = Arm,
    values_from = Result
  ) %>%
  arrange(Scale, Outcome)

readr::write_csv(
  response_table_wide,
  file.path(DATA_DIR, "Response_Remission_PublicationTable_Wide.csv")
)

# ===================================================
# NARRATIVE OUTPUT
# ===================================================

get_result <- function(scale, outcome_pattern, arm) {
  response_summary_export %>%
    filter(
      Scale == scale,
      stringr::str_detect(Outcome, stringr::fixed(outcome_pattern)),
      Arm == arm
    ) %>%
    slice(1)
}

sentence_metric <- function(scale, outcome_pattern, label) {
  int <- get_result(scale, outcome_pattern, "Intervention")
  ctl <- get_result(scale, outcome_pattern, "Control")
  
  paste0(
    label,
    " was observed in ",
    int$n,
    "/",
    int$N,
    " Intervention participants (",
    fmt_num(int$Percent, 1),
    "%, exact 95% CI ",
    fmt_num(int$`95% CI lower`, 1),
    "–",
    fmt_num(int$`95% CI upper`, 1),
    "%) and ",
    ctl$n,
    "/",
    ctl$N,
    " Control participants (",
    fmt_num(ctl$Percent, 1),
    "%, exact 95% CI ",
    fmt_num(ctl$`95% CI lower`, 1),
    "–",
    fmt_num(ctl$`95% CI upper`, 1),
    "%)."
  )
}

response_narrative <- paste0(
  "Response and remission outcomes were summarised descriptively by trial arm. ",
  "For PHQ-9, response was defined as a reduction of at least 50% from baseline and remission as endpoint PHQ-9 < 5. ",
  "For BDI-II, response was defined as a reduction of at least 50% from baseline and minimal-symptom status as endpoint BDI-II ≤ 13. ",
  sentence_metric("PHQ-9", "Response", "PHQ-9 response"),
  " ",
  sentence_metric("PHQ-9", "Remission", "PHQ-9 remission"),
  " ",
  sentence_metric("BDI-II", "Response", "BDI-II response"),
  " ",
  sentence_metric("BDI-II", "Minimal", "BDI-II minimal-symptom status"),
  " These binary outcomes are descriptive and should be interpreted as feasibility-trial estimates rather than confirmatory evidence of efficacy."
)

response_caption <- paste0(
  "Response and remission outcomes by trial arm. Bars show the percentage of participants meeting each criterion, with exact binomial 95% confidence intervals. ",
  "PHQ-9 response was defined as ≥50% reduction from baseline and PHQ-9 remission as endpoint PHQ-9 < 5. ",
  "BDI-II response was defined as ≥50% reduction from baseline and BDI-II minimal-symptom status as endpoint BDI-II ≤ 13. ",
  "Labels above bars show n/N. Outcomes are reported descriptively."
)

cat("\n")
cat("============================================================\n")
cat("RESPONSE / REMISSION SUMMARY\n")
cat("============================================================\n\n")
cat(response_narrative)
cat("\n\nSuggested figure caption:\n")
cat(response_caption)
cat("\n\nSummary table:\n")
print(response_summary_export, n = Inf)
cat("\nWide table:\n")
print(response_table_wide, n = Inf)
cat("\nArm comparison tests, optional/descriptive only:\n")
print(response_tests, n = Inf)
cat("\n============================================================\n\n")

writeLines(
  response_narrative,
  con = file.path(DATA_DIR, "Response_Remission_Narrative.txt")
)

writeLines(
  response_caption,
  con = file.path(DATA_DIR, "Response_Remission_FigureCaption.txt")
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
# MID / CLINICALLY MEANINGFUL CHANGE BENCHMARKS
# PHQ-9, BDI-II, M3VAS, SPANE
#####################################################

library(tidyverse)
library(showtext)
library(sysfonts)
library(scales)
library(grid)

# ===================================================
# SETUP
# ===================================================

DATA_DIR <- Sys.getenv("MRC_DATA_DIR", unset = "C:/Users/dn284/Desktop/MRC_omni/data")
SEARCH_DIRS <- c(DATA_DIR)

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

ARM_ORDER <- c("Intervention", "Control")

ARM_PALETTE <- c(
  "Intervention" = "#F2A100",
  "Control"      = "#4F76BC"
)

# ===================================================
# BENCHMARK SETTINGS
# ===================================================

PHQ9_IMPROVEMENT_BENCHMARK <- -5
PHQ9_WORSENING_BENCHMARK   <-  5

# BDI-II has no single universal MCID across contexts.
# This is deliberately labelled as a pragmatic/external benchmark.
# Change this if your manuscript chooses a different value.
BDI_ABS_BENCHMARK <- 10

# BDI-II percentage response benchmark.
BDI_PERCENT_RESPONSE <- 0.50

# Pragmatic interpretive thresholds, not established clinical MCIDs.
M3VAS_PRAGMATIC_BENCHMARK <- 10
SPANE_PRAGMATIC_BENCHMARK <- 3

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

read_qualtrics_real <- function(path) {
  df <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
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

rowsum_py <- function(df_cols) {
  if (ncol(df_cols) == 0) return(rep(NA_real_, nrow(df_cols)))
  
  out <- rowSums(df_cols, na.rm = TRUE)
  out[rowSums(!is.na(df_cols)) == 0] <- NA_real_
  out
}

rowmean_py <- function(df_cols) {
  if (ncol(df_cols) == 0) return(rep(NA_real_, nrow(df_cols)))
  
  out <- rowMeans(df_cols, na.rm = TRUE)
  out[rowSums(!is.na(df_cols)) == 0] <- NA_real_
  out
}

find_first_col <- function(df, patterns) {
  nms <- names(df)
  
  for (p in patterns) {
    hit <- nms[stringr::str_detect(nms, stringr::regex(p, ignore_case = TRUE))]
    if (length(hit) > 0) return(hit[1])
  }
  
  NA_character_
}

normalise_condition <- function(x) {
  x <- stringr::str_to_lower(stringr::str_trim(as.character(x)))
  
  case_when(
    stringr::str_detect(x, "inter") ~ "Intervention",
    stringr::str_detect(x, "control") ~ "Control",
    TRUE ~ NA_character_
  )
}

score_sum_col_first <- function(df, summary_col, prefixes, n_items) {
  if (summary_col %in% names(df)) {
    out <- vapply(df[[summary_col]], qnum, numeric(1))
    out[is.nan(out)] <- NA_real_
    return(out)
  }
  
  for (prefix in prefixes) {
    cols <- paste0(prefix, "_", seq_len(n_items))
    cols <- cols[cols %in% names(df)]
    
    if (length(cols) > 0) {
      tmp <- df[, cols, drop = FALSE] %>%
        mutate(across(everything(), ~ vapply(.x, qnum, numeric(1))))
      
      out <- rowSums(tmp, na.rm = TRUE)
      out[rowSums(!is.na(tmp)) == 0] <- NA_real_
      return(out)
    }
  }
  
  rep(NA_real_, nrow(df))
}

score_m3vas <- function(df) {
  nms <- names(df)
  
  cols <- nms[stringr::str_detect(
    nms,
    stringr::regex(
      "^m3vas|m3_vas|m3vas_ch|mood.*vas|depression.*vas|anxiety.*vas|stress.*vas|sad.*vas|happy.*vas",
      ignore_case = TRUE
    )
  )]
  
  cols <- cols[!stringr::str_detect(
    cols,
    stringr::regex("duration|timing|attention|captcha|id|date|response|recipient|text|comment", ignore_case = TRUE)
  )]
  
  if (length(cols) == 0) {
    return(rep(NA_real_, nrow(df)))
  }
  
  tmp <- df[, cols, drop = FALSE] %>%
    mutate(across(everything(), ~ vapply(.x, qnum, numeric(1))))
  
  rowmean_py(tmp)
}

score_spane_balance <- function(df) {
  if (all(c("spane_p", "spane_n") %in% names(df))) {
    p <- vapply(df$spane_p, qnum, numeric(1))
    n <- vapply(df$spane_n, qnum, numeric(1))
    out <- p - n
    out[is.nan(out)] <- NA_real_
    return(out)
  }
  
  simple_cols <- paste0("spane_", 1:12)
  simple_cols <- simple_cols[simple_cols %in% names(df)]
  
  if (length(simple_cols) >= 12) {
    pos_cols <- paste0("spane_", 1:6)
    neg_cols <- paste0("spane_", 7:12)
    
    pos_tmp <- df[, pos_cols, drop = FALSE] %>%
      mutate(across(everything(), ~ vapply(.x, qnum, numeric(1))))
    
    neg_tmp <- df[, neg_cols, drop = FALSE] %>%
      mutate(across(everything(), ~ vapply(.x, qnum, numeric(1))))
    
    pos <- rowSums(pos_tmp, na.rm = TRUE)
    neg <- rowSums(neg_tmp, na.rm = TRUE)
    
    pos[rowSums(!is.na(pos_tmp)) == 0] <- NA_real_
    neg[rowSums(!is.na(neg_tmp)) == 0] <- NA_real_
    
    return(pos - neg)
  }
  
  rep(NA_real_, nrow(df))
}

exact_binom_ci <- function(x, n) {
  if (is.na(x) || is.na(n) || n == 0) {
    return(tibble(
      prop = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_
    ))
  }
  
  bt <- binom.test(x, n)
  
  tibble(
    prop = unname(bt$estimate),
    ci_low = bt$conf.int[1],
    ci_high = bt$conf.int[2]
  )
}

fmt_num <- function(x, digits = 1) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "NA", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}

fmt_p <- function(p) {
  case_when(
    is.na(p) ~ "NA",
    p < .001 ~ "< .001",
    TRUE ~ sprintf("%.3f", p)
  )
}

# ===================================================
# LOAD FILES
# ===================================================

ASSIGN_PATH <- find_csv_any(c(
  "wp2_assignments",
  "assignments"
))

PRE1_PATH <- find_csv_any(c(
  "wp2_pre_session_1",
  "pre_session_1"
))

POST4_PATH <- find_csv_any(c(
  "wp2_post_session_4",
  "post_session_4"
))

message("[load] assignments:   ", basename(ASSIGN_PATH))
message("[load] pre_session_1: ", basename(PRE1_PATH))
message("[load] post_session_4:", basename(POST4_PATH))

assign <- read_qualtrics_real(ASSIGN_PATH) %>%
  mutate(part_id = clean_id(part_id))

pre1 <- read_qualtrics_real(PRE1_PATH) %>%
  mutate(part_id = clean_id(part_id))

post4 <- read_qualtrics_real(POST4_PATH) %>%
  mutate(part_id = clean_id(part_id))

# ===================================================
# ARM / CONDITION
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
# SCORE BASELINE AND ENDPOINT
# ===================================================

pre_scores <- pre1 %>%
  mutate(
    phq9_pre = score_sum_col_first(
      .,
      summary_col = "phq9_sum",
      prefixes = c("phq9", "phq"),
      n_items = 9
    ),
    bdi_pre = score_sum_col_first(
      .,
      summary_col = "bdi_sum",
      prefixes = c("bdi"),
      n_items = 21
    ),
    m3vas_pre = score_m3vas(.),
    spane_pre = score_spane_balance(.)
  ) %>%
  select(part_id, phq9_pre, bdi_pre, m3vas_pre, spane_pre)

post_scores <- post4 %>%
  mutate(
    phq9_post = score_sum_col_first(
      .,
      summary_col = "phq9_sum",
      prefixes = c("phq9", "phq"),
      n_items = 9
    ),
    bdi_post = score_sum_col_first(
      .,
      summary_col = "bdi_sum",
      prefixes = c("bdi"),
      n_items = 21
    ),
    m3vas_post = score_m3vas(.),
    spane_post = score_spane_balance(.)
  ) %>%
  select(part_id, phq9_post, bdi_post, m3vas_post, spane_post)

mid_df <- pre_scores %>%
  inner_join(post_scores, by = "part_id") %>%
  left_join(assign_clean, by = "part_id") %>%
  mutate(
    condition = factor(condition, levels = ARM_ORDER),
    
    phq9_change = phq9_post - phq9_pre,
    phq9_pct_change = ifelse(!is.na(phq9_pre) & phq9_pre > 0, phq9_change / phq9_pre, NA_real_),
    phq9_mid_improved = ifelse(!is.na(phq9_change), phq9_change <= PHQ9_IMPROVEMENT_BENCHMARK, NA),
    phq9_mid_worsened = ifelse(!is.na(phq9_change), phq9_change >= PHQ9_WORSENING_BENCHMARK, NA),
    phq9_mid_category = case_when(
      phq9_mid_improved == TRUE ~ "Improved by ≥5 points",
      phq9_mid_worsened == TRUE ~ "Worsened by ≥5 points",
      !is.na(phq9_change) ~ "Did not meet ±5-point threshold",
      TRUE ~ NA_character_
    ),
    
    bdi_change = bdi_post - bdi_pre,
    bdi_pct_change = ifelse(!is.na(bdi_pre) & bdi_pre > 0, bdi_change / bdi_pre, NA_real_),
    bdi_pct_reduction = -bdi_pct_change,
    bdi_abs_benchmark_improved = ifelse(!is.na(bdi_change), bdi_change <= -BDI_ABS_BENCHMARK, NA),
    bdi_pct_response = ifelse(!is.na(bdi_pct_reduction), bdi_pct_reduction >= BDI_PERCENT_RESPONSE, NA),
    bdi_minimal = ifelse(!is.na(bdi_post), bdi_post <= 13, NA),
    
    m3vas_change = m3vas_post - m3vas_pre,
    m3vas_pragmatic_improved = ifelse(!is.na(m3vas_change), m3vas_change >= M3VAS_PRAGMATIC_BENCHMARK, NA),
    m3vas_pragmatic_worsened = ifelse(!is.na(m3vas_change), m3vas_change <= -M3VAS_PRAGMATIC_BENCHMARK, NA),
    m3vas_pragmatic_category = case_when(
      m3vas_pragmatic_improved == TRUE ~ paste0("Improved by ≥", M3VAS_PRAGMATIC_BENCHMARK, " points"),
      m3vas_pragmatic_worsened == TRUE ~ paste0("Worsened by ≥", M3VAS_PRAGMATIC_BENCHMARK, " points"),
      !is.na(m3vas_change) ~ paste0("Did not meet ±", M3VAS_PRAGMATIC_BENCHMARK, "-point threshold"),
      TRUE ~ NA_character_
    ),
    
    spane_change = spane_post - spane_pre,
    spane_pragmatic_improved = ifelse(!is.na(spane_change), spane_change >= SPANE_PRAGMATIC_BENCHMARK, NA),
    spane_pragmatic_worsened = ifelse(!is.na(spane_change), spane_change <= -SPANE_PRAGMATIC_BENCHMARK, NA),
    spane_pragmatic_category = case_when(
      spane_pragmatic_improved == TRUE ~ paste0("Improved by ≥", SPANE_PRAGMATIC_BENCHMARK, " points"),
      spane_pragmatic_worsened == TRUE ~ paste0("Worsened by ≥", SPANE_PRAGMATIC_BENCHMARK, " points"),
      !is.na(spane_change) ~ paste0("Did not meet ±", SPANE_PRAGMATIC_BENCHMARK, "-point threshold"),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(condition))

# ===================================================
# AUDIT
# ===================================================

cat("\n============================================================\n")
cat("MID BENCHMARK DATA AUDIT\n")
cat("============================================================\n")
cat("Rows with arm assignment:", nrow(mid_df), "\n\n")
print(
  mid_df %>%
    group_by(condition) %>%
    summarise(
      n_phq9_pair = sum(!is.na(phq9_pre) & !is.na(phq9_post)),
      n_bdi_pair = sum(!is.na(bdi_pre) & !is.na(bdi_post)),
      n_m3vas_pair = sum(!is.na(m3vas_pre) & !is.na(m3vas_post)),
      n_spane_pair = sum(!is.na(spane_pre) & !is.na(spane_post)),
      .groups = "drop"
    )
)
cat("============================================================\n\n")

# ===================================================
# CONTINUOUS CHANGE SUMMARIES
# ===================================================

summarise_change <- function(df, scale_name, change_var, pct_var = NULL) {
  out <- df %>%
    group_by(condition) %>%
    summarise(
      N = sum(!is.na(.data[[change_var]])),
      mean_change = mean(.data[[change_var]], na.rm = TRUE),
      sd_change = sd(.data[[change_var]], na.rm = TRUE),
      median_change = median(.data[[change_var]], na.rm = TRUE),
      iqr_change = IQR(.data[[change_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Scale = scale_name
    )
  
  if (!is.null(pct_var)) {
    pct_out <- df %>%
      group_by(condition) %>%
      summarise(
        mean_pct_change = mean(.data[[pct_var]], na.rm = TRUE),
        sd_pct_change = sd(.data[[pct_var]], na.rm = TRUE),
        median_pct_change = median(.data[[pct_var]], na.rm = TRUE),
        .groups = "drop"
      )
    
    out <- out %>%
      left_join(pct_out, by = "condition")
  } else {
    out <- out %>%
      mutate(
        mean_pct_change = NA_real_,
        sd_pct_change = NA_real_,
        median_pct_change = NA_real_
      )
  }
  
  out %>%
    select(
      Scale,
      Arm = condition,
      N,
      mean_change,
      sd_change,
      median_change,
      iqr_change,
      mean_pct_change,
      sd_pct_change,
      median_pct_change
    )
}

continuous_change_summary <- bind_rows(
  summarise_change(mid_df, "PHQ-9", "phq9_change", "phq9_pct_change"),
  summarise_change(mid_df, "BDI-II", "bdi_change", "bdi_pct_change"),
  summarise_change(mid_df, "M3VAS", "m3vas_change", NULL),
  summarise_change(mid_df, "SPANE balance", "spane_change", NULL)
) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )

# ===================================================
# BINARY BENCHMARK SUMMARIES
# ===================================================

summarise_binary <- function(df, scale_name, benchmark_name, var_name) {
  df %>%
    group_by(condition) %>%
    summarise(
      N = sum(!is.na(.data[[var_name]])),
      n = sum(.data[[var_name]] == TRUE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      ci = list(exact_binom_ci(n, N))
    ) %>%
    unnest(ci) %>%
    ungroup() %>%
    mutate(
      Scale = scale_name,
      Benchmark = benchmark_name,
      Percent = 100 * prop,
      `95% CI lower` = 100 * ci_low,
      `95% CI upper` = 100 * ci_high,
      Result = paste0(
        n,
        "/",
        N,
        " (",
        fmt_num(Percent, 1),
        "%, exact 95% CI ",
        fmt_num(`95% CI lower`, 1),
        "–",
        fmt_num(`95% CI upper`, 1),
        "%)"
      )
    ) %>%
    select(
      Scale,
      Benchmark,
      Arm = condition,
      n,
      N,
      Percent,
      `95% CI lower`,
      `95% CI upper`,
      Result
    )
}

mid_binary_summary <- bind_rows(
  summarise_binary(
    mid_df,
    "PHQ-9",
    "Clinically meaningful improvement: ≥5-point reduction",
    "phq9_mid_improved"
  ),
  summarise_binary(
    mid_df,
    "PHQ-9",
    "Clinically meaningful worsening: ≥5-point increase",
    "phq9_mid_worsened"
  ),
  summarise_binary(
    mid_df,
    "BDI-II",
    paste0("Absolute improvement benchmark: ≥", BDI_ABS_BENCHMARK, "-point reduction"),
    "bdi_abs_benchmark_improved"
  ),
  summarise_binary(
    mid_df,
    "BDI-II",
    "Response benchmark: ≥50% reduction from baseline",
    "bdi_pct_response"
  ),
  summarise_binary(
    mid_df,
    "BDI-II",
    "Minimal-symptom status: endpoint BDI-II ≤13",
    "bdi_minimal"
  ),
  summarise_binary(
    mid_df,
    "M3VAS",
    paste0("Pragmatic improvement threshold: ≥", M3VAS_PRAGMATIC_BENCHMARK, "-point increase"),
    "m3vas_pragmatic_improved"
  ),
  summarise_binary(
    mid_df,
    "M3VAS",
    paste0("Pragmatic worsening threshold: ≥", M3VAS_PRAGMATIC_BENCHMARK, "-point decrease"),
    "m3vas_pragmatic_worsened"
  ),
  summarise_binary(
    mid_df,
    "SPANE balance",
    paste0("Pragmatic improvement threshold: ≥", SPANE_PRAGMATIC_BENCHMARK, "-point increase"),
    "spane_pragmatic_improved"
  ),
  summarise_binary(
    mid_df,
    "SPANE balance",
    paste0("Pragmatic worsening threshold: ≥", SPANE_PRAGMATIC_BENCHMARK, "-point decrease"),
    "spane_pragmatic_worsened"
  )
) %>%
  mutate(
    Arm = factor(Arm, levels = ARM_ORDER),
    Scale = factor(Scale, levels = c("PHQ-9", "BDI-II", "M3VAS", "SPANE balance"))
  )

mid_binary_export <- mid_binary_summary %>%
  transmute(
    Scale = as.character(Scale),
    Benchmark,
    Arm = as.character(Arm),
    n,
    N,
    Percent = round(Percent, 1),
    `95% CI lower` = round(`95% CI lower`, 1),
    `95% CI upper` = round(`95% CI upper`, 1),
    Result
  )

mid_binary_wide <- mid_binary_export %>%
  select(Scale, Benchmark, Arm, Result) %>%
  pivot_wider(
    names_from = Arm,
    values_from = Result
  ) %>%
  arrange(Scale, Benchmark)

# ===================================================
# EXPORT TABLES
# ===================================================

readr::write_csv(
  mid_df,
  file.path(DATA_DIR, "MID_Benchmarks_ParticipantLevel.csv")
)

readr::write_csv(
  continuous_change_summary,
  file.path(DATA_DIR, "MID_ContinuousChange_Summary_ByArm.csv")
)

readr::write_csv(
  mid_binary_export,
  file.path(DATA_DIR, "MID_Benchmark_Binary_Summary_ByArm.csv")
)

readr::write_csv(
  mid_binary_wide,
  file.path(DATA_DIR, "MID_Benchmark_PublicationTable_Wide.csv")
)

# ===================================================
# PLOT
# ===================================================

plot_binary <- mid_binary_summary %>%
  filter(
    Scale %in% c("PHQ-9", "BDI-II"),
    stringr::str_detect(Benchmark, "improvement|Response|Minimal|worsening|Worsening|Clinically")
  ) %>%
  mutate(
    Benchmark_short = case_when(
      Benchmark == "Clinically meaningful improvement: ≥5-point reduction" ~ "≥5-point\nimprovement",
      Benchmark == "Clinically meaningful worsening: ≥5-point increase" ~ "≥5-point\nworsening",
      stringr::str_detect(Benchmark, "Absolute improvement") ~ paste0("≥", BDI_ABS_BENCHMARK, "-point\nimprovement"),
      stringr::str_detect(Benchmark, "Response") ~ "≥50%\nreduction",
      stringr::str_detect(Benchmark, "Minimal") ~ "Minimal\nsymptoms",
      TRUE ~ Benchmark
    ),
    Benchmark_short = factor(
      Benchmark_short,
      levels = c(
        "≥5-point\nimprovement",
        "≥5-point\nworsening",
        paste0("≥", BDI_ABS_BENCHMARK, "-point\nimprovement"),
        "≥50%\nreduction",
        "Minimal\nsymptoms"
      )
    )
  )

theme_mid <- theme_minimal(base_family = PALATINO_NAME) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = alpha("grey70", 0.32),
      linewidth = 0.25
    ),
    axis.line = element_line(colour = "grey25", linewidth = 0.35),
    axis.ticks = element_line(colour = "grey25", linewidth = 0.25),
    strip.text = element_text(
      family = PALATINO_NAME,
      size = 15,
      colour = "#111827",
      margin = margin(b = 5)
    ),
    axis.title = element_text(
      family = PALATINO_NAME,
      size = 13.5,
      colour = "#111827"
    ),
    axis.text = element_text(
      family = PALATINO_NAME,
      size = 10.5,
      colour = "grey25"
    ),
    legend.title = element_blank(),
    legend.text = element_text(
      family = PALATINO_NAME,
      size = 10.5
    ),
    legend.position = "bottom",
    plot.margin = margin(6, 8, 6, 6)
  )

p_mid <- ggplot(plot_binary, aes(x = Benchmark_short, y = Percent, fill = Arm)) +
  geom_col(
    position = position_dodge(width = 0.68),
    width = 0.58,
    alpha = 0.88,
    colour = "white",
    linewidth = 0.25
  ) +
  geom_errorbar(
    aes(
      ymin = `95% CI lower`,
      ymax = `95% CI upper`
    ),
    position = position_dodge(width = 0.68),
    width = 0.15,
    linewidth = 0.55,
    colour = "grey25"
  ) +
  geom_text(
    aes(label = paste0(n, "/", N)),
    position = position_dodge(width = 0.68),
    vjust = -0.45,
    size = 3.0,
    family = PALATINO_NAME,
    colour = "grey20"
  ) +
  facet_wrap(~ Scale, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = ARM_PALETTE, breaks = ARM_ORDER) +
  scale_y_continuous(
    limits = c(0, 105),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "Participants meeting benchmark"
  ) +
  theme_mid

print(p_mid)

ggsave(
  filename = file.path(DATA_DIR, "MID_Benchmarks_PHQ9_BDIII_ByArm_BarPlot.png"),
  plot = p_mid,
  width = 7.8,
  height = 3.8,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(DATA_DIR, "MID_Benchmarks_PHQ9_BDIII_ByArm_BarPlot.pdf"),
  plot = p_mid,
  width = 7.8,
  height = 3.8,
  dpi = 300,
  bg = "white"
)

# ===================================================
# NARRATIVE OUTPUT
# ===================================================

get_binary_result <- function(scale, benchmark_pattern, arm) {
  mid_binary_export %>%
    filter(
      Scale == scale,
      stringr::str_detect(Benchmark, stringr::regex(benchmark_pattern, ignore_case = TRUE)),
      Arm == arm
    ) %>%
    slice(1)
}

sentence_binary <- function(scale, benchmark_pattern, label) {
  int <- get_binary_result(scale, benchmark_pattern, "Intervention")
  ctl <- get_binary_result(scale, benchmark_pattern, "Control")
  
  paste0(
    label,
    " was observed in ",
    int$n,
    "/",
    int$N,
    " Intervention participants (",
    fmt_num(int$Percent, 1),
    "%, exact 95% CI ",
    fmt_num(int$`95% CI lower`, 1),
    "–",
    fmt_num(int$`95% CI upper`, 1),
    "%) and ",
    ctl$n,
    "/",
    ctl$N,
    " Control participants (",
    fmt_num(ctl$Percent, 1),
    "%, exact 95% CI ",
    fmt_num(ctl$`95% CI lower`, 1),
    "–",
    fmt_num(ctl$`95% CI upper`, 1),
    "%)."
  )
}

mid_narrative <- paste0(
  "Clinical change was interpreted using minimally important difference or pragmatic benchmark summaries where available. ",
  "For PHQ-9, a 5-point reduction from baseline was treated as a clinically meaningful improvement benchmark, and a 5-point increase was used as a monitoring threshold for clinically meaningful worsening. ",
  sentence_binary("PHQ-9", "improvement", "PHQ-9 clinically meaningful improvement"),
  " ",
  sentence_binary("PHQ-9", "worsening", "PHQ-9 clinically meaningful worsening"),
  " ",
  "For BDI-II, absolute and percentage change were both summarised. Because there is no single universally accepted BDI-II MCID for this context, the absolute benchmark was treated as a prespecified pragmatic/external interpretive threshold rather than a definitive clinical MCID. ",
  sentence_binary("BDI-II", "Absolute improvement", paste0("BDI-II ≥", BDI_ABS_BENCHMARK, "-point improvement")),
  " ",
  sentence_binary("BDI-II", "Response", "BDI-II ≥50% reduction"),
  " ",
  sentence_binary("BDI-II", "Minimal", "BDI-II minimal-symptom status"),
  " ",
  "For M3VAS and SPANE, thresholds were treated as pragmatic interpretive summaries rather than established clinical MCIDs. ",
  sentence_binary("M3VAS", "Pragmatic improvement", paste0("M3VAS pragmatic improvement of ≥", M3VAS_PRAGMATIC_BENCHMARK, " points")),
  " ",
  sentence_binary("SPANE balance", "Pragmatic improvement", paste0("SPANE balance pragmatic improvement of ≥", SPANE_PRAGMATIC_BENCHMARK, " points")),
  " These benchmark summaries are descriptive and should be interpreted as feasibility-trial estimates rather than confirmatory evidence of efficacy."
)

mid_caption <- paste0(
  "Clinically meaningful change and pragmatic benchmark summaries by trial arm. Bars show the percentage of participants meeting each benchmark, with exact binomial 95% confidence intervals and n/N labels. ",
  "For PHQ-9, clinically meaningful improvement was defined as a ≥5-point reduction from baseline and clinically meaningful worsening as a ≥5-point increase. ",
  "For BDI-II, percentage response was defined as ≥50% reduction from baseline, minimal-symptom status as endpoint BDI-II ≤13, and the absolute change benchmark was treated as a pragmatic/external interpretive threshold rather than a universal MCID. ",
  "M3VAS and SPANE thresholds were treated as pragmatic interpretive benchmarks rather than established clinical MCIDs."
)

cat("\n")
cat("============================================================\n")
cat("MID BENCHMARK SUMMARY\n")
cat("============================================================\n\n")
cat(mid_narrative)
cat("\n\nSuggested figure caption:\n")
cat(mid_caption)
cat("\n\nContinuous change summary:\n")
print(continuous_change_summary, n = Inf)
cat("\nBinary benchmark summary:\n")
print(mid_binary_export, n = Inf)
cat("\nWide publication table:\n")
print(mid_binary_wide, n = Inf)
cat("\n============================================================\n\n")

writeLines(
  mid_narrative,
  con = file.path(DATA_DIR, "MID_Benchmarks_Narrative.txt")
)

writeLines(
  mid_caption,
  con = file.path(DATA_DIR, "MID_Benchmarks_FigureCaption.txt")
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


# ------------------------------------------------------------
# SECTION: SM clinical CI stuff
# ------------------------------------------------------------

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

# ==========================================================
# DROP-IN R SCRIPT:
# 95% CIs FOR REPORTED CLINICAL SUMMARIES AND CONTRASTS
# FULL CORRECTED VERSION — ROBUST NARRATIVE GENERATION
# ==========================================================
#
# Outputs:
#   Clinical_CI_ArmMeans.csv
#   Clinical_CI_PairedChanges.csv
#   Clinical_CI_BetweenArm_ImprovementContrasts.csv
#   Clinical_CI_BaselineAdjustedEndpointContrasts.csv
#   Clinical_CI_DirectChange_ArmSummaries.csv
#   Clinical_CI_DirectChange_BetweenArmContrasts.csv
#   Clinical_CI_ScoreAudit.csv
#   Clinical_CI_DirectChange_Audit.csv
#   Clinical_CI_Narrative_Snippets.txt
#   Clinical_CI_All_Reported.xlsx
#
# Core assumptions:
#   - Baseline = wp2_pre_session_1
#   - Endpoint = wp2_post_session_4
#   - Condition comes ONLY from wp2_assignments
#   - Within-arm changes use paired available-case data
#   - Positive improvement contrasts favour Intervention
#   - Baseline-adjusted endpoint contrasts are calculated for PHQ-9 and BDI-II
#
# ==========================================================

# ----------------------------------------------------------
# 0. Packages
# ----------------------------------------------------------

needed_pkgs <- c(
  "tidyverse",
  "lubridate",
  "stringr",
  "readr",
  "openxlsx"
)

to_install <- needed_pkgs[!needed_pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(lubridate)
library(stringr)
library(readr)
library(openxlsx)

# ----------------------------------------------------------
# 1. Setup
# ----------------------------------------------------------

if (!exists("DATA_DIR")) {
  DATA_DIR <- Sys.getenv("MRC_DATA_DIR", unset = "C:/Users/dn284/Desktop/MRC_omni/data")
}

SEARCH_DIRS <- c(DATA_DIR)
OUT_PREFIX <- file.path(DATA_DIR, "Clinical_CI")

# ----------------------------------------------------------
# 2. General helpers
# ----------------------------------------------------------

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
      
      matched <- all_files[
        str_detect(file_names, regex(pat_regex, ignore_case = TRUE))
      ]
      
      hits <- c(hits, matched)
    }
  }
  
  hits <- unique(hits[file.exists(hits)])
  
  if (length(hits) == 0) {
    if (required) {
      stop("No files found for patterns: ", paste(patterns, collapse = ", "))
    }
    return(NULL)
  }
  
  info <- file.info(hits)
  hits[order(info$mtime, decreasing = TRUE)][1]
}

read_qualtrics_real <- function(path, skiprows = NULL) {
  df <- readr::read_csv(
    path,
    col_types = cols(.default = col_character()),
    skip = skiprows %||% 0,
    show_col_types = FALSE
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

standardise_part_id <- function(df, file_label = "unknown file") {
  id_col <- find_col(
    df,
    c(
      "part_id",
      "participant_id",
      "participant id",
      "participant",
      "participant_number",
      "participant number",
      "pid",
      "subject_id",
      "subject id",
      "subject",
      "id"
    )
  )
  
  if (is.null(id_col)) {
    stop(
      "Could not find a participant ID column in ", file_label, ".\n",
      "Available columns are:\n",
      paste(names(df), collapse = ", ")
    )
  }
  
  df %>%
    mutate(part_id = clean_id(.data[[id_col]]))
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

drop_condition_like_cols <- function(df) {
  condition_like <- names(df)[
    tolower(names(df)) %in% c(
      "condition",
      "condition.x",
      "condition.y",
      "allocation",
      "group",
      "arm",
      "assigned_condition",
      "randomised_condition",
      "randomized_condition",
      "treatment"
    )
  ]
  
  if (length(condition_like) > 0) {
    df <- df %>% select(-all_of(condition_like))
  }
  
  df
}

cols_exact_regex <- function(df, pattern) {
  names(df)[str_detect(names(df), regex(pattern, ignore_case = TRUE))]
}

parse_score_number <- function(x) {
  suppressWarnings(readr::parse_number(as.character(x)))
}

# ----------------------------------------------------------
# 3. CI helpers
# ----------------------------------------------------------

ci_mean <- function(x, conf = 0.95) {
  x <- x[!is.na(x)]
  n_val <- length(x)
  
  if (n_val == 0) {
    return(tibble(
      n = 0,
      estimate = NA_real_,
      sd = NA_real_,
      se = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_
    ))
  }
  
  est <- mean(x)
  sd_val <- sd(x)
  
  if (n_val == 1 || is.na(sd_val)) {
    return(tibble(
      n = n_val,
      estimate = est,
      sd = sd_val,
      se = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_
    ))
  }
  
  se_val <- sd_val / sqrt(n_val)
  crit <- qt(1 - (1 - conf) / 2, df = n_val - 1)
  
  tibble(
    n = n_val,
    estimate = est,
    sd = sd_val,
    se = se_val,
    ci_low = est - crit * se_val,
    ci_high = est + crit * se_val
  )
}

ci_diff_welch <- function(x_int, x_ctrl, conf = 0.95) {
  x_int <- x_int[!is.na(x_int)]
  x_ctrl <- x_ctrl[!is.na(x_ctrl)]
  
  n_i <- length(x_int)
  n_c <- length(x_ctrl)
  
  if (n_i < 2 || n_c < 2) {
    return(tibble(
      n_intervention = n_i,
      n_control = n_c,
      mean_intervention = ifelse(n_i > 0, mean(x_int), NA_real_),
      mean_control = ifelse(n_c > 0, mean(x_ctrl), NA_real_),
      contrast = NA_real_,
      se = NA_real_,
      df = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      p_value = NA_real_
    ))
  }
  
  m_i <- mean(x_int)
  m_c <- mean(x_ctrl)
  v_i <- var(x_int)
  v_c <- var(x_ctrl)
  
  se_val <- sqrt(v_i / n_i + v_c / n_c)
  
  df_val <- (v_i / n_i + v_c / n_c)^2 /
    ((v_i / n_i)^2 / (n_i - 1) + (v_c / n_c)^2 / (n_c - 1))
  
  crit <- qt(1 - (1 - conf) / 2, df = df_val)
  contrast_val <- m_i - m_c
  
  t_stat <- contrast_val / se_val
  p_val <- 2 * pt(abs(t_stat), df = df_val, lower.tail = FALSE)
  
  tibble(
    n_intervention = n_i,
    n_control = n_c,
    mean_intervention = m_i,
    mean_control = m_c,
    contrast = contrast_val,
    se = se_val,
    df = df_val,
    ci_low = contrast_val - crit * se_val,
    ci_high = contrast_val + crit * se_val,
    p_value = p_val
  )
}

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "NA", sprintf(paste0("%.", digits, "f"), x))
}

fmt_ci <- function(est, lo, hi, digits = 2) {
  paste0(fmt_num(est, digits), " [", fmt_num(lo, digits), ", ", fmt_num(hi, digits), "]")
}

# ----------------------------------------------------------
# 4. Locate files
# ----------------------------------------------------------

WP2_ASSIGN_PATH <- newest_match(
  c("*wp2_assignments*.csv"),
  required = TRUE
)

WP2_PRE1_PATH <- newest_match(
  c("*wp2_pre_session_1*.csv", "*pre_session_1*.csv"),
  required = TRUE
)

WP2_POST4_PATH <- newest_match(
  c("*wp2_post_session_4*.csv", "*post_session_4*.csv"),
  required = TRUE
)

message("Using assignment file: ", basename(WP2_ASSIGN_PATH))
message("Using baseline file:   ", basename(WP2_PRE1_PATH))
message("Using endpoint file:   ", basename(WP2_POST4_PATH))

# ----------------------------------------------------------
# 5. Assignment file and definitive condition join
# ----------------------------------------------------------

wp2_assign <- read_qualtrics_real(WP2_ASSIGN_PATH)

wp2_assign <- standardise_part_id(
  wp2_assign,
  file_label = paste0("WP2 assignment file / ", basename(WP2_ASSIGN_PATH))
)

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
    "Could not identify condition/allocation column in WP2 assignment file.\n",
    "Available columns are:\n",
    paste(names(wp2_assign), collapse = ", ")
  )
}

wp2_assign_clean <- wp2_assign %>%
  mutate(condition = standardise_condition(.data[[condition_col]])) %>%
  filter(!is.na(part_id), !is.na(condition)) %>%
  distinct(part_id, .keep_all = TRUE) %>%
  select(part_id, condition)

cat("\nAssignment denominators\n")
cat("=======================\n")
print(wp2_assign_clean %>% count(condition, name = "n"))

read_wp2_file <- function(path, label) {
  df <- read_qualtrics_real(path)
  
  df <- standardise_part_id(
    df,
    file_label = paste0(label, " / ", basename(path))
  )
  
  df %>%
    drop_condition_like_cols() %>%
    mutate(
      source_file = basename(path),
      timepoint_source = label
    ) %>%
    left_join(wp2_assign_clean, by = "part_id") %>%
    filter(!is.na(part_id), !is.na(condition))
}

baseline_raw <- read_wp2_file(WP2_PRE1_PATH, "baseline_pre_session_1")
endpoint_raw <- read_wp2_file(WP2_POST4_PATH, "endpoint_post_session_4")

# ----------------------------------------------------------
# 6. Scoring helpers
# ----------------------------------------------------------

score_sum_items <- function(df, item_cols, min_items) {
  item_cols <- intersect(item_cols, names(df))
  
  if (length(item_cols) == 0) {
    return(rep(NA_real_, nrow(df)))
  }
  
  mat <- df[, item_cols, drop = FALSE] %>%
    mutate(across(everything(), parse_score_number))
  
  n_valid <- rowSums(!is.na(mat))
  score <- rowSums(mat, na.rm = TRUE)
  score[n_valid < min_items] <- NA_real_
  
  score
}

score_first_available_total <- function(df, total_cols) {
  total_cols <- intersect(total_cols, names(df))
  
  if (length(total_cols) == 0) {
    return(rep(NA_real_, nrow(df)))
  }
  
  out <- rep(NA_real_, nrow(df))
  
  for (cc in total_cols) {
    vals <- parse_score_number(df[[cc]])
    out <- ifelse(is.na(out) & !is.na(vals), vals, out)
  }
  
  out
}

score_outcome <- function(df, outcome) {
  if (outcome == "PHQ-9") {
    total <- score_first_available_total(
      df,
      cols_exact_regex(df, "^(phq9_sum|phq9_total|phq_sum|phq_total)$")
    )
    
    items <- score_sum_items(
      df,
      cols_exact_regex(df, "^(phq9|phq)_?([1-9])$"),
      min_items = 8
    )
    
    return(ifelse(!is.na(total), total, items))
  }
  
  if (outcome == "BDI-II") {
    total <- score_first_available_total(
      df,
      cols_exact_regex(df, "^(bdi_sum|bdi_sum_calc|bdi_total|bdi_ii_total)$")
    )
    
    items <- score_sum_items(
      df,
      cols_exact_regex(df, "^bdi_?([1-9]|1[0-9]|2[0-1])$"),
      min_items = 18
    )
    
    return(ifelse(!is.na(total), total, items))
  }
  
  if (outcome == "BAI") {
    total <- score_first_available_total(
      df,
      cols_exact_regex(df, "^(bai_sum|bai_total)$")
    )
    
    items <- score_sum_items(
      df,
      cols_exact_regex(df, "^bai_?([1-9]|1[0-9]|2[0-1])$"),
      min_items = 18
    )
    
    return(ifelse(!is.na(total), total, items))
  }
  
  if (outcome == "MADRS-S") {
    total <- score_first_available_total(
      df,
      cols_exact_regex(
        df,
        "^(madrs_sum|madrs_total|madrs_s_total|madrss_total|mards_sum|mards_total)$"
      )
    )
    
    items <- score_sum_items(
      df,
      cols_exact_regex(df, "^(madrs|mards|madrs_s|madrss)_?([1-9])(_[0-9]+)?$"),
      min_items = 7
    )
    
    return(ifelse(!is.na(total), total, items))
  }
  
  if (outcome == "SPANE Positive") {
    total <- score_first_available_total(
      df,
      cols_exact_regex(df, "^(spane_p|spane_positive)$")
    )
    
    if (all(is.na(total))) {
      total <- score_sum_items(
        df,
        cols_exact_regex(df, "^spane_?([1-6])$"),
        min_items = 5
      )
    }
    
    return(total)
  }
  
  if (outcome == "SPANE Negative") {
    total <- score_first_available_total(
      df,
      cols_exact_regex(df, "^(spane_n|spane_negative)$")
    )
    
    if (all(is.na(total))) {
      total <- score_sum_items(
        df,
        cols_exact_regex(df, "^spane_?(7|8|9|10|11|12)$"),
        min_items = 5
      )
    }
    
    return(total)
  }
  
  if (outcome == "SPANE Balance") {
    p <- score_outcome(df, "SPANE Positive")
    n <- score_outcome(df, "SPANE Negative")
    return(ifelse(!is.na(p) & !is.na(n), p - n, NA_real_))
  }
  
  rep(NA_real_, nrow(df))
}

score_direct_change <- function(df, outcome) {
  if (outcome == "M3VAS Mood Change") {
    return(score_first_available_total(
      df,
      cols_exact_regex(df, "^m3vas_ch_mood_1$|m3vas.*mood")
    ))
  }
  
  if (outcome == "M3VAS Pleasure Change") {
    return(score_first_available_total(
      df,
      cols_exact_regex(df, "^m3vas_ch_pleasure_1$|m3vas.*pleasure")
    ))
  }
  
  if (outcome == "M3VAS Suicidal Change") {
    return(score_first_available_total(
      df,
      cols_exact_regex(df, "^m3vas_ch_suicidal_1$|m3vas.*suicidal")
    ))
  }
  
  rep(NA_real_, nrow(df))
}

# ----------------------------------------------------------
# 7. Outcome registry
# ----------------------------------------------------------

paired_outcomes <- tribble(
  ~outcome, ~direction, ~baseline_adjusted,
  "PHQ-9", "lower_better", TRUE,
  "BDI-II", "lower_better", TRUE,
  "BAI", "lower_better", FALSE,
  "MADRS-S", "lower_better", FALSE,
  "SPANE Positive", "higher_better", FALSE,
  "SPANE Negative", "lower_better", FALSE,
  "SPANE Balance", "higher_better", FALSE
)

direct_change_outcomes <- tribble(
  ~outcome, ~direction,
  "M3VAS Mood Change", "higher_better",
  "M3VAS Pleasure Change", "higher_better",
  "M3VAS Suicidal Change", "lower_better"
)

# ----------------------------------------------------------
# 8. Build paired baseline-endpoint dataset
# ----------------------------------------------------------

baseline_scores <- paired_outcomes %>%
  pull(outcome) %>%
  map_dfr(function(oo) {
    baseline_raw %>%
      transmute(
        part_id,
        condition,
        outcome = oo,
        baseline = score_outcome(baseline_raw, oo)
      )
  })

endpoint_scores <- paired_outcomes %>%
  pull(outcome) %>%
  map_dfr(function(oo) {
    endpoint_raw %>%
      transmute(
        part_id,
        condition,
        outcome = oo,
        endpoint = score_outcome(endpoint_raw, oo)
      )
  })

paired_data <- baseline_scores %>%
  full_join(endpoint_scores, by = c("part_id", "condition", "outcome")) %>%
  left_join(paired_outcomes, by = "outcome") %>%
  mutate(
    paired_available = !is.na(baseline) & !is.na(endpoint),
    improvement = case_when(
      direction == "lower_better" ~ baseline - endpoint,
      direction == "higher_better" ~ endpoint - baseline,
      TRUE ~ NA_real_
    )
  )

# ----------------------------------------------------------
# 9. Arm-level baseline and endpoint summaries
# ----------------------------------------------------------

arm_means <- paired_data %>%
  pivot_longer(
    cols = c(baseline, endpoint),
    names_to = "timepoint",
    values_to = "score"
  ) %>%
  filter(!is.na(score)) %>%
  group_by(outcome, condition, timepoint) %>%
  summarise(ci_mean(score), .groups = "drop") %>%
  mutate(summary = fmt_ci(estimate, ci_low, ci_high)) %>%
  arrange(outcome, timepoint, condition)

# ----------------------------------------------------------
# 10. Within-arm paired changes
# ----------------------------------------------------------

paired_changes <- paired_data %>%
  filter(paired_available) %>%
  group_by(outcome, condition, direction) %>%
  summarise(ci_mean(improvement), .groups = "drop") %>%
  mutate(
    change_label = "Improvement",
    summary = fmt_ci(estimate, ci_low, ci_high)
  ) %>%
  arrange(outcome, condition)

# ----------------------------------------------------------
# 11. Between-arm improvement contrasts
# ----------------------------------------------------------

between_arm_contrasts <- paired_data %>%
  filter(paired_available) %>%
  group_by(outcome, direction) %>%
  group_modify(~ {
    x_int <- .x %>%
      filter(condition == "Intervention") %>%
      pull(improvement)
    
    x_ctrl <- .x %>%
      filter(condition == "Control") %>%
      pull(improvement)
    
    ci_diff_welch(x_int, x_ctrl)
  }) %>%
  ungroup() %>%
  mutate(
    contrast_label = "Intervention-minus-control difference in improvement",
    interpretation = "Positive values indicate greater improvement in Intervention",
    summary = fmt_ci(contrast, ci_low, ci_high)
  ) %>%
  arrange(outcome)

# ----------------------------------------------------------
# 12. Baseline-adjusted endpoint contrasts
# ----------------------------------------------------------

baseline_adjusted_contrasts <- paired_data %>%
  filter(paired_available) %>%
  filter(baseline_adjusted) %>%
  group_by(outcome, direction) %>%
  group_modify(~ {
    direction_value <- .y$direction[[1]]
    
    dat <- .x %>%
      mutate(condition = factor(condition, levels = c("Control", "Intervention")))
    
    empty_return <- function(n_rows = nrow(dat)) {
      tibble(
        n = n_rows,
        adjusted_endpoint_difference = NA_real_,
        adjusted_endpoint_ci_low = NA_real_,
        adjusted_endpoint_ci_high = NA_real_,
        adjusted_endpoint_p = NA_real_,
        improvement_direction_contrast = NA_real_,
        improvement_direction_ci_low = NA_real_,
        improvement_direction_ci_high = NA_real_
      )
    }
    
    if (
      nrow(dat) < 4 ||
      n_distinct(dat$condition) < 2 ||
      all(is.na(dat$baseline)) ||
      all(is.na(dat$endpoint))
    ) {
      return(empty_return())
    }
    
    fit <- tryCatch(
      lm(endpoint ~ baseline + condition, data = dat),
      error = function(e) NULL
    )
    
    if (is.null(fit)) {
      return(empty_return())
    }
    
    coef_name <- "conditionIntervention"
    
    if (!coef_name %in% names(coef(fit))) {
      return(empty_return())
    }
    
    est <- coef(fit)[[coef_name]]
    
    ci_vec <- tryCatch(
      stats::confint(fit, level = 0.95)[coef_name, ],
      error = function(e) c(NA_real_, NA_real_)
    )
    
    p_val <- tryCatch(
      summary(fit)$coefficients[coef_name, "Pr(>|t|)"],
      error = function(e) NA_real_
    )
    
    if (direction_value == "lower_better") {
      imp_est <- -est
      imp_low <- -ci_vec[2]
      imp_high <- -ci_vec[1]
    } else if (direction_value == "higher_better") {
      imp_est <- est
      imp_low <- ci_vec[1]
      imp_high <- ci_vec[2]
    } else {
      imp_est <- NA_real_
      imp_low <- NA_real_
      imp_high <- NA_real_
    }
    
    tibble(
      n = nrow(dat),
      adjusted_endpoint_difference = est,
      adjusted_endpoint_ci_low = ci_vec[1],
      adjusted_endpoint_ci_high = ci_vec[2],
      adjusted_endpoint_p = p_val,
      improvement_direction_contrast = imp_est,
      improvement_direction_ci_low = imp_low,
      improvement_direction_ci_high = imp_high
    )
  }) %>%
  ungroup() %>%
  mutate(
    endpoint_difference_label = "Baseline-adjusted endpoint difference: Intervention minus Control",
    improvement_direction_label = "Baseline-adjusted contrast transformed so positive favours Intervention",
    endpoint_summary = fmt_ci(
      adjusted_endpoint_difference,
      adjusted_endpoint_ci_low,
      adjusted_endpoint_ci_high
    ),
    improvement_direction_summary = fmt_ci(
      improvement_direction_contrast,
      improvement_direction_ci_low,
      improvement_direction_ci_high
    )
  ) %>%
  arrange(outcome)

# ----------------------------------------------------------
# 13. Direct change score summaries: M3VAS-Change
# ----------------------------------------------------------

direct_change_data <- direct_change_outcomes %>%
  pull(outcome) %>%
  map_dfr(function(oo) {
    endpoint_raw %>%
      transmute(
        part_id,
        condition,
        outcome = oo,
        raw_change = score_direct_change(endpoint_raw, oo)
      )
  }) %>%
  left_join(direct_change_outcomes, by = "outcome") %>%
  mutate(
    improvement = case_when(
      direction == "higher_better" ~ raw_change,
      direction == "lower_better" ~ -raw_change,
      TRUE ~ NA_real_
    )
  )

direct_change_arm_summaries <- direct_change_data %>%
  filter(!is.na(raw_change)) %>%
  group_by(outcome, condition, direction) %>%
  summarise(ci_mean(raw_change), .groups = "drop") %>%
  mutate(summary = fmt_ci(estimate, ci_low, ci_high)) %>%
  arrange(outcome, condition)

direct_change_contrasts <- direct_change_data %>%
  filter(!is.na(improvement)) %>%
  group_by(outcome, direction) %>%
  group_modify(~ {
    x_int <- .x %>%
      filter(condition == "Intervention") %>%
      pull(improvement)
    
    x_ctrl <- .x %>%
      filter(condition == "Control") %>%
      pull(improvement)
    
    ci_diff_welch(x_int, x_ctrl)
  }) %>%
  ungroup() %>%
  mutate(
    contrast_label = "Intervention-minus-control difference in direction-coded M3VAS change",
    interpretation = "Positive values indicate greater improvement in Intervention",
    summary = fmt_ci(contrast, ci_low, ci_high)
  ) %>%
  arrange(outcome)

# ----------------------------------------------------------
# 14. Audit tables
# ----------------------------------------------------------

score_audit <- paired_data %>%
  group_by(outcome, condition) %>%
  summarise(
    n_baseline = sum(!is.na(baseline)),
    n_endpoint = sum(!is.na(endpoint)),
    n_paired = sum(paired_available),
    .groups = "drop"
  ) %>%
  arrange(outcome, condition)

direct_change_audit <- direct_change_data %>%
  group_by(outcome, condition) %>%
  summarise(
    n_change_score = sum(!is.na(raw_change)),
    .groups = "drop"
  ) %>%
  arrange(outcome, condition)

# ----------------------------------------------------------
# 15. Narrative snippets — robust base-R loop version
# ----------------------------------------------------------
# This avoids the previous map_chr()/discard() failure.

safe_one_row <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df[1, , drop = FALSE]
}

make_change_sentence <- function(outcome_name) {
  rows <- paired_changes %>% filter(outcome == outcome_name)
  
  ctrl <- safe_one_row(rows %>% filter(condition == "Control"))
  intv <- safe_one_row(rows %>% filter(condition == "Intervention"))
  con  <- safe_one_row(between_arm_contrasts %>% filter(outcome == outcome_name))
  
  if (is.null(ctrl) || is.null(intv) || is.null(con)) return(NA_character_)
  
  paste0(
    outcome_name, ": Control mean improvement was ",
    fmt_ci(ctrl$estimate, ctrl$ci_low, ctrl$ci_high),
    " (n = ", ctrl$n, "); Intervention mean improvement was ",
    fmt_ci(intv$estimate, intv$ci_low, intv$ci_high),
    " (n = ", intv$n, "). The intervention-minus-control difference in improvement was ",
    fmt_ci(con$contrast, con$ci_low, con$ci_high),
    " (p = ", fmt_num(con$p_value, 3), ")."
  )
}

paired_narrative_vec <- character(0)

for (oo in paired_outcomes$outcome) {
  sent <- make_change_sentence(oo)
  if (length(sent) == 1 && !is.na(sent) && nzchar(sent)) {
    paired_narrative_vec <- c(paired_narrative_vec, sent)
  }
}

make_adjusted_sentence <- function(outcome_name) {
  row <- safe_one_row(baseline_adjusted_contrasts %>% filter(outcome == outcome_name))
  
  if (is.null(row)) return(NA_character_)
  
  paste0(
    outcome_name, ": the baseline-adjusted endpoint difference ",
    "(Intervention minus Control) was ",
    fmt_ci(
      row$adjusted_endpoint_difference,
      row$adjusted_endpoint_ci_low,
      row$adjusted_endpoint_ci_high
    ),
    " (p = ", fmt_num(row$adjusted_endpoint_p, 3), "). ",
    "Expressed in the improvement-favouring direction, the contrast was ",
    fmt_ci(
      row$improvement_direction_contrast,
      row$improvement_direction_ci_low,
      row$improvement_direction_ci_high
    ),
    "."
  )
}

adjusted_narrative_vec <- character(0)

for (oo in paired_outcomes$outcome[paired_outcomes$baseline_adjusted]) {
  sent <- make_adjusted_sentence(oo)
  if (length(sent) == 1 && !is.na(sent) && nzchar(sent)) {
    adjusted_narrative_vec <- c(adjusted_narrative_vec, sent)
  }
}

narrative_lines <- c(
  "WITHIN-ARM CHANGES AND BETWEEN-ARM IMPROVEMENT CONTRASTS",
  "========================================================",
  paired_narrative_vec,
  "",
  "BASELINE-ADJUSTED ENDPOINT CONTRASTS",
  "====================================",
  adjusted_narrative_vec
)

writeLines(narrative_lines, paste0(OUT_PREFIX, "_Narrative_Snippets.txt"))

# ----------------------------------------------------------
# 16. Export CSVs
# ----------------------------------------------------------

write_csv(arm_means, paste0(OUT_PREFIX, "_ArmMeans.csv"))
write_csv(paired_changes, paste0(OUT_PREFIX, "_PairedChanges.csv"))
write_csv(between_arm_contrasts, paste0(OUT_PREFIX, "_BetweenArm_ImprovementContrasts.csv"))
write_csv(baseline_adjusted_contrasts, paste0(OUT_PREFIX, "_BaselineAdjustedEndpointContrasts.csv"))
write_csv(direct_change_arm_summaries, paste0(OUT_PREFIX, "_DirectChange_ArmSummaries.csv"))
write_csv(direct_change_contrasts, paste0(OUT_PREFIX, "_DirectChange_BetweenArmContrasts.csv"))
write_csv(score_audit, paste0(OUT_PREFIX, "_ScoreAudit.csv"))
write_csv(direct_change_audit, paste0(OUT_PREFIX, "_DirectChange_Audit.csv"))

# ----------------------------------------------------------
# 17. Export Excel workbook safely
# ----------------------------------------------------------

wb <- createWorkbook()

safe_add_sheet <- function(wb, sheet_name, df) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df)
  
  if (ncol(df) > 0) {
    setColWidths(
      wb,
      sheet = sheet_name,
      cols = seq_len(ncol(df)),
      widths = 18
    )
  }
  
  invisible(wb)
}

safe_add_sheet(wb, "Arm means", arm_means)
safe_add_sheet(wb, "Paired changes", paired_changes)
safe_add_sheet(wb, "Improvement contrasts", between_arm_contrasts)
safe_add_sheet(wb, "Adjusted endpoint", baseline_adjusted_contrasts)
safe_add_sheet(wb, "M3VAS change summaries", direct_change_arm_summaries)
safe_add_sheet(wb, "M3VAS change contrasts", direct_change_contrasts)
safe_add_sheet(wb, "Score audit", score_audit)
safe_add_sheet(wb, "M3VAS audit", direct_change_audit)
safe_add_sheet(wb, "Narrative snippets", tibble(text = narrative_lines))

saveWorkbook(
  wb,
  paste0(OUT_PREFIX, "_All_Reported.xlsx"),
  overwrite = TRUE
)

# ----------------------------------------------------------
# 18. Console output
# ----------------------------------------------------------

cat("\nClinical CI outputs saved:\n")
cat("==========================\n")
cat(paste0(OUT_PREFIX, "_ArmMeans.csv\n"))
cat(paste0(OUT_PREFIX, "_PairedChanges.csv\n"))
cat(paste0(OUT_PREFIX, "_BetweenArm_ImprovementContrasts.csv\n"))
cat(paste0(OUT_PREFIX, "_BaselineAdjustedEndpointContrasts.csv\n"))
cat(paste0(OUT_PREFIX, "_DirectChange_ArmSummaries.csv\n"))
cat(paste0(OUT_PREFIX, "_DirectChange_BetweenArmContrasts.csv\n"))
cat(paste0(OUT_PREFIX, "_ScoreAudit.csv\n"))
cat(paste0(OUT_PREFIX, "_DirectChange_Audit.csv\n"))
cat(paste0(OUT_PREFIX, "_Narrative_Snippets.txt\n"))
cat(paste0(OUT_PREFIX, "_All_Reported.xlsx\n"))

cat("\nScore audit:\n")
print(score_audit, n = Inf, width = Inf)

cat("\nBetween-arm improvement contrasts:\n")
print(between_arm_contrasts, n = Inf, width = Inf)

cat("\nBaseline-adjusted endpoint contrasts:\n")
print(baseline_adjusted_contrasts, n = Inf, width = Inf)

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


# ------------------------------------------------------------
# SECTION: wp2 scheduled-event denominators
# ------------------------------------------------------------

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

# ==========================================================
# DROP-IN R SCRIPT:
# WP2 Scheduled-Event Denominator Audit
# FULLY ADJUSTED VECTORISED VERSION
# ==========================================================
#
# Fixes:
#   - Vectorises percentage functions for mutate()
#   - Separates passed screening, randomised, never-attended,
#     Session 1 starters, taster/dropout categories, and
#     session-specific scheduled-event denominators.
#
# Outputs:
#   WP2_ScheduledEventDenominator_FlowSummary.csv
#   WP2_ScheduledEventDenominator_BySession.csv
#   WP2_ScheduledEventDenominator_ByArmSession.csv
#   WP2_ScheduledEventDenominator_IDLevel.csv
#   WP2_ScheduledEventDenominator_AuditColumns.csv
#   WP2_ScheduledEventDenominator_Narrative.txt
#   WP2_ScheduledEventDenominator_All.xlsx
#
# ==========================================================

# ----------------------------------------------------------
# 0. Packages
# ----------------------------------------------------------

needed_pkgs <- c(
  "tidyverse",
  "lubridate",
  "stringr",
  "readr",
  "openxlsx"
)

to_install <- needed_pkgs[!needed_pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(lubridate)
library(stringr)
library(readr)
library(openxlsx)

# ----------------------------------------------------------
# 1. Setup
# ----------------------------------------------------------

if (!exists("DATA_DIR")) {
  DATA_DIR <- Sys.getenv("MRC_DATA_DIR", unset = "C:/Users/dn284/Desktop/MRC_omni/data")
}

SEARCH_DIRS <- c(DATA_DIR)

OUT_PREFIX <- file.path(DATA_DIR, "WP2_ScheduledEventDenominator")

# Optional manual override vectors.
# Add IDs as character strings if the monitoring notebook has confirmed cases.
MANUAL_NEVER_ATTENDED_IDS <- character(0)
MANUAL_TASTER_DROPOUT_IDS <- character(0)
MANUAL_POST_S1_DROPOUT_IDS <- character(0)

# ----------------------------------------------------------
# 2. General helpers
# ----------------------------------------------------------

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
      
      matched <- all_files[
        str_detect(file_names, regex(pat_regex, ignore_case = TRUE))
      ]
      
      hits <- c(hits, matched)
    }
  }
  
  hits <- unique(hits[file.exists(hits)])
  
  if (length(hits) == 0) {
    if (required) {
      stop("No files found for patterns: ", paste(patterns, collapse = ", "))
    }
    return(NULL)
  }
  
  info <- file.info(hits)
  hits[order(info$mtime, decreasing = TRUE)][1]
}

read_qualtrics_real <- function(path, skiprows = NULL) {
  df <- readr::read_csv(
    path,
    col_types = cols(.default = col_character()),
    skip = skiprows %||% 0,
    show_col_types = FALSE
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

standardise_part_id <- function(df, file_label = "unknown file") {
  id_col <- find_col(
    df,
    c(
      "part_id",
      "participant_id",
      "participant id",
      "participant",
      "participant_number",
      "participant number",
      "pid",
      "subject_id",
      "subject id",
      "subject",
      "id"
    )
  )
  
  if (is.null(id_col)) {
    stop(
      "Could not find a participant ID column in ", file_label, ".\n",
      "Available columns are:\n",
      paste(names(df), collapse = ", ")
    )
  }
  
  df %>%
    mutate(part_id = clean_id(.data[[id_col]]))
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

drop_condition_like_cols <- function(df) {
  condition_like <- names(df)[
    tolower(names(df)) %in% c(
      "condition",
      "condition.x",
      "condition.y",
      "allocation",
      "group",
      "arm",
      "assigned_condition",
      "randomised_condition",
      "randomized_condition",
      "treatment"
    )
  ]
  
  if (length(condition_like) > 0) {
    df <- df %>% select(-all_of(condition_like))
  }
  
  df
}

valid_text <- function(x) {
  x_chr <- str_trim(as.character(x))
  
  !(is.na(x_chr) |
      x_chr == "" |
      str_to_lower(x_chr) %in% c(
        "na", "nan", "none", "null", "missing",
        "not applicable", "n/a"
      ))
}

get_session_col <- function(df) {
  find_col(
    df,
    c(
      "session_n",
      "session",
      "session_number",
      "session number",
      "visit",
      "visit_n",
      "visit_number"
    )
  )
}

add_session_n <- function(df, default_session = NA_integer_) {
  if (is.null(df)) return(NULL)
  
  session_col <- get_session_col(df)
  
  if (is.null(session_col)) {
    df$session_n <- default_session
  } else {
    parsed <- suppressWarnings(as.integer(readr::parse_number(df[[session_col]])))
    df$session_n <- ifelse(is.na(parsed), default_session, parsed)
  }
  
  df
}

safe_n <- function(x) length(unique(na.omit(x)))

# Vectorised percentage helpers for mutate()
pct_vec <- function(n, d) {
  n <- as.numeric(n)
  d <- as.numeric(d)
  ifelse(is.na(d) | d == 0, NA_real_, 100 * n / d)
}

fmt_n_pct_one <- function(n, d) {
  if (is.na(d) || d == 0) {
    return(paste0(n, "/0 (NA%)"))
  }
  paste0(n, "/", d, " (", sprintf("%.1f", 100 * n / d), "%)")
}

fmt_n_pct_vec <- function(n, d) {
  mapply(
    fmt_n_pct_one,
    n,
    d,
    USE.NAMES = FALSE
  )
}

first_or_na <- function(x) {
  if (length(x) == 0) NA else x[[1]]
}

# ----------------------------------------------------------
# 3. Locate files
# ----------------------------------------------------------

WP2_PRE_SCREEN_PATH <- newest_match(
  c("*wp2_pre_screen*.csv"),
  required = TRUE
)

WP2_ASSIGN_PATH <- newest_match(
  c("*wp2_assignments*.csv"),
  required = TRUE
)

WP2_PRE1_PATH <- newest_match(
  c("*wp2_pre_session_1*.csv", "*pre_session_1*.csv"),
  required = TRUE
)

WP2_PRE24_PATH <- newest_match(
  c(
    "*wp2_pre_sessions_2-4*.csv",
    "*pre_sessions_2-4*.csv",
    "*pre_sessions_2_4*.csv"
  ),
  required = FALSE
)

WP2_POST13_PATH <- newest_match(
  c(
    "*wp2_post_sessions_1-3*.csv",
    "*wp2_post_session_1_3*.csv",
    "*wp2_post_session_13*.csv",
    "*wp2_post*session*1*3*.csv",
    "*post_sessions_1-3*.csv"
  ),
  required = FALSE
)

WP2_POST4_PATH <- newest_match(
  c("*wp2_post_session_4*.csv", "*post_session_4*.csv"),
  required = FALSE
)

message("Using pre-screen file:     ", basename(WP2_PRE_SCREEN_PATH))
message("Using assignment file:     ", basename(WP2_ASSIGN_PATH))
message("Using pre-session 1 file:  ", basename(WP2_PRE1_PATH))
message("Using pre-sessions 2-4:    ", ifelse(is.null(WP2_PRE24_PATH), "NULL", basename(WP2_PRE24_PATH)))
message("Using post-sessions 1-3:   ", ifelse(is.null(WP2_POST13_PATH), "NULL", basename(WP2_POST13_PATH)))
message("Using post-session 4:      ", ifelse(is.null(WP2_POST4_PATH), "NULL", basename(WP2_POST4_PATH)))

# ----------------------------------------------------------
# 4. Load pre-screen and assignments
# ----------------------------------------------------------

wp2_pre_screen <- read_qualtrics_real(WP2_PRE_SCREEN_PATH) %>%
  standardise_part_id(file_label = paste0("pre-screen / ", basename(WP2_PRE_SCREEN_PATH)))

excluded_col <- find_col(
  wp2_pre_screen,
  c("excluded", "exclude", "exclusion", "screening_status", "eligible")
)

if (is.null(excluded_col)) {
  warning("Could not find an exclusion/status column in pre-screen. Passed screening will be NA-based.")
  wp2_pre_screen_clean <- wp2_pre_screen %>%
    filter(!is.na(part_id)) %>%
    distinct(part_id, .keep_all = TRUE) %>%
    mutate(
      excluded_clean = NA_character_,
      passed_screening = NA
    )
} else {
  wp2_pre_screen_clean <- wp2_pre_screen %>%
    filter(!is.na(part_id)) %>%
    distinct(part_id, .keep_all = TRUE) %>%
    mutate(
      excluded_clean = str_to_lower(str_trim(as.character(.data[[excluded_col]]))),
      excluded_clean = na_if(excluded_clean, ""),
      excluded_clean = na_if(excluded_clean, "nan"),
      excluded_clean = na_if(excluded_clean, "none"),
      passed_screening = case_when(
        excluded_clean == "false" ~ TRUE,
        excluded_clean %in% c("no", "0", "eligible", "passed", "pass") ~ TRUE,
        is.na(excluded_clean) ~ NA,
        TRUE ~ FALSE
      )
    )
}

wp2_assign <- read_qualtrics_real(WP2_ASSIGN_PATH) %>%
  standardise_part_id(file_label = paste0("assignments / ", basename(WP2_ASSIGN_PATH)))

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
    "Could not identify condition/allocation column in wp2_assignments.\n",
    "Available columns are:\n",
    paste(names(wp2_assign), collapse = ", ")
  )
}

wp2_assign_clean <- wp2_assign %>%
  mutate(condition = standardise_condition(.data[[condition_col]])) %>%
  filter(!is.na(part_id), !is.na(condition)) %>%
  distinct(part_id, .keep_all = TRUE) %>%
  select(part_id, condition)

# ----------------------------------------------------------
# 5. Load session files and join condition from assignments
# ----------------------------------------------------------

read_wp2_session_file <- function(path, label, default_session = NA_integer_) {
  if (is.null(path)) return(NULL)
  
  df <- read_qualtrics_real(path) %>%
    standardise_part_id(file_label = paste0(label, " / ", basename(path))) %>%
    drop_condition_like_cols() %>%
    add_session_n(default_session = default_session) %>%
    mutate(
      source_file = basename(path),
      source_type = label
    ) %>%
    left_join(wp2_assign_clean, by = "part_id")
  
  df
}

pre1 <- read_wp2_session_file(WP2_PRE1_PATH, "pre_session", default_session = 1)
pre24 <- read_wp2_session_file(WP2_PRE24_PATH, "pre_session", default_session = NA_integer_)
post13 <- read_wp2_session_file(WP2_POST13_PATH, "post_session", default_session = NA_integer_)
post4 <- read_wp2_session_file(WP2_POST4_PATH, "post_session", default_session = 4)

all_pre_sessions <- bind_rows(pre1, pre24) %>%
  filter(!is.na(part_id), !is.na(condition), session_n %in% 1:4)

all_post_sessions <- bind_rows(post13, post4) %>%
  filter(!is.na(part_id), !is.na(condition), session_n %in% 1:4)

all_session_evidence <- bind_rows(
  all_pre_sessions %>% mutate(evidence_type = "pre_session_form"),
  all_post_sessions %>% mutate(evidence_type = "post_session_form")
) %>%
  filter(!is.na(part_id), !is.na(condition), session_n %in% 1:4)

# ----------------------------------------------------------
# 6. Detect possible monitoring/dropout/taster columns
# ----------------------------------------------------------

dropout_pattern <- regex(
  "drop|withdraw|withdrew|taster|attendance|attend|status|reason|note|comment|complete",
  ignore_case = TRUE
)

safe_names <- function(x) {
  if (is.null(x)) character(0) else names(x)
}

audit_columns <- bind_rows(
  tibble(file_group = "pre_screen", column = names(wp2_pre_screen)),
  tibble(file_group = "assignments", column = names(wp2_assign)),
  tibble(file_group = "pre_session_1", column = safe_names(pre1)),
  tibble(file_group = "pre_sessions_2_4", column = safe_names(pre24)),
  tibble(file_group = "post_sessions_1_3", column = safe_names(post13)),
  tibble(file_group = "post_session_4", column = safe_names(post4))
) %>%
  mutate(possible_monitoring_column = str_detect(column, dropout_pattern)) %>%
  filter(possible_monitoring_column)

write_csv(audit_columns, paste0(OUT_PREFIX, "_AuditColumns.csv"))

detect_text_flag <- function(df, pattern) {
  if (is.null(df) || nrow(df) == 0) return(tibble(part_id = character(0)))
  
  text_cols <- names(df)[str_detect(names(df), dropout_pattern)]
  
  if (length(text_cols) == 0) {
    return(tibble(part_id = character(0)))
  }
  
  df %>%
    select(part_id, all_of(text_cols)) %>%
    pivot_longer(
      cols = all_of(text_cols),
      names_to = "column",
      values_to = "value"
    ) %>%
    filter(valid_text(value)) %>%
    mutate(value_l = str_to_lower(as.character(value))) %>%
    filter(str_detect(value_l, regex(pattern, ignore_case = TRUE))) %>%
    distinct(part_id)
}

possible_taster_dropout_ids <- bind_rows(
  detect_text_flag(pre1, "taster"),
  detect_text_flag(pre24, "taster"),
  detect_text_flag(post13, "taster"),
  detect_text_flag(post4, "taster")
) %>%
  pull(part_id) %>%
  unique()

possible_withdrawal_ids <- bind_rows(
  detect_text_flag(pre1, "withdraw|withdrew|drop"),
  detect_text_flag(pre24, "withdraw|withdrew|drop"),
  detect_text_flag(post13, "withdraw|withdrew|drop"),
  detect_text_flag(post4, "withdraw|withdrew|drop")
) %>%
  pull(part_id) %>%
  unique()

# ----------------------------------------------------------
# 7. ID-level denominator flags
# ----------------------------------------------------------

randomised_ids <- wp2_assign_clean$part_id

passed_screening_ids <- wp2_pre_screen_clean %>%
  filter(passed_screening %in% TRUE) %>%
  pull(part_id) %>%
  unique()

pre_by_session <- all_pre_sessions %>%
  distinct(part_id, condition, session_n) %>%
  mutate(pre_form_present = TRUE)

post_by_session <- all_post_sessions %>%
  distinct(part_id, condition, session_n) %>%
  mutate(post_form_present = TRUE)

attendance_by_session <- full_join(
  pre_by_session,
  post_by_session,
  by = c("part_id", "condition", "session_n")
) %>%
  mutate(
    pre_form_present = replace_na(pre_form_present, FALSE),
    post_form_present = replace_na(post_form_present, FALSE),
    attended_session = pre_form_present | post_form_present
  )

id_level <- wp2_assign_clean %>%
  mutate(
    passed_screening = part_id %in% passed_screening_ids,
    randomised = TRUE,
    manual_never_attended = part_id %in% MANUAL_NEVER_ATTENDED_IDS,
    manual_taster_dropout = part_id %in% MANUAL_TASTER_DROPOUT_IDS,
    manual_post_s1_dropout = part_id %in% MANUAL_POST_S1_DROPOUT_IDS,
    detected_taster_flag = part_id %in% possible_taster_dropout_ids,
    detected_withdrawal_flag = part_id %in% possible_withdrawal_ids
  )

session_flags_wide <- attendance_by_session %>%
  mutate(session_label = paste0("s", session_n)) %>%
  select(part_id, session_label, pre_form_present, post_form_present, attended_session) %>%
  pivot_wider(
    names_from = session_label,
    values_from = c(pre_form_present, post_form_present, attended_session),
    values_fill = FALSE
  )

id_level <- id_level %>%
  left_join(session_flags_wide, by = "part_id")

for (cc in c(
  "pre_form_present_s1", "pre_form_present_s2", "pre_form_present_s3", "pre_form_present_s4",
  "post_form_present_s1", "post_form_present_s2", "post_form_present_s3", "post_form_present_s4",
  "attended_session_s1", "attended_session_s2", "attended_session_s3", "attended_session_s4"
)) {
  if (!cc %in% names(id_level)) id_level[[cc]] <- FALSE
  id_level[[cc]] <- replace_na(id_level[[cc]], FALSE)
}

id_level <- id_level %>%
  mutate(
    any_session_evidence =
      attended_session_s1 |
      attended_session_s2 |
      attended_session_s3 |
      attended_session_s4,
    
    attended_session_1_broad = attended_session_s1,
    attended_session_1_strict_post = post_form_present_s1,
    
    never_attended =
      manual_never_attended |
      (!any_session_evidence),
    
    taster_dropout =
      manual_taster_dropout |
      detected_taster_flag,
    
    post_s1_dropout_data_defined =
      attended_session_1_broad &
      !attended_session_s2 &
      !attended_session_s3 &
      !attended_session_s4,
    
    post_s1_dropout =
      manual_post_s1_dropout |
      post_s1_dropout_data_defined,
    
    completed_session_4 = attended_session_s4,
    completed_post_session_4 = post_form_present_s4
  )

write_csv(id_level, paste0(OUT_PREFIX, "_IDLevel.csv"))

# ----------------------------------------------------------
# 8. Flow summary
# ----------------------------------------------------------

flow_summary <- tibble(
  denominator_stage = c(
    "Passed screening",
    "Randomised",
    "Never attended any WP2 session",
    "Attended / started Session 1, broad definition",
    "Provided post-Session-1 data, strict post-treatment evidence",
    "Possible taster-session dropout",
    "Post-Session-1 dropout, data-defined or manual",
    "Attended / started Session 4",
    "Provided post-Session-4 data"
  ),
  definition = c(
    "Pre-screen record with excluded/status coded as eligible or false",
    "Present in wp2_assignments with condition allocation",
    "Randomised but no pre- or post-session evidence for Sessions 1-4",
    "Evidence of Session 1 pre-session or post-session form",
    "Evidence of post-session-1 form",
    "Detected from taster/dropout text fields or manual override",
    "Started Session 1 but no evidence of Sessions 2-4, or manual override",
    "Evidence of Session 4 pre- or post-session form",
    "Evidence of post-session-4 form"
  ),
  n = c(
    safe_n(passed_screening_ids),
    safe_n(randomised_ids),
    sum(id_level$never_attended, na.rm = TRUE),
    sum(id_level$attended_session_1_broad, na.rm = TRUE),
    sum(id_level$attended_session_1_strict_post, na.rm = TRUE),
    sum(id_level$taster_dropout, na.rm = TRUE),
    sum(id_level$post_s1_dropout, na.rm = TRUE),
    sum(id_level$attended_session_s4, na.rm = TRUE),
    sum(id_level$post_form_present_s4, na.rm = TRUE)
  ),
  denominator = c(
    safe_n(wp2_pre_screen_clean$part_id),
    safe_n(randomised_ids),
    safe_n(randomised_ids),
    safe_n(randomised_ids),
    sum(id_level$attended_session_1_broad, na.rm = TRUE),
    sum(id_level$attended_session_1_broad, na.rm = TRUE),
    sum(id_level$attended_session_1_broad, na.rm = TRUE),
    sum(id_level$attended_session_1_broad, na.rm = TRUE),
    sum(id_level$attended_session_s4, na.rm = TRUE)
  )
) %>%
  mutate(
    percent = pct_vec(n, denominator),
    n_percent = fmt_n_pct_vec(n, denominator)
  )

write_csv(flow_summary, paste0(OUT_PREFIX, "_FlowSummary.csv"))

# ----------------------------------------------------------
# 9. Scheduled-event denominator by session
# ----------------------------------------------------------

session_grid <- expand_grid(
  part_id = wp2_assign_clean$part_id,
  session_n = 1:4
) %>%
  left_join(wp2_assign_clean, by = "part_id") %>%
  left_join(
    attendance_by_session,
    by = c("part_id", "condition", "session_n")
  ) %>%
  mutate(
    pre_form_present = replace_na(pre_form_present, FALSE),
    post_form_present = replace_na(post_form_present, FALSE),
    attended_session = replace_na(attended_session, FALSE)
  ) %>%
  left_join(
    id_level %>%
      select(
        part_id,
        never_attended,
        attended_session_1_broad,
        post_s1_dropout,
        taster_dropout
      ),
    by = "part_id"
  )

scheduled_by_session <- session_grid %>%
  group_by(session_n) %>%
  summarise(
    randomised_n = n_distinct(part_id),
    treatment_started_denominator = sum(attended_session_1_broad, na.rm = TRUE),
    attended_this_session_n = sum(attended_session, na.rm = TRUE),
    scheduled_event_denominator = attended_this_session_n,
    post_session_data_n = sum(post_form_present, na.rm = TRUE),
    missing_post_session_n = scheduled_event_denominator - post_session_data_n,
    post_session_completion_percent = pct_vec(post_session_data_n, scheduled_event_denominator),
    retention_from_session1_percent = pct_vec(attended_this_session_n, treatment_started_denominator),
    .groups = "drop"
  ) %>%
  mutate(
    scheduled_event_completion = fmt_n_pct_vec(post_session_data_n, scheduled_event_denominator),
    retention_from_session1 = fmt_n_pct_vec(attended_this_session_n, treatment_started_denominator)
  )

write_csv(scheduled_by_session, paste0(OUT_PREFIX, "_BySession.csv"))

scheduled_by_arm_session <- session_grid %>%
  group_by(condition, session_n) %>%
  summarise(
    randomised_n = n_distinct(part_id),
    treatment_started_denominator = sum(attended_session_1_broad, na.rm = TRUE),
    attended_this_session_n = sum(attended_session, na.rm = TRUE),
    scheduled_event_denominator = attended_this_session_n,
    post_session_data_n = sum(post_form_present, na.rm = TRUE),
    missing_post_session_n = scheduled_event_denominator - post_session_data_n,
    post_session_completion_percent = pct_vec(post_session_data_n, scheduled_event_denominator),
    retention_from_session1_percent = pct_vec(attended_this_session_n, treatment_started_denominator),
    .groups = "drop"
  ) %>%
  mutate(
    scheduled_event_completion = fmt_n_pct_vec(post_session_data_n, scheduled_event_denominator),
    retention_from_session1 = fmt_n_pct_vec(attended_this_session_n, treatment_started_denominator)
  ) %>%
  arrange(session_n, condition)

write_csv(scheduled_by_arm_session, paste0(OUT_PREFIX, "_ByArmSession.csv"))

# ----------------------------------------------------------
# 10. Dropout category tables
# ----------------------------------------------------------

dropout_categories <- id_level %>%
  transmute(
    part_id,
    condition,
    passed_screening,
    randomised,
    never_attended,
    attended_session_1_broad,
    attended_session_1_strict_post,
    taster_dropout,
    post_s1_dropout,
    detected_withdrawal_flag,
    completed_session_4,
    completed_post_session_4,
    dropout_category = case_when(
      never_attended ~ "Never attended any WP2 session",
      taster_dropout ~ "Possible taster-session dropout",
      post_s1_dropout ~ "Post-Session-1 dropout",
      completed_post_session_4 ~ "Completed post-session-4 data",
      completed_session_4 ~ "Attended Session 4 but missing post-session-4 data",
      attended_session_1_broad ~ "Started Session 1 but incomplete follow-up",
      TRUE ~ "Unclassified"
    )
  )

dropout_summary <- dropout_categories %>%
  count(dropout_category, name = "n") %>%
  mutate(
    denominator = nrow(id_level),
    percent_randomised = pct_vec(n, denominator),
    n_percent_randomised = fmt_n_pct_vec(n, denominator)
  ) %>%
  arrange(desc(n))

dropout_summary_by_arm <- dropout_categories %>%
  count(condition, dropout_category, name = "n") %>%
  group_by(condition) %>%
  mutate(
    denominator = sum(n),
    percent_within_arm = pct_vec(n, denominator),
    n_percent_within_arm = fmt_n_pct_vec(n, denominator)
  ) %>%
  ungroup() %>%
  arrange(condition, desc(n))

write_csv(dropout_categories, paste0(OUT_PREFIX, "_DropoutCategories_IDLevel.csv"))
write_csv(dropout_summary, paste0(OUT_PREFIX, "_DropoutSummary.csv"))
write_csv(dropout_summary_by_arm, paste0(OUT_PREFIX, "_DropoutSummary_ByArm.csv"))

# ----------------------------------------------------------
# 11. Narrative
# ----------------------------------------------------------

get_flow_n <- function(stage_label) {
  val <- flow_summary %>%
    filter(denominator_stage == stage_label) %>%
    pull(n)
  
  first_or_na(val)
}

n_passed <- get_flow_n("Passed screening")
n_rand <- get_flow_n("Randomised")
n_never <- get_flow_n("Never attended any WP2 session")
n_s1_broad <- get_flow_n("Attended / started Session 1, broad definition")
n_s1_post <- get_flow_n("Provided post-Session-1 data, strict post-treatment evidence")
n_taster <- get_flow_n("Possible taster-session dropout")
n_post_s1_dropout <- get_flow_n("Post-Session-1 dropout, data-defined or manual")
n_s4 <- get_flow_n("Attended / started Session 4")
n_post4 <- get_flow_n("Provided post-Session-4 data")

session_lines <- scheduled_by_session %>%
  mutate(
    line = paste0(
      "Session ", session_n, ": scheduled-event denominator = ",
      scheduled_event_denominator,
      "; post-session data available = ",
      post_session_data_n,
      "; missing among scheduled = ",
      missing_post_session_n,
      "; completion = ",
      scheduled_event_completion,
      "."
    )
  ) %>%
  pull(line)

narrative_lines <- c(
  "WP2 SCHEDULED-EVENT DENOMINATOR AUDIT",
  "=====================================",
  "",
  paste0(
    "The WP2 denominator audit distinguished between participants who passed screening (n = ",
    n_passed,
    "), participants who were randomised (n = ",
    n_rand,
    "), participants who never attended any WP2 session after randomisation (n = ",
    n_never,
    "), and participants with evidence of starting or attending Session 1 (n = ",
    n_s1_broad,
    ")."
  ),
  "",
  paste0(
    "Using the broad treatment-started definition, Session 1 attendance was defined by evidence of either a pre-session-1 or post-session-1 form. ",
    "A stricter post-treatment evidence definition counted participants with post-Session-1 data only (n = ",
    n_s1_post,
    "). Participants who never attended Session 1 should not be counted as missing post-baseline treatment data, whereas participants who started Session 1 but did not continue contribute to retention and adherence denominators."
  ),
  "",
  paste0(
    "The audit identified n = ",
    n_post_s1_dropout,
    " post-Session-1 dropouts using data-defined or manual criteria, and n = ",
    n_taster,
    " possible taster-session dropouts based on detected text fields or manual override. These categories should be checked against the monitoring notebook before final reporting."
  ),
  "",
  paste0(
    "By Session 4, n = ",
    n_s4,
    " participants had evidence of attending or starting the session, and n = ",
    n_post4,
    " provided post-Session-4 data."
  ),
  "",
  "Session-specific scheduled-event denominators:",
  session_lines,
  "",
  "Interpretation note:",
  "For post-session tolerability and experience outcomes, the event-specific denominator should be the number of participants with evidence of attending that session, because post-session data are only expected after a session occurs. For treatment retention/adherence summaries, the denominator should generally be the Session 1 starter denominator, with never-attended randomised IDs separated from post-baseline dropouts."
)

writeLines(narrative_lines, paste0(OUT_PREFIX, "_Narrative.txt"))

# ----------------------------------------------------------
# 12. Excel workbook
# ----------------------------------------------------------

wb <- createWorkbook()

safe_add_sheet <- function(wb, sheet_name, df) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df)
  
  if (ncol(df) > 0) {
    setColWidths(
      wb,
      sheet = sheet_name,
      cols = seq_len(ncol(df)),
      widths = 18
    )
  }
  
  invisible(wb)
}

safe_add_sheet(wb, "Flow summary", flow_summary)
safe_add_sheet(wb, "By session", scheduled_by_session)
safe_add_sheet(wb, "By arm session", scheduled_by_arm_session)
safe_add_sheet(wb, "ID level", id_level)
safe_add_sheet(wb, "Dropout ID level", dropout_categories)
safe_add_sheet(wb, "Dropout summary", dropout_summary)
safe_add_sheet(wb, "Dropout by arm", dropout_summary_by_arm)
safe_add_sheet(wb, "Audit columns", audit_columns)
safe_add_sheet(wb, "Narrative", tibble(text = narrative_lines))

saveWorkbook(
  wb,
  paste0(OUT_PREFIX, "_All.xlsx"),
  overwrite = TRUE
)

# ----------------------------------------------------------
# 13. Console output
# ----------------------------------------------------------

cat("\nWP2 scheduled-event denominator outputs saved:\n")
cat("=============================================\n")
cat(paste0(OUT_PREFIX, "_FlowSummary.csv\n"))
cat(paste0(OUT_PREFIX, "_BySession.csv\n"))
cat(paste0(OUT_PREFIX, "_ByArmSession.csv\n"))
cat(paste0(OUT_PREFIX, "_IDLevel.csv\n"))
cat(paste0(OUT_PREFIX, "_DropoutCategories_IDLevel.csv\n"))
cat(paste0(OUT_PREFIX, "_DropoutSummary.csv\n"))
cat(paste0(OUT_PREFIX, "_DropoutSummary_ByArm.csv\n"))
cat(paste0(OUT_PREFIX, "_AuditColumns.csv\n"))
cat(paste0(OUT_PREFIX, "_Narrative.txt\n"))
cat(paste0(OUT_PREFIX, "_All.xlsx\n"))

cat("\nFlow summary:\n")
print(flow_summary, n = Inf, width = Inf)

cat("\nScheduled-event denominator by session:\n")
print(scheduled_by_session, n = Inf, width = Inf)

cat("\nScheduled-event denominator by arm/session:\n")
print(scheduled_by_arm_session, n = Inf, width = Inf)

cat("\nDropout category summary:\n")
print(dropout_summary, n = Inf, width = Inf)

cat("\nPotential monitoring/dropout columns detected:\n")
print(audit_columns, n = Inf, width = Inf)

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

# ============================================================
# OPTIONAL FINAL CONSOLIDATED SM WORKBOOK BUILDER
# ============================================================
#
# This section does not replace the section-specific exports above.
# It tries to collect the key final objects created by the blocks above into
# a single workbook with the current SM workbook tab names.
#
# Tabs with no available source object are skipped and listed in the audit sheet.
# This is intentional: sequence/Jupyter/static tabs may still need to be imported
# from frozen CSVs or copied across once their source is confirmed.
# ============================================================

if (requireNamespace("openxlsx", quietly = TRUE)) {

  mrc_object_exists <- function(object_name) {
    exists(object_name, envir = .GlobalEnv, inherits = TRUE)
  }

  mrc_get_object <- function(object_name) {
    get(object_name, envir = .GlobalEnv, inherits = TRUE)
  }

  mrc_stack_objects <- function(object_names) {
    dfs <- list()
    for (nm in object_names) {
      if (mrc_object_exists(nm)) {
        dfs[[nm]] <- mrc_as_sheet_df(mrc_get_object(nm), source_name = nm)
      }
    }

    if (length(dfs) == 0) {
      return(NULL)
    }

    suppressWarnings(dplyr::bind_rows(dfs))
  }

  mrc_add_sheet <- function(wb, sheet_name, df) {
    if (is.null(df)) return(FALSE)

    sheet_name <- substr(sheet_name, 1, 31)

    if (sheet_name %in% openxlsx::sheets(wb)) {
      openxlsx::removeWorksheet(wb, sheet_name)
    }

    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet_name, df)

    if (ncol(df) > 0) {
      openxlsx::setColWidths(
        wb,
        sheet = sheet_name,
        cols = seq_len(ncol(df)),
        widths = "auto"
      )
    }

    TRUE
  }

  # Current workbook tab names, using Excel-safe <=31-character names.
  # Each tab is mapped to the most likely object(s) created by the code above.
  final_sheet_specs <- list(
    "WP1 Demographics" = c("wp1_demo_summary", "demo_table_wide"),
    "WP1 Parameters" = c("wp1_param_table"),
    "WP1 Session Tolerability" = c("table_s3_2_excel", "table_s3_2"),
    "WP1 PrePost Mood Comparisons" = c(
      "table_s3_3_baseline", "prepost_descriptives", "paired_change",
      "desc_table", "change_table"
    ),

    "Interim Demographics" = c(
      "interim_demo_summary", "interim_demographics_table"
    ),
    "Interim Parameters" = c(
      "table_pair_1", "table_pair_2", "duration_summary"
    ),
    "Parameter Bins" = c("parameter_bins"),
    "Sonata Form Structure" = c("sonata_algorithm"),
    "Sequence Condensation" = c("condensation_algorithm"),
    "WP2 Parameters" = c("wp2_parameter_table", "wp2_parameters", "wp2_param_table"),
    "Interim Validation" = c("validation_algorithm"),
    "Fidelity Summary" = c("fidelity_summary"),
    "Fidelity per Segment" = c("segment_fidelity"),

    "WP2 Demographics" = c("wp2_demo_summary"),
    "WP2 Assessment Schedule" = c("wp2_schedule_long", "wp2_schedule_wide"),
    "WP2 Data Collection Details" = c(
      "event_summary_with_total", "id_event_level", "wide_table"
    ),
    "WP2 Scale Definitions" = c("scale_definitions", "wp2_scale_definitions"),
    "WP2 Attendance by ArmSession" = c(
      "by_arm_event_with_total", "attendance_by_session", "arm_denoms"
    ),
    "WP2 Data-Collection Adherence" = c(
      "wp2_adherence_wide", "wp2_adherence_long", "wide_table"
    ),

    "Exploratory WP2 Clinical and Af" = c(
      "arm_means", "paired_changes", "between_arm_contrasts",
      "baseline_adjusted_contrasts", "direct_change_arm_summaries",
      "direct_change_contrasts"
    ),
    "WP2 ASC Predictors" = c("fig6_asc_predictors"),
    "WP2 Discomfort Summary by Arm a" = c(
      "discomfort_summary", "participant_session_counts"
    ),
    "WP2 FISBER SE Summaries by ArmS" = c(
      "fisber_summary", "fisber_long", "fisber_text_summary",
      "fisber_topic_summary", "fisber_word_counts"
    ),
    "WP2 Measure-Level Analysis & Mi" = c(
      "s9_text", "ae_summary", "ae_listing", "discontinuation_details",
      "symptom_summary", "symptom_long"
    ),
    "WP2 Scheduled-Event Denominator" = c(
      "flow_summary", "scheduled_by_session", "scheduled_by_arm_session",
      "id_level", "dropout_summary", "dropout_summary_by_arm"
    )
  )

  final_wb <- openxlsx::createWorkbook()

  workbook_audit <- purrr::imap_dfr(final_sheet_specs, function(object_names, sheet_name) {
    df <- mrc_stack_objects(object_names)
    written <- mrc_add_sheet(final_wb, sheet_name, df)

    tibble::tibble(
      sheet_name = sheet_name,
      written = written,
      source_objects_requested = paste(object_names, collapse = "; "),
      source_objects_found = paste(object_names[vapply(object_names, mrc_object_exists, logical(1))], collapse = "; "),
      n_rows = if (is.null(df)) NA_integer_ else nrow(df),
      n_cols = if (is.null(df)) NA_integer_ else ncol(df)
    )
  })

  mrc_add_sheet(final_wb, "Workbook build audit", workbook_audit)

  final_workbook_path <- file.path(
    SM_WORKBOOK_OUT,
    paste0("MRC_SM_workbook_consolidated_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
  )

  openxlsx::saveWorkbook(final_wb, final_workbook_path, overwrite = TRUE)

  readr::write_csv(
    workbook_audit,
    file.path(SM_AUDIT_OUT, "SM_workbook_consolidated_build_audit.csv")
  )

  message("Final consolidated SM workbook attempted: ", final_workbook_path)
  message("Workbook build audit written to: ", file.path(SM_AUDIT_OUT, "SM_workbook_consolidated_build_audit.csv"))

} else {
  message("Package openxlsx is not available, so the optional consolidated workbook builder was skipped.")
}

# ============================================================
# END SCRIPT
# ============================================================
