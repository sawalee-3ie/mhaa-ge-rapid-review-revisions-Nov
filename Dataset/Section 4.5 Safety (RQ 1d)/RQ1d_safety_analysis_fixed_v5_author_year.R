# ==============================================================================
# RQ 1d — Safety analysis for preliminary report Section 4.5
#
# Review question:
# What harms, adverse events, risks, and unintended consequences have been
# reported, and how have interventions sought to mitigate them?
#
# Inputs used:
#   - extraction_final-revised-SL_n=325.xlsx
#   - sheet "1d" (fields 1d.1–1d.10)
#   - sheet "baseline"
#
# Extraction gate: none. All nonblank record IDs in the 1d sheet are retained;
# missingness is then described at field level.
#
# Main outputs:
#   RQ1d_outputs/Section-4.5-Safety-RQ1d.md
#   RQ1d_outputs/Section-4.5-Safety-RQ1d.pdf
#   RQ1d_outputs/RQ1d_reporting_quality_by_design.png
#   RQ1d_outputs/RQ1d_analysis_outputs.xlsx
#   RQ1d_outputs/RQ1d_record_level.csv
#   RQ1d_outputs/RQ1d_quote_bank.csv
#   RQ1d_outputs/RQ1d_AE_rate_validation.csv
#   RQ1d_outputs/RQ1d_verification_sample.csv
#
# Interpretation rules:
#   1. Counts describe records and reporting gaps, not independent studies.
#   2. Reporting frequency is not evidence that a harm is more important or a
#      mitigation is more effective. No vote counting is used for implications.
#   3. Risks, simulated failures, observed events and author recommendations are
#      kept distinct wherever the extraction fields permit.
#   4. "No adverse events reported" is not evidence that no events occurred.
#   5. Mitigations documented as implemented/tested are distinguished from
#      safeguards that were proposed or recommended only.
# ==============================================================================

# ---- 0. Packages and file paths -----------------------------------------------

required_packages <- c(
  "readxl", "dplyr", "tidyr", "purrr", "stringr", "readr",
  "tibble", "ggplot2", "patchwork", "writexl"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running this script: ",
    paste(missing_packages, collapse = ", "),
    "\nRun: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

input_candidates <- c(
  "extraction_final-revised-SL_n=325.xlsx",
  "extraction_final-revised-SL_n=325(6).xlsx"
)

existing_input <- input_candidates[file.exists(input_candidates)]
input_file <- if (length(existing_input) > 0) existing_input[[1]] else input_candidates[[1]]
output_dir <- "RQ1d_outputs"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop(
    "Input workbook not found: ", input_file,
    "\nSet input_file to the correct workbook path."
  )
}

# ---- 1. General helper functions ----------------------------------------------

has_text <- function(x) {
  !is.na(x) & stringr::str_trim(as.character(x)) != ""
}

blank_to_na <- function(x) {
  x <- as.character(x)
  x[!has_text(x)] <- NA_character_
  x
}

has_substantive_text <- function(x) {
  value <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  has_text(value) & !value %in% c(
    "not reported", "not specified", "none reported", "unknown",
    "nr", "n/r", "na", "n/a", "not applicable"
  )
}

parse_bool <- function(x) {
  value <- stringr::str_to_upper(stringr::str_trim(as.character(x)))
  dplyr::case_when(
    value %in% c("TRUE", "T", "YES", "Y", "1") ~ TRUE,
    value %in% c("FALSE", "F", "NO", "N", "0") ~ FALSE,
    TRUE ~ NA
  )
}

extract_composite_bool <- function(x, key) {
  matched <- stringr::str_match(
    dplyr::coalesce(as.character(x), ""),
    stringr::regex(
      paste0("\\b", key, "\\s*:\\s*(true|false|1|0)"),
      ignore_case = TRUE
    )
  )[, 2]
  parse_bool(matched)
}

# Extract a key from pipe-delimited text such as:
# numerator: 1 | denominator: 51 | time_window: 8 weeks
extract_composite_key <- function(x, key) {
  pattern <- paste0(
    "(?is)(?:^|\\|)\\s*", key, "\\s*:\\s*(.*?)",
    "(?=\\s*\\|\\s*[A-Za-z][A-Za-z0-9_ ]*\\s*:|$)"
  )
  blank_to_na(
    stringr::str_match(
      dplyr::coalesce(as.character(x), ""),
      stringr::regex(pattern, ignore_case = TRUE)
    )[, 2]
  )
}

extract_first_number <- function(x, pattern) {
  matched <- stringr::str_match(
    dplyr::coalesce(as.character(x), ""),
    stringr::regex(pattern, ignore_case = TRUE)
  )[, 2]
  suppressWarnings(as.numeric(stringr::str_remove_all(matched, ",")))
}

safe_pct <- function(n, denominator) {
  ifelse(
    is.na(denominator) | denominator == 0,
    NA_real_,
    100 * n / denominator
  )
}

format_n_pct <- function(n, denominator, digits = 1) {
  if (is.na(denominator) || denominator == 0) return(sprintf("n=%s", n))
  sprintf(paste0("n=%s (%.", digits, "f%%)"), n, 100 * n / denominator)
}

field_number <- function(field_id) {
  suppressWarnings(
    as.numeric(stringr::str_extract(field_id, "(?<=1d\\.)[0-9]+"))
  )
}

require_columns <- function(data, required, data_name) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      data_name, " is missing required column(s): ",
      paste(missing, collapse = ", ")
    )
  }
  invisible(TRUE)
}

format_id_list <- function(ids, max_ids = 8) {
  ids <- unique(ids[has_text(ids)])
  if (length(ids) == 0) return("none identified")
  shown <- utils::head(ids, max_ids)
  suffix <- if (length(ids) > max_ids) paste0("; plus ", length(ids) - max_ids, " others") else ""
  paste0(paste0("`", shown, "`", collapse = ", "), suffix)
}

format_count_list <- function(data, label_col, n_col, max_items = 8) {
  if (nrow(data) == 0) return("none")
  data <- data %>%
    dplyr::arrange(dplyr::desc(.data[[n_col]]), .data[[label_col]]) %>%
    dplyr::slice_head(n = max_items)
  paste0(
    data[[label_col]], " (n=", data[[n_col]], ")",
    collapse = "; "
  )
}

standard_count <- function(data, variable, missing_label = "Not reported") {
  data %>%
    dplyr::transmute(
      category = dplyr::if_else(
        has_text(.data[[variable]]),
        stringr::str_squish(as.character(.data[[variable]])),
        missing_label
      )
    ) %>%
    dplyr::count(category, sort = TRUE, name = "n")
}

read_sheet_text <- function(sheet_name) {
  readxl::read_excel(
    input_file,
    sheet = sheet_name,
    col_types = "text",
    .name_repair = "minimal"
  )
}

empty_index <- function() {
  tibble::tibble(
    record_id = character(),
    title_index = character(),
    groups_extracted = character(),
    gated_out_groups = character(),
    total_reported = numeric(),
    needs_human = numeric()
  )
}

empty_review_queue <- function() {
  tibble::tibble(
    record_id = character(), title = character(), group = character(),
    field_id = character(), field_label = character(), issue = character(),
    agreement = character(), consensus_value = character(), quotes = character(),
    notes = character()
  )
}

# Apply a regex dictionary only to the source fields specified. This prevents,
# for example, a mitigation statement mentioning privacy from being counted as
# evidence that a privacy harm occurred.
tag_fields <- function(data, fields, dictionary) {
  fields <- intersect(fields, names(data))

  if (length(fields) == 0 || nrow(dictionary) == 0) {
    return(tibble::tibble(
      record_id = character(), title = character(), source_field = character(),
      text = character(), domain = character(), tag = character(),
      pattern = character()
    ))
  }

  text_long <- data %>%
    dplyr::select(record_id, title, dplyr::all_of(fields)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(fields),
      names_to = "source_field",
      values_to = "text"
    ) %>%
    dplyr::filter(has_text(record_id), has_text(text))

  tidyr::crossing(text_long, dictionary) %>%
    dplyr::filter(
      stringr::str_detect(text, stringr::regex(pattern, ignore_case = TRUE))
    ) %>%
    dplyr::select(
      record_id, title, source_field, text, domain, tag, pattern
    ) %>%
    dplyr::distinct()
}

classify_implementation <- function(x) {
  text <- dplyr::coalesce(as.character(x), "")

  explicit_not_operational <- stringr::str_detect(
    text,
    stringr::regex(
      paste0(
        "not implemented|not operational|not deployed|not tested|",
        "future work only|planned only|recommend(?:ed|ation)? only|",
        "proposed but not|envisaged for future|aspirational"
      ),
      ignore_case = TRUE
    )
  )

  proposed <- stringr::str_detect(
    text,
    stringr::regex(
      paste0(
        "recommend|propos|planned|future work|future iteration|should |",
        "could |would |needed|calls? for|envisaged|aspirational|",
        "policy development|framework suggests"
      ),
      ignore_case = TRUE
    )
  )

  operational <- stringr::str_detect(
    text,
    stringr::regex(
      paste0(
        "implemented|operational|in place|activated|triggered|deployed|",
        "conducted|used during|provided to users|reported within|",
        "successfully tested|tested in live|data safety monitoring board|",
        "\\bDSMB\\b|central monitoring|alert was triggered|",
        "human review was performed|clinician reviewed|system logged"
      ),
      ignore_case = TRUE
    )
  )

  dplyr::case_when(
    explicit_not_operational ~ "Proposed/recommended only",
    operational & proposed ~ "Mixed operational and proposed",
    operational ~ "Implemented/operational",
    proposed ~ "Proposed/recommended only",
    TRUE ~ "Unclear from extraction"
  )
}

# Basic, dependency-light PDF writer. It converts the Markdown text into a
# readable multi-page PDF and appends the selected ggplot as the final page.
# It does not require LaTeX, Quarto, Chrome or pandoc.
write_report_pdf <- function(lines, plot_object, output_path) {
  clean <- lines %>%
    stringr::str_replace_all("`", "") %>%
    stringr::str_replace_all("^#{1,6}\\s*", "") %>%
    stringr::str_replace_all("^[-*]\\s+", "- ")

  wrapped <- unlist(
    lapply(clean, function(line) {
      if (!has_text(line)) return("")
      stringr::str_wrap(line, width = 102)
    }),
    use.names = FALSE
  )

  # str_wrap embeds newlines; split them into physical PDF lines.
  wrapped <- unlist(strsplit(wrapped, "\\n", fixed = FALSE), use.names = FALSE)
  lines_per_page <- 52L
  page_groups <- split(wrapped, ceiling(seq_along(wrapped) / lines_per_page))

  grDevices::pdf(
    output_path,
    width = 8.27,
    height = 11.69,
    onefile = TRUE,
    paper = "a4"
  )

  for (page in page_groups) {
    grid::grid.newpage()
    grid::grid.text(
      paste(page, collapse = "\n"),
      x = grid::unit(0.065, "npc"),
      y = grid::unit(0.965, "npc"),
      just = c("left", "top"),
      gp = grid::gpar(fontfamily = "sans", fontsize = 9, lineheight = 1.18)
    )
  }

  if (!is.null(plot_object)) {
    print(plot_object)
  }

  grDevices::dev.off()
  invisible(output_path)
}

# ---- 2. Read and validate workbook structure ----------------------------------

available_sheets <- readxl::excel_sheets(input_file)
required_sheets <- c("baseline", "1d")
missing_sheets <- setdiff(required_sheets, available_sheets)

if (length(missing_sheets) > 0) {
  stop(
    "Missing required workbook sheet(s): ",
    paste(missing_sheets, collapse = ", ")
  )
}

baseline_raw <- read_sheet_text("baseline")
rq1d_raw <- read_sheet_text("1d")

index_raw <- if ("Index" %in% available_sheets) {
  read_sheet_text("Index")
} else {
  NULL
}

review_queue_raw <- if ("Review queue" %in% available_sheets) {
  read_sheet_text("Review queue")
} else {
  NULL
}

codebook_raw <- if ("Codebook" %in% available_sheets) {
  read_sheet_text("Codebook")
} else {
  NULL
}

baseline_required <- c(
  "record_id", "title", "base.1 value", "base.2 value", "base.3 value",
  "base.4 value", "base.5 value", "base.6 value", "base.7 value",
  "base.8 value", "base.9 value", "base.10 value", "base.11a value",
  "base.11b value", "base.12 value", "base.15 value"
)
require_columns(baseline_raw, baseline_required, "baseline sheet")

expected_1d_components <- unlist(
  lapply(
    paste0("1d.", 1:10),
    function(field) paste(field, c("value", "quotes", "loc", "agree"))
  ),
  use.names = FALSE
)

require_columns(
  rq1d_raw,
  c("record_id", "title", expected_1d_components),
  "1d sheet"
)

# ---- 3. Resolve the 1d row surplus and establish the denominator --------------

raw_1d_data_rows <- nrow(rq1d_raw)
raw_1d_blank_id_rows <- sum(!has_text(rq1d_raw$record_id))

rq1d <- rq1d_raw %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), blank_to_na)) %>%
  dplyr::filter(has_text(.data$record_id)) %>%
  dplyr::rename(title_1d = title)

baseline <- baseline_raw %>%
  dplyr::transmute(
    record_id = blank_to_na(.data$record_id),
    title_baseline = blank_to_na(.data$title),
    bibliographic = blank_to_na(.data[["base.1 value"]]),
    country = blank_to_na(.data[["base.2 value"]]),
    setting = blank_to_na(.data[["base.3 value"]]),
    sample_size = blank_to_na(.data[["base.4 value"]]),
    age_band = blank_to_na(.data[["base.5 value"]]),
    study_design = blank_to_na(.data[["base.6 value"]]),
    object_type = blank_to_na(.data[["base.7 value"]]),
    rq_addressed = blank_to_na(.data[["base.8 value"]]),
    outcomes_reported = blank_to_na(.data[["base.9 value"]]),
    lmic_flag_raw = blank_to_na(.data[["base.10 value"]]),
    ssa_flag_raw = blank_to_na(.data[["base.11a value"]]),
    south_africa_flag_raw = blank_to_na(.data[["base.11b value"]]),
    evidence_type_tag = blank_to_na(.data[["base.12 value"]]),
    minimal_quality = blank_to_na(.data[["base.15 value"]])
  ) %>%
  dplyr::filter(has_text(.data$record_id))

index_data <- if (!is.null(index_raw)) {
  require_columns(
    index_raw,
    c(
      "record_id", "title", "groups_extracted", "gated_out_groups",
      "total_reported", "needs_human"
    ),
    "Index sheet"
  )

  index_raw %>%
    dplyr::transmute(
      record_id = blank_to_na(.data$record_id),
      title_index = blank_to_na(.data$title),
      groups_extracted = blank_to_na(.data$groups_extracted),
      gated_out_groups = blank_to_na(.data$gated_out_groups),
      total_reported = suppressWarnings(as.numeric(.data$total_reported)),
      needs_human = suppressWarnings(as.numeric(.data$needs_human))
    ) %>%
    dplyr::filter(has_text(.data$record_id))
} else {
  empty_index()
}

check_duplicate_ids <- function(data, data_name) {
  duplicates <- data %>%
    dplyr::count(.data$record_id, name = "n_rows") %>%
    dplyr::filter(.data$n_rows > 1)

  if (nrow(duplicates) > 0) {
    stop(
      data_name, " contains duplicated nonblank record_id values: ",
      paste(duplicates$record_id, collapse = ", "),
      ". Resolve them before analysis; the script will not silently deduplicate."
    )
  }
  duplicates
}

duplicate_1d_ids <- check_duplicate_ids(rq1d, "1d sheet")
duplicate_baseline_ids <- check_duplicate_ids(baseline, "baseline sheet")
if (nrow(index_data) > 0) check_duplicate_ids(index_data, "Index sheet")

ids_1d <- unique(rq1d$record_id)
ids_baseline <- unique(baseline$record_id)
ids_index <- unique(index_data$record_id)

ids_1d_not_baseline <- setdiff(ids_1d, ids_baseline)
ids_baseline_not_1d <- setdiff(ids_baseline, ids_1d)
ids_1d_not_index <- if (length(ids_index) > 0) setdiff(ids_1d, ids_index) else character()

if (length(ids_1d_not_baseline) > 0) {
  stop(
    "The following 1d record IDs are absent from baseline: ",
    paste(ids_1d_not_baseline, collapse = ", ")
  )
}

stream_n <- length(ids_1d)
filename_expected_n <- suppressWarnings(
  as.numeric(stringr::str_match(basename(input_file), "n=([0-9]+)")[, 2])
)

physical_surplus_rows <- raw_1d_data_rows - stream_n

surplus_resolution <- dplyr::case_when(
  physical_surplus_rows == raw_1d_blank_id_rows & nrow(duplicate_1d_ids) == 0 ~
    paste0(
      "Resolved: all ", physical_surplus_rows,
      " surplus physical rows have blank record_id values; no duplicated nonblank IDs were found."
    ),
  nrow(duplicate_1d_ids) > 0 ~
    "Unresolved: duplicated nonblank record IDs are present.",
  TRUE ~
    "Requires manual review: physical row surplus is not fully explained by blank record IDs."
)

filename_difference_note <- if (!is.na(filename_expected_n) && filename_expected_n != stream_n) {
  paste0(
    "The filename implies n=", filename_expected_n,
    ", whereas baseline and 1d contain ", stream_n,
    " aligned unique IDs (difference ", filename_expected_n - stream_n,
    "). The supplied workbook does not contain the absent IDs, so whether this difference reflects linked-publication removal must be confirmed from the review log/PRISMA documentation rather than inferred from these sheets."
  )
} else {
  "The filename denominator matches the unique 1d denominator."
}

denominator_audit <- tibble::tibble(
  item = c(
    "Extraction gate",
    "Physical data rows in 1d",
    "Rows with blank record_id in 1d",
    "Rows with nonblank record_id in 1d",
    "Unique record IDs in 1d",
    "Duplicated nonblank record IDs in 1d",
    "Unique record IDs in baseline",
    "Unique record IDs in Index",
    "1d IDs absent from baseline",
    "Baseline IDs absent from 1d",
    "1d IDs absent from Index",
    "Denominator resolution",
    "Filename-versus-data note"
  ),
  value = c(
    "None",
    as.character(raw_1d_data_rows),
    as.character(raw_1d_blank_id_rows),
    as.character(nrow(rq1d)),
    as.character(stream_n),
    as.character(nrow(duplicate_1d_ids)),
    as.character(length(ids_baseline)),
    as.character(length(ids_index)),
    as.character(length(ids_1d_not_baseline)),
    as.character(length(ids_baseline_not_1d)),
    as.character(length(ids_1d_not_index)),
    surplus_resolution,
    filename_difference_note
  )
)

# ---- 4. Build the record-level master -----------------------------------------

master <- rq1d %>%
  dplyr::left_join(baseline, by = "record_id") %>%
  dplyr::left_join(index_data, by = "record_id") %>%
  dplyr::mutate(
    title = dplyr::coalesce(.data$title_baseline, .data$title_1d, .data$title_index)
  )

value_cols <- paste0("1d.", 1:10, " value")

master <- master %>%
  dplyr::mutate(
    rq1d_any = dplyr::if_any(dplyr::all_of(value_cols), ~ has_text(.x)),
    has_1d_1 = has_text(.data[["1d.1 value"]]),
    has_1d_2 = has_text(.data[["1d.2 value"]]),
    has_1d_3 = has_text(.data[["1d.3 value"]]),
    has_1d_4 = has_text(.data[["1d.4 value"]]),
    has_1d_5 = has_text(.data[["1d.5 value"]]),
    has_1d_6 = has_text(.data[["1d.6 value"]]),
    has_1d_7 = has_text(.data[["1d.7 value"]]),
    has_1d_8 = has_text(.data[["1d.8 value"]]),
    has_1d_9 = has_text(.data[["1d.9 value"]]),
    has_1d_10 = has_text(.data[["1d.10 value"]]),
    lmic = parse_bool(.data$lmic_flag_raw),
    ssa = parse_bool(.data$ssa_flag_raw),
    south_africa = parse_bool(.data$south_africa_flag_raw),
    age_10_19 = extract_composite_bool(.data$age_band, "maps_10_19"),
    age_10_24 = extract_composite_bool(.data$age_band, "maps_10_24"),
    age_18_24 = extract_composite_bool(.data$age_band, "maps_18_24"),
    age_as_reported = extract_composite_key(.data$age_band, "as_reported"),
    sample_n = extract_first_number(.data$sample_size, "\\bn\\s*:\\s*([0-9][0-9,]*)"),
    adolescent_or_young_direct = dplyr::coalesce(.data$age_10_19, FALSE) |
      dplyr::coalesce(.data$age_10_24, FALSE),
    south_africa_direct = dplyr::coalesce(.data$south_africa, FALSE)
  )

# Architecture is classified from the baseline intervention/object description,
# not from safety-field text. This avoids treating a review that merely mentions
# an LLM-related risk as a GenAI intervention record.
genai_pattern <- paste0(
  "\\b(",
  paste(
    c(
      "llm", "large language model", "generative ai", "genai",
      "gpt(?:-[0-9a-z.]+)?", "chatgpt", "rag[- ]based",
      "retrieval[- ]augmented", "foundation model", "dialogpt",
      "gemini", "claude", "llama", "mistral", "ernie bot", "lamda"
    ),
    collapse = "|"
  ),
  ")\\b"
)

rule_based_pattern <- paste0(
  "rule[- ]based|decision tree|pre[- ]defined|preprogrammed|pre-programmed|",
  "scripted dialogue|finite[- ]state|deterministic response|hard[- ]coded"
)

ai_unspecified_pattern <- paste0(
  "artificial intelligence|\\bai\\b|natural language processing|",
  "\\bnlp\\b|machine learning|deep learning"
)

master <- master %>%
  dplyr::mutate(
    technology_text = dplyr::coalesce(.data$object_type, ""),
    mentions_genai = stringr::str_detect(
      .data$technology_text,
      stringr::regex(genai_pattern, ignore_case = TRUE)
    ),
    mentions_rule_based = stringr::str_detect(
      .data$technology_text,
      stringr::regex(rule_based_pattern, ignore_case = TRUE)
    ),
    mentions_ai_unspecified = stringr::str_detect(
      .data$technology_text,
      stringr::regex(ai_unspecified_pattern, ignore_case = TRUE)
    ),
    genai_direct = .data$mentions_genai,
    architecture_class = dplyr::case_when(
      .data$mentions_genai & .data$mentions_rule_based ~
        "Hybrid/multiple generative + rule-based",
      .data$mentions_genai ~ "Generative AI / LLM",
      .data$mentions_rule_based ~ "Rule-based / deterministic",
      .data$mentions_ai_unspecified ~ "AI/NLP, architecture unclear",
      TRUE ~ "Other digital or unclear"
    ),
    mhaa_all_three_direct = .data$adolescent_or_young_direct &
      .data$genai_direct & .data$south_africa_direct,
    design_group = dplyr::case_when(
      .data$study_design == "RCT" ~ "RCT",
      .data$study_design %in% c("non-RCT trial", "cohort", "cross-sectional") ~
        "Other primary quantitative",
      .data$study_design %in% c("qualitative", "mixed-methods") ~
        "Qualitative or mixed-methods",
      .data$study_design %in% c("SR", "SR+MA", "scoping", "umbrella") ~
        "Evidence synthesis",
      .data$study_design %in% c("framework", "regulatory") ~
        "Framework or guidance",
      TRUE ~ "Other or not reported"
    ),
    feasibility_or_pilot = stringr::str_detect(
      dplyr::coalesce(.data$evidence_type_tag, ""),
      stringr::regex(
        "feasib|pilot|proof[- ]of[- ]concept|acceptab|usability|formative",
        ignore_case = TRUE
      )
    ),
    is_review = .data$study_design %in% c("SR", "SR+MA", "scoping", "umbrella"),
    is_rct = .data$study_design == "RCT",
    reporting_category_detail = dplyr::case_when(
      .data$has_1d_2 ~ "Quantified AE/SAE rate reported",
      .data$has_1d_1 ~ "Narrative harms only (no quantified AE/SAE rate)",
      .data$rq1d_any ~ "Other safety information only",
      TRUE ~ "No RQ1d safety information"
    ),
    reporting_category_plot = dplyr::case_when(
      .data$has_1d_2 ~ "Quantified AE/SAE rate (1d.2)",
      .data$rq1d_any ~ "Non-quantified safety reporting",
      TRUE ~ "No RQ1d safety reporting"
    )
  )

contributing <- master %>% dplyr::filter(.data$rq1d_any)
contributing_n <- nrow(contributing)

# ---- 5. Profile field coverage and the 1d.1–1d.2 reporting gap ----------------

fallback_codebook_1d <- tibble::tribble(
  ~group, ~field_id, ~label, ~type, ~allowed, ~what, ~feeds_eval,
  "1d", "1d.1", "Types of harm reported", "text", NA_character_,
  "Unsafe content, over-reliance, privacy, bias and distress.", "Eval §11.3",
  "1d", "1d.2", "Rate of AE / SAE", "composite", NA_character_,
  "Numerator, denominator, definition and time window.", "Eval Annex A",
  "1d", "1d.3", "Failure modes documented", "text", NA_character_,
  "Drift, sycophancy, hallucination, jailbreak and crisis-routing failure.", "Eval §11.2",
  "1d", "1d.4", "Detection methods", "text", NA_character_,
  "Human, classifier, rule-based and user-report detection.", "Eval §11.1",
  "1d", "1d.5", "Preventative and responsive mitigation strategies", "text", NA_character_,
  "Guardrails, filters, oversight, red-teaming and model updates.", "Eval §3.2; §11.5",
  "1d", "1d.6", "Crisis / safety-alert protocol", "text", NA_character_,
  "Trigger, escalation, referral, follow-through and live testing.", "Eval §11.4",
  "1d", "1d.7", "Causality attribution method used", "text", NA_character_,
  "Attribution of session-related versus concurrent distress.", "Eval §11.3",
  "1d", "1d.8", "Post-launch monitoring and governance", "text", NA_character_,
  "Drift detection, incident register, transparency and audit status.", "Eval §11.5",
  "1d", "1d.9", "Tessa / drift-type failures", "text", NA_character_,
  "Evidence-based designs that failed after deployment.", "Eval §3.5; §11.5",
  "1d", "1d.10", "Equity-related harms", "text", NA_character_,
  "Bias, exclusion and widening inequities.", "Eval §7"
)

codebook_1d <- if (!is.null(codebook_raw)) {
  require_columns(
    codebook_raw,
    c("group", "field_id", "label", "type", "allowed", "what", "feeds_eval"),
    "Codebook sheet"
  )

  codebook_raw %>%
    dplyr::filter(.data$group == "1d") %>%
    dplyr::select(
      group, field_id, label, type, allowed, what, feeds_eval
    )
} else {
  fallback_codebook_1d
}

rq1d_long <- master %>%
  dplyr::select(
    record_id, title,
    dplyr::all_of(expected_1d_components),
    study_design, design_group, country, setting, age_as_reported,
    object_type, architecture_class, adolescent_or_young_direct,
    genai_direct, south_africa_direct, mhaa_all_three_direct
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::matches("^1d\\.\\d+ (value|quotes|loc|agree)$"),
    names_to = c("field_id", ".value"),
    names_pattern = "(1d\\.\\d+) (value|quotes|loc|agree)"
  ) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::any_of(c("value", "quotes", "loc", "agree")),
      blank_to_na
    )
  ) %>%
  dplyr::left_join(codebook_1d, by = "field_id")

field_coverage <- rq1d_long %>%
  dplyr::group_by(.data$field_id, .data$label) %>%
  dplyr::summarise(
    denominator = stream_n,
    n_reported = sum(has_text(.data$value)),
    pct_all_1d_records = safe_pct(sum(has_text(.data$value)), stream_n),
    n_with_quotes = sum(has_text(.data$quotes)),
    n_conflict = sum(
      stringr::str_to_lower(dplyr::coalesce(.data$agree, "")) == "conflict"
    ),
    n_minor = sum(
      stringr::str_to_lower(dplyr::coalesce(.data$agree, "")) == "minor"
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(field_number(.data$field_id))

field_count <- function(field_id) {
  row <- field_coverage %>%
    dplyr::filter(.data$field_id == .env$field_id)
  if (nrow(row) == 0) return(0L)
  as.integer(row$n_reported[[1]])
}

n_harms_discussed <- field_count("1d.1")
n_ae_rates <- field_count("1d.2")

headline_reporting_gap <- tibble::tibble(
  measure = c(
    "All unique records in 1d",
    "Records with any RQ1d information",
    "Harms discussed (1d.1)",
    "AE/SAE rates quantified (1d.2)",
    "Gap: harms discussed minus AE/SAE rates quantified",
    "Records with no RQ1d information"
  ),
  n = c(
    stream_n,
    contributing_n,
    n_harms_discussed,
    n_ae_rates,
    n_harms_discussed - n_ae_rates,
    stream_n - contributing_n
  ),
  denominator = stream_n
) %>%
  dplyr::mutate(percent = safe_pct(.data$n, .data$denominator))

reporting_profile_counts <- master %>%
  dplyr::count(.data$reporting_category_detail, name = "n_records") %>%
  dplyr::mutate(
    denominator = stream_n,
    percent = safe_pct(.data$n_records, .data$denominator)
  ) %>%
  dplyr::arrange(dplyr::desc(.data$n_records))

# ---- 6. Validate AE/SAE rate extraction (1d.2) --------------------------------

ae_rate_detail <- master %>%
  dplyr::filter(.data$has_1d_2) %>%
  dplyr::transmute(
    record_id, title, study_design, design_group, country,
    raw_1d_2_value = .data[["1d.2 value"]],
    quote = .data[["1d.2 quotes"]],
    location = .data[["1d.2 loc"]],
    agreement = .data[["1d.2 agree"]],
    numerator = extract_composite_key(.data[["1d.2 value"]], "numerator"),
    denominator = extract_composite_key(.data[["1d.2 value"]], "denominator"),
    definition = extract_composite_key(.data[["1d.2 value"]], "definition"),
    time_window = extract_composite_key(.data[["1d.2 value"]], "time_window"),
    n_users_affected = extract_composite_key(
      .data[["1d.2 value"]], "n_users_affected"
    ),
    n_events = extract_composite_key(.data[["1d.2 value"]], "n_events"),
    arm = extract_composite_key(.data[["1d.2 value"]], "arm"),
    zero_meaning = extract_composite_key(.data[["1d.2 value"]], "zero_meaning"),
    confidence_interval = extract_composite_key(.data[["1d.2 value"]], "ci")
  ) %>%
  dplyr::mutate(
    numerator_reported = has_substantive_text(.data$numerator),
    denominator_reported = has_substantive_text(.data$denominator),
    definition_reported = has_substantive_text(.data$definition),
    time_window_reported = has_substantive_text(.data$time_window),
    rate_core_complete = .data$numerator_reported &
      .data$denominator_reported &
      .data$definition_reported &
      .data$time_window_reported,
    zero_event_wording = stringr::str_detect(
      dplyr::coalesce(.data$numerator, ""),
      stringr::regex(
        "(^|[^0-9])0([^0-9]|$)|\\bzero\\b",
        ignore_case = TRUE
      )
    ) | (
      stringr::str_detect(
        dplyr::coalesce(.data$zero_meaning, ""),
        stringr::regex(
          "none occurred|none reported|zero events|zero saes|zero harmful",
          ignore_case = TRUE
        )
      ) &
        !stringr::str_detect(
          dplyr::coalesce(.data$zero_meaning, ""),
          stringr::regex("not applicable|no zero[- ]event", ignore_case = TRUE)
        )
    ),
    zero_meaning_reported = has_substantive_text(.data$zero_meaning),
    zero_meaning_clear = .data$zero_meaning_reported &
      stringr::str_detect(
        dplyr::coalesce(.data$zero_meaning, ""),
        stringr::regex(
          paste0(
            "none occurred|actively monitored|confirmed by expert review|",
            "observed by the interventionist|detected by the safety system"
          ),
          ignore_case = TRUE
        )
      ),
    zero_event_ambiguous = .data$zero_event_wording & !.data$zero_meaning_clear,
    zero_ascertainment = dplyr::case_when(
      !.data$zero_event_wording ~ "Not a zero-event record",
      !.data$zero_meaning_reported ~ "Zero meaning not reported",
      .data$zero_meaning_clear ~ "None occurred / active ascertainment stated",
      TRUE ~ "None reported / passive or unclear ascertainment"
    )
  )

# Build the validation message row by row because each record can have a
# different combination of missing core components.
ae_rate_detail <- ae_rate_detail %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    validation_flag = {
      missing_core <- c(
        "numerator" = !.data$numerator_reported,
        "denominator" = !.data$denominator_reported,
        "definition" = !.data$definition_reported,
        "time window" = !.data$time_window_reported
      )

      issues <- character()

      if (any(missing_core)) {
        issues <- c(
          issues,
          paste0(
            "Missing/unclear: ",
            paste(names(missing_core)[missing_core], collapse = ", ")
          )
        )
      }

      if (.data$zero_event_ambiguous) {
        issues <- c(issues, "Check zero-event meaning/ascertainment")
      }

      if (length(issues) == 0) "Complete core fields" else paste(issues, collapse = "; ")
    }
  ) %>%
  dplyr::ungroup()

ae_rate_completeness <- tibble::tibble(
  metric = c(
    "Records with a 1d.2 value",
    "Numerator reported",
    "Denominator reported",
    "Definition reported",
    "Time window reported",
    "Core fields complete (numerator, denominator, definition, time window)",
    "Zero-event wording",
    "Zero-event wording with passive/unclear ascertainment",
    "Zero-event wording with active/clear ascertainment",
    "Confidence interval reported"
  ),
  n = c(
    nrow(ae_rate_detail),
    sum(ae_rate_detail$numerator_reported),
    sum(ae_rate_detail$denominator_reported),
    sum(ae_rate_detail$definition_reported),
    sum(ae_rate_detail$time_window_reported),
    sum(ae_rate_detail$rate_core_complete),
    sum(ae_rate_detail$zero_event_wording),
    sum(ae_rate_detail$zero_event_ambiguous),
    sum(ae_rate_detail$zero_event_wording & ae_rate_detail$zero_meaning_clear),
    sum(has_substantive_text(ae_rate_detail$confidence_interval))
  ),
  denominator = nrow(ae_rate_detail)
) %>%
  dplyr::mutate(percent = safe_pct(.data$n, .data$denominator))

# ---- 7. Harms and failure modes: field-specific mechanism taxonomy ------------

harm_dictionary <- tibble::tribble(
  ~domain, ~tag, ~pattern,
  "Clinical and psychological", "Symptom deterioration or distress",
  "symptom deterioration|worsen(?:ed|ing)?|felt worse|increased distress|iatrogenic|dysphoria|anxiety exacerb|psychological distress",
  "Clinical and psychological", "Unsafe, harmful or inappropriate content/advice",
  "unsafe content|harmful content|harmful advice|inappropriate advice|dangerous advice|incorrect advice|misleading advice|medical advice|triggering content|encourag(?:e|ed).*self[- ]harm|diet advice",
  "Clinical and psychological", "Crisis-detection or routing failure",
  "crisis[- ]routing failure|failed to (?:detect|respond|refer|escalate)|missed suicidal|missed risk|suicid.*not (?:detect|recogn|flag)|inadequate crisis|no hotline referral|failed.*self[- ]harm",
  "Clinical and psychological", "Over-reliance, dependency or delayed human care",
  "over[- ]reliance|overreliance|dependen(?:cy|ce)|emotional attachment|replace.*therap|delay(?:ed)? care|avoid.*professional|substitut(?:e|ion).*human|anthropomorph",
  "Model and content", "Hallucination, fabrication or factual inaccuracy",
  "hallucinat(?:e|ed|ion)|fabricat(?:e|ed|ion)|factual(?:ly)? incorrect|inaccurate information|false information|non[- ]existent article|made[- ]up",
  "Model and content", "Sycophancy or harmful over-validation",
  "sycophan|over[- ]validat|overly agreeable|overly flattering|agreeable response|reinforc.*delusion|validat.*harmful",
  "Model and content", "Persona or model drift / instability",
  "persona drift|model drift|personality change|persona.*break|role[- ]play confusion|off[- ]script|response instability|became.*cold|became.*heartless",
  "Model and content", "Jailbreak or prompt manipulation",
  "jailbreak|prompt injection|prompt manipulation|adversarial prompt|bypass.*guardrail|circumvent.*safety",
  "Privacy and governance", "Privacy, confidentiality or data-use risk",
  "privacy|confidential|data breach|data security|data leak|data use|data sharing|data ownership|sold|traded|marketed|training data|unauthori[sz]ed access",
  "Equity and access", "Bias, stereotyping or discrimination",
  "algorithmic bias|gender bias|racial bias|stereotyp|discriminat|racis|sexism|misgender|under[- ]represented",
  "Equity and access", "Cultural or linguistic mismatch",
  "cultural mismatch|culturally inappropriate|language barrier|language mismatch|translation error|local language|dialect|western[- ]centric|cultural relevance",
  "Equity and access", "Digital exclusion or widening inequity",
  "digital divide|digital exclusion|unequal access|widen.*inequit|access inequity|device access|internet access|connectivity|data cost|low literacy|disability access|socioeconomic",
  "Implementation", "Trust loss, frustration or disengagement",
  "loss of trust|mistrust|frustrat(?:ed|ion)|disengag(?:ed|ement)|dropout|abandon(?:ed|ment)|repetitive|irrelevant response",
  "Implementation", "Technical failure or interrupted access",
  "technical failure|system failure|software error|bug|crash|downtime|failed to load|unavailable"
)

harm_source_fields <- c(
  "1d.1 value", "1d.3 value", "1d.9 value", "1d.10 value"
)

harm_tags <- tag_fields(master, harm_source_fields, harm_dictionary) %>%
  dplyr::left_join(
    master %>%
      dplyr::select(
        record_id, study_design, design_group, country,
        architecture_class, adolescent_or_young_direct, genai_direct,
        south_africa_direct, mhaa_all_three_direct
      ),
    by = "record_id"
  ) %>%
  dplyr::mutate(
    evidential_basis = dplyr::case_when(
      .data$source_field == "1d.9 value" ~ "Documented deployment/drift-type failure",
      .data$source_field == "1d.3 value" ~ "Documented failure mode",
      .data$source_field == "1d.10 value" ~ "Equity-related harm or risk",
      stringr::str_detect(
        .data$text,
        stringr::regex(
          "adverse event|experienced|occurred|withdrew|reported by users|staff intervention|hospitali[sz]",
          ignore_case = TRUE
        )
      ) ~ "Observed/reported harm wording",
      TRUE ~ "Harm or risk described"
    )
  )

harm_counts <- harm_tags %>%
  dplyr::distinct(.data$record_id, .data$domain, .data$tag) %>%
  dplyr::count(.data$domain, .data$tag, name = "n_records") %>%
  dplyr::mutate(
    denominator = stream_n,
    percent = safe_pct(.data$n_records, .data$denominator),
    interpretation = "Descriptive reporting count only; not a measure of importance or effect"
  ) %>%
  dplyr::arrange(.data$domain, .data$tag)

harm_domain_counts <- harm_tags %>%
  dplyr::distinct(.data$record_id, .data$domain) %>%
  dplyr::count(.data$domain, name = "n_records") %>%
  dplyr::mutate(
    denominator = stream_n,
    percent = safe_pct(.data$n_records, .data$denominator)
  )

# ---- 8. Detection, mitigation and governance: field-specific taxonomy ---------

mitigation_dictionary <- tibble::tribble(
  ~domain, ~tag, ~pattern,
  "Detection", "Human review or clinical assessment",
  "human review|clinician review|clinical review|psychologist review|expert review|manual review|human moderation|health professional",
  "Detection", "Automated classifier or model-based detection",
  "llm classifier|risk classifier|automated classifier|machine learning classifier|nlp algorithm|model[- ]based detection|automated detection",
  "Detection", "Rule-based or keyword safety detection",
  "rule[- ]based detection|keyword detection|keyword match|lexicon|trigger phrase|safety net detection|threshold.*trigger",
  "Detection", "User report or feedback signal",
  "user report|participant report|self[- ]report|feedback mechanism|flag response|report content",
  "Safeguarding and escalation", "Predefined crisis or safety response",
  "predefined crisis|scripted crisis|crisis protocol|standard crisis response|emergency response|safety plan|crisis plan|crisis resource",
  "Safeguarding and escalation", "Human escalation, referral or follow-through",
  "human escalation|escalat(?:e|ion)|handoff|hand[- ]off|refer(?:ral)?|route.*professional|contact.*clinician|emergency services|helpline|hotline|psychiatric services",
  "Safeguarding and escalation", "Human oversight or moderation",
  "human oversight|human[- ]in[- ]the[- ]loop|moderation|clinician oversight|professional oversight|supervision|research team notified",
  "Model and content controls", "Bounded or rule-based safety layer",
  "rule[- ]based safety|deterministic safety|bounded function|predefined response|constrained response|guardrail|safety layer|hard[- ]coded",
  "Model and content controls", "Approved, curated or RAG-grounded content",
  "approved content|curated content|validated content|clinician[- ]approved|expert[- ]reviewed|evidence[- ]based corpus|knowledge base|retrieval[- ]augmented|\\brag\\b",
  "Model and content controls", "Content filtering or blocking",
  "content filter|safety filter|toxicity filter|blocked content|blocklist|allowlist|prohibited content",
  "Model and content controls", "Prompt or model configuration constraints",
  "system prompt|prompt constraint|prompt engineering|temperature|fine[- ]tun|model configuration|instruction hierarchy|few[- ]shot",
  "Monitoring and assurance", "Adverse-event or safety monitoring",
  "adverse event monitoring|monitor.*adverse|safety monitoring|harm monitoring|deterioration monitoring|clinical monitoring|DSMB|data safety monitoring",
  "Monitoring and assurance", "Logging, incident register or audit trail",
  "audit trail|logging|conversation log|incident register|incident log|traceability|post[- ]deployment monitoring|surveillance",
  "Monitoring and assurance", "Red-teaming or adversarial testing",
  "red team|red[- ]team|jailbreak test|adversarial test|stress test|safety test|penetration test",
  "Monitoring and assurance", "Model update, drift monitoring or change control",
  "model update|content update|change control|version control|revalidation|re[- ]validate|drift monitoring|versioning",
  "Privacy and governance", "Privacy-by-design or data protection",
  "privacy by design|data minimi[sz]|de[- ]identif|anonymi[sz]|pseudonymi[sz]|encrypt|access control|secure storage|authentication|data protection|privacy notice",
  "Equity and inclusion", "Bias, fairness or subgroup testing",
  "bias test|fairness test|subgroup analysis|equity audit|algorithmic audit|disparity monitoring",
  "Equity and inclusion", "Cultural, linguistic or accessibility adaptation",
  "cultural adaptation|locali[sz]ation|local language|translation review|cultural review|community input|accessibility|low bandwidth|plain language|easy read"
)

mitigation_source_fields <- c(
  "1d.4 value", "1d.5 value", "1d.6 value", "1d.8 value"
)

mitigation_tags <- tag_fields(
  master,
  mitigation_source_fields,
  mitigation_dictionary
) %>%
  dplyr::mutate(
    implementation_status = classify_implementation(.data$text)
  ) %>%
  dplyr::left_join(
    master %>%
      dplyr::select(
        record_id, study_design, design_group, country,
        architecture_class, adolescent_or_young_direct, genai_direct,
        south_africa_direct, mhaa_all_three_direct
      ),
    by = "record_id"
  )

mitigation_counts <- mitigation_tags %>%
  dplyr::distinct(
    .data$record_id, .data$domain, .data$tag, .data$implementation_status
  ) %>%
  dplyr::count(
    .data$domain, .data$tag, .data$implementation_status,
    name = "n_records"
  ) %>%
  dplyr::mutate(
    denominator = stream_n,
    percent = safe_pct(.data$n_records, .data$denominator),
    interpretation = "Reporting count; does not establish mitigation effectiveness"
  ) %>%
  dplyr::arrange(.data$domain, .data$tag, .data$implementation_status)

mitigation_status_counts <- mitigation_tags %>%
  dplyr::distinct(.data$record_id, .data$implementation_status) %>%
  dplyr::count(.data$implementation_status, name = "n_records") %>%
  dplyr::mutate(
    denominator = stream_n,
    percent = safe_pct(.data$n_records, .data$denominator)
  )

governance_detail <- master %>%
  dplyr::filter(.data$has_1d_8) %>%
  dplyr::transmute(
    record_id, title,
    governance_text = .data[["1d.8 value"]],
    quote = .data[["1d.8 quotes"]],
    location = .data[["1d.8 loc"]],
    agreement = .data[["1d.8 agree"]],
    implementation_status = classify_implementation(.data[["1d.8 value"]])
  )

governance_status_counts <- governance_detail %>%
  dplyr::count(.data$implementation_status, name = "n_records") %>%
  dplyr::mutate(
    denominator = nrow(governance_detail),
    percent = safe_pct(.data$n_records, .data$denominator)
  )

# ---- 9. Evidence-base profile --------------------------------------------------

study_design_counts <- standard_count(contributing, "study_design") %>%
  dplyr::rename(study_design = category) %>%
  dplyr::mutate(
    denominator = contributing_n,
    percent = safe_pct(.data$n, .data$denominator)
  )

design_group_counts <- contributing %>%
  dplyr::count(.data$design_group, name = "n") %>%
  dplyr::mutate(
    denominator = contributing_n,
    percent = safe_pct(.data$n, .data$denominator)
  )

architecture_counts <- contributing %>%
  dplyr::count(.data$architecture_class, name = "n") %>%
  dplyr::mutate(
    denominator = contributing_n,
    percent = safe_pct(.data$n, .data$denominator)
  )

geography_counts <- tibble::tibble(
  geography = c("L&MIC", "Sub-Saharan Africa", "South Africa"),
  n = c(
    sum(dplyr::coalesce(contributing$lmic, FALSE)),
    sum(dplyr::coalesce(contributing$ssa, FALSE)),
    sum(dplyr::coalesce(contributing$south_africa, FALSE))
  ),
  denominator = contributing_n
) %>%
  dplyr::mutate(percent = safe_pct(.data$n, .data$denominator))

country_counts <- contributing %>%
  dplyr::transmute(
    country = dplyr::if_else(has_text(.data$country), .data$country, "Not reported")
  ) %>%
  tidyr::separate_rows(country, sep = "\\s*,\\s*") %>%
  dplyr::mutate(country = stringr::str_squish(.data$country)) %>%
  dplyr::count(.data$country, name = "n_records", sort = TRUE) %>%
  dplyr::mutate(
    denominator = contributing_n,
    percent = safe_pct(.data$n_records, .data$denominator)
  )

directness_counts <- tibble::tibble(
  directness_dimension = c(
    "Adolescent or young-person population",
    "Generative AI / LLM",
    "South Africa",
    "Population + GenAI + South Africa"
  ),
  n = c(
    sum(contributing$adolescent_or_young_direct),
    sum(contributing$genai_direct),
    sum(contributing$south_africa_direct),
    sum(contributing$mhaa_all_three_direct)
  ),
  denominator = contributing_n
) %>%
  dplyr::mutate(percent = safe_pct(.data$n, .data$denominator))

# ---- 10. Two-panel visual: harms and safeguards -------------------------------

# The figure is descriptive. Bar length shows the number of unique records in
# which a category was identified. It does not indicate severity, causal effect,
# effectiveness, or priority. Evidential/implementation status is shown because
# observed failures, test findings, stakeholder concerns and proposed safeguards
# should not be treated as equivalent.

# Number of categories displayed in each panel. Increase these values to show
# more categories, but the figure may become difficult to read.
max_harm_categories <- 12
max_mitigation_categories <- 12

# ---- 10a. Classify the evidential basis of each harm/failure mention -----------

classify_harm_figure_basis <- function(text, source_field) {
  text <- dplyr::coalesce(as.character(text), "")
  source_field <- dplyr::coalesce(as.character(source_field), "")

  observed_or_deployment <- source_field == "1d.9 value" |
    stringr::str_detect(
      text,
      stringr::regex(
        paste0(
          "experienced|occurred|happened|reported by (?:a )?(?:participant|user|patient|family)|",
          "users? reported|participants? reported|withdrew|withdrawal attributable|",
          "hospitali[sz]|serious adverse event|adverse event|symptom worsening|",
          "required withdrawal|removed from deployment|withdrawn after deployment|",
          "rolled back|incident occurred|staff intervention|harmful advice was provided"
        ),
        ignore_case = TRUE
      )
    )

  identified_in_testing <- stringr::str_detect(
    text,
    stringr::regex(
      paste0(
        "simulat(?:ed|ion)|expert rater|system evaluation|benchmark|",
        "test case|scenario testing|research(?:er| team).*evaluat|",
        "evaluat(?:ed|ion).*response|adversarial test|red[- ]team|",
        "development testing|development experience|transcript review"
      ),
      ignore_case = TRUE
    )
  )

  user_or_stakeholder_concern <- stringr::str_detect(
    text,
    stringr::regex(
      paste0(
        "participant.*concern|user.*concern|patient.*concern|",
        "stakeholder|caregiver|parent.*concern|clinician.*concern|",
        "provider.*concern|raised.*concern|perceived risk|",
        "acceptability concern|reported as a barrier"
      ),
      ignore_case = TRUE
    )
  )

  anticipated_or_theoretical <- stringr::str_detect(
    text,
    stringr::regex(
      paste0(
        "potential|possible|may |might |could |risk of|ethical risk|",
        "hypothetical|theoretical|authors? (?:note|warn|caution)|",
        "concern that|could inadvertently|may inadvertently|",
        "recommend(?:ed|ation)?"
      ),
      ignore_case = TRUE
    )
  )

  dplyr::case_when(
    observed_or_deployment ~ "Observed/reported or deployment failure",
    identified_in_testing ~ "Identified through testing/evaluation",
    user_or_stakeholder_concern ~ "User/stakeholder concern",
    anticipated_or_theoretical ~ "Anticipated/theoretical risk",
    TRUE ~ "Basis unclear from extraction"
  )
}

harm_figure_rows <- harm_tags %>%
  dplyr::mutate(
    figure_basis = classify_harm_figure_basis(
      .data$text,
      .data$source_field
    )
  )

# Assign one evidential basis to each record-category pair using a conservative
# hierarchy. This prevents one record being counted more than once in the same
# bar merely because the category appeared in several RQ1d fields.
harm_basis_levels <- c(
  "Observed/reported or deployment failure",
  "Identified through testing/evaluation",
  "User/stakeholder concern",
  "Anticipated/theoretical risk",
  "Basis unclear from extraction"
)

harm_figure_record_tag <- harm_figure_rows %>%
  dplyr::mutate(
    basis_rank = match(.data$figure_basis, harm_basis_levels)
  ) %>%
  dplyr::group_by(.data$record_id, .data$domain, .data$tag) %>%
  dplyr::slice_min(
    order_by = .data$basis_rank,
    n = 1,
    with_ties = FALSE
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    .data$record_id, .data$domain, .data$tag,
    evidential_basis = .data$figure_basis
  )

harm_figure_counts_all <- harm_figure_record_tag %>%
  dplyr::count(
    .data$domain, .data$tag, .data$evidential_basis,
    name = "n_records"
  ) %>%
  dplyr::group_by(.data$domain, .data$tag) %>%
  dplyr::mutate(total_records = sum(.data$n_records)) %>%
  dplyr::ungroup()

harm_categories_shown <- harm_figure_counts_all %>%
  dplyr::distinct(.data$domain, .data$tag, .data$total_records) %>%
  dplyr::slice_max(
    order_by = .data$total_records,
    n = max_harm_categories,
    with_ties = FALSE
  ) %>%
  dplyr::arrange(.data$total_records, .data$tag)

harm_order <- harm_categories_shown$tag

harm_plot_data <- harm_figure_counts_all %>%
  dplyr::semi_join(
    harm_categories_shown,
    by = c("domain", "tag", "total_records")
  ) %>%
  dplyr::mutate(
    tag = factor(.data$tag, levels = harm_order),
    evidential_basis = factor(
      .data$evidential_basis,
      levels = rev(harm_basis_levels)
    ),
    label = dplyr::if_else(
      .data$n_records >= 5,
      as.character(.data$n_records),
      ""
    ),
    label_colour = dplyr::if_else(
      .data$evidential_basis == "Observed/reported or deployment failure",
      "white",
      "#1F1F1F"
    )
  )

harm_totals <- harm_categories_shown %>%
  dplyr::mutate(tag = factor(.data$tag, levels = harm_order))

# ---- 10b. Classify whether safeguards were implemented, tested or proposed ----

mitigation_testing_pattern <- paste0(
  "evaluat(?:ed|ion)|assess(?:ed|ment)|tested|validation|validated|",
  "performance metric|sensitivity|specificity|precision|recall|accuracy|",
  "false positive|false negative|benchmark|simulation|scenario test|",
  "red[- ]team|adversarial test|audit|iterative testing|",
  "expert review of transcripts|reviewed transcripts"
)

mitigation_figure_rows <- mitigation_tags %>%
  dplyr::mutate(
    tested_or_evaluated = stringr::str_detect(
      dplyr::coalesce(.data$text, ""),
      stringr::regex(mitigation_testing_pattern, ignore_case = TRUE)
    ),
    implemented_flag = .data$implementation_status %in% c(
      "Implemented/operational",
      "Mixed operational and proposed"
    ),
    proposed_flag = .data$implementation_status %in% c(
      "Proposed/recommended only",
      "Mixed operational and proposed"
    )
  )

# Aggregate multiple mentions of the same safeguard within one record before
# counting. "Implemented and tested/evaluated" means the extraction reports both
# implementation and some assessment activity; it does not mean effectiveness
# was demonstrated.
mitigation_figure_record_tag <- mitigation_figure_rows %>%
  dplyr::group_by(.data$record_id, .data$domain, .data$tag) %>%
  dplyr::summarise(
    any_implemented = any(.data$implemented_flag),
    any_proposed = any(.data$proposed_flag),
    any_tested = any(.data$implemented_flag & .data$tested_or_evaluated),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    implementation_evidence = dplyr::case_when(
      .data$any_implemented & .data$any_proposed ~
        "Mixed implemented and proposed",
      .data$any_tested ~ "Implemented and tested/evaluated",
      .data$any_implemented ~ "Implemented; evaluation not reported",
      .data$any_proposed ~ "Proposed/recommended only",
      TRUE ~ "Status unclear from extraction"
    )
  ) %>%
  dplyr::select(
    .data$record_id, .data$domain, .data$tag,
    .data$implementation_evidence
  )

mitigation_status_levels <- c(
  "Implemented and tested/evaluated",
  "Implemented; evaluation not reported",
  "Mixed implemented and proposed",
  "Proposed/recommended only",
  "Status unclear from extraction"
)

mitigation_figure_counts_all <- mitigation_figure_record_tag %>%
  dplyr::count(
    .data$domain, .data$tag, .data$implementation_evidence,
    name = "n_records"
  ) %>%
  dplyr::group_by(.data$domain, .data$tag) %>%
  dplyr::mutate(total_records = sum(.data$n_records)) %>%
  dplyr::ungroup()

mitigation_categories_shown <- mitigation_figure_counts_all %>%
  dplyr::distinct(.data$domain, .data$tag, .data$total_records) %>%
  dplyr::slice_max(
    order_by = .data$total_records,
    n = max_mitigation_categories,
    with_ties = FALSE
  ) %>%
  dplyr::arrange(.data$total_records, .data$tag)

mitigation_order <- mitigation_categories_shown$tag

mitigation_plot_data <- mitigation_figure_counts_all %>%
  dplyr::semi_join(
    mitigation_categories_shown,
    by = c("domain", "tag", "total_records")
  ) %>%
  dplyr::mutate(
    tag = factor(.data$tag, levels = mitigation_order),
    implementation_evidence = factor(
      .data$implementation_evidence,
      levels = rev(mitigation_status_levels)
    ),
    label = dplyr::if_else(
      .data$n_records >= 5,
      as.character(.data$n_records),
      ""
    ),
    label_colour = dplyr::if_else(
      .data$implementation_evidence ==
        "Implemented and tested/evaluated",
      "white",
      "#1F1F1F"
    )
  )

mitigation_totals <- mitigation_categories_shown %>%
  dplyr::mutate(tag = factor(.data$tag, levels = mitigation_order))

# ---- 10c. Shared figure styling ----------------------------------------------

figure_background <- "#F3F3F3"
figure_text <- "#1F1F1F"
figure_subtext <- "#505050"

harm_palette <- c(
  "Observed/reported or deployment failure" = "#3778B4",
  "Identified through testing/evaluation" = "#6F99C5",
  "User/stakeholder concern" = "#9CB8D6",
  "Anticipated/theoretical risk" = "#C1D2E4",
  "Basis unclear from extraction" = "#DEE7F0"
)

mitigation_palette <- c(
  "Implemented and tested/evaluated" = "#3778B4",
  "Implemented; evaluation not reported" = "#6F99C5",
  "Mixed implemented and proposed" = "#9CB8D6",
  "Proposed/recommended only" = "#C1D2E4",
  "Status unclear from extraction" = "#DEE7F0"
)

base_panel_theme <- ggplot2::theme_minimal(
  base_size = 14,
  base_family = "sans"
) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(
      fill = figure_background,
      colour = NA
    ),
    plot.background = ggplot2::element_rect(
      fill = figure_background,
      colour = NA
    ),
    axis.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(
      size = 12.5,
      colour = "#404040",
      margin = ggplot2::margin(r = 10)
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = ggplot2::element_text(
      size = 10.5,
      colour = "#404040"
    ),
    legend.key.width = grid::unit(0.9, "cm"),
    legend.key.height = grid::unit(0.42, "cm"),
    plot.title = ggplot2::element_text(
      size = 18,
      face = "bold",
      colour = figure_text,
      margin = ggplot2::margin(b = 8)
    ),
    plot.subtitle = ggplot2::element_text(
      size = 11.5,
      colour = figure_subtext,
      margin = ggplot2::margin(b = 14)
    ),
    plot.margin = ggplot2::margin(t = 8, r = 34, b = 8, l = 8)
  )

# ---- 10d. Panel A: harms and failure modes -----------------------------------

p_harms <- ggplot2::ggplot(
  harm_plot_data,
  ggplot2::aes(
    x = .data$tag,
    y = .data$n_records,
    fill = .data$evidential_basis
  )
) +
  ggplot2::geom_col(width = 0.74, colour = NA) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = .data$label,
      colour = .data$label_colour
    ),
    position = ggplot2::position_stack(vjust = 0.5),
    size = 4.2,
    show.legend = FALSE
  ) +
  ggplot2::geom_text(
    data = harm_totals,
    ggplot2::aes(
      x = .data$tag,
      y = .data$total_records,
      label = .data$total_records
    ),
    inherit.aes = FALSE,
    hjust = -0.25,
    size = 4.8,
    colour = figure_text
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(
    breaks = NULL,
    labels = NULL,
    expand = ggplot2::expansion(mult = c(0, 0.13))
  ) +
  ggplot2::scale_fill_manual(
    values = harm_palette,
    drop = FALSE
  ) +
  ggplot2::scale_colour_identity() +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      nrow = 2,
      byrow = TRUE,
      reverse = TRUE,
      title = NULL
    )
  ) +
  ggplot2::labs(
    title = "A. Harms and failure modes",
    subtitle = paste0(
      "Unique record counts by category and evidential basis; ",
      "the largest ", max_harm_categories, " categories are shown."
    )
  ) +
  base_panel_theme

# ---- 10e. Panel B: detection, mitigation and governance ----------------------

p_safeguards <- ggplot2::ggplot(
  mitigation_plot_data,
  ggplot2::aes(
    x = .data$tag,
    y = .data$n_records,
    fill = .data$implementation_evidence
  )
) +
  ggplot2::geom_col(width = 0.74, colour = NA) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = .data$label,
      colour = .data$label_colour
    ),
    position = ggplot2::position_stack(vjust = 0.5),
    size = 4.2,
    show.legend = FALSE
  ) +
  ggplot2::geom_text(
    data = mitigation_totals,
    ggplot2::aes(
      x = .data$tag,
      y = .data$total_records,
      label = .data$total_records
    ),
    inherit.aes = FALSE,
    hjust = -0.25,
    size = 4.8,
    colour = figure_text
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(
    breaks = NULL,
    labels = NULL,
    expand = ggplot2::expansion(mult = c(0, 0.13))
  ) +
  ggplot2::scale_fill_manual(
    values = mitigation_palette,
    drop = FALSE
  ) +
  ggplot2::scale_colour_identity() +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      nrow = 2,
      byrow = TRUE,
      reverse = TRUE,
      title = NULL
    )
  ) +
  ggplot2::labs(
    title = "B. Detection, mitigation and governance measures",
    subtitle = paste0(
      "Unique record counts by category and implementation status; ",
      "the largest ", max_mitigation_categories, " categories are shown."
    )
  ) +
  base_panel_theme

# ---- 10f. Combine and save ----------------------------------------------------

n_postlaunch_monitoring <- sum(master$has_1d_8)

p_safety_two_panel <- (p_harms / p_safeguards) +
  patchwork::plot_layout(heights = c(1, 1.05)) +
  patchwork::plot_annotation(
    title = "Harms, failure modes and safety responses reported in RQ1d records",
    subtitle = paste0(
      "Non-mutually exclusive record-level categories. Bar lengths are descriptive ",
      "counts and do not indicate severity or effectiveness."
    ),
    caption = paste0(
      "Post-launch monitoring/governance information was extracted for n=",
      n_postlaunch_monitoring, " of ", stream_n, " records. ",
      "Implementation or testing does not establish that a safeguard reduced harm. ",
      "Counts refer to records, not independent studies."
    ),
    theme = ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = figure_background,
        colour = NA
      ),
      plot.title = ggplot2::element_text(
        size = 24,
        face = "bold",
        colour = figure_text,
        margin = ggplot2::margin(b = 7)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 14,
        colour = figure_subtext,
        margin = ggplot2::margin(b = 14)
      ),
      plot.caption = ggplot2::element_text(
        size = 10,
        colour = figure_subtext,
        hjust = 0,
        margin = ggplot2::margin(t = 12)
      ),
      plot.margin = ggplot2::margin(t = 18, r = 24, b = 18, l = 18)
    )
  ) &
  ggplot2::theme(
    plot.background = ggplot2::element_rect(
      fill = figure_background,
      colour = NA
    )
  )

ggplot2::ggsave(
  filename = file.path(output_dir, "RQ1d_harms_and_safeguards_two_panel.png"),
  plot = p_safety_two_panel,
  width = 13.5,
  height = 15.5,
  dpi = 300,
  bg = figure_background
)

# ---- 11. Five-record quote-level verification sample --------------------------

target_verification_fields <- c("1d.2", "1d.9", "1d.5", "1d.8", "1d.10")
selected_verification_ids <- character()

for (target_field in target_verification_fields) {
  candidates <- rq1d_long %>%
    dplyr::filter(
      .data$field_id == target_field,
      has_text(.data$value),
      has_text(.data$quotes),
      !(.data$record_id %in% selected_verification_ids)
    ) %>%
    dplyr::arrange(.data$record_id) %>%
    dplyr::pull(.data$record_id)

  if (length(candidates) > 0) {
    selected_verification_ids <- c(selected_verification_ids, candidates[[1]])
  }
}

if (length(selected_verification_ids) < 5) {
  fill_ids <- rq1d_long %>%
    dplyr::filter(
      has_text(.data$value),
      has_text(.data$quotes),
      !(.data$record_id %in% selected_verification_ids)
    ) %>%
    dplyr::arrange(.data$record_id, field_number(.data$field_id)) %>%
    dplyr::distinct(.data$record_id) %>%
    dplyr::pull(.data$record_id)

  selected_verification_ids <- unique(
    c(selected_verification_ids, fill_ids)
  )[seq_len(min(5, length(unique(c(selected_verification_ids, fill_ids)))))]
}

verification_sample <- rq1d_long %>%
  dplyr::filter(.data$record_id %in% selected_verification_ids) %>%
  dplyr::filter(has_text(.data$value) | has_text(.data$quotes)) %>%
  dplyr::mutate(
    selected_record_order = match(.data$record_id, selected_verification_ids),
    quote_available = has_text(.data$quotes),
    location_available = has_text(.data$loc),
    manual_check_required = TRUE
  ) %>%
  dplyr::arrange(.data$selected_record_order, field_number(.data$field_id)) %>%
  dplyr::select(
    selected_record_order, record_id, title, field_id, label,
    value, quotes, loc, agree, quote_available, location_available,
    manual_check_required
  )

verification_summary <- verification_sample %>%
  dplyr::group_by(.data$record_id, .data$title) %>%
  dplyr::summarise(
    fields_shown = paste(.data$field_id, collapse = ", "),
    all_rows_have_quotes = all(.data$quote_available),
    all_rows_have_locations = all(.data$location_available),
    any_conflict = any(
      stringr::str_to_lower(dplyr::coalesce(.data$agree, "")) == "conflict"
    ),
    status = "Selected for manual value-versus-quote/source verification",
    .groups = "drop"
  )

# ---- 12. Review queue and quote bank ------------------------------------------

review_queue_1d <- if (!is.null(review_queue_raw)) {
  require_columns(review_queue_raw, c("record_id", "group", "field_id"), "Review queue")
  review_queue_raw %>%
    dplyr::filter(.data$group == "1d") %>%
    dplyr::arrange(.data$record_id, .data$field_id)
} else {
  empty_review_queue()
}

quote_bank <- rq1d_long %>%
  dplyr::filter(has_text(.data$value) | has_text(.data$quotes)) %>%
  dplyr::select(
    record_id, title, field_id, label, value, quotes, loc, agree,
    study_design, design_group, country, setting, age_as_reported,
    object_type, architecture_class, adolescent_or_young_direct,
    genai_direct, south_africa_direct, mhaa_all_three_direct
  ) %>%
  dplyr::arrange(field_number(.data$field_id), .data$record_id)

# ---- 13. Helpers for mechanism-based author-year citations -------------------

# Author-year labels for records cited in the generated narrative. These labels
# were checked against the baseline bibliographic field in the source workbook.
# Keep record_id in supporting tables for auditability, while using author-year
# labels in report prose.
citation_lookup <- tibble::tribble(
  ~record_id, ~citation_label,
  "25CCVACX", "Fujita et al., 2024",
  "2ERB5LMK", "Guo et al., 2024",
  "2HWTYAYN", "Villarreal-Zegarra et al., 2026",
  "2VVVJMPU", "Nicoletti et al., 2025",
  "2XH5HFH9", "He et al., 2022",
  "3C49MIHM", "Luo et al., 2024",
  "3C7P3Q3D", "Wu et al., 2026",
  "3YYJ3T9I", "Bragazzi et al., 2023",
  "52K5G2XE", "Vaidyam et al., 2019",
  "589B53XW", "Nepal et al., 2024",
  "5CFMCI3V", "Reichenpfader et al., 2025",
  "5RIYFHZJ", "Vogt et al., 2025",
  "5WCF85I8", "Potts et al., 2021",
  "5ZTTWPVP", "Kambar et al., 2026",
  "6283I4FE", "Cross et al., 2025",
  "646VG48B", "Pinna et al., 2026",
  "6XA9SS8X", "Kuta et al., 2026",
  "A4C7CN6S", "Feng et al., 2025",
  "A655XJ52", "Martin & Richmond, 2023",
  "B3BBDFIY", "Humphreys et al., 2025",
  "BP54XTRB", "Xygkou-Tsiamoulou, 2024",
  "CCANQ4QY", "Olisaeloka et al., 2026",
  "CJ3GR52J", "Humphries et al., 2026",
  "CQNUWVPX", "Zhang, 2025",
  "D6S3TBS9", "Dechant et al., 2025",
  "DYE6GFYY", "Kim, 2024",
  "E2WZUDCN", "Chamarthi et al., 2026",
  "F3GJBSRH", "Mansoor et al., 2025",
  "KGMTWILF", "Atinga et al., 2026",
  "LNR5GJCE", "Im & Woo, 2025",
  "M99HZERC", "Zisquit et al., 2025",
  "Q5B3VGPX", "Sawesi et al., 2026",
  "QIWP8DXB", "Tulbure et al., 2026",
  "QKV238ZE", "Anaduaka et al., 2025",
  "SER7KR8A", "Roh et al., 2026",
  "USRGRGRJ", "Yu et al., 2026",
  "UU3JSM2W", "Balan & Gumpel, 2025",
  "VN82DGR2", "Hasei et al., 2025",
  "XJBXG9QY", "Hanzawa et al., 2026"
)

format_citation_list <- function(ids, max_ids = 8) {
  ids <- unique(ids[has_text(ids)])
  if (length(ids) == 0) return("none identified")

  shown_ids <- utils::head(ids, max_ids)
  labels <- citation_lookup %>%
    dplyr::right_join(
      tibble::tibble(record_id = shown_ids, citation_order = seq_along(shown_ids)),
      by = "record_id"
    ) %>%
    dplyr::arrange(.data$citation_order) %>%
    dplyr::mutate(
      citation_label = dplyr::coalesce(
        .data$citation_label,
        paste0("record ", .data$record_id)
      )
    ) %>%
    dplyr::pull(.data$citation_label)

  suffix <- if (length(ids) > max_ids) {
    paste0("; plus ", length(ids) - max_ids, " other records")
  } else {
    ""
  }

  paste0(paste(labels, collapse = "; "), suffix)
}

format_citation_id_list <- function(ids, max_ids = 5) {
  ids <- unique(ids[has_text(ids)])
  if (length(ids) == 0) return("none identified")
  shown_ids <- utils::head(ids, max_ids)
  labels <- citation_lookup %>%
    dplyr::right_join(
      tibble::tibble(record_id = shown_ids, citation_order = seq_along(shown_ids)),
      by = "record_id"
    ) %>%
    dplyr::arrange(.data$citation_order) %>%
    dplyr::mutate(
      citation_label = dplyr::coalesce(
        .data$citation_label,
        paste0("record ", .data$record_id)
      ),
      labelled_id = paste0(.data$citation_label, " [", .data$record_id, "]")
    ) %>%
    dplyr::pull(.data$labelled_id)
  paste(labels, collapse = "; ")
}


ids_for_harm <- function(tag_name, max_ids = 8) {
  ids <- harm_tags %>%
    dplyr::filter(.data$tag == tag_name) %>%
    dplyr::arrange(
      dplyr::desc(.data$mhaa_all_three_direct),
      dplyr::desc(.data$south_africa_direct),
      dplyr::desc(.data$adolescent_or_young_direct),
      dplyr::desc(.data$genai_direct),
      .data$record_id
    ) %>%
    dplyr::pull(.data$record_id)
  format_citation_list(ids, max_ids)
}

ids_for_mitigation <- function(status_filter = NULL, max_ids = 8) {
  data <- mitigation_tags
  if (!is.null(status_filter)) {
    data <- data %>% dplyr::filter(.data$implementation_status %in% status_filter)
  }
  ids <- data %>%
    dplyr::arrange(
      dplyr::desc(.data$mhaa_all_three_direct),
      dplyr::desc(.data$south_africa_direct),
      dplyr::desc(.data$adolescent_or_young_direct),
      dplyr::desc(.data$genai_direct),
      .data$record_id
    ) %>%
    dplyr::pull(.data$record_id)
  format_citation_list(ids, max_ids)
}

n_ae_core_complete <- sum(ae_rate_detail$rate_core_complete)
n_zero_ambiguous <- sum(ae_rate_detail$zero_event_ambiguous)
n_causality <- field_count("1d.7")
n_governance <- field_count("1d.8")
n_equity <- field_count("1d.10")

n_governance_operational <- governance_status_counts %>%
  dplyr::filter(.data$implementation_status == "Implemented/operational") %>%
  dplyr::summarise(n = sum(.data$n_records)) %>%
  dplyr::pull(.data$n)
if (length(n_governance_operational) == 0) n_governance_operational <- 0L

n_governance_proposed <- governance_status_counts %>%
  dplyr::filter(.data$implementation_status == "Proposed/recommended only") %>%
  dplyr::summarise(n = sum(.data$n_records)) %>%
  dplyr::pull(.data$n)
if (length(n_governance_proposed) == 0) n_governance_proposed <- 0L

n_governance_mixed <- governance_status_counts %>%
  dplyr::filter(.data$implementation_status == "Mixed operational and proposed") %>%
  dplyr::summarise(n = sum(.data$n_records)) %>%
  dplyr::pull(.data$n)
if (length(n_governance_mixed) == 0) n_governance_mixed <- 0L

n_governance_unclear <- governance_status_counts %>%
  dplyr::filter(.data$implementation_status == "Unclear from extraction") %>%
  dplyr::summarise(n = sum(.data$n_records)) %>%
  dplyr::pull(.data$n)
if (length(n_governance_unclear) == 0) n_governance_unclear <- 0L

# ---- 14. Section 4.5 report ----------------------------------------------------

report_lines <- c(
  "# 4.5 Safety",
  "",
  "**Review question.** What harms, adverse events, risks, and unintended consequences have been reported, and how have interventions sought to mitigate them?",
  "",
  "**Interpretation.** Counts in this section describe the evidence base and the completeness of safety reporting. They do not estimate the incidence or comparative importance of harms, and they are not used as votes for or against particular safeguards. Author-year citations identify examples supporting each mechanism or reporting statement; record IDs remain available in the supporting tables for auditability. Records may include overlapping reviews or linked evidence.",
  "",
  "## Direct preliminary answer",
  "",
  paste0(
    "The extracted evidence identifies several distinct ways that digital mental-health interventions can cause harm or fail safely. These include unsafe, harmful or inappropriate content/advice (e.g., ",
    ids_for_harm("Unsafe, harmful or inappropriate content/advice"),
    "); failures to detect or route crisis and self-harm content (e.g., ",
    ids_for_harm("Crisis-detection or routing failure"),
    "); over-reliance, dependency or delayed access to human care (e.g., ",
    ids_for_harm("Over-reliance, dependency or delayed human care"),
    "); and privacy, confidentiality or secondary-data-use risks (e.g., ",
    ids_for_harm("Privacy, confidentiality or data-use risk"),
    "). The underlying evidence spans observed events, user experiences, simulated safety tests, deployment failures and anticipated risks, so these examples should not be combined into a single harm rate."
  ),
  "",
  paste0(
    "Safety was discussed much more often than it was systematically quantified. Harms were described in 1d.1 for ",
    n_harms_discussed, " of ", stream_n, " records, whereas only ", n_ae_rates,
    " records contained any quantified AE/SAE information in 1d.2, a reporting gap of ",
    n_harms_discussed - n_ae_rates, " records. Of the ", n_ae_rates,
    " rate-bearing records, ", n_ae_core_complete,
    " contained substantive numerator, denominator, definition and time-window fields. Post-launch monitoring or governance information was extracted for ",
    n_governance, " records; automated status coding classified ",
    n_governance_operational, " as operational, ", n_governance_mixed,
    " as mixed operational/proposed, ", n_governance_proposed,
    " as proposed/recommended only, and ", n_governance_unclear,
    " as unclear; these classifications require quote-level verification before final reporting."
  ),
  "",
  "## Evidence base",
  "",
  paste0(
    "The analysis denominator is ", stream_n,
    " unique record IDs. The 1d worksheet contains ", raw_1d_data_rows,
    " physical data-area rows, of which ", raw_1d_blank_id_rows,
    " have blank record IDs; there are no duplicated nonblank IDs. The same ",
    stream_n, " IDs occur in baseline, so the worksheet-row surplus is explained by blank spacer rows rather than duplicate records. ",
    filename_difference_note
  ),
  "",
  paste0(
    "Of the ", stream_n, " records, ", contributing_n,
    " contained at least one populated RQ1d field and ", stream_n - contributing_n,
    " contained no safety information in fields 1d.1–1d.10. Among contributing records, study-design groups were: ",
    format_count_list(design_group_counts, "design_group", "n", 8),
    ". Technology architecture, classified from the baseline object description rather than safety text, was: ",
    format_count_list(architecture_counts, "architecture_class", "n", 8),
    ". Geographic directness was: ",
    format_count_list(geography_counts, "geography", "n", 3),
    "."
  ),
  "",
  "## Key findings",
  "",
  paste0(
    "**Harms and failure modes (1d.1–1d.3 and 1d.9).** The evidence does not describe a single, uniform adverse-event construct. Reported mechanisms include symptom deterioration or distress (e.g., ",
    ids_for_harm("Symptom deterioration or distress"),
    "), hallucination or factual fabrication (e.g., ",
    ids_for_harm("Hallucination, fabrication or factual inaccuracy"),
    "), sycophancy or harmful over-validation (e.g., ",
    ids_for_harm("Sycophancy or harmful over-validation"),
    "), persona/model drift or instability (e.g., ",
    ids_for_harm("Persona or model drift / instability"),
    "), jailbreak or prompt manipulation (e.g., ",
    ids_for_harm("Jailbreak or prompt manipulation"),
    "), and failures in crisis detection or referral. Field 1d.9 also contains deployment examples in which an apparently evidence-informed design did not remain safe after release or an update. These records support a mechanism catalogue for evaluation, but not comparative claims about which failure mode is most prevalent."
  ),
  "",
  paste0(
    "**Detection, mitigation, governance and equity (1d.4–1d.8 and 1d.10).** Extracted approaches include human or clinical review, automated or rule-based risk detection, predefined crisis responses, referral and human escalation, bounded or deterministic safety layers, curated/RAG-grounded content, content filtering, prompt/model constraints, logging, red-teaming, drift monitoring and change control. Records classified as implemented or operational include ",
    ids_for_mitigation(c("Implemented/operational", "Mixed operational and proposed")),
    "; records describing safeguards as proposed or recommended only include ",
    ids_for_mitigation("Proposed/recommended only"),
    ". These labels show implementation status, not effectiveness. Equity-related harms were extracted in ",
    n_equity, " records and included bias or discrimination (e.g., ",
    ids_for_harm("Bias, stereotyping or discrimination"),
    "), cultural or linguistic mismatch (e.g., ",
    ids_for_harm("Cultural or linguistic mismatch"),
    "), and digital exclusion or widening inequity (e.g., ",
    ids_for_harm("Digital exclusion or widening inequity"),
    ")."
  ),
  "",
  "## Implications for MHAA",
  "",
  "The AE definition set for Evaluation §11.3 should distinguish at least: symptom deterioration or acute distress; unsafe or inappropriate advice/content; crisis-recognition, escalation or referral failure; over-reliance or delayed human care; privacy/confidentiality or inappropriate data use; and equity-related exclusion or discriminatory performance. These categories derive from documented mechanisms in the extraction, not from the number of times each was reported.",
  "",
  "The continuous drift-monitoring list for §11.2 should include persona or role drift, sycophancy/over-validation, hallucination or fabrication, jailbreak/prompt manipulation, off-script content, and crisis-routing failure. Monitoring should retain model/version identifiers so that failures following updates can be linked to the deployed configuration.",
  "",
  "The §3.2 guardrail stack can reasonably be specified as layered rather than relying on a single prompt: high-risk intent detection; predefined and testable crisis responses; bounded or deterministic handling of safety-critical intents; human escalation and referral follow-through; approved or grounded content; logging and incident review; model-update/change control; privacy-by-design; and subgroup/equity testing. The review documents examples of several safeguards being implemented or tested, but also many recommendations that were not operational. It does not establish that any one stack is superior or sufficient.",
  "",
  "Annex A stopping-rule baselines should prospectively capture affected users and events, withdrawals attributable to harm, symptom deterioration, harmful-response rates, crisis-detection false negatives, failed escalation/referral, and serious privacy or equity incidents, each with a defined denominator and observation window. The review does not provide a defensible universal stopping threshold; thresholds should therefore be pre-specified and justified for the MHAA deployment and risk context.",
  "",
  "## Gaps and caveats",
  "",
  paste0(
    "AE/SAE definitions and reporting structures were inconsistent. Only ",
    n_ae_rates, " records populated 1d.2, and ", n_ae_core_complete,
    " included substantive numerator, denominator, definition and time window. ",
    n_zero_ambiguous,
    " rate-bearing records used zero-event wording without an extracted explanation sufficient to distinguish 'none occurred' from 'none were actively sought or identified'. Every individual AE rate used in the final prose should therefore be checked in `RQ1d_AE_rate_validation.csv` and should carry a numerator, denominator and time window or be explicitly flagged as incomplete."
  ),
  "",
  paste0(
    "Causality attribution was populated in only ", n_causality,
    " records (1d.7), and methods varied from formal oversight decisions to author interpretation or participant attribution. Concurrent distress, underlying clinical risk and intervention-attributable harm should not be conflated."
  ),
  "",
  paste0(
    "The unit of counting is the record, not the independent study. Evidence syntheses may overlap, and linked publications may describe the same underlying intervention or sample. The filename-to-data difference of ",
    ifelse(is.na(filename_expected_n), "unknown", filename_expected_n - stream_n),
    " cannot be resolved as linked-publication removal from the supplied baseline/1d sheets alone; the review log should document the relationship before final counts are presented."
  ),
  "",
  "Regex-derived mechanism and implementation tags are transparent synthesis aids. They must be checked against the extracted value, quote, location and agreement fields, especially before describing a failure as observed, a safeguard as operational, or a mitigation as effective.",
  "",
  "## Verification checklist",
  "",
  paste0(
    "Five records have been selected for manual value-versus-quote verification: ",
    format_citation_id_list(selected_verification_ids, 5),
    ". Review `verification_sample` in the output workbook or `RQ1d_verification_sample.csv`; confirm the extracted value against the quote and source location, resolve any agreement conflict, and verify any rate-bearing statement against the full numerator, denominator and time window."
  )
)

readr::write_lines(
  report_lines,
  file.path(output_dir, "Section-4.5-Safety-RQ1d.md")
)

write_report_pdf(
  report_lines,
  p_safety_two_panel,
  file.path(output_dir, "Section-4.5-Safety-RQ1d.pdf")
)

# ---- 15. Record-level export ---------------------------------------------------

record_tag_counts <- master %>%
  dplyr::transmute(record_id) %>%
  dplyr::left_join(
    harm_tags %>%
      dplyr::count(.data$record_id, name = "n_harm_tags"),
    by = "record_id"
  ) %>%
  dplyr::left_join(
    mitigation_tags %>%
      dplyr::count(.data$record_id, name = "n_mitigation_tags"),
    by = "record_id"
  ) %>%
  dplyr::mutate(
    n_harm_tags = dplyr::coalesce(.data$n_harm_tags, 0L),
    n_mitigation_tags = dplyr::coalesce(.data$n_mitigation_tags, 0L)
  )

record_level_export <- master %>%
  dplyr::left_join(record_tag_counts, by = "record_id") %>%
  dplyr::left_join(
    ae_rate_detail %>%
      dplyr::select(
        record_id, rate_core_complete, zero_event_wording,
        zero_meaning_clear, zero_event_ambiguous, zero_ascertainment,
        validation_flag
      ),
    by = "record_id"
  ) %>%
  dplyr::select(
    record_id, title, bibliographic, country, setting, sample_size, sample_n,
    age_band, age_as_reported, age_10_19, age_10_24, age_18_24,
    study_design, design_group, evidence_type_tag, object_type,
    architecture_class, lmic, ssa, south_africa,
    adolescent_or_young_direct, genai_direct, south_africa_direct,
    mhaa_all_three_direct, rq1d_any,
    reporting_category_detail, reporting_category_plot,
    n_harm_tags, n_mitigation_tags,
    rate_core_complete, zero_event_wording, zero_meaning_clear,
    zero_event_ambiguous, zero_ascertainment, validation_flag,
    dplyr::all_of(value_cols)
  ) %>%
  dplyr::arrange(.data$record_id)

# ---- 16. Write CSV and Excel outputs ------------------------------------------

readr::write_csv(
  record_level_export,
  file.path(output_dir, "RQ1d_record_level.csv"),
  na = ""
)

readr::write_csv(
  quote_bank,
  file.path(output_dir, "RQ1d_quote_bank.csv"),
  na = ""
)

readr::write_csv(
  ae_rate_detail,
  file.path(output_dir, "RQ1d_AE_rate_validation.csv"),
  na = ""
)

readr::write_csv(
  verification_sample,
  file.path(output_dir, "RQ1d_verification_sample.csv"),
  na = ""
)

readr::write_csv(
  field_coverage,
  file.path(output_dir, "RQ1d_field_coverage.csv"),
  na = ""
)

output_workbook <- list(
  denominator_audit = denominator_audit,
  headline_gap = headline_reporting_gap,
  field_coverage = field_coverage,
  reporting_profile = reporting_profile_counts,
  ae_rate_completeness = ae_rate_completeness,
  ae_rate_detail = ae_rate_detail,
  study_designs = study_design_counts,
  design_groups = design_group_counts,
  architecture = architecture_counts,
  geography = geography_counts,
  countries = country_counts,
  directness = directness_counts,
  harm_domain_counts = harm_domain_counts,
  harm_counts = harm_counts,
  harm_tags = harm_tags,
  harm_figure_data = harm_figure_counts_all,
  mitigation_counts = mitigation_counts,
  mitigation_status = mitigation_status_counts,
  mitigation_tags = mitigation_tags,
  mitigation_figure_data = mitigation_figure_counts_all,
  governance_status = governance_status_counts,
  governance_detail = governance_detail,
  verification_summary = verification_summary,
  verification_sample = verification_sample,
  review_queue_1d = review_queue_1d,
  quote_bank = quote_bank,
  record_level = record_level_export,
  codebook_1d = codebook_1d
)

writexl::write_xlsx(
  output_workbook,
  path = file.path(output_dir, "RQ1d_analysis_outputs.xlsx")
)

# ---- 17. Completion message ---------------------------------------------------

message("RQ1d safety analysis complete.")
message("Unique 1d denominator: ", stream_n)
message("Physical 1d data rows: ", raw_1d_data_rows)
message("Blank-ID rows removed: ", raw_1d_blank_id_rows)
message("Records contributing >=1 RQ1d field: ", contributing_n)
message("Harms discussed (1d.1): ", n_harms_discussed)
message("AE/SAE rate reported (1d.2): ", n_ae_rates)
message("Core-complete AE rate records: ", n_ae_core_complete)
message("Outputs written to: ", normalizePath(output_dir, winslash = "/"))
message("Open Section-4.5-Safety-RQ1d.md and RQ1d_analysis_outputs.xlsx first.")
