# ==============================================================================
# Section 4.7 — South African context (additional stream)
#
# Review question:
# What contextual factors should inform the design, implementation, and
# evaluation of a GenAI mental health intervention for young people in
# South Africa?
#
# Inputs:
#   - extraction_final-revised-SL_n=325.xlsx
#   - sheet "sa"       (SA.1–SA.8)
#   - sheet "baseline" (study and intervention characteristics)
#   - sheet "Index"    (stream assignment and review flags)
#   - sheet "Codebook" (field labels and definitions)
#
# Main outputs, written to RQ_SA_context_outputs/:
#   - Section-4.7-South-African-context-scaffold.md
#   - RQ_SA_context_analysis_outputs.xlsx
#   - RQ_SA_context_record_level.csv
#   - RQ_SA_context_field_coverage.csv
#   - RQ_SA_context_theme_detail.csv
#   - RQ_SA_context_theme_summary.csv
#   - RQ_SA_context_quote_bank.csv
#   - RQ_SA_context_verification_queue.csv
#   - RQ_SA_context_field_coverage.png
#   - RQ_SA_context_theme_by_directness.png
#
# Interpretation rules:
#   1. Counts refer to records, not necessarily independent studies.
#   2. A contextual factor mentioned in a review, framework or non-South African
#      study is indirect evidence for MHAA and must not be presented as a
#      South African finding.
#   3. "Direct on all three MHAA dimensions" is reported in two ways:
#        a) record-level intersection: the record concerns young people,
#           GenAI and South Africa somewhere in the record;
#        b) strict primary intersection: one primary study directly concerns
#           young people, an explicitly generative system and South Africa.
#      The strict measure should be used in the report text.
#   4. Keyword-derived tags are synthesis aids. They show that a record mentions
#      a topic, not the direction, prevalence or strength of the finding.
#   5. Evidence-supported implications are kept separate from review-team
#      interpretations and proposed design decisions.
# ==============================================================================

# ---- 0. Packages and paths ----------------------------------------------------

required_packages <- c(
  "readxl", "dplyr", "tidyr", "purrr", "stringr", "readr",
  "tibble", "ggplot2", "writexl", "scales"
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
  "extraction_final-revised-SL_n=325.xlsx"
)

existing_input <- input_candidates[file.exists(input_candidates)]
input_file <- if (length(existing_input) > 0) existing_input[[1]] else input_candidates[[1]]
output_dir <- "RQ_SA_context_outputs"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop(
    "Input workbook not found: ", input_file,
    "\nPlace the workbook in the working directory or update input_file."
  )
}

# ---- 1. General helpers -------------------------------------------------------

has_text <- function(x) {
  !is.na(x) & stringr::str_trim(as.character(x)) != ""
}

blank_to_na <- function(x) {
  x <- as.character(x)
  x[!has_text(x)] <- NA_character_
  x
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

safe_pct <- function(n, denominator) {
  ifelse(is.na(denominator) | denominator == 0, NA_real_, 100 * n / denominator)
}

format_n_pct <- function(n, denominator, digits = 1) {
  if (is.na(denominator) || denominator == 0) return(sprintf("n=%s", n))
  sprintf(paste0("n=%s (%.", digits, "f%%)"), n, 100 * n / denominator)
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

read_sheet_text <- function(sheet_name) {
  readxl::read_excel(
    input_file,
    sheet = sheet_name,
    col_types = "text",
    .name_repair = "minimal"
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

format_count_list <- function(data, label_col, n_col, max_items = 8) {
  if (nrow(data) == 0) return("none")
  data <- data %>%
    dplyr::arrange(dplyr::desc(.data[[n_col]]), .data[[label_col]]) %>%
    dplyr::slice_head(n = max_items)
  paste0(data[[label_col]], " (n=", data[[n_col]], ")", collapse = "; ")
}

collapse_citations <- function(x, max_n = 2) {
  x <- unique(x[has_text(x)])
  if (length(x) == 0) return(NA_character_)
  paste(utils::head(x, max_n), collapse = "; ")
}

extract_bib_part <- function(x, key) {
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

first_author_surname <- function(authors) {
  purrr::map_chr(as.character(authors), function(author_string) {
    author_string <- stringr::str_squish(dplyr::coalesce(author_string, ""))
    if (!has_text(author_string)) return(NA_character_)

    # First handle lists separated by semicolons or an ampersand.
    if (stringr::str_detect(author_string, ";")) {
      first_author <- stringr::str_split_fixed(author_string, ";", 2)[1, 1]
    } else if (stringr::str_detect(author_string, "\\s+&\\s+")) {
      first_author <- stringr::str_split_fixed(author_string, "\\s+&\\s+", 2)[1, 1]
    } else if (stringr::str_detect(author_string, ",")) {
      comma_parts <- stringr::str_split(author_string, "\\s*,\\s*")[[1]]

      # "Surname, Initials, ..." versus "Full Name, Next Full Name, ...".
      if (
        length(comma_parts) >= 2 &&
        stringr::str_detect(
          stringr::str_trim(comma_parts[[2]]),
          "^[A-Z](?:[A-Z.]|\\s|-)*$"
        )
      ) {
        return(blank_to_na(stringr::str_trim(comma_parts[[1]])))
      }

      first_author <- comma_parts[[1]]
    } else {
      first_author <- author_string
    }

    blank_to_na(stringr::word(stringr::str_squish(first_author), -1))
  })
}

make_author_year <- function(bibliographic) {
  authors <- extract_bib_part(bibliographic, "authors")
  year <- stringr::str_match(
    dplyr::coalesce(as.character(bibliographic), ""),
    stringr::regex("\\byear\\s*:\\s*(\\d{4})", ignore_case = TRUE)
  )[, 2]
  surname <- first_author_surname(authors)

  dplyr::case_when(
    has_text(surname) & has_text(year) ~ paste0(surname, " et al. (", year, ")"),
    has_text(surname) ~ surname,
    has_text(year) ~ paste0("Study (", year, ")"),
    TRUE ~ NA_character_
  )
}

field_number <- function(field_id) {
  suppressWarnings(as.integer(stringr::str_extract(field_id, "(?<=SA\\.)[0-9]+")))
}

# ---- 2. Read source sheets ----------------------------------------------------

baseline_raw <- read_sheet_text("baseline")
sa_raw <- read_sheet_text("sa")
index_raw <- read_sheet_text("Index")
codebook_raw <- read_sheet_text("Codebook")

require_columns(
  baseline_raw,
  c(
    "record_id", "title", "base.1 value", "base.2 value", "base.3 value",
    "base.4 value", "base.5 value", "base.6 value", "base.7 value",
    "base.8 value", "base.10 value", "base.11a value", "base.11b value"
  ),
  "baseline sheet"
)

require_columns(sa_raw, c("record_id", "title"), "sa sheet")
require_columns(index_raw, c("record_id", "groups_extracted", "needs_human"), "Index sheet")
require_columns(codebook_raw, c("group", "field_id", "label", "what", "feeds_eval"), "Codebook sheet")

value_cols <- grep("^SA\\.[1-8] value$", names(sa_raw), value = TRUE)
quote_cols <- grep("^SA\\.[1-8] quotes$", names(sa_raw), value = TRUE)
loc_cols <- grep("^SA\\.[1-8] loc$", names(sa_raw), value = TRUE)
agree_cols <- grep("^SA\\.[1-8] agree$", names(sa_raw), value = TRUE)

if (length(value_cols) != 8) {
  stop(
    "Expected eight South African context value fields (SA.1–SA.8), but found: ",
    paste(value_cols, collapse = ", ")
  )
}

# ---- 3. Prepare baseline, index and codebook ----------------------------------

baseline <- baseline_raw %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), blank_to_na)) %>%
  dplyr::filter(has_text(record_id)) %>%
  dplyr::transmute(
    record_id,
    title_baseline = title,
    bibliographic = `base.1 value`,
    country = `base.2 value`,
    setting = `base.3 value`,
    sample_size = `base.4 value`,
    age_band = `base.5 value`,
    study_design = `base.6 value`,
    object_type = `base.7 value`,
    rq_addressed = `base.8 value`,
    lmic_raw = `base.10 value`,
    ssa_raw = `base.11a value`,
    south_africa_raw = `base.11b value`
  )

index_data <- index_raw %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), blank_to_na)) %>%
  dplyr::filter(has_text(record_id)) %>%
  dplyr::transmute(
    record_id,
    groups_extracted,
    gated_out_groups,
    total_reported = suppressWarnings(as.numeric(total_reported)),
    needs_human = suppressWarnings(as.numeric(needs_human))
  )

sa_codebook <- codebook_raw %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), blank_to_na)) %>%
  dplyr::filter(stringr::str_to_lower(group) == "sa") %>%
  dplyr::transmute(
    field_id,
    field_label = label,
    field_definition = what,
    feeds_eval
  ) %>%
  dplyr::mutate(field_no = field_number(field_id)) %>%
  dplyr::arrange(field_no)

# ---- 4. Prepare SA-context extraction and join -------------------------------

sa <- sa_raw %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), blank_to_na)) %>%
  dplyr::filter(has_text(record_id))

blank_id_rows <- sum(!has_text(sa_raw$record_id))
duplicated_sa_ids <- sa %>% dplyr::count(record_id) %>% dplyr::filter(n > 1)
duplicated_baseline_ids <- baseline %>% dplyr::count(record_id) %>% dplyr::filter(n > 1)

if (nrow(duplicated_sa_ids) > 0) {
  stop("Duplicate nonblank record IDs were found in the sa sheet.")
}

if (nrow(duplicated_baseline_ids) > 0) {
  stop("Duplicate nonblank record IDs were found in the baseline sheet.")
}

master <- baseline %>%
  dplyr::full_join(sa, by = "record_id", suffix = c("_baseline", "_sa")) %>%
  dplyr::left_join(index_data, by = "record_id") %>%
  dplyr::mutate(
    title = dplyr::coalesce(title_baseline, title),
    author_year = make_author_year(bibliographic),
    lmic = parse_bool(lmic_raw),
    ssa = parse_bool(ssa_raw),
    south_africa = parse_bool(south_africa_raw)
  )

joined_value_cols <- intersect(value_cols, names(master))

master$sa_context_any <- apply(
  dplyr::select(master, dplyr::all_of(joined_value_cols)),
  1,
  function(row) any(has_text(row))
)

master$sa_context_text <- apply(
  dplyr::select(master, dplyr::all_of(joined_value_cols)),
  1,
  function(row) paste(row[has_text(row)], collapse = " | ")
)
master$sa_context_text[!has_text(master$sa_context_text)] <- NA_character_

# ---- 5. Operationalise study design and MHAA directness ----------------------

genai_pattern <- paste0(
  "\\b(",
  paste(
    c(
      "llm", "large language model", "generative ai", "genai",
      "gpt(?:-[0-9a-z.]+)?", "chatgpt", "claude", "gemini", "llama",
      "mistral", "foundation model", "retrieval[- ]augmented", "rag[- ]based"
    ),
    collapse = "|"
  ),
  ")\\b"
)

rule_based_pattern <- paste0(
  "rule[- ]based|decision tree|pre[- ]defined|preprogrammed|pre-programmed|",
  "scripted dialogue|finite[- ]state|deterministic response"
)

ai_unspecified_pattern <- paste0(
  "artificial intelligence|\\bai[- ]?(?:based|powered|enabled|driven)?\\b|",
  "natural language processing|\\bnlp\\b|machine learning"
)

master <- master %>%
  dplyr::mutate(
    age_10_19 = extract_composite_bool(age_band, "maps_10_19"),
    age_10_24 = extract_composite_bool(age_band, "maps_10_24"),
    age_18_24 = extract_composite_bool(age_band, "maps_18_24"),
    adolescent_or_young_direct = dplyr::coalesce(age_10_19, FALSE) |
      dplyr::coalesce(age_10_24, FALSE) |
      dplyr::coalesce(age_18_24, FALSE),
    genai_direct = stringr::str_detect(
      dplyr::coalesce(object_type, ""),
      stringr::regex(genai_pattern, ignore_case = TRUE)
    ),
    rule_based_direct = stringr::str_detect(
      dplyr::coalesce(object_type, ""),
      stringr::regex(rule_based_pattern, ignore_case = TRUE)
    ),
    ai_unspecified_direct = stringr::str_detect(
      dplyr::coalesce(object_type, ""),
      stringr::regex(ai_unspecified_pattern, ignore_case = TRUE)
    ),
    south_africa_direct = dplyr::coalesce(south_africa, FALSE) |
      stringr::str_detect(
        dplyr::coalesce(country, ""),
        stringr::regex("\\bZAF\\b|South Africa", ignore_case = TRUE)
      ),
    design_group = dplyr::case_when(
      stringr::str_detect(
        dplyr::coalesce(study_design, ""),
        stringr::regex("SR\\+MA|\\bSR\\b|systematic|scoping|umbrella|review", ignore_case = TRUE)
      ) ~ "Evidence synthesis",
      stringr::str_detect(
        dplyr::coalesce(study_design, ""),
        stringr::regex("qualitative|mixed", ignore_case = TRUE)
      ) ~ "Qualitative or mixed-methods",
      stringr::str_detect(
        dplyr::coalesce(study_design, ""),
        stringr::regex("RCT|trial|cohort|cross-sectional", ignore_case = TRUE)
      ) ~ "Primary quantitative",
      stringr::str_detect(
        dplyr::coalesce(study_design, ""),
        stringr::regex("framework|regulatory|guidance|viewpoint", ignore_case = TRUE)
      ) ~ "Framework or guidance",
      TRUE ~ "Other or not reported"
    ),
    primary_record = !design_group %in% c("Evidence synthesis", "Framework or guidance"),
    south_africa_primary = south_africa_direct & primary_record,
    mhaa_all_three_record_level = adolescent_or_young_direct &
      genai_direct & south_africa_direct,
    mhaa_all_three_strict_primary = adolescent_or_young_direct &
      genai_direct & south_africa_primary,
    architecture_class = dplyr::case_when(
      genai_direct & rule_based_direct ~ "Hybrid GenAI + rule-based",
      genai_direct ~ "Generative AI / LLM",
      rule_based_direct ~ "Rule-based / deterministic",
      ai_unspecified_direct ~ "AI / NLP, generative status unclear",
      TRUE ~ "Other digital or unclear"
    ),
    source_directness = dplyr::case_when(
      south_africa_primary ~ "Direct South African primary evidence",
      south_africa_direct & design_group == "Evidence synthesis" ~
        "South Africa included in evidence synthesis",
      dplyr::coalesce(ssa, FALSE) ~ "Other sub-Saharan African evidence",
      dplyr::coalesce(lmic, FALSE) ~ "Other L&MIC evidence",
      TRUE ~ "Other or global indirect evidence"
    ),
    index_flagged_sa = stringr::str_detect(
      dplyr::coalesce(groups_extracted, ""),
      stringr::regex("(^|,\\s*)sa(\\s*,|$)|SA-context", ignore_case = TRUE)
    )
  )

contributing <- master %>% dplyr::filter(sa_context_any)
stream_n <- nrow(master %>% dplyr::filter(has_text(record_id)))
contributing_n <- nrow(contributing)

if (contributing_n == 0) {
  stop("No records contain a populated SA.1–SA.8 value field.")
}

# ---- 6. Evidence-base summaries ----------------------------------------------

design_group_counts <- contributing %>%
  dplyr::count(design_group, sort = TRUE, name = "n") %>%
  dplyr::mutate(pct = safe_pct(n, contributing_n))

study_design_counts <- standard_count(contributing, "study_design") %>%
  dplyr::mutate(pct = safe_pct(n, contributing_n))

setting_counts <- standard_count(contributing, "setting") %>%
  dplyr::mutate(pct = safe_pct(n, contributing_n))

country_counts <- standard_count(contributing, "country") %>%
  dplyr::rename(country_label = category) %>%
  dplyr::mutate(pct = safe_pct(n, contributing_n))

architecture_counts <- contributing %>%
  dplyr::count(architecture_class, sort = TRUE, name = "n") %>%
  dplyr::mutate(pct = safe_pct(n, contributing_n))

source_directness_counts <- contributing %>%
  dplyr::count(source_directness, sort = TRUE, name = "n") %>%
  dplyr::mutate(pct = safe_pct(n, contributing_n))

directness_metrics <- tibble::tibble(
  metric = c(
    "Adolescent or young-person direct",
    "Generative-AI or LLM direct",
    "South Africa direct at record level",
    "Direct South African primary evidence",
    "Record-level intersection: young people + GenAI + South Africa",
    "Strict primary intersection: young people + GenAI + South Africa"
  ),
  n = c(
    sum(contributing$adolescent_or_young_direct, na.rm = TRUE),
    sum(contributing$genai_direct, na.rm = TRUE),
    sum(contributing$south_africa_direct, na.rm = TRUE),
    sum(contributing$south_africa_primary, na.rm = TRUE),
    sum(contributing$mhaa_all_three_record_level, na.rm = TRUE),
    sum(contributing$mhaa_all_three_strict_primary, na.rm = TRUE)
  ),
  denominator = contributing_n
) %>%
  dplyr::mutate(pct = safe_pct(n, denominator))

# ---- 7. Field coverage and quote bank ----------------------------------------

sa_long <- sa %>%
  dplyr::select(record_id, title, dplyr::all_of(c(value_cols, quote_cols, loc_cols, agree_cols))) %>%
  tidyr::pivot_longer(
    cols = -c(record_id, title),
    names_to = c("field_id", "component"),
    names_pattern = "^(SA\\.[1-8]) (value|quotes|loc|agree)$",
    values_to = "text"
  ) %>%
  tidyr::pivot_wider(names_from = component, values_from = text) %>%
  dplyr::left_join(sa_codebook, by = "field_id") %>%
  dplyr::left_join(
    master %>%
      dplyr::select(
        record_id, author_year, bibliographic, country, setting, age_band,
        study_design, design_group, object_type, architecture_class,
        source_directness, adolescent_or_young_direct, genai_direct,
        south_africa_direct, south_africa_primary,
        mhaa_all_three_record_level, mhaa_all_three_strict_primary
      ),
    by = "record_id"
  ) %>%
  dplyr::mutate(field_no = field_number(field_id)) %>%
  dplyr::arrange(field_no, record_id)

field_coverage <- sa_long %>%
  dplyr::group_by(field_id, field_no, field_label, field_definition) %>%
  dplyr::summarise(
    n_value = sum(has_text(value)),
    n_quote = sum(has_text(quotes)),
    n_location = sum(has_text(loc)),
    n_agree = sum(stringr::str_to_lower(dplyr::coalesce(agree, "")) == "agree"),
    n_minor = sum(stringr::str_to_lower(dplyr::coalesce(agree, "")) == "minor"),
    n_conflict = sum(stringr::str_to_lower(dplyr::coalesce(agree, "")) == "conflict"),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    pct_contributing = safe_pct(n_value, contributing_n),
    pct_all_records = safe_pct(n_value, stream_n)
  ) %>%
  dplyr::arrange(field_no)

quote_bank <- sa_long %>%
  dplyr::filter(has_text(value) | has_text(quotes)) %>%
  dplyr::select(
    record_id, author_year, title, field_id, field_label,
    value, quotes, loc, agree, source_directness,
    country, setting, age_band, study_design, object_type
  )

# ---- 8. Context-theme tagging -------------------------------------------------

# These tags are deliberately narrow. They identify records for manual synthesis;
# they do not establish prevalence, direction or causal importance.
context_dictionary <- tibble::tribble(
  ~theme, ~subtheme, ~pattern,
  "Help-seeking, disclosure and engagement", "Preference for human or blended support",
  "prefer(?:red|ence)?.{0,40}(?:human|in[- ]person|counsell|therap)|supplement rather than replace|hybrid support|blended support",
  "Help-seeking, disclosure and engagement", "Anonymity or lower fear of judgement",
  "anonym|judg(?:e|ement|ment)|embarrass|private|privacy|confidential|disclos",
  "Help-seeking, disclosure and engagement", "Digital-first access or convenience",
  "digital[- ]first|online access|available 24|availability|convenien|remote support|mobile access",
  "Stigma and social context", "Mental-health or identity-related stigma",
  "stigma|discriminat|conceal|internalised stigma|internalized stigma|anti[- ]MSM|LGBTQI|transgender|sexual identity",
  "Service access and referral", "Provider shortage, waitlists or treatment gap",
  "shortage|waitlist|waiting time|treatment gap|receive no care|limited availability|few professionals|provider capacity",
  "Service access and referral", "Cost, distance or geographic access",
  "cost|afford|distance|geograph|rural|urban cent|outlying|transport|facility access",
  "Service access and referral", "Referral, handoff or crisis pathway",
  "refer|referral|handoff|hand[- ]off|helpline|crisis|human support|professional support|service linkage",
  "Language, culture and concepts of distress", "Local language or multilingual support",
  "isiZulu|isiXhosa|Afrikaans|Sesotho|multilingual|native language|local language|English only|English-speaking",
  "Language, culture and concepts of distress", "Code-switching or emotional expression",
  "code[- ]switch|express(?:ing)? emotions|emotional expression|concepts? of distress|idiom|cultural nuance|culturally",
  "Language, culture and concepts of distress", "Literacy, readability or voice access",
  "literacy|low[- ]literate|reading age|readability|grade level|voice[- ]based|cannot read|verbal comprehension",
  "Gender and differential access", "Gender-based violence or relationship harms",
  "gender[- ]based violence|\\bGBV\\b|intimate partner violence|\\bIPV\\b|unhealthy relationship|early union|menstrual",
  "Gender and differential access", "Gender, sexuality or population differences",
  "women|girls|gender|male|female|MSM|men who have sex with men|transgender|LGBTQI|sexual minorit",
  "Prior South African digital deployments", "Named South African deployment",
  "Chatty Cuz|Nolwazi|Chommie|STARS|South Africa|KwaZulu[- ]Natal",
  "Regulatory and data context", "South African or international regulation",
  "POPIA|SAHPRA|national mental health policy|GDPR|HIPAA|medical device|regulat"
)

context_source <- sa_long %>%
  dplyr::filter(has_text(value)) %>%
  dplyr::transmute(
    record_id, author_year, title, field_id, field_label,
    source_text = value, source_directness,
    country, setting, age_band, study_design, design_group,
    object_type, architecture_class,
    adolescent_or_young_direct, genai_direct,
    south_africa_direct, south_africa_primary,
    mhaa_all_three_record_level, mhaa_all_three_strict_primary
  )

context_tags <- tidyr::crossing(context_source, context_dictionary) %>%
  dplyr::filter(
    stringr::str_detect(
      dplyr::coalesce(source_text, ""),
      stringr::regex(pattern, ignore_case = TRUE)
    )
  ) %>%
  dplyr::select(-pattern) %>%
  dplyr::distinct()

theme_summary <- context_tags %>%
  dplyr::distinct(record_id, theme, source_directness) %>%
  dplyr::count(theme, source_directness, name = "n_records") %>%
  tidyr::complete(
    theme,
    source_directness = c(
      "Direct South African primary evidence",
      "South Africa included in evidence synthesis",
      "Other sub-Saharan African evidence",
      "Other L&MIC evidence",
      "Other or global indirect evidence"
    ),
    fill = list(n_records = 0)
  ) %>%
  dplyr::group_by(theme) %>%
  dplyr::mutate(total_records = sum(n_records)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(dplyr::desc(total_records), theme, source_directness)

subtheme_summary <- context_tags %>%
  dplyr::distinct(record_id, theme, subtheme, source_directness) %>%
  dplyr::count(theme, subtheme, source_directness, name = "n_records") %>%
  dplyr::arrange(theme, dplyr::desc(n_records), subtheme)

# ---- 9. Representative evidence for drafting --------------------------------

representative_evidence <- context_tags %>%
  dplyr::mutate(
    directness_rank = dplyr::case_when(
      source_directness == "Direct South African primary evidence" ~ 1L,
      source_directness == "South Africa included in evidence synthesis" ~ 2L,
      source_directness == "Other sub-Saharan African evidence" ~ 3L,
      source_directness == "Other L&MIC evidence" ~ 4L,
      TRUE ~ 5L
    )
  ) %>%
  dplyr::arrange(theme, subtheme, directness_rank, record_id) %>%
  dplyr::group_by(theme, subtheme) %>%
  dplyr::mutate(example_order = dplyr::row_number()) %>%
  dplyr::ungroup()

example_citations <- function(theme_name, max_n = 2) {
  x <- representative_evidence %>%
    dplyr::filter(theme == .env$theme_name) %>%
    dplyr::arrange(directness_rank, record_id) %>%
    dplyr::pull(author_year)
  collapse_citations(x, max_n = max_n)
}

field_citations <- function(field_id_value, max_n = 2) {
  x <- quote_bank %>%
    dplyr::filter(field_id == .env$field_id_value) %>%
    dplyr::mutate(
      directness_rank = dplyr::case_when(
        source_directness == "Direct South African primary evidence" ~ 1L,
        source_directness == "South Africa included in evidence synthesis" ~ 2L,
        source_directness == "Other sub-Saharan African evidence" ~ 3L,
        source_directness == "Other L&MIC evidence" ~ 4L,
        TRUE ~ 5L
      )
    ) %>%
    dplyr::arrange(directness_rank, record_id) %>%
    dplyr::pull(author_year)
  collapse_citations(x, max_n = max_n)
}

cite_or_placeholder <- function(x) {
  ifelse(has_text(x), paste0(" (e.g., ", x, ")"), " [add verified citation]")
}

# ---- 10. Verification queue ---------------------------------------------------

agreement_long <- sa_long %>%
  dplyr::filter(has_text(value)) %>%
  dplyr::mutate(
    agreement_issue = dplyr::case_when(
      !has_text(quotes) ~ "Populated value has no supporting quote",
      stringr::str_to_lower(dplyr::coalesce(agree, "")) == "conflict" ~
        "Extraction agreement recorded as conflict",
      stringr::str_to_lower(dplyr::coalesce(agree, "")) == "minor" ~
        "Extraction agreement recorded as minor",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(has_text(agreement_issue)) %>%
  dplyr::transmute(
    record_id, author_year, title, field_id, field_label,
    issue = agreement_issue, value, quotes, loc, agree,
    source_directness
  )

directness_queue <- contributing %>%
  dplyr::filter(mhaa_all_three_record_level & !mhaa_all_three_strict_primary) %>%
  dplyr::transmute(
    record_id, author_year, title,
    field_id = NA_character_,
    field_label = NA_character_,
    issue = paste0(
      "Record-level intersection of young people, GenAI and South Africa, ",
      "but not a strict South African primary-study intersection; verify that ",
      "all three dimensions refer to the same underlying evidence."
    ),
    value = sa_context_text,
    quotes = NA_character_,
    loc = NA_character_,
    agree = NA_character_,
    source_directness
  )

flag_queue <- contributing %>%
  dplyr::filter(
    south_africa_direct &
      (!dplyr::coalesce(lmic, FALSE) | !dplyr::coalesce(ssa, FALSE))
  ) %>%
  dplyr::transmute(
    record_id, author_year, title,
    field_id = NA_character_,
    field_label = NA_character_,
    issue = paste0(
      "Record includes South Africa but the extracted L&MIC or SSA flag is not TRUE; ",
      "verify geographic coding before reporting totals."
    ),
    value = paste0(
      "country: ", country,
      " | L&MIC flag: ", lmic_raw,
      " | SSA flag: ", ssa_raw,
      " | South Africa flag: ", south_africa_raw
    ),
    quotes = NA_character_,
    loc = NA_character_,
    agree = NA_character_,
    source_directness
  )

verification_queue <- dplyr::bind_rows(
  agreement_long,
  directness_queue,
  flag_queue
) %>%
  dplyr::distinct() %>%
  dplyr::arrange(record_id, field_id)

# ---- 11. Figures --------------------------------------------------------------

# Shared visual style, matched to the earlier blue RQ1b figure:
#   - white background
#   - blue-only palette
#   - no grid lines, axis lines, x-axis labels or ticks
#   - horizontal bars with values shown at the bar ends
#   - bold, left-aligned title and plain subtitle

figure_background <- "white"
figure_text <- "#1F1F1F"
figure_subtext <- "#505050"

blue_palette <- c(
  "Direct South African primary evidence" = "#3778B4",
  "South Africa included in evidence synthesis" = "#6F99C5",
  "Other sub-Saharan African evidence" = "#9CB8D6",
  "Other L&MIC evidence" = "#C1D2E4",
  "Other or global indirect evidence" = "#DEE7F0"
)

base_sa_figure_theme <- ggplot2::theme_minimal(
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
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 18,
      colour = figure_text,
      hjust = 0,
      margin = ggplot2::margin(b = 7)
    ),
    plot.subtitle = ggplot2::element_text(
      size = 11.5,
      colour = figure_subtext,
      hjust = 0,
      margin = ggplot2::margin(b = 13)
    ),
    plot.caption = ggplot2::element_text(
      size = 9.5,
      colour = figure_subtext,
      hjust = 0,
      margin = ggplot2::margin(t = 10)
    ),
    plot.margin = ggplot2::margin(t = 12, r = 36, b = 12, l = 12)
  )

# ---- 11a. Coverage of South African-context extraction fields ----------------

field_plot_data <- field_coverage %>%
  dplyr::mutate(
    display_label = paste0(field_id, ". ", field_label),
    display_label = factor(display_label, levels = rev(display_label))
  )

field_x_max <- max(field_plot_data$n_value, na.rm = TRUE)
if (!is.finite(field_x_max) || field_x_max <= 0) field_x_max <- 1

p_field <- ggplot2::ggplot(
  field_plot_data,
  ggplot2::aes(x = n_value, y = display_label, fill = n_value)
) +
  ggplot2::geom_col(width = 0.70, colour = NA) +
  ggplot2::geom_text(
    ggplot2::aes(label = n_value),
    hjust = -0.22,
    size = 4.8,
    colour = figure_text
  ) +
  ggplot2::scale_fill_gradient(
    low = "#DCE8F3",
    high = "#3778B4",
    guide = "none"
  ) +
  ggplot2::scale_x_continuous(
    breaks = NULL,
    labels = NULL,
    limits = c(0, field_x_max * 1.14 + 0.5),
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::labs(
    title = "South African context information extracted by field",
    subtitle = paste0(
      "Records with a populated value; n=", contributing_n,
      " records contributed to at least one SA-context field"
    ),
    caption = paste0(
      "Counts describe records, not independent studies; fields are not mutually exclusive."
    )
  ) +
  base_sa_figure_theme

ggplot2::ggsave(
  filename = file.path(output_dir, "RQ_SA_context_field_coverage.png"),
  plot = p_field,
  width = 12,
  height = 7.2,
  dpi = 320,
  bg = figure_background
)

# ---- 11b. Contextual themes by directness to South Africa --------------------

if (nrow(theme_summary) > 0) {
  directness_levels <- names(blue_palette)

  theme_order <- theme_summary %>%
    dplyr::distinct(theme, total_records) %>%
    dplyr::arrange(total_records, theme) %>%
    dplyr::pull(theme)

  theme_plot_data <- theme_summary %>%
    dplyr::mutate(
      source_directness = factor(
        source_directness,
        levels = rev(directness_levels)
      ),
      theme = factor(theme, levels = theme_order)
    )

  theme_totals <- theme_summary %>%
    dplyr::distinct(theme, total_records) %>%
    dplyr::mutate(theme = factor(theme, levels = theme_order))

  theme_x_max <- max(theme_totals$total_records, na.rm = TRUE)
  if (!is.finite(theme_x_max) || theme_x_max <= 0) theme_x_max <- 1

  p_theme <- ggplot2::ggplot(
    theme_plot_data,
    ggplot2::aes(x = n_records, y = theme, fill = source_directness)
  ) +
    ggplot2::geom_col(width = 0.70, colour = NA) +
    ggplot2::geom_text(
      data = theme_totals,
      ggplot2::aes(
        x = total_records,
        y = theme,
        label = total_records
      ),
      inherit.aes = FALSE,
      hjust = -0.22,
      size = 4.8,
      colour = figure_text
    ) +
    ggplot2::scale_fill_manual(
      values = blue_palette,
      breaks = directness_levels,
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(
      breaks = NULL,
      labels = NULL,
      limits = c(0, theme_x_max * 1.14 + 0.5),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(
      title = "Contextual themes by directness to South Africa",
      subtitle = "Non-mutually exclusive record-level mentions",
      fill = NULL,
      caption = paste0(
        "Keyword-derived tags require quote-level checking. Strict primary evidence direct on ",
        "young people, GenAI and South Africa: n=",
        sum(contributing$mhaa_all_three_strict_primary, na.rm = TRUE), "."
      )
    ) +
    base_sa_figure_theme +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.text = ggplot2::element_text(
        size = 10.2,
        colour = "#404040"
      ),
      legend.key.width = grid::unit(0.85, "cm"),
      legend.key.height = grid::unit(0.40, "cm")
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        nrow = 2,
        byrow = TRUE,
        reverse = FALSE
      )
    )

  ggplot2::ggsave(
    filename = file.path(output_dir, "RQ_SA_context_theme_by_directness.png"),
    plot = p_theme,
    width = 12.5,
    height = 8.0,
    dpi = 320,
    bg = figure_background
  )
}

# ---- 12. Generate Section 4.7 narrative scaffold -----------------------------

n_sa_record <- sum(contributing$south_africa_direct, na.rm = TRUE)
n_sa_primary <- sum(contributing$south_africa_primary, na.rm = TRUE)
n_sa_review <- sum(
  contributing$south_africa_direct & contributing$design_group == "Evidence synthesis",
  na.rm = TRUE
)
n_young <- sum(contributing$adolescent_or_young_direct, na.rm = TRUE)
n_genai <- sum(contributing$genai_direct, na.rm = TRUE)
n_all_three_record <- sum(contributing$mhaa_all_three_record_level, na.rm = TRUE)
n_all_three_strict <- sum(contributing$mhaa_all_three_strict_primary, na.rm = TRUE)

cit_help <- example_citations("Help-seeking, disclosure and engagement")
cit_stigma <- example_citations("Stigma and social context")
cit_service <- example_citations("Service access and referral")
cit_language <- example_citations("Language, culture and concepts of distress")
cit_gender <- example_citations("Gender and differential access")
cit_deploy <- example_citations("Prior South African digital deployments")
cit_reg <- example_citations("Regulatory and data context")


preliminary_answer <- paste0(
  "The directly relevant South African evidence was limited, but it identified ",
  "several contextual issues that may shape uptake and safe implementation. These included stigma ",
  "and fear of judgement, preference for human or blended support, shortages and geographic ",
  "concentration of services, and the importance of privacy and anonymity",
  cite_or_placeholder(collapse_citations(c(cit_help, cit_stigma, cit_service))),
  ". South African evidence also highlighted the need for local-language support, code-switching ",
  "and options suitable for users with lower literacy, while review-mediated evidence identified ",
  "gender- and identity-related differences in distress and access",
  cite_or_placeholder(collapse_citations(c(cit_language, cit_gender))),
  ". No strict primary record in this stream directly evaluated a GenAI mental-health intervention ",
  "for young people in South Africa, so these findings should inform adaptation and testing rather ",
  "than be treated as evidence that a particular design will work."
)

evidence_base_paragraph <- paste0(
  "We identified ", contributing_n, " records with information in at least one South African-context ",
  "field. The contributing evidence comprised ",
  format_count_list(design_group_counts, "design_group", "n", max_items = 6),
  ". Settings were ",
  format_count_list(setting_counts, "category", "n", max_items = 6),
  ", and the technology or object types ranged from rule-based and conventional AI chatbots to ",
  "GenAI or LLM systems and broader digital mental-health interventions. ",
  n_sa_record, " records included South Africa, of which ", n_sa_primary,
  " provided direct South African primary evidence and ", n_sa_review,
  " included South Africa through an evidence synthesis. Among all contributing records, ",
  n_young, " directly concerned adolescents or young people and ", n_genai,
  " directly concerned GenAI or LLM systems. The record-level intersection of young people, GenAI ",
  "and South Africa was ", format_n_pct(n_all_three_record, contributing_n),
  ", but the stricter primary-study intersection was ",
  format_n_pct(n_all_three_strict, contributing_n),
  ". The latter is the more defensible measure of evidence directly matching MHAA."
)

key_findings_1 <- paste0(
  "Help-seeking, stigma, service access and referral. The most directly relevant South African ",
  "qualitative evidence suggested that some users would prefer in-person counselling or use a ",
  "chatbot as a supplement rather than a replacement. Privacy, anonymity and freedom from judgement ",
  "were valued, particularly where stigma or discrimination discouraged disclosure and engagement. ",
  "Access was also constrained by shortages of mental-health providers, concentration of services in ",
  "urban areas, distance, cost and limited services tailored to marginalised groups",
  cite_or_placeholder(collapse_citations(c(cit_help, cit_stigma, cit_service))),
  ". Referral was frequently discussed as an important function, but the extracted evidence provided ",
  "limited detail on the capacity, availability or completion of referral pathways."
)

key_findings_2 <- paste0(
  "Language, culture, literacy and population differences. South African records identified ",
  "isiZulu and English use and called for support for isiZulu, isiXhosa and code-switching, including ",
  "the ability to express emotions in the language that feels most natural. Voice-based interaction ",
  "was suggested as one way to improve access for users with lower literacy or limited English",
  cite_or_placeholder(cit_language),
  ". Evidence on gender and other differences was thinner. It included review-mediated examples ",
  "concerning intimate-partner violence among young women and stigma affecting men who have sex with ",
  "men and transgender women, but did not provide a comprehensive account of how gender, disability, ",
  "rurality or socioeconomic position affect digital engagement",
  cite_or_placeholder(cit_gender),
  ". Named South African deployments were identified, but implementation lessons on adaptation, ",
  "engagement and referral were often not reported in detail",
  cite_or_placeholder(cit_deploy),
  "."
)

implications_paragraph <- paste0(
  "The evidence supports treating language, privacy, stigma and service availability as explicit ",
  "design and implementation considerations rather than as background characteristics. MHAA should ",
  "therefore test whether users can disclose distress comfortably in locally relevant language, ",
  "including code-switching, and whether its privacy and anonymity messaging is understood by young ",
  "people who may share devices or fear judgement. It should also specify what the intervention can ",
  "and cannot provide and assess whether referral options are geographically accessible, affordable ",
  "and able to accept users, rather than measuring referral only as a link or message being shown. ",
  "These implications are grounded in the extracted South African evidence on stigma, language and ",
  "service constraints",
  cite_or_placeholder(collapse_citations(c(cit_stigma, cit_language, cit_service))),
  ". As a review-team interpretation, MHAA should co-design and pilot the language choices, disclosure ",
  "flow and referral pathway with South African young people and service partners, retain a route to ",
  "human support, and examine engagement and safety separately by language, gender, location and other ",
  "relevant groups. The review does not establish that any one interface, language configuration or ",
  "referral model is superior."
)

gaps_paragraph <- paste0(
  "The evidence base was sparse and predominantly indirect. Only ", n_sa_primary,
  " contributing record provided direct South African primary evidence, and none met the strict ",
  "primary-study definition of being simultaneously about young people, GenAI and South Africa. ",
  "Field coverage was uneven: help-seeking patterns and gender-specific drivers were each populated ",
  "in only ",
  field_coverage %>% dplyr::filter(field_id == "SA.1") %>% dplyr::pull(n_value),
  " record(s), while language and service constraints were more commonly described. The extraction ",
  "contained no robust South African reading-age estimate and limited South Africa-specific material ",
  "on POPIA, SAHPRA, referral capacity or completion of referrals. Several findings came from reviews ",
  "that mentioned South Africa without reporting the underlying study in detail, and records or ",
  "reviews may overlap. Keyword tags, country flags and any record classified as direct on all three ",
  "MHAA dimensions should therefore be checked against the supporting quotation and, where necessary, ",
  "the source publication before the final section is drafted."
)

section_lines <- c(
  "# 4.7 South African context (additional stream)",
  "",
  "**Research question.** What contextual factors should inform the design, implementation, and evaluation of a GenAI mental health intervention for young people in South Africa?",
  "",
  "**Interpretation note.** Counts describe records and extraction coverage. Mentions from reviews, frameworks and non-South African studies are indirect evidence and are not treated as South African prevalence estimates.",
  "",
  "**Direct preliminary answer.**",
  preliminary_answer,
  "",
  "**Evidence base.**",
  evidence_base_paragraph,
  "",
  "**Key findings.**",
  "",
  "**Help-seeking, stigma, service access and referral.**",
  key_findings_1,
  "",
  "**Language, culture, literacy and population differences.**",
  key_findings_2,
  "",
  "**Implications for MHAA.**",
  implications_paragraph,
  "",
  "**Gaps and caveats.**",
  gaps_paragraph,
  "",
  "## Manual checks before using this draft",
  "1. Verify every South Africa-specific statement against its value, quotation and source location.",
  "2. Confirm whether the multi-agent South African chatbot is explicitly generative before calling it GenAI.",
  "3. Treat review-level intersections of young people, GenAI and South Africa as review-mediated unless all three refer to the same underlying study.",
  "4. Recheck L&MIC and SSA flags before presenting geographic totals.",
  "5. Replace generic or review-mediated citations with the underlying South African primary study where available.",
  "6. Do not infer referral effectiveness from the presence of a referral link, message or recommendation."
)

readr::write_lines(
  section_lines,
  file.path(output_dir, "Section-4.7-South-African-context-scaffold.md")
)

# ---- 13. Record-level export --------------------------------------------------

record_level_export <- contributing %>%
  dplyr::select(
    record_id, author_year, title, bibliographic,
    country, setting, sample_size, age_band, study_design, design_group,
    object_type, architecture_class, rq_addressed,
    lmic, ssa, south_africa,
    adolescent_or_young_direct, genai_direct,
    south_africa_direct, south_africa_primary,
    mhaa_all_three_record_level, mhaa_all_three_strict_primary,
    source_directness, index_flagged_sa, needs_human,
    dplyr::all_of(joined_value_cols)
  ) %>%
  dplyr::arrange(source_directness, record_id)

# ---- 14. Export CSV and workbook outputs -------------------------------------

readr::write_csv(
  record_level_export,
  file.path(output_dir, "RQ_SA_context_record_level.csv"),
  na = ""
)

readr::write_csv(
  field_coverage,
  file.path(output_dir, "RQ_SA_context_field_coverage.csv"),
  na = ""
)

readr::write_csv(
  representative_evidence,
  file.path(output_dir, "RQ_SA_context_theme_detail.csv"),
  na = ""
)

readr::write_csv(
  theme_summary,
  file.path(output_dir, "RQ_SA_context_theme_summary.csv"),
  na = ""
)

readr::write_csv(
  quote_bank,
  file.path(output_dir, "RQ_SA_context_quote_bank.csv"),
  na = ""
)

readr::write_csv(
  verification_queue,
  file.path(output_dir, "RQ_SA_context_verification_queue.csv"),
  na = ""
)

output_workbook <- list(
  overview = tibble::tibble(
    metric = c(
      "All aligned records",
      "Records contributing at least one SA-context field",
      "South Africa direct at record level",
      "Direct South African primary evidence",
      "Young-person direct",
      "GenAI or LLM direct",
      "Record-level all-three intersection",
      "Strict primary all-three intersection",
      "Blank ID rows in sa sheet"
    ),
    n = c(
      stream_n,
      contributing_n,
      n_sa_record,
      n_sa_primary,
      n_young,
      n_genai,
      n_all_three_record,
      n_all_three_strict,
      blank_id_rows
    )
  ),
  directness_metrics = directness_metrics,
  source_directness = source_directness_counts,
  field_coverage = field_coverage,
  design_groups = design_group_counts,
  study_designs = study_design_counts,
  settings = setting_counts,
  countries = country_counts,
  architectures = architecture_counts,
  theme_summary = theme_summary,
  subtheme_summary = subtheme_summary,
  theme_detail = representative_evidence,
  quote_bank = quote_bank,
  verification_queue = verification_queue,
  record_level = record_level_export,
  codebook_sa = sa_codebook
)

writexl::write_xlsx(
  output_workbook,
  path = file.path(output_dir, "RQ_SA_context_analysis_outputs.xlsx")
)

# ---- 15. Completion message ---------------------------------------------------

message("South African context analysis complete.")
message("Input workbook: ", normalizePath(input_file, winslash = "/"))
message("Records with >=1 populated SA-context field: ", contributing_n)
message("South Africa direct at record level: ", n_sa_record)
message("Direct South African primary evidence: ", n_sa_primary)
message("Strict primary all-three intersection: ", n_all_three_strict)
message("Outputs written to: ", normalizePath(output_dir, winslash = "/"))
message("Open Section-4.7-South-African-context-scaffold.md and RQ_SA_context_analysis_outputs.xlsx first.")
