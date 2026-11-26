# ==============================================================================
# RQ 1b — Intervention design analysis pipeline
# Research question:
# What intervention modalities, therapeutic approaches, and design features
# have been used, and are they associated with different outcomes?
#
# Designed for:
#   extraction_final-revised-SL_n=325.xlsx
#
# Main outputs:
#   RQ1b_outputs/RQ1b_analysis_outputs.xlsx
#   RQ1b_outputs/RQ1b_record_level.csv
#   RQ1b_outputs/RQ1b_design_outcome_links.csv
#   RQ1b_outputs/RQ1b_quote_bank.csv
#   RQ1b_outputs/RQ1b_narrative_scaffold.md
#   RQ1b_outputs/*.png
#
# Important interpretation rule:
# Counts describe the evidence base and reporting patterns. They must not be
# treated as evidence that a frequently reported feature is more effective.
# Feature–outcome claims should be based primarily on the parsed 1b.11 evidence
# and should distinguish experimental comparisons, observational associations,
# qualitative accounts, and author interpretation.
# ==============================================================================

# ---- 0. Packages and file paths ----------------------------------------------

required_packages <- c(
  "readxl", "dplyr", "tidyr", "purrr", "stringr", "readr",
  "tibble", "ggplot2", "writexl"
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

# Change this path only if the workbook is not in the same folder as this script.
input_file <- "extraction_final-revised-SL_n=325.xlsx"
output_dir <- "RQ1b_outputs"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop(
    "Input workbook not found: ", input_file,
    "\nSet input_file to the correct workbook path."
  )
}

# ---- 1. General helper functions ---------------------------------------------

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

extract_composite_text <- function(x, key, next_keys = NULL) {
  x <- dplyr::coalesce(as.character(x), "")

  if (is.null(next_keys) || length(next_keys) == 0) {
    pattern <- paste0(
      "(?is)(?:^|[|;]\\s*)", key, "\\s*:\\s*(.*)$"
    )
  } else {
    pattern <- paste0(
      "(?is)(?:^|[|;]\\s*)", key, "\\s*:\\s*(.*?)",
      "(?=[|;]\\s*(?:", paste(next_keys, collapse = "|"), ")\\s*:|$)"
    )
  }

  blank_to_na(stringr::str_match(x, stringr::regex(pattern))[, 2])
}

extract_first_number <- function(x, pattern) {
  matched <- stringr::str_match(
    dplyr::coalesce(as.character(x), ""),
    stringr::regex(pattern, ignore_case = TRUE)
  )[, 2]
  suppressWarnings(as.numeric(stringr::str_remove_all(matched, ",")))
}

safe_pct <- function(n, denominator) {
  ifelse(is.na(denominator) | denominator == 0, NA_real_, 100 * n / denominator)
}

field_number <- function(field_id) {
  suppressWarnings(
    as.numeric(stringr::str_extract(field_id, "(?<=1b\\.)[0-9]+"))
  )
}

format_n_pct <- function(n, denominator, digits = 1) {
  if (is.na(denominator) || denominator == 0) {
    return(sprintf("n=%s", n))
  }
  sprintf(paste0("n=%s (%.", digits, "f%%)"), n, 100 * n / denominator)
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

# Apply a transparent regex dictionary to one text field.
tag_text <- function(data, text_col, dictionary) {
  text_data <- data %>%
    dplyr::transmute(
      record_id,
      title,
      text = dplyr::coalesce(as.character(.data[[text_col]]), "")
    )

  tidyr::crossing(text_data, dictionary) %>%
    dplyr::filter(
      stringr::str_detect(
        text,
        stringr::regex(pattern, ignore_case = TRUE)
      )
    ) %>%
    dplyr::select(record_id, title, domain, tag, source_field, text) %>%
    dplyr::distinct()
}

# ---- 2. Read workbook sheets as text -----------------------------------------

read_sheet_text <- function(sheet_name) {
  readxl::read_excel(
    input_file,
    sheet = sheet_name,
    col_types = "text",
    .name_repair = "minimal"
  )
}

index_raw <- read_sheet_text("Index")
baseline_raw <- read_sheet_text("baseline")
rq1b_raw <- read_sheet_text("1b")
review_queue_raw <- read_sheet_text("Review queue")
codebook_raw <- read_sheet_text("Codebook")

# ---- 3. Select and rename relevant fields ------------------------------------

baseline <- baseline_raw %>%
  dplyr::transmute(
    record_id = blank_to_na(record_id),
    title_baseline = blank_to_na(title),
    bibliographic = blank_to_na(`base.1 value`),
    country = blank_to_na(`base.2 value`),
    setting = blank_to_na(`base.3 value`),
    sample_size = blank_to_na(`base.4 value`),
    age_band = blank_to_na(`base.5 value`),
    study_design = blank_to_na(`base.6 value`),
    object_type = blank_to_na(`base.7 value`),
    rq_addressed = blank_to_na(`base.8 value`),
    outcomes_reported = blank_to_na(`base.9 value`),
    lmic_flag_raw = blank_to_na(`base.10 value`),
    ssa_flag_raw = blank_to_na(`base.11a value`),
    south_africa_flag_raw = blank_to_na(`base.11b value`),
    evidence_type_tag = blank_to_na(`base.12 value`),
    minimal_quality = blank_to_na(`base.15 value`)
  ) %>%
  dplyr::filter(has_text(record_id))

rq1b <- rq1b_raw %>%
  dplyr::transmute(
    record_id = blank_to_na(record_id),
    title_1b = blank_to_na(title),

    modality = blank_to_na(`1b.1 value`),
    modality_quotes = blank_to_na(`1b.1 quotes`),
    modality_loc = blank_to_na(`1b.1 loc`),
    modality_agree = blank_to_na(`1b.1 agree`),

    therapeutic_approach = blank_to_na(`1b.2 value`),
    therapeutic_approach_quotes = blank_to_na(`1b.2 quotes`),
    therapeutic_approach_loc = blank_to_na(`1b.2 loc`),
    therapeutic_approach_agree = blank_to_na(`1b.2 agree`),

    best_elements = blank_to_na(`1b.3 value`),
    best_elements_quotes = blank_to_na(`1b.3 quotes`),
    best_elements_loc = blank_to_na(`1b.3 loc`),
    best_elements_agree = blank_to_na(`1b.3 agree`),

    session_structure = blank_to_na(`1b.4 value`),
    session_structure_quotes = blank_to_na(`1b.4 quotes`),
    session_structure_loc = blank_to_na(`1b.4 loc`),
    session_structure_agree = blank_to_na(`1b.4 agree`),

    architecture = blank_to_na(`1b.5 value`),
    architecture_quotes = blank_to_na(`1b.5 quotes`),
    architecture_loc = blank_to_na(`1b.5 loc`),
    architecture_agree = blank_to_na(`1b.5 agree`),

    personalisation = blank_to_na(`1b.6 value`),
    personalisation_quotes = blank_to_na(`1b.6 quotes`),
    personalisation_loc = blank_to_na(`1b.6 loc`),
    personalisation_agree = blank_to_na(`1b.6 agree`),

    content_corpus = blank_to_na(`1b.7 value`),
    content_corpus_quotes = blank_to_na(`1b.7 quotes`),
    content_corpus_loc = blank_to_na(`1b.7 loc`),
    content_corpus_agree = blank_to_na(`1b.7 agree`),

    interaction_design = blank_to_na(`1b.8 value`),
    interaction_design_quotes = blank_to_na(`1b.8 quotes`),
    interaction_design_loc = blank_to_na(`1b.8 loc`),
    interaction_design_agree = blank_to_na(`1b.8 agree`),

    human_support = blank_to_na(`1b.9 value`),
    human_support_quotes = blank_to_na(`1b.9 quotes`),
    human_support_loc = blank_to_na(`1b.9 loc`),
    human_support_agree = blank_to_na(`1b.9 agree`),

    engagement_features = blank_to_na(`1b.10 value`),
    engagement_features_quotes = blank_to_na(`1b.10 quotes`),
    engagement_features_loc = blank_to_na(`1b.10 loc`),
    engagement_features_agree = blank_to_na(`1b.10 agree`),

    design_outcome_link = blank_to_na(`1b.11 value`),
    design_outcome_link_quotes = blank_to_na(`1b.11 quotes`),
    design_outcome_link_loc = blank_to_na(`1b.11 loc`),
    design_outcome_link_agree = blank_to_na(`1b.11 agree`),

    co_design = blank_to_na(`1b.12 value`),
    co_design_quotes = blank_to_na(`1b.12 quotes`),
    co_design_loc = blank_to_na(`1b.12 loc`),
    co_design_agree = blank_to_na(`1b.12 agree`),

    comparator = blank_to_na(`1b.13 value`),
    comparator_quotes = blank_to_na(`1b.13 quotes`),
    comparator_loc = blank_to_na(`1b.13 loc`),
    comparator_agree = blank_to_na(`1b.13 agree`)
  ) %>%
  dplyr::filter(has_text(record_id))

index_data <- index_raw %>%
  dplyr::transmute(
    record_id = blank_to_na(record_id),
    groups_extracted = blank_to_na(groups_extracted),
    gated_out_groups = blank_to_na(gated_out_groups),
    total_reported = suppressWarnings(as.numeric(total_reported)),
    needs_human = suppressWarnings(as.numeric(needs_human))
  ) %>%
  dplyr::filter(has_text(record_id))

# ---- 4. Create record-level master data --------------------------------------

rq1b_value_cols <- c(
  "modality", "therapeutic_approach", "best_elements", "session_structure",
  "architecture", "personalisation", "content_corpus", "interaction_design",
  "human_support", "engagement_features", "design_outcome_link", "co_design",
  "comparator"
)

master <- baseline %>%
  dplyr::full_join(rq1b, by = "record_id") %>%
  dplyr::left_join(index_data, by = "record_id")

master$rq1b_any <- apply(
  dplyr::select(master, dplyr::all_of(rq1b_value_cols)),
  1,
  function(row) any(has_text(row))
)

master <- master %>%
  dplyr::mutate(
    title = dplyr::coalesce(title_baseline, title_1b),
    index_flagged_1b = stringr::str_detect(
      dplyr::coalesce(groups_extracted, ""),
      stringr::regex("(^|,\\s*)1b(\\s*,|$)", ignore_case = TRUE)
    ),
    lmic = parse_bool(lmic_flag_raw),
    ssa = parse_bool(ssa_flag_raw),
    south_africa = parse_bool(south_africa_flag_raw),
    age_10_19 = extract_composite_bool(age_band, "maps_10_19"),
    age_10_24 = extract_composite_bool(age_band, "maps_10_24"),
    age_18_24 = extract_composite_bool(age_band, "maps_18_24"),
    age_as_reported = extract_composite_text(
      age_band,
      "as_reported",
      c("maps_10_19", "maps_10_24", "maps_18_24")
    ),
    sample_n = extract_first_number(sample_size, "\\bn\\s*:\\s*([0-9][0-9,]*)"),
    design_group = dplyr::case_when(
      study_design %in% c("RCT", "non-RCT trial", "cohort", "cross-sectional") ~
        "Primary quantitative",
      study_design %in% c("qualitative", "mixed-methods") ~
        "Qualitative or mixed-methods",
      study_design %in% c("SR", "SR+MA", "scoping", "umbrella") ~
        "Evidence synthesis",
      study_design %in% c("framework", "regulatory") ~
        "Framework or guidance",
      TRUE ~ "Other or not reported"
    )
  ) %>%
  dplyr::select(-title_baseline, -title_1b)

# ---- 5. Operationalise directness to MHAA ------------------------------------

# Editable operationalisation of the three main dimensions:
#   1) population: adolescent/young-person evidence (maps to 10–19 or 10–24);
#   2) technology: generative-AI/LLM evidence;
#   3) context: South Africa.
#
# Single-session relevance is calculated separately because it is a design
# decision rather than one of the three population/technology/context dimensions.

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
  "scripted dialogue|finite[- ]state|deterministic response"
)

ai_unspecified_pattern <- paste0(
  "artificial intelligence|\\bai[- ](?:based|powered|enabled|driven)\\b|",
  "natural language processing|\\bnlp\\b|machine learning"
)

master <- master %>%
  dplyr::mutate(
    technology_text = stringr::str_c(
      dplyr::coalesce(object_type, ""),
      dplyr::coalesce(modality, ""),
      dplyr::coalesce(architecture, ""),
      sep = " | "
    ),
    mentions_genai = stringr::str_detect(
      technology_text,
      stringr::regex(genai_pattern, ignore_case = TRUE)
    ),
    mentions_rule_based = stringr::str_detect(
      technology_text,
      stringr::regex(rule_based_pattern, ignore_case = TRUE)
    ),
    mentions_ai_unspecified = stringr::str_detect(
      technology_text,
      stringr::regex(ai_unspecified_pattern, ignore_case = TRUE)
    ),
    adolescent_or_young_direct = dplyr::coalesce(age_10_19, FALSE) |
      dplyr::coalesce(age_10_24, FALSE),
    genai_direct = mentions_genai,
    south_africa_direct = dplyr::coalesce(south_africa, FALSE),
    mhaa_all_three_direct = adolescent_or_young_direct &
      genai_direct & south_africa_direct,
    explicit_single_session = stringr::str_detect(
      dplyr::coalesce(session_structure, ""),
      stringr::regex(
        "single[- ]session|one[- ]session|one session|single encounter|one[- ]off",
        ignore_case = TRUE
      )
    ),
    architecture_class = dplyr::case_when(
      design_group == "Evidence synthesis" & mentions_genai & mentions_rule_based ~
        "Multiple/varied architectures",
      mentions_genai & mentions_rule_based ~ "Hybrid generative + deterministic",
      mentions_genai ~ "Generative AI / LLM",
      mentions_rule_based ~ "Rule-based / deterministic",
      mentions_ai_unspecified ~ "AI/NLP, architecture unclear",
      TRUE ~ "Other digital or unclear"
    )
  )

contributing <- master %>% dplyr::filter(rq1b_any)
stream_n <- nrow(master)
contributing_n <- nrow(contributing)

# ---- 6. Long-form field coverage and quote bank ------------------------------

codebook_1b <- codebook_raw %>%
  dplyr::filter(group == "1b") %>%
  dplyr::select(group, field_id, label, type, allowed, what, feeds_eval)

rq1b_long <- rq1b_raw %>%
  dplyr::filter(has_text(record_id)) %>%
  tidyr::pivot_longer(
    cols = dplyr::matches("^1b\\.\\d+ (value|quotes|loc|agree)$"),
    names_to = c("field_id", ".value"),
    names_pattern = "(1b\\.\\d+) (value|quotes|loc|agree)"
  ) %>%
  dplyr::mutate(
    dplyr::across(c(value, quotes, loc, agree), blank_to_na)
  ) %>%
  dplyr::left_join(codebook_1b, by = "field_id") %>%
  dplyr::left_join(
    master %>%
      dplyr::select(
        record_id, design_group, country, setting, age_as_reported,
        adolescent_or_young_direct, genai_direct, south_africa_direct,
        mhaa_all_three_direct, architecture_class
      ),
    by = "record_id"
  )

field_coverage <- rq1b_long %>%
  dplyr::group_by(field_id, label) %>%
  dplyr::summarise(
    n_reported = sum(has_text(value)),
    pct_contributing = safe_pct(n_reported, contributing_n),
    pct_stream = safe_pct(n_reported, stream_n),
    n_with_quotes = sum(has_text(quotes)),
    n_conflict = sum(
      stringr::str_to_lower(dplyr::coalesce(agree, "")) == "conflict"
    ),
    n_minor = sum(
      stringr::str_to_lower(dplyr::coalesce(agree, "")) == "minor"
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(field_number(field_id))

quote_bank <- rq1b_long %>%
  dplyr::filter(has_text(value) | has_text(quotes)) %>%
  dplyr::select(
    record_id, title, field_id, label, value, quotes, loc, agree,
    design_group, country, setting, age_as_reported,
    adolescent_or_young_direct, genai_direct, south_africa_direct,
    mhaa_all_three_direct, architecture_class
  ) %>%
  dplyr::arrange(field_id, record_id)

# ---- 7. Evidence-base tables -------------------------------------------------

country_counts <- contributing %>%
  dplyr::transmute(
    country = dplyr::if_else(has_text(country), country, "Not reported")
  ) %>%
  tidyr::separate_rows(country, sep = "\\s*,\\s*") %>%
  dplyr::mutate(country = stringr::str_squish(country)) %>%
  dplyr::count(country, sort = TRUE, name = "n_records") %>%
  dplyr::mutate(pct_contributing = safe_pct(n_records, contributing_n))

setting_counts <- standard_count(contributing, "setting") %>%
  dplyr::rename(setting = category) %>%
  dplyr::mutate(pct_contributing = safe_pct(n, contributing_n))

study_design_counts <- standard_count(contributing, "study_design") %>%
  dplyr::rename(study_design = category) %>%
  dplyr::mutate(pct_contributing = safe_pct(n, contributing_n))

design_group_counts <- contributing %>%
  dplyr::count(design_group, sort = TRUE, name = "n") %>%
  dplyr::mutate(pct_contributing = safe_pct(n, contributing_n))

architecture_counts <- contributing %>%
  dplyr::count(architecture_class, sort = TRUE, name = "n") %>%
  dplyr::mutate(pct_contributing = safe_pct(n, contributing_n))

age_mapping_counts <- tibble::tibble(
  age_mapping = c("Maps to 10–19", "Maps to 10–24", "Maps to 18–24"),
  n = c(
    sum(dplyr::coalesce(contributing$age_10_19, FALSE)),
    sum(dplyr::coalesce(contributing$age_10_24, FALSE)),
    sum(dplyr::coalesce(contributing$age_18_24, FALSE))
  )
) %>%
  dplyr::mutate(pct_contributing = safe_pct(n, contributing_n))

geography_counts <- tibble::tibble(
  geography = c("L&MIC", "Sub-Saharan Africa", "South Africa"),
  n = c(
    sum(dplyr::coalesce(contributing$lmic, FALSE)),
    sum(dplyr::coalesce(contributing$ssa, FALSE)),
    sum(dplyr::coalesce(contributing$south_africa, FALSE))
  )
) %>%
  dplyr::mutate(pct_contributing = safe_pct(n, contributing_n))

# ---- 8. Modality and technology tags ----------------------------------------

modality_dictionary <- tibble::tribble(
  ~domain, ~tag, ~pattern,
  "Product/delivery", "Chatbot or conversational agent", "chatbot|conversational agent|relational agent|virtual therapist|embodied conversational agent",
  "Product/delivery", "Mobile app", "mobile app|smartphone app|app[- ]based|android|ios",
  "Product/delivery", "Web platform/module", "web[- ]based|web platform|website|browser[- ]based|web module",
  "Product/delivery", "SMS or messaging platform", "\\bsms\\b|text messag|instant messag|whatsapp|telegram|facebook messenger|wechat",
  "Product/delivery", "Voice/IVR", "\\bivr\\b|interactive voice response|voice[- ]based|voice assistant|spoken interaction",
  "Product/delivery", "Virtual reality", "virtual reality|\\bvr\\b",
  "Product/delivery", "Robot or embodied agent", "\\brobot\\b|robotic|embodied agent|embodied conversational agent",
  "Interaction mode", "Text-based", "text[- ]based|written interaction|typed input|free[- ]text",
  "Interaction mode", "Audio/voice", "audio|voice[- ]based|spoken interaction|speech",
  "Interaction mode", "Multimodal", "multimodal|video.*text|text.*video|audio.*visual|written and spoken|spoken and visual",
  "Interaction timing", "Asynchronous", "asynchronous|at their convenience|on demand|anytime",
  "Interaction timing", "Synchronous", "synchronous|real[- ]time interaction|live interaction",
  "Delivery support", "Self-guided/automated", "self[- ]guided|fully automated|automated intervention|standalone system",
  "Delivery support", "Guided or supported", "clinician[- ]supported|coach[- ]supported|therapist[- ]supported|human assistance|supported by (?:a )?(?:clinician|coach|therapist|counsellor|counselor)"
) %>%
  dplyr::mutate(source_field = "1b.1 modality and delivery format")

modality_tags <- tag_text(contributing, "modality", modality_dictionary)

modality_tag_counts <- modality_tags %>%
  dplyr::count(domain, tag, sort = TRUE, name = "n_records") %>%
  dplyr::mutate(pct_contributing = safe_pct(n_records, contributing_n))

# ---- 9. Therapeutic approach and SSI tags -----------------------------------

approach_dictionary <- tibble::tribble(
  ~domain, ~tag, ~pattern,
  "Therapeutic approach", "Cognitive behavioural therapy (CBT)", "cognitive behavio(?:u)?ral therapy|\\bcbt\\b",
  "Therapeutic approach", "Behavioural activation", "behavio(?:u)?ral activation|\\bba\\b",
  "Therapeutic approach", "Cognitive restructuring/reframing", "cognitive restructur|cognitive refram|thought challeng",
  "Therapeutic approach", "Psychoeducation", "psychoeducation|psycho-education|educational content",
  "Therapeutic approach", "Acceptance and commitment therapy (ACT)", "acceptance and commitment therapy|\\bact\\b",
  "Therapeutic approach", "Mindfulness", "mindfulness|meditation|mindful breathing",
  "Therapeutic approach", "Growth mindset", "growth[- ]mindset|growth mindset",
  "Therapeutic approach", "Self-compassion", "self[- ]compassion",
  "Therapeutic approach", "Interpersonal therapy (IPT)", "interpersonal therapy|\\bipt\\b",
  "Therapeutic approach", "Dialectical behaviour therapy (DBT)", "dialectical behavio(?:u)?r therapy|\\bdbt\\b",
  "Therapeutic approach", "Problem-solving", "problem[- ]solving|problem solving therapy",
  "Therapeutic approach", "Motivational interviewing", "motivational interviewing|\\bmi\\b",
  "Therapeutic approach", "Safety planning/crisis planning", "safety plan|crisis plan",
  "Therapeutic approach", "Emotion regulation", "emotion regulation|emotional regulation",
  "Therapeutic approach", "Positive psychology", "positive psychology|positive affect|gratitude",
  "Therapeutic approach", "Screening/referral/navigation", "screening|referral|resource navigation|stepped care",
  "Therapeutic approach", "Supportive counselling/listening", "supportive counsel|supportive listening|active listening|emotional support"
) %>%
  dplyr::mutate(source_field = "1b.2 therapeutic approach")

contributing <- contributing %>%
  dplyr::mutate(
    therapeutic_approach_text = stringr::str_remove(
      dplyr::coalesce(therapeutic_approach, ""),
      stringr::regex("\\|\\s*ssi_framing\\s*:.*$", ignore_case = TRUE)
    ),
    ssi_framing = extract_composite_bool(therapeutic_approach, "ssi_framing")
  )

# Update the same fields in master for the record-level export.
master <- master %>%
  dplyr::left_join(
    contributing %>%
      dplyr::select(record_id, therapeutic_approach_text, ssi_framing),
    by = "record_id"
  )

approach_tags <- tag_text(
  contributing,
  "therapeutic_approach_text",
  approach_dictionary
)

approach_tag_counts <- approach_tags %>%
  dplyr::count(tag, sort = TRUE, name = "n_records") %>%
  dplyr::mutate(pct_contributing = safe_pct(n_records, contributing_n))

ssi_summary <- tibble::tibble(
  ssi_framing = c("Explicit SSI framing", "Explicitly not SSI", "Not extractable/not reported"),
  n = c(
    sum(dplyr::coalesce(contributing$ssi_framing, FALSE)),
    sum(contributing$ssi_framing %in% FALSE, na.rm = TRUE),
    sum(is.na(contributing$ssi_framing))
  )
) %>%
  dplyr::mutate(pct_contributing = safe_pct(n, contributing_n))

# ---- 10. Session dose and structure -----------------------------------------

session_text <- dplyr::coalesce(contributing$session_structure, "")

minute_range <- stringr::str_match(
  session_text,
  stringr::regex(
    "([0-9]{1,3})\\s*[–—-]\\s*([0-9]{1,3})\\s*(?:minutes?|mins?)",
    ignore_case = TRUE
  )
)

minute_single <- stringr::str_match(
  session_text,
  stringr::regex("([0-9]{1,3})\\s*(?:minutes?|mins?)", ignore_case = TRUE)
)[, 2]

session_number_range <- stringr::str_match(
  session_text,
  stringr::regex(
    "([0-9]{1,2})\\s*[–—-]\\s*([0-9]{1,2})\\s+(?:weekly\\s+)?sessions?",
    ignore_case = TRUE
  )
)

session_number_single <- stringr::str_match(
  session_text,
  stringr::regex(
    "([0-9]{1,2})\\s+(?:weekly\\s+)?sessions?",
    ignore_case = TRUE
  )
)[, 2]

contributing <- contributing %>%
  dplyr::mutate(
    session_minutes_min = dplyr::coalesce(
      suppressWarnings(as.numeric(minute_range[, 2])),
      suppressWarnings(as.numeric(minute_single))
    ),
    session_minutes_max = dplyr::coalesce(
      suppressWarnings(as.numeric(minute_range[, 3])),
      suppressWarnings(as.numeric(minute_single))
    ),
    sessions_min = dplyr::case_when(
      explicit_single_session ~ 1,
      TRUE ~ dplyr::coalesce(
        suppressWarnings(as.numeric(session_number_range[, 2])),
        suppressWarnings(as.numeric(session_number_single))
      )
    ),
    sessions_max = dplyr::case_when(
      explicit_single_session ~ 1,
      TRUE ~ dplyr::coalesce(
        suppressWarnings(as.numeric(session_number_range[, 3])),
        suppressWarnings(as.numeric(session_number_single))
      )
    ),
    mentions_multi_session = stringr::str_detect(
      dplyr::coalesce(session_structure, ""),
      stringr::regex(
        "multi[- ]session|multiple sessions|repeated sessions|weekly sessions|daily sessions|sessions over|[2-9][0-9]*\\s+(?:weekly\\s+)?sessions",
        ignore_case = TRUE
      )
    ),
    session_class = dplyr::case_when(
      explicit_single_session & mentions_multi_session ~ "Mixed/varied dose",
      explicit_single_session ~ "Single-session",
      mentions_multi_session | dplyr::coalesce(sessions_max, 0) > 1 ~ "Multi-session/repeated",
      has_text(session_structure) ~ "Dose reported but not classifiable",
      TRUE ~ "Not reported"
    ),
    overlaps_mhaa_15_30_min = !is.na(session_minutes_min) &
      !is.na(session_minutes_max) &
      session_minutes_min <= 30 & session_minutes_max >= 15
  )

session_structure_counts <- contributing %>%
  dplyr::count(session_class, sort = TRUE, name = "n") %>%
  dplyr::mutate(pct_contributing = safe_pct(n, contributing_n))

session_detail <- contributing %>%
  dplyr::select(
    record_id, title, study_design, design_group, architecture_class,
    session_class, sessions_min, sessions_max,
    session_minutes_min, session_minutes_max, overlaps_mhaa_15_30_min,
    session_structure, session_structure_quotes, session_structure_loc,
    session_structure_agree
  )

# ---- 11. BEST-element analogue parsing --------------------------------------

best_detail <- contributing %>%
  dplyr::filter(has_text(best_elements)) %>%
  dplyr::transmute(
    record_id,
    title,
    brain_science_B = extract_composite_bool(best_elements, "brain_science_B"),
    empowerment_E = extract_composite_bool(best_elements, "empowerment_E"),
    saying_is_believing_S = extract_composite_bool(best_elements, "saying_is_believing_S"),
    testimonials_T = extract_composite_bool(best_elements, "testimonials_T"),
    evidence_span = extract_composite_text(best_elements, "evidence_span"),
    raw_value = best_elements,
    quotes = best_elements_quotes,
    location = best_elements_loc,
    agreement = best_elements_agree
  )

best_counts <- best_detail %>%
  dplyr::select(
    brain_science_B, empowerment_E,
    saying_is_believing_S, testimonials_T
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "BEST_element",
    values_to = "present"
  ) %>%
  dplyr::group_by(BEST_element) %>%
  dplyr::summarise(
    n_present = sum(dplyr::coalesce(present, FALSE)),
    n_reported = sum(!is.na(present)),
    pct_among_reported = safe_pct(n_present, n_reported),
    .groups = "drop"
  )

# ---- 12. Design-feature tags -------------------------------------------------

personalisation_dictionary <- tibble::tribble(
  ~domain, ~tag, ~pattern,
  "Personalisation", "Language/register adaptation", "language adaptation|language register|reading level|literacy level|tone adaptation|cultural language|multilingual|local language",
  "Personalisation", "Examples/content matched to user", "example matching|content matching|matched to.*preference|tailor.*content|personalized content|personalised content",
  "Personalisation", "Adaptive pacing", "adaptive pacing|pace.*adapt|user[- ]paced|self[- ]paced",
  "Personalisation", "Memory/context retention", "memory|conversation history|context[- ]aware|context retention|remember",
  "Personalisation", "Name or profile personalisation", "preferred name|uses.*name|name personali[sz]ation|profile personali[sz]ation",
  "Personalisation", "Tailoring by symptoms/preferences", "tailor|personali[sz]|matched to distress|preference|symptom level|psychological state|intent[- ]aware"
) %>%
  dplyr::mutate(source_field = "1b.6 personalisation")

interaction_dictionary <- tibble::tribble(
  ~domain, ~tag, ~pattern,
  "Interaction design", "Free-text/open-ended input", "free[- ]text|open[- ]ended|natural language input|typed input|multi[- ]turn conversation",
  "Interaction design", "Menu/button/constrained input", "menu[- ]based|buttons?|multiple choice|predefined options|constrained dialogue|fixed response options",
  "Interaction design", "Mixed input", "mixed input|free[- ]text.*menu|menu.*free[- ]text",
  "Interaction design", "Persona/avatar/character", "persona|avatar|character|embodied agent|relational agent",
  "Interaction design", "Empathetic/supportive tone", "empathetic|empathic|supportive tone|nonjudg|non-judg|warm tone|friendly language",
  "Interaction design", "Reflection/summarisation/feedback", "reflection|reflective|summari[sz]|feedback|paraphras",
  "Interaction design", "User choice/branching", "choose|choice|select.*path|branching|user-selected|pathway",
  "Interaction design", "Skip/revise/stop controls", "skip|revise|edit response|stop|pause|resume|repeat"
) %>%
  dplyr::mutate(source_field = "1b.8 interaction design")

human_support_dictionary <- tibble::tribble(
  ~domain, ~tag, ~pattern,
  "Human support", "Fully automated/no routine human support", "fully automated|no human involvement|no routine human|self[- ]guided|standalone",
  "Human support", "Clinician/therapist-supported", "clinician[- ]supported|therapist[- ]supported|psychologist|psychiatrist|clinical support|professional oversight",
  "Human support", "Coach/counsellor-supported", "coach[- ]supported|counsel(?:l)?or|coach support|health coach",
  "Human support", "Peer-supported", "peer[- ]supported|peer support|peer mentor",
  "Human support", "Research/technical monitoring", "research assistant|technical assistance|research staff|study staff|monitoring only",
  "Human support", "Human-in-the-loop/escalation", "human[- ]in[- ]the[- ]loop|human review|moderation|escalat|handoff|hand-off|route.*professional",
  "Human support", "Blended delivery", "blended|combined.*human|human assistance|integrated human"
) %>%
  dplyr::mutate(source_field = "1b.9 human-support model")

engagement_dictionary <- tibble::tribble(
  ~domain, ~tag, ~pattern,
  "Engagement", "Reminders/notifications", "reminder|notification|push notification|text message reminder",
  "Engagement", "Prompts/check-ins", "prompt|check[- ]in|mood check",
  "Engagement", "Gamification", "gamif|game[- ]like|points|badges|levels",
  "Engagement", "Progress indicators/tracking", "progress indicator|progress tracking|completion bar|usage tracking|engagement metrics",
  "Engagement", "Rewards/incentives", "reward|incentive|achievement",
  "Engagement", "Goal setting/action planning", "goal[- ]setting|goal setting|action plan|implementation intention",
  "Engagement", "Choice of content", "choice of content|choose.*content|content selection|select.*module",
  "Engagement", "Re-engagement/follow-up messages", "re-engagement|follow[- ]up message|return message|nudge",
  "Engagement", "Logging/self-monitoring", "logging|diary|journal|self[- ]monitor|sleep tracking|mood tracking|emotional tracking"
) %>%
  dplyr::mutate(source_field = "1b.10 engagement features")

co_design_dictionary <- tibble::tribble(
  ~domain, ~tag, ~pattern,
  "Development", "Formal co-design/participatory development", "co-design|codesign|participatory|human[- ]centred|human[- ]centered|user[- ]centred|user[- ]centered",
  "Development", "Youth/end-user involvement", "end users?.*(?:involv|consult|interview|workshop)|users? involved|adolescents?.*(?:involv|consult|interview|workshop)|young people.*(?:involv|consult|interview|workshop)|youth advisory|youth panel|user workshop",
  "Development", "Lived-experience involvement", "lived experience|experts? by experience|service users?",
  "Development", "Clinician/stakeholder input", "clinician|therapist|psychologist|stakeholder|mental health professional|expert feedback",
  "Development", "Prototype/usability/cognitive testing", "prototype|usability|cognitive pretest|cognitive testing|pilot test|think[- ]aloud|interview|workshop",
  "Development", "No formal co-design reported", "no formal co-design|no co-design|not co-designed|no participatory"
) %>%
  dplyr::mutate(source_field = "1b.12 co-design/formative development")

personalisation_tags <- tag_text(
  contributing, "personalisation", personalisation_dictionary
)
interaction_tags <- tag_text(
  contributing, "interaction_design", interaction_dictionary
)
human_support_tags <- tag_text(
  contributing, "human_support", human_support_dictionary
)
engagement_tags <- tag_text(
  contributing, "engagement_features", engagement_dictionary
)
co_design_tags <- tag_text(
  contributing, "co_design", co_design_dictionary
)

design_feature_tags <- dplyr::bind_rows(
  personalisation_tags,
  interaction_tags,
  human_support_tags,
  engagement_tags,
  co_design_tags
) %>%
  dplyr::distinct()

design_feature_counts <- design_feature_tags %>%
  dplyr::count(domain, tag, sort = TRUE, name = "n_records") %>%
  dplyr::mutate(pct_contributing = safe_pct(n_records, contributing_n))

# ---- 13. Parse comparators ---------------------------------------------------

comparator_detail <- contributing %>%
  dplyr::transmute(
    record_id,
    title,
    comparator_type_raw = extract_composite_text(
      comparator,
      "comparator_type",
      c("description")
    ),
    comparator_description = extract_composite_text(comparator, "description"),
    comparator_raw = comparator,
    comparator_quotes,
    comparator_loc,
    comparator_agree
  ) %>%
  dplyr::mutate(
    comparator_text = stringr::str_c(
      dplyr::coalesce(comparator_type_raw, ""),
      dplyr::coalesce(comparator_description, ""),
      dplyr::coalesce(comparator_raw, ""),
      sep = " | "
    ),
    comparator_group = dplyr::case_when(
      !has_text(comparator_raw) ~ "Not reported",
      stringr::str_detect(comparator_text, stringr::regex("multiple comparator", TRUE)) ~
        "Multiple/varied comparators",
      stringr::str_detect(comparator_text, stringr::regex("waitlist|waiting list", TRUE)) ~
        "Waitlist",
      stringr::str_detect(comparator_text, stringr::regex("static content|psychoeducation|information website", TRUE)) ~
        "Static content/information",
      stringr::str_detect(comparator_text, stringr::regex("usual care|treatment as usual|standard care", TRUE)) ~
        "Usual care",
      stringr::str_detect(comparator_text, stringr::regex("active digital|another chatbot|other chatbot|mobile app|\\bgpt", TRUE)) ~
        "Active digital",
      stringr::str_detect(comparator_text, stringr::regex("no control|uncontrolled|no comparator", TRUE)) ~
        "No control/comparator",
      TRUE ~ "Other/unclear"
    )
  )

comparator_counts <- comparator_detail %>%
  dplyr::count(comparator_group, sort = TRUE, name = "n") %>%
  dplyr::mutate(pct_contributing = safe_pct(n, contributing_n))

# ---- 14. Parse 1b.11 design–outcome links -----------------------------------

split_feature_blocks <- function(x) {
  x <- dplyr::coalesce(as.character(x), "")
  if (!stringr::str_detect(x, stringr::regex("\\bfeature\\s*:", TRUE))) {
    return(character(0))
  }

  blocks <- stringr::str_extract_all(
    x,
    stringr::regex(
      "feature\\s*:.*?(?=(?:\\n\\s*feature\\s*:)|$)",
      ignore_case = TRUE,
      dotall = TRUE
    )
  )[[1]]

  stringr::str_trim(blocks)
}

extract_link_field <- function(block, label, next_labels = NULL) {
  block <- dplyr::coalesce(as.character(block), "")

  if (is.null(next_labels) || length(next_labels) == 0) {
    pattern <- paste0(
      "(?is)(?:^|;\\s*)", label, "\\s*:\\s*(.*)$"
    )
  } else {
    pattern <- paste0(
      "(?is)(?:^|;\\s*)", label, "\\s*:\\s*(.*?)",
      "(?=;\\s*(?:", paste(next_labels, collapse = "|"), ")\\s*:|$)"
    )
  }

  blank_to_na(stringr::str_match(block, stringr::regex(pattern))[, 2])
}

design_outcome_links <- contributing %>%
  dplyr::filter(has_text(design_outcome_link)) %>%
  dplyr::select(
    record_id, title, study_design, design_group, country, setting,
    age_as_reported, architecture_class,
    adolescent_or_young_direct, genai_direct, south_africa_direct,
    mhaa_all_three_direct,
    design_outcome_link, design_outcome_link_quotes,
    design_outcome_link_loc, design_outcome_link_agree
  ) %>%
  dplyr::mutate(block = purrr::map(design_outcome_link, split_feature_blocks)) %>%
  tidyr::unnest_longer(block, values_to = "link_block") %>%
  dplyr::mutate(
    feature = extract_link_field(
      link_block,
      "feature",
      c("comparator", "outcome", "direction", "evidence_type", "linked_1a_record", "context")
    ),
    comparator_link = extract_link_field(
      link_block,
      "comparator",
      c("outcome", "direction", "evidence_type", "linked_1a_record", "context")
    ),
    outcome = extract_link_field(
      link_block,
      "outcome",
      c("direction", "evidence_type", "linked_1a_record", "context")
    ),
    direction = extract_link_field(
      link_block,
      "direction",
      c("evidence_type", "linked_1a_record", "context")
    ),
    evidence_type_raw = extract_link_field(
      link_block,
      "evidence_type",
      c("linked_1a_record", "context")
    ),
    linked_1a_record = extract_link_field(
      link_block,
      "linked_1a_record",
      c("context")
    ),
    contextual_conditions = extract_link_field(link_block, "context"),
    is_experimental = stringr::str_detect(
      dplyr::coalesce(evidence_type_raw, ""),
      stringr::regex("experimental|randomi[sz]ed|\\brct\\b|a/b", TRUE)
    ),
    is_observational = stringr::str_detect(
      dplyr::coalesce(evidence_type_raw, ""),
      stringr::regex("observational|association|regression|correlation|subgroup|meta-regression", TRUE)
    ),
    is_qualitative = stringr::str_detect(
      dplyr::coalesce(evidence_type_raw, ""),
      stringr::regex("qualitative|user review|participant report|interview|thematic", TRUE)
    ),
    is_author_interpretation = stringr::str_detect(
      dplyr::coalesce(evidence_type_raw, ""),
      stringr::regex("author interpretation|author synthesis|interpretation", TRUE)
    ),
    evidence_class = dplyr::case_when(
      is_experimental ~ "Experimental comparison",
      is_observational ~ "Observational association",
      is_qualitative ~ "Qualitative account",
      is_author_interpretation ~ "Author interpretation",
      TRUE ~ "Other/unclear"
    ),
    evidence_tier = dplyr::case_when(
      is_experimental ~ 1L,
      is_observational ~ 2L,
      is_qualitative ~ 3L,
      is_author_interpretation ~ 4L,
      TRUE ~ 5L
    ),
    direction_group = dplyr::case_when(
      stringr::str_detect(dplyr::coalesce(direction, ""), stringr::regex("mixed|inconsistent|varied", TRUE)) ~
        "Mixed/inconsistent",
      stringr::str_detect(dplyr::coalesce(direction, ""), stringr::regex("no association|not significant|null|no difference", TRUE)) ~
        "Null/no clear difference",
      stringr::str_detect(dplyr::coalesce(direction, ""), stringr::regex("negative|worse|lower|reduced|harm|burden", TRUE)) ~
        "Negative/unfavourable",
      stringr::str_detect(dplyr::coalesce(direction, ""), stringr::regex("positive|better|higher|improved|increase|favou?r", TRUE)) ~
        "Positive/favourable",
      TRUE ~ "Unclear/not standardised"
    )
  ) %>%
  dplyr::mutate(link_id = dplyr::row_number()) %>%
  dplyr::select(
    link_id, record_id, title, study_design, design_group, country, setting,
    age_as_reported, architecture_class,
    adolescent_or_young_direct, genai_direct, south_africa_direct,
    mhaa_all_three_direct,
    feature, comparator_link, outcome, direction, direction_group,
    evidence_type_raw, evidence_class, evidence_tier,
    is_experimental, is_observational, is_qualitative,
    is_author_interpretation, linked_1a_record, contextual_conditions,
    design_outcome_link_quotes, design_outcome_link_loc,
    design_outcome_link_agree, link_block
  )

design_outcome_evidence_summary <- design_outcome_links %>%
  dplyr::count(evidence_class, direction_group, sort = TRUE, name = "n_links")

experimental_links <- design_outcome_links %>%
  dplyr::filter(is_experimental) %>%
  dplyr::arrange(record_id, link_id)

# Theme the free-text feature descriptions without replacing the raw wording.
feature_theme_dictionary <- tibble::tribble(
  ~feature_theme, ~pattern,
  "Architecture, grounding or prompting", "rag|retrieval|ground|prompt|fine[- ]tun|temperature|model|llm|generative|rule[- ]based|architecture",
  "Empathy, tone or relational style", "empath|nonjudg|non-judg|tone|relational|supportive|warmth|human-like",
  "Personalisation or adaptation", "personali[sz]|tailor|adapt|memory|context-aware|preference|pacing|name",
  "Human support or escalation", "human support|clinician|therapist|coach|peer|human-in-the-loop|escalat|handoff|professional oversight",
  "Session dose, sequence or complexity", "session|dose|duration|sequence|protocol complexity|asynchronous|synchronous|single-session|multi-session",
  "Engagement, reminders or gamification", "engagement|reminder|notification|prompt|gamif|progress|reward|goal|logging|tracking",
  "Interface, interaction or usability", "interface|free-text|menu|input|usability|navigation|response length|repetit|technical|interaction",
  "Safety, crisis response or guardrails", "safety|crisis|suicid|guardrail|disclaimer|harm|emergency|risk",
  "Therapeutic content/component", "cbt|behavio(?:u)?ral activation|cognitive restructur|psychoeducation|mindfulness|act|therapy|therapeutic",
  "Co-design or formative development", "co-design|codesign|participatory|user-centred|user-centered|formative|prototype|usability test",
  "Anonymity, stigma or privacy", "anonym|stigma|label avoidance|privacy|confidential",
  "Persona, embodiment or visual design", "persona|avatar|character|embod|visual|robot"
)

feature_outcome_themes <- tidyr::crossing(
  design_outcome_links %>%
    dplyr::select(
      link_id, record_id, title, feature, outcome, direction_group,
      evidence_class, evidence_tier
    ),
  feature_theme_dictionary
) %>%
  dplyr::filter(
    stringr::str_detect(
      dplyr::coalesce(feature, ""),
      stringr::regex(pattern, ignore_case = TRUE)
    )
  ) %>%
  dplyr::select(-pattern) %>%
  dplyr::distinct()

feature_outcome_theme_summary <- feature_outcome_themes %>%
  dplyr::count(
    feature_theme, evidence_class, direction_group,
    sort = TRUE, name = "n_links"
  )

# ---- 15. Overview metrics for the Evidence base paragraph -------------------

n_experimental_records <- design_outcome_links %>%
  dplyr::filter(is_experimental) %>%
  dplyr::distinct(record_id) %>%
  nrow()

n_observational_records <- design_outcome_links %>%
  dplyr::filter(is_observational) %>%
  dplyr::distinct(record_id) %>%
  nrow()

n_qualitative_records <- design_outcome_links %>%
  dplyr::filter(is_qualitative) %>%
  dplyr::distinct(record_id) %>%
  nrow()

n_author_interpretation_records <- design_outcome_links %>%
  dplyr::filter(is_author_interpretation) %>%
  dplyr::distinct(record_id) %>%
  nrow()

overview_metrics <- dplyr::bind_rows(
  tibble::tibble(
    metric = "All records represented in the 1b sheet",
    n = stream_n,
    denominator = stream_n,
    denominator_label = "1b sheet records"
  ),
  tibble::tibble(
    metric = "Records contributing at least one RQ1b field",
    n = contributing_n,
    denominator = stream_n,
    denominator_label = "1b sheet records"
  ),
  tibble::tibble(
    metric = "Primary quantitative records",
    n = sum(contributing$design_group == "Primary quantitative"),
    denominator = contributing_n,
    denominator_label = "RQ1b contributing records"
  ),
  tibble::tibble(
    metric = "Qualitative or mixed-methods records",
    n = sum(contributing$design_group == "Qualitative or mixed-methods"),
    denominator = contributing_n,
    denominator_label = "RQ1b contributing records"
  ),
  tibble::tibble(
    metric = "Evidence-synthesis records",
    n = sum(contributing$design_group == "Evidence synthesis"),
    denominator = contributing_n,
    denominator_label = "RQ1b contributing records"
  ),
  tibble::tibble(
    metric = "Adolescent/young-person direct",
    n = sum(contributing$adolescent_or_young_direct),
    denominator = contributing_n,
    denominator_label = "RQ1b contributing records"
  ),
  tibble::tibble(
    metric = "Generative-AI/LLM direct",
    n = sum(contributing$genai_direct),
    denominator = contributing_n,
    denominator_label = "RQ1b contributing records"
  ),
  tibble::tibble(
    metric = "South Africa direct",
    n = sum(contributing$south_africa_direct),
    denominator = contributing_n,
    denominator_label = "RQ1b contributing records"
  ),
  tibble::tibble(
    metric = "Direct on population + GenAI + South Africa",
    n = sum(contributing$mhaa_all_three_direct),
    denominator = contributing_n,
    denominator_label = "RQ1b contributing records"
  ),
  tibble::tibble(
    metric = "Explicit single-session intervention/design",
    n = sum(contributing$explicit_single_session),
    denominator = contributing_n,
    denominator_label = "RQ1b contributing records"
  ),
  tibble::tibble(
    metric = "Reported duration overlapping 15–30 minutes",
    n = sum(contributing$overlaps_mhaa_15_30_min, na.rm = TRUE),
    denominator = contributing_n,
    denominator_label = "RQ1b contributing records"
  ),
  tibble::tibble(
    metric = "Records with experimental feature–outcome evidence",
    n = n_experimental_records,
    denominator = contributing_n,
    denominator_label = "RQ1b contributing records"
  ),
  tibble::tibble(
    metric = "Records with observational feature–outcome evidence",
    n = n_observational_records,
    denominator = contributing_n,
    denominator_label = "RQ1b contributing records"
  ),
  tibble::tibble(
    metric = "Records with qualitative feature–outcome evidence",
    n = n_qualitative_records,
    denominator = contributing_n,
    denominator_label = "RQ1b contributing records"
  ),
  tibble::tibble(
    metric = "Records with author-interpreted feature–outcome claims",
    n = n_author_interpretation_records,
    denominator = contributing_n,
    denominator_label = "RQ1b contributing records"
  )
) %>%
  dplyr::mutate(percent = safe_pct(n, denominator))

# ---- 16. Review queue and verification sample -------------------------------

review_queue_1b <- review_queue_raw %>%
  dplyr::filter(group == "1b") %>%
  dplyr::arrange(record_id, field_id)

conflict_counts <- rq1b_long %>%
  dplyr::group_by(record_id) %>%
  dplyr::summarise(
    n_conflict_fields = sum(
      stringr::str_to_lower(dplyr::coalesce(agree, "")) == "conflict"
    ),
    .groups = "drop"
  )

verification_ids <- contributing %>%
  dplyr::left_join(conflict_counts, by = "record_id") %>%
  dplyr::mutate(
    has_experimental_link = record_id %in% experimental_links$record_id,
    has_best = has_text(best_elements),
    n_conflict_fields = dplyr::coalesce(n_conflict_fields, 0L)
  ) %>%
  dplyr::arrange(
    dplyr::desc(has_experimental_link),
    dplyr::desc(has_best),
    dplyr::desc(mhaa_all_three_direct),
    dplyr::desc(n_conflict_fields),
    record_id
  ) %>%
  dplyr::slice_head(n = 5) %>%
  dplyr::pull(record_id)

verification_sample <- rq1b_long %>%
  dplyr::filter(record_id %in% verification_ids) %>%
  dplyr::filter(has_text(value) | has_text(quotes)) %>%
  dplyr::arrange(match(record_id, verification_ids), field_number(field_id)) %>%
  dplyr::select(
    record_id, title, field_id, label, value, quotes, loc, agree,
    design_group, country, age_as_reported, architecture_class,
    adolescent_or_young_direct, genai_direct,
    south_africa_direct, mhaa_all_three_direct
  )

# ---- 17. Record-level analysis export ---------------------------------------

record_level_export <- contributing %>%
  dplyr::left_join(
    session_detail %>%
      dplyr::select(
        record_id, session_class, sessions_min, sessions_max,
        session_minutes_min, session_minutes_max,
        overlaps_mhaa_15_30_min
      ),
    by = "record_id",
    suffix = c("", "_parsed")
  ) %>%
  dplyr::mutate(
    ssi_framing = extract_composite_bool(therapeutic_approach, "ssi_framing")
  ) %>%
  dplyr::select(
    record_id, title, bibliographic, country, setting, sample_size, sample_n,
    age_band, age_as_reported, age_10_19, age_10_24, age_18_24,
    study_design, design_group, object_type, evidence_type_tag, minimal_quality,
    lmic, ssa, south_africa,
    adolescent_or_young_direct, genai_direct, south_africa_direct,
    mhaa_all_three_direct, architecture_class,
    explicit_single_session, ssi_framing,
    session_class, sessions_min, sessions_max,
    session_minutes_min, session_minutes_max, overlaps_mhaa_15_30_min,
    dplyr::all_of(rq1b_value_cols)
  ) %>%
  dplyr::arrange(record_id)

# ---- 18. Visuals -------------------------------------------------------------

# 18a. Therapeutic approach frequencies
if (nrow(approach_tag_counts) > 0) {
  
  approach_plot_data <- approach_tag_counts %>%
    dplyr::slice_max(
      order_by = n_records,
      n = 15,
      with_ties = FALSE
    ) %>%
    dplyr::mutate(
      # Change displayed labels to US English without altering the source data
      tag = dplyr::recode(
        tag,
        "Cognitive behavioural therapy (CBT)" =
          "Cognitive behavioral therapy (CBT)",
        "Behavioural activation" =
          "Behavioral activation",
        "Dialectical behaviour therapy (DBT)" =
          "Dialectical behavior therapy (DBT)",
        "Supportive counselling/listening" =
          "Supportive counseling/listening",
        .default = tag
      )
    ) %>%
    dplyr::arrange(n_records) %>%
    dplyr::mutate(
      tag = factor(tag, levels = tag)
    )
  
  p_approach <- ggplot2::ggplot(
    approach_plot_data,
    ggplot2::aes(
      x = n_records,
      y = tag,
      fill = n_records
    )
  ) +
    ggplot2::geom_col(
      width = 0.72
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = n_records),
      hjust = -0.18,
      size = 5,
      color = "#1F1F1F"
    ) +
    ggplot2::scale_fill_gradient(
      low = "#DCEAF5",
      high = "#2F75B5",
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.10))
    ) +
    ggplot2::labs(
      title = "Therapeutic approaches reported in RQ1b records",
      subtitle = paste0(
        "Non-mutually exclusive record-level tags; ",
        "frequency is not evidence of effectiveness"
      ),
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      # Remove all grid lines
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      
      # Remove the x-axis
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      
      # Improve category labels
      axis.text.y = ggplot2::element_text(
        size = 13,
        color = "#404040",
        margin = ggplot2::margin(r = 10)
      ),
      axis.ticks.y = ggplot2::element_blank(),
      
      # Improve title and subtitle
      plot.title = ggplot2::element_text(
        size = 19,
        face = "bold",
        color = "#1F1F1F",
        margin = ggplot2::margin(b = 7)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 13,
        color = "#505050",
        margin = ggplot2::margin(b = 18)
      ),
      
      # Add clean spacing around the plot
      plot.margin = ggplot2::margin(
        t = 18,
        r = 35,
        b = 18,
        l = 18
      )
    )
  
  ggplot2::ggsave(
    filename = file.path(
      output_dir,
      "RQ1b_therapeutic_approaches.png"
    ),
    plot = p_approach,
    width = 12,
    height = 7.5,
    dpi = 300,
    bg = "white"
  )
}
# 18b. Architecture × therapeutic approach heatmap
architecture_approach_heatmap <- approach_tags %>%
  dplyr::left_join(
    contributing %>% dplyr::select(record_id, architecture_class),
    by = "record_id"
  ) %>%
  dplyr::count(architecture_class, tag, name = "n_records")

if (nrow(architecture_approach_heatmap) > 0) {
  top_approaches <- approach_tag_counts %>%
    dplyr::slice_max(order_by = n_records, n = 12, with_ties = FALSE) %>%
    dplyr::pull(tag)

  heatmap_plot_data <- architecture_approach_heatmap %>%
    dplyr::filter(tag %in% top_approaches) %>%
    dplyr::mutate(
      tag = factor(tag, levels = rev(top_approaches))
    )

  p_heatmap <- ggplot2::ggplot(
    heatmap_plot_data,
    ggplot2::aes(x = architecture_class, y = tag, fill = n_records)
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = n_records), size = 3.2) +
    ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
    ggplot2::labs(
      title = "Therapeutic approach × technology architecture",
      subtitle = "Cell values are record counts; records may mention more than one approach",
      x = NULL,
      y = NULL,
      fill = "Records"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )

  ggplot2::ggsave(
    filename = file.path(output_dir, "RQ1b_architecture_approach_heatmap.png"),
    plot = p_heatmap,
    width = 11,
    height = 7.5,
    dpi = 300
  )
}

# 18c. Session-structure distribution
if (nrow(session_structure_counts) > 0) {
  
  session_plot_data <- session_structure_counts %>%
    dplyr::arrange(n) %>%
    dplyr::mutate(
      session_class = factor(
        session_class,
        levels = session_class
      )
    )
  
  p_session <- ggplot2::ggplot(
    session_plot_data,
    ggplot2::aes(
      x = n,
      y = session_class,
      fill = n
    )
  ) +
    ggplot2::geom_col(
      width = 0.72
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = n),
      hjust = -0.18,
      size = 5,
      color = "#1F1F1F"
    ) +
    ggplot2::scale_fill_gradient(
      low = "#DCEAF5",
      high = "#2F75B5",
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.10))
    ) +
    ggplot2::labs(
      title = "Session structure reported in RQ1b records",
      subtitle = paste0(
        "Classification was derived conservatively from field 1b.4 ",
        "and should be checked against the raw text"
      ),
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      # Remove all grid lines
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      
      # Remove the x-axis
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      
      # Improve category labels
      axis.text.y = ggplot2::element_text(
        size = 14,
        color = "#404040",
        margin = ggplot2::margin(r = 12)
      ),
      axis.ticks.y = ggplot2::element_blank(),
      
      # Improve title and subtitle
      plot.title = ggplot2::element_text(
        size = 19,
        face = "bold",
        color = "#1F1F1F",
        margin = ggplot2::margin(b = 7)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 13,
        color = "#505050",
        margin = ggplot2::margin(b = 18)
      ),
      
      # Add clean spacing around the plot
      plot.margin = ggplot2::margin(
        t = 18,
        r = 35,
        b = 18,
        l = 18
      )
    )
  
  ggplot2::ggsave(
    filename = file.path(
      output_dir,
      "RQ1b_session_structure.png"
    ),
    plot = p_session,
    width = 11,
    height = 6.5,
    dpi = 300,
    bg = "white"
  )
}
# ---- 19. Narrative scaffold --------------------------------------------------

top_items <- function(data, label_col, n_col, k = 5) {
  if (nrow(data) == 0) return("None identified")
  data <- data %>% dplyr::slice_head(n = min(k, nrow(data)))
  paste0(
    data[[label_col]], " (n=", data[[n_col]], ")",
    collapse = "; "
  )
}

get_metric <- function(metric_name) {
  row <- overview_metrics %>% dplyr::filter(metric == metric_name)
  if (nrow(row) == 0) return("not calculated")
  format_n_pct(row$n[[1]], row$denominator[[1]])
}

n_links_total <- nrow(design_outcome_links)
n_links_experimental <- sum(design_outcome_links$is_experimental)
n_links_observational <- sum(design_outcome_links$is_observational)
n_links_qualitative <- sum(design_outcome_links$is_qualitative)
n_links_author <- sum(design_outcome_links$is_author_interpretation)

narrative_lines <- c(
  "# RQ 1b — Intervention design: analysis scaffold",
  "",
  "## Interpretation rule",
  "Counts below describe records and reporting patterns, not independent studies and not effectiveness. Linked reports may represent the same underlying study. Any claim that a design feature is associated with outcomes should be traced to `design_outcome_links`, prioritising experimental comparisons and distinguishing them from observational, qualitative and author-interpreted evidence.",
  "",
  "## Direct preliminary answer — evidence to inspect",
  paste0("- Most frequently tagged architecture profiles: ", top_items(architecture_counts, "architecture_class", "n", 5), "."),
  paste0("- Most frequently tagged therapeutic approaches: ", top_items(approach_tag_counts, "tag", "n_records", 7), "."),
  paste0(
    "- Parsed design–outcome links: ", n_links_total,
    " total; ", n_links_experimental, " experimentally classified; ",
    n_links_observational, " observationally classified; ",
    n_links_qualitative, " qualitatively classified; and ",
    n_links_author, " containing author interpretation. Categories can overlap in the raw extraction."
  ),
  "- Draft the preliminary answer from the strongest recurring findings in `experimental_links` and `feature_outcome_theme_summary`; do not infer effectiveness from the frequency tables.",
  "",
  "## Evidence base",
  paste0("- RQ1b contribution: ", get_metric("Records contributing at least one RQ1b field"), "."),
  paste0("- Primary quantitative: ", get_metric("Primary quantitative records"), "."),
  paste0("- Qualitative or mixed-methods: ", get_metric("Qualitative or mixed-methods records"), "."),
  paste0("- Evidence syntheses: ", get_metric("Evidence-synthesis records"), "."),
  paste0("- Adolescent/young-person direct: ", get_metric("Adolescent/young-person direct"), "."),
  paste0("- Generative-AI/LLM direct: ", get_metric("Generative-AI/LLM direct"), "."),
  paste0("- South Africa direct: ", get_metric("South Africa direct"), "."),
  paste0("- Direct on all three operationalised MHAA dimensions: ", get_metric("Direct on population + GenAI + South Africa"), "."),
  paste0("- Most common study designs: ", top_items(study_design_counts, "study_design", "n", 8), "."),
  paste0("- Most common settings: ", top_items(setting_counts, "setting", "n", 6), "."),
  paste0("- Most represented countries/country labels: ", top_items(country_counts, "country", "n_records", 10), "."),
  "",
  "## Key findings — tables to use",
  "- Modalities and technology: `architecture_counts`, `modality_tag_counts`, and `architecture_approach_heatmap`.",
  "- Therapeutic approaches and active components: `approach_tag_counts` and the raw 1b.2 rows in `quote_bank`.",
  "- Session dose and structure: `session_structure_counts` and `session_detail`.",
  "- Personalisation, interaction, human support, engagement and co-design: `design_feature_counts` and `design_feature_tags`.",
  "- Feature–outcome evidence: `design_outcome_links`, `experimental_links`, and `feature_outcome_theme_summary`.",
  "- BEST elements: `best_detail` and `best_counts`; reporting is expected to be sparse, so state the denominator explicitly.",
  "",
  "## Implications for MHAA — decision points",
  "Use the strongest direct evidence to address: deterministic workflow versus model orchestration; a 15–30-minute single-session format; portability/adaptation of the Project YES corpus; candidate A/B variants; degree of human support; safeguarding and escalation. Label any recommendation that is based on pattern interpretation rather than comparative evidence as team interpretation.",
  "",
  "## Gaps and caveats",
  "- Counts are records, not deduplicated studies.",
  "- Keyword tags are transparent aids for synthesis and must be checked against the raw value and quote columns.",
  "- Systematic reviews can describe multiple intervention types; their tags should not be interpreted as one intervention architecture.",
  "- A positive or negative direction in 1b.11 does not by itself establish causality; use `evidence_class` and the original quote.",
  "- BEST-element reporting is sparse and should not be treated as absence unless the source was explicitly assessed.",
  "- The GenAI directness flag is keyword-based and should be manually verified for borderline systems.",
  "- South Africa directness uses the extracted baseline flag; multi-country and regional records require contextual checking.",
  "",
  "## Manual checks before drafting",
  "1. Resolve relevant rows in `review_queue_1b`.",
  "2. Spot-check the five records in `verification_sample` against source PDFs.",
  "3. Verify all BEST claims against their evidence spans.",
  "4. Review all experimental links and any design–outcome claim proposed for the report.",
  "5. State the linked-publication/record-overlap caveat."
)

readr::write_lines(
  narrative_lines,
  file.path(output_dir, "RQ1b_narrative_scaffold.md")
)

# ---- 20. Export tables -------------------------------------------------------

readr::write_csv(
  record_level_export,
  file.path(output_dir, "RQ1b_record_level.csv"),
  na = ""
)

readr::write_csv(
  design_outcome_links,
  file.path(output_dir, "RQ1b_design_outcome_links.csv"),
  na = ""
)

readr::write_csv(
  quote_bank,
  file.path(output_dir, "RQ1b_quote_bank.csv"),
  na = ""
)

readr::write_csv(
  field_coverage,
  file.path(output_dir, "RQ1b_field_coverage.csv"),
  na = ""
)

# Excel workbook: all principal outputs in one place.
output_workbook <- list(
  overview = overview_metrics,
  field_coverage = field_coverage,
  design_groups = design_group_counts,
  study_designs = study_design_counts,
  countries = country_counts,
  settings = setting_counts,
  age_mappings = age_mapping_counts,
  geography = geography_counts,
  architecture = architecture_counts,
  modality_tags = modality_tag_counts,
  approach_tags = approach_tag_counts,
  SSI_summary = ssi_summary,
  session_structure = session_structure_counts,
  session_detail = session_detail,
  design_feature_counts = design_feature_counts,
  design_feature_tags = design_feature_tags,
  comparator_counts = comparator_counts,
  comparator_detail = comparator_detail,
  design_outcome_summary = design_outcome_evidence_summary,
  design_outcome_links = design_outcome_links,
  experimental_links = experimental_links,
  feature_outcome_themes = feature_outcome_theme_summary,
  BEST_counts = best_counts,
  BEST_detail = best_detail,
  review_queue_1b = review_queue_1b,
  verification_sample = verification_sample,
  quote_bank = quote_bank,
  record_level = record_level_export,
  codebook_1b = codebook_1b
)

writexl::write_xlsx(
  output_workbook,
  path = file.path(output_dir, "RQ1b_analysis_outputs.xlsx")
)

# ---- 21. Completion message -------------------------------------------------

message("RQ1b analysis complete.")
message("Input records in 1b sheet: ", stream_n)
message("Records contributing >=1 RQ1b field: ", contributing_n)
message("Outputs written to: ", normalizePath(output_dir, winslash = "/"))
message("Open RQ1b_analysis_outputs.xlsx and RQ1b_narrative_scaffold.md first.")
