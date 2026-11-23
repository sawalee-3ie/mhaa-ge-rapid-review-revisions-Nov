# -----------------------------------------------------------------------------
# Section 3.2: Characteristics of the included evidence
# Dataset-specific analysis for:
#   extraction_final-revised-SL_n=325.xlsx
#
# Purpose
#   1. Create an auditable record-level characteristics dataset.
#   2. Produce the summaries needed for Section 3.2 and Table 2.
#   3. Draft two narrative paragraphs using the calculated counts.
#   4. Export audit files for classifications that require human verification.
#
# Important interpretation
#   - Counts are record-level unless explicitly labelled primary empirical.
#   - South Africa includes primary records conducted in South Africa and
#     review-level records flagged as covering South African evidence.
#   - GenAI, delivery channel, intervention category and sponsor type are
#     keyword-derived from extracted text and must be manually verified.
#   - Directness is reported separately for ages 10-19 and ages 10-24.
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# 0. Packages, file locations and constants
# -----------------------------------------------------------------------------

required_packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "purrr",
  "readr",
  "glue",
  "tibble"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(glue)
library(tibble)

input_file <- "extraction_final-revised-SL_n=325.xlsx"
output_dir <- "section_3_2_outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Optional manually reviewed overrides. The script creates a blank template on
# the first run. Edit the template and rerun the script to replace derived flags.
override_file <- file.path(
  output_dir,
  "section_3_2_manual_overrides.csv"
)

gated_marker <- "— gated out —"

region_order <- c(
  "South Africa",
  "Other SSA",
  "Other L&MIC",
  "HIC / global / not positively classified"
)

evidence_group_order <- c(
  "Primary empirical",
  "Review-level",
  "Methods/framework/regulatory",
  "Other non-empirical/unclear"
)

population_order <- c(
  "Adolescent/youth-focused (includes ages 10-19)",
  "Mixed-age evidence including adolescents or young people",
  "Includes ages 18-24 but not ages 10-19",
  "Adult or other population",
  "Review/methodological record with age not applicable or not reported",
  "Not reported/unclear"
)


# -----------------------------------------------------------------------------
# 1. Helper functions
# -----------------------------------------------------------------------------

as_bool <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))

  case_when(
    x %in% c("true", "t", "yes", "y", "1") ~ TRUE,
    x %in% c("false", "f", "no", "n", "0") ~ FALSE,
    TRUE ~ NA
  )
}

is_present_text <- function(x) {
  x <- str_squish(as.character(x))
  !is.na(x) & x != "" & x != gated_marker
}

first_non_missing_character <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & str_squish(x) != ""]

  if (length(x) == 0) {
    NA_character_
  } else {
    x[[1]]
  }
}

first_non_missing_integer <- function(x) {
  x <- suppressWarnings(as.integer(x))
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    NA_integer_
  } else {
    x[[1]]
  }
}

format_percent <- function(n, denominator, digits = 1) {
  if (is.na(denominator) || denominator == 0) {
    return("NA")
  }

  paste0(
    formatC(100 * n / denominator, format = "f", digits = digits),
    "%"
  )
}

format_n_percent <- function(n, denominator, digits = 1) {
  glue("{n} ({format_percent(n, denominator, digits)})")
}

format_count_rows <- function(count_data, denominator, max_items = Inf) {
  ordered_data <- count_data |>
    arrange(desc(n), category)

  if (is.finite(max_items)) {
    ordered_data <- ordered_data |>
      slice_head(n = max_items)
  }

  ordered_data |>
    mutate(
      item = glue(
        "{category}: {n} ({format_percent(n, denominator)})"
      )
    ) |>
    pull(item) |>
    paste(collapse = "; ")
}

parse_age_mapping <- function(x, mapping_name) {
  str_detect(
    replace_na(as.character(x), ""),
    regex(
      paste0(mapping_name, "\\s*:\\s*(true|yes|1)"),
      ignore_case = TRUE
    )
  )
}

extract_age_as_reported <- function(x) {
  x <- replace_na(as.character(x), "")

  extracted <- str_match(
    x,
    regex(
      "as_reported\\s*:\\s*(.*?)(?=\\s*\\|\\s*maps_10_19|$)",
      ignore_case = TRUE
    )
  )[, 2]

  extracted[extracted == ""] <- NA_character_
  extracted
}

assert_fields <- function(data, required_fields, sheet_name) {
  missing_fields <- setdiff(required_fields, names(data))

  if (length(missing_fields) > 0) {
    stop(
      "The following required fields are missing from sheet '",
      sheet_name,
      "': ",
      paste(missing_fields, collapse = ", ")
    )
  }
}


# -----------------------------------------------------------------------------
# 2. Read and validate the workbook
# -----------------------------------------------------------------------------

if (!file.exists(input_file)) {
  stop(
    "Input workbook not found: ",
    input_file,
    ". Update input_file at the top of the script if needed."
  )
}

workbook_sheets <- excel_sheets(input_file)
required_sheets <- c("Index", "baseline", "1b", "pl", "Codebook")
missing_sheets <- setdiff(required_sheets, workbook_sheets)

if (length(missing_sheets) > 0) {
  stop(
    "The workbook is missing the following required sheets: ",
    paste(missing_sheets, collapse = ", ")
  )
}

index_raw <- read_excel(
  input_file,
  sheet = "Index",
  .name_repair = "minimal"
)

baseline_raw <- read_excel(
  input_file,
  sheet = "baseline",
  .name_repair = "minimal"
)

one_b_raw <- read_excel(
  input_file,
  sheet = "1b",
  .name_repair = "minimal"
)

product_landscape_raw <- read_excel(
  input_file,
  sheet = "pl",
  .name_repair = "minimal"
)

codebook_raw <- read_excel(
  input_file,
  sheet = "Codebook",
  .name_repair = "minimal"
)

assert_fields(
  index_raw,
  c("record_id", "title", "year"),
  "Index"
)

assert_fields(
  baseline_raw,
  c(
    "record_id",
    "title",
    "base.1 value",
    "base.2 value",
    "base.3 value",
    "base.5 value",
    "base.6 value",
    "base.7 value",
    "base.10 value",
    "base.11a value",
    "base.11b value",
    "base.12 value",
    "base.13 value"
  ),
  "baseline"
)

assert_fields(
  one_b_raw,
  c(
    "record_id",
    "1b.1 value",
    "1b.2 value",
    "1b.4 value",
    "1b.5 value"
  ),
  "1b"
)

assert_fields(
  product_landscape_raw,
  c("record_id", "PL.2 value"),
  "pl"
)


# -----------------------------------------------------------------------------
# 3. Create a one-row-per-record source dataset
# -----------------------------------------------------------------------------

index_data <- index_raw |>
  transmute(
    record_id = str_squish(as.character(record_id)),
    title_index = as.character(title),
    publication_year = suppressWarnings(as.integer(year))
  ) |>
  filter(!is.na(record_id), record_id != "") |>
  group_by(record_id) |>
  summarise(
    title_index = first_non_missing_character(title_index),
    publication_year = first_non_missing_integer(publication_year),
    .groups = "drop"
  )

baseline_data <- baseline_raw |>
  transmute(
    record_id = str_squish(as.character(record_id)),
    title = as.character(title),
    bibliographic_raw = as.character(`base.1 value`),
    country_raw = as.character(`base.2 value`),
    setting_raw = as.character(`base.3 value`),
    population_age_raw = as.character(`base.5 value`),
    study_design_raw = as.character(`base.6 value`),
    intervention_object_raw = as.character(`base.7 value`),
    lmic_flag_raw = `base.10 value`,
    ssa_flag_raw = `base.11a value`,
    south_africa_flag_raw = `base.11b value`,
    evidence_type_raw = as.character(`base.12 value`),
    funding_coi_raw = as.character(`base.13 value`),
    age_agreement = if (
      "base.5 agree" %in% names(baseline_raw)
    ) as.character(`base.5 agree`) else NA_character_,
    design_agreement = if (
      "base.6 agree" %in% names(baseline_raw)
    ) as.character(`base.6 agree`) else NA_character_,
    intervention_agreement = if (
      "base.7 agree" %in% names(baseline_raw)
    ) as.character(`base.7 agree`) else NA_character_,
    geography_lmic_agreement = if (
      "base.10 agree" %in% names(baseline_raw)
    ) as.character(`base.10 agree`) else NA_character_,
    geography_ssa_agreement = if (
      "base.11a agree" %in% names(baseline_raw)
    ) as.character(`base.11a agree`) else NA_character_,
    geography_sa_agreement = if (
      "base.11b agree" %in% names(baseline_raw)
    ) as.character(`base.11b agree`) else NA_character_,
    funding_agreement = if (
      "base.13 agree" %in% names(baseline_raw)
    ) as.character(`base.13 agree`) else NA_character_
  ) |>
  filter(!is.na(record_id), record_id != "") |>
  distinct(record_id, .keep_all = TRUE)

one_b_data <- one_b_raw |>
  transmute(
    record_id = str_squish(as.character(record_id)),
    modality_delivery_raw = as.character(`1b.1 value`),
    therapeutic_approach_raw = as.character(`1b.2 value`),
    dose_structure_raw = as.character(`1b.4 value`),
    architecture_raw = as.character(`1b.5 value`),
    modality_agreement = if (
      "1b.1 agree" %in% names(one_b_raw)
    ) as.character(`1b.1 agree`) else NA_character_,
    dose_agreement = if (
      "1b.4 agree" %in% names(one_b_raw)
    ) as.character(`1b.4 agree`) else NA_character_,
    architecture_agreement = if (
      "1b.5 agree" %in% names(one_b_raw)
    ) as.character(`1b.5 agree`) else NA_character_
  ) |>
  filter(!is.na(record_id), record_id != "") |>
  distinct(record_id, .keep_all = TRUE)

product_landscape_data <- product_landscape_raw |>
  transmute(
    record_id = str_squish(as.character(record_id)),
    entry_channel_raw = as.character(`PL.2 value`),
    entry_channel_agreement = if (
      "PL.2 agree" %in% names(product_landscape_raw)
    ) as.character(`PL.2 agree`) else NA_character_
  ) |>
  filter(!is.na(record_id), record_id != "") |>
  distinct(record_id, .keep_all = TRUE)

source_data <- baseline_data |>
  left_join(index_data, by = "record_id") |>
  left_join(one_b_data, by = "record_id") |>
  left_join(product_landscape_data, by = "record_id") |>
  mutate(
    title = coalesce(title, title_index)
  )

n_analytical_records <- n_distinct(source_data$record_id)
message("Analytical records: ", n_analytical_records)


# -----------------------------------------------------------------------------
# 4. Evidence type: primary empirical, review-level and non-empirical records
# -----------------------------------------------------------------------------

review_designs <- c("sr", "sr+ma", "scoping", "umbrella")
methods_designs <- c("framework", "regulatory")
primary_designs <- c(
  "rct",
  "non-rct trial",
  "cohort",
  "cross-sectional",
  "qualitative",
  "mixed-methods"
)

record_level_derived <- source_data |>
  mutate(
    study_design_norm = str_to_lower(str_squish(study_design_raw)),
    evidence_classification_text = str_c(
      replace_na(title, ""),
      replace_na(evidence_type_raw, ""),
      sep = " || "
    ),
    review_text_marker = str_detect(
      evidence_classification_text,
      regex(
        paste(
          "(systematic|scoping|umbrella|narrative|literature|comprehensive|rapid)\\s+review",
          "meta[- ]analysis",
          "evidence synthesis",
          "review of reviews",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    methods_text_marker = str_detect(
      evidence_classification_text,
      regex(
        paste(
          "framework",
          "guidance",
          "guideline",
          "regulatory",
          "policy paper",
          "methodolog",
          "methods paper",
          "reporting standard",
          "consensus statement",
          "research agenda",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    empirical_text_marker = str_detect(
      evidence_classification_text,
      regex(
        paste(
          "study",
          "trial",
          "evaluation",
          "validation",
          "audit",
          "analysis",
          "case series",
          "feasibility",
          "acceptability",
          "usability",
          "observational",
          "experimental",
          "co-design",
          "pilot",
          "deployment",
          "survey",
          "interview",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    evidence_group_derived = case_when(
      study_design_norm %in% review_designs |
        review_text_marker ~ "Review-level",
      study_design_norm %in% methods_designs |
        methods_text_marker ~ "Methods/framework/regulatory",
      study_design_norm %in% primary_designs ~ "Primary empirical",
      study_design_norm == "other" & empirical_text_marker ~
        "Primary empirical",
      TRUE ~ "Other non-empirical/unclear"
    )
  )


# -----------------------------------------------------------------------------
# 5. Geography
# -----------------------------------------------------------------------------

# ISO-3 codes for Sub-Saharan African countries. The extracted SSA and South
# Africa flags remain the primary source; these codes provide a safeguard where
# a positive country is explicit in base.2 but a flag is missing.
ssa_iso3 <- c(
  "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD",
  "COM", "COD", "COG", "CIV", "GNQ", "ERI", "SWZ", "ETH", "GAB",
  "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "MDG", "MWI",
  "MLI", "MRT", "MUS", "MOZ", "NAM", "NER", "NGA", "RWA", "STP",
  "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA", "TGO",
  "UGA", "ZMB", "ZWE"
)

ssa_iso3_pattern <- paste0(
  "\\b(",
  paste(ssa_iso3, collapse = "|"),
  ")\\b"
)

record_level_derived <- record_level_derived |>
  mutate(
    lmic_flag_extracted = as_bool(lmic_flag_raw),
    ssa_flag_extracted = as_bool(ssa_flag_raw),
    south_africa_flag_extracted = as_bool(south_africa_flag_raw),
    south_africa_country_text_hit = str_detect(
      replace_na(country_raw, ""),
      regex("\\bZAF\\b|South Africa", ignore_case = TRUE)
    ),
    ssa_country_text_hit = str_detect(
      str_to_upper(replace_na(country_raw, "")),
      regex(ssa_iso3_pattern)
    ),
    south_africa_derived = coalesce(
      south_africa_flag_extracted,
      FALSE
    ) | south_africa_country_text_hit,
    ssa_derived = south_africa_derived |
      coalesce(ssa_flag_extracted, FALSE) |
      ssa_country_text_hit,
    lmic_derived = ssa_derived |
      coalesce(lmic_flag_extracted, FALSE),
    region_group_derived = case_when(
      south_africa_derived ~ "South Africa",
      ssa_derived ~ "Other SSA",
      lmic_derived ~ "Other L&MIC",
      TRUE ~ "HIC / global / not positively classified"
    ),
    geography_review_issue = case_when(
      south_africa_flag_extracted == TRUE &
        !south_africa_country_text_hit ~
        "SA flag is TRUE but South Africa is not explicit in base.2; this may be review-level coverage and requires verification",
      south_africa_country_text_hit &
        south_africa_flag_extracted != TRUE ~
        "South Africa is explicit in base.2 but the SA flag is not TRUE",
      ssa_flag_extracted == TRUE &
        !ssa_country_text_hit &
        !south_africa_country_text_hit ~
        "SSA flag is TRUE but no SSA country is explicit in base.2; check the supporting quote",
      ssa_country_text_hit &
        ssa_flag_extracted != TRUE &
        !south_africa_country_text_hit ~
        "An SSA country is explicit in base.2 but the SSA flag is not TRUE",
      ssa_flag_extracted == TRUE & lmic_flag_extracted == FALSE ~
        "SSA flag is TRUE but the L&MIC flag is FALSE",
      south_africa_flag_extracted == TRUE & ssa_flag_extracted == FALSE ~
        "South Africa flag is TRUE but the SSA flag is FALSE",
      TRUE ~ NA_character_
    )
  )


# -----------------------------------------------------------------------------
# 6. Population and age composition
# -----------------------------------------------------------------------------

record_level_derived <- record_level_derived |>
  mutate(
    age_as_reported = extract_age_as_reported(population_age_raw),
    age_10_19_derived = parse_age_mapping(
      population_age_raw,
      "maps_10_19"
    ),
    age_10_24_derived = parse_age_mapping(
      population_age_raw,
      "maps_10_24"
    ),
    age_18_24_derived = parse_age_mapping(
      population_age_raw,
      "maps_18_24"
    ),
    mixed_population_marker = str_detect(
      replace_na(age_as_reported, ""),
      regex(
        paste(
          "mixed",
          "all ages",
          "children.*adults",
          "adolescents.*adults",
          "young people.*adults",
          "parents",
          "caregivers",
          "clinicians",
          "providers",
          "professionals",
          "general population",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    population_category_derived = case_when(
      age_10_19_derived & mixed_population_marker ~
        "Mixed-age evidence including adolescents or young people",
      age_10_19_derived ~
        "Adolescent/youth-focused (includes ages 10-19)",
      !age_10_19_derived & age_10_24_derived ~
        "Includes ages 18-24 but not ages 10-19",
      !is_present_text(population_age_raw) &
        evidence_group_derived != "Primary empirical" ~
        "Review/methodological record with age not applicable or not reported",
      is_present_text(population_age_raw) ~
        "Adult or other population",
      TRUE ~ "Not reported/unclear"
    )
  )


# -----------------------------------------------------------------------------
# 7. GenAI, single-session relevance and intervention/object type
# -----------------------------------------------------------------------------

record_level_derived <- record_level_derived |>
  mutate(
    genai_classification_text = str_c(
      replace_na(title, ""),
      replace_na(intervention_object_raw, ""),
      replace_na(evidence_type_raw, ""),
      replace_na(modality_delivery_raw, ""),
      replace_na(architecture_raw, ""),
      sep = " || "
    ),
    genai_explicit_derived = str_detect(
      genai_classification_text,
      regex(
        paste(
          "generative (AI|artificial intelligence)",
          "large language model",
          "\\bLLMs?\\b",
          "ChatGPT",
          "\\bGPT(?:[- ]?[0-9]+(?:\\.[0-9]+)?|[- ]?4o)?\\b",
          "Claude",
          "LLaMA",
          "Gemini",
          "retrieval[- ]augmented generation",
          "\\bRAG[- ]based\\b",
          "foundation model",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    single_session_classification_text = str_c(
      replace_na(title, ""),
      replace_na(intervention_object_raw, ""),
      replace_na(therapeutic_approach_raw, ""),
      replace_na(dose_structure_raw, ""),
      sep = " || "
    ),
    single_session_explicit_derived = str_detect(
      single_session_classification_text,
      regex(
        paste(
          "single[- ]session intervention",
          "\\bdigital SSI\\b",
          "\\bSSI\\b",
          "one[- ]session",
          "one(?:[- ][A-Za-z0-9]+){0,3}[- ]session",
          "single therapy session",
          "single(?:[- ][A-Za-z0-9]+){0,4}[- ]session",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    chatbot_marker = str_detect(
      genai_classification_text,
      regex(
        paste(
          "chatbot",
          "conversational agent",
          "relational agent",
          "virtual agent",
          "embodied conversational agent",
          "digital coach",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    rule_based_marker = str_detect(
      genai_classification_text,
      regex(
        paste(
          "rule[- ]based",
          "decision tree",
          "predefined",
          "pre-scripted",
          "prescripted",
          "algorithmic workflow",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    hybrid_marker = str_detect(
      genai_classification_text,
      regex("\\bhybrid\\b", ignore_case = TRUE)
    ),
    app_web_marker = str_detect(
      genai_classification_text,
      regex(
        paste(
          "mobile app",
          "smartphone app",
          "web[- ]based",
          "web platform",
          "website",
          "digital intervention",
          "online intervention",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    sms_messaging_marker = str_detect(
      genai_classification_text,
      regex(
        "\\bSMS\\b|\\bUSSD\\b|WhatsApp|text messaging",
        ignore_case = TRUE
      )
    ),
    measurement_validation_marker = str_detect(
      str_c(
        replace_na(title, ""),
        replace_na(evidence_type_raw, ""),
        sep = " || "
      ),
      regex(
        paste(
          "measure validation",
          "psychometric",
          "validation study",
          "audit",
          "benchmark",
          "safety testing",
          "response evaluation",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    intervention_object_category_derived = case_when(
      genai_explicit_derived & chatbot_marker ~
        "GenAI/LLM chatbot or conversational agent",
      genai_explicit_derived ~
        "Other GenAI/LLM application or evaluation object",
      hybrid_marker & chatbot_marker ~
        "Hybrid chatbot/agent (non-LLM or unclear)",
      rule_based_marker & chatbot_marker ~
        "Rule-based chatbot or conversational agent",
      chatbot_marker ~
        "Chatbot/conversational agent (architecture mixed or unclear)",
      single_session_explicit_derived ~
        "Digital single-session intervention",
      measurement_validation_marker ~
        "Measure, validation, audit or evaluation object",
      evidence_group_derived == "Methods/framework/regulatory" ~
        "Framework, guidance, regulatory or methods object",
      sms_messaging_marker ~
        "SMS or messaging intervention",
      app_web_marker ~
        "Other app/web digital mental-health intervention",
      TRUE ~ "Other or mixed intervention/object"
    )
  )


# -----------------------------------------------------------------------------
# 8. Setting and delivery channel
# -----------------------------------------------------------------------------

record_level_derived <- record_level_derived |>
  mutate(
    setting_category = case_when(
      str_to_lower(str_squish(setting_raw)) == "school" ~ "School",
      str_to_lower(str_squish(setting_raw)) == "phc" ~ "PHC",
      str_to_lower(str_squish(setting_raw)) == "community" ~ "Community",
      str_to_lower(str_squish(setting_raw)) == "online" ~ "Online",
      str_to_lower(str_squish(setting_raw)) == "mixed" ~ "Mixed",
      str_to_lower(str_squish(setting_raw)) == "other" ~ "Other",
      TRUE ~ "Not reported"
    ),
    delivery_channel_text = str_c(
      replace_na(intervention_object_raw, ""),
      replace_na(modality_delivery_raw, ""),
      replace_na(entry_channel_raw, ""),
      sep = " || "
    ),
    channel_sms_ussd = str_detect(
      delivery_channel_text,
      regex(
        "\\bSMS\\b|\\bUSSD\\b|short message|text messaging",
        ignore_case = TRUE
      )
    ),
    channel_whatsapp = str_detect(
      delivery_channel_text,
      regex("WhatsApp", ignore_case = TRUE)
    ),
    channel_mobile_app = str_detect(
      delivery_channel_text,
      regex(
        paste(
          "mobile app",
          "smartphone app",
          "tablet app",
          "native iOS",
          "Android app",
          "in-app",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    channel_web = str_detect(
      delivery_channel_text,
      regex(
        paste(
          "web[- ]based",
          "web platform",
          "web app",
          "website",
          "browser",
          "online platform",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    channel_social_messaging = str_detect(
      delivery_channel_text,
      regex(
        paste(
          "Facebook Messenger",
          "WeChat",
          "Telegram",
          "instant messenger",
          "messaging platform",
          "social media",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    channel_voice_ivr = str_detect(
      delivery_channel_text,
      regex(
        paste(
          "\\bIVR\\b",
          "interactive voice",
          "voice assistant",
          "telephone",
          "phone call",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    channel_vr_xr = str_detect(
      delivery_channel_text,
      regex(
        "virtual reality|extended reality|\\bVR\\b|\\bXR\\b",
        ignore_case = TRUE
      )
    ),
    any_named_channel = channel_sms_ussd |
      channel_whatsapp |
      channel_mobile_app |
      channel_web |
      channel_social_messaging |
      channel_voice_ivr |
      channel_vr_xr,
    delivery_channel_source_present =
      is_present_text(intervention_object_raw) |
      is_present_text(modality_delivery_raw) |
      is_present_text(entry_channel_raw),
    channel_other = delivery_channel_source_present &
      !any_named_channel,
    channel_not_reported = !delivery_channel_source_present
  )


# -----------------------------------------------------------------------------
# 9. Sponsor and declared interest
# -----------------------------------------------------------------------------

record_level_derived <- record_level_derived |>
  mutate(
    funding_text = str_to_lower(replace_na(funding_coi_raw, "")),
    no_external_funding_marker = str_detect(
      funding_text,
      regex(
        paste(
          "no external funding",
          "no specific funding",
          "no financial support",
          "received no funding",
          "received no financial support",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    sponsor_industry_marker = str_detect(
      funding_text,
      regex(
        paste(
          "intervention_funder:.*(inc\\.|ltd\\.|corp\\.|company|commercial|vendor)",
          "research_funder:.*(inc\\.|ltd\\.|corp\\.|company|commercial|industry)",
          "provided the (app|platform|software|chatbot)",
          "employee of",
          "stockholder",
          "shareholder",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    sponsor_government_marker = str_detect(
      funding_text,
      regex(
        paste(
          "ministry",
          "department of",
          "government",
          "national institutes? of health",
          "\\bnih\\b",
          "national health service",
          "\\bnhs\\b",
          "uk research and innovation",
          "\\bukri\\b",
          "medical research council",
          "national science foundation",
          "european commission",
          "public health agency",
          "research council",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    sponsor_ngo_marker = str_detect(
      funding_text,
      regex(
        paste(
          "non[- ]profit",
          "nonprofit",
          "\\bngo\\b",
          "charit",
          "philanthrop",
          "charitable foundation",
          "wellcome trust",
          "gates foundation",
          "unicef",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    sponsor_academic_marker = str_detect(
      funding_text,
      regex(
        paste(
          "university",
          "college",
          "academic",
          "research institute",
          "medical school",
          "hospital",
          "medical cent",
          "health system",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    sponsor_marker_count = rowSums(
      across(
        c(
          sponsor_industry_marker,
          sponsor_government_marker,
          sponsor_ngo_marker,
          sponsor_academic_marker
        )
      ),
      na.rm = TRUE
    ),
    sponsor_category_derived = case_when(
      sponsor_marker_count > 1 ~ "Mixed",
      sponsor_industry_marker ~ "Industry/commercial",
      sponsor_government_marker ~ "Government/public agency",
      sponsor_ngo_marker ~ "NGO/philanthropic",
      sponsor_academic_marker ~ "Academic/research institution",
      no_external_funding_marker ~ "No external funding stated",
      !is_present_text(funding_coi_raw) ~ "Not reported",
      TRUE ~ "Other/unclear"
    ),
    coi_declaration_text = str_match(
      replace_na(funding_coi_raw, ""),
      regex("coi_declaration\\s*:\\s*(.*)$", ignore_case = TRUE)
    )[, 2],
    coi_no_interest_marker = str_detect(
      replace_na(coi_declaration_text, ""),
      regex(
        paste(
          "none declared",
          "no conflict",
          "no competing",
          "declared no",
          "absence of any commercial or financial relationships",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),
    coi_declared_interest_marker = !coi_no_interest_marker &
      str_detect(
        replace_na(coi_declaration_text, ""),
        regex(
          paste(
            "conflict",
            "competing interest",
            "employee",
            "stock",
            "shareholder",
            "consult",
            "advisory",
            "patent",
            "commercial relationship",
            sep = "|"
          ),
          ignore_case = TRUE
        )
      ),
    declared_interest_status = case_when(
      coi_no_interest_marker ~ "No declared conflict/interest",
      coi_declared_interest_marker ~ "Declared interest or relationship",
      is_present_text(coi_declaration_text) ~ "Declaration present but unclear",
      TRUE ~ "Not reported"
    )
  )


# -----------------------------------------------------------------------------
# 10. Optional manual overrides
# -----------------------------------------------------------------------------

# The override file uses TRUE/FALSE text. Leave a cell blank to retain the
# derived value. evidence_group_override must use one of evidence_group_order.
override_columns <- c(
  "record_id",
  "south_africa_override",
  "ssa_override",
  "lmic_override",
  "age_10_19_override",
  "age_10_24_override",
  "age_18_24_override",
  "genai_override",
  "single_session_override",
  "evidence_group_override",
  "override_notes"
)

if (!file.exists(override_file)) {
  override_template <- record_level_derived |>
    transmute(
      record_id,
      title,
      south_africa_derived,
      ssa_derived,
      lmic_derived,
      age_10_19_derived,
      age_10_24_derived,
      age_18_24_derived,
      genai_explicit_derived,
      single_session_explicit_derived,
      evidence_group_derived,
      south_africa_override = NA_character_,
      ssa_override = NA_character_,
      lmic_override = NA_character_,
      age_10_19_override = NA_character_,
      age_10_24_override = NA_character_,
      age_18_24_override = NA_character_,
      genai_override = NA_character_,
      single_session_override = NA_character_,
      evidence_group_override = NA_character_,
      override_notes = NA_character_
    )

  write_csv(override_template, override_file, na = "")
}

overrides <- read_csv(
  override_file,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

for (column_name in override_columns) {
  if (!column_name %in% names(overrides)) {
    overrides[[column_name]] <- NA_character_
  }
}

overrides <- overrides |>
  select(all_of(override_columns)) |>
  mutate(record_id = str_squish(record_id)) |>
  filter(!is.na(record_id), record_id != "") |>
  distinct(record_id, .keep_all = TRUE)

record_level <- record_level_derived |>
  left_join(overrides, by = "record_id") |>
  mutate(
    south_africa = coalesce(
      as_bool(south_africa_override),
      south_africa_derived
    ),
    ssa = south_africa | coalesce(
      as_bool(ssa_override),
      ssa_derived
    ),
    lmic = ssa | coalesce(
      as_bool(lmic_override),
      lmic_derived
    ),
    age_10_19 = coalesce(
      as_bool(age_10_19_override),
      age_10_19_derived
    ),
    age_10_24 = coalesce(
      as_bool(age_10_24_override),
      age_10_24_derived
    ),
    age_18_24 = coalesce(
      as_bool(age_18_24_override),
      age_18_24_derived
    ),
    genai_explicit = coalesce(
      as_bool(genai_override),
      genai_explicit_derived
    ),
    single_session_explicit = coalesce(
      as_bool(single_session_override),
      single_session_explicit_derived
    ),
    evidence_group = case_when(
      evidence_group_override %in% evidence_group_order ~
        evidence_group_override,
      TRUE ~ evidence_group_derived
    ),
    region_group = case_when(
      south_africa ~ "South Africa",
      ssa ~ "Other SSA",
      lmic ~ "Other L&MIC",
      TRUE ~ "HIC / global / not positively classified"
    ),
    population_category = case_when(
      age_10_19 & mixed_population_marker ~
        "Mixed-age evidence including adolescents or young people",
      age_10_19 ~
        "Adolescent/youth-focused (includes ages 10-19)",
      !age_10_19 & age_10_24 ~
        "Includes ages 18-24 but not ages 10-19",
      !is_present_text(population_age_raw) &
        evidence_group != "Primary empirical" ~
        "Review/methodological record with age not applicable or not reported",
      is_present_text(population_age_raw) ~
        "Adult or other population",
      TRUE ~ "Not reported/unclear"
    ),
    intervention_object_category = case_when(
      genai_explicit & chatbot_marker ~
        "GenAI/LLM chatbot or conversational agent",
      genai_explicit ~
        "Other GenAI/LLM application or evaluation object",
      TRUE ~ intervention_object_category_derived
    ),
    direct_sa_adolescent_10_19_genai =
      south_africa & age_10_19 & genai_explicit,
    direct_sa_youth_10_24_genai =
      south_africa & age_10_24 & genai_explicit,
    strict_primary_sa_adolescent_10_19_genai =
      evidence_group == "Primary empirical" &
      direct_sa_adolescent_10_19_genai,
    strict_primary_sa_youth_10_24_genai =
      evidence_group == "Primary empirical" &
      direct_sa_youth_10_24_genai,
    genai_single_session = genai_explicit & single_session_explicit,
    primary_genai_single_session =
      evidence_group == "Primary empirical" & genai_single_session,
    region_group = factor(region_group, levels = region_order),
    evidence_group = factor(
      evidence_group,
      levels = evidence_group_order
    ),
    population_category = factor(
      population_category,
      levels = population_order
    )
  )


# -----------------------------------------------------------------------------
# 11. Summary datasets
# -----------------------------------------------------------------------------

n_records <- n_distinct(record_level$record_id)
n_primary <- sum(record_level$evidence_group == "Primary empirical", na.rm = TRUE)
n_reviews <- sum(record_level$evidence_group == "Review-level", na.rm = TRUE)
n_methods <- sum(
  record_level$evidence_group == "Methods/framework/regulatory",
  na.rm = TRUE
)
n_other_evidence <- sum(
  record_level$evidence_group == "Other non-empirical/unclear",
  na.rm = TRUE
)

evidence_type_counts <- record_level |>
  count(evidence_group, name = "n", .drop = FALSE) |>
  transmute(category = as.character(evidence_group), n)

geography_counts <- record_level |>
  count(region_group, name = "n", .drop = FALSE) |>
  transmute(category = as.character(region_group), n)

population_counts <- record_level |>
  count(population_category, name = "n", .drop = FALSE) |>
  transmute(category = as.character(population_category), n)

study_design_counts <- record_level |>
  mutate(
    study_design_category = if_else(
      is_present_text(study_design_raw),
      study_design_raw,
      "Not reported"
    )
  ) |>
  count(study_design_category, name = "n") |>
  transmute(category = study_design_category, n)

intervention_object_counts <- record_level |>
  count(intervention_object_category, name = "n") |>
  transmute(category = intervention_object_category, n)

setting_counts <- record_level |>
  count(setting_category, name = "n") |>
  transmute(category = setting_category, n)

channel_counts <- record_level |>
  transmute(
    record_id,
    `SMS/USSD` = channel_sms_ussd,
    WhatsApp = channel_whatsapp,
    `Mobile app/in-app` = channel_mobile_app,
    `Web/browser` = channel_web,
    `Other social/messaging platform` = channel_social_messaging,
    `Voice/IVR` = channel_voice_ivr,
    `VR/XR` = channel_vr_xr,
    `Other/unclear` = channel_other,
    `Not reported/not applicable` = channel_not_reported
  ) |>
  pivot_longer(
    -record_id,
    names_to = "category",
    values_to = "present"
  ) |>
  filter(present) |>
  count(category, name = "n")

sponsor_counts <- record_level |>
  count(sponsor_category_derived, name = "n") |>
  transmute(category = sponsor_category_derived, n)

declared_interest_counts <- record_level |>
  count(declared_interest_status, name = "n") |>
  transmute(category = declared_interest_status, n)

publication_year_counts <- record_level |>
  count(publication_year, name = "n") |>
  arrange(publication_year)

valid_years <- record_level$publication_year[!is.na(record_level$publication_year)]
publication_year_min <- if (
  length(valid_years) > 0
) min(valid_years) else NA_integer_
publication_year_max <- if (
  length(valid_years) > 0
) max(valid_years) else NA_integer_
publication_year_median <- if (
  length(valid_years) > 0
) median(valid_years) else NA_real_
n_year_missing <- sum(is.na(record_level$publication_year))
n_since_2020 <- sum(record_level$publication_year >= 2020, na.rm = TRUE)
n_since_2023 <- sum(record_level$publication_year >= 2023, na.rm = TRUE)

n_age_10_19 <- sum(record_level$age_10_19, na.rm = TRUE)
n_age_10_24 <- sum(record_level$age_10_24, na.rm = TRUE)
n_age_18_24_not_10_19 <- sum(
  record_level$age_18_24 & !record_level$age_10_19,
  na.rm = TRUE
)
n_genai <- sum(record_level$genai_explicit, na.rm = TRUE)
n_primary_genai <- sum(
  record_level$genai_explicit &
    record_level$evidence_group == "Primary empirical",
  na.rm = TRUE
)
n_genai_single_session <- sum(record_level$genai_single_session, na.rm = TRUE)
n_primary_genai_single_session <- sum(
  record_level$primary_genai_single_session,
  na.rm = TRUE
)

n_direct_record_10_19 <- sum(
  record_level$direct_sa_adolescent_10_19_genai,
  na.rm = TRUE
)
n_direct_record_10_24 <- sum(
  record_level$direct_sa_youth_10_24_genai,
  na.rm = TRUE
)
n_direct_primary_10_19 <- sum(
  record_level$strict_primary_sa_adolescent_10_19_genai,
  na.rm = TRUE
)
n_direct_primary_10_24 <- sum(
  record_level$strict_primary_sa_youth_10_24_genai,
  na.rm = TRUE
)

directness_summary <- tribble(
  ~metric, ~numerator, ~denominator, ~interpretation,
  "All records: South Africa + ages 10-19 + explicit GenAI",
  n_direct_record_10_19,
  n_records,
  "Includes review-level records flagged as covering South African evidence",
  "All records: South Africa + ages 10-24 + explicit GenAI",
  n_direct_record_10_24,
  n_records,
  "Uses the broader youth definition and includes review-level coverage",
  "Primary empirical only: South Africa + ages 10-19 + explicit GenAI",
  n_direct_primary_10_19,
  n_primary,
  "Strictest estimate of direct evidence for adolescents",
  "Primary empirical only: South Africa + ages 10-24 + explicit GenAI",
  n_direct_primary_10_24,
  n_primary,
  "Broader youth estimate among primary empirical records",
  "All records: explicit GenAI + explicit single-session format",
  n_genai_single_session,
  n_records,
  "Keyword-derived and requires manual verification",
  "Primary empirical only: explicit GenAI + explicit single-session format",
  n_primary_genai_single_session,
  n_primary,
  "Closest intervention-level evidence to a GenAI single-session tool"
) |>
  mutate(
    proportion = if_else(
      denominator > 0,
      numerator / denominator,
      NA_real_
    ),
    percent = if_else(
      is.na(proportion),
      NA_character_,
      paste0(
        formatC(100 * proportion, format = "f", digits = 1),
        "%"
      )
    )
  )


# -----------------------------------------------------------------------------
# 12. Table 2
# -----------------------------------------------------------------------------

geography_distribution_text <- format_count_rows(
  geography_counts,
  n_records
)

population_distribution_text <- format_count_rows(
  population_counts,
  n_records
)

study_design_distribution_text <- format_count_rows(
  study_design_counts,
  n_records,
  max_items = 12
)

intervention_distribution_text <- format_count_rows(
  intervention_object_counts,
  n_records,
  max_items = 12
)

setting_distribution_text <- format_count_rows(
  setting_counts,
  n_records,
  max_items = 10
)

channel_distribution_text <- format_count_rows(
  channel_counts,
  n_records,
  max_items = 10
)

sponsor_distribution_text <- format_count_rows(
  sponsor_counts,
  n_records,
  max_items = 10
)

coi_distribution_text <- format_count_rows(
  declared_interest_counts,
  n_records,
  max_items = 10
)

table_2 <- tribble(
  ~Dimension, ~`Preliminary distribution`, ~`Interpretive note`,
  "Evidence type",
  glue(
    "Primary empirical: {format_n_percent(n_primary, n_records)}; ",
    "review-level: {format_n_percent(n_reviews, n_records)}; ",
    "methods/framework/regulatory: {format_n_percent(n_methods, n_records)}; ",
    "other non-empirical/unclear: {format_n_percent(n_other_evidence, n_records)}."
  ),
  paste(
    "Review-level records are catalogued separately from primary empirical",
    "records and should not be interpreted as additional independent studies."
  ),
  "Geography",
  geography_distribution_text,
  paste(
    "South Africa includes primary records conducted in South Africa and",
    "review-level records flagged as covering South African evidence; it is",
    "therefore a count of records containing South African evidence, not only",
    "studies conducted in South Africa. The HIC/global category also includes",
    "global and geographically unspecified records that were not positively",
    "classified as South Africa, other SSA or other L&MIC evidence."
  ),
  "Population",
  population_distribution_text,
  glue(
    "{format_n_percent(n_age_10_19, n_records)} records included ages 10-19, ",
    "and {format_n_percent(n_age_10_24, n_records)} included some part of ages 10-24. ",
    "Age mappings are extracted fields and remain subject to verification."
  ),
  "Study design",
  study_design_distribution_text,
  paste(
    "These are descriptive design categories only; no quality or certainty",
    "rating is derived from study design."
  ),
  "Intervention/object",
  intervention_distribution_text,
  glue(
    "Explicit GenAI was identified in {format_n_percent(n_genai, n_records)} records; ",
    "{format_n_percent(n_primary_genai_single_session, n_primary)} primary empirical ",
    "records were provisionally classified as both GenAI and single-session. ",
    "GenAI and object categories are keyword-derived and require manual checking."
  ),
  "Settings",
  setting_distribution_text,
  paste(
    "Online, school, community and PHC records are the settings most readily",
    "compared with possible MHAA delivery pathways, but transferability depends",
    "on the actual recruitment, access and referral model."
  ),
  "Delivery channel",
  channel_distribution_text,
  paste(
    "Delivery channels are non-mutually exclusive and keyword-derived from",
    "base.7, 1b.1 and PL.2. Counts may exceed the number of records and require",
    "record-level verification."
  ),
  "Sponsor / declared interest",
  glue(
    "Sponsor: {sponsor_distribution_text}. Declared interest: {coi_distribution_text}."
  ),
  paste(
    "Sponsor type and declared-interest status are descriptive classifications",
    "derived from base.13. They are not used to rate study quality."
  )
)


# -----------------------------------------------------------------------------
# 13. Draft Section 3.2 narrative
# -----------------------------------------------------------------------------

top_designs_text <- study_design_counts |>
  arrange(desc(n), category) |>
  slice_head(n = 4) |>
  mutate(item = glue("{category} (n={n})")) |>
  pull(item) |>
  paste(collapse = ", ")

top_objects_text <- intervention_object_counts |>
  arrange(desc(n), category) |>
  slice_head(n = 3) |>
  mutate(item = glue("{category} (n={n})")) |>
  pull(item) |>
  paste(collapse = ", ")

top_settings_text <- setting_counts |>
  arrange(desc(n), category) |>
  slice_head(n = 3) |>
  mutate(item = glue("{category} (n={n})")) |>
  pull(item) |>
  paste(collapse = ", ")

paragraph_1 <- glue(
  "The preliminary evidence base comprised {n_records} analytical records: ",
  "{format_n_percent(n_primary, n_records)} primary empirical records, ",
  "{format_n_percent(n_reviews, n_records)} review-level records, ",
  "{format_n_percent(n_methods, n_records)} methods, framework or regulatory records, ",
  "and {format_n_percent(n_other_evidence, n_records)} other non-empirical or unclear records. ",
  "Publication years ranged from {publication_year_min} to {publication_year_max} ",
  "(median {publication_year_median}); {format_n_percent(n_since_2023, n_records)} ",
  "were published from 2023 onwards. The most frequent extracted design categories were ",
  "{top_designs_text}. Review-level records are catalogued separately and do not represent ",
  "additional independent primary studies."
)

paragraph_2 <- glue(
  "The most common intervention or object categories were {top_objects_text}, and the ",
  "predominant settings were {top_settings_text}. The extracted age mappings indicated that ",
  "{format_n_percent(n_age_10_19, n_records)} records included ages 10-19 and ",
  "{format_n_percent(n_age_10_24, n_records)} included some part of ages 10-24. ",
  "Explicit GenAI terminology was identified in {format_n_percent(n_genai, n_records)} records, ",
  "including {format_n_percent(n_primary_genai, n_primary)} of primary empirical records. ",
  "Across all record types, {format_n_percent(n_direct_record_10_19, n_records)} were flagged as ",
  "covering South Africa, ages 10-19 and explicit GenAI, and ",
  "{format_n_percent(n_direct_record_10_24, n_records)} met the same criterion using the broader ",
  "10-24 age definition. When restricted to primary empirical evidence, the corresponding counts ",
  "were {format_n_percent(n_direct_primary_10_19, n_primary)} and ",
  "{format_n_percent(n_direct_primary_10_24, n_primary)}, respectively. These directness estimates ",
  "are preliminary: South African review coverage, age mappings and GenAI classifications remain ",
  "subject to record-level verification and may change in the final report."
)

section_3_2_draft <- paste(
  "3.2 Characteristics of the included evidence",
  "",
  paragraph_1,
  "",
  paragraph_2,
  sep = "\n"
)


# -----------------------------------------------------------------------------
# 14. Audit files
# -----------------------------------------------------------------------------

geography_audit <- record_level |>
  filter(
    !is.na(geography_review_issue) |
      south_africa |
      ssa
  ) |>
  select(
    record_id,
    title,
    evidence_group,
    country_raw,
    lmic_flag_raw,
    ssa_flag_raw,
    south_africa_flag_raw,
    lmic_flag_extracted,
    ssa_flag_extracted,
    south_africa_flag_extracted,
    south_africa_country_text_hit,
    ssa_country_text_hit,
    lmic,
    ssa,
    south_africa,
    region_group,
    geography_review_issue,
    geography_lmic_agreement,
    geography_ssa_agreement,
    geography_sa_agreement,
    override_notes
  ) |>
  arrange(region_group, title)

genai_directness_audit <- record_level |>
  filter(
    genai_explicit |
      south_africa |
      direct_sa_adolescent_10_19_genai |
      direct_sa_youth_10_24_genai
  ) |>
  select(
    record_id,
    title,
    publication_year,
    evidence_group,
    region_group,
    south_africa,
    country_raw,
    population_age_raw,
    age_10_19,
    age_10_24,
    intervention_object_raw,
    modality_delivery_raw,
    architecture_raw,
    dose_structure_raw,
    genai_explicit,
    single_session_explicit,
    direct_sa_adolescent_10_19_genai,
    direct_sa_youth_10_24_genai,
    strict_primary_sa_adolescent_10_19_genai,
    strict_primary_sa_youth_10_24_genai,
    age_agreement,
    intervention_agreement,
    modality_agreement,
    architecture_agreement,
    override_notes
  ) |>
  arrange(
    desc(strict_primary_sa_youth_10_24_genai),
    desc(direct_sa_youth_10_24_genai),
    desc(south_africa),
    title
  )

population_audit <- record_level |>
  select(
    record_id,
    title,
    evidence_group,
    population_age_raw,
    age_as_reported,
    age_10_19,
    age_10_24,
    age_18_24,
    mixed_population_marker,
    population_category,
    age_agreement,
    override_notes
  ) |>
  arrange(population_category, title)

sponsor_audit <- record_level |>
  select(
    record_id,
    title,
    funding_coi_raw,
    sponsor_category_derived,
    declared_interest_status,
    sponsor_industry_marker,
    sponsor_government_marker,
    sponsor_ngo_marker,
    sponsor_academic_marker,
    funding_agreement
  ) |>
  arrange(sponsor_category_derived, title)

classification_rules <- tribble(
  ~classification, ~source_fields, ~rule,
  "Evidence group",
  "base.6, base.12 and title",
  "Design categories are used first, supplemented by explicit review, framework/regulatory and empirical keywords.",
  "South Africa/SSA/L&MIC",
  "base.10, base.11a, base.11b and base.2",
  "Positive extracted flags are retained; explicit country codes in base.2 provide a safeguard. Disagreements are exported for review.",
  "Age 10-19/10-24/18-24",
  "base.5",
  "Uses the explicit maps_10_19, maps_10_24 and maps_18_24 components of the extracted composite field.",
  "GenAI",
  "title, base.7, base.12, 1b.1 and 1b.5",
  "Requires explicit GenAI/LLM terminology. AI, NLP or machine learning alone does not count as GenAI.",
  "Single-session",
  "title, base.7, 1b.2 and 1b.4",
  "Requires explicit single-session, one-session or SSI terminology.",
  "Delivery channel",
  "base.7, 1b.1 and PL.2",
  "Non-mutually-exclusive keyword flags. Counts require manual verification.",
  "Sponsor and declared interest",
  "base.13",
  "Keyword-derived descriptive classification; no quality rating is inferred."
)


# -----------------------------------------------------------------------------
# 15. Export all outputs
# -----------------------------------------------------------------------------

write_csv(
  record_level,
  file.path(output_dir, "section_3_2_record_level_characteristics.csv"),
  na = ""
)

write_csv(
  table_2,
  file.path(output_dir, "table_2_characteristics_of_included_evidence.csv"),
  na = ""
)

write_csv(
  evidence_type_counts,
  file.path(output_dir, "section_3_2_evidence_type_counts.csv"),
  na = ""
)

write_csv(
  geography_counts,
  file.path(output_dir, "section_3_2_geography_counts.csv"),
  na = ""
)

write_csv(
  population_counts,
  file.path(output_dir, "section_3_2_population_counts.csv"),
  na = ""
)

write_csv(
  study_design_counts,
  file.path(output_dir, "section_3_2_study_design_counts.csv"),
  na = ""
)

write_csv(
  intervention_object_counts,
  file.path(output_dir, "section_3_2_intervention_object_counts.csv"),
  na = ""
)

write_csv(
  setting_counts,
  file.path(output_dir, "section_3_2_setting_counts.csv"),
  na = ""
)

write_csv(
  channel_counts,
  file.path(output_dir, "section_3_2_delivery_channel_counts.csv"),
  na = ""
)

write_csv(
  sponsor_counts,
  file.path(output_dir, "section_3_2_sponsor_counts.csv"),
  na = ""
)

write_csv(
  declared_interest_counts,
  file.path(output_dir, "section_3_2_declared_interest_counts.csv"),
  na = ""
)

write_csv(
  publication_year_counts,
  file.path(output_dir, "section_3_2_publication_year_counts.csv"),
  na = ""
)

write_csv(
  directness_summary,
  file.path(output_dir, "section_3_2_directness_summary.csv"),
  na = ""
)

write_csv(
  geography_audit,
  file.path(output_dir, "section_3_2_geography_audit.csv"),
  na = ""
)

write_csv(
  genai_directness_audit,
  file.path(output_dir, "section_3_2_genai_directness_audit.csv"),
  na = ""
)

write_csv(
  population_audit,
  file.path(output_dir, "section_3_2_population_audit.csv"),
  na = ""
)

write_csv(
  sponsor_audit,
  file.path(output_dir, "section_3_2_sponsor_audit.csv"),
  na = ""
)

write_csv(
  classification_rules,
  file.path(output_dir, "section_3_2_classification_rules.csv"),
  na = ""
)

write_csv(
  codebook_raw,
  file.path(output_dir, "section_3_2_source_codebook.csv"),
  na = ""
)

write_lines(
  section_3_2_draft,
  file.path(output_dir, "section_3_2_draft_narrative.txt")
)

write_lines(
  c(
    "SECTION 3.2 ANALYSIS COMPLETE",
    "",
    glue("Analytical records: {n_records}"),
    glue("Primary empirical records: {n_primary}"),
    glue("Review-level records: {n_reviews}"),
    glue("Methods/framework/regulatory records: {n_methods}"),
    glue("Other non-empirical/unclear records: {n_other_evidence}"),
    glue("Publication years: {publication_year_min}-{publication_year_max}"),
    glue("Records including ages 10-19: {n_age_10_19}"),
    glue("Records including ages 10-24: {n_age_10_24}"),
    glue("Explicit GenAI records: {n_genai}"),
    glue("Primary empirical GenAI records: {n_primary_genai}"),
    glue("All records direct on SA + 10-19 + GenAI: {n_direct_record_10_19}"),
    glue("All records direct on SA + 10-24 + GenAI: {n_direct_record_10_24}"),
    glue("Primary empirical direct on SA + 10-19 + GenAI: {n_direct_primary_10_19}"),
    glue("Primary empirical direct on SA + 10-24 + GenAI: {n_direct_primary_10_24}"),
    "",
    paste(
      "Review the geography, GenAI/directness, population and sponsor audit",
      "files before using these preliminary counts in the final report."
    )
  ),
  file.path(output_dir, "section_3_2_run_summary.txt")
)

message("Section 3.2 outputs written to: ", normalizePath(output_dir))
message("Draft narrative: section_3_2_draft_narrative.txt")
message("Table 2: table_2_characteristics_of_included_evidence.csv")
