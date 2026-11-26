# ================================================================
# Section 3.3: Generate evidence descriptions for
#   1) Other L&MIC evidence
#   2) HIC and global evidence
#
# Inputs expected in the working directory:
#   - section_3_3_records_by_region_for_narrative.csv
#   - extraction_final-revised-SL_n=325.xlsx
#
# Outputs:
#   - section_3_3_paragraph_inputs/other_lmic_evidence_for_paragraph.csv
#   - section_3_3_paragraph_inputs/other_lmic_counts_by_subregion.csv
#   - section_3_3_paragraph_inputs/hic_global_chatbot_rcts_for_paragraph.csv
#   - section_3_3_paragraph_inputs/hic_global_guidance_for_paragraph.csv
#   - section_3_3_paragraph_inputs/region_classification_review.csv
#   - section_3_3_paragraph_inputs/section_3_3_auto_draft_descriptions.txt
#
# The text file is a deterministic drafting aid, not a substitute for
# study-level interpretation. Review the evidence CSVs before using it.
# ================================================================

library(tidyverse)
library(readxl)
library(glue)

# ----------------------------------------------------------------
# 1. File paths
# ----------------------------------------------------------------

regional_file <- "section_3_3_records_by_region_for_narrative.csv"
extraction_file <- "extraction_final-revised-SL_n=325.xlsx"
output_dir <- "section_3_3_paragraph_inputs"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(regional_file)) {
  stop("Cannot find: ", regional_file)
}

if (!file.exists(extraction_file)) {
  stop("Cannot find: ", extraction_file)
}


# ----------------------------------------------------------------
# 2. Helper functions
# ----------------------------------------------------------------

as_bool <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))

  case_when(
    x %in% c("true", "t", "yes", "y", "1") ~ TRUE,
    x %in% c("false", "f", "no", "n", "0") ~ FALSE,
    TRUE ~ NA
  )
}

clean_text <- function(x) {
  x <- str_squish(as.character(x))
  x[x %in% c("", "NA", "N/A", "na", "n/a", "— gated out —")] <- NA_character_
  x
}

is_present <- function(x) {
  x <- clean_text(x)
  !is.na(x)
}

compact_text <- function(x, width = 260) {
  x <- clean_text(x)
  if_else(
    is.na(x),
    NA_character_,
    str_trunc(x, width = width, side = "right", ellipsis = "…")
  )
}

# Combine several record-level text fields while removing blanks and duplicates.
combine_fields <- function(...) {
  fields <- list(...)

  pmap_chr(fields, function(...) {
    values <- clean_text(c(...))
    values <- unique(values[!is.na(values)])

    if (length(values) == 0) {
      NA_character_
    } else {
      paste(values, collapse = " | ")
    }
  })
}

# Read selected "... value" fields from an extraction sheet.
# named_fields must be a named character vector in this form:
# c(new_column_name = "field.id")
read_value_fields <- function(sheet_name, named_fields) {
  dat <- read_excel(
    extraction_file,
    sheet = sheet_name,
    .name_repair = "minimal"
  )

  source_columns <- paste0(unname(named_fields), " value")
  missing_columns <- setdiff(source_columns, names(dat))

  if (length(missing_columns) > 0) {
    warning(
      "Missing expected columns on sheet '", sheet_name, "': ",
      paste(missing_columns, collapse = ", ")
    )
  }

  available <- named_fields[source_columns %in% names(dat)]
  rename_pairs <- setNames(
    paste0(unname(available), " value"),
    names(available)
  )

  dat |>
    select(record_id, any_of(unname(rename_pairs))) |>
    rename(!!!rename_pairs) |>
    mutate(
      record_id = str_squish(as.character(record_id)),
      across(-record_id, clean_text)
    ) |>
    filter(!is.na(record_id), record_id != "") |>
    distinct(record_id, .keep_all = TRUE)
}

make_citation <- function(bibliographic, title) {
  bibliographic <- replace_na(as.character(bibliographic), "")
  title <- replace_na(as.character(title), "")

  author_string <- str_match(
    bibliographic,
    regex("authors:\\s*(.*?)\\s*\\|\\s*year:", ignore_case = TRUE)
  )[, 2]

  first_author <- str_extract(author_string, "^[^,;]+") |>
    str_squish()

  year <- str_match(
    bibliographic,
    regex("year:\\s*(\\d{4})", ignore_case = TRUE)
  )[, 2]

  organisation_record <- str_detect(
    str_c(title, bibliographic, sep = " | "),
    regex("World Health Organization|\\bWHO\\b", ignore_case = TRUE)
  )

  case_when(
    organisation_record & !is.na(year) ~ paste0("WHO (", year, ")"),
    !is.na(first_author) & !is.na(year) ~ paste0(first_author, " et al. (", year, ")"),
    !is.na(first_author) ~ first_author,
    TRUE ~ str_trunc(title, 80)
  )
}

has_any_iso <- function(x, codes) {
  pattern <- paste0("\\b(", paste(codes, collapse = "|"), ")\\b")
  str_detect(replace_na(x, ""), regex(pattern, ignore_case = TRUE))
}

tag_macro_regions <- function(x) {
  south_asia <- c("AFG", "BGD", "BTN", "IND", "MDV", "NPL", "PAK", "LKA")
  southeast_asia <- c(
    "BRN", "KHM", "IDN", "LAO", "MYS", "MMR", "PHL", "SGP",
    "THA", "TLS", "VNM"
  )
  latin_america_caribbean <- c(
    "ARG", "BOL", "BRA", "CHL", "COL", "CRI", "CUB", "DOM", "ECU",
    "SLV", "GTM", "GUY", "HTI", "HND", "JAM", "MEX", "NIC", "PAN",
    "PRY", "PER", "SUR", "TTO", "URY", "VEN"
  )
  mena <- c(
    "DZA", "BHR", "EGY", "IRN", "IRQ", "ISR", "JOR", "KWT", "LBN",
    "LBY", "MAR", "OMN", "PSE", "QAT", "SAU", "SYR", "TUN", "TUR",
    "ARE", "YEM"
  )

  tags <- character(0)

  if (has_any_iso(x, south_asia)) {
    tags <- c(tags, "South Asia")
  }
  if (has_any_iso(x, southeast_asia)) {
    tags <- c(tags, "Southeast Asia")
  }
  if (has_any_iso(x, latin_america_caribbean)) {
    tags <- c(tags, "Latin America/Caribbean")
  }
  if (has_any_iso(x, mena)) {
    tags <- c(tags, "MENA")
  }

  if (length(tags) == 0) {
    "Other/unclear L&MIC geography"
  } else {
    paste(tags, collapse = "; ")
  }
}

format_examples <- function(data, text_column, n = 4, width = 230) {
  selected <- data |>
    filter(is_present(.data[[text_column]])) |>
    arrange(desc(evidence_priority), citation) |>
    distinct(record_id, .keep_all = TRUE) |>
    slice_head(n = n) |>
    mutate(
      example = paste0(
        citation,
        ": ",
        compact_text(.data[[text_column]], width = width)
      )
    ) |>
    pull(example)

  if (length(selected) == 0) {
    "not clearly reported in the extracted fields"
  } else {
    paste(selected, collapse = "; ")
  }
}

format_trial_features <- function(data, n = 5) {
  selected <- data |>
    arrange(desc(mhaa_directness_score), citation) |>
    distinct(record_id, .keep_all = TRUE) |>
    slice_head(n = n) |>
    transmute(
      example = paste0(
        citation,
        " [population: ",
        coalesce(compact_text(population_age, 100), "not clearly reported"),
        "; comparator: ",
        coalesce(compact_text(comparator, 100), "not clearly reported"),
        "; follow-up: ",
        coalesce(compact_text(effect_trajectory, 120), "not clearly reported"),
        "; dose: ",
        coalesce(compact_text(session_dose, 100), "not clearly reported"),
        "]"
      )
    ) |>
    pull(example)

  if (length(selected) == 0) {
    "no HIC/global chatbot RCTs were identifiable from the extracted fields"
  } else {
    paste(selected, collapse = "; ")
  }
}


# ----------------------------------------------------------------
# 3. Read the regional narrative file
# ----------------------------------------------------------------

regional <- read_csv(
  regional_file,
  show_col_types = FALSE,
  na = c("", "NA", "N/A")
) |>
  mutate(
    record_id = str_squish(as.character(record_id)),
    across(
      any_of(c(
        "lmic_flag", "ssa_flag", "south_africa_flag", "genai_derived",
        "has_RQ 1a. Effectiveness & engagement",
        "has_RQ 1b. Intervention design",
        "has_RQ 1c. Implementation",
        "has_RQ 1d. Safety",
        "has_RQ 1e. Measurement",
        "has_Stream: SA context",
        "has_Stream: Product / digital landscape"
      )),
      as_bool
    ),
    across(where(is.character), clean_text)
  ) |>
  filter(!is.na(record_id), record_id != "") |>
  distinct(record_id, .keep_all = TRUE)


# ----------------------------------------------------------------
# 4. Manual adjudications of known regional coding errors
# ----------------------------------------------------------------

# Add any further adjudicated corrections here.
# This known record is Canadian and should not remain in an African category.
manual_region_overrides <- tribble(
  ~record_id,  ~region_group_override,
  "MXITFJ4G", "HIC / global"
)

regional <- regional |>
  left_join(manual_region_overrides, by = "record_id") |>
  mutate(
    region_group_original = region_group,
    region_group = coalesce(region_group_override, region_group)
  ) |>
  select(-region_group_override)


# ----------------------------------------------------------------
# 5. Read the detailed fields needed for the two paragraphs
# ----------------------------------------------------------------

effectiveness <- read_value_fields(
  "1a",
  c(
    primary_effect = "1a.1",
    effect_trajectory = "1a.3",
    uptake = "1a.4",
    completion = "1a.5",
    user_experience = "1a.8",
    subgroup_variation = "1a.12"
  )
)

design <- read_value_fields(
  "1b",
  c(
    modality = "1b.1",
    therapeutic_approach = "1b.2",
    session_dose = "1b.4",
    human_support = "1b.9",
    co_design = "1b.12",
    comparator = "1b.13"
  )
)

implementation <- read_value_fields(
  "1c",
  c(
    implementation_languages = "1c.1",
    implementation_connectivity = "1c.2",
    adaptation_method = "1c.5",
    implementation_ethics = "1c.10",
    scalability_lessons = "1c.11",
    detailed_implementation_contexts = "1c.12"
  )
)

safety <- read_value_fields(
  "1d",
  c(
    harms_reported = "1d.1",
    failure_modes = "1d.3",
    mitigation_strategies = "1d.5",
    crisis_protocol = "1d.6",
    post_launch_governance = "1d.8",
    equity_harms = "1d.10"
  )
)

product_landscape <- read_value_fields(
  "pl",
  c(
    comparator_products = "PL.1",
    entry_channels = "PL.2",
    product_safety_approaches = "PL.5",
    regulator_guidance = "PL.6",
    data_handling = "PL.7"
  )
)


# ----------------------------------------------------------------
# 6. Create one full evidence table
# ----------------------------------------------------------------

evidence <- regional |>
  left_join(effectiveness, by = "record_id") |>
  left_join(design, by = "record_id") |>
  left_join(implementation, by = "record_id") |>
  left_join(safety, by = "record_id") |>
  left_join(product_landscape, by = "record_id") |>
  mutate(
    citation = make_citation(bibliographic, title),

    # Prefer the detailed 1c extraction, while retaining the earlier narrative fields.
    connectivity_evidence = combine_fields(
      implementation_connectivity,
      device_connectivity,
      scalability_lessons
    ),
    language_evidence = combine_fields(
      implementation_languages,
      languages_supported,
      translation_adaptation
    ),
    adaptation_evidence = combine_fields(
      adaptation_method,
      cultural_adaptation,
      co_design
    ),
    context_evidence = combine_fields(
      detailed_implementation_contexts,
      implementation_contexts,
      human_support,
      scalability_lessons
    ),
    safety_guidance_evidence = combine_fields(
      implementation_ethics,
      harms_reported,
      failure_modes,
      mitigation_strategies,
      crisis_protocol,
      post_launch_governance,
      equity_harms,
      product_safety_approaches,
      regulator_guidance,
      data_handling
    ),

    chatbot_flag = str_detect(
      str_c(title, intervention_object, modality, comparator_products, sep = " | "),
      regex(
        "chatbot|conversational agent|relational agent|large language model|\\bLLM\\b|GPT",
        ignore_case = TRUE
      )
    ),

    rct_flag = str_detect(
      str_c(study_design, evidence_type, sep = " | "),
      regex("\\bRCTs?\\b|randomi[sz]ed|randomised|randomized", ignore_case = TRUE)
    ),

    adolescent_youth_flag = str_detect(
      replace_na(population_age, ""),
      regex(
        "adolesc|youth|young people|young adult|maps_10_19:\\s*True|maps_10_24:\\s*True",
        ignore_case = TRUE
      )
    ),

    single_session_flag = str_detect(
      str_c(session_dose, therapeutic_approach, intervention_object, sep = " | "),
      regex("single[- ]session|one[- ]session|single use|brief session", ignore_case = TRUE)
    ),

    follow_up_reported = is_present(effect_trajectory),
    comparator_reported = is_present(comparator),

    active_comparator_flag = str_detect(
      replace_na(comparator, ""),
      regex(
        "active|usual care|treatment as usual|digital|psychoeducation|static content|attention control",
        ignore_case = TRUE
      )
    ),

    low_bandwidth_flag = str_detect(
      replace_na(connectivity_evidence, ""),
      regex(
        paste(
          "offline", "low[- ]bandwidth", "low[- ]data", "data[- ]free",
          "zero[- ]rated", "feature phone", "SMS", "USSD", "WhatsApp",
          "data cost", "connectivity", "limited internet", "unstable internet",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),

    task_shifting_flag = str_detect(
      replace_na(context_evidence, ""),
      regex(
        paste(
          "task[- ]shift", "lay counsell", "lay worker", "community health",
          "peer", "coach", "nurse", "health worker", "facilitator",
          "human support", "supervision", "referral", "integrat",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),

    cultural_specificity_flag = str_detect(
      str_c(language_evidence, adaptation_evidence, sep = " | "),
      regex(
        paste(
          "local language", "translat", "cultur", "relig", "stigma",
          "family", "gender norm", "social norm", "idiom", "dialect",
          "vernacular", "localized", "localised",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    ),

    who_flag = str_detect(
      str_c(title, bibliographic, regulator_guidance, sep = " | "),
      regex("World Health Organization|\\bWHO\\b", ignore_case = TRUE)
    ),

    guidance_flag = str_detect(
      str_c(title, evidence_type, safety_guidance_evidence, sep = " | "),
      regex(
        "guidance|guideline|regulat|framework|governance|ethic|policy|WHO|World Health Organization",
        ignore_case = TRUE
      )
    ),

    multi_country_or_global_flag = str_detect(
      str_c(country_raw, mapped_iso3, title, evidence_type, sep = " | "),
      regex("multi-country|global|systematic review|scoping review|,", ignore_case = TRUE)
    )
  )


# ----------------------------------------------------------------
# 7. Regional-classification audit
# ----------------------------------------------------------------

ssa_iso3 <- c(
  "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD",
  "COM", "COD", "COG", "CIV", "DJI", "GNQ", "ERI", "SWZ", "ETH",
  "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "MDG",
  "MWI", "MLI", "MRT", "MUS", "MOZ", "NAM", "NER", "NGA", "RWA",
  "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA",
  "TGO", "UGA", "ZMB", "ZWE"
)

region_classification_review <- evidence |>
  mutate(
    has_ssa_country = map_lgl(mapped_iso3, has_any_iso, codes = ssa_iso3),
    possible_issue = case_when(
      region_group == "South Africa" &
        !str_detect(replace_na(mapped_iso3, ""), "\\bZAF\\b") ~
        "South Africa category but ZAF is absent from mapped_iso3",

      region_group == "Other SSA" & !has_ssa_country ~
        "Other SSA category but no SSA country is present in mapped_iso3",

      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(possible_issue)) |>
  select(
    record_id,
    title,
    country_raw,
    mapped_iso3,
    region_group_original,
    region_group,
    possible_issue
  )

write_csv(
  region_classification_review,
  file.path(output_dir, "region_classification_review.csv")
)

if (nrow(region_classification_review) > 0) {
  warning(
    nrow(region_classification_review),
    " record(s) have potentially inconsistent regional coding. Review: ",
    file.path(output_dir, "region_classification_review.csv")
  )
}


# ----------------------------------------------------------------
# 8. Other L&MIC evidence table and drafting description
# ----------------------------------------------------------------

other_lmic <- evidence |>
  filter(region_group == "Other L&MIC") |>
  mutate(
    macro_region = map_chr(mapped_iso3, tag_macro_regions),

    transferable_design_evidence = combine_fields(
      connectivity_evidence,
      human_support,
      context_evidence,
      co_design,
      entry_channels
    ),

    culture_bound_evidence = combine_fields(
      language_evidence,
      adaptation_evidence,
      subgroup_variation
    ),

    evidence_priority =
      3L * as.integer(replace_na(low_bandwidth_flag, FALSE)) +
      3L * as.integer(replace_na(task_shifting_flag, FALSE)) +
      2L * as.integer(replace_na(chatbot_flag, FALSE)) +
      1L * as.integer(replace_na(cultural_specificity_flag, FALSE)) +
      1L * as.integer(adolescent_youth_flag)
  ) |>
  arrange(macro_region, desc(evidence_priority), citation)

write_csv(
  other_lmic |>
    select(
      record_id,
      citation,
      title,
      country_raw,
      mapped_iso3,
      macro_region,
      population_age,
      study_design,
      intervention_object,
      chatbot_flag,
      low_bandwidth_flag,
      task_shifting_flag,
      cultural_specificity_flag,
      connectivity_evidence,
      human_support,
      context_evidence,
      language_evidence,
      adaptation_evidence,
      transferable_design_evidence,
      culture_bound_evidence,
      evidence_priority
    ),
  file.path(output_dir, "other_lmic_evidence_for_paragraph.csv")
)

other_lmic_counts_by_subregion <- other_lmic |>
  separate_rows(macro_region, sep = ";\\s*") |>
  count(macro_region, name = "n_records", sort = TRUE)

write_csv(
  other_lmic_counts_by_subregion,
  file.path(output_dir, "other_lmic_counts_by_subregion.csv")
)

other_lmic_region_phrase <- other_lmic_counts_by_subregion |>
  mutate(label = paste0(macro_region, " (", n_records, ")")) |>
  pull(label) |>
  paste(collapse = ", ")

connectivity_examples <- format_examples(
  other_lmic |> filter(low_bandwidth_flag),
  "connectivity_evidence",
  n = 4
)

task_shifting_examples <- format_examples(
  other_lmic |> filter(task_shifting_flag),
  "context_evidence",
  n = 4
)

chatbot_examples <- format_examples(
  other_lmic |> filter(chatbot_flag),
  "intervention_object",
  n = 5,
  width = 150
)

culture_examples <- format_examples(
  other_lmic |> filter(cultural_specificity_flag),
  "culture_bound_evidence",
  n = 4
)

other_lmic_paragraph <- paste0(
  "Other L&MIC evidence comprised ", nrow(other_lmic),
  " records, distributed across ",
  if_else(other_lmic_region_phrase == "", "unclear regional groupings", other_lmic_region_phrase),
  ". Connectivity or low-bandwidth considerations were identifiable in ",
  sum(other_lmic$low_bandwidth_flag, na.rm = TRUE),
  " records, including ", connectivity_examples, ". ",
  "Task-shifting, human support or integration with health and community systems appeared in ",
  sum(other_lmic$task_shifting_flag, na.rm = TRUE),
  " records, including ", task_shifting_examples, ". ",
  "Chatbot or conversational-agent deployment was represented by ",
  sum(other_lmic$chatbot_flag, na.rm = TRUE),
  " records, including ", chatbot_examples, ". ",
  "The most design-transferable lessons concern device and channel flexibility, low-data delivery, ",
  "clear human-support and referral arrangements, and formative co-design. Language, idioms, ",
  "social and gender norms, stigma, and locally framed content are more culture-bound and require ",
  "context-specific adaptation; examples included ", culture_examples, "."
)


# ----------------------------------------------------------------
# 9. HIC/global chatbot RCT and guidance tables
# ----------------------------------------------------------------

hic_global <- evidence |>
  filter(region_group == "HIC / global") |>
  mutate(
    population_directness = case_when(
      adolescent_youth_flag ~ "Adolescent/youth population represented",
      str_detect(
        replace_na(population_age, ""),
        regex("adult|university|college|clinical population", ignore_case = TRUE)
      ) ~ "Primarily adult, student or clinical population",
      TRUE ~ "Population directness unclear"
    ),

    comparator_directness = case_when(
      active_comparator_flag ~ "Active/usual-care comparator reported",
      comparator_reported ~ "Comparator reported but may be weak or indirect",
      TRUE ~ "Comparator not clearly reported in extraction"
    ),

    follow_up_directness = case_when(
      follow_up_reported ~ "Follow-up trajectory reported",
      TRUE ~ "Follow-up not clearly reported in extraction"
    ),

    dose_directness = case_when(
      single_session_flag ~ "Single-session or brief format identified",
      is_present(session_dose) ~ "Repeated or other dose described",
      TRUE ~ "Dose not clearly reported"
    ),

    mhaa_directness_score =
      3L * as.integer(adolescent_youth_flag) +
      2L * as.integer(single_session_flag) +
      1L * as.integer(active_comparator_flag) +
      1L * as.integer(follow_up_reported),

    mhaa_applicability_features = paste0(
      population_directness, "; ",
      comparator_directness, "; ",
      follow_up_directness, "; ",
      dose_directness
    )
  )

hic_global_chatbot_rcts <- hic_global |>
  filter(chatbot_flag, rct_flag) |>
  arrange(desc(mhaa_directness_score), citation)

write_csv(
  hic_global_chatbot_rcts |>
    select(
      record_id,
      citation,
      title,
      country_raw,
      mapped_iso3,
      genai_derived,
      population_age,
      study_design,
      intervention_object,
      modality,
      therapeutic_approach,
      session_dose,
      comparator,
      primary_effect,
      effect_trajectory,
      adolescent_youth_flag,
      single_session_flag,
      active_comparator_flag,
      follow_up_reported,
      mhaa_directness_score,
      mhaa_applicability_features
    ),
  file.path(output_dir, "hic_global_chatbot_rcts_for_paragraph.csv")
)

hic_global_guidance <- hic_global |>
  filter(
    who_flag |
      (guidance_flag & multi_country_or_global_flag)
  ) |>
  mutate(
    guidance_summary = combine_fields(
      regulator_guidance,
      implementation_ethics,
      mitigation_strategies,
      crisis_protocol,
      post_launch_governance,
      equity_harms,
      data_handling
    ),
    evidence_priority =
      4L * as.integer(who_flag) +
      2L * as.integer(is_present(regulator_guidance)) +
      1L * as.integer(is_present(post_launch_governance)) +
      1L * as.integer(is_present(equity_harms))
  ) |>
  arrange(desc(evidence_priority), citation)

write_csv(
  hic_global_guidance |>
    select(
      record_id,
      citation,
      title,
      country_raw,
      mapped_iso3,
      evidence_type,
      who_flag,
      multi_country_or_global_flag,
      regulator_guidance,
      implementation_ethics,
      harms_reported,
      mitigation_strategies,
      crisis_protocol,
      post_launch_governance,
      equity_harms,
      data_handling,
      guidance_summary,
      evidence_priority
    ),
  file.path(output_dir, "hic_global_guidance_for_paragraph.csv")
)

trial_examples <- format_trial_features(hic_global_chatbot_rcts, n = 5)

guidance_citations <- hic_global_guidance |>
  distinct(record_id, .keep_all = TRUE) |>
  slice_head(n = 6) |>
  pull(citation) |>
  paste(collapse = "; ")

if (guidance_citations == "") {
  guidance_citations <- "no WHO or multi-country guidance records were identifiable from the extracted fields"
}

guidance_examples <- format_examples(
  hic_global_guidance,
  "guidance_summary",
  n = 4,
  width = 260
)

hic_global_paragraph <- paste0(
  "GenAI and chatbot evaluation was concentrated in HIC/global evidence. Of ",
  nrow(hic_global), " HIC/global records, ",
  sum(hic_global$genai_derived, na.rm = TRUE),
  " were keyword-classified as GenAI-specific and ",
  nrow(hic_global_chatbot_rcts),
  " were identifiable chatbot RCT records. Among these RCT records, ",
  sum(hic_global_chatbot_rcts$adolescent_youth_flag, na.rm = TRUE),
  " represented adolescent or youth populations, ",
  sum(hic_global_chatbot_rcts$active_comparator_flag, na.rm = TRUE),
  " reported an active or usual-care comparator, ",
  sum(hic_global_chatbot_rcts$follow_up_reported, na.rm = TRUE),
  " reported a follow-up trajectory, and ",
  sum(hic_global_chatbot_rcts$single_session_flag, na.rm = TRUE),
  " were explicitly single-session or brief. Representative study features were ",
  trial_examples, ". Findings are more directly applicable to MHAA when studies use ",
  "adolescent samples, brief or single-session delivery, an informative comparator and follow-up ",
  "beyond immediate post-intervention; adult or clinical-only samples, repeated-use products, ",
  "waitlist-only comparisons and immediate-only assessment reduce directness. WHO or multi-country ",
  "regulatory and ethical guidance was identifiable in ", nrow(hic_global_guidance),
  " records, including ", guidance_citations, ". Extracted guidance addressed ",
  guidance_examples, "."
)


# ----------------------------------------------------------------
# 10. Write the auto-draft descriptions and a compact count summary
# ----------------------------------------------------------------

summary_lines <- c(
  "SECTION 3.3 AUTO-DRAFT DESCRIPTIONS",
  "===================================",
  "",
  "IMPORTANT:",
  "These are deterministic drafting aids assembled from extracted fields.",
  "Check the evidence CSVs and shorten/rephrase before inserting them into the report.",
  "",
  "OTHER L&MIC EVIDENCE",
  "---------------------",
  other_lmic_paragraph,
  "",
  "HIC AND GLOBAL EVIDENCE",
  "-----------------------",
  hic_global_paragraph,
  "",
  "KEY COUNTS",
  "----------",
  glue("Other L&MIC records: {nrow(other_lmic)}"),
  glue("Other L&MIC records with low-bandwidth/connectivity evidence: {sum(other_lmic$low_bandwidth_flag, na.rm = TRUE)}"),
  glue("Other L&MIC records with task-shifting/human-system integration evidence: {sum(other_lmic$task_shifting_flag, na.rm = TRUE)}"),
  glue("Other L&MIC chatbot/conversational-agent records: {sum(other_lmic$chatbot_flag, na.rm = TRUE)}"),
  glue("HIC/global records: {nrow(hic_global)}"),
  glue("HIC/global GenAI-specific records, keyword-derived: {sum(hic_global$genai_derived, na.rm = TRUE)}"),
  glue("HIC/global chatbot RCT records: {nrow(hic_global_chatbot_rcts)}"),
  glue("WHO or multi-country regulatory/ethical guidance records: {nrow(hic_global_guidance)}"),
  glue("Potential regional-classification issues requiring review: {nrow(region_classification_review)}")
)

writeLines(
  enc2utf8(summary_lines),
  file.path(output_dir, "section_3_3_auto_draft_descriptions.txt"),
  useBytes = TRUE
)

message("Done. Review outputs in: ", output_dir)
message("Primary drafting file: ", file.path(output_dir, "section_3_3_auto_draft_descriptions.txt"))
