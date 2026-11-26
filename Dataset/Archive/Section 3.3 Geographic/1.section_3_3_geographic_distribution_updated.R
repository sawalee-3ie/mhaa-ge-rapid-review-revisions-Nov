# ---------------------------------------------------------------
# Section 3.3 Geographic distribution
# Dataset-specific script for:
#   extraction_final-revised-SL_n=325.xlsx
#
# Workbook structure verified:
#   Index, baseline, 1a, 1b, 1c, 1d, 1e, sa, pl,
#   Review queue, Eval crosswalk, Codebook
#
# The current baseline sheet contains 319 analytical record IDs.
# All denominators below are calculated dynamically; none are hard-coded.
#
# Outputs:
#   Figure 3. Global distribution of included records
#   Figure 4. Sub-Saharan Africa close-up
#   Optional two-panel GenAI / other evidence map
#   Table 3. Regional distribution by review question / stream
#   Supporting CSVs for drafting the Section 3.3 narrative
# ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(scales)
library(glue)


# ---------------------------------------------------------------
# 1. File locations and constants
# ---------------------------------------------------------------

input_file <- "extraction_final-revised-SL_n=325.xlsx"
output_dir <- "section_3_3_outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

region_order <- c(
  "South Africa",
  "Other SSA",
  "Other L&MIC",
  "HIC / global"
)

gated_marker <- "— gated out —"


# ---------------------------------------------------------------
# 2. Helper functions
# ---------------------------------------------------------------

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

# Country strings sometimes distinguish the countries represented in the
# evidence from the country where a review was conducted or its authors were
# based. Remove the latter before generating the maps.
clean_country_text <- function(x) {
  x <- str_squish(as.character(x))
  x[x == ""] <- NA_character_

  x <- str_remove(
    x,
    regex(
      "^\\s*[A-Z]{3}\\s*\\((review|review conducted|author affiliation)\\)\\s*;\\s*",
      ignore_case = TRUE
    )
  )

  x <- str_remove(
    x,
    regex(
      ";\\s*(review conducted( from| in)?|study based( in)?).*?$",
      ignore_case = TRUE
    )
  )

  x <- str_remove(
    x,
    regex(";\\s*global review scope.*$", ignore_case = TRUE)
  )

  str_squish(x)
}

# A record counts as contributing to a review stream when at least one
# "... value" cell in that stream contains extracted content. The special
# 1c value "— gated out —" is not substantive extraction.
read_stream_membership <- function(sheet_name, stream_label) {
  dat <- read_excel(
    input_file,
    sheet = sheet_name,
    .name_repair = "minimal"
  ) |>
    mutate(
      record_id = str_squish(as.character(record_id)),
      title = as.character(title)
    ) |>
    filter(!is.na(record_id), record_id != "")

  value_cols <- names(dat)[str_ends(names(dat), " value")]

  if (length(value_cols) == 0) {
    stop("No value columns found in sheet: ", sheet_name)
  }

  dat |>
    mutate(
      substantive_stream_evidence = if_any(
        all_of(value_cols),
        ~ is_present_text(.x)
      )
    ) |>
    filter(substantive_stream_evidence) |>
    distinct(record_id, title) |>
    mutate(
      stream = stream_label,
      source_sheet = sheet_name
    )
}

pull_count <- function(summary_data, category) {
  result <- summary_data |>
    filter(region_group == category) |>
    pull(n)

  if (length(result) == 0) 0L else as.integer(result[[1]])
}


# ---------------------------------------------------------------
# 3. Read the verified baseline fields
# ---------------------------------------------------------------

baseline <- read_excel(
  input_file,
  sheet = "baseline",
  .name_repair = "minimal"
) |>
  transmute(
    record_id = str_squish(as.character(record_id)),
    title = as.character(title),
    bibliographic = as.character(`base.1 value`),
    country_raw = as.character(`base.2 value`),
    setting = as.character(`base.3 value`),
    population_age = as.character(`base.5 value`),
    study_design = as.character(`base.6 value`),
    intervention_object = as.character(`base.7 value`),
    review_questions_recorded = as.character(`base.8 value`),
    lmic_flag = as_bool(`base.10 value`),
    ssa_flag = as_bool(`base.11a value`),
    south_africa_flag = as_bool(`base.11b value`),
    evidence_type = as.character(`base.12 value`)
  ) |>
  filter(!is.na(record_id), record_id != "") |>
  distinct(record_id, .keep_all = TRUE) |>
  mutate(
    # This follows the hierarchy used in the earlier RQ 1c gap-map work.
    # The categories are mutually exclusive at record level.
    region_group = case_when(
      south_africa_flag == TRUE ~ "South Africa",
      ssa_flag == TRUE ~ "Other SSA",
      lmic_flag == TRUE ~ "Other L&MIC",
      TRUE ~ "HIC / global"
    ),
    region_group = factor(region_group, levels = region_order),
    country_for_map = clean_country_text(country_raw),

    # No dedicated GenAI field exists in the workbook. This transparent,
    # keyword-derived field is therefore an analysis aid and must be reviewed.
    genai_derived = str_detect(
      str_c(
        replace_na(title, ""),
        replace_na(intervention_object, ""),
        replace_na(evidence_type, ""),
        sep = " | "
      ),
      regex(
        paste(
          "generative AI",
          "generative artificial intelligence",
          "large language model",
          "\\bLLMs?\\b",
          "\\bGPT(?:-[0-9.]+)?\\b",
          "ChatGPT",
          "retrieval[- ]augmented generation",
          "\\bRAG[- ]based\\b",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    )
  )

n_analytical_records <- n_distinct(baseline$record_id)
message("Analytical records in baseline sheet: ", n_analytical_records)


# ---------------------------------------------------------------
# 4. Parse country codes from the actual base.2 country field
# ---------------------------------------------------------------

# Most populated base.2 cells already contain ISO-3 codes. A smaller number
# contain country names embedded in prose; the aliases below cover the forms
# observed in this workbook. Add rows here if the country-mapping audit reveals
# further named countries.
country_aliases <- tribble(
  ~pattern, ~country_iso3,
  "\\bUnited Kingdom\\b|\\bUK\\b|\\bScotland\\b|\\bNorthern Ireland\\b", "GBR",
  "\\bRepublic of Ireland\\b", "IRL",
  "\\bUnited States\\b|\\bU\\.?S\\.?\\b", "USA",
  "\\bChina\\b", "CHN",
  "\\bHong Kong\\b", "HKG",
  "\\bSouth Korea\\b", "KOR",
  "\\bArgentina\\b", "ARG",
  "\\bJapan\\b", "JPN",
  "\\bUkraine\\b", "UKR",
  "\\bEgypt\\b", "EGY",
  "\\bPhilippines\\b", "PHL",
  "\\bTaiwan\\b", "TWN",
  "\\bItaly\\b", "ITA",
  "\\bRomania\\b", "ROU",
  "\\bSingapore\\b", "SGP",
  "\\bBrazil\\b", "BRA",
  "\\bTurkey\\b|\\bTürkiye\\b", "TUR",
  "\\bAustralia\\b", "AUS",
  "\\bCanada\\b", "CAN",
  "\\bIndia\\b", "IND",
  "\\bBahrain\\b", "BHR",
  "\\bKazakhstan\\b", "KAZ",
  "\\bMalaysia\\b", "MYS",
  "\\bMorocco\\b", "MAR",
  "\\bPakistan\\b", "PAK",
  "\\bSri Lanka\\b", "LKA",
  "\\bGermany\\b", "DEU",
  "\\bAustria\\b", "AUT",
  "\\bSwitzerland\\b", "CHE",
  "\\bNorway\\b", "NOR",
  "\\bSweden\\b", "SWE",
  "\\bFinland\\b", "FIN",
  "\\bFrance\\b", "FRA",
  "\\bSpain\\b", "ESP",
  "\\bVietnam\\b", "VNM",
  "\\bIran\\b", "IRN",
  "\\bNepal\\b", "NPL",
  "\\bBangladesh\\b", "BGD",
  "\\bIndonesia\\b", "IDN",
  "\\bSaudi Arabia\\b", "SAU",
  "\\bUnited Arab Emirates\\b", "ARE",
  "\\bQatar\\b", "QAT",
  "\\bMexico\\b", "MEX",
  "\\bPeru\\b", "PER",
  "\\bChile\\b", "CHL",
  "\\bColombia\\b", "COL",
  "\\bSouth Africa\\b", "ZAF",
  "\\bKenya\\b", "KEN",
  "\\bNigeria\\b", "NGA",
  "\\bUganda\\b", "UGA",
  "\\bZambia\\b", "ZMB",
  "\\bMalawi\\b", "MWI",
  "\\bEthiopia\\b", "ETH"
)

valid_iso3 <- countrycode::codelist$iso3c |>
  na.omit() |>
  unique()

iso_code_hits <- baseline |>
  select(record_id, country_raw, country_for_map) |>
  mutate(
    country_iso3 = str_extract_all(
      str_to_upper(replace_na(country_for_map, "")),
      "\\b[A-Z]{3}\\b"
    )
  ) |>
  unnest_longer(country_iso3) |>
  mutate(
    # Scotland is occasionally coded separately in review descriptions;
    # map it to the UK country polygon.
    country_iso3 = recode(country_iso3, SCO = "GBR")
  ) |>
  filter(country_iso3 %in% valid_iso3) |>
  select(record_id, country_iso3)

named_country_hits <- baseline |>
  select(record_id, country_for_map) |>
  crossing(country_aliases) |>
  filter(
    str_detect(
      replace_na(country_for_map, ""),
      regex(pattern, ignore_case = TRUE)
    )
  ) |>
  select(record_id, country_iso3)

country_links <- bind_rows(
  iso_code_hits,
  named_country_hits
) |>
  distinct(record_id, country_iso3)

country_mapping_audit <- baseline |>
  select(record_id, title, country_raw, country_for_map) |>
  left_join(
    country_links |>
      group_by(record_id) |>
      summarise(
        parsed_iso3 = paste(sort(unique(country_iso3)), collapse = ", "),
        .groups = "drop"
      ),
    by = "record_id"
  ) |>
  mutate(
    mapping_status = case_when(
      is.na(country_raw) | str_squish(country_raw) == "" ~
        "Country not reported",
      is.na(parsed_iso3) | parsed_iso3 == "" ~
        "No mappable country parsed",
      TRUE ~ "Mapped"
    )
  )

write_csv(
  country_mapping_audit,
  file.path(output_dir, "section_3_3_country_mapping_audit.csv")
)

n_mapped_records <- n_distinct(country_links$record_id)
n_unmapped_records <- n_analytical_records - n_mapped_records
message("Records mapped to at least one country: ", n_mapped_records)
message("Records without a mappable country: ", n_unmapped_records)


# ---------------------------------------------------------------
# 5. Natural Earth geography and map-ready counts
# ---------------------------------------------------------------

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Natural Earth releases can contain several ISO-3 variables. Use the first
# available non--99 value in a stable order.
iso_candidates <- intersect(
  c("iso_a3_eh", "iso_a3", "adm0_a3"),
  names(world)
)

if (length(iso_candidates) == 0) {
  stop("No ISO-3 field found in the Natural Earth country data.")
}

world_attributes <- st_drop_geometry(world)
iso_vectors <- lapply(
  world_attributes[iso_candidates],
  function(x) na_if(as.character(x), "-99")
)
world$country_iso3 <- do.call(dplyr::coalesce, iso_vectors)

name_candidate <- intersect(
  c("name_long", "admin", "name"),
  names(world)
)[1]
world$country_name <- world[[name_candidate]]

world <- world |>
  select(
    country_name,
    continent,
    subregion,
    country_iso3,
    geometry
  )

country_counts <- country_links |>
  count(country_iso3, name = "n_records")

world_counts <- world |>
  left_join(country_counts, by = "country_iso3") |>
  mutate(n_records = replace_na(n_records, 0L))

unmatched_map_codes <- country_counts |>
  anti_join(
    world |>
      st_drop_geometry() |>
      distinct(country_iso3),
    by = "country_iso3"
  )

write_csv(
  unmatched_map_codes,
  file.path(output_dir, "section_3_3_iso3_not_in_natural_earth.csv")
)

write_csv(
  country_counts |>
    arrange(desc(n_records), country_iso3),
  file.path(output_dir, "section_3_3_country_counts.csv")
)

# ---------------------------------------------------------------
# 6. Figure 3: Global geographic distribution
# ---------------------------------------------------------------

# Sequential blue palette with greater contrast between lower,
# medium and higher record counts.
map_colours <- c(
  "#f4f7fb",
  "#dce7f3",
  "#b7cee4",
  "#83acd0",
  "#4f86ba",
  "#1f5f9f",
  "#08306b"
)


# The square-root transformation prevents countries with very high
# counts from making all lower-count countries appear almost identical.
global_fill_max <- max(
  world_counts$n_records,
  na.rm = TRUE
)


global_legend_breaks <- c(
  0,
  1,
  5,
  10,
  25,
  50,
  100
)

global_legend_breaks <- global_legend_breaks[
  global_legend_breaks <= global_fill_max
]


# Create one internal anchor point for every country with evidence.
# st_point_on_surface() places the point inside the country polygon.
global_label_points_sf <- suppressWarnings(
  world_counts |>
    filter(n_records > 0) |>
    st_point_on_surface()
)


global_label_coordinates <- st_coordinates(
  global_label_points_sf
)


global_label_points <- global_label_points_sf |>
  mutate(
    anchor_x = global_label_coordinates[, 1],
    anchor_y = global_label_coordinates[, 2]
  ) |>
  st_drop_geometry()


# Manually reposition labels in crowded areas.
#
# label_x and label_y are longitude and latitude coordinates.
# Add or amend rows here if additional labels need adjustment.
global_manual_positions <- tribble(
  ~country_iso3, ~label_x, ~label_y,
  
  # Western and Northern Europe
  "IRL", -14.0, 56.0,
  "GBR", -10.0, 52.5,
  "PRT", -12.0, 39.0,
  "ESP",  -7.0, 37.5,
  "FRA",  -4.0, 46.5,
  "BEL",  -2.0, 52.5,
  "NLD",   2.0, 55.0,
  "DEU",   8.5, 51.5,
  "DNK",  10.0, 58.5,
  "NOR",   8.0, 65.0,
  "SWE",  18.0, 63.0,
  "FIN",  28.0, 66.0,
  
  # Central, Eastern and Southern Europe
  "CHE",   4.0, 44.5,
  "AUT",  15.0, 46.5,
  "ITA",  14.0, 40.5,
  "CZE",  17.0, 50.0,
  "POL",  24.0, 53.0,
  "HUN",  22.0, 46.0,
  "ROU",  30.0, 45.0,
  "GRC",  27.0, 37.5,
  "UKR",  36.0, 50.0,
  "LTU",  29.0, 57.0,
  "LVA",  31.0, 60.0,
  "EST",  32.0, 63.0,
  "TUR",  39.0, 39.0,
  
  # Middle East and small Asian locations
  "ISR",  39.5, 31.0,
  "LBN",  37.0, 34.0,
  "BHR",  52.0, 26.0,
  "QAT",  55.0, 24.0,
  "HKG", 118.0, 23.0,
  "SGP", 108.0,  0.0
)


# Labels that have been manually repositioned
global_labels_manual <- global_label_points |>
  inner_join(
    global_manual_positions,
    by = "country_iso3"
  )


# Labels that remain inside their country polygons
global_labels_automatic <- global_label_points |>
  anti_join(
    global_manual_positions,
    by = "country_iso3"
  ) |>
  mutate(
    # Match the label colour to the darkness of the country fill.
    #
    # Countries above the threshold use white text.
    # Countries below the threshold use dark text.
    #
    # Reduce 0.45 if more labels should be white.
    # Increase 0.45 if fewer labels should be white.
    label_colour = if_else(
      sqrt(n_records) / sqrt(global_fill_max) >= 0.45,
      "white",
      "#111111"
    )
  )


figure_3 <- ggplot() +
  
  # Base world choropleth
  geom_sf(
    data = world_counts,
    aes(fill = n_records),
    colour = "#ffffff",
    linewidth = 0.20
  ) +
  
  # Emphasise South Africa with a red border
  geom_sf(
    data = world_counts |>
      filter(country_iso3 == "ZAF"),
    fill = NA,
    colour = "#c51b29",
    linewidth = 1.25
  ) +
  
  # Connector lines for labels moved outside their country polygons
  geom_segment(
    data = global_labels_manual,
    aes(
      x = anchor_x,
      y = anchor_y,
      xend = label_x,
      yend = label_y
    ),
    colour = "grey45",
    linewidth = 0.30,
    lineend = "round"
  ) +
  
  # Labels remaining inside country polygons:
  # white on darker fills and black on lighter fills
  geom_text(
    data = global_labels_automatic,
    aes(
      x = anchor_x,
      y = anchor_y,
      label = n_records,
      colour = label_colour
    ),
    size = 3.4,
    fontface = "bold"
  ) +
  
  # Manually displaced labels generally sit on the white background,
  # so they remain dark rather than using the adaptive colour rule.
  geom_text(
    data = global_labels_manual,
    aes(
      x = label_x,
      y = label_y,
      label = n_records
    ),
    colour = "#111111",
    size = 3.4,
    fontface = "bold"
  ) +
  
  # Use the colour values stored in label_colour directly.
  # Do not produce a separate text-colour legend.
  scale_colour_identity(
    guide = "none"
  ) +
  
  scale_fill_gradientn(
    colours = map_colours,
    trans = "sqrt",
    limits = c(
      0,
      global_fill_max
    ),
    breaks = global_legend_breaks,
    oob = scales::squish,
    name = "Included\nrecords",
    guide = guide_colourbar(
      title.position = "top",
      title.hjust = 0,
      barheight = grid::unit(
        52,
        "mm"
      ),
      barwidth = grid::unit(
        6,
        "mm"
      ),
      ticks.colour = "grey35",
      frame.colour = "grey75"
    )
  ) +
  
  coord_sf(
    xlim = c(
      -180,
      180
    ),
    ylim = c(
      -58,
      85
    ),
    expand = FALSE,
    clip = "off"
  ) +
  
  labs(
    title = "Global distribution of included evidence",
    
    subtitle = glue(
      "Country associations among {n_analytical_records} analytical records; ",
      "South Africa is outlined"
    ),
    
    x = NULL,
    y = NULL,
    
    caption = glue(
      "Counts are distinct records associated with each country using baseline field base.2. ",
      "Multi-country records may contribute to more than one country. ",
      "{n_unmapped_records} records had no mappable country; ",
      "see the country-mapping audit."
    )
  ) +
  
  theme_void(
    base_size = 12
  ) +
  
  theme(
    plot.title.position = "plot",
    
    plot.title = element_text(
      face = "bold",
      size = 17,
      colour = "#111111",
      margin = margin(
        b = 4
      )
    ),
    
    plot.subtitle = element_text(
      size = 11.5,
      colour = "grey30",
      margin = margin(
        b = 10
      )
    ),
    
    plot.caption = element_text(
      colour = "grey35",
      size = 9,
      hjust = 0,
      lineheight = 1.15,
      margin = margin(
        t = 10
      )
    ),
    
    legend.position = "right",
    
    legend.title = element_text(
      face = "bold",
      size = 11.5,
      lineheight = 1
    ),
    
    legend.text = element_text(
      size = 10.5,
      colour = "#222222"
    ),
    
    plot.margin = margin(
      t = 12,
      r = 18,
      b = 10,
      l = 12
    )
  )


figure_3


ggsave(
  filename = file.path(
    output_dir,
    "figure_3_global_distribution.png"
  ),
  plot = figure_3,
  width = 12,
  height = 6.8,
  dpi = 320,
  bg = "white"
)


ggsave(
  filename = file.path(
    output_dir,
    "figure_3_global_distribution.svg"
  ),
  plot = figure_3,
  width = 12,
  height = 6.8,
  bg = "white"
)


# ---------------------------------------------------------------
# 7. Figure 4: Sub-Saharan Africa close-up
# ---------------------------------------------------------------

ssa_world <- world_counts |>
  filter(
    continent == "Africa",
    subregion != "Northern Africa"
  )


# Retain this object because it is also used later when creating
# the other-SSA narrative-support dataset.
ssa_iso3 <- ssa_world$country_iso3 |>
  na.omit() |>
  unique()


ssa_fill_max <- max(
  ssa_world$n_records,
  na.rm = TRUE
)


ssa_legend_breaks <- c(
  0,
  1,
  5,
  10,
  25,
  50
)

ssa_legend_breaks <- ssa_legend_breaks[
  ssa_legend_breaks <= ssa_fill_max
]


ssa_label_points_sf <- suppressWarnings(
  ssa_world |>
    filter(n_records > 0) |>
    st_point_on_surface()
)


ssa_label_coordinates <- st_coordinates(
  ssa_label_points_sf
)


ssa_label_points <- ssa_label_points_sf |>
  mutate(
    anchor_x = ssa_label_coordinates[, 1],
    anchor_y = ssa_label_coordinates[, 2]
  ) |>
  st_drop_geometry()


# Reposition labels for small or tightly clustered SSA countries.
# Add or amend rows if the final figure reveals additional overlap.
ssa_manual_positions <- tribble(
  ~country_iso3, ~label_x, ~label_y,
  "GMB", -18.5,  14.0,
  "RWA",  34.5,  -0.5,
  "BDI",  35.5,  -4.0,
  "LSO",  31.5, -31.5,
  "SWZ",  34.5, -26.5
)


ssa_labels_manual <- ssa_label_points |>
  inner_join(
    ssa_manual_positions,
    by = "country_iso3"
  )


ssa_labels_automatic <- ssa_label_points |>
  anti_join(
    ssa_manual_positions,
    by = "country_iso3"
  ) |>
  mutate(
    # Adaptive text colour for labels located inside country polygons.
    label_colour = if_else(
      sqrt(n_records) / sqrt(ssa_fill_max) >= 0.45,
      "white",
      "#111111"
    )
  )


figure_4 <- ggplot() +
  
  # SSA choropleth
  geom_sf(
    data = ssa_world,
    aes(fill = n_records),
    colour = "#ffffff",
    linewidth = 0.38
  ) +
  
  # South Africa emphasis
  geom_sf(
    data = ssa_world |>
      filter(country_iso3 == "ZAF"),
    fill = NA,
    colour = "#c51b29",
    linewidth = 1.35
  ) +
  
  # Connector lines for displaced labels
  geom_segment(
    data = ssa_labels_manual,
    aes(
      x = anchor_x,
      y = anchor_y,
      xend = label_x,
      yend = label_y
    ),
    colour = "grey45",
    linewidth = 0.35,
    lineend = "round"
  ) +
  
  # Labels within country polygons:
  # white on dark fills and black on light fills
  geom_text(
    data = ssa_labels_automatic,
    aes(
      x = anchor_x,
      y = anchor_y,
      label = n_records,
      colour = label_colour
    ),
    size = 4,
    fontface = "bold"
  ) +
  
  # Manually displaced labels remain black
  geom_text(
    data = ssa_labels_manual,
    aes(
      x = label_x,
      y = label_y,
      label = n_records
    ),
    colour = "#111111",
    size = 4,
    fontface = "bold"
  ) +
  
  scale_colour_identity(
    guide = "none"
  ) +
  
  scale_fill_gradientn(
    colours = map_colours,
    trans = "sqrt",
    limits = c(
      0,
      ssa_fill_max
    ),
    breaks = ssa_legend_breaks,
    oob = scales::squish,
    name = "Included\nrecords",
    guide = guide_colourbar(
      title.position = "top",
      title.hjust = 0,
      barheight = grid::unit(
        48,
        "mm"
      ),
      barwidth = grid::unit(
        6,
        "mm"
      ),
      ticks.colour = "grey35",
      frame.colour = "grey75"
    )
  ) +
  
  coord_sf(
    xlim = c(
      -20,
      55
    ),
    ylim = c(
      -36,
      20
    ),
    expand = FALSE,
    clip = "off"
  ) +
  
  labs(
    title = "Distribution of included evidence in Sub-Saharan Africa",
    
    subtitle = paste0(
      "Country-level record counts; ",
      "South Africa is outlined"
    ),
    
    x = NULL,
    y = NULL,
    
    caption = paste0(
      "Countries without an overlaid number have no mapped records. ",
      "Multi-country records may contribute to more than one country."
    )
  ) +
  
  theme_void(
    base_size = 12
  ) +
  
  theme(
    plot.title.position = "plot",
    
    plot.title = element_text(
      face = "bold",
      size = 17,
      colour = "#111111",
      margin = margin(
        b = 4
      )
    ),
    
    plot.subtitle = element_text(
      size = 11.5,
      colour = "grey30",
      margin = margin(
        b = 10
      )
    ),
    
    plot.caption = element_text(
      colour = "grey35",
      size = 9,
      hjust = 0,
      lineheight = 1.15,
      margin = margin(
        t = 10
      )
    ),
    
    legend.position = "right",
    
    legend.title = element_text(
      face = "bold",
      size = 11.5,
      lineheight = 1
    ),
    
    legend.text = element_text(
      size = 10.5,
      colour = "#222222"
    ),
    
    plot.margin = margin(
      t = 12,
      r = 18,
      b = 10,
      l = 12
    )
  )


figure_4


ggsave(
  filename = file.path(
    output_dir,
    "figure_4_ssa_distribution.png"
  ),
  plot = figure_4,
  width = 8.8,
  height = 7.4,
  dpi = 320,
  bg = "white"
)


ggsave(
  filename = file.path(
    output_dir,
    "figure_4_ssa_distribution.svg"
  ),
  plot = figure_4,
  width = 8.8,
  height = 7.4,
  bg = "white"
)


# ---------------------------------------------------------------
# 8. Optional two-panel GenAI / other evidence map
# ---------------------------------------------------------------

# Export the keyword-derived GenAI classification for manual review.
# The workbook has no dedicated GenAI field, so this classification
# should not be reported without checking the exported file.
write_csv(
  baseline |>
    select(
      record_id,
      title,
      intervention_object,
      evidence_type,
      genai_derived,
      region_group
    ) |>
    arrange(
      desc(genai_derived),
      title
    ),
  file.path(
    output_dir,
    "section_3_3_genai_classification_for_review.csv"
  )
)


country_counts_genai <- country_links |>
  left_join(
    baseline |>
      select(
        record_id,
        genai_derived
      ),
    by = "record_id"
  ) |>
  mutate(
    genai_group = if_else(
      genai_derived,
      "GenAI-specific evidence (keyword-derived)",
      "Other evidence"
    )
  ) |>
  count(
    genai_group,
    country_iso3,
    name = "n_records"
  )


genai_group_order <- c(
  "GenAI-specific evidence (keyword-derived)",
  "Other evidence"
)


# Create one complete world-map dataset for each evidence group.
world_genai <- map(
  genai_group_order,
  function(current_group) {
    
    world |>
      left_join(
        country_counts_genai |>
          filter(
            genai_group == current_group
          ) |>
          select(
            country_iso3,
            n_records
          ),
        by = "country_iso3"
      ) |>
      mutate(
        genai_group = current_group,
        n_records = replace_na(
          n_records,
          0L
        )
      )
  }
) |>
  bind_rows() |>
  mutate(
    genai_group = factor(
      genai_group,
      levels = genai_group_order
    )
  )


genai_fill_max <- max(
  world_genai$n_records,
  na.rm = TRUE
)


genai_legend_breaks <- c(
  0,
  1,
  5,
  10,
  25,
  50,
  100
)

genai_legend_breaks <- genai_legend_breaks[
  genai_legend_breaks <= genai_fill_max
]


# Generate label anchor points separately within each panel.
genai_label_points_sf <- suppressWarnings(
  world_genai |>
    filter(n_records > 0) |>
    group_by(genai_group) |>
    st_point_on_surface() |>
    ungroup()
)


genai_label_coordinates <- st_coordinates(
  genai_label_points_sf
)


genai_label_points <- genai_label_points_sf |>
  mutate(
    anchor_x = genai_label_coordinates[, 1],
    anchor_y = genai_label_coordinates[, 2]
  ) |>
  st_drop_geometry()


genai_labels_manual <- genai_label_points |>
  inner_join(
    global_manual_positions,
    by = "country_iso3"
  )


genai_labels_automatic <- genai_label_points |>
  anti_join(
    global_manual_positions,
    by = "country_iso3"
  ) |>
  mutate(
    # Adaptive text colour for each panel.
    label_colour = if_else(
      sqrt(n_records) / sqrt(genai_fill_max) >= 0.45,
      "white",
      "#111111"
    )
  )


figure_3_genai <- ggplot() +
  
  # Map polygons for both panels
  geom_sf(
    data = world_genai,
    aes(fill = n_records),
    colour = "#ffffff",
    linewidth = 0.16
  ) +
  
  # South Africa outline in each panel
  geom_sf(
    data = world_genai |>
      filter(country_iso3 == "ZAF"),
    fill = NA,
    colour = "#c51b29",
    linewidth = 1
  ) +
  
  # Connector lines for manually displaced labels
  geom_segment(
    data = genai_labels_manual,
    aes(
      x = anchor_x,
      y = anchor_y,
      xend = label_x,
      yend = label_y
    ),
    colour = "grey45",
    linewidth = 0.25,
    lineend = "round"
  ) +
  
  # Labels within country polygons
  geom_text(
    data = genai_labels_automatic,
    aes(
      x = anchor_x,
      y = anchor_y,
      label = n_records,
      colour = label_colour
    ),
    size = 2.9,
    fontface = "bold"
  ) +
  
  # Manually displaced labels
  geom_text(
    data = genai_labels_manual,
    aes(
      x = label_x,
      y = label_y,
      label = n_records
    ),
    colour = "#111111",
    size = 2.9,
    fontface = "bold"
  ) +
  
  scale_colour_identity(
    guide = "none"
  ) +
  
  facet_wrap(
    ~ genai_group,
    ncol = 1
  ) +
  
  scale_fill_gradientn(
    colours = map_colours,
    trans = "sqrt",
    limits = c(
      0,
      genai_fill_max
    ),
    breaks = genai_legend_breaks,
    oob = scales::squish,
    name = "Included\nrecords",
    guide = guide_colourbar(
      title.position = "top",
      title.hjust = 0,
      barheight = grid::unit(
        48,
        "mm"
      ),
      barwidth = grid::unit(
        6,
        "mm"
      ),
      ticks.colour = "grey35",
      frame.colour = "grey75"
    )
  ) +
  
  coord_sf(
    xlim = c(
      -180,
      180
    ),
    ylim = c(
      -58,
      85
    ),
    expand = FALSE,
    clip = "off"
  ) +
  
  labs(
    title = "Global distribution of GenAI-specific and other evidence",
    
    subtitle = paste0(
      "The GenAI classification is keyword-derived and should be ",
      "manually verified"
    ),
    
    x = NULL,
    y = NULL,
    
    caption = paste0(
      "Counts are distinct records associated with each country. ",
      "Multi-country records may contribute to more than one country."
    )
  ) +
  
  theme_void(
    base_size = 12
  ) +
  
  theme(
    plot.title.position = "plot",
    
    strip.text = element_text(
      face = "bold",
      size = 11.5,
      colour = "#111111",
      margin = margin(
        t = 5,
        b = 7
      )
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 17,
      colour = "#111111",
      margin = margin(
        b = 4
      )
    ),
    
    plot.subtitle = element_text(
      size = 11.5,
      colour = "grey30",
      margin = margin(
        b = 8
      )
    ),
    
    plot.caption = element_text(
      colour = "grey35",
      size = 9,
      hjust = 0,
      lineheight = 1.15,
      margin = margin(
        t = 10
      )
    ),
    
    legend.position = "right",
    
    legend.title = element_text(
      face = "bold",
      size = 11.5,
      lineheight = 1
    ),
    
    legend.text = element_text(
      size = 10.5,
      colour = "#222222"
    ),
    
    plot.margin = margin(
      t = 12,
      r = 18,
      b = 10,
      l = 12
    )
  )


figure_3_genai


ggsave(
  filename = file.path(
    output_dir,
    "figure_3_global_distribution_genai_split.png"
  ),
  plot = figure_3_genai,
  width = 12,
  height = 9,
  dpi = 320,
  bg = "white"
)


# ---------------------------------------------------------------
# 9. Determine substantive evidence in each RQ / stream sheet
# ---------------------------------------------------------------

stream_sheets <- c(
  "RQ 1a. Effectiveness & engagement" = "1a",
  "RQ 1b. Intervention design" = "1b",
  "RQ 1c. Implementation" = "1c",
  "RQ 1d. Safety" = "1d",
  "RQ 1e. Measurement" = "1e",
  "Stream: SA context" = "sa",
  "Stream: Product / digital landscape" = "pl"
)

stream_order <- names(stream_sheets)

stream_membership <- imap_dfr(
  stream_sheets,
  function(sheet_name, stream_label) {
    read_stream_membership(sheet_name, stream_label)
  }
)

stream_denominators <- stream_membership |>
  count(stream, name = "n_records") |>
  complete(stream = stream_order, fill = list(n_records = 0)) |>
  arrange(match(stream, stream_order))

write_csv(
  stream_denominators,
  file.path(output_dir, "section_3_3_stream_denominators.csv")
)

message(
  "Substantive RQ 1c records in the current workbook: ",
  stream_denominators |>
    filter(stream == "RQ 1c. Implementation") |>
    pull(n_records)
)


# ---------------------------------------------------------------
# 10. Table 3: Regional distribution by RQ / stream
# ---------------------------------------------------------------

missing_baseline_ids <- stream_membership |>
  anti_join(baseline, by = "record_id")

write_csv(
  missing_baseline_ids,
  file.path(output_dir, "section_3_3_stream_ids_missing_from_baseline.csv")
)

table_3_counts <- stream_membership |>
  left_join(
    baseline |>
      select(record_id, region_group),
    by = "record_id"
  ) |>
  filter(!is.na(region_group)) |>
  distinct(record_id, stream, region_group) |>
  count(stream, region_group, name = "n") |>
  complete(
    stream = stream_order,
    region_group = region_order,
    fill = list(n = 0)
  )

table_3 <- table_3_counts |>
  pivot_wider(
    names_from = region_group,
    values_from = n,
    values_fill = 0
  ) |>
  mutate(
    total = `South Africa` + `Other SSA` + `Other L&MIC` + `HIC / global`,
    directness_note = case_when(
      `South Africa` > 0 ~ glue(
        "{`South Africa`} South African and {`Other SSA`} other-SSA records; ",
        "the remaining {`Other L&MIC` + `HIC / global`} records are geographically indirect."
      ),
      `Other SSA` > 0 ~ glue(
        "No South African records; {`Other SSA`} other-SSA records provide the closest regional evidence."
      ),
      `Other L&MIC` > 0 ~ glue(
        "No African records; transferability relies on {`Other L&MIC`} other-L&MIC records."
      ),
      TRUE ~ "Evidence is entirely HIC/global and requires substantial contextual interpretation."
    )
  ) |>
  arrange(match(stream, stream_order)) |>
  select(
    `Review question / stream` = stream,
    `South Africa (n)` = `South Africa`,
    `Other SSA (n)` = `Other SSA`,
    `Other L&MIC (n)` = `Other L&MIC`,
    `HIC / global (n)` = `HIC / global`,
    `Total (n)` = total,
    `Directness note` = directness_note
  )

print(table_3, n = Inf)

write_csv(
  table_3,
  file.path(output_dir, "table_3_regional_distribution_by_review_question.csv")
)

# Optional formatted HTML version if gt is installed.
if (requireNamespace("gt", quietly = TRUE)) {
  table_3_gt <- table_3 |>
    gt::gt() |>
    gt::tab_header(
      title = "Table 3. Regional distribution of evidence by review question"
    ) |>
    gt::cols_align(
      align = "center",
      columns = c(
        `South Africa (n)`,
        `Other SSA (n)`,
        `Other L&MIC (n)`,
        `HIC / global (n)`,
        `Total (n)`
      )
    ) |>
    gt::cols_align(
      align = "left",
      columns = c(`Review question / stream`, `Directness note`)
    )

  gt::gtsave(
    table_3_gt,
    file.path(output_dir, "table_3_regional_distribution_by_review_question.html")
  )
}


# ---------------------------------------------------------------
# 11. Overall regional spread and GenAI regional concentration
# ---------------------------------------------------------------

overall_region_summary <- baseline |>
  count(region_group, name = "n") |>
  complete(region_group = region_order, fill = list(n = 0)) |>
  mutate(
    percent = n / sum(n)
  ) |>
  arrange(match(region_group, region_order))

genai_region_summary <- baseline |>
  mutate(
    evidence_group = if_else(
      genai_derived,
      "GenAI-specific evidence (keyword-derived)",
      "Other evidence"
    )
  ) |>
  count(evidence_group, region_group, name = "n") |>
  group_by(evidence_group) |>
  mutate(percent_within_group = n / sum(n)) |>
  ungroup() |>
  arrange(evidence_group, match(region_group, region_order))

write_csv(
  overall_region_summary,
  file.path(output_dir, "section_3_3_overall_region_summary.csv")
)

write_csv(
  genai_region_summary,
  file.path(output_dir, "section_3_3_genai_region_summary.csv")
)


# ---------------------------------------------------------------
# 12. Supporting tables for the narrative paragraphs
# ---------------------------------------------------------------

# Read the fields most directly relevant to geographic interpretation.
implementation_support <- read_excel(
  input_file,
  sheet = "1c",
  .name_repair = "minimal"
) |>
  transmute(
    record_id = str_squish(as.character(record_id)),
    languages_supported = as.character(`1c.1 value`),
    device_connectivity = as.character(`1c.2 value`),
    cultural_adaptation = as.character(`1c.5 value`),
    implementation_contexts = as.character(`1c.12 value`)
  ) |>
  filter(!is.na(record_id), record_id != "") |>
  distinct(record_id, .keep_all = TRUE)

sa_support <- read_excel(
  input_file,
  sheet = "sa",
  .name_repair = "minimal"
) |>
  transmute(
    record_id = str_squish(as.character(record_id)),
    sa_language_coverage = as.character(`SA.3 value`),
    prior_sa_digital_mh = as.character(`SA.6 value`),
    sa_regulatory_context = as.character(`SA.7 value`)
  ) |>
  filter(!is.na(record_id), record_id != "") |>
  distinct(record_id, .keep_all = TRUE)

measurement_support <- read_excel(
  input_file,
  sheet = "1e",
  .name_repair = "minimal"
) |>
  transmute(
    record_id = str_squish(as.character(record_id)),
    psychometric_ssa_lmic = as.character(`1e.3 value`),
    translation_adaptation = as.character(`1e.9 value`)
  ) |>
  filter(!is.na(record_id), record_id != "") |>
  distinct(record_id, .keep_all = TRUE)

record_countries <- country_links |>
  group_by(record_id) |>
  summarise(
    mapped_iso3 = paste(sort(unique(country_iso3)), collapse = ", "),
    .groups = "drop"
  )

stream_flags <- stream_membership |>
  distinct(record_id, stream) |>
  mutate(value = TRUE) |>
  pivot_wider(
    names_from = stream,
    values_from = value,
    values_fill = FALSE,
    names_prefix = "has_"
  )

narrative_base <- baseline |>
  left_join(record_countries, by = "record_id") |>
  left_join(stream_flags, by = "record_id") |>
  left_join(implementation_support, by = "record_id") |>
  left_join(sa_support, by = "record_id") |>
  left_join(measurement_support, by = "record_id")

# South African evidence: include records flagged as South African and records
# whose country field explicitly maps to ZAF. The basis variable makes any
# discrepancy between the two transparent.
zaf_mapped_ids <- country_links |>
  filter(country_iso3 == "ZAF") |>
  pull(record_id) |>
  unique()

south_africa_records <- narrative_base |>
  filter(
    south_africa_flag == TRUE |
      record_id %in% zaf_mapped_ids
  ) |>
  mutate(
    south_africa_evidence_basis = case_when(
      south_africa_flag == TRUE & record_id %in% zaf_mapped_ids ~
        "SA flag TRUE and ZAF present in country mapping",
      south_africa_flag == TRUE ~
        "SA flag TRUE; ZAF not explicit in parsed country field",
      TRUE ~
        "ZAF present in parsed country field; SA flag not TRUE"
    )
  ) |>
  arrange(desc(country_raw == "ZAF"), title)

write_csv(
  south_africa_records,
  file.path(output_dir, "section_3_3_south_africa_records_for_narrative.csv")
)

# Other SSA evidence, prioritised mechanically for manual narrative review.
other_ssa_record_countries <- country_links |>
  filter(
    country_iso3 %in% setdiff(ssa_iso3, "ZAF")
  ) |>
  group_by(record_id) |>
  summarise(
    other_ssa_countries = paste(sort(unique(country_iso3)), collapse = ", "),
    .groups = "drop"
  )

other_ssa_records <- narrative_base |>
  inner_join(other_ssa_record_countries, by = "record_id") |>
  mutate(
    chatbot_or_ssi = str_detect(
      str_c(
        replace_na(title, ""),
        replace_na(intervention_object, ""),
        sep = " | "
      ),
      regex(
        "chatbot|conversational agent|digital SSI|single[- ]session intervention",
        ignore_case = TRUE
      )
    ),
    adolescent_or_youth = str_detect(
      str_c(
        replace_na(title, ""),
        replace_na(population_age, ""),
        sep = " | "
      ),
      regex("adolesc|youth|young people|young adult", ignore_case = TRUE)
    ),
    implementation_evidence = coalesce(
      .data[["has_RQ 1c. Implementation"]],
      FALSE
    )
  ) |>
  arrange(
    desc(chatbot_or_ssi),
    desc(adolescent_or_youth),
    desc(implementation_evidence),
    title
  )

write_csv(
  other_ssa_records,
  file.path(output_dir, "section_3_3_other_ssa_records_for_narrative.csv")
)

# Full record-level file for drafting the other-L&MIC and HIC/global paragraphs.
write_csv(
  narrative_base |>
    arrange(region_group, desc(genai_derived), title),
  file.path(output_dir, "section_3_3_records_by_region_for_narrative.csv")
)


# ---------------------------------------------------------------
# 13. Language coverage against the South African priority set
# ---------------------------------------------------------------

language_evidence <- narrative_base |>
  mutate(
    across(
      c(
        languages_supported,
        cultural_adaptation,
        sa_language_coverage,
        psychometric_ssa_lmic,
        translation_adaptation
      ),
      ~ replace_na(as.character(.x), "")
    ),
    language_text = str_c(
      languages_supported,
      cultural_adaptation,
      sa_language_coverage,
      psychometric_ssa_lmic,
      translation_adaptation,
      sep = " || "
    ),
    validation_adaptation_text = str_c(
      psychometric_ssa_lmic,
      translation_adaptation,
      sep = " || "
    )
  ) |>
  filter(str_detect(language_text, "\\S"))

priority_languages <- tribble(
  ~priority_language, ~pattern,
  "English", "\\bEnglish\\b",
  "isiZulu", "\\bisiZulu\\b|\\bZulu\\b",
  "isiXhosa", "\\bisiXhosa\\b|\\bXhosa\\b",
  "Afrikaans", "\\bAfrikaans\\b",
  "Sesotho", "\\bSesotho\\b|\\bSotho\\b"
)

priority_language_mentions <- language_evidence |>
  select(
    record_id,
    title,
    region_group,
    language_text,
    validation_adaptation_text
  ) |>
  crossing(priority_languages) |>
  filter(
    str_detect(
      language_text,
      regex(pattern, ignore_case = TRUE)
    )
  ) |>
  mutate(
    validation_or_adaptation_mentioned = str_detect(
      validation_adaptation_text,
      regex(pattern, ignore_case = TRUE)
    )
  ) |>
  select(-pattern)

priority_language_summary <- priority_language_mentions |>
  group_by(priority_language) |>
  summarise(
    records_mentioning_language = n_distinct(record_id),
    records_with_validation_or_adaptation_mention = n_distinct(
      record_id[validation_or_adaptation_mentioned]
    ),
    .groups = "drop"
  ) |>
  complete(
    priority_language = priority_languages$priority_language,
    fill = list(
      records_mentioning_language = 0,
      records_with_validation_or_adaptation_mention = 0
    )
  ) |>
  arrange(match(priority_language, priority_languages$priority_language))

# These are keyword mentions for review, not automatically verified claims of
# language support or validation.
write_csv(
  priority_language_mentions,
  file.path(output_dir, "section_3_3_priority_language_mentions_for_review.csv")
)

write_csv(
  priority_language_summary,
  file.path(output_dir, "section_3_3_priority_language_summary.csv")
)

write_csv(
  language_evidence,
  file.path(output_dir, "section_3_3_all_language_evidence_for_narrative.csv")
)


# ---------------------------------------------------------------
# 14. Quality-control audit of the regional flags
# ---------------------------------------------------------------

regional_flag_audit <- baseline |>
  mutate(
    flag_issue = case_when(
      south_africa_flag == TRUE &
        !str_detect(
          replace_na(country_raw, ""),
          regex("\\bZAF\\b|South Africa", ignore_case = TRUE)
        ) ~
        "South Africa flag TRUE but ZAF/South Africa is not explicit in base.2",

      ssa_flag == TRUE & lmic_flag == FALSE ~
        "SSA flag TRUE but L&MIC flag FALSE",

      south_africa_flag == TRUE & ssa_flag == FALSE ~
        "South Africa flag TRUE but SSA flag FALSE",

      south_africa_flag == TRUE & lmic_flag == FALSE ~
        "South Africa flag TRUE but L&MIC flag FALSE",

      is.na(lmic_flag) & is.na(ssa_flag) & is.na(south_africa_flag) ~
        "All geographic flags missing; classified as HIC/global by the stated rule",

      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(flag_issue)) |>
  select(
    record_id,
    title,
    country_raw,
    lmic_flag,
    ssa_flag,
    south_africa_flag,
    region_group,
    flag_issue
  )

write_csv(
  regional_flag_audit,
  file.path(output_dir, "section_3_3_regional_flag_audit.csv")
)


# ---------------------------------------------------------------
# 15. Drafting counts in a plain-text file
# ---------------------------------------------------------------

n_sa <- pull_count(overall_region_summary, "South Africa")
n_other_ssa <- pull_count(overall_region_summary, "Other SSA")
n_other_lmic <- pull_count(overall_region_summary, "Other L&MIC")
n_hic_global <- pull_count(overall_region_summary, "HIC / global")
n_genai <- sum(baseline$genai_derived, na.rm = TRUE)

summary_lines <- c(
  glue("Analytical records: {n_analytical_records}."),
  glue(
    "Mutually exclusive regional classification based on baseline flags: ",
    "South Africa {n_sa}; other SSA {n_other_ssa}; other L&MIC {n_other_lmic}; ",
    "HIC/global {n_hic_global}."
  ),
  glue(
    "Country mapping: {n_mapped_records} records mapped to at least one country; ",
    "{n_unmapped_records} had no mappable country."
  ),
  glue(
    "Keyword-derived GenAI classification: {n_genai} records. ",
    "Review section_3_3_genai_classification_for_review.csv before reporting this count."
  ),
  glue(
    "Current substantive RQ 1c denominator: ",
    "{stream_denominators |> filter(stream == 'RQ 1c. Implementation') |> pull(n_records)} records."
  ),
  "Country-level map counts and mutually exclusive regional table counts answer different questions and should not be summed or directly compared."
)

writeLines(
  summary_lines,
  file.path(output_dir, "section_3_3_drafting_counts.txt")
)

cat(paste(summary_lines, collapse = "\n"), "\n")


# ---------------------------------------------------------------
# End of script
# ---------------------------------------------------------------
