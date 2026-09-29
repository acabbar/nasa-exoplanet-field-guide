# The Exoplanet Field Guide
# Interactive dashboard built from a NASA Exoplanet Archive snapshot

library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(DT)

field_navy <- "#173B57"
field_teal <- "#5F9E95"
field_gold <- "#D1A448"
field_blue <- "#7699B2"
field_coral <- "#C87868"
field_dark <- "#183D3B"
field_ink <- "#384E50"
field_muted <- "#6F7871"
field_grid <- "#E7DED1"
field_plot_background <- "#FFFDF8"

data_file <- file.path("data", "nasa_exoplanets.csv")
snapshot_date <- "30 July 2026"
snapshot_cutoff <- "30 July"

if (!file.exists(data_file)) {
  stop(
    paste0(
      "Dataset not found at ", data_file, ". ",
      "Follow app/data/README.md to download and place the NASA snapshot."
    ),
    call. = FALSE
  )
}

archive_url <- paste0(
  "https://exoplanetarchive.ipac.caltech.edu/cgi-bin/TblView/",
  "nph-tblView?app=ExoTbls&config=PSCompPars"
)

kepler_2014_url <- paste0(
  "https://www.nasa.gov/news-release/",
  "nasas-kepler-mission-announces-a-planet-bonanza-715-new-worlds/"
)

kepler_2016_url <- paste0(
  "https://www.nasa.gov/news-release/",
  "nasas-kepler-mission-announces-largest-collection-of-planets-ever-discovered/"
)

selected_columns <- c(
  "pl_name",
  "hostname",
  "disc_year",
  "discoverymethod",
  "disc_facility",
  "disc_telescope",
  "sy_snum",
  "sy_pnum",
  "pl_orbper",
  "pl_orbsmax",
  "pl_orbeccen",
  "pl_eqt",
  "pl_insol",
  "pl_dens",
  "pl_rade",
  "pl_bmasse",
  "pl_bmassprov",
  "st_teff",
  "st_mass",
  "st_rad",
  "st_met",
  "sy_dist"
)

exoplanets <- suppressWarnings(
  fread(
    data_file,
    select = selected_columns,
    data.table = FALSE
  )
)

discoveries_by_year <- exoplanets %>%
  filter(!is.na(disc_year)) %>%
  count(disc_year, name = "planet_count") %>%
  mutate(
    year_status = ifelse(
      disc_year == 2026,
      "2026 year to date",
      "Complete year"
    )
  )

discovery_research_events <- discoveries_by_year %>%
  filter(disc_year %in% c(2014, 2016)) %>%
  mutate(
    announcement_label = case_when(
      disc_year == 2014 ~ "2014\n715 Kepler planets\nvalidated",
      disc_year == 2016 ~ "2016\n1,284 Kepler planets\nvalidated"
    ),
    label_height = case_when(
      disc_year == 2014 ~ 1160,
      disc_year == 2016 ~ 1820
    )
  )

method_counts <- exoplanets %>%
  count(discoverymethod, name = "planet_count", sort = TRUE)

top_methods <- method_counts %>%
  slice_head(n = 5)

other_methods <- method_counts %>%
  filter(!discoverymethod %in% top_methods$discoverymethod) %>%
  summarise(
    discoverymethod = "Other",
    planet_count = sum(planet_count)
  )

methods_overview <- bind_rows(top_methods, other_methods) %>%
  mutate(
    method_label = ifelse(
      discoverymethod == "Transit Timing Variations",
      "Transit Timing\nVariations",
      discoverymethod
    )
  )

size_band_levels <- c(
  "Smaller than Earth\n(< 1 Earth radius)",
  "1 to 2 Earth radii",
  "2 to 4 Earth radii",
  "4 to 10 Earth radii",
  "10 or more Earth radii"
)

radius_data <- exoplanets %>%
  filter(
    is.finite(pl_rade),
    pl_rade > 0
  ) %>%
  mutate(
    size_band = cut(
      pl_rade,
      breaks = c(-Inf, 1, 2, 4, 10, Inf),
      labels = size_band_levels,
      right = FALSE
    )
  )

size_band_counts <- radius_data %>%
  count(size_band, name = "planet_count", .drop = FALSE) %>%
  mutate(share = planet_count / sum(planet_count))

radius_median <- median(radius_data$pl_rade)

orbital_period_levels <- c(
  "Less than 1 Earth day",
  "1 to 10 Earth days",
  "10 to 100 Earth days",
  "100 days to 1 Earth year",
  "1 to 10 Earth years",
  "10 or more Earth years"
)

orbital_period_data <- exoplanets %>%
  filter(
    is.finite(pl_orbper),
    pl_orbper > 0
  ) %>%
  mutate(
    period_band = cut(
      pl_orbper,
      breaks = c(-Inf, 1, 10, 100, 365.25, 3652.5, Inf),
      labels = orbital_period_levels,
      right = FALSE
    )
  )

orbital_period_counts <- orbital_period_data %>%
  count(period_band, name = "planet_count", .drop = FALSE) %>%
  mutate(share = planet_count / sum(planet_count))

orbital_median <- median(orbital_period_data$pl_orbper)
orbital_shorter_earth_share <- mean(orbital_period_data$pl_orbper < 365.25)


distance_band_levels <- c(
  "Within 10\nlight-years",
  "10 to 100\nlight-years",
  "100 to 500\nlight-years",
  "500 to 1,000\nlight-years",
  "1,000 to 5,000\nlight-years",
  "5,000 or more\nlight-years"
)

distance_data <- exoplanets %>%
  filter(
    is.finite(sy_dist),
    sy_dist > 0
  ) %>%
  mutate(
    distance_light_years = sy_dist * 3.26156,
    distance_band = cut(
      distance_light_years,
      breaks = c(-Inf, 10, 100, 500, 1000, 5000, Inf),
      labels = distance_band_levels,
      right = FALSE
    )
  )

distance_band_counts <- distance_data %>%
  count(distance_band, name = "planet_count", .drop = FALSE) %>%
  mutate(share = planet_count / sum(planet_count))

distance_median <- median(distance_data$distance_light_years)

orbit_shape_levels <- c(
  "Nearly circular\n(0.0 to under 0.1)",
  "Slightly stretched\n(0.1 to under 0.3)",
  "Moderately stretched\n(0.3 to under 0.6)",
  "Highly stretched\n(0.6 to 1.0)"
)

orbit_shape_data <- exoplanets %>%
  filter(
    is.finite(pl_orbeccen),
    pl_orbeccen >= 0,
    pl_orbeccen <= 1
  ) %>%
  mutate(
    orbit_shape = cut(
      pl_orbeccen,
      breaks = c(-Inf, 0.1, 0.3, 0.6, Inf),
      labels = orbit_shape_levels,
      right = FALSE
    )
  )

orbit_shape_counts <- orbit_shape_data %>%
  count(orbit_shape, name = "planet_count", .drop = FALSE) %>%
  mutate(share = planet_count / sum(planet_count))

nearly_circular_share <- mean(orbit_shape_data$pl_orbeccen < 0.1)
earth_eccentricity <- 0.0167


earth_comparison_data <- exoplanets %>%
  filter(
    is.finite(pl_rade),
    pl_rade > 0,
    is.finite(pl_insol),
    pl_insol > 0,
    !is.na(discoverymethod)
  )

smallest_planet <- exoplanets %>%
  filter(
    is.finite(pl_rade),
    pl_rade > 0
  ) %>%
  arrange(pl_rade, pl_name) %>%
  slice(1)

largest_planet <- exoplanets %>%
  filter(
    is.finite(pl_rade),
    pl_rade > 0
  ) %>%
  arrange(desc(pl_rade), pl_name) %>%
  slice(1)

hottest_planet <- exoplanets %>%
  filter(
    is.finite(pl_eqt),
    pl_eqt > 0
  ) %>%
  arrange(desc(pl_eqt), pl_name) %>%
  slice(1)

shortest_year_planet <- exoplanets %>%
  filter(
    is.finite(pl_orbper),
    pl_orbper > 0
  ) %>%
  arrange(pl_orbper, pl_name) %>%
  slice(1)

closest_system <- exoplanets %>%
  filter(
    is.finite(sy_dist),
    sy_dist > 0
  ) %>%
  arrange(sy_dist, hostname, pl_name) %>%
  slice(1)

peak_complete_year <- discoveries_by_year %>%
  filter(disc_year < 2026) %>%
  arrange(desc(planet_count), disc_year) %>%
  slice(1)

transit_count <- method_counts %>%
  filter(discoverymethod == "Transit") %>%
  pull(planet_count)

transit_share <- transit_count / nrow(exoplanets)


system_multiplicity_counts <- exoplanets %>%
  filter(
    !is.na(hostname),
    is.finite(sy_pnum),
    sy_pnum > 0
  ) %>%
  group_by(hostname) %>%
  summarise(
    confirmed_planets = max(sy_pnum),
    .groups = "drop"
  ) %>%
  mutate(
    system_group = case_when(
      confirmed_planets == 1 ~ "1 confirmed planet",
      confirmed_planets == 2 ~ "2 confirmed planets",
      confirmed_planets >= 3 ~ "3 or more confirmed planets"
    ),
    system_group = factor(
      system_group,
      levels = c(
        "1 confirmed planet",
        "2 confirmed planets",
        "3 or more confirmed planets"
      )
    )
  ) %>%
  count(system_group, name = "system_count", .drop = FALSE) %>%
  mutate(share = system_count / sum(system_count))



accessible_plot_output <- function(output_id, height, alt_text) {
  div(
    class = "accessible-plot-wrapper",
    `data-alt` = alt_text,
    role = "group",
    `aria-label` = alt_text,
    plotOutput(output_id, height = height)
  )
}

accessible_plotly_output <- function(output_id, height, alt_text) {
  div(
    class = "accessible-plot-wrapper interactive-plot-wrapper",
    role = "group",
    `aria-label` = alt_text,
    tabindex = "0",
    plotly::plotlyOutput(output_id, height = height)
  )
}

field_theme <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = field_plot_background, color = NA),
      panel.background = element_rect(fill = field_plot_background, color = NA),
      panel.grid.minor = element_blank(),
      axis.title = element_text(color = field_ink),
      axis.text = element_text(color = field_muted)
    )
}

stat_card <- function(colour, label, output_id, note) {
  column(
    width = 3,
    div(
      class = paste("stat-card", colour),
      div(class = "stat-label", label),
      div(class = "stat-value", textOutput(output_id)),
      div(class = "stat-note", note)
    )
  )
}

summary_card <- function(label, value, note) {
  div(
    class = "feature-kpi-card",
    div(class = "feature-kpi-label", label),
    div(class = "feature-kpi-value", value),
    div(class = "feature-kpi-note", note)
  )
}


ui <- navbarPage(
  title = "The Exoplanet Field Guide",
  id = "main_navigation",
  inverse = TRUE,
  collapsible = TRUE,

  header = tags$head(
    tags$style(HTML("
      .filter-sidebar {
        position: sticky;
        top: 70px;
      }

      .filter-scope {
        border-top: 1px solid #e6ecef;
        color: #667985;
        font-size: 12px;
        line-height: 1.5;
        margin-top: 18px;
        padding-top: 16px;
      }

      .filter-scope strong {
        color: #425b69;
      }

      .record-section {
        margin-bottom: 24px;
      }

      .record-section h3 {
        color: #0e2c3e;
        font-size: 20px;
        font-weight: bold;
        margin: 0 0 6px;
      }

      .record-description {
        color: #667985;
        font-size: 13px;
        margin-bottom: 16px;
      }

      .record-grid {
        display: grid;
        gap: 16px;
        grid-template-columns: repeat(5, minmax(0, 1fr));
      }

      .record-card {
        background-color: white;
        border-radius: 8px;
        box-shadow: 0 3px 10px rgba(8, 31, 45, 0.08);
        min-height: 215px;
        padding: 18px;
      }

      .record-label {
        color: #667985;
        font-size: 10px;
        font-weight: bold;
        letter-spacing: 0.6px;
        text-transform: uppercase;
      }

      .record-name {
        color: #0e2c3e;
        font-size: 16px;
        font-weight: bold;
        margin: 10px 0 6px;
      }

      .record-value {
        color: #173042;
        font-size: 18px;
        font-weight: bold;
        line-height: 1.3;
      }

      .record-note {
        color: #7a8d98;
        font-size: 11px;
        line-height: 1.4;
        margin-top: 8px;
      }

      .filter-summary {
        background-color: #eaf7f5;
        border-left: 4px solid #55c7b5;
        border-radius: 4px;
        color: #173042;
        font-weight: bold;
        margin-top: 12px;
        padding: 12px 14px;
      }

      .filter-summary p {
        margin: 0;
      }

      .filter-summary p + p {
        margin-top: 8px;
      }

      .table-card {
        overflow: hidden;
      }

      .dataTables_wrapper {
        color: #425b69;
        font-size: 12px;
      }

      .dataTables_wrapper input,
      .dataTables_wrapper select {
        border: 1px solid #cfdde3;
        border-radius: 4px;
        padding: 5px 7px;
      }

      table.dataTable tbody tr.selected {
        background-color: #dff3ef !important;
      }

      .planet-detail-card {
        border-top: 4px solid #55c7b5;
      }

      .detail-grid {
        display: grid;
        gap: 12px;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        margin-top: 16px;
      }

      .detail-item {
        background-color: #f4f7f9;
        border-radius: 6px;
        min-height: 86px;
        padding: 14px;
      }

      .detail-label {
        color: #667985;
        font-size: 10px;
        font-weight: bold;
        letter-spacing: 0.5px;
        text-transform: uppercase;
      }

      .detail-value {
        color: #173042;
        font-size: 15px;
        font-weight: bold;
        line-height: 1.35;
        margin-top: 6px;
      }

      .detail-note {
        color: #667985;
        font-size: 12px;
        margin-top: 8px;
      }

      .conclusion-grid {
        display: grid;
        gap: 16px;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        margin-bottom: 20px;
      }

      .conclusion-card {
        background-color: white;
        border-radius: 8px;
        box-shadow: 0 3px 10px rgba(8, 31, 45, 0.08);
        min-height: 180px;
        padding: 22px;
      }

      .conclusion-label {
        color: #667985;
        font-size: 11px;
        font-weight: bold;
        letter-spacing: 0.7px;
        text-transform: uppercase;
      }

      .conclusion-value {
        color: #0e2c3e;
        font-size: 28px;
        font-weight: bold;
        margin: 8px 0 4px;
      }

      .conclusion-card h3 {
        color: #173042;
        font-size: 17px;
        font-weight: bold;
        margin: 0 0 8px;
      }

      .conclusion-card p,
      .conclusion-panel li {
        color: #667985;
        font-size: 13px;
        line-height: 1.55;
      }

      .conclusion-panel {
        min-height: 235px;
      }

      .conclusion-panel ul {
        padding-left: 20px;
      }

      .conclusion-panel li + li {
        margin-top: 9px;
      }

      .conclusion-banner {
        background-color: #0e2c3e;
        border-left: 6px solid #55c7b5;
        border-radius: 8px;
        color: white;
        margin-bottom: 24px;
        padding: 24px;
      }

      .conclusion-banner h3 {
        font-size: 20px;
        font-weight: bold;
        margin: 0 0 8px;
      }

      .conclusion-banner p {
        color: #d9e7ed;
        font-size: 14px;
        line-height: 1.6;
        margin: 0;
      }

      @media (max-width: 991px) {
        .filter-sidebar {
          position: static;
        }

        .record-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }

        .detail-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
      }

      @media (max-width: 600px) {
        .record-grid {
          grid-template-columns: 1fr;
        }

        .detail-grid,
        .conclusion-grid {
          grid-template-columns: 1fr;
        }
      }

      /* Field Guide theme */
      html { scroll-behavior: smooth; }

      body {
        background: #f6f0e6;
        color: #263d45;
        font-family: 'Segoe UI', Arial, sans-serif;
      }

      .navbar-inverse {
        background: #f6f0e6;
        border: 0;
        border-bottom: 1px solid #d8cbb8;
        box-shadow: none;
        margin-bottom: 0;
        position: sticky;
        top: 0;
        z-index: 1000;
      }

      .navbar-inverse .navbar-brand {
        color: #183d3b;
        font-family: Georgia, 'Times New Roman', serif;
        font-size: 19px;
        font-weight: bold;
      }

      .navbar-inverse .navbar-nav > li > a {
        color: #62716d;
        font-size: 13px;
        margin: 8px 2px;
        padding: 8px 12px;
      }

      .navbar-inverse .navbar-nav > li > a:hover { color: #183d3b; }

      .navbar-inverse .navbar-nav > .active > a,
      .navbar-inverse .navbar-nav > .active > a:hover {
        background: #183d3b;
        border-radius: 4px;
        color: #fffaf1;
      }

      .overview-container {
        margin: 0 auto;
        max-width: 1280px;
        padding: 38px 24px 52px;
      }

      .intro-panel {
        background:
          radial-gradient(circle at 86% 16%, rgba(255,255,255,0.8) 0 2px, transparent 3px),
          radial-gradient(circle at 74% 78%, rgba(255,255,255,0.7) 0 1px, transparent 2px),
          #1e5552;
        border: 0;
        border-radius: 2px 34px 2px 34px;
        box-shadow: 10px 10px 0 #e1d3bf;
        color: #fffaf1;
        margin-bottom: 24px;
        min-height: 265px;
        overflow: hidden;
        padding: 48px 410px 44px 44px;
        position: relative;
      }

      .intro-panel::before {
        background: linear-gradient(145deg, #efc875 0%, #d47855 48%, #864f5d 100%);
        border: 5px solid rgba(255,255,255,0.25);
        border-radius: 50%;
        box-shadow: inset -18px -15px 0 rgba(67,45,68,0.18);
        content: '';
        height: 148px;
        position: absolute;
        right: 125px;
        top: 54px;
        width: 148px;
      }

      .intro-panel::after {
        border: 12px solid rgba(245, 221, 166, 0.78);
        border-left-color: transparent;
        border-radius: 50%;
        content: '';
        height: 55px;
        position: absolute;
        right: 70px;
        top: 98px;
        transform: rotate(-14deg);
        width: 255px;
      }

      .intro-panel h1 {
        color: #fffaf1;
        font-family: Georgia, 'Times New Roman', serif;
        font-size: 41px;
        font-weight: normal;
        letter-spacing: -0.6px;
        line-height: 1.08;
        margin-top: 0;
        position: relative;
        z-index: 2;
      }

      .intro-panel p {
        color: #dbe9e4;
        font-family: Georgia, 'Times New Roman', serif;
        font-size: 17px;
        line-height: 1.6;
        max-width: 850px;
        position: relative;
        z-index: 2;
      }

      .source-text {
        color: #b9d1cb;
        font-family: 'Segoe UI', Arial, sans-serif;
        font-size: 12px;
        margin-top: 16px;
        position: relative;
        z-index: 2;
      }

      .source-text a { color: #f4d895; font-weight: 600; text-decoration: none; }

      .section-heading {
        border-bottom: 1px solid #d8cbb8;
        margin-bottom: 20px;
        padding-bottom: 14px;
      }

      .section-heading h2,
      .record-section h3,
      .chart-card h3,
      .filter-card h3,
      .conclusion-banner h3 {
        color: #183d3b;
        font-family: Georgia, 'Times New Roman', serif;
      }

      .section-heading h2 {
        font-size: 34px;
        font-weight: normal;
        letter-spacing: -0.4px;
        margin: 0 0 8px;
      }

      .section-heading p { color: #6f7871; font-size: 15px; }

      .stat-card {
        border: 0;
        border-radius: 2px 18px 2px 18px;
        box-shadow: none;
        margin-bottom: 20px;
        min-height: 145px;
        padding: 22px;
        position: relative;
      }

      .stat-card.turquoise-card { background: #dcece6; border-top: 0; }
      .stat-card.gold-card { background: #f3e3b8; border-top: 0; }
      .stat-card.blue-card { background: #dce7ed; border-top: 0; }
      .stat-card.rose-card { background: #efd9d2; border-top: 0; }

      .stat-card::after {
        color: rgba(38,61,69,0.22);
        content: '✦';
        font-family: Georgia, serif;
        font-size: 35px;
        position: absolute;
        right: 19px;
        top: 16px;
      }

      .stat-label {
        color: #53645e;
        font-size: 12px;
        font-weight: bold;
        letter-spacing: 1px;
        text-transform: uppercase;
      }

      .stat-value {
        color: #183d3b;
        font-family: Georgia, 'Times New Roman', serif;
        font-size: 35px;
        font-weight: normal;
        margin: 8px 0 4px;
      }

      .stat-note { color: #6f7871; font-size: 12px; }

      .chart-card,
      .filter-card,
      .record-card,
      .conclusion-card {
        background: #fffdf8;
        border: 1px solid #d9cdbb;
        border-radius: 2px 18px 2px 18px;
        box-shadow: 5px 5px 0 rgba(209, 194, 173, 0.45);
      }

      .chart-card { margin-bottom: 20px; padding: 28px; }
      .chart-card h3 { font-size: 22px; font-weight: normal; margin-top: 0; }
      .chart-description {
        color: #74786f;
        font-size: 13px;
        margin-bottom: 14px;
      }
      .guide-text {
        color: #74786f;
        font-size: 13px;
        line-height: 1.6;
      }
      .record-description { color: #74786f; }

      .filter-card {
        background: #f0e6d6;
        border-color: #cabba4;
        box-shadow: none;
        margin-bottom: 20px;
        padding: 22px;
      }

      .filter-card h3 {
        font-size: 21px;
        font-weight: normal;
        margin: 0 0 22px;
      }

      .filter-summary {
        background: #fffaf0;
        border: 1px solid #d6c5a9;
        border-left: 4px solid #c57a4e;
        border-radius: 2px 10px 2px 10px;
        color: #384e50;
      }

      .reset-button {
        background: #183d3b;
        border: 0;
        border-radius: 2px 10px 2px 10px;
        color: #fffaf1;
        font-family: Georgia, 'Times New Roman', serif;
        margin-top: 5px;
        padding: 8px 14px;
        width: 100%;
      }

      .reset-button:hover,
      .reset-button:focus { background: #2b6661; color: white; }

      .download-label {
        align-items: center;
        color: #53645e;
        display: flex;
        font-size: 11px;
        font-weight: 700;
        gap: 6px;
        margin: 13px 0 7px;
        text-transform: uppercase;
      }

      .download-button,
      .download-button:visited {
        background: #173b57;
        border: 0;
        border-radius: 2px 10px 2px 10px;
        color: #fffaf1;
        display: block;
        font-family: Georgia, 'Times New Roman', serif;
        margin-bottom: 14px;
        padding: 8px 14px;
        text-align: center;
        text-decoration: none;
        width: 100%;
      }

      .download-button:hover,
      .download-button:focus {
        background: #285a7a;
        color: white;
        text-decoration: none;
      }

      .earth-comparison-card { margin-top: 8px; }

      .earth-comparison-controls {
        background: #f0e6d6;
        border: 1px solid #cabba4;
        border-radius: 2px 14px 2px 14px;
        min-height: 405px;
        padding: 18px;
      }

      .earth-comparison-controls input[type='checkbox'] {
        accent-color: #183d3b;
      }

      .earth-comparison-controls .filter-summary {
        font-size: 12px;
        line-height: 1.5;
        margin-top: 18px;
      }

      .earth-caveat {
        color: #6f7871;
        font-size: 11px;
        line-height: 1.5;
        margin-top: 15px;
      }

      .interactive-plot-wrapper {
        outline: none;
      }

      .interactive-plot-wrapper:focus {
        box-shadow: 0 0 0 3px rgba(23, 59, 87, 0.18);
      }

      .interactive-plot-wrapper .modebar-btn path {
        fill: #183d3b !important;
      }

      .feature-kpi-grid {
        display: grid;
        gap: 14px;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        margin-bottom: 20px;
      }

      .feature-kpi-card {
        background: #fffdf8;
        border: 1px solid #d9cdbb;
        border-radius: 2px 14px 2px 14px;
        min-height: 118px;
        padding: 17px;
      }

      .feature-kpi-card:nth-child(1) { background: #dcece6; }
      .feature-kpi-card:nth-child(2) { background: #f3e3b8; }
      .feature-kpi-card:nth-child(3) { background: #dce7ed; }
      .feature-kpi-card:nth-child(4) { background: #efd9d2; }

      .feature-kpi-label {
        color: #53645e;
        font-size: 10px;
        font-weight: 700;
        letter-spacing: 0.8px;
        text-transform: uppercase;
      }

      .feature-kpi-value {
        color: #183d3b;
        font-family: Georgia, 'Times New Roman', serif;
        font-size: 24px;
        line-height: 1.1;
        margin: 8px 0 5px;
      }

      .feature-kpi-note {
        color: #6f7871;
        font-size: 11px;
        line-height: 1.35;
      }

      .overview-chart-row,
      .overview-feature-row {
        align-items: stretch;
        display: flex;
      }

      .overview-chart-row > [class*='col-'],
      .overview-feature-row > [class*='col-'] {
        display: flex;
      }

      .overview-chart-row .chart-card,
      .overview-feature-row .chart-card,
      .overview-feature-row .explore-guide-card {
        flex: 1;
        width: 100%;
      }

      .overview-feature-row {
        margin-top: 8px;
      }

      .explore-guide-card {
        background: #fffdf8;
        border: 1px solid #d9cdbb;
        border-radius: 2px 18px 2px 18px;
        box-shadow: 5px 5px 0 rgba(209, 194, 173, 0.45);
        display: flex;
        flex-direction: column;
        margin-bottom: 20px;
        padding: 28px;
      }

      .explore-guide-card h3 {
        color: #183d3b;
        font-family: Georgia, 'Times New Roman', serif;
        font-size: 22px;
        font-weight: normal;
        margin: 0 0 8px;
      }

      .explore-guide-card p,
      .explore-guide-card li {
        color: #6f7871;
        font-size: 13px;
        line-height: 1.55;
      }

      .explore-guide-list {
        list-style: none;
        margin: 8px 0 18px;
        padding: 0;
      }

      .explore-guide-list li {
        border-bottom: 1px solid #e8dece;
        padding: 9px 0 9px 34px;
        position: relative;
      }

      .explore-guide-list li::before {
        background: #183d3b;
        border-radius: 50%;
        color: #fffaf1;
        content: counter(list-item);
        font-family: Georgia, 'Times New Roman', serif;
        height: 22px;
        left: 0;
        line-height: 22px;
        position: absolute;
        text-align: center;
        top: 8px;
        width: 22px;
      }

      .explore-route {
        background: #dcece6;
        border-left: 4px solid #5f9e95;
        border-radius: 2px 10px 2px 10px;
        color: #384e50;
        font-size: 12px;
        line-height: 1.5;
        margin-top: auto;
        padding: 13px 15px;
      }

      .shiny-options-group input[type='radio'] {
        accent-color: #183d3b;
      }

      .irs-bar,
      .irs-bar-edge,
      .irs--shiny .irs-bar {
        background: #183d3b !important;
        border-color: #183d3b !important;
      }

      .irs-from,
      .irs-to,
      .irs-single {
        background: #183d3b !important;
        color: #fffaf1 !important;
      }

      .irs-from::after,
      .irs-to::after,
      .irs-single::after {
        border-top-color: #183d3b !important;
      }

      .irs-slider,
      .irs--shiny .irs-handle {
        border-color: #183d3b !important;
      }

      .record-grid { gap: 18px; }

      .record-card {
        box-shadow: 4px 4px 0 rgba(209, 194, 173, 0.5);
        min-height: 225px;
        transition: transform 0.2s ease;
      }

      .record-card:hover { transform: translateY(-4px) rotate(-0.4deg); }
      .record-name, .record-value { color: #183d3b; font-family: Georgia, 'Times New Roman', serif; }
      .record-name { font-size: 17px; }

      .record-card.turquoise-card { background: #dcece6; border-top: 0; }
      .record-card.gold-card { background: #f3e3b8; border-top: 0; }
      .record-card.blue-card { background: #dce7ed; border-top: 0; }
      .record-card.rose-card { background: #efd9d2; border-top: 0; }

      .detail-item {
        border: 1px solid #e1d5c3;
        border-radius: 2px 10px 2px 10px;
      }

      .detail-item:nth-child(4n + 1) { background: #dcece6; }
      .detail-item:nth-child(4n + 2) { background: #f3e3b8; }
      .detail-item:nth-child(4n + 3) { background: #dce7ed; }
      .detail-item:nth-child(4n) { background: #efd9d2; }

      .detail-value { color: #183d3b; }

      .conclusion-card.turquoise-card { background: #dcece6; border-top: 0; }
      .conclusion-card.gold-card { background: #f3e3b8; border-top: 0; }
      .conclusion-card.blue-card { background: #dce7ed; border-top: 0; }
      .conclusion-card.rose-card { background: #efd9d2; border-top: 0; }

      .conclusion-panel.turquoise-card { background: #dcece6; border-top: 0; }
      .conclusion-panel.rose-card { background: #efd9d2; border-top: 0; }

      .conclusion-card h3,
      .conclusion-value {
        color: #183d3b;
        font-family: Georgia, 'Times New Roman', serif;
      }

      .conclusion-banner {
        background: #1e5552;
        border: 0;
        border-left: 8px solid #e7bd65;
        border-radius: 2px 20px 2px 20px;
      }

      .conclusion-banner h3 { color: #fff7e8; font-size: 23px; }
      .conclusion-banner p { color: #dcebe5; }

      .dashboard-footer {
        background: #183d3b;
        color: #bdd1ca;
        font-size: 12px;
        padding: 22px 24px;
      }

      .footer-inner {
        display: flex;
        justify-content: space-between;
        margin: 0 auto;
        max-width: 1230px;
      }

      .dashboard-footer a { color: #f0cf87; font-weight: 600; }

      @media (max-width: 991px) {
        .intro-panel { padding: 40px 260px 40px 30px; }
        .intro-panel::before { right: 70px; }
        .intro-panel::after { right: 18px; }
        .earth-comparison-controls { min-height: auto; }
        .feature-kpi-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
        .overview-chart-row,
        .overview-feature-row { display: block; }
        .overview-chart-row > [class*='col-'],
        .overview-feature-row > [class*='col-'] { display: block; width: 100%; }
      }

      @media (max-width: 700px) {
        .intro-panel { min-height: 430px; padding: 30px 23px 220px; }
        .intro-panel h1 { font-size: 34px; }
        .intro-panel::before { bottom: 40px; right: calc(50% - 74px); top: auto; }
        .intro-panel::after { bottom: 85px; right: calc(50% - 128px); top: auto; }
        .footer-inner { flex-direction: column; gap: 8px; }
        .feature-kpi-grid { grid-template-columns: 1fr; }
      }
    ")),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function () {
        function applyPlotDescriptions(root) {
          var wrappers = root.querySelectorAll
            ? root.querySelectorAll('.accessible-plot-wrapper')
            : [];

          wrappers.forEach(function (wrapper) {
            var description = wrapper.getAttribute('data-alt');
            var image = wrapper.querySelector('img');

            if (description && image) {
              image.setAttribute('alt', description);
              image.setAttribute('aria-label', description);
            }
          });
        }

        var plotObserver = new MutationObserver(function () {
          applyPlotDescriptions(document);
        });

        plotObserver.observe(document.body, {
          childList: true,
          subtree: true
        });

        applyPlotDescriptions(document);

        // Open every main dashboard tab at the top of the page.
        if (window.jQuery) {
          window.jQuery(document).on(
            'shown.bs.tab',
            '.navbar-nav a[data-toggle=tab]',
            function () {
              window.requestAnimationFrame(function () {
                window.scrollTo({
                  top: 0,
                  left: 0,
                  behavior: 'auto'
                });
              });
            }
          );
        }
      });
    "))
  ),

  footer = tags$footer(
    class = "dashboard-footer",
    div(
      class = "footer-inner",
      span(paste0("NASA Exoplanet Explorer | Snapshot: ", snapshot_date)),
      tags$a(
        "Open NASA Exoplanet Archive",
        href = archive_url,
        target = "_blank",
        rel = "noopener noreferrer"
      )
    )
  ),

  
  tabPanel(
    "Overview",
    div(
      class = "overview-container",

      div(
        class = "intro-panel",
        h1("A Field Guide to Worlds Beyond Earth"),
        p(
          "Discover how confirmed exoplanets have been found over time and ",
          "explore the diversity of their planets, stars and orbital systems."
        ),
        div(
          class = "source-text",
          "Source: ",
          tags$a(
            "NASA Exoplanet Archive",
            href = archive_url,
            target = "_blank",
            rel = "noopener noreferrer"
          ),
          paste0(" | Snapshot: ", snapshot_date, " | "),
          paste0("2026 figures are year to date through ", snapshot_cutoff, ".")
        )
      ),

      fluidRow(
        stat_card(
          "turquoise-card",
          "Confirmed Planets",
          "total_planets",
          "One row per confirmed planet"
        ),
        stat_card(
          "gold-card",
          "Host Systems",
          "total_hosts",
          "Unique host stars or systems"
        ),
        stat_card(
          "blue-card",
          "Discovery Methods",
          "total_methods",
          "Distinct methods in the archive"
        ),
        stat_card(
          "rose-card",
          "Years Covered",
          "year_coverage",
          "Discovery-year range"
        )
      ),

      fluidRow(
        class = "overview-chart-row",
        column(
          width = 8,
          div(
            class = "chart-card",
            h3("Confirmed Planets by Discovery Year"),
            p(
              class = "chart-description",
              "Annual number of confirmed planets in the snapshot. ",
              "The coral bar shows 2026 year-to-date data through 30 July. ",
              "The annotations add context from NASA's ",
              tags$a(
                "2014",
                href = kepler_2014_url,
                target = "_blank",
                rel = "noopener noreferrer"
              ),
              " and ",
              tags$a(
                "2016",
                href = kepler_2016_url,
                target = "_blank",
                rel = "noopener noreferrer"
              ),
              " Kepler validation announcements."
            ),
            accessible_plot_output(
              "discovery_year_plot",
              height = "380px",
              alt_text = paste(
                "Bar chart of confirmed planets by discovery year from 1992 to 2026.",
                "The largest peak is 1,504 planets in 2016, followed by 869 in 2014.",
                "Annotations connect those peaks with NASA Kepler validation announcements.",
                "The 2026 bar is marked as year-to-date."
              )
            )
          )
        ),

        column(
          width = 4,
          div(
            class = "chart-card",
            h3("Planets by Discovery Method"),
            p(
              class = "chart-description",
              "The five largest methods are shown separately; ",
              "the remaining six are grouped as Other."
            ),
            accessible_plot_output(
              "discovery_method_plot",
              height = "380px",
              alt_text = paste(
                "Ranked horizontal bar chart of confirmed planets by discovery method.",
                "Transit is the largest method with 4,667 planets, followed by Radial Velocity."
              )
            )
          )
        )
      ),

      fluidRow(
        class = "overview-feature-row",
        column(
          width = 7,
          div(
            class = "chart-card",
            h3("How Many Planets Share a System?"),
            p(
              class = "chart-description",
              "Counts each host system once and groups it by the number of confirmed planets reported by NASA."
            ),
            accessible_plot_output(
              "system_multiplicity_plot",
              height = "310px",
              alt_text = paste(
                "Horizontal bar chart grouping host systems by their number of confirmed planets.",
                "Most systems have one confirmed planet, followed by two planets and then three or more."
              )
            )
          )
        ),
        column(
          width = 5,
          div(
            class = "explore-guide-card",
            h3("How to Explore"),
            p(
              "Follow the tabs from a broad view of the archive to individual planets and final conclusions."
            ),
            tags$ol(
              class = "explore-guide-list",
              tags$li(
                tags$strong("Overview: "),
                "Understand the scale of the archive and its headline patterns."
              ),
              tags$li(
                tags$strong("Discovery Trends: "),
                "Change the years and compare how planets were detected."
              ),
              tags$li(
                tags$strong("Planet Characteristics: "),
                "Translate size, year length, distance and orbit shape into clear groups."
              ),
              tags$li(
                tags$strong("Data Explorer: "),
                "Search the archive and select a planet to inspect its reported measurements."
              ),
              tags$li(
                tags$strong("Key Findings: "),
                "Finish with the main interpretations, limitations and overall conclusion."
              )
            ),
            div(
              class = "explore-route",
              tags$strong("Suggested route: "),
              "Start with Overview, test a question in the middle tabs, then use Key Findings to connect the evidence."
            )
          )
        )
      ),

      div(
        class = "record-section",
        h3("Exoplanet Record Breakers"),
        p(
          class = "record-description",
          "Each record is translated into an Earth-based comparison. ",
          "The underlying NASA measurements are estimates and may change ",
          "as new observations become available."
        ),

        div(
          class = "record-grid",

          div(
            class = "record-card turquoise-card",
            div(class = "record-label", "Smallest Reported Radius"),
            div(class = "record-name", smallest_planet$pl_name),
            div(
              class = "record-value",
              paste(
                scales::percent(
                  smallest_planet$pl_rade,
                  accuracy = 1
                ),
                "of Earth's diameter"
              )
            ),
            div(
              class = "record-note",
              paste(
                "NASA radius estimate:",
                scales::number(
                  smallest_planet$pl_rade,
                  accuracy = 0.01
                ),
                "Earth radii."
              )
            )
          ),

          div(
            class = "record-card gold-card",
            div(class = "record-label", "Largest Reported Radius"),
            div(class = "record-name", largest_planet$pl_name),
            div(
              class = "record-value",
              paste(
                "About",
                scales::number(
                  largest_planet$pl_rade,
                  accuracy = 0.1
                ),
                "times Earth's diameter"
              )
            ),
            div(
              class = "record-note",
              "An unusually large estimate for a young imaged companion."
            )
          ),

          div(
            class = "record-card rose-card",
            div(class = "record-label", "Highest Temperature Estimate"),
            div(class = "record-name", hottest_planet$pl_name),
            div(
              class = "record-value",
              paste(
                "About",
                scales::number(
                  hottest_planet$pl_eqt / 255,
                  accuracy = 1
                ),
                "times Earth's equilibrium temperature"
              )
            ),
            div(
              class = "record-note",
              paste(
                "NASA estimate:",
                scales::number(
                  hottest_planet$pl_eqt,
                  accuracy = 1,
                  big.mark = ","
                ),
                "K. Earth's comparable value is about 255 K; neither ",
                "is a surface temperature."
              )
            )
          ),

          div(
            class = "record-card blue-card",
            div(class = "record-label", "Shortest Reported Year"),
            div(class = "record-name", shortest_year_planet$pl_name),
            div(
              class = "record-value",
              paste(
                "About",
                scales::number(
                  365.25 / shortest_year_planet$pl_orbper,
                  accuracy = 100,
                  big.mark = ","
                ),
                "times shorter than an Earth year"
              )
            ),
            div(
              class = "record-note",
              paste(
                "One orbit takes just",
                scales::number(
                  shortest_year_planet$pl_orbper * 24,
                  accuracy = 0.01
                ),
                "hours; Earth takes about 8,766 hours."
              )
            )
          ),

          div(
            class = "record-card turquoise-card",
            div(class = "record-label", "Closest Host System"),
            div(class = "record-name", closest_system$hostname),
            div(
              class = "record-value",
              paste(
                "Its light reaches Earth in",
                scales::number(
                  closest_system$sy_dist * 3.26156,
                  accuracy = 0.01
                ),
                "years"
              )
            ),
            div(
              class = "record-note",
              paste(
                "Sunlight reaches Earth in about 8 minutes.",
                closest_system$hostname,
                "hosts",
                format(closest_system$sy_pnum, trim = TRUE),
                "confirmed planets."
              )
            )
          )
        )
      )
    )
  ),

  tabPanel(
    "Discovery Trends",
    div(
      class = "overview-container",

      div(
        class = "section-heading",
        h2("Discovery Trends"),
        p(
          "Use the filter navigator to explore how confirmed exoplanet ",
          "discoveries changed over time and across detection methods."
        )
      ),

      fluidRow(
        column(
          width = 3,
          div(
            class = "filter-card filter-sidebar",
            h3("Filter Navigator"),

            sliderInput(
              "trend_years",
              "Discovery year range (all charts)",
              min = min(exoplanets$disc_year, na.rm = TRUE),
              max = max(exoplanets$disc_year, na.rm = TRUE),
              value = c(
                min(exoplanets$disc_year, na.rm = TRUE),
                max(exoplanets$disc_year, na.rm = TRUE)
              ),
              step = 1,
              sep = ""
            ),

            selectInput(
              "trend_method",
              "Discovery method (timeline only)",
              choices = c(
                "All methods",
                sort(unique(exoplanets$discoverymethod))
              ),
              selected = "All methods"
            ),

            actionButton(
              "reset_trend_filters",
              "Reset filters",
              class = "reset-button"
            ),

            div(
              class = "filter-scope",
              p(
                tags$strong("Year range: "),
                "Updates every chart and the four summary boxes."
              ),
              p(
                tags$strong("Discovery method: "),
                "Updates only the timeline."
              )
            ),

            div(
              class = "filter-summary",
              textOutput("trend_filter_summary")
            )
          )
        ),

        column(
          width = 9,

          div(
            class = "feature-kpi-grid",
            summary_card(
              "Largest annual increase",
              textOutput("trend_largest_increase"),
              textOutput("trend_largest_increase_note")
            ),
            summary_card(
              "Peak discovery year",
              textOutput("trend_peak_year"),
              textOutput("trend_peak_note")
            ),
            summary_card(
              "Average per year",
              textOutput("trend_average"),
              "Includes zero-discovery years in the range"
            ),
            summary_card(
              "Leading method",
              textOutput("trend_leading_method"),
              textOutput("trend_leading_note")
            )
          ),

          div(
            class = "chart-card",
            h3("Annual Discoveries in the Selected Period"),
            p(
              class = "chart-description",
              "Each point shows the number of confirmed planets discovered in that year. ",
              "The timeline updates when either filter changes."
            ),
            accessible_plot_output(
              "trend_timeline_plot",
              height = "380px",
              alt_text = paste(
                "Line chart of annual confirmed-planet discoveries for the selected years and method.",
                "Changing either filter updates the plotted values; 2026 is identified as partial when shown."
              )
            )
          ),

          div(
            class = "chart-card",
            h3("Discovery Methods in the Selected Period"),
            p(
              class = "chart-description",
              "Compares every discovery method for the selected years. ",
              "The discovery-method filter does not affect this chart. ",
              "Totals including 2026 are year to date."
            ),
            accessible_plot_output(
              "trend_method_plot",
              height = "450px",
              alt_text = paste(
                "Ranked horizontal bar chart comparing every discovery method in the selected year range.",
                "The discovery-method menu does not filter this comparison."
              )
            )
          ),

          div(
            class = "chart-card",
            h3("How Discovery Methods Changed Over Time"),
            p(
              class = "chart-description",
              "Each column shows the percentage of discoveries made with the four leading methods in the selected period; all remaining methods are grouped as Other."
            ),
            accessible_plot_output(
              "trend_method_mix_plot",
              height = "410px",
              alt_text = paste(
                "One-hundred-percent stacked bar chart showing how the share of discovery methods changed by year.",
                "The four leading methods are shown separately and the remainder are grouped as Other."
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              div(
                class = "chart-card",
                h3("Top Discovery Facilities"),
                p(
                  class = "chart-description",
                  "The six facilities credited with the most confirmed discoveries in the selected period."
                ),
                accessible_plot_output(
                  "trend_facility_plot",
                  height = "390px",
                  alt_text = paste(
                    "Ranked horizontal bar chart of the six facilities credited with the most discoveries",
                    "in the selected year range."
                  )
                )
              )
            ),
            column(
              width = 6,
              div(
                class = "chart-card",
                h3("Cumulative Growth in the Period"),
                p(
                  class = "chart-description",
                  "A running total beginning at zero for the first selected year, rather than the archive's lifetime total."
                ),
                accessible_plot_output(
                  "trend_cumulative_plot",
                  height = "390px",
                  alt_text = paste(
                    "Area and line chart of the running total of discoveries during the selected period.",
                    "The series starts at zero in the first selected year."
                  )
                )
              )
            )
          )
        )
      )
    )
  ),

  tabPanel(
    "Planet Characteristics",
    div(
      class = "overview-container",

      div(
        class = "section-heading",
        h2("Planet Characteristics"),
        p(
          "Compare planet sizes, the length of an exoplanet year and ",
          "how far these worlds are from Earth, then explore their orbit shapes."
        )
      ),

      div(
        class = "feature-kpi-grid",
        summary_card(
          "Typical planet size",
          paste0(
            scales::number(radius_median, accuracy = 0.01),
            "× Earth's radius"
          ),
          "Median among planets with reported radius"
        ),
        summary_card(
          "Typical exoplanet year",
          paste0(
            scales::number(orbital_median, accuracy = 0.01),
            " Earth days"
          ),
          paste0(
            scales::percent(orbital_shorter_earth_share, accuracy = 1),
            " of measured years are shorter than Earth's"
          )
        ),
        summary_card(
          "Typical recorded distance",
          paste0(
            scales::number(distance_median, accuracy = 1, big.mark = ","),
            " light-years"
          ),
          "Median among planets with reported distance"
        ),
        summary_card(
          "Most common orbit shape",
          paste0(
            scales::percent(nearly_circular_share, accuracy = 1),
            " nearly circular"
          ),
          "Among planets with reported eccentricity"
        )
      ),

      fluidRow(
        column(
          width = 3,
          div(
            class = "filter-card filter-sidebar",
            h3("How to Read These Charts"),

            p(
              class = "guide-text",
              "Earth is the reference point. One Earth radius means a ",
              "planet has the same radius as Earth."
            ),

            p(
              class = "guide-text",
              "An exoplanet year is the time it takes to complete one orbit. ",
              "One Earth year is about 365 days."
            ),

            p(
              class = "guide-text",
              "A light-year measures distance, not time. It is the distance ",
              "light travels in one year."
            ),

            p(
              class = "guide-text",
              paste0(
                "Orbit shape uses eccentricity: 0 means a circle, while values ",
                "closer to 1 describe a more stretched oval. Earth's value is ",
                scales::number(earth_eccentricity, accuracy = 0.0001),
                "."
              )
            ),

            radioButtons(
              "size_display",
              "Show all four charts as",
              choices = c(
                "Number of planets" = "count",
                "Percentage of measured planets" = "share"
              ),
              selected = "count"
            )
          )
        ),

        column(
          width = 9,
          div(
            class = "chart-card",
            h3("How Large Are Confirmed Exoplanets?"),
            p(
              class = "chart-description",
              "Each bar shows one radius range relative to Earth. ",
              "These are simple descriptive ranges for this dashboard, ",
              "not official planet classifications."
            ),
            accessible_plot_output(
              "planet_size_plot",
              height = "380px",
              alt_text = paste(
                "Horizontal bar chart grouping measured exoplanet radii relative to Earth.",
                "The display can switch between planet counts and percentages."
              )
            )
          ),

          div(
            class = "chart-card",
            h3("How Long Is a Year on an Exoplanet?"),
            p(
              class = "chart-description",
              "Each point represents an orbital-period range. ",
              "Shorter periods mean the planet completes its orbit more quickly."
            ),
            accessible_plot_output(
              "orbital_period_plot",
              height = "400px",
              alt_text = paste(
                "Dot-and-line chart grouping measured orbital periods from less than one Earth day",
                "to ten or more Earth years. The display can switch between counts and percentages."
              )
            )
          ),

          div(
            class = "chart-card",
            h3("How Far Away Are Confirmed Exoplanets?"),
            p(
              class = "chart-description",
              "Distance from Earth in light-years. Each confirmed planet is ",
              "counted once, so planets orbiting the same host system share ",
              "the same distance."
            ),
            accessible_plot_output(
              "distance_plot",
              height = "410px",
              alt_text = paste(
                "Vertical bar chart grouping confirmed planets by recorded distance from Earth in light-years.",
                "The display can switch between counts and percentages."
              )
            )
          ),

          div(
            class = "chart-card",
            h3("What Shapes Are Their Orbits?"),
            p(
              class = "chart-description",
              "The groups move from rounder to more stretched orbits. ",
              "They are simple descriptive ranges for this dashboard. ",
              "A reported value of zero may also reflect an assumed ",
              "circular orbit."
            ),
            accessible_plot_output(
              "orbit_shape_plot",
              height = "390px",
              alt_text = paste(
                "Horizontal bar chart grouping reported orbital eccentricity from nearly circular",
                "to highly stretched. The display can switch between counts and percentages."
              )
            )
          ),

          div(
            class = "chart-card earth-comparison-card",
            h3("How Do Their Size and Starlight Compare with Earth?"),
            p(
              class = "chart-description",
              "Each point is a confirmed exoplanet with both measurements. ",
              "Both axes use logarithmic scales because the archive spans very small and very large values. ",
              "Hover over a point for its name, discovery method, year, radius and starlight. ",
              "Drag to zoom; use the toolbar to pan, zoom, reset the view or download the chart. ",
              "Earth is shown only as a measurement reference, not as a habitability classification."
            ),
            fluidRow(
              column(
                width = 3,
                div(
                  class = "earth-comparison-controls",
                  selectInput(
                    "earth_method",
                    "Discovery method",
                    choices = c(
                      "All methods",
                      sort(unique(earth_comparison_data$discoverymethod))
                    ),
                    selected = "All methods"
                  ),
                  checkboxInput(
                    "earth_focus",
                    paste(
                      "Focus around Earth:",
                      "0.5 to 2 Earth radii and",
                      "0.1 to 10 times Earth's starlight"
                    ),
                    value = FALSE
                  ),
                  div(
                    class = "filter-summary",
                    textOutput("earth_comparison_summary")
                  ),
                  p(
                    class = "earth-caveat",
                    "Similar radius and starlight do not prove that a planet has liquid water, an atmosphere or life."
                  )
                )
              ),
              column(
                width = 9,
                accessible_plotly_output(
                  "earth_comparison_plot",
                  height = "430px",
                  alt_text = paste(
                    "Interactive log-scale scatter plot comparing planet radius and received starlight in Earth units.",
                    "Each point is a confirmed exoplanet with both measurements, while a labelled gold marker",
                    "shows Earth at one Earth radius and one Earth flux. Hover reveals planet details;",
                    "the toolbar supports zooming, panning, resetting and image download."
                  )
                )
              )
            )
          )
        )
      )
    )
  ),

  tabPanel(
    "Data Explorer",
    div(
      class = "overview-container",

      div(
        class = "section-heading",
        h2("Data Explorer"),
        p(
          "Search the NASA snapshot, narrow the planet list and select a row ",
          "to learn more about one confirmed world."
        )
      ),

      fluidRow(
        column(
          width = 3,
          div(
            class = "filter-card filter-sidebar",
            h3("Filter the Archive"),

            sliderInput(
              "explorer_years",
              "Discovery year",
              min = min(exoplanets$disc_year, na.rm = TRUE),
              max = max(exoplanets$disc_year, na.rm = TRUE),
              value = c(
                min(exoplanets$disc_year, na.rm = TRUE),
                max(exoplanets$disc_year, na.rm = TRUE)
              ),
              step = 1,
              sep = ""
            ),

            selectInput(
              "explorer_method",
              "Discovery method",
              choices = c(
                "All methods",
                sort(unique(exoplanets$discoverymethod))
              ),
              selected = "All methods"
            ),

            actionButton(
              "reset_explorer_filters",
              "Reset filters",
              class = "reset-button"
            ),

            div(
              class = "download-label",
              span("Filtered download")
            ),

            downloadButton(
              "download_explorer",
              "Download filtered records",
              class = "download-button"
            ),

            p(
              class = "guide-text",
              "The CSV contains all 22 dashboard variables for the planets matching the year and method filters."
            ),

            p(
              class = "guide-text",
              "Use the search box or the column filters above the table. ",
              "Click one row to open its planet details."
            ),

            div(
              class = "filter-summary",
              textOutput("explorer_summary")
            )
          )
        ),

        column(
          width = 9,
          div(
            class = "chart-card table-card",
            h3("Confirmed Planet Table"),
            p(
              class = "chart-description",
              "The table contains one row per confirmed planet. ",
              "Blank cells mean that NASA does not report that measurement ",
              "in this snapshot."
            ),
            DTOutput("planet_table")
          ),

          uiOutput("selected_planet_details")
        )
      )
    )
  ),

  tabPanel(
    "Key Findings",
    div(
      class = "overview-container",

      div(
        class = "section-heading",
        h2("Key Findings and Conclusions"),
        p(
          "The main patterns from the NASA snapshot, translated into clear " ,
          "conclusions for a non-technical audience."
        )
      ),

      div(
        class = "conclusion-grid",

        div(
          class = "conclusion-card turquoise-card",
          div(class = "conclusion-label", "Discovery Peak"),
          div(
            class = "conclusion-value",
            format(
              peak_complete_year$disc_year,
              scientific = FALSE
            )
          ),
          h3("The busiest complete discovery year"),
          p(
            paste(
              scales::comma(peak_complete_year$planet_count),
              "confirmed planets in this snapshot were discovered in",
              paste0(peak_complete_year$disc_year, "."),
              "The large annual peaks show how major surveys can quickly",
              "expand the confirmed-planet catalogue."
            )
          )
        ),

        div(
          class = "conclusion-card blue-card",
          div(class = "conclusion-label", "Leading Discovery Method"),
          div(
            class = "conclusion-value",
            scales::percent(transit_share, accuracy = 1)
          ),
          h3("Transit discoveries dominate the archive"),
          p(
            paste(
              scales::comma(transit_count),
              "of the",
              scales::comma(nrow(exoplanets)),
              "confirmed planets were found using the Transit method.",
              "This reflects the strength of the method and the surveys that",
              "use it, rather than proving that most planets in the universe transit."
            )
          )
        ),

        div(
          class = "conclusion-card gold-card",
          div(class = "conclusion-label", "Length of a Year"),
          div(
            class = "conclusion-value",
            scales::percent(
              orbital_shorter_earth_share,
              accuracy = 1
            )
          ),
          h3("Most measured exoplanet years are shorter than Earth's"),
          p(
            "Among planets with a reported orbital period, nearly nine in ten " ,
            "complete an orbit in less than 365.25 Earth days. Short-period " ,
            "planets are also generally easier to detect repeatedly."
          )
        ),

        div(
          class = "conclusion-card rose-card",
          div(class = "conclusion-label", "Orbit Shape"),
          div(
            class = "conclusion-value",
            scales::percent(nearly_circular_share, accuracy = 1)
          ),
          h3("Nearly circular reported orbits are the largest group"),
          p(
            "Among planets with a reported eccentricity, about three quarters " ,
            "fall below 0.1. This means their recorded paths are closer to a " ,
            "circle than to a strongly stretched oval."
          )
        )
      ),

      fluidRow(
        column(
          width = 6,
          div(
            class = "chart-card conclusion-panel turquoise-card",
            h3("What These Results Mean"),
            tags$ul(
              tags$li(
                "The number of confirmed planets has grown unevenly, with " ,
                "major survey years producing large jumps."
              ),
              tags$li(
                "The archive is shaped by how astronomers search. The most " ,
                "commonly detected planets are not necessarily the most common planets."
              ),
              tags$li(
                "Confirmed exoplanets display a wide range of sizes, distances, " ,
                "year lengths and orbit shapes, far beyond the conditions found on Earth."
              )
            )
          )
        ),

        column(
          width = 6,
          div(
            class = "chart-card conclusion-panel rose-card",
            h3("Limitations and Future Improvements"),
            tags$ul(
              tags$li(
                "The dashboard describes confirmed discoveries, not every planet " ,
                "that exists in the galaxy."
              ),
              tags$li(
                "Some measurements are unavailable, and the reported values may " ,
                "change as NASA receives new observations."
              ),
              tags$li(
                "The 2026 total is incomplete because the snapshot ends on 30 July."
              ),
              tags$li(
                "A future version could refresh automatically and compare planet " ,
                "properties with the characteristics of their host stars."
              )
            )
          )
        )
      ),

      div(
        class = "conclusion-banner",
        h3("Overall Conclusion"),
        p(
          "NASA's confirmed exoplanet archive shows both the remarkable diversity " ,
          "of worlds beyond our Solar System and the strong influence of detection " ,
          "technology on what we currently know. The dashboard should therefore be " ,
          "read as a picture of confirmed discoveries, not as a complete census of " ,
          "all planets in the universe."
        )
      )
    )
  )
)


server <- function(input, output, session) {


  observeEvent(input$reset_trend_filters, {
    updateSliderInput(
      session,
      "trend_years",
      value = c(
        min(exoplanets$disc_year, na.rm = TRUE),
        max(exoplanets$disc_year, na.rm = TRUE)
      )
    )
    updateSelectInput(
      session,
      "trend_method",
      selected = "All methods"
    )
  })

  observeEvent(input$reset_explorer_filters, {
    updateSliderInput(
      session,
      "explorer_years",
      value = c(
        min(exoplanets$disc_year, na.rm = TRUE),
        max(exoplanets$disc_year, na.rm = TRUE)
      )
    )
    updateSelectInput(
      session,
      "explorer_method",
      selected = "All methods"
    )
  })


  explorer_filtered <- reactive({
    req(input$explorer_years, input$explorer_method)

    filtered_data <- exoplanets %>%
      filter(
        disc_year >= input$explorer_years[1],
        disc_year <= input$explorer_years[2]
      )

    if (input$explorer_method != "All methods") {
      filtered_data <- filtered_data %>%
        filter(discoverymethod == input$explorer_method)
    }

    filtered_data %>%
      arrange(desc(disc_year), pl_name)
  })

  explorer_table_data <- reactive({
    explorer_filtered() %>%
      transmute(
        Planet = pl_name,
        `Host system` = hostname,
        `Discovery year` = disc_year,
        Method = discoverymethod,
        `Radius (Earths)` = round(pl_rade, 2),
        `Year length (days)` = round(pl_orbper, 2),
        `Distance (light-years)` = round(sy_dist * 3.26156, 1),
        `Temperature (K)` = round(pl_eqt)
      )
  })

  output$explorer_summary <- renderText({
    paste(
      format(nrow(explorer_filtered()), big.mark = ","),
      "planets match these filters."
    )
  })

  output$download_explorer <- downloadHandler(
    filename = function() {
      method_name <- if (input$explorer_method == "All methods") {
        "all_methods"
      } else {
        gsub(
          "[^a-z0-9]+",
          "_",
          tolower(input$explorer_method)
        )
      }

      paste0(
        "nasa_exoplanets_",
        input$explorer_years[1],
        "_to_",
        input$explorer_years[2],
        "_",
        method_name,
        ".csv"
      )
    },
    content = function(file) {
      utils::write.csv(
        explorer_filtered(),
        file,
        row.names = FALSE,
        na = ""
      )
    },
    contentType = "text/csv"
  )

  output$planet_table <- DT::renderDT(
    {
      DT::datatable(
        explorer_table_data(),
        rownames = FALSE,
        filter = "top",
        selection = "single",
        class = "stripe hover compact",
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50),
          scrollX = TRUE,
          autoWidth = TRUE,
          language = list(
            search = "Search all columns:"
          )
        )
      )
    },
    server = FALSE
  )

  selected_planet <- reactive({
    selected_row <- input$planet_table_rows_selected
    filtered_data <- explorer_filtered()

    if (
      length(selected_row) != 1 ||
      selected_row < 1 ||
      selected_row > nrow(filtered_data)
    ) {
      return(NULL)
    }

    filtered_data[selected_row, , drop = FALSE]
  })

  output$selected_planet_details <- renderUI({
    planet <- selected_planet()

    if (is.null(planet)) {
      return(
        div(
          class = "chart-card planet-detail-card",
          h3("Select a Planet"),
          p(
            class = "chart-description",
            "Click any row in the table to view its measurements and ",
            "discovery information."
          )
        )
      )
    }

    format_measurement <- function(
      value,
      accuracy = 0.01,
      suffix = ""
    ) {
      if (
        length(value) == 0 ||
        is.na(value) ||
        !is.finite(value)
      ) {
        return("Not available")
      }

      paste0(
        scales::number(
          value,
          accuracy = accuracy,
          big.mark = ","
        ),
        suffix
      )
    }

    detail_item <- function(label, value) {
      div(
        class = "detail-item",
        div(class = "detail-label", label),
        div(class = "detail-value", value)
      )
    }

    year_length <- if (
      is.finite(planet$pl_orbper) &&
      planet$pl_orbper > 0 &&
      planet$pl_orbper < 1
    ) {
      format_measurement(
        planet$pl_orbper * 24,
        accuracy = 0.01,
        suffix = " hours"
      )
    } else {
      format_measurement(
        planet$pl_orbper,
        accuracy = 0.01,
        suffix = " Earth days"
      )
    }

    orbit_shape <- if (
      is.finite(planet$pl_orbeccen) &&
      planet$pl_orbeccen >= 0 &&
      planet$pl_orbeccen <= 1
    ) {
      shape_name <- case_when(
        planet$pl_orbeccen < 0.1 ~ "Nearly circular",
        planet$pl_orbeccen < 0.3 ~ "Slightly stretched",
        planet$pl_orbeccen < 0.6 ~ "Moderately stretched",
        TRUE ~ "Highly stretched"
      )

      paste0(
        shape_name,
        " (",
        format_measurement(
          planet$pl_orbeccen,
          accuracy = 0.01
        ),
        ")"
      )
    } else {
      "Not available"
    }

    discovery_text <- if (
      !is.na(planet$disc_year) &&
      !is.na(planet$discoverymethod)
    ) {
      paste(
        format(planet$disc_year, scientific = FALSE),
        "using",
        planet$discoverymethod
      )
    } else {
      "Not available"
    }

    div(
      class = "chart-card planet-detail-card",
      h3(planet$pl_name),
      p(
        class = "chart-description",
        paste("Host system:", planet$hostname)
      ),

      div(
        class = "detail-grid",
        detail_item("Discovery", discovery_text),
        detail_item(
          "Planet radius",
          format_measurement(
            planet$pl_rade,
            accuracy = 0.01,
            suffix = " Earth radii"
          )
        ),
        detail_item("Length of year", year_length),
        detail_item(
          "Temperature estimate",
          format_measurement(
            planet$pl_eqt,
            accuracy = 1,
            suffix = " K"
          )
        ),
        detail_item(
          "Distance from Earth",
          format_measurement(
            planet$sy_dist * 3.26156,
            accuracy = 0.1,
            suffix = " light-years"
          )
        ),
        detail_item("Orbit shape", orbit_shape),
        detail_item(
          "Planet mass",
          format_measurement(
            planet$pl_bmasse,
            accuracy = 0.01,
            suffix = " Earth masses"
          )
        ),
        detail_item(
          "Planets in this system",
          format_measurement(
            planet$sy_pnum,
            accuracy = 1,
            suffix = " confirmed"
          )
        ),
        detail_item(
          "Discovery facility",
          ifelse(
            is.na(planet$disc_facility),
            "Not available",
            planet$disc_facility
          )
        )
      ),

      p(
        class = "detail-note",
        "Measurements are estimates from the NASA snapshot and may be updated ",
        "as new observations become available."
      )
    )
  })


  trend_period_data <- reactive({
    req(input$trend_years)

    exoplanets %>%
      filter(
        disc_year >= input$trend_years[1],
        disc_year <= input$trend_years[2]
      )
  })

  filtered_trends <- reactive({
    req(input$trend_method)

    filtered_data <- trend_period_data()

    if (input$trend_method != "All methods") {
      filtered_data <- filtered_data %>%
        filter(discoverymethod == input$trend_method)
    }

    filtered_data
  })

  trend_period_by_year <- reactive({
    year_counts <- trend_period_data() %>%
      count(disc_year, name = "planet_count")

    data.frame(
      disc_year = seq(
        input$trend_years[1],
        input$trend_years[2]
      )
    ) %>%
      left_join(year_counts, by = "disc_year") %>%
      mutate(
        planet_count = ifelse(
          is.na(planet_count),
          0,
          planet_count
        )
      )
  })

  trend_by_year <- reactive({
    year_counts <- filtered_trends() %>%
      count(disc_year, name = "planet_count")

    data.frame(
      disc_year = seq(
        input$trend_years[1],
        input$trend_years[2]
      )
    ) %>%
      left_join(year_counts, by = "disc_year") %>%
      mutate(
        planet_count = ifelse(
          is.na(planet_count),
          0,
          planet_count
        )
      )
  })

  trend_by_method <- reactive({
    trend_period_data() %>%
      filter(!is.na(discoverymethod)) %>%
      count(discoverymethod, name = "planet_count", sort = TRUE) %>%
      mutate(
        method_label = gsub(
          " Timing ",
          " Timing\n",
          discoverymethod,
          fixed = TRUE
        ),
        method_label = ifelse(
          discoverymethod == "Orbital Brightness Modulation",
          "Orbital Brightness\nModulation",
          method_label
        )
      )
  })

  output$trend_filter_summary <- renderText({
    matching_planets <- nrow(filtered_trends())

    paste(
      "Timeline selection:",
      format(matching_planets, big.mark = ","),
      ifelse(matching_planets == 1, "planet matches", "planets match"),
      "the selected years and discovery method."
    )
  })

  trend_period_summary <- reactive({
    period_data <- trend_period_data()
    year_data <- trend_period_by_year()
    total_planets <- nrow(period_data)

    increase_row <- year_data %>%
      arrange(disc_year) %>%
      mutate(
        previous_year = lag(disc_year),
        annual_increase = planet_count - lag(planet_count)
      ) %>%
      filter(
        !is.na(annual_increase),
        annual_increase > 0
      ) %>%
      arrange(desc(annual_increase), disc_year) %>%
      slice(1)

    if (nrow(increase_row) == 0) {
      increase_row <- NULL
    }

    if (total_planets > 0) {
      peak_row <- year_data %>%
        arrange(desc(planet_count), disc_year) %>%
        slice(1)

      leading_row <- period_data %>%
        filter(!is.na(discoverymethod)) %>%
        count(discoverymethod, name = "planet_count", sort = TRUE) %>%
        slice(1)
    } else {
      peak_row <- NULL
      leading_row <- NULL
    }

    list(
      total_planets = total_planets,
      average = total_planets / nrow(year_data),
      largest_increase = increase_row,
      peak = peak_row,
      leading = leading_row
    )
  })

  output$trend_largest_increase <- renderText({
    increase_row <- trend_period_summary()$largest_increase

    if (is.null(increase_row)) {
      "No increase"
    } else {
      paste0(
        "+",
        scales::comma(increase_row$annual_increase)
      )
    }
  })

  output$trend_largest_increase_note <- renderText({
    increase_row <- trend_period_summary()$largest_increase

    if (is.null(increase_row)) {
      "Select at least two years with a positive change"
    } else {
      paste(
        "From",
        format(increase_row$previous_year, scientific = FALSE),
        "to",
        format(increase_row$disc_year, scientific = FALSE)
      )
    }
  })

  output$trend_peak_year <- renderText({
    peak_row <- trend_period_summary()$peak

    if (is.null(peak_row)) {
      "No discoveries"
    } else {
      format(peak_row$disc_year, scientific = FALSE)
    }
  })

  output$trend_peak_note <- renderText({
    peak_row <- trend_period_summary()$peak

    if (is.null(peak_row)) {
      "No planets were recorded in this period"
    } else {
      paste0(
        scales::comma(peak_row$planet_count),
        " discoveries",
        ifelse(peak_row$disc_year == 2026, " (year to date)", "")
      )
    }
  })

  output$trend_average <- renderText({
    scales::number(
      trend_period_summary()$average,
      accuracy = 0.1,
      big.mark = ","
    )
  })

  output$trend_leading_method <- renderText({
    leading_row <- trend_period_summary()$leading

    if (is.null(leading_row) || nrow(leading_row) == 0) {
      "No method"
    } else {
      leading_row$discoverymethod
    }
  })

  output$trend_leading_note <- renderText({
    summary_data <- trend_period_summary()
    leading_row <- summary_data$leading

    if (is.null(leading_row) || nrow(leading_row) == 0) {
      "No method is represented"
    } else {
      paste(
        scales::comma(leading_row$planet_count),
        "planets |",
        scales::percent(
          leading_row$planet_count / summary_data$total_planets,
          accuracy = 1
        ),
        "of the period"
      )
    }
  })

  output$trend_timeline_plot <- renderPlot({
    timeline_data <- trend_by_year()

    timeline_plot <- ggplot(
      timeline_data,
      aes(
        x = disc_year,
        y = planet_count
      )
    )

    if (nrow(timeline_data) > 1) {
      timeline_plot <- timeline_plot +
        geom_line(
          color = field_navy,
          linewidth = 1.2
        )
    }

    timeline_plot <- timeline_plot +
      geom_point(
        color = field_navy,
        fill = field_gold,
        shape = 21,
        size = 3,
        stroke = 0.8
      ) +
      scale_x_continuous(
        breaks = scales::breaks_pretty(n = 8)
      ) +
      scale_y_continuous(
        labels = scales::comma,
        expand = expansion(mult = c(0, 0.12))
      ) +
      labs(
        x = "Discovery year",
        y = "Confirmed planets"
      ) +
      field_theme(base_size = 12) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = field_grid)
      )

    if (2026 %in% timeline_data$disc_year) {
      timeline_plot <- timeline_plot +
        geom_vline(
          xintercept = 2026,
          color = field_coral,
          linetype = "dashed",
          linewidth = 0.9
        ) +
        annotate(
          "text",
          x = 2026,
          y = max(timeline_data$planet_count) * 1.07,
          label = "2026 year to date",
          color = field_coral,
          hjust = 1.05,
          size = 3.5
        )
    }

    timeline_plot
  })

  output$trend_method_plot <- renderPlot({
    method_data <- trend_by_method()

    validate(
      need(
        nrow(method_data) > 0,
        "No discovery methods match the selected filters."
      )
    )

    ggplot(
      method_data,
      aes(
        x = planet_count,
        y = reorder(method_label, -planet_count)
      )
    ) +
      geom_col(
        fill = field_navy,
        width = 0.68
      ) +
      geom_text(
        aes(label = scales::comma(planet_count)),
        hjust = -0.15,
        color = field_ink,
        size = 3.5
      ) +
      scale_x_continuous(
        labels = scales::comma,
        expand = expansion(mult = c(0, 0.16))
      ) +
      labs(
        x = "Confirmed planets",
        y = NULL
      ) +
      field_theme(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = field_grid),
        plot.margin = margin(5, 30, 5, 5)
      )
  })

  trend_method_mix_data <- reactive({
    period_data <- trend_period_data() %>%
      filter(!is.na(discoverymethod))

    top_method_names <- period_data %>%
      count(discoverymethod, sort = TRUE) %>%
      slice_head(n = 4) %>%
      pull(discoverymethod)

    method_levels <- unique(c(top_method_names, "Other"))

    period_data %>%
      mutate(
        method_group = ifelse(
          discoverymethod %in% top_method_names,
          discoverymethod,
          "Other"
        ),
        method_group = factor(
          method_group,
          levels = method_levels
        )
      ) %>%
      count(disc_year, method_group, name = "planet_count")
  })

  trend_facility_data <- reactive({
    trend_period_data() %>%
      filter(
        !is.na(disc_facility),
        trimws(disc_facility) != ""
      ) %>%
      count(disc_facility, name = "planet_count", sort = TRUE) %>%
      slice_head(n = 6) %>%
      mutate(
        facility_label = vapply(
          disc_facility,
          function(facility_name) {
            paste(strwrap(facility_name, width = 24), collapse = "\n")
          },
          character(1)
        )
      )
  })

  trend_cumulative_data <- reactive({
    trend_period_by_year() %>%
      mutate(cumulative_planets = cumsum(planet_count))
  })

  output$trend_method_mix_plot <- renderPlot({
    mix_data <- trend_method_mix_data()

    validate(
      need(
        nrow(mix_data) > 0,
        "No discovery-method data are available for this period."
      )
    )

    method_levels <- levels(mix_data$method_group)
    method_palette <- setNames(
      c(
        field_navy,
        field_teal,
        field_gold,
        field_blue,
        field_coral
      )[seq_along(method_levels)],
      method_levels
    )

    ggplot(
      mix_data,
      aes(
        x = disc_year,
        y = planet_count,
        fill = method_group
      )
    ) +
      geom_col(
        position = "fill",
        width = 0.88,
        color = field_plot_background,
        linewidth = 0.15
      ) +
      scale_fill_manual(
        values = method_palette,
        drop = TRUE
      ) +
      scale_x_continuous(
        breaks = scales::breaks_pretty(n = 8)
      ) +
      scale_y_continuous(
        labels = scales::percent,
        expand = expansion(mult = c(0, 0))
      ) +
      labs(
        x = "Discovery year",
        y = "Share of discoveries",
        fill = "Discovery method"
      ) +
      field_theme(base_size = 12) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = field_grid),
        legend.position = "bottom",
        legend.title = element_text(color = field_ink),
        legend.text = element_text(color = field_muted, size = 10)
      ) +
      guides(
        fill = guide_legend(
          nrow = 2,
          byrow = TRUE
        )
      )
  })

  output$trend_facility_plot <- renderPlot({
    facility_data <- trend_facility_data()

    validate(
      need(
        nrow(facility_data) > 0,
        "No discovery facilities are reported for this period."
      )
    )

    ggplot(
      facility_data,
      aes(
        x = planet_count,
        y = reorder(facility_label, planet_count)
      )
    ) +
      geom_col(
        fill = field_navy,
        width = 0.66
      ) +
      geom_text(
        aes(label = scales::comma(planet_count)),
        hjust = -0.12,
        color = field_ink,
        size = 3.3
      ) +
      scale_x_continuous(
        labels = scales::comma,
        expand = expansion(mult = c(0, 0.24))
      ) +
      labs(
        x = "Confirmed planets",
        y = NULL
      ) +
      field_theme(base_size = 11) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = field_grid),
        plot.margin = margin(5, 30, 5, 5)
      )
  })

  output$trend_cumulative_plot <- renderPlot({
    cumulative_data <- trend_cumulative_data()

    cumulative_plot <- ggplot(
      cumulative_data,
      aes(
        x = disc_year,
        y = cumulative_planets
      )
    )

    if (nrow(cumulative_data) > 1) {
      cumulative_plot <- cumulative_plot +
        geom_area(
          fill = field_teal,
          alpha = 0.28
        ) +
        geom_line(
          color = field_navy,
          linewidth = 1.25
        ) +
        geom_point(
          data = tail(cumulative_data, 1),
          color = field_navy,
          fill = field_gold,
          shape = 21,
          size = 3.4,
          stroke = 0.9
        )
    } else {
      cumulative_plot <- cumulative_plot +
        geom_col(
          fill = field_teal,
          width = 0.5
        )
    }

    cumulative_plot +
      geom_text(
        data = tail(cumulative_data, 1),
        aes(label = scales::comma(cumulative_planets)),
        color = field_ink,
        hjust = 1.1,
        vjust = -0.8,
        size = 3.4
      ) +
      scale_x_continuous(
        breaks = scales::breaks_pretty(n = 6)
      ) +
      scale_y_continuous(
        labels = scales::comma,
        expand = expansion(mult = c(0, 0.16))
      ) +
      labs(
        x = "Discovery year",
        y = "Running total"
      ) +
      field_theme(base_size = 11) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = field_grid)
      )
  })


  earth_comparison_filtered <- reactive({
    req(input$earth_method)

    comparison_data <- earth_comparison_data

    if (input$earth_method != "All methods") {
      comparison_data <- comparison_data %>%
        filter(discoverymethod == input$earth_method)
    }

    if (isTRUE(input$earth_focus)) {
      comparison_data <- comparison_data %>%
        filter(
          pl_rade >= 0.5,
          pl_rade <= 2,
          pl_insol >= 0.1,
          pl_insol <= 10
        )
    }

    comparison_data
  })

  output$earth_comparison_summary <- renderText({
    comparison_data <- earth_comparison_filtered()

    paste(
      format(nrow(comparison_data), big.mark = ","),
      ifelse(
        nrow(comparison_data) == 1,
        "planet is shown with both measurements.",
        "planets are shown with both measurements."
      )
    )
  })

  output$earth_comparison_plot <- plotly::renderPlotly({
    plot_data <- earth_comparison_filtered() %>%
      mutate(
        hover_text = paste0(
          "<b>", htmltools::htmlEscape(pl_name), "</b>",
          "<br>Discovery method: ",
          htmltools::htmlEscape(discoverymethod),
          "<br>Discovery year: ", disc_year,
          "<br>Radius: ",
          format(
            signif(pl_rade, 4),
            scientific = FALSE,
            trim = TRUE,
            big.mark = ","
          ),
          " times Earth's radius",
          "<br>Starlight: ",
          format(
            signif(pl_insol, 4),
            scientific = FALSE,
            trim = TRUE,
            big.mark = ","
          ),
          " times Earth's"
        )
      )

    validate(
      need(
        nrow(plot_data) > 0,
        "No planets with both measurements match these controls."
      )
    )

    earth_plot <- plotly::plot_ly(
      data = plot_data,
      x = ~pl_rade,
      y = ~pl_insol,
      type = "scattergl",
      mode = "markers",
      text = ~hover_text,
      hoverinfo = "text",
      marker = list(
        color = field_navy,
        opacity = 0.38,
        size = 7
      ),
      name = "Confirmed exoplanets"
    )

    earth_plot <- plotly::add_markers(
      earth_plot,
      data = data.frame(
        pl_rade = 1,
        pl_insol = 1,
        hover_text = paste0(
          "<b>Earth reference</b>",
          "<br>Radius: 1 Earth radius",
          "<br>Starlight: 1 Earth flux"
        )
      ),
      x = ~pl_rade,
      y = ~pl_insol,
      text = ~hover_text,
      hoverinfo = "text",
      marker = list(
        color = field_gold,
        line = list(color = field_dark, width = 1.5),
        size = 13,
        symbol = "diamond"
      ),
      name = "Earth reference",
      inherit = FALSE
    )

    earth_plot <- plotly::add_text(
      earth_plot,
      data = data.frame(
        pl_rade = 1,
        pl_insol = 1,
        label = "Earth reference<br>1 radius | 1 flux"
      ),
      x = ~pl_rade,
      y = ~pl_insol,
      text = ~label,
      textposition = "top right",
      textfont = list(color = field_dark, size = 11),
      hoverinfo = "skip",
      inherit = FALSE
    )

    x_axis_settings <- list(
      title = list(text = "Planet radius (Earth = 1)"),
      type = "log",
      gridcolor = field_grid,
      linecolor = field_grid,
      tickfont = list(color = field_muted),
      tickmode = "array",
      tickvals = c(0.5, 1, 2, 5, 10, 20, 50),
      ticktext = c("0.5", "1", "2", "5", "10", "20", "50"),
      zeroline = FALSE
    )

    y_axis_settings <- list(
      title = list(text = "Starlight received (Earth = 1)"),
      type = "log",
      gridcolor = field_grid,
      linecolor = field_grid,
      tickfont = list(color = field_muted),
      tickmode = "array",
      tickvals = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
      ticktext = c("0.0001", "0.001", "0.01", "0.1", "1", "10", "100", "1k", "10k", "100k"),
      zeroline = FALSE
    )

    if (isTRUE(input$earth_focus)) {
      x_axis_settings$range <- log10(c(0.5, 2))
      x_axis_settings$tickvals <- c(0.5, 1, 2)
      x_axis_settings$ticktext <- c("0.5", "1", "2")

      y_axis_settings$range <- log10(c(0.1, 10))
      y_axis_settings$tickvals <- c(0.1, 0.3, 1, 3, 10)
      y_axis_settings$ticktext <- c("0.1", "0.3", "1", "3", "10")
    }

    earth_plot <- plotly::layout(
      earth_plot,
      showlegend = FALSE,
      dragmode = "zoom",
      hovermode = "closest",
      paper_bgcolor = field_plot_background,
      plot_bgcolor = field_plot_background,
      font = list(
        family = "'Segoe UI', Arial, sans-serif",
        color = field_ink,
        size = 12
      ),
      margin = list(l = 78, r = 24, t = 18, b = 68),
      xaxis = x_axis_settings,
      yaxis = y_axis_settings,
      shapes = list(
        list(
          type = "line",
          x0 = 1,
          x1 = 1,
          y0 = 0,
          y1 = 1,
          xref = "x",
          yref = "paper",
          line = list(color = field_coral, width = 1.2, dash = "dash")
        ),
        list(
          type = "line",
          x0 = 0,
          x1 = 1,
          y0 = 1,
          y1 = 1,
          xref = "paper",
          yref = "y",
          line = list(color = field_coral, width = 1.2, dash = "dash")
        )
      )
    )

    plotly::config(
      earth_plot,
      displayModeBar = TRUE,
      displaylogo = FALSE,
      responsive = TRUE,
      scrollZoom = TRUE,
      modeBarButtonsToRemove = c(
        "select2d",
        "lasso2d",
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )
  })

  prepare_distribution <- function(data, group, levels, reverse = TRUE) {
    req(input$size_display)
    show_counts <- input$size_display == "count"
    display_levels <- if (reverse) rev(levels) else levels

    data[[group]] <- factor(data[[group]], levels = display_levels)
    data$plot_value <- if (show_counts) data$planet_count else data$share
    data$plot_label <- if (show_counts) {
      scales::comma(data$planet_count)
    } else {
      scales::percent(data$share, accuracy = 0.1)
    }

    data
  }

  size_band_display <- reactive({
    prepare_distribution(size_band_counts, "size_band", size_band_levels)
  })

  orbital_period_display <- reactive({
    prepare_distribution(
      orbital_period_counts,
      "period_band",
      orbital_period_levels
    )
  })

  distance_display <- reactive({
    prepare_distribution(
      distance_band_counts,
      "distance_band",
      distance_band_levels,
      reverse = FALSE
    )
  })

  orbit_shape_display <- reactive({
    prepare_distribution(
      orbit_shape_counts,
      "orbit_shape",
      orbit_shape_levels
    )
  })

  output$planet_size_plot <- renderPlot({
    plot_data <- size_band_display()

    size_plot <- ggplot(
      plot_data,
      aes(
        x = plot_value,
        y = size_band
      )
    ) +
      geom_col(
        fill = field_navy,
        width = 0.68
      ) +
      geom_text(
        aes(label = plot_label),
        hjust = -0.15,
        color = field_ink,
        size = 3.7
      )

    if (input$size_display == "count") {
      size_plot <- size_plot +
        scale_x_continuous(
          labels = scales::comma,
          expand = expansion(mult = c(0, 0.18))
        ) +
        labs(x = "Number of planets", y = NULL)
    } else {
      size_plot <- size_plot +
        scale_x_continuous(
          labels = scales::percent,
          expand = expansion(mult = c(0, 0.18))
        ) +
        labs(x = "Percentage of measured planets", y = NULL)
    }

    size_plot +
      field_theme(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = field_grid),
        plot.margin = margin(5, 30, 5, 5)
      )
  })

  output$orbital_period_plot <- renderPlot({
    plot_data <- orbital_period_display()

    period_plot <- ggplot(
      plot_data,
      aes(
        x = plot_value,
        y = period_band
      )
    ) +
      geom_segment(
        aes(
          x = 0,
          xend = plot_value,
          yend = period_band
        ),
        color = "#DCE7EC",
        linewidth = 3,
        lineend = "round"
      ) +
      geom_point(
        color = field_teal,
        size = 6
      ) +
      geom_text(
        aes(label = plot_label),
        hjust = -0.45,
        color = field_ink,
        size = 3.7
      )

    if (input$size_display == "count") {
      period_plot <- period_plot +
        scale_x_continuous(
          labels = scales::comma,
          expand = expansion(mult = c(0, 0.2))
        ) +
        labs(x = "Number of planets", y = NULL)
    } else {
      period_plot <- period_plot +
        scale_x_continuous(
          labels = scales::percent,
          expand = expansion(mult = c(0, 0.2))
        ) +
        labs(x = "Percentage of measured planets", y = NULL)
    }

    period_plot +
      field_theme(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = field_grid),
        plot.margin = margin(5, 30, 5, 5)
      )
  })

  output$distance_plot <- renderPlot({
    plot_data <- distance_display()

    distance_plot <- ggplot(
      plot_data,
      aes(
        x = distance_band,
        y = plot_value
      )
    ) +
      geom_col(
        fill = field_coral,
        width = 0.68
      ) +
      geom_text(
        aes(label = plot_label),
        vjust = -0.5,
        color = field_ink,
        size = 3.7
      )

    if (input$size_display == "count") {
      distance_plot <- distance_plot +
        scale_y_continuous(
          labels = scales::comma,
          expand = expansion(mult = c(0, 0.14))
        ) +
        labs(x = NULL, y = "Number of planets")
    } else {
      distance_plot <- distance_plot +
        scale_y_continuous(
          labels = scales::percent,
          expand = expansion(mult = c(0, 0.14))
        ) +
        labs(x = NULL, y = "Percentage of measured planets")
    }

    distance_plot +
      field_theme(base_size = 12) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = field_grid),
        axis.text.x = element_text(size = 10, lineheight = 0.95),
        plot.margin = margin(5, 15, 5, 5)
      )
  })

  output$orbit_shape_plot <- renderPlot({
    plot_data <- orbit_shape_display()

    orbit_palette <- c(
      "Nearly circular\n(0.0 to under 0.1)" = field_teal,
      "Slightly stretched\n(0.1 to under 0.3)" = field_blue,
      "Moderately stretched\n(0.3 to under 0.6)" = field_gold,
      "Highly stretched\n(0.6 to 1.0)" = field_coral
    )

    orbit_plot <- ggplot(
      plot_data,
      aes(
        x = plot_value,
        y = orbit_shape,
        fill = orbit_shape
      )
    ) +
      geom_col(
        width = 0.68
      ) +
      geom_text(
        aes(label = plot_label),
        hjust = -0.15,
        color = field_ink,
        size = 3.7
      ) +
      scale_fill_manual(
        values = orbit_palette,
        guide = "none",
        drop = FALSE
      )

    if (input$size_display == "count") {
      orbit_plot <- orbit_plot +
        scale_x_continuous(
          labels = scales::comma,
          expand = expansion(mult = c(0, 0.18))
        ) +
        labs(x = "Number of planets", y = NULL)
    } else {
      orbit_plot <- orbit_plot +
        scale_x_continuous(
          labels = scales::percent,
          expand = expansion(mult = c(0, 0.18))
        ) +
        labs(x = "Percentage of measured planets", y = NULL)
    }

    orbit_plot +
      field_theme(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = field_grid),
        plot.margin = margin(5, 30, 5, 5)
      )
  })


  output$system_multiplicity_plot <- renderPlot({
    multiplicity_palette <- c(
      "1 confirmed planet" = field_navy,
      "2 confirmed planets" = field_teal,
      "3 or more confirmed planets" = field_gold
    )

    ggplot(
      system_multiplicity_counts,
      aes(
        x = system_count,
        y = reorder(system_group, system_count),
        fill = system_group
      )
    ) +
      geom_col(width = 0.64) +
      geom_text(
        aes(
          label = paste0(
            scales::comma(system_count),
            " (",
            scales::percent(share, accuracy = 1),
            ")"
          )
        ),
        color = field_ink,
        hjust = -0.12,
        size = 3.5
      ) +
      scale_fill_manual(
        values = multiplicity_palette,
        guide = "none"
      ) +
      scale_x_continuous(
        labels = scales::comma,
        expand = expansion(mult = c(0, 0.28))
      ) +
      labs(
        x = "Host systems",
        y = NULL
      ) +
      field_theme(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = field_grid),
        plot.margin = margin(5, 45, 5, 5)
      )
  })

  output$total_planets <- renderText({
    format(nrow(exoplanets), big.mark = ",")
  })

  output$total_hosts <- renderText({
    format(length(unique(exoplanets$hostname)), big.mark = ",")
  })

  output$total_methods <- renderText({
    length(unique(exoplanets$discoverymethod))
  })

  output$year_coverage <- renderText({
    paste(
      min(exoplanets$disc_year, na.rm = TRUE),
      max(exoplanets$disc_year, na.rm = TRUE),
      sep = "–"
    )
  })

  output$discovery_year_plot <- renderPlot({

    ggplot(
      discoveries_by_year,
      aes(
        x = disc_year,
        y = planet_count,
        fill = year_status
      )
    ) +
      geom_col(width = 0.8) +
      geom_segment(
        data = discovery_research_events,
        aes(
          x = disc_year,
          xend = disc_year,
          y = planet_count + 30,
          yend = label_height - 75
        ),
        inherit.aes = FALSE,
        color = field_gold,
        linewidth = 0.8
      ) +
      geom_label(
        data = discovery_research_events,
        aes(
          x = disc_year,
          y = label_height,
          label = announcement_label
        ),
        inherit.aes = FALSE,
        color = field_dark,
        fill = "#F3E3B8",
        linewidth = 0.25,
        lineheight = 0.95,
        size = 2.8
      ) +
      geom_text(
        data = subset(discoveries_by_year, disc_year == 2026),
        aes(label = "Year to date"),
        vjust = -0.6,
        color = field_coral,
        size = 3.5
      ) +
      scale_fill_manual(
        values = c(
          "Complete year" = field_navy,
          "2026 year to date" = field_coral
        ),
        guide = "none"
      ) +
      scale_x_continuous(
        breaks = seq(1992, 2026, by = 4)
      ) +
      scale_y_continuous(
        labels = scales::comma,
        expand = expansion(mult = c(0, 0.28))
      ) +
      labs(
        x = "Discovery year",
        y = "Confirmed planets"
      ) +
      field_theme(base_size = 12) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = field_grid)
      )
  })

  output$discovery_method_plot <- renderPlot({

    ggplot(
      methods_overview,
      aes(
        x = planet_count,
        y = reorder(method_label, -planet_count)
      )
    ) +
      geom_col(
        fill = field_navy,
        width = 0.65
      ) +
      geom_text(
        aes(label = scales::comma(planet_count)),
        hjust = -0.15,
        color = field_ink,
        size = 3.4
      ) +
      scale_x_continuous(
        labels = scales::comma,
        expand = expansion(mult = c(0, 0.2))
      ) +
      labs(
        x = "Confirmed planets",
        y = NULL
      ) +
      field_theme(base_size = 11) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = field_grid)
      )
  })
}


shinyApp(ui = ui, server = server)
