library(osmactive)
library(osmdata)
library(ohsome)
library(purrr)
library(dplyr)

backfill_osmactive_cols <- function(df) {
  expected_cols <- gsub(":", "_", osmactive::et_active())
  missing <- setdiff(expected_cols, names(df))
  for (col in missing) df[[col]] <- NA_character_
  df
}

bbox <- getbb("Barcelona, Spain")

# generate query from 2008 to 2024, keep drive columns in for distance calc
q <- ohsome_query('elements/geometry') |>
  set_boundary(bbox) |>
  set_filter('highway in (primary, secondary, tertiary, residential, cycleway) and type:way') |>
  set_time('2008/2024/P1Y') |>
  set_parameters(properties = "tags")

# import query
osm_years <- ohsome_post(q) |>
  janitor::clean_names() |>
  rename_with(~ gsub(":", "_", .x)) |>
  backfill_osmactive_cols()

# Add a year column from the snapshot timestamp
osm_years = osm_years |>
  mutate(year = as.integer(format(as.Date(snapshot_timestamp), "%Y")))

# Split by year, classify each separately
classified_by_year <- split(osm_years, osm_years$year) |>
  imap(\(df, yr) {
    message("Processing ", yr)
    tryCatch({
      dnet <- osmactive::get_driving_network(df)
      cnet <- osmactive::get_cycling_network(df)
      cycle_net_d <- osmactive::distance_to_road(cnet, dnet)
      classified <- osmactive::classify_cycle_infrastructure(cycle_net_d)
      classified$year <- as.integer(yr)
      classified
    }, error = function(e) {
      message("Failed for ", yr, ": ", e$message)
      NULL
    })
  }) |>
  compact()

# Bind into one sf dataframe for plotting
final_all <- bind_rows(classified_by_year)
final_all$year <- factor(final_all$year)

library(tmap)

tm_shape(final_all) +
  tm_lines(
    col = "detailed_segregation",
    lwd = 0.4
  ) +
  tm_facets_wrap(by = "year", ncol = 4) +
  tm_title("Barcelona cycling infrastructure evolution (OSM)") +
  tm_layout(frame = FALSE)
