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



library(euroaq)
library(sf)
library(mapview)
library(dplyr)
library(tmaptools)
library(saqgetr)
library(openair)
library(rmweather)
library(htmlwidgets)

source("R/create.R")
source("R/get.R")

saq_sites = saqgetr::get_saq_sites() |>
  filter(!is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"),crs = 4326)

city = "Barcelona"

# use tmap city geocode to estimate the city centre
cty_coords <- tmaptools::geocode_OSM(city)

# city centre
cc = cty_coords$coords |>
  st_point() |>
  st_sfc(crs = 4326) |>
  st_as_sf()

# get approx 10km buffer from city centre
cc_10km <- cty_coords$coords |>
  st_point() |>
  st_sfc(crs = 4326) |>
  st_buffer(10000)

city_sites = saq_sites[cc_10km,]

saq_traffic = filter(city_sites,site_type == "traffic")

library(sf)
library(tmap)
library(tmap.networks)

bb = st_bbox(final_all)

palette_npt = c(
  "Segregated Track (wide)" = "#054d05", # Dark Green
  "Off Road Path" = "#3a9120", # Medium Green
  "Segregated Track (narrow)" = "#87d668", # Light Green
  "Shared Footway" = "#ffbf00", # Amber
  "Painted Cycle Lane" = "#FF0000" # Red
)
palette_npt = c(
  "Segregated Track (wide)" = "#054d05", # Dark Green
  "Off Road Path" = "#3a9120", # Medium Green
  "Segregated Track (narrow)" = "#87d668", # Light Green
  "Shared Footway" = "#ffbf00", # Amber
  "Painted Cycle Lane" = "#FF0000" # Red
)

pal = c("#054d05","#3a9120", "#87d668", "#ffbf00", "#FF0000" )

final_2 = select(final_all, detailed_segregation,year)

lvls = unique(final_2$detailed_segregation)

# get background map
bg <- basemaps::basemap_raster(ext = bb, map_service = "carto", map_type = "light")

yrs = as.character(unique(final_2$year))

tm1 <- tm_shape(bg)+
  tm_rgb()+
  tm_shape(final_2) +
  tm_edges(
    #col = "detailed_segregation",col.scale = tm_scale_categorical(values = pal,levels = lvls),
    col = "detailed_segregation",
    lwd = 3
  )+
  tm_legend(show = TRUE,frame = FALSE, position = c(0.73,0.20))+
  tm_title(text = paste0("Barcelona cycling infrastructure evolution (OSM)\n",yrs), position = c(0.00,0.99))+
  tm_layout(frame = FALSE, panel.show = FALSE)+
  tm_animate("year",fps = 1)

tmap_animation(tm1, filename = paste0("plots/osm_evolution.gif"))

final_1 = filter(final_2, year == "2009")
tmap_mode("view")

tm1 = tm_shape(final_1) +  # cas_df is captured in this iteration's closure
  tm_edges(
    col = "detailed_segregation",col.scale = tm_scale_categorical(values = pal),
    #col = "detailed_segregation",
    lwd = 3,
    popup.vars = c("detailed_segregation"),
    hover = "detailed_segregation",
    group = "2009",
    group.control = "radio")+
  tm_shape(saq_traffic)+
  tm_dots("site",size = 2,
          popup.vars = c("site","site_name"),
          group = "AQ sites",
          group.control = "check")

tm1 = tm1 + purrr::reduce(yrs, function(map_obj, ct) {


  yr_df <- dplyr::filter(final_2, year == ct)


  map_obj = map_obj +
    tm_shape(yr_df)+
    tm_lines(
      col = "detailed_segregation",lwd = 3,col.scale = tm_scale_categorical(values = pal),
      popup.vars = c("detailed_segregation"),
      hover = "detailed_segregation",
      group = ct,
      group.control = "radio")

  map_obj

}, .init = tm1)



tm1 = tm1+
  tm_basemap(group.control = "none")+
  tm_title("Barcelona cycle network evolution")+
  tm_view(control.collapse = FALSE)

tmap_save(tm1,"maps/osm_cycle_paths.html")
