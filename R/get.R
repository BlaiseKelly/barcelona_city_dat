library(sf)
library(dplyr)
library(ecmwfr)


snap_to_era5_grid = function(domain, product = NULL) {

  # Get bounding box in WGS84
  bbox = domain |>
    st_transform(4326) |>
    st_bbox()

  # single level or land
  if(!is.null(product) & product == "single_layer"){
    grid_deg = 0.25
  }
  if(!is.null(product) & product == "land"){
    grid_deg = 0.1
  }
  if(!is.null(product)){grid_deg = 0.25}

  # Snap outward to nearest 0.25 degree grid
  north = ceiling(bbox["ymax"] / grid_deg) * grid_deg
  south = floor(bbox["ymin"] / grid_deg) * grid_deg
  east  = ceiling(bbox["xmax"] / grid_deg) * grid_deg
  west  = floor(bbox["xmin"] / grid_deg) * grid_deg

  # Check minimum span — must cover at least 2 grid cells in each direction
  # otherwise CDS sometimes returns nothing
  if ((north - south) < 2 * grid_deg) {
    north = north + grid_deg
    south = south - grid_deg
  }
  if ((east - west) < 2 * grid_deg) {
    east = east + grid_deg
    west = west - grid_deg
  }

  paste0(north, "/", west, "/", south, "/", east)
}


#' Download ERA5 reanalysis data for a single location
#'
#' @param lon Longitude of the location (default: -2.283846)
#' @param lat Latitude of the location (default: 53.439953)
#' @param dataset Character string specifying the dataset. Options:
#'     - "reanalysis-era5-single-levels"
#'     - "reanalysis-era5-land"
#' @param variables A vector of variables to download. Options:
#'      - "2m_dewpoint_temperature",
#'      -"2m_temperature",
#'      -"soil_temperature_level_1",
#'      - "snow_cover",
#'      -"surface_solar_radiation_downwards",
#'      -"10m_u_component_of_wind",
#'      -"10m_v_component_of_wind",
#'      -"surface_pressure"
#' @param years A vector of years for which data is requested
#' @param api_key API key for authentication (must be provided)
#' @param path_out Directory to save output files (default: "out/")
#' @return A dataframe containing the combined data for all variables and years

get_era <- function(ecmwf_format_bb,
                    dataset = "land",
                    variables,
                    start_date,
                    end_date,
                    api_key,
                    path_out) {


  if (is.null(api_key)) {
    stop("API key must be provided.")
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(path_out)) {
    dir.create(path_out, recursive = TRUE)
  }

  if(dataset == "land"){
    ds = "reanalysis-era5-land"
  }
  if(dataset == "single-level"){
    ds = "reanalysis-era5-single-levels"
  }
    # Set the API key
  wf_set_key(key = api_key)

  # create data frame of days for period of interest
  date_df = create_dates_df(start_date = start_date, end_date = end_date, time_zone = "UTC", timestep = "month")

  for(d in date_df){

    m = month(date_df)
    y = year(date_df)

      for (v in variables) {

        tryCatch({

          request <- list(
            dataset_short_name = ds,
            product_type = "reanalysis",
            variable = v,
            year = as.character(y),
            month = m,
            #month = unique(month(kr8_d8)),
            day = sprintf("%02d", 1:31),
            time = sprintf("%02d:00", 0:23),
            area = ecmwf_format_bb,
            format = "netcdf",
            download_format = "unarchived",
            target = paste0(v, "_",sprintf("%02d",m),"_", y, ".nc")
          )

          print(paste("requesting", v, "for ",y, " in ", sprintf("%02d",m)))

          tryCatch({
            file <- wf_request(request = request, transfer = TRUE, path = paste0(path_out, "/"))
          }, error = function(e) {
            message("Error requesting variable ", v, " for month ", m, " year ", y, ": ", conditionMessage(e))
          })

        }, error = function(e) {
          message("Error requesting variable ", v, " for month ", m, " year ", y, ": ", conditionMessage(e))
        })

      }

  }

}

library(worldmet)
library(dplyr)
library(tidyr)
library(lubridate)

get_noaa = function(poi_lat = NULL,   # your point of interest
                    poi_lon = NULL,
                    start_year = NULL,
                    end_year = NULL,
                    n_stations = 5,
                    fill_na_with_closest = TRUE){

  # poi_lat = cty_coords$coords[2]  # your point of interest
  # poi_lon = cty_coords$coords[1]
  # start_year = 2020
  # end_year = 2026
  # n_stations = 5
  # fill_na_with_closest = TRUE

# ---- CONFIG ----
   # search radius - get more than 3 in case some have no data
met_vars <- c("ws", "wd", "air_temp", "atmos_pres", "RH", "cl", "ceil_hgt")

# ---- 1. Find nearest stations ----
stations <- getMeta(lat = poi_lat, lon = poi_lon, n = n_stations, plot = FALSE)

message("Nearest stations:")
print(select(stations, station, code, dist, begin, end))

# ---- 2. Download data from top 3 stations ----
years <- start_year:end_year

download_station <- function(code, station_name, years) {
  message(paste("Downloading:", station_name, "(", code, ")"))
  tryCatch(
    importNOAA(code = code, year = years, quiet = TRUE),
    error = function(e) {
      message(paste("  Failed:", e$message))
      return(NULL)
    }
  )
}

dat1 <- download_station(stations$code[1], stations$station[1], years)
dat2 <- download_station(stations$code[2], stations$station[2], years)
dat3 <- download_station(stations$code[3], stations$station[3], years)

# ---- 3. Build complete hourly sequence ----
full_dates <- tibble(
  date = seq(
    from = make_datetime(start_year, 1, 1, 0, tz = "UTC"),
    to   = make_datetime(end_year, 12, 31, 23, tz = "UTC"),
    by   = "1 hour"
  )
)

# ---- 4. Helper: join and fill from a station ----
join_station <- function(existing, new_data, station_label, vars) {
  if (is.null(new_data)) {
    message(paste(" ", station_label, "- no data available, skipping"))
    return(existing)
  }

  # select only the vars we need, prefixed with station label for tracking
  fill_data <- new_data |>
    select(date, all_of(intersect(vars, names(new_data)))) |>
    distinct(date, .keep_all = TRUE)

  # join to existing
  result <- existing |>
    left_join(fill_data, by = "date", suffix = c("", paste0("_", station_label)))

  # fill NAs in each variable from this station
  for (v in intersect(vars, names(fill_data))) {
    new_col <- paste0(v, "_", station_label)
    if (new_col %in% names(result)) {
      result[[v]] <- coalesce(result[[v]], result[[new_col]])
      result[[new_col]] <- NULL
    }
  }

  result
}

# ---- 5. Start with station 1, fill gaps with 2 then 3 ----
if (!is.null(dat1)) {
  met <- full_dates |>
    left_join(
      dat1 |> select(date, all_of(intersect(met_vars, names(dat1)))) |>
        distinct(date, .keep_all = TRUE),
      by = "date"
    )
} else {
  # if station 1 has no data, start with empty columns
  met <- full_dates
  for (v in met_vars) met[[v]] <- NA_real_
}

# track completeness
report_completeness <- function(df, vars, label) {
  n <- nrow(df)
  for (v in intersect(vars, names(df))) {
    pct <- round(100 * sum(!is.na(df[[v]])) / n, 1)
    message(paste0("  ", label, " - ", v, ": ", pct, "% complete"))
  }
}

message("\nAfter station 1 (", stations$station[1], ", ",
        round(stations$dist[1], 1), " km):")
report_completeness(met, met_vars, "S1")

met <- join_station(met, dat2, "s2", met_vars)
message("\nAfter station 2 (", stations$station[2], ", ",
        round(stations$dist[2], 1), " km):")
report_completeness(met, met_vars, "S1+S2")

met <- join_station(met, dat3, "s3", met_vars)
message("\nAfter station 3 (", stations$station[3], ", ",
        round(stations$dist[3], 1), " km):")
report_completeness(met, met_vars, "S1+S2+S3")

# ---- 6. Add source tracking column per variable ----
# Useful to know which station each hour came from
if (!is.null(dat1)) dates1 <- dat1$date else dates1 <- as.POSIXct(character(0))
if (!is.null(dat2)) dates2 <- dat2$date else dates2 <- as.POSIXct(character(0))
if (!is.null(dat3)) dates3 <- dat3$date else dates3 <- as.POSIXct(character(0))

met <- met |>
  mutate(
    source = case_when(
      date %in% dates1 & !is.na(ws) ~ stations$station[1],
      date %in% dates2 & !is.na(ws) ~ stations$station[2],
      date %in% dates3 & !is.na(ws) ~ stations$station[3],
      TRUE ~ NA_character_
    ),
    source_dist_km = case_when(
      source == stations$station[1] ~ stations$dist[1],
      source == stations$station[2] ~ stations$dist[2],
      source == stations$station[3] ~ stations$dist[3]
    )
  )

return(met)

# message("\nSource breakdown:")
# print(count(met, source, sort = TRUE))
#
# message("\nFinal dataset: ", nrow(met), " hours, ",
#         sum(complete.cases(met[met_vars])), " fully complete")

}
