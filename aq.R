
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

species = c("no2", "pm10", "pm25")
# try and get as much as possible
all_dat = list()
for (t in saq_traffic$site){

  for (s in species){

  dat_df = saqgetr::get_saq_observations(t,variable = s,start = "2000-01-01", end = "2024-02-28")

all_dat[[paste(t,s)]] = dat_df
print(paste(t,s))
  }

}

dat_all = do.call(rbind,all_dat)


##METEO
# get observation data from nearest meteo sites
met_data = get_noaa(poi_lat = cty_coords$coords[2],
                    poi_lon = cty_coords$coords[1],
                    start_year = year(min(dat_all$date)),
                    end_year = year(max(dat_all$date))
                    )


dir.create("plots/")
##Join AQ data with met
aq_met <- left_join(dat_all, met_data, by = "date")

for (s in unique(aq_met$site)){

  site_dat = filter(aq_met,site == s)

  specz = unique(site_dat$variable)

  site_df = filter(saq_sites,site == s)

  for (v in specz){

    aq_df = filter(site_dat,variable == v)

##Rm weather works on daily averages so data needs to be averaged
#aq_day <- timeAverage(aq_df, "day")

##Specify the number of 'trees' and 'samples'
Trees <- 300
Samples <- 300

# Met normalisation
list_rm <- rmw_do_all(
  rmw_prepare_data(aq_df, value = "value"),
  variables = c(
    "wd", "ws", "air_temp", "RH", "ceil_hgt","cl","atmos_pres", "date_unix", "day_julian", "weekday", "hour"
  ),
  n_trees = Trees,
  n_samples = Samples,
  verbose = TRUE
)

list_rm$normalised %>%
  rename(value = value_predict) %>%
  threadr::time_dygraph()

normalised <- list_rm$normalised

#normalised_month = timeAverage(list_rm$normalised, avg.time = "month")


site_dets <- paste0("Monitoring site: ", s, ", ", site_df$site_name, ", ",site_df$site_area, " ", site_df$site_type)

if(v == "no2"){
spec_expression <- expression("NO"[2]*" (ug/m3)")
spec_expression = "NO~2~"
spec_expression = "NO2"
}
if(v == "pm25"){
  spec_expression <- expression("PM"[2.5]*" (ug/m3)")
  spec_expression = "PM~2.5~"
  spec_expression = "PM2.5"
}
if(v == "pm10"){
  spec_expression1 = expression("PM"[10]*" (ug/m3)")
  #spec_expression2 = "PM~10~"
  spec_expression2 = "PM10"
}
# Plot normalised time series
rmw_plot_normalised(normalised) +
  labs(subtitle=site_dets,
       y=spec_expression1,
       x="Year",
       title=paste0("Weather normalised ",spec_expression2," Concentrations"),
       caption = paste0("Data source: ",site_df$data_source,", accessed using the openair R package. Weather normalisation carried out using the RMweather R package"))

#p1 <- timePlot(normalised, "value_predict", smooth = TRUE)
#dir.create("out/plots")

ggsave(paste0("plots/", v, "_",s ,"_norm.png"))
print(paste0(v, " ", s))
#}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  }

}

aq_met_L5Y = filter(aq_met,date > "2020-01-01")

site_coords = saq_sites |>
  mutate(lat = st_coordinates(saq_sites)[,2], lon = st_coordinates(saq_sites)[,1]) |>
  select(site,lat,lon)

aq_met_L5Y = left_join(aq_met_L5Y,site_coords, by = "site")

p1 = openairmaps::polarMap(aq_met_L5Y, pollutant = "value", latitude = "lat",
                      longitude = "lon",type = "variable",
                      provider = c("CyclOSM", "OpenStreetMap", "Esri.WorldImagery"))
dir.create("maps/")
htmlwidgets::saveWidget(p1,"maps/L5Y_polar_map.html",selfcontained = TRUE)

for (s in unique(aq_met$site)){

  site_dat = filter(aq_met_L5Y,site == s)

  specz = unique(site_dat$variable)

  site_df = filter(saq_sites,site == s)

  for (v in specz){

    aq_df = filter(site_dat,variable == v)

    if(v == "no2"){
      spec_expression <- expression("NO"[2]*" (ug/m3)")
      spec_expression = "NO~2~"
      spec_expression = "NO2"
    }
    if(v == "pm25"){
      spec_expression <- expression("PM"[2.5]*" (ug/m3)")
      spec_expression = "PM~2.5~"
      spec_expression = "PM2.5"
    }
    if(v == "pm10"){
      spec_expression1 = expression("PM"[10]*" (ug/m3)")
      #spec_expression2 = "PM~10~"
      spec_expression2 = "PM10"
    }

    min_yr = year(min(aq_df$date))
    max_yr = year(max(aq_df$date))

    p1 = polarPlot(aq_df, "value", key.header = paste0(v," (ug/m3)"),key.footer = NULL, type = "year",
                   main = paste0("Polar plot showing ",v," grouped by wind direction and speed\n",site_df$site_name,
                                 " between ",min_yr, " and ", max_yr))

    filename <- paste0("plots/", v, "_",s ,"_polar.png")
    png(filename)
    print(p1)
    dev.off()

  }

}


