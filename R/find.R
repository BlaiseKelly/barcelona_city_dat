library(worldmet)

find_noaa_domain <- function(domain, html_out = FALSE){
  #domain <- sub_domain
  ## import all meteo sites
  met_info <- getMeta(plot = FALSE)
  ## geo reference them
  met_sf <- st_as_sf(met_info, coords = c("longitude", "latitude"), crs = 4326)

  met_in <- met_sf[domain,]

  met_in$start_year <- year(met_in$begin)
  met_in$end_year <- year(met_in$end)

  if(html_out == TRUE){

    dir.create('plots/noaa', recursive = TRUE)
    ## set palette for site types
    pal_g <- colorFactor("Set3", reverse = FALSE, domain = met_in$end_year)

    lon <- mean(st_coordinates(sub_domain)[,1])
    lat <- mean(st_coordinates(sub_domain)[,2])

    ## create base base
    m <- leaflet() %>%
      setView(lon, lat, zoom = 6) %>%
      addProviderTiles('CartoDB.Positron')


    m <- m %>% addCircleMarkers(data = met_in, fillColor = ~pal_g(end_year), color = 'black', weight = 1,
                                opacity = 1.0, fillOpacity = 1.0,
                                popup = paste("station code:", met_in$code, "<br>",
                                              "station name:", met_in$station, "<br>",
                                              "country:", met_in$ctry, "<br>",
                                              "date start:", met_in$begin, "<br>",
                                              "date end:", met_in$end, "<br>",
                                              "elevation (m):", met_in$`elev(m)`, "<br>",
                                              "usaf:", met_in$usaf, "<br>"))


    m <- m %>% addLegend("bottomleft", pal=pal_g, values=met_in$end_year, opacity=1, title = "Latest year")

    print(paste0("saving html to: plots/noaa/",round(lon,1), "_", round(lat,1) ,".html"))
    ## use html widget saveWidget function to make a standalone html
    withr::with_dir('./', saveWidget(m, file = paste0("plots/noaa/obs_domain_",round(lon,1), "_", round(lat,1) ,".html")))

  }


  return(met_in)

}


find_noaa_sites <- function(sites, start_date, end_date){

  ## import all meteo sites
  met_info <- getMeta()
  ## geo reference them
  met_info <- st_as_sf(met_info, coords = c("longitude", "latitude"), crs = 4326)

  ## find nearest meteo station to site
  sites$nearest_NOAA <- met_info$code[st_nearest_feature(sites, met_info)]

  all_met_sites <- unique(sites$nearest_NOAA)

  ## find data range
  d8s <- seq(start_date, end_date)
  all_met <- list()

  for (m in all_met_sites){
    tryCatch({
      data_met <- importNOAA(m, d8s) %>%
        select(code, station, date, latitude, longitude, elev, ws, wd, air_temp, atmos_pres,RH, ceil_hgt)

      all_met[[m]] <- data_met
      print(m)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }

  obs_met <- do.call(rbind, all_met)


}
