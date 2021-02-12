
library(UK2GTFS)
library(dplyr)
library(sf)
library(tmap)
tmap_mode("plot")

# files <- list.files("D:/OneDrive - University of Leeds/Data/UK2GTFS/GTFS/gtfs_20200326", full.names = TRUE)
#
# gtfs <- list()
#
# for(i in 1:length(files)){
#   message(i)
#   gtfs[[i]] <- gtfs_read(files[i])
# }
#
# gtfs <- gtfs_merge(gtfs, force = TRUE)
# gtfs_write(gtfs, "D:/OneDrive - University of Leeds/Data/UK2GTFS/GTFS/gtfs_20200326", "AllGB")

#gtfs <- gtfs_read("D:/OneDrive - University of Leeds/Data/UK2GTFS/GTFS/gtfs_20200326/AllGB.zip")
gtfs <- gtfs_read("D:/OneDrive - University of Leeds/Data/UK2GTFS/OpenBusData/GTFS/20210229/itm_all_gtfs.zip")
gtfs_validate_internal(gtfs)
gtfs <- gtfs_force_valid(gtfs)
#gtfs_write(gtfs, "D:/OneDrive - University of Leeds/Data/UK2GTFS/GTFS/gtfs_20200326/", "AllGBvalid")

foo <- gtfs$stops[is.na(gtfs$stops$stop_lat),]


make_trip_geoms <- function(gtfs){
  stops <- gtfs$stops
  stop_times <- gtfs$stop_times
  #routes <- gtfs$routes
  trips <- gtfs$trips
  #calendar <- gtfs$calendar

  stops <- stops[,c("stop_id","stop_lon","stop_lat")]
  stop_times <- dplyr::left_join(stop_times, stops, by = "stop_id")


  stop_times <- split(stop_times, stop_times$trip_id)

  make_geom <- function(x){
    geom <- x[,c("stop_lon", "stop_lat")]
    geom <- as.matrix(geom)
    geom <- sf::st_linestring(geom, dim = "XY")
    return(geom)
  }

  geom <- pbapply::pblapply(stop_times, make_geom)

  trips_geom <- sf::st_as_sf(data.frame(trip_id = names(stop_times),
                                        geometry = sf::st_sfc(geom, crs = 4326),
                                        stringsAsFactors = FALSE))

  return(trips_geom)

}




countwd2 <- function(startdate, enddate, weekday){
  if(is.na(startdate)){
    return(0)
  }
  if(is.na(enddate)){
    return(0)
  }
  d <- as.integer(enddate - startdate) + 1
  d %/% 7 +
    (weekday %in% weekdays(seq(startdate, length.out=d %% 7, by=1)))
}


count_stops <- function(gtfs, startdate, enddate){
  stop_times <- gtfs$stop_times
  trips <- gtfs$trips
  calendar <- gtfs$calendar
  calendar_days <- gtfs$calendar_dates

  calendar_days <- calendar_days %>%
    dplyr::group_by(service_id) %>%
    dplyr::summarise(runs_extra = sum(exception_type == 1),
                     runs_canceled = sum(exception_type == 2))

  # work out how many times the trip in run
  trips <- dplyr::left_join(trips, calendar, by = "service_id")
  trips <- dplyr::left_join(trips, calendar_days, by = "service_id")

  trips$runs_canceled[is.na(trips$runs_canceled)] <- 0
  trips$runs_extra[is.na(trips$runs_extra)] <- 0

  message("Counting trips on each day")
  future::plan("future::multisession")

  trips$n_monday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Monday")
  trips$n_tuesday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Tuesday")
  trips$n_wednesday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Wednesday")
  trips$n_thursday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Thursday")
  trips$n_friday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Friday")
  trips$n_saturday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Saturday")
  trips$n_sunday <- future.apply::future_mapply(countwd2, startdate = trips$start_date, enddate = trips$end_date, weekday = "Sunday")

  future::plan("future::sequential")

  trips$runs_monday <- trips$monday * trips$n_monday
  trips$runs_tuesday <- trips$tuesday * trips$n_tuesday
  trips$runs_wednesday <- trips$wednesday * trips$n_wednesday
  trips$runs_thursday <- trips$thursday * trips$n_thursday
  trips$runs_friday <- trips$friday * trips$n_friday
  trips$runs_saturday <- trips$saturday * trips$n_saturday
  trips$runs_sunday <- trips$sunday * trips$n_sunday

  message("Summariseing results")
  trips$runs_total <- trips$runs_monday + trips$runs_tuesday +
    trips$runs_wednesday + trips$runs_thursday + trips$runs_friday +
    trips$runs_saturday + trips$runs_sunday + trips$runs_extra - trips$runs_canceled

  trips$runs_per_week <- trips$runs_total / ((as.numeric(trips$end_date - trips$start_date) + 1)/7)

  # Catch Single Day services
  trips$runs_per_week <- ifelse(trips$start_date == trips$end_date, 1, trips$runs_per_week)

  trips <- trips[,c("trip_id","start_date","end_date","runs_total","runs_per_week")]
  stop_times <- dplyr::left_join(stop_times, trips, by = "trip_id")
  stop_times_summary <- stop_times %>%
    dplyr::group_by(stop_id) %>%
    dplyr::summarise(stops_total = sum(runs_total),
                     stops_per_week = sum(runs_per_week))

  stops <- dplyr::left_join(gtfs$stops, stop_times_summary, by = "stop_id")
  return(stops)
}



stops_with_count = count_stops(gtfs)
summary(stops_with_count$stops_per_week)

foo = stops_with_count[!is.na(stops_with_count$stop_lon),]
foo = foo[!is.infinite(foo$stops_per_week),]
foo = foo[!is.na(foo$stops_per_week),]
foo = st_as_sf(foo, coords = c("stop_lon","stop_lat"), crs = 4326)
summary(foo$stops_per_week)
foo <- foo[,c("stop_code","stop_name","stops_per_week","geometry")]

st_write(foo,"../../creds2/CarbonCalculator/data/busstops/openbusdata.geojson")

bar <- foo[foo$stops_per_week == max(foo$stops_per_week),]

qtm()


# tm_shape(foo) +
#   tm_dots(col = "stops_per_week",
#           size = 0.01,
#           style = "quantile",
#           n = 10,
#           palette = "RdYlBu",
#           border.lwd = NA,
#           border.alpha = 0)


dir.create("tmp")
unzip("D:/OneDrive - University of Leeds/Data/OA Bounadries/GB_LSOA_2011_clipped.zip",
      exdir = "tmp")
bounds <- read_sf("tmp/infuse_lsoa_lyr_2011_clipped.shp")
unlink("tmp")

bounds <- st_transform(bounds, 4326)

foo2 <- st_join(foo, bounds)
foo2 <- st_drop_geometry(foo2)
foo2 <- foo2 %>%
  dplyr::group_by(geo_code) %>%
  dplyr::summarise(stops_total = sum(stops_total),
                   stops_per_week = sum(stops_per_week, na.rm = TRUE))

bounds2 <- dplyr::left_join(bounds, foo2, by = "geo_code")
bounds2 <- bounds2[!is.na(bounds2$stops_total),]
bounds2 <- sf::st_transform(bounds2, 27700)
bounds2 <- sf::st_simplify(bounds2, 100, preserveTopology = TRUE)

tm_shape(bounds2) +
  tm_fill(col = "stops_per_week",
          breaks = quantile(bounds2$stops_per_week, probs = c(seq(0,1,0.1))))

write_sf(bounds2,"lsoa_bus.gpkg")
write_sf(foo,"bus_stops.gpkg")

# foo = trips_geom[!duplicated(trips_geom$geometry),]
# qtm(foo, lines.col = "length_km")
#trip_join <- trips[,c("route_id","trip_id")]
#trip_join <- unique(trip_join)
# stop_times <- dplyr::left_join(stop_times, trip_join, by = "trip_id")
# stop_times_summary <- stop_times %>%
#   group_by(trip_id) %>%
#   summarise(stops = paste(stop_id, collapse = " "))
# summary(duplicated(stop_times_summary$trip_id))
# summary(duplicated(stop_times_summary$stops))


trips_geom <- make_trip_geoms(gtfs)
trips_geom$length_km <- as.numeric(st_length(trips_geom)) / 1000
# trips_geom$ntrip <- 1
# trips_overline <- stplanr::overline2(trips_geom, "ntrip")
# tm_shape(trips_overline) +
#   tm_lines(col = "ntrip",
#            lwd = 2,
#            breaks = c(0,1,10,20,30,40,50,60,70,80,100,1000,1600))


trips_local_id <- trips_geom$trip_id[trips_geom$length_km < 50]
trips_ld_id <- trips_geom$trip_id[trips_geom$length_km >= 50]

# Work out total stops per day
period_start <- min(calendar$start_date)
period_end <- max(calendar$end_date)
period_end - period_start
