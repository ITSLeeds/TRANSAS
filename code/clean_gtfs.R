path = "D:/OneDrive - University of Leeds/Data/opentripplanner/graphs/great-britain/busGB-2020-03-26-clean.zip"
remotes::install_github("itsleeds/uk2gtfs")
library(UK2GTFS)
library(sf)
library(tmap)
tmap_mode("view")
gtfs = gtfs_read(path)

stop_times <- gtfs$stop_times

stop_times$arrival_time_char <- UK2GTFS:::period2gtfs(stop_times$arrival_time)
stop_times$nchar <- nchar(stop_times$arrival_time_char)
summary(stop_times$nchar)

foo = stop_times[stop_times$nchar != 8,]
problems = unique(foo$trip_id)

stop_times <- stop_times[stop_times$trip_id %in% problems,]
stop_times_orig <- stop_times

stops <- gtfs$stops[gtfs$stops$stop_id %in% foo$stop_id,]
stops <- st_as_sf(stops, coords = c("stop_lon","stop_lat"), crs = 4326)

x = stop_times[stop_times$trip_id == 1000212,]


gtfs2 = gtfs_interpolate_times(gtfs, 1)
gtfs_validate_internal(gtfs2)

gtfs_write(gtfs2, "D:/OneDrive - University of Leeds/Data/opentripplanner/graphs/great-britain2", "busGB-2020-03-26-interpolated2")


stop_times = read.csv("D:/OneDrive - University of Leeds/Data/opentripplanner/graphs/great-britain2/busGB-2020-03-26-interpolated/stop_times.txt",
                      stringsAsFactors = FALSE)
stop_times$nchar <- nchar(stop_times$arrival_time)
