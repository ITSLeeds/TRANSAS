path = "D:/OneDrive - University of Leeds/Data/opentripplanner/graphs/great-britain/busGB-2020-03-26-clean.zip"

library(UK2GTFS)
gtfs = gtfs_read(path)

gtfs = gtfs_interpolate_times(gtfs, 30)
gtfs_validate_internal(gtfs)

gtfs_write(gtfs, "D:/OneDrive - University of Leeds/Data/opentripplanner/graphs/great-britain2", "busGB-2020-03-26-interpolated")
