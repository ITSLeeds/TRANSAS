remotes::install_github("ropensci/opentripplanner", ref = 'otp2')
library(opentripplanner)
library(UK2GTFS)
library(sf)
library(tmap)
tmap_mode("view")

source("../../creds2/CarbonCalculator/R/secure_path.R")





gtfs = gtfs_read("D:/OneDrive - University of Leeds/Data/UK2GTFS/GTFS/gtfs_20200326/AllGB.zip")
gtfs = gtfs_force_valid(gtfs)
gtfs_write(gtfs, "D:/OneDrive - University of Leeds/Data/opentripplanner/graphs/great-britain/",
           name = "busGB-2020-03-26")

path_data = "D:/OneDrive - University of Leeds/Data/opentripplanner"
path_opt = "D:/OneDrive - University of Leeds/Data/opentripplanner/otp-1.5.0-shaded.jar"

opts_build = otp_make_config(type = "build")
opts_build$matchBusRoutesToStreets = TRUE
opts_build$osmWayPropertySet = "uk"
opts_build$dataImportReport = TRUE
otp_validate_config(opts_build)
otp_write_config(opts_build, path_data, "great-britain")

opts_router = otp_make_config(type = "router")
opts_router$routingDefaults$driveOnRight = FALSE
opts_router$timeouts <- c(20,10,5,3)

otp_validate_config(opts_router)
otp_write_config(opts_router, path_data, "great-britain")

otp_dl_demo("D:/OneDrive - University of Leeds/Data/opentripplanner")

log1 = otp_build_graph(path_opt,
                path_data,
                memory = 80000,
                router = "great-britain")

# Parse the logs

log_summary <- substr(log1, 14, nchar(log1))
log_summary <- substr(log_summary, 1, 65)
log_summary <- as.data.frame(table(log_summary))
log_summary <- log_summary[order(log_summary$Freq, decreasing = TRUE),]

log2 = otp_setup(path_opt,
          path_data,
          router = "great-britain",
          securePort = 8082,
          analyst = TRUE,
          memory = 80000,
          quiet = FALSE)

otpcon <- otp_connect(router = "great-britain")







postcodes <- read.csv("data/postcode_lookup.csv")
cp  <- readRDS(paste0(substr(secure_path,1,39),"Postcodes/code_point_open.Rds"))
cp$postcode <- gsub(" ","",cp$postcode)
cp <- cp[cp$postcode %in% postcodes$postcode_clean,]
sf::st_geometry(cp) <- sf::st_as_sfc(cp$geometry)
sf::st_crs(cp) <- 27700
cp <- sf::st_transform(cp, 4326)


isos2 <- otp_isochrone(otpcon, cp, mode = "CAR",
                      cutoffSec = 60 * c(10,15,20,25, 30), ncores = 30)

foo <- isos2[isos2$time == 1800,]

tm_shape(foo[1,]) +
  tm_fill("time", alpha = 0.8)

