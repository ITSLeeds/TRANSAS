library(opentripplanner)
library(tmap)
library(dplyr)
library(sf)
library(tidyr)
tmap_mode("view")

path_data = "D:/OneDrive - University of Leeds/Data/opentripplanner"
path_opt = "D:/OneDrive - University of Leeds/Data/opentripplanner/otp-1.5.0-shaded.jar"

# log2 = otp_setup(path_opt,
#                  path_data,
#                  router = "great-britain",
#                  port = 8091,
#                  securePort = 8092,
#                  analyst = TRUE,
#                  memory = 80000,
#                  quiet = FALSE)

otpcon <- otp_connect(router = "great-britain2", port = 8091)

#java -Xmx110000M -d64 -jar "D:/OneDrive - University of Leeds/Data/opentripplanner/otp-1.5.0-shaded.jar" --router great-britain2 --graphs "D:/OneDrive - University of Leeds/Data/opentripplanner/graphs" --server --port 8091 --securePort 8092 --analyst --pointSets "D:/OneDrive - University of Leeds/Data/opentripplanner/pointsets"

dz <- read_sf("data/datazone_centroids_mod.gpkg")
dz2 <- dz

buff <- st_buffer(st_transform(dz,27700),40000) # Make quick Scotland polygon
buff <- st_union(buff)
buff <- st_transform(buff, 4326)
buff <- st_as_sf(data.frame(geometry = buff))

# Make the toPlace Pointset
schools <- readRDS("data/schools_scotland.Rds")
schools_primary <- schools[schools$SchType == "Primary", ]
schools_secondary <- schools[schools$SchType == "Secondary", ]
food <- readRDS("data/supermarkets.Rds")
food <- food[buff,]
gp <- readRDS("data/GP-Scotland.Rds")
town <- readRDS("data/towns_scotland.Rds")
pharm <- readRDS("data/pharmacy_scotland.Rds")
pharm <- pharm[!duplicated(pharm$uprn),]

schools_primary$id <- paste0("pschool_",schools_primary$SchUID)
schools_secondary$id <- paste0("sschool_",schools_secondary$SchUID)
food$id <- paste0("food_",food$id)
gp$id <- paste0("gp_",gp$PracticeCode)
town$id <- paste0("town_",town$id)
pharm$id <- paste0("pharm_",pharm$uprn)
dz2$id <- paste0("dz_",dz2$DataZone)

schools_primary <- schools_primary[,"id"]
schools_secondary <- schools_secondary[,"id"]
food <- food[,"id"]
gp <- gp[,"id"]
town <- town[,"id"]
pharm <- pharm[,"id"]
dz2 <- dz2[,"id"]

dz2$geometry <- dz2$geom
dz2$geom <- NULL

gp$geometry <- gp$geom
gp$geom <- NULL

toPlace <- bind_rows(list(schools_primary, schools_secondary, food,
                          gp, town, pharm, dz2))



ttimes_car <- otp_traveltime(otpcon = otpcon,
                          path_data = path_data,
                         fromPlace = dz,
                         toPlace = toPlace,
                         fromID = dz$DataZone,
                         toID = toPlace$id,
                         mode = "CAR",
                         ncores = 20)
saveRDS(ttimes_car,"data/ttmatrix_scotland_drive.Rds")

# missing points
rs <- rowSums(ttimes_car, na.rm = TRUE)
miss <- rownames(ttimes_car)[rs == 0]
miss <- toPlace[toPlace$id %in% miss,]

toPlace$need_mod <- toPlace$id %in% miss$id
st_write(toPlace,"data/scotland_desinations_raw.gpkg")

# Do With Improved locations
toPlace <- st_read("data/scotland_desinations_mod.gpkg")

# Limit fromPlaces to survey areas
transas <- read.csv("data/transas_id_with_data.csv")
transas <- unique(transas$LSOA_2011)
transas <- transas[!is.na(transas)]

fromPlace <- dz[dz$DataZone %in% transas,]

ttimes_bike <- otp_traveltime(otpcon = otpcon,
                             path_data = path_data,
                             fromPlace = fromPlace,
                             toPlace = toPlace,
                             fromID = fromPlace$DataZone,
                             toID = toPlace$id,
                             mode = "BICYCLE",
                             ncores = 28)
saveRDS(ttimes_bike,"data/ttmatrix_scotland_bike_v2.Rds")



ttimes_transit <- otp_traveltime(otpcon = otpcon,
                             path_data = path_data,
                             fromPlace = fromPlace,
                             toPlace = toPlace,
                             fromID = fromPlace$DataZone,
                             toID = toPlace$id,
                             mode = c("WALK","TRANSIT"),
                             date_time = lubridate::ymd_hms("2020-06-18 08:30:00"),
                             ncores = 28)
saveRDS(ttimes_transit,"data/ttmatrix_scotland_transit_v2.Rds")

ttimes_car <- otp_traveltime(otpcon = otpcon,
                             path_data = path_data,
                             fromPlace = fromPlace,
                             toPlace = toPlace,
                             fromID = fromPlace$DataZone,
                             toID = toPlace$id,
                             mode = "CAR",
                             ncores = 28)
saveRDS(ttimes_car,"data/ttmatrix_scotland_car_v3.Rds")
