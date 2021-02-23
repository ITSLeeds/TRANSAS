library(opentripplanner)
library(nngeo)
library(tmap)
tmap_mode("view")

path_data = "D:/OneDrive - University of Leeds/Data/opentripplanner"
path_opt = "D:/OneDrive - University of Leeds/Data/opentripplanner/otp-1.5.0-shaded.jar"

log2 = otp_setup(path_opt,
                 path_data,
                 router = "great-britain",
                 port = 8091,
                 securePort = 8092,
                 analyst = TRUE,
                 memory = 80000,
                 quiet = FALSE)

otpcon <- otp_connect(router = "great-britain", port = 8091)

#java -Xmx100000M -d64 -jar "D:/OneDrive - University of Leeds/Data/opentripplanner/otp-1.5.0-shaded.jar" --router great-britain --graphs "D:/OneDrive - University of Leeds/Data/opentripplanner/graphs" --server --port 8091 --securePort 8092 --analyst

gp <- readRDS("data/GP-Scotland.Rds")
#dz <- readRDS("data/datazone_centroids.Rds")
#write_sf(dz,"data/datazone_centroids_mod.gpkg")
dz <- read_sf("data/datazone_centroids_mod.gpkg")
#dz_bounds <- read_sf("../../creds2/Excess-Data-Exploration/data-prepared/LSOA_full.gpkg")
#dz_bounds <- dz_bounds[dz_bounds$LSOA11 %in% dz$DataZone,]
schools <- readRDS("data/schools_scotland.Rds")
schools_primary <- schools[schools$SchType == "Primary", ]
schools_secondary <- schools[schools$SchType == "Secondary", ]

# For each DataZone get the nearest 5 GPs

nn <- st_nn(dz, gp, k = 5)
summary(lengths(nn))
nn <- unlist(nn)
gp$PracticeCode <- as.character(gp$PracticeCode)

fromPlace <- dz[rep(1:nrow(dz), each = 5),]
toPlace <- gp[nn, ]

routes <- otp_plan(otpcon, fromPlace = fromPlace, toPlace = toPlace,
                   fromID = fromPlace$DataZone, toID = toPlace$PracticeCode,
                   mode = "CAR", ncores = 20, distance_balance = TRUE)

saveRDS(routes, "data/routes_scotland_driving_gp.Rds")


routes_cycle <- otp_plan(otpcon, fromPlace = fromPlace, toPlace = toPlace,
                   fromID = fromPlace$DataZone, toID = toPlace$PracticeCode,
                   mode = "BICYCLE", ncores = 30, distance_balance = TRUE)

saveRDS(routes_cycle, "data/routes_scotland_cycle_gp.Rds")

# fromPlace <- fromPlace[1:100,]
# toPlace <- toPlace[1:100,]

# Batched of 10,000
splitvec <- split(1:nrow(fromPlace), rep(1:4, each = nrow(fromPlace)/4))

res <- list()
for(i in 2:4){
  message(Sys.time()," ",i)
  routes_transit <- otp_plan(otpcon,
                             fromPlace = fromPlace[splitvec[[i]], ],
                             toPlace = toPlace[splitvec[[i]], ],
                             fromID = fromPlace$DataZone[splitvec[[i]]],
                             toID = toPlace$PracticeCode[splitvec[[i]]],
                             mode = c("TRANSIT","WALK"),
                             ncores = 30,
                             distance_balance = FALSE,
                             date_time = lubridate::ymd_hms("2020-06-18 08:30:00"))
  res[[i]] <- routes_transit
}


# routes_transit <- otp_plan(otpcon, fromPlace = fromPlace, toPlace = toPlace,
#                          fromID = fromPlace$DataZone, toID = toPlace$PracticeCode,
#                          mode = c("TRANSIT","WALK"), ncores = 30, distance_balance = FALSE,
#                          date_time = lubridate::ymd_hms("2020-06-18 08:30:00"))

res_final <- dplyr::bind_rows(res)


# saveRDS(res_final, "data/routes_scotland_transit_gp2.Rds")
#
# # COmbine the two parts
#
# par1 <- readRDS("data/routes_scotland_transit_gp.Rds")
# res_final <- rbind(par1, res_final)
# summary(duplicated(res_final))
saveRDS(res_final, "data/routes_scotland_transit_gp.Rds")

qtm(routes[1:10,])

miss_gp <- gp[!gp$PracticeCode %in% routes_transit$toPlace, ]
miss_dz <- dz[!dz$DataZone %in% routes_transit$fromPlace, ]

qtm(miss_dz, dots.col = "red") +
  #qtm(dz_bounds[dz_bounds$LSOA11 %in% miss_dz$DataZone,]) +
  qtm(gp)


# Primary Schools

nn <- st_nn(dz, schools_primary, k = 5)
nn <- unlist(nn)

fromPlace <- dz[rep(1:nrow(dz), each = 5),]
toPlace <- schools_primary[nn, ]

# Batched of 10,000
splitvec <- split(1:nrow(fromPlace), rep(1:4, each = nrow(fromPlace)/4))
routes <- list()
for(i in 1:4){
  message(Sys.time()," ",i)
  routes_part <- otp_plan(otpcon,
                             fromPlace = fromPlace[splitvec[[i]], ],
                             toPlace = toPlace[splitvec[[i]], ],
                             fromID = fromPlace$DataZone[splitvec[[i]]],
                             toID = toPlace$SchUID[splitvec[[i]]],
                             mode = c("CAR"),
                             ncores = 25,
                             distance_balance = TRUE)
  routes[[i]] <- routes_part
}

# routes <- otp_plan(otpcon, fromPlace = fromPlace, toPlace = toPlace,
#                    fromID = fromPlace$DataZone, toID = toPlace$SchUID,
#                    mode = "CAR", ncores = 28, distance_balance = TRUE)
routes_final <- dplyr::bind_rows(routes)
saveRDS(routes_final, "data/routes_scotland_driving_primary_school.Rds")

miss_ps <- schools_primary[!schools_primary$SchUID %in% routes_final$toPlace, ]
miss_dz <- dz[!dz$DataZone %in% routes_final$fromPlace, ]

qtm(miss_ps, dots.col = "red") +
  qtm(dz)

routes <- list()
for(i in 1:4){
  message(Sys.time()," ",i)
  routes_part <- otp_plan(otpcon,
                          fromPlace = fromPlace[splitvec[[i]], ],
                          toPlace = toPlace[splitvec[[i]], ],
                          fromID = fromPlace$DataZone[splitvec[[i]]],
                          toID = toPlace$SchUID[splitvec[[i]]],
                          mode = c("BICYCLE"),
                          ncores = 28,
                          distance_balance = TRUE)
  routes[[i]] <- routes_part
}

routes_final <- dplyr::bind_rows(routes)
saveRDS(routes_final, "data/routes_scotland_cycle_primary_school.Rds")



# seondary Schools

nn <- st_nn(dz, schools_secondary, k = 5)
nn <- unlist(nn)

fromPlace <- dz[rep(1:nrow(dz), each = 5),]
toPlace <- schools_secondary[nn, ]

# Batched of 10,000
splitvec <- split(1:nrow(fromPlace), rep(1:4, each = nrow(fromPlace)/4))
routes <- list()
for(i in 1:4){
  message(Sys.time()," ",i)
  routes_part <- otp_plan(otpcon,
                          fromPlace = fromPlace[splitvec[[i]], ],
                          toPlace = toPlace[splitvec[[i]], ],
                          fromID = fromPlace$DataZone[splitvec[[i]]],
                          toID = toPlace$SchUID[splitvec[[i]]],
                          mode = c("CAR"),
                          ncores = 25,
                          distance_balance = TRUE)
  routes[[i]] <- routes_part
}

# routes <- otp_plan(otpcon, fromPlace = fromPlace, toPlace = toPlace,
#                    fromID = fromPlace$DataZone, toID = toPlace$SchUID,
#                    mode = "CAR", ncores = 28, distance_balance = TRUE)
routes_final <- dplyr::bind_rows(routes)
saveRDS(routes_final, "data/routes_scotland_driving_secondary_school.Rds")

miss_ps <- schools_secondary[!schools_secondary$SchUID %in% routes_final$toPlace, ]
miss_dz <- dz[!dz$DataZone %in% routes_final$fromPlace, ]

qtm(miss_ps, dots.col = "red") +
  qtm(dz)

routes <- list()
for(i in 1:4){
  message(Sys.time()," ",i)
  routes_part <- otp_plan(otpcon,
                          fromPlace = fromPlace[splitvec[[i]], ],
                          toPlace = toPlace[splitvec[[i]], ],
                          fromID = fromPlace$DataZone[splitvec[[i]]],
                          toID = toPlace$SchUID[splitvec[[i]]],
                          mode = c("BICYCLE"),
                          ncores = 28,
                          distance_balance = TRUE)
  routes[[i]] <- routes_part
}

routes_final <- dplyr::bind_rows(routes)
saveRDS(routes_final, "data/routes_scotland_cycle_secondary_school.Rds")


