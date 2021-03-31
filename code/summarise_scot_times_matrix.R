library(dplyr)
library(sf)
library(readxl)
library(tmap)
library(tidyr)
tmap_mode("view")


ttimes_bike <- readRDS("data/ttmatrix_scotland_bike_v2.Rds")
ttimes_car <- readRDS("data/ttmatrix_scotland_car_v3.Rds")
ttimes_transit <- readRDS("data/ttmatrix_scotland_transit_v2.Rds")

jobs_100_500 <- readRDS("data/jobs_100_500_scotland.Rds")
jobs_500_5000 <- readRDS("data/jobs_500_5000_scotland.Rds")
jobs_5000p <- readRDS("data/jobs_5000p_scotland.Rds")

dz <- read_sf("data/datazone_centroids_mod.gpkg")

toPlace <- st_read("data/scotland_desinations_mod.gpkg")
# Limit fromPlaces to survey areas
transas <- read.csv("data/transas_id_with_data.csv")
transas <- unique(transas$LSOA_2011)
transas <- transas[!is.na(transas)]

fromPlace <- dz[dz$DataZone %in% transas,]


matrix2long <- function(mat, nms){
  mat <- mat[,c("DataZone",names(mat)[grep(nms,names(mat))])]
  mat <- pivot_longer(mat,
                      cols = starts_with(nms),
                      names_to = "id",
                      values_to = "time",
                      values_drop_na = TRUE
  )
  mat <- mat %>%
    group_by(DataZone) %>%
    summarise(nms = round(min(time, na.rm = TRUE) / 60,2))
  names(mat)[2] <- nms
  return(mat)

}


matrix2jobs <- function(mat, jobs100, jobs500, jobs5000){
  mat <- mat[,names(mat)[names(mat) %in% c("DataZone",paste0("dz_",c(jobs100,jobs500,jobs5000)))] ]
  mat <- pivot_longer(mat,
                      cols = starts_with("dz_"),
                      names_to = "id",
                      values_to = "time",
                      values_drop_na = TRUE
  )
  mat$cat <- ""
  mat$cat <- ifelse(mat$id %in% paste0("dz_",jobs100),"jobs100",mat$cat)
  mat$cat <- ifelse(mat$id %in% paste0("dz_",jobs500),"jobs500",mat$cat)
  mat$cat <- ifelse(mat$id %in% paste0("dz_",jobs5000),"jobs5000",mat$cat)

  mat <- mat %>%
    group_by(DataZone, cat) %>%
    summarise(nms = round(min(time, na.rm = TRUE) / 60,2))

  mat <- pivot_wider(mat, names_from = "cat", values_from = "nms")
  return(mat)
}


# missing points
rs <- rowSums(ttimes_transit, na.rm = TRUE)
miss_transit <- rownames(ttimes_transit)[rs == 0]
rs <- rowSums(ttimes_car, na.rm = TRUE)
miss_car <- rownames(ttimes_car)[rs == 0]
rs <- rowSums(ttimes_bike, na.rm = TRUE)
miss_bike <- rownames(ttimes_bike)[rs == 0]
miss <- unique(c(miss_transit,miss_car,miss_bike))

miss <- toPlace
miss$car <- miss$id %in% miss_car
miss$bike <- miss$id %in% miss_bike
miss$transit <- miss$id %in% miss_transit

tm_shape(miss) +
  tm_dots(col = "bike", palette=c('cyan','red')) +
  tm_shape(fromPlace$geom) +
  tm_dots(col = "black", size = 0.01)


# Summarise Results
ttimes_car <- as.data.frame(t(ttimes_car))
ttimes_car$DataZone <- rownames(ttimes_car)
ttimes_car <- ttimes_car_long[,sapply(ttimes_car,
                                           function(x){!all(is.na(x))},
                                           USE.NAMES = FALSE)]

ttimes_bike <- as.data.frame(t(ttimes_bike))
ttimes_bike$DataZone <- rownames(ttimes_bike)
ttimes_bike <- ttimes_bike[,sapply(ttimes_bike,
                                   function(x){!all(is.na(x))},
                                   USE.NAMES = FALSE)]

ttimes_transit <- as.data.frame(t(ttimes_transit))
ttimes_transit$DataZone <- rownames(ttimes_transit)
ttimes_transit <- ttimes_transit[,sapply(ttimes_transit,
                                         function(x){!all(is.na(x))},
                                         USE.NAMES = FALSE)]




ttimes_bike_pschool <- matrix2long(ttimes_bike, "pschool")
ttimes_bike_sschool <- matrix2long(ttimes_bike, "sschool")
ttimes_bike_food <- matrix2long(ttimes_bike, "food")
ttimes_bike_town <- matrix2long(ttimes_bike, "town")
ttimes_bike_pharm <- matrix2long(ttimes_bike, "pharm")
ttimes_bike_gp <- matrix2long(ttimes_bike, "gp")
ttimes_bike_emp <- matrix2jobs(ttimes_bike,
                               jobs100 = jobs_100_500$DataZone,
                               jobs500 = jobs_500_5000$DataZone,
                               jobs5000 = jobs_5000p$DataZone)

ttimes_bike_summary <- left_join(ttimes_bike_pschool, ttimes_bike_sschool, by = "DataZone")
ttimes_bike_summary <- left_join(ttimes_bike_summary, ttimes_bike_food, by = "DataZone")
ttimes_bike_summary <- left_join(ttimes_bike_summary, ttimes_bike_town, by = "DataZone")
ttimes_bike_summary <- left_join(ttimes_bike_summary, ttimes_bike_pharm, by = "DataZone")
ttimes_bike_summary <- left_join(ttimes_bike_summary, ttimes_bike_gp, by = "DataZone")
ttimes_bike_summary <- left_join(ttimes_bike_summary, ttimes_bike_emp, by = "DataZone")

ttimes_transit_pschool <- matrix2long(ttimes_transit, "pschool")
ttimes_transit_sschool <- matrix2long(ttimes_transit, "sschool")
ttimes_transit_food <- matrix2long(ttimes_transit, "food")
ttimes_transit_town <- matrix2long(ttimes_transit, "town")
ttimes_transit_pharm <- matrix2long(ttimes_transit, "pharm")
ttimes_transit_gp <- matrix2long(ttimes_transit, "gp")
ttimes_transit_emp <- matrix2jobs(ttimes_transit,
                               jobs100 = jobs_100_500$DataZone,
                               jobs500 = jobs_500_5000$DataZone,
                               jobs5000 = jobs_5000p$DataZone)

ttimes_transit_summary <- left_join(ttimes_transit_pschool, ttimes_transit_sschool, by = "DataZone")
ttimes_transit_summary <- left_join(ttimes_transit_summary, ttimes_transit_food, by = "DataZone")
ttimes_transit_summary <- left_join(ttimes_transit_summary, ttimes_transit_town, by = "DataZone")
ttimes_transit_summary <- left_join(ttimes_transit_summary, ttimes_transit_pharm, by = "DataZone")
ttimes_transit_summary <- left_join(ttimes_transit_summary, ttimes_transit_gp, by = "DataZone")
ttimes_transit_summary <- left_join(ttimes_transit_summary, ttimes_transit_emp, by = "DataZone")

ttimes_car_pschool <- matrix2long(ttimes_car, "pschool")
ttimes_car_sschool <- matrix2long(ttimes_car, "sschool")
ttimes_car_food <- matrix2long(ttimes_car, "food")
ttimes_car_town <- matrix2long(ttimes_car, "town")
ttimes_car_pharm <- matrix2long(ttimes_car, "pharm")
ttimes_car_gp <- matrix2long(ttimes_car, "gp")
ttimes_car_emp <- matrix2jobs(ttimes_car,
                                  jobs100 = jobs_100_500$DataZone,
                                  jobs500 = jobs_500_5000$DataZone,
                                  jobs5000 = jobs_5000p$DataZone)

ttimes_car_summary <- left_join(ttimes_car_pschool, ttimes_car_sschool, by = "DataZone")
ttimes_car_summary <- left_join(ttimes_car_summary, ttimes_car_food, by = "DataZone")
ttimes_car_summary <- left_join(ttimes_car_summary, ttimes_car_town, by = "DataZone")
ttimes_car_summary <- left_join(ttimes_car_summary, ttimes_car_pharm, by = "DataZone")
ttimes_car_summary <- left_join(ttimes_car_summary, ttimes_car_gp, by = "DataZone")
ttimes_car_summary <- left_join(ttimes_car_summary, ttimes_car_emp, by = "DataZone")

saveRDS(ttimes_bike_summary, "data/traveltime_bike_summary.Rds")
saveRDS(ttimes_transit_summary, "data/traveltime_transit_summary.Rds")
saveRDS(ttimes_car_summary, "data/traveltime_car_summary.Rds")



# foo <- left_join(dz, ttimes_transit_pschool, by = "DataZone")
# foo <- foo[!is.na(foo$pschool),]
# qtm(foo, dots.col = "pschool") +
#   qtm(schools_primary$geometry)

# ttimes_car_pschool <- ttimes_car_long[,c("DataZone",names(ttimes_car_long)[grep("pschool",names(ttimes_car_long))])]
#
# ttimes_car_pschool <- pivot_longer(ttimes_car_pschool,
#                                 cols = starts_with("pschool_"),
#                                 names_to = "id",
#                                 values_to = "time",
#                                 values_drop_na = TRUE
#                                 )
#
#
# ttimes_car_pschool <- ttimes_car_pschool %>%
#   group_by(DataZone) %>%
#   summarise(PSCart = min(time, na.rm = TRUE))

