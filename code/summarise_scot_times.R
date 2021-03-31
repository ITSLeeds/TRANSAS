library(sf)
library(dplyr)
library(tmap)
tmap_mode("view")

dz <- read_sf("data/datazone_centroids_mod.gpkg")

# GP
r_gp_drive <- st_drop_geometry(readRDS("data/routes_scotland_driving_gp.Rds"))
r_gp_drive <- r_gp_drive[,c("fromPlace","duration")]
r_gp_drive <- r_gp_drive %>%
  group_by(DataZone = fromPlace) %>%
  summarise(gp_drive_time = round(min(duration) / 60, 0))


r_gp_transit <- st_drop_geometry(readRDS("data/routes_scotland_transit_gp.Rds"))
r_gp_transit <- r_gp_transit[,c("fromPlace","duration")]
r_gp_transit <- r_gp_transit %>%
  group_by(DataZone = fromPlace) %>%
  summarise(gp_transit_time = round(min(duration) / 60, 0))


# Primary Schools
r_school_primary_drive <- st_drop_geometry(readRDS("data/routes_scotland_driving_primary_school.Rds"))
r_school_primary_drive <- r_school_primary_drive[,c("fromPlace","duration")]
r_school_primary_drive <- r_school_primary_drive %>%
  group_by(DataZone = fromPlace) %>%
  summarise(ps_drive_time = round(min(duration) / 60, 0))

r_school_primary_cycle <- st_drop_geometry(readRDS("data/routes_scotland_cycle_primary_school.Rds"))
r_school_primary_cycle <- r_school_primary_cycle[,c("fromPlace","duration")]
r_school_primary_cycle <- r_school_primary_cycle %>%
  group_by(DataZone = fromPlace) %>%
  summarise(ps_cycle_time = round(min(duration) / 60, 0))

# secondary Schools
r_school_secondary_drive <- st_drop_geometry(readRDS("data/routes_scotland_driving_secondary_school.Rds"))
r_school_secondary_drive <- r_school_secondary_drive[,c("fromPlace","duration")]
r_school_secondary_drive <- r_school_secondary_drive %>%
  group_by(DataZone = fromPlace) %>%
  summarise(ss_drive_time = round(min(duration) / 60, 0))




# Food
# something wrong with food times
# r_food_drive <- st_drop_geometry(readRDS("data/routes_scotland_driving_food.Rds"))
# r_food_drive <- r_food_drive[,c("fromPlace","duration")]
# r_food_drive <- r_food_drive %>%
#   group_by(DataZone = fromPlace) %>%
#   summarise(food_drive_time = round(min(duration) / 60, 0))



dz <- left_join(dz, r_school_primary_cycle, by = "DataZone")
dz <- left_join(dz, r_school_primary_drive, by = "DataZone")
dz <- left_join(dz, r_school_secondary_drive, by = "DataZone")
dz <- left_join(dz, r_gp_drive, by = "DataZone")
dz <- left_join(dz, r_gp_transit, by = "DataZone")
#dz <- left_join(dz, r_food_drive, by = "DataZone")

saveRDS(st_drop_geometry(dz),"data/scotland_traveltimes_v1.Rds")

stop()
qtm(dz, dots.col = "ps_cycle_time")
qtm(dz, dots.col = "gp_drive_time")
qtm(dz, dots.col = "gp_transit_time")
#qtm(dz, dots.col = "food_drive_time")
qtm(dz, dots.col = "ss_drive_time")
