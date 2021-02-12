ahah <- read.csv("D:/OneDrive - University of Leeds/Data/AHAH/allvariableslsoawdeciles.csv")

ahah <- ahah[,c("lsoa11","gpp_dist","pharm_dist","blue_dist",
                "green_pas",
                "green_act","no2_mean","pm10_mean",
                "so2_mean")]
names(ahah) <- c("lsoa11","gpp_dist","pharm_dist","blue_dist",
                 "green_pas_km2",
                 "green_act_dist","no2_mean","pm10_mean",
                 "so2_mean")
saveRDS(ahah, "data/AccesHeathAndHazard.Rds")
