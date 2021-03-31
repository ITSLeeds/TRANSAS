library(dplyr)
library(sf)
library(readxl)
library(tmap)
tmap_mode("view")

jobs <- read_excel("D:/OneDrive - University of Leeds/Data/Business Register and Employment/nomis_scotland_employees.xlsx")
names(jobs) <- c("DataZone","jobs","Flag")
jobs <- jobs[10:6985,]
jobs$DataZone <- substr(jobs$DataZone,1,9)
jobs$Flag <- NULL
jobs$jobs <- as.numeric(jobs$jobs)


dz <- read_sf("data/datazone_centroids_mod.gpkg")
summary(jobs$DataZone %in% dz$DataZone)

jobs <- left_join(dz, jobs, by = "DataZone")
summary(jobs$jobs)

jobs_100_500 <- jobs[jobs$jobs >= 100 & jobs$jobs < 500, ]
jobs_500_5000 <- jobs[jobs$jobs >= 500 & jobs$jobs < 5000, ]
jobs_5000p <- jobs[jobs$jobs >= 5000, ]

saveRDS(jobs_100_500, "data/jobs_100_500_scotland.Rds")
saveRDS(jobs_500_5000, "data/jobs_500_5000_scotland.Rds")
saveRDS(jobs_5000p, "data/jobs_5000p_scotland.Rds")

tm_shape(jobs_5000p) +
  tm_dots(col = "jobs")
