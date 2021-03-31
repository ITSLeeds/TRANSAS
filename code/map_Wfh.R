library(dplyr)
library(sf)
library(tmap)
tmap_mode("view")

source("../../creds2/CarbonCalculator/R/secure_path.R")
postcodes <- readRDS(paste0(substr(secure_path,1,39),"Postcodes/code_point_open.Rds"))
postcodes$postcode <- gsub(" ","",postcodes$postcode)

transas <- readxl::read_excel("data/transas_id_with_data_plusscotland plus VI Plus WFH Levels.xls")
transas <- transas[,c("ID","WFH_Levels_Before_3Cat","WFH_Levels_W2_3Cat","WFH_Change_5Cat")]

pcmatch <- read.csv("data/transas_id_with_data_plusscotland.csv")
pcmatch <- pcmatch[,c("ID","postcode_clean")]

transas <- left_join(transas, pcmatch, by = "ID")
transas <- left_join(transas, postcodes[,"postcode"],
                     by = c("postcode_clean" = "postcode"))

transas <- st_as_sf(transas)

transas_plot <- transas[!st_is_empty(transas),]
transas_noplot <- transas[st_is_empty(transas),]

transas_plot$WFH_Change_5Cat <- as.character(transas_plot$WFH_Change_5Cat)
transas_plot$WFH_Levels_Before_3Cat <- as.character(transas_plot$WFH_Levels_Before_3Cat)
transas_plot$WFH_Levels_W2_3Cat <- as.character(transas_plot$WFH_Levels_W2_3Cat)

tm_shape(transas_plot[!is.na(transas_plot$WFH_Change_5Cat),]) +
  tm_dots(col = "WFH_Change_5Cat")


tm_shape(transas_plot[!is.na(transas_plot$WFH_Levels_Before_3Cat),]) +
  tm_dots(col = "WFH_Levels_Before_3Cat")

tm_shape(transas_plot[!is.na(transas_plot$WFH_Levels_W2_3Cat),]) +
  tm_dots(col = "WFH_Levels_W2_3Cat")


d1 <- dmy("11/10/21")
d2 <- dmy("30/11/21")
s1 <- seq(d1, d2, "days")
s1 <- as.character(wday(s1, label = TRUE))
length(s1[!s1 %in% c("Sat","Sun")])
