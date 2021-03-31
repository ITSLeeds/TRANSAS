library(dplyr)

dir.create("tmp")
unzip("D:/OneDrive - University of Leeds/Data/LSOA Flow Data/Public/WM12EW[CT0489]_lsoa.zip",
      exdir = "tmp")
od <- readr::read_csv("tmp/WM12EW[CT0489]_lsoa.csv")
unlink("tmp", recursive = TRUE)

od <- od[,c("Area of usual residence","Area of Workplace",names(od)[grepl("AllSexes_Age16Plus",names(od))])]
names(od) <- gsub("_AllSexes_Age16Plus","",names(od))
names(od)[1:2] <- c("LSOA_from","LSOA_to")

od$LSOA_to <- NULL
od_summ <- od %>%
  group_by(LSOA_from) %>%
  summarise_all(sum, na.rm = TRUE)

od_summ[3:13] <- lapply(od_summ[3:13], function(x){
  round(x / od_summ$AllMethods * 100, 2)
})

saveRDS(od_summ,"data/travel2work.Rds")


jt2_scot <- read.csv("D:/OneDrive - University of Leeds/Data/Scotland Census/SNS Data Zone 2011 std/QS702SC.csv",
                     header = FALSE)

names(jt2_scot) <- jt2_scot[4,]
names(jt2_scot)[1] <- "DataZone"
jt2_scot <- jt2_scot[6:(nrow(jt2_scot) -4),]
jt2_scot[2:13] <- lapply(jt2_scot[2:13], as.numeric)
names(jt2_scot) <- c("DataZone","AllMethods","WorkAtHome",
                     "Underground", "Train","Bus",
                     "Taxi","CarOrVan","Passenger",
                     "Motorcycle","Bicycle","OnFoot",
                     "Other")

jt2_scot[3:13] <- lapply(jt2_scot[3:13], function(x){
  round(x / jt2_scot$AllMethods * 100, 2)
})

saveRDS(jt2_scot,"data/travel2work_scot.Rds")

foo = rowSums(jt2_scot[,3:13], na.rm = TRUE)


