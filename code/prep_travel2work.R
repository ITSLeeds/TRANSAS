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
