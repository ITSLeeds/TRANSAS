# postocde matching
library(sf)
library(dplyr)
library(purrr)
library(tmap)
tmap_mode("view")

# Get transaas postcodes
transas <- readxl::read_xlsx("data/Transas postcodes for house movers_W3.xlsx")
transas <- unique(transas$`New House Postcode`)
transas <- transas[!is.na(transas)]

#clean format
transas <- data.frame(postcode_original = transas,
                      postcode_clean = toupper(gsub(" ","",transas)),
                      stringsAsFactors = FALSE)



source("../../creds2/CarbonCalculator/R/secure_path.R")
postcodes <- readRDS(paste0(substr(secure_path,1,39),"Postcodes/code_point_open.Rds"))
postcodes$postcode <- gsub(" ","",postcodes$postcode)

summary(transas$postcode_clean %in% postcodes$postcode)

transas <- left_join(transas, postcodes[,"postcode"], by = c("postcode_clean" = "postcode"))

transas$category <- pmap_chr(transas, function(postcode_original,postcode_clean,geometry){
  if(st_is_empty(geometry)){
    if(nchar(postcode_clean) <= 3){
      return("partial postcode")
    } else {
      return("invalid postcode")
    }
  } else {
    return("vaild postcode")
  }
})
transas <- st_as_sf(transas)

qtm(transas)
dir.create("tmp")
unzip(paste0(substr(secure_path,1,39),"OA Bounadries/GB_OA_2011_clipped.zip"),
      exdir = "tmp")
oa <- read_sf("tmp/infuse_oa_lyr_2011_clipped.shp")
unlink("tmp",recursive = TRUE)

nrow(transas)
transas <- st_join(transas, oa)
nrow(transas)



lsoa <- read.csv(paste0(substr(secure_path,1,39),"OA Bounadries/GB_OA_LSOA_MSOA_LAD_Classifications_2017.csv"))
names(lsoa)[1] <- "geo_code"

transas <- left_join(transas, lsoa, "geo_code")
names(transas) <- c("postcode_original","postcode_clean","category","OA_2011","del1","OA_classification","LSOA_2011",
                    "LSOA_2011_name","del2","LSOA_classification","MSOA_2011","MSOA_2011_name","LA_2017","LA_2017_name",
                    "del3","LA_classification","region","region_name","country","country_name","del4",
                    "geometry")

transas <- transas[,!grepl("del",names(transas))]
transas <- transas[order(transas$postcode_clean),]
write.csv(st_drop_geometry(transas), "data/postcode_lookup_w3.csv", row.names = FALSE)
