library(sf)
library(tmap)
library(dplyr)
tmap_mode("view")

# Supermartks
supermarket <- read.csv("D:/OneDrive - University of Leeds/Data/Accessibility Destinations/Supermarkets/GEOLYTIX - RetailPoints/geolytix_retailpoints_v18_202011.csv")
supermarket <- st_as_sf(supermarket, coords = c("long_wgs","lat_wgs"), crs = 4326)

saveRDS(supermarket, "data/supermarkets.Rds")

qtm(supermarket, dots.col = "size_band")

# Postcodes
source("../../creds2/CarbonCalculator/R/secure_path.R")
dir.create("tmp")
unzip(paste0(paste0(substr(secure_path,1,39),"Postcodes/codepo_20210208/codepo_gpkg_gb.zip")),
      exdir = "tmp")
postcodes <- read_sf("tmp/data/codepo_gb.gpkg")
unlink("tmp",recursive = TRUE)
postcodes <- postcodes[,c("Postcode")]
postcodes$Postcode <- gsub(" ","",postcodes$Postcode)

postcodeold <- readRDS("D:/OneDrive - University of Leeds/Data/Postcodes/code_point_open.Rds")
postcodeold$postcode <- gsub(" ","",postcodeold$postcode)
postcodeold <- postcodeold["postcode"]
names(postcodeold) <- c("Postcode","geom")
st_geometry(postcodeold) <- "geom"
postcodeold <- postcodeold[!postcodeold$Postcode %in% postcodes$Postcode,]
postcodes <- rbind(postcodes, postcodeold)
postcodes <- st_transform(postcodes, 4326)

postcode_extra <- data.frame(Postcode = c("HD19RX","L281ST","WA84NJ",
                                          "AB252AY","DD46RB"),
geom = st_as_sfc(list(st_point(c(-1.7717652719418815, 53.6570249965402)),
            st_point(c(-2.866979314978537, 53.43585645658988)),
            st_point(c(-2.773202302411166, 53.37855298012618)),
            st_point(c(-2.1371413487457978, 57.153039145166176)),
            st_point(c(-2.9595948641150347, 56.46821038934813))
            ), crs = 4326)
)
names(postcode_extra)[2] <- "geom"
postcode_extra <- st_as_sf(postcode_extra)

postcodes <- rbind(postcodes, postcode_extra)

# GP - England
gp <- read.csv("D:/OneDrive - University of Leeds/Data/Accessibility Destinations/GP/gp-reg-pat-prac-all.csv")
gp$postcode_clean <- gsub(" ","",gp$POSTCODE)
summary(gp$postcode_clean %in% postcodes$Postcode)
summary(duplicated(gp$postcode_clean))
gp <- gp %>%
  group_by(postcode_clean) %>%
  summarise(CODE = CODE[1],
            NUMBER_OF_PATIENTS = sum(NUMBER_OF_PATIENTS, na.rm = TRUE))
gp <- left_join(gp,postcodes, by = c("postcode_clean" = "Postcode"))
gp <- st_as_sf(gp)
qtm(gp, dots.col = "NUMBER_OF_PATIENTS")
saveRDS(gp, "data/GP-England.Rds")

# GP Scotland
gp <- read.csv("D:/OneDrive - University of Leeds/Data/Accessibility Destinations/GP/gp_contactdetails_jan2021-open-data-scotland.csv")
gp <- gp[,c("PracticeCode","Postcode")]
gp <- gp[!duplicated(gp$Postcode),]
gp$postcode_clean <- gsub(" ","", gp$Postcode)
summary(gp$postcode_clean %in% postcodes$Postcode)
foo <- gp[!gp$postcode_clean %in% postcodes$Postcode,]

gp <- left_join(gp, postcodes, by = c("postcode_clean" = "Postcode"))
gp <- st_as_sf(gp)
gp <- gp[!st_is_empty(gp),]

saveRDS(gp, "data/GP-Scotland.Rds")

# Hospitals - England
hosp <- read.csv("D:/OneDrive - University of Leeds/Data/Accessibility Destinations/Hospitals/10_February_2021_CQC_directory.csv")
names(hosp) <- hosp[4,]
hosp <- hosp[5:nrow(hosp),]
hosp <- hosp[,c("Name","Postcode","Service types")]


# Data Zones
dir.create("tmp")
unzip("D:/OneDrive - University of Leeds/Data/Accessibility Destinations/DataZone Centroids/SG_DataZoneCent_2011.zip",
      exdir = "tmp")
dz <- read_sf("tmp/SG_DataZone_Cent_2011.shp")
unlink("tmp",recursive = TRUE)
dz <- dz[,c("DataZone")]
dz <- st_transform(dz, 4326)
qtm(dz)
saveRDS(dz, "data/datazone_centroids.Rds")


# schools - england and wales
schools <- read.csv("D:/OneDrive - University of Leeds/Data/Accessibility Destinations/Schools/edubasealldata20210211.csv")
schools$country <- substr(schools$LSOA..code.,1,1)
schools <- schools[,c("URN","EstablishmentName","TypeOfEstablishment..name.","EstablishmentStatus..name.",
                      "PhaseOfEducation..name.","country","Postcode")]
schools <- schools[schools$EstablishmentStatus..name. == "Open",]
names(schools) = c("URN","EstablishmentName","TypeOfEstablishment","EstablishmentStatus",
                   "PhaseOfEducation","country","Postcode")
schools <- schools[!schools$TypeOfEstablishment %in% c("British schools overseas","Offshore schools","Service children's education"),]
schools$postcode_clean <- gsub(" ","",schools$Postcode)
summary(schools$postcode_clean %in% postcodes$Postcode)
foo <- schools[!schools$postcode_clean %in% postcodes$Postcode,]

schools <- left_join(schools, postcodes, by = c("postcode_clean" = "Postcode"))
schools <- st_as_sf(schools)
schools <- schools[!st_is_empty(schools),]

saveRDS(schools,"data/schools_england_wales.Rds")

#Schools - Scotland
schools <- read_sf("D:/OneDrive - University of Leeds/Data/Accessibility Destinations/Schools/SG_SchoolRoll_2018/SG_SchoolRoll_2018.shp")
schools <- schools[,c("SchUID","SchType","SchoolName")]
schools <- st_transform(schools, 4326)
saveRDS(schools,"data/schools_scotland.Rds")

#town centres - scotland
town <- read_sf("D:/OneDrive - University of Leeds/Data/Accessibility Destinations/Scotland Accessability/town-centres_3938404/pub_townc.shp")
town <- st_centroid(town)
town$id <- as.character(1:nrow(town))
town <- st_transform(town, 4326)
saveRDS(town,"data/towns_scotland.Rds")


