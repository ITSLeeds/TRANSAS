# Add data to postcode lookup
library(dplyr)

postcodes <- read.csv("data/postcode_lookup.csv")
postcodes <- postcodes[!duplicated(postcodes$postcode_clean),]

ahah <- readRDS("data/AccesHeathAndHazard.Rds")
jts <- readRDS("data/JounreyTimeStats2017.Rds")
t2w <- readRDS("data/travel2work.Rds")
t2w_scot <- readRDS("data/travel2work_scot.Rds")
names(t2w_scot)[1] <- "LSOA_from"
names(t2w_scot)[13] <- "OtherMethod"
t2w <- bind_rows(list(t2w, t2w_scot))

kadam <- readxl::read_excel("data/transas_id_with_data_plusscotland_MM_KLVer1.1.xlsx")
kadam <- kadam[,c(1,73:79)]
kadam[2:8] <- lapply(kadam[2:8], as.numeric)

scot_car <- readRDS("data/traveltime_car_summary.Rds")
names(scot_car) <- c("DataZone","PSCart","SSCart","FoodCart","TownCart","pharm","GPCart","Emp100Cart","Emp500Cart","Emp5000Cart")
scot_bike <- readRDS("data/traveltime_bike_summary.Rds")
names(scot_bike) <- c("DataZone","PSCyct","SSCyct","FoodCyct","TownCyct","pharm","GPCyct","Emp100Cyct","Emp500Cyct","Emp5000Cyct")
scot_transit <- readRDS("data/traveltime_transit_summary.Rds")
names(scot_transit) <- c("DataZone","PSPTt","SSPTt","FoodPTt","TownPTt","pharm","GPPTt","Emp100PTt","Emp500PTt","Emp5000PTt")

scot <- left_join(scot_car, scot_bike, by = "DataZone")
scot <- left_join(scot, scot_transit, by = "DataZone")

scot$pharm <- NULL
scot$pharm.x <- NULL
scot$pharm.y <- NULL

t2w$AllMethods <- NULL

postcodes <- left_join(postcodes, ahah, by = c("LSOA_2011" = "lsoa11"))
postcodes <- left_join(postcodes, jts, by = c("LSOA_2011" = "LSOA_code"))
postcodes <- left_join(postcodes, t2w, by = c("LSOA_2011" = "LSOA_from"))

write.csv(postcodes, "data/postcode_lookup_with_data.csv", row.names = FALSE)

# Get transaas postcodes
transas <- readxl::read_xlsx("data/W1W2Postcodes_v1.xlsx")
transas$postcode_clean <- toupper(gsub(" ","",transas$`Postcode 1`))

transas <- left_join(transas, postcodes, by = c("postcode_clean"))

write.csv(transas, "data/transas_id_with_data.csv", row.names = FALSE)

invalid <- transas[transas$category != "vaild postcode",]
invalid <- invalid[,1:9]


write.csv(invalid, "data/transas_missing_location.csv", row.names = FALSE)

# Join Scotland
#names(scot) <- c("DataZone","PSCyct","PSCart","SSCart","GPCart","GPPTt")

transas_scot <- transas[transas$country_name == "Scotland",]
transas_scot <- transas_scot[!is.na(transas_scot$country_name),]
transas_scot <- transas_scot[,!names(transas_scot) %in% names(scot)]
transas_scot <- left_join(transas_scot, scot, by = c("LSOA_2011" = "DataZone"))
transas_scot <- transas_scot[,names(transas)]

transas_england <- transas[transas$country_name != "Scotland",]
transas_england <- transas_england[!is.na(transas_england$country_name),]

transas_na <- transas[is.na(transas$country_name),]

transas_new <- rbind(transas_england, transas_scot)
transas_new <- rbind(transas_new, transas_na)

transas_new <- left_join(transas_new, kadam, by = "ID")

write.csv(transas_new, "data/transas_id_with_data_plusscotland.csv", row.names = FALSE)
