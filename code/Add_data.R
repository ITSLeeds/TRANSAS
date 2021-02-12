# Add data to postcode lookup
library(dplyr)

postcodes <- read.csv("data/postcode_lookup.csv")
postcodes <- postcodes[!duplicated(postcodes$postcode_clean),]

ahah <- readRDS("data/AccesHeathAndHazard.Rds")
jts <- readRDS("data/JounreyTimeStats2017.Rds")
t2w <- readRDS("data/travel2work.Rds")
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

