all <- readRDS("../../creds2/CarbonCalculator/data/data_with_grades.Rds")
transas <- read.csv("data/postcode_lookup.csv")
transas <- transas[,"LSOA_2011"]
transas <- transas[!is.na(transas)]

#all <- all[all$LSOA11 %in% transas,]
all <- all[,c("LSOA11","LSOA11NM","gas_percap_2017","elec_percap_2017","cars_percap_2017","car_percap_2017",
              "elec_emissions_grade","gas_emissions_grade","car_emissions_grade")]
all <- all[order(all$LSOA11NM),]
write.csv(all,"data/for_noel.csv", row.names = FALSE)
