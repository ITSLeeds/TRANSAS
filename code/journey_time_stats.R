# JTS

library(jts)
library(dplyr)

ecen <- jts::get_jts_data("jts0501")
ecen <- ecen[,c("LSOA_code",
            "100EmpPTt","100EmpCyct","100EmpCart",
            "500EmpPTt","500EmpCyct","500EmpCart",
            "5000EmpPTt","5000EmpCyct","5000EmpCart")]

names(ecen) <- c("LSOA_code",
                 "Emp100PTt","Emp100Cyct","Emp100Cart",
                 "Emp500PTt","Emp500Cyct","Emp500Cart",
                 "Emp5000PTt","Emp5000Cyct","Emp5000Cart")

psch <- jts::get_jts_data("jts0502")
psch <- psch[,c("LSOA_code","PSPTt","PSCyct","PSCart")]

ssch <- jts::get_jts_data("jts0503")
ssch <- ssch[,c("LSOA_code","SSPTt","SSCyct","SSCart")]

fed <- jts::get_jts_data("jts0504")
fed <- fed[,c("LSOA_code","FEPTt","FECyct","FECart")]

gp <- jts::get_jts_data("jts0505")
gp <- gp[,c("LSOA_code","GPPTt","GPCyct","GPCart")]

hosp <- jts::get_jts_data("jts0506")
hosp <- hosp[,c("LSOA_code","HospPTt","HospCyct","HospCart")]

food <- jts::get_jts_data("jts0507")
food <- food[,c("LSOA_code","FoodPTt","FoodCyct","FoodCart")]

tcen <- jts::get_jts_data("jts0508")
tcen <- tcen[,c("LSOA_code","TownPTt","TownCyct","TownCart")]

# phar <- readODS::read_ods("D:/OneDrive - University of Leeds/Data/Journey Time Statistics/jts0509 - pharmacies.ods",
#                           sheet = "2017")


jts <- left_join(ecen, psch, by = "LSOA_code")
jts <- left_join(jts, ssch, by = "LSOA_code")
jts <- left_join(jts, fed, by = "LSOA_code")
jts <- left_join(jts, gp, by = "LSOA_code")
jts <- left_join(jts, hosp, by = "LSOA_code")
jts <- left_join(jts, food, by = "LSOA_code")
jts <- left_join(jts, tcen, by = "LSOA_code")

saveRDS(jts, "data/JounreyTimeStats2017.Rds")
