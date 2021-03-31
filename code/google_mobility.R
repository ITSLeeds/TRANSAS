library(dplyr)
library(ggplot2)
library(tidyr)

dat1 <- read.csv("data/2020_GB_Region_Mobility_Report.csv")
dat2 <- read.csv("data/2021_GB_Region_Mobility_Report.csv")

dat1 <- dat1[,c("country_region","sub_region_1","sub_region_2","date",
                "retail_and_recreation_percent_change_from_baseline",
                "grocery_and_pharmacy_percent_change_from_baseline",
                "parks_percent_change_from_baseline",
                "transit_stations_percent_change_from_baseline",
                "workplaces_percent_change_from_baseline",
                "residential_percent_change_from_baseline")]

dat2 <- dat2[,names(dat1)]

names(dat1) <- c("country","region_1","region_2","date",
                 "retail_recreation",
                 "grocery_pharmacy",
                 "parks",
                 "transit_stations",
                 "workplaces",
                 "residential")

names(dat2) <- names(dat1)
dat1$date <- lubridate::dmy(dat1$date)
dat2$date <- lubridate::ymd(dat2$date)

dat <- rbind(dat1, dat2)

summary(dat$date)
dat$region_1[nchar(dat$region_1) == 0] <- NA
dat$region_2[nchar(dat$region_2) == 0] <- NA
dat_uk <- dat[is.na(dat$region_1),]

dat_region <- dat[!is.na(dat$region_1),]

dat_region <- dat_region[dat_region$region_1 %in% c("Aberdeen City","Aberdeenshire","Greater London",
                            "Bristol City","Glasgow City","Edinburgh","Tyne and Wear",
                            "Greater Manchester","Merseyside","Lancashire",
                            "South Ayrshire Council","East Ayrshire Council",
                            "West Lothian","East Lothian Council","Midlothian",
                            "Scottish Borders","South Lanarkshire","Inverclyde",
                            "East Dunbartonshire Council","West Dunbartonshire Council",
                            "Renfrewshire","East Renfrewshire Council"),]

dat_region$name <- dat_region$region_1
dat_uk$name <- dat_uk$country

dat_uk <- dat_uk[,c("name","date","retail_recreation", "grocery_pharmacy","parks",
                    "transit_stations","workplaces","residential")]
dat_region <- dat_region[,names(dat_uk)]

dat2 <- rbind(dat_uk, dat_region)

dat3 <- pivot_longer(dat2, cols = c("retail_recreation", "grocery_pharmacy",
                                    "parks", "transit_stations", "workplaces",
                                    "residential"),
                     names_to = "locations")

dat3$sel <- ifelse(dat3$name == "United Kingdom",TRUE,FALSE)
dat3$name_plot <- ifelse(dat3$name == "United Kingdom",NA,dat3$name)

dat3 <- dat3[!dat3$locations %in% c("residential","parks"),]

dat3$locations[dat3$locations == "transit_stations"] <- "Transit Stations"
dat3$locations[dat3$locations == "retail_recreation"] <- "Retail & Recreation"
dat3$locations[dat3$locations == "grocery_pharmacy"] <- "Grocery & Pharmacy"
dat3$locations[dat3$locations == "workplaces"] <- "Workplaces"

ggplot(dat3, aes(x = date, y = value, colour = name_plot, group = name)) +
  geom_smooth(data = subset(dat3, sel == FALSE), aes(size = sel), se= FALSE) +
  geom_smooth(data = subset(dat3, sel == TRUE), aes(size = sel), se= FALSE) +
  ylab("% change from baseline") +
  labs(color = "Local Authority") +
  guides(size = FALSE) +
  guides(colour = guide_legend(override.aes = list(size=3),
                               ncol=1)) +

  scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = 0.1)) +
  facet_grid(rows = vars(locations))



