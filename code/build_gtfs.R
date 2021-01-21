remotes::install_github("ITSleeds/UK2GTFS")
library(UK2GTFS)

path_in <- "D:/OneDrive - University of Leeds/Data/UK2GTFS/TransXchange/data_20200326"
path_out = "D:/OneDrive - University of Leeds/Data/UK2GTFS/GTFS/gtfs_20200326/"

naptan <- get_naptan()
cal <- get_bank_holidays()

files <- list.files(path_in, pattern = ".zip", full.names = TRUE)

for(i in c(10)){
  file <- files[i]
  nm <- strsplit(file, "/")[[1]]
  nm <- nm[length(nm)]
  nm <- gsub(".zip","",nm)
  message(nm)
  if(nm == "NCSD"){
    message("Skip coaches")
  } else {
    gtfs <- transxchange2gtfs(path_in = files[i],
                              ncores = 30,
                              silent = FALSE,
                              try_mode = TRUE,
                              naptan = naptan,
                              cal = cal)


    gtfs_write(gtfs,
               folder = path_out,
               name = nm)

  }


}

# Error Log

# W
# SW
# NW
# NE

