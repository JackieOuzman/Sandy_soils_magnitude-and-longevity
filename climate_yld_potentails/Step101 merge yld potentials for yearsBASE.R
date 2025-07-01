
library(ggplot2)
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)
library(DT)



list_of_sites <- data.frame(
  Name = c("Karoonda", "Wharminda", "Copeville", "Walpeup" , "Bute", 
           "Balaklava", "Annuello", "YEELANNA", "KIMBA", "NARACOORTE",
           "RED_CLIFFS"), ### add more here when you have the info
  Number = c("25006", "18113", "25003", "76065", "21012", 
             "21002", "76000", "018099", "018040", "026015",
             "076052"),
  Latitude = c("-35.09", "-33.97", "-34.80", "-35.14",  "-33.86",
               "-34.14", "-34.85", "-34.132", "11", "11",
               "11"),
  Longitude = c("139.90", "136.25", "139.85", "142.02", "138.01", 
                "138.42", "142.78", "135.730", "11", "11",
                "11"),
  State = c("SA", "SA", "SA", "VIC","SA", 
            "SA", "VIC", "SA", "SA", "SA",
            "VIC")
)

print(list_of_sites)
path_saved_files <- 
  file.path("N:","sandy soils conference", "data", "climate_data","Yld_potentails_for_DB")



list_of_file <- list.files(path = path_saved_files, pattern = ".csv")
list_of_file <- as.data.frame(list_of_file)
list_of_file

site1 <- "018099"
site2 <- "018040"
site3 <-  "21012"
site4 <- "026015"


df1 <- read.csv(  paste0( path_saved_files,"/Base_YP_yr_decile_crop_long_site_",
                          site1, 
                         ".csv") )
df2 <- read.csv(  paste0( path_saved_files,"/Base_YP_yr_decile_crop_long_site_",
                          site2, 
                          ".csv") )
df3 <- read.csv(  paste0( path_saved_files,"/Base_YP_yr_decile_crop_long_site_",
                          site3, 
                          ".csv") )
df4 <- read.csv(  paste0( path_saved_files,"/Base_YP_yr_decile_crop_long_site_",
                          site4, 
                          ".csv") )

Base <- rbind(df1, df2, df3, df4)


write_csv(Base,
          paste0(path_saved_files,"/Base_YP_yr_decile_crop_long", "_multiple_sites", ".csv" ))
