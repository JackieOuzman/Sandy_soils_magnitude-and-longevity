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

file_path_input_data_decile<-file.path("N:","sandy soils conference", "data",  "climate_data", "Decile_tables")
file_path_input_data_Yld_potentails<-file.path("N:","sandy soils conference", "data",  "climate_data", "Yld_potentails")


site_selected <- "076052"


###############################################################################
decile_table <- read.csv(
  paste0(
    file_path_input_data_decile,
    "/Year_with_Deciles_",
    site_selected, 
    ".csv") )
#Subset data I only want years after 2010
str(decile_table)
decile_table <- decile_table %>% filter(year>= 2010)
decile_table <- decile_table %>% select(
  year, Station_name, Station_number, gs_decile, gs_sum_rain, summer_sum_rain
) %>% 
  rename(decile = gs_decile)
###############################################################################

###############################################################################
Yld_potentail <- read.csv(
  paste0(
    file_path_input_data_Yld_potentails,
    "/GS_decile_table_withYP_cals_long",
    site_selected, 
    ".csv") )
#Subset data I only baseline and future

str(Yld_potentail)
Yld_potentail <- Yld_potentail %>% filter(Cal_type == "Baseline Practice" |Cal_type == "New Frontier")
Yld_potentail <- Yld_potentail %>%  select(GS_deciles_names, Cal_type, value)
Yld_potentail_wide <- Yld_potentail %>% 
  pivot_wider(names_from = Cal_type, 
              values_from = value)
str(Yld_potentail_wide)
Yld_potentail_wide <- Yld_potentail_wide %>% 
  mutate(Wheat_YP_Base = `Baseline Practice`,
         Barley_YP_Base = `Baseline Practice`,
         Canola_YP_Base = `Baseline Practice` *0.6,
         
         Wheat_YP_NewFront = `New Frontier`,
         Barley_YP_NewFront = `New Frontier`,
         Canola_YP_NewFront = `New Frontier` *0.6,
         )%>% 
  rename(decile = GS_deciles_names)


###############################################################################
str(decile_table)
str(Yld_potentail_wide)
df <- left_join(decile_table, Yld_potentail_wide)

###############################################################################
path_saved_files <- 
  file_path_input_data<-file.path("N:","sandy soils conference", "data", "climate_data","Yld_potentails_for_DB")

write_csv(df,
          paste0(path_saved_files,"/Forecast_yld_yr_decile", "_site_", site_selected, ".csv" ))


###############################################################################
# This might be more useful 2 df base and front with crop long
str(df)
df_base <- df %>% select(year: summer_sum_rain, Wheat_YP_Base, Barley_YP_Base, Canola_YP_Base)
df_base_long <- df_base %>%  pivot_longer(
  cols = c(Wheat_YP_Base,  Barley_YP_Base, Canola_YP_Base ),
  names_to = "Crop",
  values_to = "YP")

df_base_long <- df_base_long %>% mutate(
  Crop = case_when(
    Crop == "Wheat_YP_Base" ~ "Wheat",
    Crop == "Barley_YP_Base" ~ "Barley",
    Crop == "Canola_YP_Base"  ~ "Canola",
  )
)
rm(df_base)  
  
str(df)
df_front <- df %>% select(year: summer_sum_rain, Wheat_YP_NewFront, Barley_YP_NewFront, Canola_YP_NewFront)
df_front_long <- df_front %>%  pivot_longer(
  cols = c(Wheat_YP_NewFront,  Barley_YP_NewFront, Canola_YP_NewFront ),
  names_to = "Crop",
  values_to = "YP")

df_front_long <- df_front_long %>% mutate(
  Crop = case_when(
    Crop == "Wheat_YP_NewFront" ~ "Wheat",
    Crop == "Barley_YP_NewFront" ~ "Barley",
    Crop == "Canola_YP_NewFront"  ~ "Canola",
  )
)
rm(df_front)   


###############################################################################  
write_csv(df_front_long,
          paste0(path_saved_files,"/NewFront_YP_yr_decile_crop_long", "_site_", site_selected, ".csv" ))

write_csv(df_base_long,
          paste0(path_saved_files,"/Base_YP_yr_decile_crop_long", "_site_", site_selected, ".csv" ))
###############################################################################