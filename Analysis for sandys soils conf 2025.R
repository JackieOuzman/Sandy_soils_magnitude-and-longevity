
###Script for snady soils data analysis

library(ggplot2)
library(readxl)
library(tidyverse)
#library(multcompView)

library(plotly)

library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)
library(hrbrthemes)
library(stringr)
#install.packages("hrbrthemes")

library(ggpubr)



#########################################################################################################################################
### prep the data ####
#########################################################################################################################################


soil_constraints <- read.csv("N:/sandy soils conference/data/data from project DB/sites_merged_all_messy.csv")
names(soil_constraints)

#keep a subset of data
soil_constraints <- soil_constraints %>% 
  dplyr::select(site, site_display, Repellence, Acidity, Physical, Nutrient, Latitude,  Longitude, rain  )

#keep a data for sites only - remove the duplicates
soil_constraints <- soil_constraints %>%
  filter(site_display != "remove") %>% 
  dplyr::distinct(site, .keep_all = TRUE)

soil_constraints <- soil_constraints %>% 
  mutate(sum_contraints = Repellence+Acidity+Physical )


write_csv(soil_constraints,"N:/sandy soils conference/data/data for SS prestenation/contraints_mapping.csv" )

### number of sites
number_of_sites <- soil_constraints %>%  count()


### join this to the summary_control_data_all data

summary_control_data_all <- read.csv("N:/sandy soils conference/data/data from project DB/sites_control_merged.csv")
summary_control_data_all_soil_constraints <- left_join(summary_control_data_all, soil_constraints)

list_of_sites <- as.data.frame(unique(summary_control_data_all_soil_constraints$site))
list_of_sites
names(summary_control_data_all_soil_constraints)


### fix up some names

##label problem with Mt Damper
summary_control_data_all <- summary_control_data_all %>% 
  mutate(
    site = case_when(
      site == "Mt Damper" ~ "Mt_Damper",
      TRUE ~ site))

##label problem with Ouyen
summary_control_data_all <- summary_control_data_all %>% 
  mutate(
    site = case_when(
      site == "Oyen_Spade" ~ "Ouyen_Spade",
      TRUE ~ site_sub))
summary_control_data_all <- summary_control_data_all %>% 
  mutate(
    site = case_when(
      site == "Oyen_Spade" ~ "Ouyen_Spade",
      TRUE ~ site))


################################################################################

# remove some sites and treatments ----------------------------------------



#keep a data for sites only that have been approved - remove 
summary_control_data_all <- summary_control_data_all %>%
  filter(site_display != "remove") 

# remove Young husband wetter
summary_control_data_all <- summary_control_data_all %>%
  filter(Descriptors !=   "Unmodified+DeepTill.18_SE14.band_8"&
         Descriptors !=   "Unmodified_SE14.band_8"  &
         Descriptors !=   "Unmodified_Bi_Agra.surface+band_8" ) 


Younghusband_treatments <- 
  summary_control_data_all %>% 
  dplyr::filter(site == "Younghusband") %>% 
  dplyr::select(site, Descriptors) %>% 
  dplyr::distinct(Descriptors)
print(arrange(Younghusband_treatments))
rm(Younghusband_treatments)




# soil modifications ----------------------------------------


### get the soil modification from the Descriptors

#1 soil modification with depth
summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_depth =  str_extract(summary_control_data_all$Descriptors, "[^_]+"))#keep everything before the first_

check<- summary_control_data_all %>% 
  #dplyr::distinct(soil_modification_depth, .keep_all = TRUE) %>% 
  select(site, Descriptors, soil_modification_depth)

#1.1 Youndhusband has 'Unmodified+DeepTill.18' which needs to be recoded as "Unmodified"

summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_depth = case_when(
    soil_modification_depth == "Unmodified+DeepTill.18" ~ "Unmodified",
    TRUE ~ soil_modification_depth))

#how many modification_depth
soil_modification_depth <- summary_control_data_all %>% 
  distinct(soil_modification_depth) %>% 
  arrange(soil_modification_depth) %>% 
  filter(soil_modification_depth != "Unmodified")

count(soil_modification_depth) #20 excluding the unmodified


#1a working clms - soil modification without depth 1 only
summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_1 =  str_extract(summary_control_data_all$Descriptors, "[^.]+"))#keep everything before the first .

#the Unmodified seems to have a problem this fixes it :)
summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_1 = str_extract(summary_control_data_all$soil_modification_1, "[^_]+")) #keep everything after the first _

#1a.1 Youndhusband has 'Unmodified+DeepTill.18' which needs to be recoded as "Unmodified"

summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_1 = case_when(
    soil_modification_1 == "Unmodified+DeepTill" ~ "Unmodified",
    TRUE ~ soil_modification_1))

summary_control_data_all %>%  distinct(soil_modification_1) %>% arrange(soil_modification_1)
