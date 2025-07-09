

library(ggplot2)
library(readxl)
library(tidyverse)
library(stringr)

library(dmetar)
library(tidyverse)
library(meta)

library(forcats)

## plot and analysis for preso




df <- read.csv("N:/sandy soils conference/data/data for SS prestenation/control_metadata_contraints_tillage_only.csv" )

names(df)
# keep only one yield output
str(df)
df_modified <- df %>% 
  select(site_display,
         year,
    tillage_amendments_class,
    tillage_class,
    amendments_no_amend,
    yr_post_amelioration,
    Physical,
    Nutrient,
    Acidity,
    Repellence,
    crop,
   decile, 
   relative_yld_change,
   yield,
   control_yield
  ) %>% 
  dplyr::mutate(site_year = paste0(site_display,"_", year)) %>% 
  filter(tillage_amendments_class != "Unmodified_amendment")

str(df_modified)

df_modified <- df_modified %>% 
  mutate(lnR_Yield = log(yield / control_yield))
################################################################################
#Heap of sites with no yield- not sure what that is about?
df_modified <- df_modified %>% filter(yield!= 0.0)
df_modified <- df_modified %>% filter(control_yield>  0.0)
################################################################################


#variable that we are interested in
unique(df_modified$tillage_class)
unique(df_modified$amendments_no_amend)
unique(df_modified$Physical)
unique(df_modified$Nutrient)
unique(df_modified$Repellence)
unique(df_modified$Acidity)
### these are made below
unique(df_modified$multiple_constraints)
unique(df_modified$Decile_group)
unique(df_modified$crop_group)
unique(df_modified$post_tillage_group)




################################################################################
## multiple_constraints
#recode ranked as moderate or severe

df_modified <- df_modified %>% 
  mutate(Physical_M_S = case_when(
    Physical == 1 ~ 1,
    Physical == 2 ~ 1,
    .default = 0
  )) %>% 
  
  mutate(Nutrient_M_S = case_when(
    Nutrient == 1 ~ 1,
    Nutrient == 2 ~ 1,
    .default = 0
  )) %>% 
  
  mutate(Acidity_M_S = case_when(
    Acidity == 1 ~ 1,
    Acidity == 2 ~ 1,
    .default = 0
  )) %>%
  
  mutate(Repellence_M_S = case_when(
    Repellence == 1 ~ 1,
    Repellence == 2 ~ 1,
    .default = 0
  )) 
## multiple_constraints
#sum with multiple constraints
df_modified <- df_modified %>% 
  mutate(multiple_constraints =
           (Physical_M_S +  Nutrient_M_S +  Acidity_M_S + Repellence_M_S))

df_modified <- df_modified %>% select(-Physical_M_S, -Nutrient_M_S, -Acidity_M_S, -Repellence_M_S)

################################################################################
# Cal season type
# get some more information about the run of sesaons at each site.
# group into 3 season classes below ave, av and above av

distinct(df_modified,decile )

df_modified <- df_modified %>% 
  mutate(Decile_group = case_when(
    decile  == "decile_1" ~ "below_av",
    decile  == "decile_2" ~ "below_av",
    decile  == "decile_3" ~ "below_av",
    
    decile  == "decile_4" ~ "average",
    decile  == "decile_5" ~ "average",
    decile  == "decile_6" ~ "average",
    decile  == "decile_7" ~ "average",
    
    decile  == "decile_8" ~ "above_av",
    decile  == "decile_9" ~ "above_av",
    decile  == "decile_10" ~ "above_av",
    .default = "not_classed"
  )) 


################################################################################
# crop type grouping
distinct(df_modified,crop )

#Cereals (Wheat ,Barley ,Oats), Oilseeds (Canola), Grain Legumes (Beans, Peas,Lentils, Lupins )
df_modified <- df_modified %>% 
  mutate(crop_group = case_when(
    crop  == "Wheat" ~ "Cereals",
    crop  == "Barley" ~ "Cereals",
    crop  == "Canola" ~ "Oilseeds",
    crop  == "Lupins" ~ "Grain Legumes",
    crop  == "Beans" ~ "Grain Legumes",
    crop  == "Lentils" ~ "Grain Legumes",
    crop  == "Peas" ~ "Grain Legumes",
    .default = "not_classed"
  )) 

################################################################################
# crop type grouping
distinct(df_modified,yr_post_amelioration )

df_modified <- df_modified %>% 
  mutate(post_tillage_group = case_when(
    yr_post_amelioration  == 0 ~ "new_tillage",
    yr_post_amelioration  == 1 ~ "new_tillage",
    
    yr_post_amelioration  == 2 ~ "old_tillage",
    yr_post_amelioration  == 3 ~ "old_tillage",
    yr_post_amelioration  == 4 ~ "old_tillage",
    yr_post_amelioration  == 5 ~ "old_tillage",
    yr_post_amelioration  == 6 ~ "old_tillage",
    .default = "not_classed"
  )) 

################################################################################

subset_df <- df_modified %>% 
  select(relative_yld_change,
         site_year,
         tillage_class,
         amendments_no_amend,
         Physical,
         Nutrient,
         Repellence,
         Acidity,
         multiple_constraints,
         Decile_group,
         crop_group,
         post_tillage_group)
write_csv(subset_df,
          "N:/sandy soils conference/data/data for SS prestenation/control_metadata_contraints_tillage_only_subset.csv" )
