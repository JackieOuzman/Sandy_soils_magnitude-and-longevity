###Script for sandy soils data analysis

library(ggplot2)
library(readxl)
library(tidyverse)
#library(multcompView)

#library(plotly)

# library(tidyverse)
# library(hrbrthemes)
# library(viridis)
# library(forcats)
# library(hrbrthemes)
library(stringr)
#install.packages("hrbrthemes")

library(ggpubr)


################################################################################
# Import the data that has been matched to control  and the primary data ie what is on the DAP ----------

                                    
soil_constraints_control <- read.csv("N:/sandy soils conference/data/data from project DB/sites_control_merged.csv")
names(soil_constraints_control)

soil_constraints <- read.csv("N:/sandy soils conference/data/Shiny_data/primary_data_all_9_05_2023.csv")
names(soil_constraints)

# brooker_control <- soil_constraints_control %>% filter(site_display == "Brooker")
# brooker_soil_constraints <- soil_constraints %>% filter(site == "Brooker")
# 
# brooker_control %>% group_by(Descriptors, year) %>% 
#   summarise(count = n()) %>% 
#   arrange(year)
# 
# brooker_soil_constraints %>% group_by(Descriptors, year) %>% 
#   summarise(count = n()) %>% 
#   arrange(year)

################################################################################
# List of sites and constraints associated with it plus any other metadata  -------



max(soil_constraints$year)
min(soil_constraints$year)

max(soil_constraints$year) - min(soil_constraints$year)
soil_constraints %>%  dplyr::distinct(Descriptors) %>% count()
################################################################################

#keep a subset of data
site_unique <- soil_constraints %>% 
  dplyr::select(site, Repellence, Acidity, Physical, Nutrient, Latitude,  Longitude  )



#keep a data for sites only - remove the duplicates
site_unique <- site_unique %>%
  dplyr::distinct(site, .keep_all = TRUE)

site_unique <- site_unique %>% 
  mutate(sum_contraints = Repellence+Acidity+Physical )

### number of sites
site_unique %>%  count()


write_csv(site_unique,"N:/sandy soils conference/data/data for SS prestenation/contraints_mapping.csv" )

#brooker_soil_constraints <- site_unique %>% filter(site == "Brooker")

################################################################################
# Append  constraints and metatdata to control data  ---------------------------
names(soil_constraints_control)

names(site_unique)
soil_constraints_metadata_only <- soil_constraints %>% 
  select(site, year, Repellence, Acidity, Physical, Nutrient, rain)

soil_constraints_metadata_only <- soil_constraints_metadata_only %>% 
  distinct(site, year,Repellence, Acidity, Physical, Nutrient, rain)
         
str(soil_constraints_metadata_only)

df <- left_join(soil_constraints_control,soil_constraints_metadata_only, join_by( site,year)  )
str(df)

#brooker_df <- df %>% filter(site == "Brooker")

################################################################################
# double check the sites ----------------------------------------
names(df)


list_of_sites <- as.data.frame(dplyr::arrange(dplyr::distinct(df,site_display)) )
list_of_sites <- dplyr::arrange(list_of_sites, site_display)
list_of_sites

################################################################################
# remove the sites not "approved"  ----------------------------------------
names(df)

df <- df %>% filter(site_display != "remove")


### check my names are ok - use site_display

test <- df %>% select(site_display, site) %>% distinct(site, .keep_all = TRUE)
test
max(soil_constraints$year) - min(soil_constraints$year)
soil_constraints %>%  dplyr::distinct(Descriptors) %>% count()


################################################################################
# remove the treatments no included in Brooker and keep th ones appear in the app  ------
# brooker_df <- df %>% filter(site == "Brooker") %>% 
#   distinct(Descriptors) %>% 
#   arrange(Descriptors)
# brooker_df


df <- df %>% mutate(
  brooker_treatments_remove =
    case_when(
      Descriptors == "Spade.30_Lc@1.incorp_30" &
        site == "Brooker" ~ "remove",
      Descriptors == "Spade.30_Lc@1.incorp_30.K_added.surface" &
        site == "Brooker" ~ "remove",
      
      Descriptors == "Spade.30_Lc@10.incorp_30" &
        site == "Brooker" ~ "remove",
      Descriptors == "Spade.30_Lc@10.incorp_30.K_added.surface" &
        site == "Brooker" ~ "remove",
      
      Descriptors == "Spade.30_Lc@2.incorp_30" &
        site == "Brooker" ~ "remove",
      Descriptors == "Spade.30_Lc@2.incorp_30.K_added.surface" &
        site == "Brooker" ~ "remove",
      
      Descriptors == "Spade.30_Lc@20.incorp_30" &
        site == "Brooker" ~ "remove",
      Descriptors == "Spade.30_Lc@20.incorp_30.K_added.surface" &
        site == "Brooker" ~ "remove",
      
      Descriptors == "Spade.30_Lc@6.incorp_30" &
        site == "Brooker" ~ "remove",
      Descriptors == "Spade.30_Lc@6.incorp_30.K_added.surface" &
        site == "Brooker" ~ "remove",
      
      Descriptors == "Spade.8_Lc@6.incorp_30" &
        site == "Brooker" ~ "remove",
      Descriptors == "Spade.30_none" &
        site == "Brooker" ~ "remove",
      Descriptors == "Unmodified_K_added.surface" &
        site == "Brooker" ~ "remove",
      .default = "keep"
))
                   
df <- df %>% filter(brooker_treatments_remove == "keep") %>% select(-brooker_treatments_remove)

# brooker_df <- df %>% filter(site == "Brooker") %>% 
#   distinct(Descriptors, brooker_treatments_remove, site) %>% 
#   arrange(Descriptors)
# brooker_df
# 
# TEST <- brooker_df %>% filter(brooker_treatments_remove == "keep") 
# 
# brooker_df <- TEST %>% filter(site == "Brooker") %>% 
#   distinct(Descriptors) %>% 
#   arrange(Descriptors)
# brooker_df

################################################################################
# remove some sites and treatments ----------------------------------------



# remove Younghusband wetter
df <- df %>%
  filter(Descriptors !=   "Unmodified+DeepTill.18_SE14.band_8"&
           Descriptors !=   "Unmodified_SE14.band_8"  &
           Descriptors !=   "Unmodified_Bi_Agra.surface+band_8" &
           Descriptors !=   "Unmodified+OnRow_none"
         ) 


Younghusband_treatments <- 
  df %>% 
  dplyr::filter(site_display == "Younghusband") %>% 
  dplyr::select(site_display, Descriptors) %>% 
  dplyr::distinct(Descriptors)
print(arrange(Younghusband_treatments))
rm(Younghusband_treatments)

## check number of sites
test <- df %>% select(site_display, site) %>% distinct(site, .keep_all = TRUE)
test
dim(test)

max(df$year) - min(df$year)
df %>%  dplyr::distinct(Descriptors) %>% count()




# soil modifications ----------------------------------------
# this is missing in df but it exist in the soil constraints data.
# looks like I need to do the grunt again!

### get the soil modification from the Descriptors

#1 soil modification with depth
df1 <- df %>% select(Descriptors, site_display)
df <- df %>% 
  mutate(soil_modification_depth =  str_extract(df$Descriptors, "[^_]+"))#keep everything before the first_

# check_df1 <- df1 %>%  distinct(Descriptors)
# View(check_df1)

#1.1 Youndhusband has 'Unmodified+DeepTill.18' which needs to be recoded as "Unmodified"


df <- df %>% 
  mutate(soil_modification_depth = case_when(
    soil_modification_depth == "Unmodified+DeepTill.18" ~ "Unmodified",
    TRUE ~ soil_modification_depth))
 
# check_df1 <- df1 %>%  distinct(soil_modification_depth)
# View(check_df1)


#how many modification_depth
soil_modification_depth <- df %>% 
  distinct(soil_modification_depth) %>% 
  arrange(soil_modification_depth) %>% 
  filter(soil_modification_depth != "Unmodified")

count(soil_modification_depth) #19 excluding the unmodified


#1a working clms - soil modification without depth 1 only
df <- df %>% 
  mutate(soil_modification_1 =  str_extract(df$Descriptors, "[^.]+"))#keep everything before the first .

#the Unmodified seems to have a problem this fixes it :)
df <- df %>% 
  mutate(soil_modification_1 = str_extract(df$soil_modification_1, "[^_]+")) #keep everything after the first _

# check_df1 <- df1 %>%  distinct(soil_modification_1)
# View(check_df1)



#1a.1 Youndhusband has 'Unmodified+DeepTill.18' which needs to be recoded as "Unmodified"

df <- df %>% 
  mutate(soil_modification_1 = case_when(
    soil_modification_1 == "Unmodified+DeepTill" ~ "Unmodified",
    TRUE ~ soil_modification_1))

df %>%  distinct(soil_modification_1) %>% arrange(soil_modification_1)

# check_df1 <- df1 %>%  distinct(soil_modification_1)
# View(check_df1)

#1a working clms - 2 soil modification without depth 2

#temp <- summary_control_data_all %>%  distinct(soil_modification_depth) %>% arrange(soil_modification_depth)

#df1 <- df %>% select(Descriptors, site_display, soil_modification_depth, soil_modification_1)


df <- df %>% 
  mutate(soil_modification_2 = case_when(
    soil_modification_depth == "Rip.50Spade.30" ~        "Rip+Spade",
    soil_modification_depth == "Rip.60Spade.30" ~        "Rip+Spade",
    soil_modification_depth == "Rip.45IncRip+Spade.30" ~ "IncRip+Spade",
    soil_modification_depth == "Rip.60IncRip+Spade.30" ~ "IncRip+Spade",
    
    soil_modification_depth == "Rip.30IncRip" ~           "IncRip",
    soil_modification_depth == "Rip.40IncRip" ~           "IncRip",
    soil_modification_depth == "Rip.45IncRip" ~           "IncRip",
    soil_modification_depth == "Rip.50IncRip" ~           "IncRip",
    soil_modification_depth == "Rip.60IncRip" ~           "IncRip",
    
    
    TRUE ~ "NA"
  ))


#2 all soil modification without depth (if 2 soil modification used keep this)	

df <- df %>% 
  mutate(soil_modification = case_when(
    soil_modification_2 == "NA" ~ soil_modification_1,
    TRUE ~ soil_modification_2
  ))


### remove the working clms
df <- df %>% 
  dplyr::select(-soil_modification_1,
                -soil_modification_2)


#how many modification without depth
check_df <- df %>% 
  distinct(soil_modification) %>% 
  arrange(soil_modification) %>% 
  filter(soil_modification != "Unmodified")

count(check_df) #8 excluding the unmodified 
check_df

################################################################################
# soil amendments ----------------------------------------



# df1 <- df %>% select(Descriptors, site_display)
# df1 <- df1 %>% distinct(Descriptors)

df <- df %>% 
  mutate(soil_amendments = case_when(
    Descriptors == "Spade.30_Cereal.incorp_30" ~        "Cl",
    Descriptors == "Rip.30_Cl.band_30" ~                "Cl",
    Descriptors == "Rip.60_Cl.band_60" ~                "Cl",
    Descriptors == "Spade.30_Cl.incorp_30" ~            "Cl",
    Descriptors == "Rip.30_Cl.surface" ~           "Cl",
    Descriptors == "Rip.50_Cl.surface" ~           "Cl",
    Descriptors == "Rip.50IncRip_Cl.surface" ~           "Cl",
    Descriptors == "Rip.60_Cl.surface" ~           "Cl",
    Descriptors == "Unmodified_Cl.surface" ~           "Cl",
    Descriptors == "Rip.50_Cl@2.5.surface_Yr18,19,20" ~           "Cl",
    Descriptors == "Unmodified_Cl@2.5.surface_Yr18,19,20" ~           "Cl",
    Descriptors == "Rip.50_Cl@20.incorp_20" ~           "Cl",
    Descriptors == "Rip.50_Cl@20.incorp_20.Clay.incorp_20" ~           "ClClay",
    Descriptors == "Unmodified_Cl@20.incorp_8" ~           "Cl",
    Descriptors == "Sweep.30_Cl@3.incorp_30" ~           "Cl",
    Descriptors == "Unmodified_Cl@3.incorp_8" ~           "Cl",
    Descriptors == "Rip.50_Cl@5.incorp_20" ~           "Cl",
    Descriptors == "Rip.50_Cl@5.incorp_20.Clay.incorp_20" ~           "ClClay",
    Descriptors == "Unmodified_Cl@5.incorp_8" ~           "Cl",
    Descriptors == "Sweep.30_Cl@6.incorp_30" ~           "Cl",
    Descriptors == "Rip.30_Cl@7.5.band_30" ~           "Cl",
    Descriptors == "Rip.50_Cl@7.5.band_50" ~           "Cl",
    Descriptors == "Rip.50IncRip_Cl@7.5.incorp_50" ~           "Cl",
    Descriptors == "Rip.30_Cl@7.5.surface" ~           "Cl",
    Descriptors == "Rip.50_Cl@7.5.surface" ~           "Cl",
    Descriptors == "Unmodified_Cl@7.5.surface" ~           "Cl",
    Descriptors == "Sweep.30_Cl@9.incorp_30" ~           "Cl",
    Descriptors == "Sweep.30_Cl@9.incorp_30_Yr17,18,19" ~           "Cl",
    
    Descriptors == "Rip.50_Clay.incorp_20" ~           "Clay",
    Descriptors == "Spade.30_Clay.incorp_30" ~           "Clay",
    Descriptors == "Unmodified_Clay.incorp_10" ~           "Clay",
    Descriptors == "Unmodified_Clay.incorp_8" ~           "Clay",
    Descriptors == "Spade.30_Clay@250.incorp_30" ~           "Clay",
    
    Descriptors == "Unmodified_Cl@20.incorp_8.Clay.incorp_8" ~           "ClClay",
    Descriptors == "Unmodified_Cl@5.incorp_8.Clay.incorp_8" ~           "ClClay",
    
    Descriptors == "Rip.50_Cl@20.incorp_20.Fert.surface" ~           "ClFert",
    Descriptors == "Unmodified_Cl@20.incorp_8.Fert.surface" ~           "ClFert",
    Descriptors == "Rip.50_Cl@5.incorp_20.Fert.surface" ~           "ClFert",
    Descriptors == "Unmodified_Cl@5.incorp_8.Fert.surface" ~           "ClFert",
    
    Descriptors == "Rip.50_Cl@20.incorp_20.Fert.surface.Clay.incorp_20" ~           "ClFertClay",
    Descriptors == "Unmodified_Cl@20.incorp_8.Fert.surface.Clay.incorp_8" ~           "ClFertClay",
    Descriptors == "Rip.50_Cl@5.incorp_20.Fert.surface.Clay.incorp_20" ~           "ClFertClay",
    Descriptors == "Unmodified_Cl@5.incorp_8.Fert.surface.Clay.incorp_8" ~           "ClFertClay",
    
    Descriptors == "Spade.30_Cl.incorp_30.Gypsum.incorp_30" ~           "ClGypsum",
    Descriptors == "Sweep.30_Cl@3.incorp_30.Lime.incorp_8" ~           "ClLime",
    Descriptors == "Spade.30_Com.incorp_30" ~           "Com",
    
    Descriptors == "Spade.30_Com.incorp_30" ~           "Fert",
    Descriptors == "Rip_30+30_Fert.banded_30" ~           "Fert",
    Descriptors == "Rip_30+30_Fert.banded_30_annual" ~           "Fert",	
    Descriptors == "Rip.45IncRip_Fert_APP.band_45" ~           "Fert",	
    Descriptors == "Rip.45IncRip_Fert_High.band_45" ~           "Fert",
    Descriptors == "Rip.45IncRip_Fert_Low.band_45" ~           "Fert",
    Descriptors == "Rip.30_Fert.band_30" ~           "Fert",
    Descriptors == "Rip.30_Fert.band_8" ~           "Fert",
    Descriptors == "Rip.60_Fert.band_60" ~           "Fert",
    Descriptors == "Rip.60_Fert.band_8" ~           "Fert",
    Descriptors == "Unmodified_Fert.band_30" ~           "Fert",
    Descriptors == "Unmodified_Fert.band_8" ~           "Fert",
    
    Descriptors == "Unmodified_Fert.foliar" ~           "Fert",
    Descriptors == "Rip.30_Fert.incorp_30" ~           "Fert",
    Descriptors == "Rip.40_Fert.incorp_40" ~           "Fert",
    Descriptors == "Rip.45IncRip_Fert.incorp_45" ~           "Fert",
    Descriptors == "Spade.30_Fert.incorp_30" ~           "Fert",
    Descriptors == "Spade.30_Fert.incorp_30.K_added.incorp_30" ~           "Fert",
    Descriptors == "Unmodified_Fert.incorp_8" ~           "Fert",
    Descriptors == "Rip.30_Fert.surface" ~           "Fert",
    Descriptors == "Rip.50_Fert.surface" ~           "Fert",
    Descriptors == "Unmodified_Fert.surface" ~           "Fert",
    Descriptors == "Spade.30_K_added.surface" ~           "Fert",
    Descriptors == "Unmodified_K_added.surface" ~           "Fert",
    Descriptors == "Unmodified_none.Fert.surface" ~           "Fert",
    Descriptors == "Pre_drill_20+20_Fert.banded_20" ~           "Fert",
    Descriptors == "Pre_drill_20+20_Fert.banded_20_annual" ~           "Fert",
    
    Descriptors == "Spade.30_Clay@500.incorp_30_Yr07,20" ~           "FertClay",
    Descriptors == "Unmodified_Fert.band_30.Clay.incorp_10" ~           "FertClay",
    Descriptors == "Spade.30_Fert.incorp_30.Clay.incorp_30" ~           "FertClay",
    Descriptors == "Rip.50_Fert.surface.Clay.incorp_20" ~           "FertClay",
    Descriptors == "Unmodified_Fert.surface.Clay.incorp_8" ~           "FertClay",
    
    Descriptors == "Spade.30_Gypsum.incorp_30" ~           "Gypsum",
   
    Descriptors == "Rip.30_Lc.band_30" ~           "Lc",
    Descriptors == "Rip.30+60_Lc.band_30+60" ~           "Lc",	
    Descriptors == "Rip.60_Lc.band_60" ~           "Lc",
    Descriptors == "Rip.30_Lc.incorp_30" ~           "Lc",
    Descriptors == "Rip.40_Lc.incorp_40" ~           "Lc",
    Descriptors == "Rip.60Spade.30_Lc.incorp_30+band_60" ~           "Lc",
    Descriptors == "Spade.30_Lc.incorp_30" ~           "Lc",
    Descriptors == "Unmodified_Lc.surface" ~           "Lc",
    Descriptors == "Spade.30_Lc@1.incorp_30" ~           "Lc",
    Descriptors == "Spade.30_Lc@1.incorp_30.K_added.surface" ~           "Lc",
    Descriptors == "Spade.30_Lc@10.incorp_30" ~           "Lc",
    Descriptors == "Spade.30_Lc@10.incorp_30.K_added.surface" ~           "Lc",
    Descriptors == "Spade.30_Lc@15.incorp_30" ~           "Lc",
    Descriptors == "Spade.30_Lc@15.incorp_30.K_added.surface" ~           "Lc",
    Descriptors == "Spade.30_Lc@2.incorp_30" ~           "Lc",
    Descriptors == "Spade.30_Lc@2.incorp_30.K_added.surface" ~           "Lc",
    Descriptors == "Spade.30_Lc@20.incorp_30" ~           "Lc",
    Descriptors == "Spade.30_Lc@20.incorp_30.K_added.surface" ~           "Lc",
    Descriptors == "Spade.30_Lc@4.incorp_30" ~           "Lc",
    Descriptors == "Spade.30_Lc@4.incorp_30.K_added.surface" ~           "Lc",
    Descriptors == "Spade.30_Lc@6.incorp_30" ~           "Lc",
    Descriptors == "Spade.30_Lc@6.incorp_30.K_added.surface" ~           "Lc",
    Descriptors == "Spade.30_Lc@8.incorp_30" ~           "Lc",
    Descriptors == "Spade.30_Lc@8.incorp_30.K_added.surface" ~           "Lc",
    Descriptors == "Rip.30_none" ~           "none",
    
    Descriptors == "Spade.30_Lc.incorp_30.Clay.incorp_30" ~           "LcClay",
    Descriptors == "Spade.30_Lc.incorp_30.Fert.incorp_30" ~           "LcFert",
    Descriptors == "Spade.30_Lc.incorp_30.Fert.incorp_30.Clay.incorp_30" ~           "LcFertClay",
    Descriptors == "Sweep.30_Lime.incorp_30" ~           "Lime",
    
    Descriptors == "Spade.30_Vetch.incorp_30" ~           "Vet",	
    Descriptors == "Spade.30_Vet_Cer.incorp_30" ~           "VetCer",	
    Descriptors == "Spade.30_Vet_Cer_In.incorp_30" ~           "VetCer",	
    
    TRUE ~ "none"
  ))




df<- df %>% 
  mutate(amendments_grouping = case_when(
    soil_amendments ==  "none" ~ "none",
    
    soil_amendments ==  "Cl" ~ "animal",
    
    soil_amendments ==  "Lc" ~ "plant",
    soil_amendments ==  "Cereal" ~ "plant",
    soil_amendments ==  "Vetch" ~ "plant",
    soil_amendments ==  "Vet_Cer" ~ "plant",
    soil_amendments ==  "Vet_Cer_In" ~ "plant", 
    soil_amendments ==  "Com" ~ "plant",
    
    soil_amendments ==  "Vet" ~ "plant",
    soil_amendments ==  "VetCer" ~ "plant",
    
    soil_amendments ==  "Fert" ~        "fertiliser",
    
    soil_amendments ==  "Gypsum" ~ "non organic",
    soil_amendments ==  "Clay" ~ "non organic",
    soil_amendments ==  "Lime" ~ "non organic",
    
    soil_amendments ==  "ClFertClay" ~ "mixed",
    soil_amendments ==  "ClFert" ~ "mixed",
    soil_amendments ==  "ClClay" ~ "mixed",
    soil_amendments ==  "ClLime" ~ "mixed",
    soil_amendments ==  "ClGypsum" ~ "mixed",
    
    soil_amendments ==  "LcFert" ~ "mixed",
    soil_amendments ==  "LcClay" ~ "mixed",
    soil_amendments ==  "LcFertClay" ~ "mixed",
    soil_amendments ==  "LcK_added" ~ "mixed",
    
    soil_amendments ==  "FertClay" ~ "mixed",
    
    
    TRUE ~ "check"
    
  ))


df <- df %>% 
  mutate(crop = case_when(
    crop == "Lentil" ~ "Lentils",
    crop == "lentil" ~ "Lentils",
    crop == "lentils" ~ "Lentils",
    crop == "Lentils" ~ "Lentils",
    crop == "Lupin" ~ "Lupins",
    crop == "Lupins" ~ "Lupins",
    crop == "lupin" ~ "Lupins",
    crop == "lupins" ~ "Lupins",
    crop == "Peas" ~ "Peas",
    crop == "barley" ~ "Barley",
    crop == "Barley" ~ "Barley",
    crop == "beans" ~ "Beans",
    crop == "canola" ~ "Canola",
    crop == "Canola" ~ "Canola",
    crop == "triticale" ~ "Triticale",
    crop == "vetch" ~ "Vetch",
    crop == "Vetch" ~ "Vetch",
    crop == "wheat" ~ "Wheat",
    crop == "Wheat" ~ "Wheat",
  ))



check <- df %>% distinct(crop) 

check
names(df)

### class into tillage type
df <- df %>% 
  mutate(tillage_class = case_when(
    soil_modification == "DiscInv" ~          "Inversion",
    soil_modification == "IncRip" ~          "Ripping",
    soil_modification == "IncRip+Spade" ~    "Combination",
    soil_modification == "Pre" ~              "Ripping",
    soil_modification == "Rip"       ~       "Ripping",
    soil_modification == "Rip+Spade" ~       "Combination",
    soil_modification == "Spade" ~            "Mixing",
    soil_modification == "Sweep" ~            "Ripping",
    soil_modification == "Unmodified" ~        "Unmodified",
      .default = "check"
    
  ))

check <- df %>% distinct(soil_modification, .keep_all = TRUE) %>% select(soil_modification, tillage_class)
check
str(df)
amendments_grouping_check <- df %>% distinct(amendments_grouping, .keep_all = TRUE) %>% 
  select(amendments_grouping, Descriptors)
amendments_grouping_check

df <- df %>% 
  mutate(amendments_no_amend = case_when(
    amendments_grouping == "animal" ~ "amendment",
    amendments_grouping == "non organic" ~ "amendment",
    amendments_grouping == "mixed" ~ "amendment",
    amendments_grouping == "fertiliser" ~ "amendment",
    amendments_grouping == "plant" ~ "amendment",
    amendments_grouping == "none" ~ "no_amendment",
    .default = "check"
    ))


df <- df %>% 
  mutate(tillage_amendments_class = paste0(tillage_class, "_", amendments_no_amend))

amendments_grouping_check <- df %>% distinct(tillage_amendments_class, .keep_all = TRUE) %>% 
  select(tillage_amendments_class, tillage_class, amendments_no_amend )
amendments_grouping_check





### This needs to be better. Not sure why its not working.
df <- df %>% 
  mutate(yield_gain = yield-control_yield,
         relative_yld_change = ((yield- control_yield)/control_yield)*100)
brooker_df <- df %>% filter(site == "Brooker")


## check number of sites
test <- df %>% select(site_display, site) %>% distinct(site, .keep_all = TRUE)
test
dim(test)

max(df$year) - min(df$year)
df %>%  dplyr::distinct(Descriptors) %>% count()

df %>%  dplyr::distinct(Descriptors) %>% arrange(Descriptors)
df_tillage <- df %>%  filter()



###############################################################################
## only keep the tillage treatments
df_tillage <- df %>% 
  filter(!str_detect(Descriptors, "^Unmodified"))

## check number of sites
test <- df_tillage %>% select(site_display, site) %>% distinct(site, .keep_all = TRUE)
test
dim(test)

max(df_tillage$year) - min(df_tillage$year)
df_tillage %>%  dplyr::distinct(Descriptors) %>% count()

df_tillage %>%  dplyr::distinct(Descriptors) %>% arrange(Descriptors)

##############################################################################
## write out files for the next steps

write_csv(df,"N:/sandy soils conference/data/data for SS prestenation/control_metadata_contraints.csv" )
write_csv(df_tillage,"N:/sandy soils conference/data/data for SS prestenation/control_metadata_contraints_tillage_only.csv" )
