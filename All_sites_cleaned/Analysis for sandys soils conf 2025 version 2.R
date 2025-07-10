###Script for sandy soils data analysis

library(ggplot2)
library(readxl)
library(tidyverse)
library(stringr)
library(ggpubr)


################################################################################
# Import the data that has been matched to control  and the primary data ie what is on the DAP ----------

                                    
soil_constraints_control <- read.csv("N:/sandy soils conference/data/data from project DB/sites_control_merged.csv")
names(soil_constraints_control)

soil_constraints <- read.csv("N:/sandy soils conference/data/Shiny_data/primary_data_all_9_05_2023.csv")
names(soil_constraints)


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



### number of sites
site_unique %>%  count()


write_csv(site_unique,"N:/sandy soils conference/data/All_sites_cleaned/contraints_mapping.csv" )



################################################################################
# Append  constraints and metatdata to control data  ---------------------------
names(soil_constraints_control)

names(site_unique)
soil_constraints_metadata_only <- soil_constraints %>% 
  select(site, year, Repellence, Acidity, Physical, Nutrient, rain)

soil_constraints_metadata_only <- soil_constraints_metadata_only %>% 
  distinct(site, year,Repellence, Acidity, Physical, Nutrient, rain) %>% 
  mutate(join_1 =paste0(site, year)) %>% 
  select(-site, -year)

soil_constraints_control <- soil_constraints_control %>% 
  mutate(join_1 =paste0(site, year))

################################################################################
# remove the sites not "approved"  ----------------------------------------

soil_constraints_control <- soil_constraints_control %>% filter(site_display != "remove")

length(unique(soil_constraints_control$site))
length(unique(soil_constraints_metadata_only$site))

df <- left_join(soil_constraints_control,soil_constraints_metadata_only, join_by( join_1)  )
str(df)

df <- df %>% select(-join_1)

################################################################################
# double check the sites ----------------------------------------
names(df)


list_of_sites <- as.data.frame(dplyr::arrange(dplyr::distinct(df,site_display)) )
list_of_sites <- dplyr::arrange(list_of_sites, site_display)
list_of_sites




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




# There are a few sites that need more cleaning
# crops = Vetch and Triticale have no yld record so I can remove these:)
str(df)

distinct(df_tillage,crop)

df <- df %>% filter(crop!= "Vetch",  crop!=  "Triticale")
df_tillage <- df_tillage %>% filter(crop!= "Vetch",  crop!=  "Triticale")

distinct(df_test,crop)
distinct(df_tillage,crop)

# There are a few sites that need more cleaning
# Cadgee yld 2016 is missing in the app so lets remove these rows
distinct(df,site_display)

df <- df %>% filter(!(site_display == "Cadgee"&  year== 2016))
df_tillage <- df_tillage %>% filter(!(site_display == "Cadgee"&  year== 2016))

##### some treatments have very low yields I am not sure what to do with this.
# I would image yields this low reflect an error or something?
# so I will remove rows with yields and control yield lower than 0.070

df <- df %>% filter(yield >= 0.07 ,control_yield >= 0.07)
df_tillage <- df_tillage %>% filter(yield >= 0.07 ,control_yield >= 0.07)


rm(list=ls()[! ls() %in% c("df","df_tillage")])

##############################################################################
### add an extra yield response variable
df <- df %>% 
  mutate(lnR_Yield = log(yield / control_yield))
##############################################################################

################################################################################
#Heap of sites with no yield- not sure what that is about? I think this was done already
df <- df %>% filter(yield!= 0.0)
df <- df %>% filter(control_yield>  0.0)
################################################################################


################################################################################
### Make some new variable 


################################################################################
## multiple_constraints
#recode ranked as moderate or severe

df <- df %>% 
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
df <- df %>% 
  mutate(multiple_constraints =
           (Physical_M_S +  Nutrient_M_S +  Acidity_M_S + Repellence_M_S))

df <- df %>% select(-Physical_M_S, -Nutrient_M_S, -Acidity_M_S, -Repellence_M_S)

################################################################################
# Cal season type
# get some more information about the run of sesaons at each site.
# group into 3 season classes below ave, av and above av

distinct(df,decile )

df <- df %>% 
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
distinct(df,crop )

#Cereals (Wheat ,Barley ,Oats), Oilseeds (Canola), Grain Legumes (Beans, Peas,Lentils, Lupins )
df <- df %>% 
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
distinct(df,yr_post_amelioration )

df <- df %>% 
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
###############################################################################
#Check new variable
unique(df$multiple_constraints)
unique(df$Decile_group)
unique(df$crop_group)
unique(df$post_tillage_group)


##############################################################################
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

###############################################################################
## write out files for the next steps

names(df)
names(df_tillage)

write_csv(df,"N:/sandy soils conference/data/All_sites_cleaned/control_metadata_contraints.csv" )
write_csv(df_tillage,"N:/sandy soils conference/data/All_sites_cleaned/control_metadata_contraints_tillage_only.csv" )


###############################################################################
## clean data better and save! Cleaning is based on relative yield change

# examine the distribution
hist(df_tillage$relative_yld_change)
summary(df_tillage$relative_yld_change, na.rm=T)

quantile(df_tillage$relative_yld_change,               # specify numeric vector to work on
         probs = c(0, .25, .50, .75, .90, .95),   # specify the percentiles you want
         na.rm = TRUE)                            # ignore missing values 

ggplot(df_tillage, aes(relative_yld_change))+ geom_dotplot()
################################################################################
#cal the z score 1.
df_tillage <- df_tillage %>% 
  mutate(z_scores1 =
           (relative_yld_change-mean(relative_yld_change))/sd(relative_yld_change))
#trim z score 3 stdve away from mean1.
trim_once <- df_tillage %>% 
  filter(between(z_scores1, -3,3))


################################################################################
#cal the z score 2.
trim_once <- trim_once %>% 
  mutate(z_scores2 =
           (relative_yld_change-mean(relative_yld_change))/sd(relative_yld_change))
#trim z score 3 stdve away from mean1.
trim_twice <- trim_once %>% 
  filter(between(z_scores2, -3,3))

################################################################################
#cal the z score 3.
trim_twice <- trim_twice %>% 
  mutate(z_scores3 =
           (relative_yld_change-mean(relative_yld_change))/sd(relative_yld_change))
#trim z score 3 stdve away from mean1.
trim_three <- trim_twice %>% 
  filter(between(z_scores3, -3,3))


################################################################################
#cal the z score 4.
trim_three <- trim_three %>% 
  mutate(z_scores4 =
           (relative_yld_change-mean(relative_yld_change))/sd(relative_yld_change))
#trim z score 3 stdve away from mean1.
trim_four <- trim_three %>% 
  filter(between(z_scores4, -3,3))

################################################################################
################################################################################
#cal the z score 5.
trim_four <- trim_four %>% 
  mutate(z_scores5 =
           (relative_yld_change-mean(relative_yld_change))/sd(relative_yld_change))
#trim z score 3 stdve away from mean1.
trim_five <- trim_four %>% 
  filter(between(z_scores5, -3,3))

################################################################################
#cal the z score 6.
trim_five <- trim_five %>% 
  mutate(z_scores6 =
           (relative_yld_change-mean(relative_yld_change))/sd(relative_yld_change))
#trim z score 3 stdve away from mean1.
trim_six <- trim_five %>% 
  filter(between(z_scores6, -3,3))

################################################################################
#cal the z score 7.
trim_six <- trim_six %>% 
  mutate(z_scores7 =
           (relative_yld_change-mean(relative_yld_change))/sd(relative_yld_change))
#trim z score 3 stdve away from mean1.
trim_seven <- trim_six %>% 
  filter(between(z_scores7, -3,3))
################################################################################
#cal the z score 8.
trim_seven <- trim_seven %>% 
  mutate(z_scores8 =
           (relative_yld_change-mean(relative_yld_change))/sd(relative_yld_change))
#trim z score 3 stdve away from mean1.
trim_eight <- trim_seven %>% 
  filter(between(z_scores8, -3,3))
################################################################################
################################################################################
#cal the z score 9.
trim_eight <- trim_eight %>% 
  mutate(z_scores9 =
           (relative_yld_change-mean(relative_yld_change))/sd(relative_yld_change))
#trim z score 3 stdve away from mean1.
trim_nine <- trim_eight %>% 
  filter(between(z_scores8, -3,3))

# this is the same as eight we have trimmed within 3 stdev of mean 
################################################################################


hist(trim_eight$relative_yld_change)
quantile(trim_nine$relative_yld_change,               # specify numeric vector to work on
         probs = c(0, .25, .50, .75, .90, .95),   # specify the percentiles you want
         na.rm = TRUE)                            # ignore missing values 

ggplot(trim_nine, aes(relative_yld_change))+ geom_dotplot()


rm(trim_once,trim_twice, trim_three, trim_four, trim_five, trim_six, trim_seven)
###############################################################################
df_tillage_trimmed <-trim_eight
#remove the z-score clm and working
df_tillage_trimmed <- df_tillage_trimmed %>% select(!starts_with("z_scores"))


## write out files for the next steps

names(df)
names(df_tillage)


write_csv(df_tillage_trimmed,"N:/sandy soils conference/data/All_sites_cleaned/control_metadata_contraints_tillage_only_cleaned.csv" )
