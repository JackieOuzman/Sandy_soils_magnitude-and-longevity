###Script for sandy soils data analysis
## which yield output to use?


library(ggplot2)
library(readxl)
library(tidyverse)
library(stringr)
library(ggpubr)
library(nlme)

#The data has been manipulated in "Analysis for sandy soils conf 2025 version 2.rmd
# With the output as 
# this should have the reps matched to a control

df <- read.csv("N:/sandy soils conference/data/data for SS prestenation/control_metadata_contraints.csv" )

## let have a play with a subset of sites"
unique(df$site_display)
df <- df %>% filter(  site_display == "Brimpton Lake"|
                      site_display == "Brooker"|
                      site_display == "Buckleboo"|
                      site_display == "Bute"|
                      site_display == "Bute boundary"|
                      site_display == "Cadgee"
                                            )



str(df)
df_selection  <- df %>% select(
  ID,
  site_display,
  year,
  crop,
  Descriptors,
  tillage_class,
  tillage_amendments_class,
  
  yield,
  control_yield,
  yr_post_amelioration,
  yield_gain,
  relative_yld_change,
  
  Repellence,
  Acidity,
  Physical,
  Nutrient
  )

unique(df_selection$site_display)

# df_selection need the met station number

df_selection  <- df_selection %>% 
  mutate(Station_number =
           case_when(
             site_display == "Brimpton Lake" ~ 018099,
             site_display == "Brooker"       ~ 018099,
             site_display == "Buckleboo" ~     018040,
             site_display == "Bute boundary" ~ 021012,
             site_display == "Bute" ~          021012,
             site_display == "Cadgee" ~        026015
           )
                                           )
df_selection <- df_selection %>% mutate(for_join = paste0(Station_number, year, crop))

str(df_selection)
################################################################################
#Bring in the NEW FRONT yield potentials by year and crop

NewFront_YP <- read.csv("N:/sandy soils conference/data/climate_data/Yld_potentails_for_DB/NewFront_YP_yr_decile_crop_long_multiple_sites.csv" )
str(NewFront_YP)
NewFront_YP <- NewFront_YP %>% mutate(for_join = paste0(Station_number, year, Crop)) %>%
  select(-Crop,-Station_number, -year) %>% 
  rename(YP_NewFront  = YP)

df_join <- left_join(df_selection, NewFront_YP,
                       by = join_by(for_join))

test_join_anti <- anti_join(df_selection, NewFront_YP,
                       by = join_by(for_join))
check <-  df_join %>%  distinct(for_join, .keep_all = TRUE) # this is just a check and it looks good
rm(check, test_join_anti)

str(df_join)
################################################################################
#Bring in the Base yield potentials by year and crop

Base_YP <- read.csv("N:/sandy soils conference/data/climate_data/Yld_potentails_for_DB/Base_YP_yr_decile_crop_long_multiple_sites.csv" )
str(Base_YP)
Base_YP <- Base_YP %>% mutate(for_join = paste0(Station_number, year, Crop)) %>% 
  select(for_join,YP )

df_join <- left_join(df_join, Base_YP, 
                     by = join_by(for_join))

# test_join_anti <- anti_join(df_selection, Base_YP, 
#                             by = join_by(for_join))    
# check <-  df_join %>%  distinct(for_join, .keep_all = TRUE) # this is just a check and it looks good
# rm(check, test_join_anti)

str(df_join)





################################################################################
## Plots to check what should I use
str(df_join)
df_join <- df_join %>% 
  mutate( lnR_Yield = log(yield / control_yield),
          YP_t_ha = (YP/1000),
          yld_gap_base = ((yield/(YP_t_ha)*100)),
          YP_t_ha_New_FRONT = (YP_NewFront/1000),
          yld_gap_base_New_FRONT = ((yield/(YP_t_ha_New_FRONT)*100)))


write_csv(df_join,"N:/sandy soils conference/data/data for SS prestenation/control_metadata_contraints_withYP_select_sites.csv" )


Yld_gain <- df_join %>%  
  filter(!is.na(Station_number )) %>% 
  filter(crop != "Lupins" & crop != "Lentils"  ) %>% 
  filter(tillage_amendments_class != "Unmodified_amendment"   ) %>% 
  
  ggplot( mapping = aes( tillage_amendments_class, yield_gain)) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  geom_point()+
  scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "Tillage type", y = "Yield gain t/ha")+
  facet_wrap(site_display~crop)
Yld_gain


Yld_change <- df_join %>%  
  filter(!is.na(Station_number )) %>% 
  filter(crop != "Lupins" & crop != "Lentils"  ) %>% 
  filter(tillage_amendments_class != "Unmodified_amendment"   ) %>% 
  filter(site_display != "Brooker") %>% 
  
  ggplot( mapping = aes( tillage_amendments_class, relative_yld_change)) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  geom_point()+
 # scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "Tillage type", y = "relative yeild change")+
  facet_wrap(site_display~crop)
Yld_change


Yld_gap_Base <- df_join %>%  
  filter(!is.na(Station_number )) %>% 
  filter(crop != "Lupins" & crop != "Lentils"  ) %>% 
  filter(tillage_amendments_class != "Unmodified_amendment"   ) %>% 
  
  ggplot( mapping = aes( tillage_amendments_class, yld_gap_base)) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  geom_point()+
  #scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 100, linetype = "dashed", colour = "red")+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "Tillage type", y = "yield gap using base")+
  facet_wrap(site_display~crop)
Yld_gap_Base


Yld_gap_Front <- df_join %>%  
  filter(!is.na(Station_number )) %>% 
  filter(crop != "Lupins" & crop != "Lentils"  ) %>% 
  filter(tillage_amendments_class != "Unmodified_amendment"   ) %>% 
  
  ggplot( mapping = aes( tillage_amendments_class, yld_gap_base_New_FRONT)) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  geom_point()+
  #scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 100, linetype = "dashed", colour = "red")+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "Tillage type", y = "yield gap using new front")+
  facet_wrap(site_display~crop)
Yld_gap_Front


lnR_Yield <- df_join %>%  
  filter(!is.na(Station_number )) %>% 
  filter(crop != "Lupins" & crop != "Lentils"  ) %>% 
  filter(tillage_amendments_class != "Unmodified_amendment"   ) %>%
  filter(site_display != "Brooker") %>% 
  
  ggplot( mapping = aes( tillage_amendments_class, lnR_Yield)) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  geom_point()+
  #scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "Tillage type", y = "InR Yield")+
  facet_wrap(site_display~crop)
lnR_Yield


################################################################################
#something weird going on with Brooker # I think I have fixed it?

Brooker <- df %>% filter( site_display == "Brooker")
#what are the years we are expecting 2019,2020,2021
check_brooker <- Brooker %>% group_by( year, Descriptors) %>% 
  summarise(count = n()) %>% 
  arrange(year)

check_brooker


