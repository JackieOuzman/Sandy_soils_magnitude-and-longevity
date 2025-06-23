
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

                              
soil_constraints <- read.csv("N:/sandy soils conference/data/Shiny_data/primary_data_all_9_05_2023.csv")
names(soil_constraints)

#keep a subset of data
site_unique <- soil_constraints %>% 
  dplyr::select(site, Repellence, Acidity, Physical, Nutrient, Latitude,  Longitude, rain  )

#keep a data for sites only - remove the duplicates
site_unique <- site_unique %>%
   dplyr::distinct(site, .keep_all = TRUE)

site_unique <- site_unique %>% 
  mutate(sum_contraints = Repellence+Acidity+Physical )

### number of sites
site_unique %>%  count()
max(soil_constraints$year)
min(soil_constraints$year)

max(soil_constraints$year) - min(soil_constraints$year)
soil_constraints %>%  dplyr::distinct(Descriptors) %>% count()


write_csv(site_unique,"N:/sandy soils conference/data/data for SS prestenation/contraints_mapping.csv" )


################################################################################
# double check the sites ----------------------------------------
names(soil_constraints)


list_of_sites <- as.data.frame(dplyr::arrange(dplyr::distinct(soil_constraints,site)) )
list_of_sites <- dplyr::arrange(list_of_sites, site)
list_of_sites

dplyr::arrange(list_of_sites$`unique(soil_constraints$site)`)
names(summary_control_data_all_soil_constraints)


### fix up some names

##label problem with Mt Damper
soil_constraints <- soil_constraints %>% 
  mutate(
    site = case_when(
      site == "Mt Damper" ~ "Mt_Damper",
      TRUE ~ site))

##label problem with Ouyen
soil_constraints <- soil_constraints %>% 
  mutate(
    site = case_when(
      site == "Oyen_Spade" ~ "Ouyen_Spade",
      TRUE ~ site))
soil_constraints <- soil_constraints %>% 
  mutate(
    site = case_when(
      site == "Oyen_Spade" ~ "Ouyen_Spade",
      TRUE ~ site))


################################################################################
# remove some sites and treatments ----------------------------------------

# remove Young husband wetter
soil_constraints <- soil_constraints %>%
  filter(Descriptors !=   "SE14.band_8"&
         Descriptors !=   "Bi_Agra.surface+band_8"  &
         Descriptors !=   "OnRow" ) 


Younghusband_treatments <- 
  soil_constraints %>% 
  dplyr::filter(site == "Younghusband") %>% 
  dplyr::select(site, Descriptors) %>% 
  dplyr::distinct(Descriptors)
print(arrange(Younghusband_treatments))
rm(Younghusband_treatments)




# soil modifications ----------------------------------------

names(soil_constraints)
soil_constraints$soil_modification

soil_modification <- as.data.frame(dplyr::distinct(soil_constraints,soil_modification) )
soil_modification <- dplyr::arrange(soil_modification, soil_modification)
soil_modification

# soil modifications depth ----------------------------------------
# I don't have this information.
# But I am not sure it is needed what you set out to do is not always the depth you achieved.


# amendment_all ----------------------------------------
amendment_all <- as.data.frame(dplyr::distinct(soil_constraints,amendment_all) )
amendment_all <- dplyr::arrange(amendment_all, amendment_all)
amendment_all




# Facts ----------------------------------------

#How many site?
soil_constraints %>%
  distinct(site)%>%
  count()

#how many Descriptors?
soil_constraints %>%
  distinct(Descriptors)%>%
  count()

#how many soil_modification
soil_constraints %>%
  distinct(soil_modification)%>%
  count()

#how many amendment_all? the 123 has the application depth removed
soil_constraints %>%
  distinct(amendment_all)%>%
  count()


# New df.  Control yld vs  treatment yield  ------------------------------------
soil_constraints %>% distinct(Descriptors)
names(soil_constraints)


df_control <- soil_constraints %>% filter(Descriptors == "Control") %>% 
  select(site, year, plot, Descriptors, yield, soil_modification, amendment_all, yr_post_amelioration, rain, decile, ID) %>% 
  rename(control_yield = yield)

df_treatments <- soil_constraints %>% filter(Descriptors != "Control") %>% 
  select(site, year, Descriptors, yield, soil_modification, amendment_all, yr_post_amelioration, rain, decile, ID) 


df_control_treatments <- left_join(df_control, df_treatments)


# Plots ----------------------------------------

names(df_control_treatments)

ggplot(data = df_control_treatments, mapping = aes(control_yield, yield,)) +
  #geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  geom_point(alpha= 0.4) +
  geom_smooth(method = lm, se = FALSE) +
  # scale_x_continuous(breaks=seq(0,10,by=0.5))+
  # scale_y_continuous(breaks=seq(0,10,by=0.5))+
  theme_bw()+
  labs(title = "Control yield - all data - no summary\nnote each site, treatment, year and rep is matched to control",
       x = "control yield t/ha", y = "treatment yield t/ha")

