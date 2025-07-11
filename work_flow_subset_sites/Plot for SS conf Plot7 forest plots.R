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
  select(tillage_amendments_class,
         site_display,
         year,
         yield_gain,
         yield,
         relative_yld_change,
         control_yield,
         tillage_amendments_class,
         tillage_class,
         #Physical,
         #Nutrient,
         #Acidity,
         #Repellence,
         rain,
         crop,
         decile
         
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
str(df_modified)


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



site_season_step1 <- df_modified  %>% 
  group_by(site_display, Decile_group, year, decile) %>% 
  summarise(n = n()) 
site_season_step2 <- site_season_step1 %>% 
  group_by(site_display, Decile_group) %>% 
  summarise(n = n()) 
site_season_step3 <- site_season_step1 %>% 
  group_by(site_display) %>% 
  summarise(numb_seasons = n())

site_season <- left_join(site_season_step2, site_season_step3) 
site_season <- site_season %>% 
  mutate(percent_season = n/numb_seasons)

site_season
# site_season <- df_modified  %>% 
#   group_by(site_display, Decile_group, year, decile) %>% 
#   summarise(mean = mean(yield_gain, na.rm = TRUE),
#             sd = sd(yield_gain, na.rm = TRUE),
#             n = n()) 

site_season
distinct(site_season, Decile_group)

site_season$Decile_group <- factor(
  site_season$Decile_group,
  levels = c("above_av",  "average", "below_av"),
  labels = c(
    "Above average rainfall",
    "Average rainfall",
    "Below average rainfall"
  )
)
# how can we visual this?

site_season %>% 
  ggplot(aes(y = percent_season, x = site_display , fill = Decile_group)) +
  geom_col() +
  scale_fill_manual(values=c(  "darkblue","cornflowerblue","lightblue" ))+
  scale_y_continuous(labels = scales::percent)+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="bottom",
        legend.title = element_blank())+
  
  xlab("") + 
  ylab("Precentage of trial years with season type") + 
  labs(title = "",
       caption = "Above average rainfall = decile 8-10, below average = decile 1-3. Decile define as GS rainfall + 25% of summer" ) 



################################################################################
### Option 2 means ----
#### using Yield gains mean of the treatments ---

## site_season
str(df_modified)
distinct(df_modified,Decile_group )



df_modified_summary_yld_gain_season_Type <- df_modified %>% 
  group_by(Decile_group) %>% 
  summarise(mean = mean(relative_yld_change, na.rm = TRUE),
            sd = sd(relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n))   
df_modified_summary_yld_gain_season_Type

df_modified_summary_yld_gain_season_Type$Decile_group <- factor(
  df_modified_summary_yld_gain_season_Type$Decile_group,
  levels = c("below_av",  "average",  "above_av"),
  labels = c(
    "Below average rainfall",
    "Above average rainfall",
    "Average rainfall"
  )
)

df_modified_summary_yld_gain_season_Type



df_modified_summary_yld_gain_season_Type <- df_modified_summary_yld_gain_season_Type %>% 
  mutate(label2 = paste0( Decile_group , "(", n, ")"))
df_modified_summary_yld_gain_season_Type

df_modified_summary_yld_gain_season_Type <- df_modified_summary_yld_gain_season_Type %>% arrange(mean    ) %>% 
  dplyr::mutate(Index = seq(1:3))



plot2b <-df_modified_summary_yld_gain_season_Type %>% 
  #filter(label != "All tillage") %>% 
  ggplot(aes(y = label2, x = mean)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  
  labs(title = "Years with season rainfall type")+ 
  xlab("Relative yield change (SE)") + 
  ylab("") + 
  xlim(-10, 150)+
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))

plot2b
df_modified_summary_yld_gain_season_Type
