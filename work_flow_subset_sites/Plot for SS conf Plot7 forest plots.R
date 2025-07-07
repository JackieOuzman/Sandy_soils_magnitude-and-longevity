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
  ylab("Precentage of trail years with season type") + 
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
  summarise(mean = mean(yield_gain, na.rm = TRUE),
            sd = sd(yield_gain, na.rm = TRUE),
            n = n()) 
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
m.meanYG <- metamean(n = n,
                     mean = mean,
                     sd = sd,
                     studlab = Decile_group,
                     data = df_modified_summary_yld_gain_season_Type,
                     sm = "MRAW",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     method.random.ci = "HK",
                     title = "Option 2")
summary(m.meanYG)
### home made plots
dim(df_modified_summary_yld_gain_season_Type)
#how many rows of data ?
forest_plot_input_YG <- data.frame(
  #Index = seq(1:4), ## This provides an order to the data
  label = m.meanYG$studlab,
  SMD = m.meanYG$TE,
  LL = m.meanYG$lower,
  UL =  m.meanYG$upper,
  n = m.meanYG$n)

forest_plot_input_YG
forest_plot_input_YG <- forest_plot_input_YG %>% 
  arrange(SMD) %>% 
  dplyr::mutate(Index = seq(1:3))
forest_plot_input_YG


forest_plot_input_total_YG <- data.frame(
  Index = (3+1), ## This provides an order to the data
  label = "All tillage",
  SMD = m.meanYG$TE.random,
  LL = m.meanYG$lower.random,
  UL =  m.meanYG$upper.random,
  n =  sum(m.meanYG$n))
forest_plot_input_total_YG

forest_plot_input_YG <- rbind(forest_plot_input_YG,forest_plot_input_total_YG)
forest_plot_input_YG



forest_plot_input_YG <- forest_plot_input_YG %>% 
  mutate(label2 = paste0(label, "(", n, ")"))
forest_plot_input_YG
forest_plot_input_YG$label2
str(forest_plot_input_YG)


forest_plot_input_YG <- forest_plot_input_YG %>%
  mutate(label2 = fct_reorder(label2, Index)) %>%
  arrange(label2)


plot2 <-forest_plot_input_YG %>% 
  filter(label != "All tillage") %>% 
  ggplot(aes(y = label2, x = SMD)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  
  labs(title = "Years with season rainfall type")+ 
  xlab("Yield gain (95% CI)") + 
  ylab("") + 
  xlim(-0.2, 1.2)+
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))

plot2
