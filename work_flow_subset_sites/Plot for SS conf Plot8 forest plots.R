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
         relative_yld_change,
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

site_crop_step1 <- df_modified  %>% 
  group_by(site_display, year, crop_group) %>% 
  summarise(n = n()) 
site_crop_step1
site_crop_step2 <- site_crop_step1 %>% 
  group_by(site_display, crop_group) %>% 
  summarise(n = n()) 
site_crop_step3 <- site_crop_step1 %>% 
  group_by(site_display) %>% 
  summarise(numb_seasons = n())

site_crop <- left_join(site_crop_step2, site_crop_step3) 
site_crop <- site_crop %>% 
  mutate(percent_season = n/numb_seasons)

site_crop


site_crop$crop_group <- factor(
  site_crop$crop_group,
  levels = c( "Oilseeds", "Grain Legumes", "Cereals"),
  labels = c(
    "Oilseeds",
    "Grain Legumes",
    "Cereals",
  )
)
site_crop

site_crop %>% 
  ggplot(aes(y = percent_season, x = site_display , fill = crop_group)) +
  geom_col() +
  scale_fill_manual(values=c(  "darkblue","cornflowerblue","lightblue" ))+
  scale_y_continuous(labels = scales::percent)+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="bottom",
        legend.title = element_blank())+
  
  xlab("") + 
  ylab("Precentage of trial years with crop type") + 
  labs(title = "",
       caption = "Cereals = Wheat & Barley, Oilseeds = Canola, Grain Legumes = Beans, Peas, Lentils, Lupins" ) 



################################################################################
### Option 2 means ----
#### using Yield gains mean of the treatments ---

## site_season
str(df_modified)
distinct(df_modified,crop_group )



df_modified_summary_yld_gain_crop_Type <- df_modified %>% 
  group_by(crop_group) %>% 
  summarise(mean = mean(relative_yld_change, na.rm = TRUE),
            sd = sd(relative_yld_change, na.rm = TRUE),
            n = n()) 
df_modified_summary_yld_gain_crop_Type

df_modified_summary_yld_gain_crop_Type$crop_group <- factor(
  df_modified_summary_yld_gain_crop_Type$crop_group,
  levels = c("Oilseeds",  "Grain Legumes",  "Cereals"),
  labels = c(
    "Oilseeds",
    "Grain Legumes",
    "Cereals"
  )
)


df_modified_summary_yld_gain_crop_Type
m.meanYG <- metamean(n = n,
                     mean = mean,
                     sd = sd,
                     studlab = crop_group,
                     data = df_modified_summary_yld_gain_crop_Type,
                     sm = "MRAW",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     method.random.ci = "HK",
                     title = "Option 2")
summary(m.meanYG)
### home made plots
dim(df_modified_summary_yld_gain_crop_Type)
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
  
  labs(title = "Years with crop type")+ 
  xlab("Relative yield change (95% CI)") + 
  ylab("") + 
  xlim(0, 100)+
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
