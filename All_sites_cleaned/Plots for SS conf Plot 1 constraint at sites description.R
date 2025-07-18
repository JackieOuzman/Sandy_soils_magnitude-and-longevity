library(ggplot2)
library(readxl)
library(tidyverse)
library(stringr)

## plot and analsyis for preso constraints at sandy soil sites

df <- read.csv("N:/sandy soils conference/data/All_sites_cleaned/control_metadata_contraints_tillage_only_cleaned.csv" )

unique(df$crop_group)
unique(df$tillage_class)

################################################################################
## constraints at sandy soils sites
str(df)



ss_constaints <- df %>% distinct(site_display, .keep_all = TRUE) %>% 
  select(site_display, Repellence, Acidity, Physical, Nutrient)  
  
str(ss_constaints)
print(ss_constaints)
#### 

ss_constaints_summary_R <- ss_constaints %>%  group_by() %>% 
  summarise(No_problem = (length(Repellence[Repellence==0])/26),
            Moderate =   (length(Repellence[Repellence==1])/26),
            Serve =      (length(Repellence[Repellence==2])/26)) %>% 
  mutate(Constraint = "Repellence")
ss_constaints_summary_R

ss_constaints_summary_A <- ss_constaints %>%  group_by() %>% 
  summarise(No_problem = (length(Acidity[Acidity==0])/26),
            Moderate =   (length(Acidity[Acidity==1])/26),
            Serve =      (length(Acidity[Acidity==2])/26)) %>% 
  mutate(Constraint = "Acidity")

ss_constaints_summary_P <- ss_constaints %>%  group_by() %>% 
  summarise(            
    No_problem = (length(Physical[Physical==0])/26),
    Moderate =   (length(Physical[Physical==1])/26),
    Serve =      (length(Physical[Physical==2])/26))%>% 
  mutate(Constraint = "Physical")
ss_constaints_summary_P

ss_constaints_summary_N <- ss_constaints %>%  group_by() %>% 
  summarise( No_problem = (length(Nutrient[Nutrient==0])/26),
             Moderate =   (length(Nutrient[Nutrient==1])/26),
             Serve =      (length(Nutrient[Nutrient==2])/26))%>% 
  mutate(Constraint = "Nutrient")

ss_constaints_summary <- rbind(ss_constaints_summary_R, 
                               ss_constaints_summary_A,
                               ss_constaints_summary_P,
                               ss_constaints_summary_N)

ss_constaints_summary

ss_constaints_summary_long <- ss_constaints_summary %>% 
  pivot_longer(cols = c(No_problem:Serve),
               names_to = "rank",
               values_to = "percent")

ss_constaints_summary_long


ss_constaints_multiple <-  ss_constaints %>% 
  mutate(multiple_mod_severe_R = case_when(Repellence == 1 |Repellence == 2 ~ 1, .default = 0)) %>% 
  mutate(multiple_mod_severe_A = case_when(Acidity == 1 |Acidity == 2 ~ 1, .default = 0)) %>% 
  mutate(multiple_mod_severe_N = case_when(Nutrient == 1 |Nutrient == 2 ~ 1, .default = 0)) %>% 
  mutate(multiple_mod_severe_P = case_when(Physical == 1 |Physical == 2 ~ 1, .default = 0)) %>% 
         
  mutate(multiple_mod_severe = (
           multiple_mod_severe_R+ 
           multiple_mod_severe_A+ 
           multiple_mod_severe_P+
           multiple_mod_severe_N)) %>% 
  select(site_display,multiple_mod_severe )


ss_constaints_summary_Numb <- ss_constaints_multiple %>%  group_by() %>% 
  summarise(constraints2 = (length(multiple_mod_severe[multiple_mod_severe==2])/26)*100,
            constraints3 = (length(multiple_mod_severe[multiple_mod_severe==3])/26)*100,
            constraints4 = (length(multiple_mod_severe[multiple_mod_severe==4])/26)*100)
ss_constaints_summary_Numb       


            
ss_constaints_summary_long
## order 
ss_constaints_summary_long$rank <- factor(ss_constaints_summary_long$rank, 
                                                       levels = c(
                                                         "No_problem" ,
                                                         "Moderate",
                                                         "Serve"
                                                       ),
                                                       labels = c(
                                                         "No problem",
                                                         "Moderate probelm",
                                                         "Serve probelm"
                                                       ))

ss_constaints_summary_long$Constraint <- factor(ss_constaints_summary_long$Constraint, 
                                          levels = c(
                                            "Acidity" ,
                                            "Repellence",
                                            "Nutrient",
                                            "Physical"))

ss_constaints_summary_long

### Plot
plot_constraints <- ss_constaints_summary_long %>%
  ggplot(aes(y = percent, x = Constraint , fill = rank)) +
  geom_col() +
  scale_fill_manual(values=c("lightgrey", "cornflowerblue", "darkblue"))+
  scale_y_continuous(labels = scales::percent)+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        #legend.position="bottom",
        legend.title = element_blank())+
  
  xlab("") + 
  ylab("Precentage of site") + 
  labs(title = "")  
plot_constraints
ss_constaints_summary_long %>% filter(rank != "No problem") %>% arrange(percent)

ss_constaints_summary_long %>% 
  dplyr::filter(rank != "No problem") %>% 
  dplyr::group_by(Constraint) %>% 
  dplyr::summarise(percent_sum = sum(percent)) %>% 
  arrange(percent_sum)

################################################################################

## number of sites
number_of_sites <- df %>% select(site_display, site) %>% distinct(site, .keep_all = TRUE)
number_of_sites
dim(number_of_sites)

## Years trials were run
max(df$year)
min(df$year)
max(df$year) - min(df$year)

## number of different treatments
df %>%  dplyr::distinct(Descriptors) %>% count()
str(df)
treatments_with_tillage <- df %>%  group_by(tillage_amendments_class, Descriptors) %>% 
  summarise(n = n())
length(treatments_with_tillage$n)

treatments_with_tillage %>%  group_by(tillage_amendments_class) %>% 
  summarise(n = n())

str(df)
numb_site_yr_treatments <- df %>%  group_by( ) %>% 
  summarise(n = n())
numb_site_yr_treatments



tillage_percent <- df %>%  group_by(tillage_class) %>% 
  summarise(n = n())
tillage_percent
tillage_percent <- tillage_percent %>% 
  dplyr::mutate(site_yr_treatments = numb_site_yr_treatments[1,1]) %>% 
  dplyr::mutate(percentage_tillage_type = (n /site_yr_treatments)*100) %>% 
  dplyr::mutate(percentage_tillage_type_round = round(percentage_tillage_type,0)) %>% 
  arrange(percentage_tillage_type_round)
tillage_percent


tillage_amen_class_percent <- df %>%  group_by(tillage_amendments_class) %>% 
  summarise(n = n())
tillage_amen_class_percent <- tillage_amen_class_percent %>% 
  dplyr::mutate(site_yr_treatments = numb_site_yr_treatments[1,1]) %>% 
  dplyr::mutate(percentage_tillage_type = (n /site_yr_treatments)*100) %>% 
  dplyr::mutate(percentage_tillage_type_round = round(percentage_tillage_type,0))%>% 
  arrange(percentage_tillage_type_round)
tillage_amen_class_percent
