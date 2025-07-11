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
         decile, 
         yr_post_amelioration,
         amendments_no_amend
         
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

# year post tillage grouping


distinct(df_modified,yr_post_amelioration  )

yr_post_amelioration <- df_modified %>% group_by(yr_post_amelioration) %>% 
  summarise( Count_yr_post_amelioration = n()) 
yr_post_amelioration


sites_yrs_post_tillage <- df_modified %>% group_by(site_display) %>% 
  summarise( number_year_trial = max(yr_post_amelioration)+1) %>% 
  arrange(number_year_trial)
dim(sites_yrs_post_tillage)

## range number of years
sites_yrs_post_tillage <- sites_yrs_post_tillage %>% arrange(number_year_trial) %>%
  dplyr::mutate(Index = seq(1:26))

sites_yrs_post_tillage <- sites_yrs_post_tillage %>%
  mutate(site_display = fct_reorder(site_display, Index)) %>%
  arrange(site_display)  

distinct(sites_yrs_post_tillage,number_year_trial )
  
sites_yrs_post_tillage <- sites_yrs_post_tillage %>%
  mutate(grouping_for_display = case_when(
    number_year_trial  == 2 ~ "new_tillage",
    number_year_trial  == 3 ~ "new_tillage",
        
    number_year_trial  == 4 ~ "old_tillage",
    number_year_trial  == 5 ~ "old_tillage",
    number_year_trial  == 7 ~ "old_tillage",
        .default = "not_classed"
      )) 
    
      
      
      


### plot of sites and number of years of trial
sites_yrs_post_tillage %>% 
  ggplot(aes(y = number_year_trial, x = site_display, fill = grouping_for_display )) +
  geom_col() +
  theme_classic() +
  scale_fill_manual(values=c(  "darkblue","cornflowerblue" ))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",
        legend.title = element_blank())+
  
  xlab("") + 
  ylab("Length of trials (years)") + 
  labs(title = "",
       caption = "Tillage treatment imposed on first year of trial" ) 

################################################################################
## create summary of percentage of trails with years of trials
sites_yrs_post_tillage_summary <- sites_yrs_post_tillage %>%
  group_by(number_year_trial) %>%
  summarise( number_sites_with = n()) %>% 
  mutate(percenatge_sites_with_years = number_sites_with/26)
sites_yrs_post_tillage_summary

### plot of sites and number of years of trial
sites_yrs_post_tillage_summary %>% 
  ggplot(aes(y =  number_sites_with , x =  number_year_trial )) +
  geom_col() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="bottom",
        legend.title = element_blank())+
  
  xlab("Lenght of trial in years") + 
  ylab("Number of sites") + 
  labs(title = "",
       caption = "Tillage treatment imposed on first year of trial" ) 

### Looks like we can do 2 groups; 3 years or less, 4 years or more





# grouping

str(df_modified)
distinct(df_modified,yr_post_amelioration )

#3 years or less (yr_post_amelioration = 0, 1), 4 years or more (yr_post_amelioration =2,3,5,6)
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


unique(df_modified$post_tillage_group)
check <- df_modified %>% select(site_display, year, yr_post_amelioration, post_tillage_group)
check_count<-check %>% group_by(post_tillage_group) %>% summarise(n= n())
check_count








################################################################################


## post tillage
str(df_modified)
distinct(df_modified,post_tillage_group )



df_modified_summary_yld_gain_post_tillage <- df_modified %>% 
  group_by(post_tillage_group) %>% 
  summarise(mean = mean(relative_yld_change, na.rm = TRUE),
            sd = sd(relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n))    
df_modified_summary_yld_gain_post_tillage

df_modified_summary_yld_gain_post_tillage$post_tillage_group <- factor(
  df_modified_summary_yld_gain_post_tillage$post_tillage_group,
  levels = c( "new_tillage", "old_tillage"),
  labels = c(
    "new tillage",
    "old tillage"
  ))

df_modified_summary_yld_gain_post_tillage

df_modified_summary_yld_gain_post_tillage <- df_modified_summary_yld_gain_post_tillage %>% 
  mutate(label2 = paste0( post_tillage_group , "(", n, ")"))
df_modified_summary_yld_gain_post_tillage

df_modified_summary_yld_gain_post_tillage <- df_modified_summary_yld_gain_post_tillage %>% arrange(mean    ) %>% 
  dplyr::mutate(Index = seq(1:2))

df_modified_summary_yld_gain_post_tillage <- df_modified_summary_yld_gain_post_tillage %>%
  mutate(label2 = fct_reorder(label2, Index)) %>%
  arrange(label2)

df_modified_summary_yld_gain_post_tillage


plot2 <-df_modified_summary_yld_gain_post_tillage %>% 
  ggplot(aes(y = label2, x = mean)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  
  xlab("Relative yield change (SE)") + 
  ylab("") + 
  xlim(0, 150)+
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
df_modified_summary_yld_gain_post_tillage



###################################################################################
### what happens when we group the data by tillage class?
# post_tillage_group
# amendments_no_amend

str(df_modified)


df_modified_summary_yld_gain_post_tillage1 <- df_modified %>% 
  group_by(post_tillage_group, amendments_no_amend) %>% 
  summarise(mean = mean(relative_yld_change, na.rm = TRUE),
            sd = sd(relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n))    

df_modified_summary_yld_gain_post_tillage1
df_modified_summary_yld_gain_post_tillage1$post_tillage_group <- factor(
  df_modified_summary_yld_gain_post_tillage1$post_tillage_group,
  levels = c(  "old_tillage", "new_tillage"),
  labels = c(
        "old tillage",
        "new tillage"
  ))

df_modified_summary_yld_gain_post_tillage1$amendments_no_amend <- factor(
  df_modified_summary_yld_gain_post_tillage1$amendments_no_amend,
  levels = c( "amendment", "no_amendment"),
  labels = c(
    "amendment",
    "no amendment"
  ))


df_modified_summary_yld_gain_post_tillage1

df_modified_summary_yld_gain_post_tillage1 <- df_modified_summary_yld_gain_post_tillage1 %>% 
  mutate(label2 = paste0( amendments_no_amend   , "(", n, ")"))
df_modified_summary_yld_gain_post_tillage1

df_modified_summary_yld_gain_post_tillage1 <- ungroup(df_modified_summary_yld_gain_post_tillage1)
df_modified_summary_yld_gain_post_tillage1 <- df_modified_summary_yld_gain_post_tillage1 %>% 
  arrange(post_tillage_group ) %>% 
  dplyr::mutate(Index = seq(1:4))

df_modified_summary_yld_gain_post_tillage1 <- df_modified_summary_yld_gain_post_tillage1 %>%
  mutate(label2 = fct_reorder(label2, Index)) %>%
  arrange(label2)

df_modified_summary_yld_gain_post_tillage1


plot2_new <-df_modified_summary_yld_gain_post_tillage1 %>% 
  #filter(post_tillage_group == "new tillage" ) %>% 
  ggplot(aes(y = label2, x = mean)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  #labs(title = "Years new tillage")+ 
  xlab("Relative yield change (SE)") + 
  ylab("") + 
  xlim(0, 150)+
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))

plot2_new
df_modified_summary_yld_gain_post_tillage1
