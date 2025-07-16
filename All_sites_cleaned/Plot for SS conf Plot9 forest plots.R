library(ggplot2)
library(readxl)
library(tidyverse)
library(stringr)

library(dmetar)
library(tidyverse)
library(meta)

library(forcats)

## plot and analysis for preso




df <- read.csv("N:/sandy soils conference/data/All_sites_cleaned/control_metadata_contraints_tillage_only_cleaned.csv" )

names(df)
unique(df$crop_group)
unique(df$tillage_class)
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
         Physical,
         Nutrient,
         Acidity,
         Repellence,
         rain,
         crop,
         decile, 
         yr_post_amelioration,
         amendments_no_amend,
         post_tillage_group
         
  ) %>% 
  dplyr::mutate(site_year = paste0(site_display,"_", year)) %>% 
  filter(tillage_amendments_class != "Unmodified_amendment")



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

df_modified_summary_yld_gain_post_tillage <- df_modified_summary_yld_gain_post_tillage %>% 
  mutate(amendments_no_amend = "Both amendment classes")

df_modified_summary_yld_gain_post_tillage

# plot2 <-df_modified_summary_yld_gain_post_tillage %>% 
#   ggplot(aes(y = label2, x = mean)) +
#   geom_point(shape = 18, size = 6) +  
#   geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
#   geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
#   
#   xlab("Relative yield change (SE)") + 
#   ylab("") + 
#   xlim(-10, 50)+
#   theme_bw() +
#   theme(panel.border = element_blank(),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         axis.line = element_line(colour = "black"),
#         axis.text.y = element_text(size = 14, colour = "black"),
#         axis.text.x.bottom = element_text(size = 14, colour = "black"),
#         axis.title.x = element_text(size = 14, colour = "black"),
#         title = element_text(size = 14, colour = "black")) 
# 
# plot2
# df_modified_summary_yld_gain_post_tillage



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
  dplyr::mutate(Index = c(3,4,5,6))

df_modified_summary_yld_gain_post_tillage1 <- df_modified_summary_yld_gain_post_tillage1 %>%
  mutate(label2 = fct_reorder(label2, Index)) %>%
  arrange(label2)

df_modified_summary_yld_gain_post_tillage1


#################################################################################
# Add 2 df together


df_modified_summary_yld_gain_post_tillage1
df_modified_summary_yld_gain_post_tillage

df_modified_summary_yld_gain_post_tillage_join <- 
  rbind(df_modified_summary_yld_gain_post_tillage,df_modified_summary_yld_gain_post_tillage1)

## helper for plotting

df_modified_summary_yld_gain_post_tillage_join <- df_modified_summary_yld_gain_post_tillage_join %>%
  mutate(label3 = case_when(
    post_tillage_group == "old tillage" & amendments_no_amend == "Both amendment classes" ~ "old tillage",
    post_tillage_group == "new tillage" & amendments_no_amend == "Both amendment classes" ~ "new tillage",
    post_tillage_group == "old tillage" & amendments_no_amend == "amendment" ~ "old tillage with amendments",
    post_tillage_group == "new tillage" & amendments_no_amend == "amendment" ~ "new tillage with amendments",
    post_tillage_group == "old tillage" & amendments_no_amend == "no amendment" ~ "old tillage with no amendments",
    post_tillage_group == "new tillage" & amendments_no_amend == "no amendment" ~ "new tillage with no amendments",
  ))

df_modified_summary_yld_gain_post_tillage_join <- df_modified_summary_yld_gain_post_tillage_join %>%
  mutate(label3 = paste0( label3   , "(", n, ")"))

df_modified_summary_yld_gain_post_tillage_join <- df_modified_summary_yld_gain_post_tillage_join %>%
  mutate(label3 = fct_reorder(label3, Index, .desc = TRUE)) %>%
  arrange(label3)






plot2_new <-df_modified_summary_yld_gain_post_tillage_join %>% 
  #filter(post_tillage_group == "new tillage" ) %>% 
  ggplot(aes(y = label3, x = mean)) +
  geom_point(shape = 18, size = 6) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  #labs(title = "Years new tillage")+ 
  xlab("Relative yield change (SE)") + 
  ylab("") + 
  xlim(0, 50)+
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.text.x.bottom = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 14, colour = "black"))

plot2_new
df_modified_summary_yld_gain_post_tillage_join



################################################################################
### try and create a response curve

## sites with repellent rate 1 or 2
str(df)
df_modified_1 <- df %>% 
  select(tillage_amendments_class,
         site_display,
         year,
         #yield_gain,
         relative_yld_change,
         #
         tillage_amendments_class,
         tillage_class,
         Physical,
         Nutrient,
         Acidity,
         Repellence,
         # rain,
         # crop,
         # decile, 
         yr_post_amelioration,
         amendments_no_amend,
         post_tillage_group
         
  ) %>% 
  dplyr::mutate(site_year = paste0(site_display,"_", year)) %>% 
  filter(tillage_amendments_class != "Unmodified_amendment")
str(df_modified_1)

test <- df_modified_1 %>% 
  filter(Repellence == 1 |Repellence == 2 ) %>% 
  filter(tillage_class == "Mixing") 

###############################################################################
### Summaries yld resposne for sites with Repllence
Repllence_all_tillage <- df_modified_1 %>% 
filter(Repellence == 1 | Repellence == 2) %>%
  group_by(yr_post_amelioration, site_display, tillage_class) %>% 
  summarise(Mean_relative_yld_change = mean( relative_yld_change))
Repllence_all_tillage


Repllence_all_tillage %>%
  filter(site_display != "Brooker") %>% 
    ggplot(
    aes(
      x = yr_post_amelioration,
      Mean_relative_yld_change,
      group = tillage_class 
      )
  ) +
  geom_line(aes(color=tillage_class), linewidth = 1)+
  facet_wrap(.~ site_display)+
  labs(title = "Site with Repellence rated moderative or sereve. All tillage types")+
  xlab("Years after tillage") + ylab("Mean relative yield change")+
  theme_bw()+
  theme(legend.title=element_blank())

################################################################################
### Summaries yld resposne for sites with physical
str(df_modified_1)
Phyical_all_tillage <- df_modified_1 %>% 
  filter(Physical == 1 | Physical == 2) %>%
  group_by(yr_post_amelioration, site_display, tillage_class) %>% 
  summarise(Mean_relative_yld_change = mean( relative_yld_change))
Phyical_all_tillage


Phyical_all_tillage %>%
  filter(site_display != "Brooker" & 
           site_display != "Kooloonong" &
           site_display != "Telopea Downs") %>% 
  ggplot(
    aes(
      x = yr_post_amelioration,
      Mean_relative_yld_change,
      group = tillage_class 
    )
  ) +
  geom_line(aes(color=tillage_class), linewidth = 1)+
  facet_wrap(.~ site_display)+
  labs(title = "Site with Physical rated moderative or sereve. All tillage types")+
  xlab("Years after tillage") + ylab("Mean relative yield change")+
  theme_bw()+
  theme(legend.title=element_blank())

################################################################################
### Summaries yld resposne for sites with Nutrient
str(df_modified_1)
Nutrient_all_tillage <- df_modified_1 %>% 
  filter(Nutrient == 1 | Nutrient == 2) %>%
  group_by(yr_post_amelioration, site_display, tillage_class) %>% 
  summarise(Mean_relative_yld_change = mean( relative_yld_change))
Nutrient_all_tillage


Nutrient_all_tillage %>%
  filter(site_display != "Brooker") %>% 
  ggplot(
    aes(
      x = yr_post_amelioration,
      Mean_relative_yld_change,
      group = tillage_class 
    )
  ) +
  geom_line(aes(color=tillage_class), linewidth = 1)+
  facet_wrap(.~ site_display)+
  labs(title = "Site with Nutrient rated moderative or sereve. All tillage types")+
  xlab("Years after tillage") + ylab("Mean relative yield change")+
  theme_bw()+
  theme(legend.title=element_blank())


################################################################################
### Summaries yld resposne for all sites 
str(df_modified_1)
all_tillage <- df_modified_1 %>% 
  group_by(yr_post_amelioration, site_display, tillage_amendments_class) %>% 
  #group_by(yr_post_amelioration, site_display, tillage_class) %>% 
  summarise(Mean_relative_yld_change = mean( relative_yld_change))
all_tillage
unique(all_tillage$tillage_amendments_class)


## helper with plotting
all_tillage$tillage_amendments_class <- factor(all_tillage$tillage_amendments_class, 
                                            levels = c(
                                              "Mixing_amendment",
                                               "Mixing_no_amendment",
                                              "Ripping_amendment",
                                              "Ripping_no_amendment",
                                              "Combination_amendment" ,
                                              "Combination_no_amendment"
                                            ),
                                            labels = c(
                                              "Mixing with amendment",
                                               "Mixing only",
                                              "Ripping with amendment",
                                              "Ripping only", 
                                              "Combination with amendment",
                                              "Combination only"
                                              
                                            ))

### helper with plotting
unique(all_tillage$yr_post_amelioration)
all_tillage <- all_tillage %>% 
  mutate(Years_trial = case_when(
    yr_post_amelioration ==0 ~ 1,
    yr_post_amelioration ==1 ~ 2,
    yr_post_amelioration ==2 ~ 3,
    yr_post_amelioration ==3 ~ 4,
    yr_post_amelioration ==4 ~ 5,
    yr_post_amelioration ==5 ~ 6,
    yr_post_amelioration ==6 ~ 7
  ))




### Plot
all_tillage %>%
  filter(site_display != "Brooker" & 
           site_display != "Kooloonong" &
           site_display != "Telopea Downs") %>% 
  ggplot(
    aes(
      x = Years_trial,
      Mean_relative_yld_change,
      group = tillage_amendments_class
      #group = tillage_class 
    )
  ) +
  geom_line(aes(color=tillage_amendments_class), linewidth = 1)+
  scale_color_manual(values = c("#CC9966", "#996633", "#99CC99", "#669966", "#99CCFF", "#4169E1"))+
   facet_wrap(.~ site_display)+
  #labs(title = "All site and all tillage types")+
  xlab("Years after tillage") + ylab("Mean relative yield change")+
  theme_bw()+
  theme(legend.title=element_blank())

# Create ordering vector
site_order <- all_tillage %>%
  filter(site_display != "Brooker" & 
           site_display != "Kooloonong" &
           site_display != "Telopea Downs") %>%
  group_by(site_display) %>%
  summarise(max_years = max(Years_trial )) %>%
  arrange(max_years) %>%
  pull(site_display)

# Apply to main plot
all_tillage %>%
  filter(site_display != "Brooker" & 
           site_display != "Kooloonong" &
           site_display != "Telopea Downs") %>%
  mutate(site_display = factor(site_display, levels = site_order)) %>%
  ggplot(
    aes(
      x = Years_trial,
      y = Mean_relative_yld_change,
      group = tillage_amendments_class
    )
  ) +
  geom_line(aes(color = tillage_amendments_class), linewidth = 1) +
  scale_color_manual(values = c("#CC9966", "#996633", "#99CC99", "#669966", "#99CCFF", "#4169E1")) +
  facet_wrap(.~ site_display) +
  xlab("Years after tillage") + 
  ylab("Mean relative yield change") +
  theme_bw() +
  theme(legend.title = element_blank())



################################################################################
#### site specific example ##################################################

# Apply to main plot
all_tillage %>%
  filter(site_display == "Bute" ) %>%
  mutate(site_display = factor(site_display, levels = site_order)) %>%
  ggplot(
    aes(
      x = Years_trial,
      y = Mean_relative_yld_change,
      group = tillage_amendments_class
    )
  ) +
  geom_line(aes(color = tillage_amendments_class), linewidth = 1) +
  ylim(0, 150)+
  scale_color_manual(values = c( "#99CC99", "#669966")) +
  facet_wrap(.~ site_display) +
  xlab("Years after tillage") + 
  ylab("Mean relative yield change") +
  theme_bw() +
  theme(legend.title = element_blank())



str(df)
df_modified_2 <- df %>% 
  select(tillage_amendments_class,
         site_display,
         year,
         #yield_gain,
         relative_yld_change,
         #
         tillage_amendments_class,
         tillage_class,
         Physical,
         Nutrient,
         Acidity,
         Repellence,
         # rain,
         # crop,
         # decile, 
         yr_post_amelioration,
         amendments_no_amend,
         post_tillage_group,
         multiple_constraints,
         Decile_group,
         Descriptors,
         soil_amendments,
         amendments_grouping
         
  ) %>% 
  dplyr::mutate(site_year = paste0(site_display,"_", year)) %>% 
  filter(tillage_amendments_class != "Unmodified_amendment")
str(df_modified_2)
Bute <- df_modified_2 %>% 
  filter(site_display == "Bute" )

str(Bute)
### helper with plotting
unique(Bute$yr_post_amelioration)
Bute <- Bute %>% 
  mutate(Years_trial = case_when(
    yr_post_amelioration ==0 ~ 1,
    yr_post_amelioration ==1 ~ 2,
    yr_post_amelioration ==2 ~ 3,
    yr_post_amelioration ==3 ~ 4,
    yr_post_amelioration ==4 ~ 5,
    yr_post_amelioration ==5 ~ 6,
    yr_post_amelioration ==6 ~ 7
  ))

str(Bute)



write_csv(Bute,"N:/sandy soils conference/data/All_sites_cleaned/Bute.csv" )
################################################################################
### Just Bute with no 'mixed amendments"

unique(Bute$Descriptors)

summaries_bute_no_mixed <- Bute %>% 
  filter(Descriptors == "Rip.50_none"  |
           Descriptors == "Rip.50_Fert.surface" |
           Descriptors == "Rip.50_Cl@20.incorp_20"|
           Descriptors == "Rip.50_Cl@5.incorp_20"  |
           Descriptors == "Rip.50_Clay.incorp_20"
           ) %>%
  mutate(Rate = case_when(
    Descriptors == "Rip.50_Cl@20.incorp_20" ~ "20",
    Descriptors == "Rip.50_Cl@5.incorp_20" ~ "5",
    Descriptors == "Rip.50_none" ~ "0",
    Descriptors == "Rip.50_Fert.surface" ~ "various",
    Descriptors == "Rip.50_Clay.incorp_20" ~ "100",
  )) %>% 
  group_by(Years_trial, tillage_amendments_class, amendments_grouping, Rate) %>% 
  summarise(Mean_relative_yld_change = mean( relative_yld_change))
summaries_bute_no_mixed <- summaries_bute_no_mixed %>% 
  mutate(amendments_grouping_rate = paste0(amendments_grouping, ". rate = ", Rate ))
summaries_bute_no_mixed

#plot
summaries_bute_no_mixed %>%
  ggplot(aes(
      x = Years_trial,
      y = Mean_relative_yld_change     ,
      group = tillage_amendments_class
    )
  ) +
  #geom_boxplot()+
  geom_point()+
  geom_line(aes(color = tillage_amendments_class), linewidth = 1) +
  facet_wrap(.~ amendments_grouping_rate      ) +
  xlab("Years after tillage") + 
  ylab("Mean relative yield change") +
  theme_bw() +
  theme(legend.position = "none")


### Just Bute with  'mixed amendments"

unique(Bute$Descriptors)

summaries_bute_mixed <- Bute %>% 
  filter(Descriptors != "Rip.50_none"  &
           Descriptors != "Rip.50_Fert.surface" &
           Descriptors != "Rip.50_Cl@20.incorp_20"&
           Descriptors != "Rip.50_Cl@5.incorp_20"  &
           Descriptors != "Rip.50_Clay.incorp_20"
  ) %>%
  # mutate(Rate = case_when(
  #   Descriptors == "Rip.50_Cl@20.incorp_20" ~ "20",
  #   Descriptors == "Rip.50_Cl@5.incorp_20" ~ "5",
  #   Descriptors == "Rip.50_none" ~ "0",
  #   Descriptors == "Rip.50_Fert.surface" ~ "various",
  #   Descriptors == "Rip.50_Clay.incorp_20" ~ "100",
  # )) %>% 
  group_by(Years_trial, tillage_amendments_class, amendments_grouping) %>% 
  summarise(Mean_relative_yld_change = mean( relative_yld_change))
# summaries_bute_mixed <- summaries_bute_no_mixed %>% 
#   mutate(amendments_grouping_rate = paste0(amendments_grouping, ". rate = ", Rate ))
summaries_bute_mixed

#plot
summaries_bute_mixed %>%
  ggplot(aes(
    x = Years_trial,
    y = Mean_relative_yld_change     ,
    group = tillage_amendments_class
  )
  ) +
  #geom_boxplot()+
  geom_point()+
  geom_line( linewidth = 1) +
  #facet_wrap(.~ amendments_grouping_rate      ) +
  xlab("Years after tillage") + 
  ylab("Mean relative yield change") +
  theme_bw() +
  theme(legend.position = "none")

