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


# keep only one yield output
str(df)
df_modified <- df %>% 
  dplyr::select(tillage_amendments_class,
         site_display,
         year,
         yield_gain,
         yield,
         relative_yld_change,
         control_yield,
         tillage_amendments_class,
         tillage_class,
         Physical,
         Nutrient,
         Acidity,
         Repellence,
         multiple_constraints,
         amendments_no_amend
  ) %>% 
  dplyr::mutate(site_year = paste0(site_display,"_", year)) %>% 
  filter(tillage_amendments_class != "Unmodified_amendment")

str(df_modified)



#
################################################################################
### Option 2 means ----
#### using Yield gains mean of the treatments ---

## Physical
str(df_modified)
distinct(df_modified,Physical )

df_modified <- df_modified %>% 
  mutate(Physical_name1 = as.character(Physical))


df_modified_summary_yld_gain_Phy <- df_modified %>% 
  dplyr::group_by(Physical_name1) %>% 
  dplyr::summarise(mean = mean(relative_yld_change, na.rm = TRUE),
            sd = sd(relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n)) 

df_modified_summary_yld_gain_Phy$Physical_name1 <- factor(df_modified_summary_yld_gain_Phy$Physical_name1,
                                                          levels = c("0","1","2"),
                                                          labels = c("No issue", "Moderate issue","Severe issue" ))



df_modified_summary_yld_gain_Phy


################################################################################
## means and Sterror forest plots 

df_modified_summary_yld_gain_Phy

df_modified_summary_yld_gain_Phy <- df_modified_summary_yld_gain_Phy %>% 
  mutate(label2 = paste0(Physical_name1, "(", n, ")"))
df_modified_summary_yld_gain_Phy
df_modified_summary_yld_gain_Phy <- df_modified_summary_yld_gain_Phy %>% dplyr::arrange() %>% 
  dplyr::mutate(Index = seq(1:3))

df_modified_summary_yld_gain_Phy <- df_modified_summary_yld_gain_Phy %>%
  mutate(label2 = fct_reorder(label2, Index)) %>%
    arrange(label2)



plot2PHY <- df_modified_summary_yld_gain_Phy %>% 
  filter(Physical_name1 != "No issue") %>% 
  ggplot( aes(y = label2, x = mean)) +
  geom_point(shape = 18, size = 6) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  xlab("Relative yield change (SE)") + 
  ylab(" ") + 
  xlim(-10, 50)+
  labs(title = "Physical constraints" )+ 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black") ,
        title = element_text(size = 14, colour = "black")) 
#axis.title.x=element_blank(),
#axis.text.x=element_blank(),
#axis.ticks.x=element_blank())
plot2PHY

################################################################################
## Nutrition
str(df_modified)
distinct(df_modified,Nutrient )

df_modified <- df_modified %>% 
  mutate(Nutrient_name1 = as.character(Nutrient))


df_modified_summary_yld_gain_Nutr <- df_modified %>% 
  dplyr::group_by(Nutrient_name1) %>% 
  dplyr::summarise(mean = mean(relative_yld_change, na.rm = TRUE),
            sd = sd(relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n)) 

df_modified_summary_yld_gain_Nutr$Nutrient_name1 <- factor(df_modified_summary_yld_gain_Nutr$Nutrient_name1,
                                                          levels = c("0","1","2"),
                                                          labels = c("No issue", "Moderate issue","Severe issue" ))


df_modified_summary_yld_gain_Nutr

df_modified_summary_yld_gain_Nutr <- df_modified_summary_yld_gain_Nutr %>% 
  dplyr::mutate(label2 = paste0(Nutrient_name1, "(", n, ")"))
df_modified_summary_yld_gain_Nutr
df_modified_summary_yld_gain_Nutr <- df_modified_summary_yld_gain_Nutr %>% dplyr::arrange() %>% 
  dplyr::mutate(Index = seq(1:3))

df_modified_summary_yld_gain_Nutr <- df_modified_summary_yld_gain_Nutr %>%
  mutate(label2 = fct_reorder(label2, Index)) %>%
  arrange(label2)

df_modified_summary_yld_gain_Nutr


plot2 <- df_modified_summary_yld_gain_Nutr %>% 
  filter(Nutrient_name1 != "No issue") %>%
  ggplot( aes(y = label2, x = mean    )) +
  geom_point(shape = 18, size = 6) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  
  labs(title = "Nutrition constraints")+ 
  xlab("Realtive yield change (SE)") + 
  ylab("") + 
  xlim(-10, 50)+
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"),
        title = element_text(size = 14, colour = "black")) 

plot2
df_modified_summary_yld_gain_Nutr
df_modified_summary_yld_gain_Phy
################################################################################
## nutrients option 1000
str(df_modified)
distinct(df_modified,Nutrient)

df_modified_summary_yld_gain_Nutr_mod <- df_modified %>% 
  #filter(Nutrient == 2) %>% 
  filter(Nutrient == 1) %>%
  dplyr::group_by(tillage_class) %>% 
  dplyr::summarise(mean = mean(relative_yld_change, na.rm = TRUE),
            sd = sd(relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n)) 
df_modified_summary_yld_gain_Nutr_mod

df_modified_summary_yld_gain_Nutr_mod <- df_modified_summary_yld_gain_Nutr_mod %>% 
  mutate(label2 = paste0(tillage_class, "(", n, ")"))
df_modified_summary_yld_gain_Nutr_mod
df_modified_summary_yld_gain_Nutr_mod <- df_modified_summary_yld_gain_Nutr_mod %>% dplyr::arrange(mean) %>% 
  dplyr::mutate(Index = seq(1:3))

df_modified_summary_yld_gain_Nutr_mod <- df_modified_summary_yld_gain_Nutr_mod %>%
  mutate(label2 = fct_reorder(label2, Index)) %>%
  arrange(label2)

df_modified_summary_yld_gain_Nutr_mod

################################################################################

str(df_modified)
distinct(df_modified,Nutrient)

df_modified_summary_yld_gain_Nutr_Ser <- df_modified %>% 
  filter(Nutrient == 2) %>% 
  #filter(Nutrient == 1) %>%
  dplyr::group_by(tillage_class) %>% 
  dplyr::summarise(mean = mean(relative_yld_change, na.rm = TRUE),
                   sd = sd(relative_yld_change, na.rm = TRUE),
                   n = n(),
                   SE = sd/sqrt(n)) 
df_modified_summary_yld_gain_Nutr_Ser

df_modified_summary_yld_gain_Nutr_Ser <- df_modified_summary_yld_gain_Nutr_Ser %>% 
  mutate(label2 = paste0(tillage_class, "(", n, ")"))
df_modified_summary_yld_gain_Nutr_Ser
df_modified_summary_yld_gain_Nutr_Ser <- df_modified_summary_yld_gain_Nutr_Ser %>% dplyr::arrange(mean) %>% 
  dplyr::mutate(Index = seq(1:3))

df_modified_summary_yld_gain_Nutr_Ser <- df_modified_summary_yld_gain_Nutr_Ser %>%
  mutate(label2 = fct_reorder(label2, Index)) %>%
  arrange(label2)

df_modified_summary_yld_gain_Nutr_Ser
################################################################################



################################################################################
## Physical option 1000
#
str(df_modified)
distinct(df_modified,Physical)

df_modified_summary_yld_gain_Phy_mod <- df_modified %>% 
  #filter(Physical == 2) %>% 
  filter(Physical == 1) %>%
  dplyr::group_by(tillage_class) %>% 
  dplyr::summarise(mean = mean(relative_yld_change, na.rm = TRUE),
            sd = sd(relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n)) 
df_modified_summary_yld_gain_Phy_mod

df_modified_summary_yld_gain_Phy_mod <- df_modified_summary_yld_gain_Phy_mod %>% 
  dplyr::mutate(label2 = paste0(tillage_class, "(", n, ")"))
df_modified_summary_yld_gain_Phy_mod
df_modified_summary_yld_gain_Phy_mod <- df_modified_summary_yld_gain_Phy_mod %>% dplyr::arrange(mean) %>% 
  dplyr::mutate(Index = seq(1:4))

df_modified_summary_yld_gain_Phy_mod <- df_modified_summary_yld_gain_Phy_mod %>%
  dplyr::mutate(label2 = fct_reorder(label2, Index)) %>%
  dplyr::arrange(label2)

df_modified_summary_yld_gain_Phy_mod

################################################################################
## Physical option 1000
#
str(df_modified)
distinct(df_modified,Physical)

df_modified_summary_yld_gain_Phy_ser <- df_modified %>% 
  filter(Physical == 2) %>% 
  #filter(Physical == 1) %>%
  dplyr::group_by(tillage_class) %>% 
  dplyr::summarise(mean = mean(relative_yld_change, na.rm = TRUE),
                   sd = sd(relative_yld_change, na.rm = TRUE),
                   n = n(),
                   SE = sd/sqrt(n)) 
df_modified_summary_yld_gain_Phy_ser

df_modified_summary_yld_gain_Phy_ser <- df_modified_summary_yld_gain_Phy_ser %>% 
  dplyr::mutate(label2 = paste0(tillage_class, "(", n, ")"))
df_modified_summary_yld_gain_Phy_ser
df_modified_summary_yld_gain_Phy_ser <- df_modified_summary_yld_gain_Phy_ser %>% dplyr::arrange(mean) %>% 
  dplyr::mutate(Index = seq(1:3))

df_modified_summary_yld_gain_Phy_ser <- df_modified_summary_yld_gain_Phy_ser %>%
  dplyr::mutate(label2 = fct_reorder(label2, Index)) %>%
  dplyr::arrange(label2)

df_modified_summary_yld_gain_Phy_ser

################################################################################
#Put all of these together

df_modified_summary_yld_gain_Phy_ser <- df_modified_summary_yld_gain_Phy_ser %>% 
  dplyr::mutate(grouping = "Phy_ser")
df_modified_summary_yld_gain_Nutr_Ser<- df_modified_summary_yld_gain_Nutr_Ser %>% 
  dplyr::mutate(grouping = "Nutr_Ser")

df_modified_summary_yld_gain_Phy_mod<- df_modified_summary_yld_gain_Phy_mod %>% 
  dplyr::mutate(grouping = "Phy_mod")
df_modified_summary_yld_gain_Nutr_mod<- df_modified_summary_yld_gain_Nutr_mod %>% 
  dplyr::mutate(grouping = "Nutr_mod")

df_modified_summary_yld_gain_Nutr_Phyical <- rbind(
  df_modified_summary_yld_gain_Phy_ser,
  df_modified_summary_yld_gain_Nutr_Ser,
  df_modified_summary_yld_gain_Phy_mod,
  df_modified_summary_yld_gain_Nutr_mod
)
## Add some more helper clm for plotting
df_modified_summary_yld_gain_Nutr_Phyical <- df_modified_summary_yld_gain_Nutr_Phyical %>% 
  dplyr::mutate(helper_tillage = case_when(
    grouping == "Phy_ser" ~ "Physical",
    grouping == "Phy_mod" ~ "Physical",
    grouping == "Nutr_Ser" ~ "Nutrient",
    grouping == "Nutr_mod" ~ "Nutrient",
    .default = "check"
  ))
df_modified_summary_yld_gain_Nutr_Phyical <- df_modified_summary_yld_gain_Nutr_Phyical %>% 
  dplyr::mutate(label3 = paste0(label2, "-", helper_tillage) )


## Arrange data for plotting
df_modified_summary_yld_gain_Nutr_Phyical <- df_modified_summary_yld_gain_Nutr_Phyical %>%
  dplyr::mutate(label3 = fct_reorder(label3, helper_tillage)) %>%
  dplyr::arrange(label3)
df_modified_summary_yld_gain_Nutr_Phyical

plot2 <- df_modified_summary_yld_gain_Nutr_Phyical %>% 
  filter(grouping == "Phy_ser" | grouping == "Nutr_Ser") %>% 
  
  ggplot( aes(y = label3, x = mean  )) +
  geom_point(shape = 18, size = 6) +  
  geom_errorbarh(aes(xmin = mean-SE , xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  
  labs(title = "Sites with constraints rated as severe")+
  #labs(title = "Sites with phyical constraints rated as moderate issue")+
  xlab("Relative yield change (SE)") + 
  ylab("") + 
  xlim(-10, 75)+
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.text.x.bottom = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"),
        title = element_text(size = 14, colour = "black")) 


plot2
df_modified_summary_yld_gain_Nutr_Phyical %>% 
  filter(grouping == "Phy_ser" | grouping == "Nutr_Ser")



plot3 <- df_modified_summary_yld_gain_Nutr_Phyical %>% 
  filter(grouping == "Phy_mod" | grouping == "Nutr_mod") %>% 
  
  ggplot( aes(y = label3, x = mean  )) +
  geom_point(shape = 18, size = 6) +  
  geom_errorbarh(aes(xmin = mean-SE , xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  
  labs(title = "Sites with constraints rated as moderate")+
  
  xlab("Relative yield change (SE)") + 
  ylab("") + 
  xlim(-10, 75)+
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.text.x.bottom = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 14, colour = "black"),
        title = element_text(size = 14, colour = "black")) 


plot3
df_modified_summary_yld_gain_Nutr_Phyical %>% 
  filter(grouping == "Phy_mod" | grouping == "Nutr_mod")
