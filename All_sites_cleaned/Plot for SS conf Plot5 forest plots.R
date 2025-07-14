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
  mutate(Rating_Phy = as.character(Physical))


df_modified_summary_yld_gain_Phy <- df_modified %>% 
  dplyr::group_by(Rating_Phy) %>% 
  dplyr::summarise(mean = mean(relative_yld_change, na.rm = TRUE),
            sd = sd(relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n)) 

df_modified_summary_yld_gain_Phy$Rating_Phy <- factor(df_modified_summary_yld_gain_Phy$Rating_Phy,
                                                          levels = c("0","1","2"),
                                                          labels = c("No issue", "Moderate issue","Severe issue" ))



df_modified_summary_yld_gain_Phy


################################################################################
## means and Sterror forest plots 

df_modified_summary_yld_gain_Phy

df_modified_summary_yld_gain_Phy <- df_modified_summary_yld_gain_Phy %>% 
  mutate(label2 = paste0(Rating_Phy, "(", n, ")"))
df_modified_summary_yld_gain_Phy
df_modified_summary_yld_gain_Phy <- df_modified_summary_yld_gain_Phy %>% dplyr::arrange() %>% 
  dplyr::mutate(Index = seq(1:3))

df_modified_summary_yld_gain_Phy <- df_modified_summary_yld_gain_Phy %>%
  mutate(label2 = fct_reorder(label2, Index)) %>%
    arrange(label2)



plot2PHY <- df_modified_summary_yld_gain_Phy %>% 
  filter(Rating_Phy != "No issue") %>% 
  ggplot( aes(y = label2, x = mean)) +
  geom_point(shape = 18, size = 6) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  xlab("Relative yield change (SE)") + 
  ylab(" ") + 
  xlim(-10, 70)+
  labs(title = "Physical constraints" )+ 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.text.x.bottom = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 14, colour = "black") ,
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
  mutate(Rating_N = as.character(Nutrient))


df_modified_summary_yld_gain_Nutr <- df_modified %>% 
  dplyr::group_by(Rating_N) %>% 
  dplyr::summarise(mean = mean(relative_yld_change, na.rm = TRUE),
            sd = sd(relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n)) 

df_modified_summary_yld_gain_Nutr$Rating_N <- factor(df_modified_summary_yld_gain_Nutr$Rating_N,
                                                          levels = c("0","1","2"),
                                                          labels = c("No issue", "Moderate issue","Severe issue" ))


df_modified_summary_yld_gain_Nutr

df_modified_summary_yld_gain_Nutr <- df_modified_summary_yld_gain_Nutr %>% 
  dplyr::mutate(label2 = paste0(Rating_N, "(", n, ")"))
df_modified_summary_yld_gain_Nutr
df_modified_summary_yld_gain_Nutr <- df_modified_summary_yld_gain_Nutr %>% dplyr::arrange() %>% 
  dplyr::mutate(Index = seq(1:3))

df_modified_summary_yld_gain_Nutr <- df_modified_summary_yld_gain_Nutr %>%
  mutate(label2 = fct_reorder(label2, Index)) %>%
  arrange(label2)

df_modified_summary_yld_gain_Nutr


plot2 <- df_modified_summary_yld_gain_Nutr %>% 
  filter(Rating_N != "No issue") %>%
  ggplot( aes(y = label2, x = mean    )) +
  geom_point(shape = 18, size = 6) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  
  labs(title = "Nutrition constraints")+ 
  xlab("Realtive yield change (SE)") + 
  ylab("") + 
  xlim(-10, 70)+
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

plot2
df_modified_summary_yld_gain_Nutr
df_modified_summary_yld_gain_Phy
################################################################################
## Repellence
str(df_modified)
distinct(df_modified,Repellence )

df_modified <- df_modified %>% 
  mutate(Rating_R = as.character(Repellence))


df_modified_summary_yld_gain_Repl <- df_modified %>% 
  dplyr::group_by(Rating_R) %>% 
  dplyr::summarise(mean = mean(relative_yld_change, na.rm = TRUE),
                   sd = sd(relative_yld_change, na.rm = TRUE),
                   n = n(),
                   SE = sd/sqrt(n)) 

df_modified_summary_yld_gain_Repl$Rating_R <- factor(df_modified_summary_yld_gain_Repl$Rating_R,
                                                           levels = c("0","1","2"),
                                                           labels = c("No issue", "Moderate issue","Severe issue" ))


df_modified_summary_yld_gain_Repl

df_modified_summary_yld_gain_Repl <- df_modified_summary_yld_gain_Repl %>% 
  dplyr::mutate(label2 = paste0(Rating_R, "(", n, ")"))
df_modified_summary_yld_gain_Repl
df_modified_summary_yld_gain_Repl <- df_modified_summary_yld_gain_Repl %>% dplyr::arrange() %>% 
  dplyr::mutate(Index = seq(1:3))

df_modified_summary_yld_gain_Repl <- df_modified_summary_yld_gain_Repl %>%
  mutate(label2 = fct_reorder(label2, Index)) %>%
  arrange(label2)

df_modified_summary_yld_gain_Repl


plot3 <- df_modified_summary_yld_gain_Repl %>% 
  filter(Rating_R != "No issue") %>%
  ggplot( aes(y = label2, x = mean    )) +
  geom_point(shape = 18, size = 6) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  
  labs(title = "Repellence constraints")+ 
  xlab("Realtive yield change (SE)") + 
  ylab("") + 
  xlim(-10, 70)+
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
df_modified_summary_yld_gain_Nutr <- df_modified_summary_yld_gain_Nutr %>% mutate(constraint ="Nutrient" ) %>% 
  rename("Rating" = "Rating_N")
df_modified_summary_yld_gain_Phy <- df_modified_summary_yld_gain_Phy %>% mutate(constraint ="Physical" ) %>% 
  rename("Rating" = "Rating_Phy")
df_modified_summary_yld_gain_Repl <- df_modified_summary_yld_gain_Repl %>% mutate(constraint ="Repellence" ) %>% 
  rename("Rating" = "Rating_R")


df_modified_summary_yld_gain_Nutr_Ph_Rep <- rbind(df_modified_summary_yld_gain_Nutr, 
                                                  df_modified_summary_yld_gain_Phy,
                                                  df_modified_summary_yld_gain_Repl)

df_modified_summary_yld_gain_Nutr_Ph_Rep


df_modified_summary_yld_gain_Nutr_Ph_Rep <- df_modified_summary_yld_gain_Nutr_Ph_Rep %>% 
  dplyr::mutate(label3 = paste0(constraint, " with " ,Rating, "(", n, ")"))

###############################################################################
plot4 <- df_modified_summary_yld_gain_Nutr_Ph_Rep %>% 
  filter(Rating != "No issue") %>%
  ggplot( aes(y = label3, x = mean    )) +
  geom_point(shape = 18, size = 6) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  
  labs(title = "")+ 
  xlab("Realtive yield change (SE)") + 
  ylab("") + 
  xlim(-10, 70)+
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

plot4


df_modified_summary_yld_gain_Nutr_Ph_Rep %>% 
  filter(Rating != "No issue")


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
## Repllency option 1000
#
str(df_modified)
distinct(df_modified,Repellence)

df_modified_summary_yld_gain_Rep_ser <- df_modified %>% 
  filter(Repellence == 2) %>% 
  dplyr::group_by(tillage_class) %>% 
  dplyr::summarise(mean = mean(relative_yld_change, na.rm = TRUE),
                   sd = sd(relative_yld_change, na.rm = TRUE),
                   n = n(),
                   SE = sd/sqrt(n)) 
df_modified_summary_yld_gain_Rep_ser

df_modified_summary_yld_gain_Rep_ser <- df_modified_summary_yld_gain_Rep_ser %>% 
  dplyr::mutate(label2 = paste0(tillage_class, "(", n, ")"))
df_modified_summary_yld_gain_Rep_ser
df_modified_summary_yld_gain_Rep_ser <- df_modified_summary_yld_gain_Rep_ser %>% dplyr::arrange(mean) %>% 
  dplyr::mutate(Index = seq(1:2))

df_modified_summary_yld_gain_Rep_ser <- df_modified_summary_yld_gain_Rep_ser %>%
  dplyr::mutate(label2 = fct_reorder(label2, Index)) %>%
  dplyr::arrange(label2)

df_modified_summary_yld_gain_Rep_ser

################################################################################
## Repllency option 1000
#
str(df_modified)
distinct(df_modified,Repellence)

df_modified_summary_yld_gain_Rep_mod <- df_modified %>% 
  filter(Repellence == 1) %>%
  dplyr::group_by(tillage_class) %>% 
  dplyr::summarise(mean = mean(relative_yld_change, na.rm = TRUE),
                   sd = sd(relative_yld_change, na.rm = TRUE),
                   n = n(),
                   SE = sd/sqrt(n)) 
df_modified_summary_yld_gain_Rep_mod

df_modified_summary_yld_gain_Rep_mod <- df_modified_summary_yld_gain_Rep_mod %>% 
  dplyr::mutate(label2 = paste0(tillage_class, "(", n, ")"))
df_modified_summary_yld_gain_Rep_mod
df_modified_summary_yld_gain_Rep_mod <- df_modified_summary_yld_gain_Rep_mod %>% dplyr::arrange(mean) %>% 
  dplyr::mutate(Index = seq(1:4))

df_modified_summary_yld_gain_Rep_mod <- df_modified_summary_yld_gain_Rep_mod %>%
  dplyr::mutate(label2 = fct_reorder(label2, Index)) %>%
  dplyr::arrange(label2)

df_modified_summary_yld_gain_Rep_mod


################################################################################
#Put all of these together

df_modified_summary_yld_gain_Phy_ser <- df_modified_summary_yld_gain_Phy_ser %>% 
  dplyr::mutate(grouping = "Phy_ser")
df_modified_summary_yld_gain_Nutr_Ser<- df_modified_summary_yld_gain_Nutr_Ser %>% 
  dplyr::mutate(grouping = "Nutr_Ser")
df_modified_summary_yld_gain_Rep_ser<- df_modified_summary_yld_gain_Rep_ser %>% 
  dplyr::mutate(grouping = "Rep_Ser")

df_modified_summary_yld_gain_Phy_mod<- df_modified_summary_yld_gain_Phy_mod %>% 
  dplyr::mutate(grouping = "Phy_mod")
df_modified_summary_yld_gain_Nutr_mod<- df_modified_summary_yld_gain_Nutr_mod %>% 
  dplyr::mutate(grouping = "Nutr_mod")
df_modified_summary_yld_gain_Rep_mod<- df_modified_summary_yld_gain_Rep_mod %>% 
  dplyr::mutate(grouping = "Rep_mod")


df_modified_summary_yld_gain_Nutr_Phyical <- rbind(
  df_modified_summary_yld_gain_Phy_ser,
  df_modified_summary_yld_gain_Nutr_Ser,
  df_modified_summary_yld_gain_Rep_ser,
  df_modified_summary_yld_gain_Phy_mod,
  df_modified_summary_yld_gain_Nutr_mod,
  df_modified_summary_yld_gain_Rep_mod
)
## Add some more helper clm for plotting
df_modified_summary_yld_gain_Nutr_Phyical <- df_modified_summary_yld_gain_Nutr_Phyical %>% 
  dplyr::mutate(helper_tillage = case_when(
    grouping == "Phy_ser" ~ "Physical",
    grouping == "Phy_mod" ~ "Physical",
    grouping == "Rep_Ser" ~ "Repllence",
    grouping == "Nutr_Ser" ~ "Nutrient",
    grouping == "Nutr_mod" ~ "Nutrient",
    grouping == "Rep_mod" ~ "Repllence",
    
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
  filter(grouping == "Phy_ser" | grouping == "Nutr_Ser"| grouping == "Rep_Ser") %>% 
  
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
        axis.title.x = element_text(size = 14, colour = "black"),
        title = element_text(size = 14, colour = "black")) 


plot2
df_modified_summary_yld_gain_Nutr_Phyical %>% 
  filter(grouping == "Phy_ser" | grouping == "Nutr_Ser"| grouping == "Rep_Ser")



plot3 <- df_modified_summary_yld_gain_Nutr_Phyical %>% 
  filter(grouping == "Phy_mod" | grouping == "Nutr_mod" | grouping == "Rep_mod") %>% 
  
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
  filter(grouping == "Phy_mod" | grouping == "Nutr_mod" | grouping == "Rep_mod")
