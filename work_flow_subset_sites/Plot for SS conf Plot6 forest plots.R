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
         Repellence
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
# Cal multiple constraint

str(df_modified)

## recode ranked as moderate or severe

df_modified <- df_modified %>% 
  mutate(Physical_M_S = case_when(
    Physical == 1 ~ 1,
    Physical == 2 ~ 1,
    .default = 0
  )) %>% 
  
  mutate(Nutrient_M_S = case_when(
    Nutrient == 1 ~ 1,
    Nutrient == 2 ~ 1,
    .default = 0
  )) %>% 

  mutate(Acidity_M_S = case_when(
    Acidity == 1 ~ 1,
    Acidity == 2 ~ 1,
    .default = 0
  )) %>%
  
  mutate(Repellence_M_S = case_when(
    Repellence == 1 ~ 1,
    Repellence == 2 ~ 1,
    .default = 0
  )) 

df_modified <- df_modified %>% 
  mutate(multiple_constraints =
           (Physical_M_S +  Nutrient_M_S +  Acidity_M_S + Repellence_M_S))

df_modified <- df_modified %>% select(-Physical_M_S, -Nutrient_M_S, -Acidity_M_S, -Repellence_M_S)

################################################################################
### Option 2 means ----
#### using relative_yld_change mean of the treatments ---

## multiple_constraints
str(df_modified)
distinct(df_modified,multiple_constraints )

df_modified <- df_modified %>% 
  mutate(multiple_name1 = as.character(multiple_constraints))


df_modified_summary_yld_gain_multiple <- df_modified %>% 
  group_by(multiple_name1) %>% 
  summarise(mean = mean(relative_yld_change, na.rm = TRUE),
            sd = sd(relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n))  

df_modified_summary_yld_gain_multiple$multiple_name1 <- factor(df_modified_summary_yld_gain_multiple$multiple_name1,
                                                          levels = c("1","2","3", "4"),
                                                          labels = c("One constraint", "Two constraint",
                                                                     "Three constraint" , "Four constraint"))


df_modified_summary_yld_gain_multiple


df_modified_summary_yld_gain_multiple <- df_modified_summary_yld_gain_multiple %>% 
  mutate(label2 = paste0( multiple_name1 , "(", n, ")"))
df_modified_summary_yld_gain_multiple
df_modified_summary_yld_gain_multiple <- df_modified_summary_yld_gain_multiple %>% arrange() %>% 
  dplyr::mutate(Index = seq(1:4))

df_modified_summary_yld_gain_multiple <- df_modified_summary_yld_gain_multiple %>%
  mutate(label2 = fct_reorder(label2, Index)) %>%
  arrange(label2)

df_modified_summary_yld_gain_multiple


plot2 <-df_modified_summary_yld_gain_multiple %>% 
  filter(multiple_name1 != "One constraint"  ) %>% 
  ggplot(aes(y = label2, x = mean)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  
  labs(title = "Multiple soil constraints")+ 
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

plot2
################################################################################
##  option 1000
str(df_modified)
distinct(df_modified,multiple_constraints)


#Below are checks on grouping
# multiple_constraints4 <- df_modified %>% 
#   filter(multiple_constraints == 4) %>%
#   #distinct(tillage_class)
#   distinct(site_display)
# multiple_constraints4
# #only one site - Brimption Lake had 4 soil constraints and this site only used mixing
# 
# multiple_constraints3 <- df_modified %>% 
#   filter(multiple_constraints == 3) %>%
#   #distinct(tillage_class)
#   distinct(site_display)
# multiple_constraints3
# #only one site - Brimption Lake had 4 soil constraints and this site only used mixing



df_modified_summary_yld_gain_multiple_2 <- df_modified %>% 
  filter(multiple_constraints == 2) %>% 
  #filter(multiple_constraints == 3) %>%
  #filter(multiple_constraints == 4) %>%
  group_by(tillage_class) %>% 
  summarise(mean = mean(relative_yld_change, na.rm = TRUE),
            sd = sd(relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n))  
df_modified_summary_yld_gain_multiple_2

df_modified_summary_yld_gain_multiple_2_all <- df_modified %>% 
  filter(multiple_constraints == 2) %>% 
  #filter(multiple_constraints == 3) %>%
  #filter(multiple_constraints == 4) %>%
  group_by() %>% 
  summarise(mean = mean(relative_yld_change, na.rm = TRUE),
            sd = sd(relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n))  
df_modified_summary_yld_gain_multiple_2_all <- df_modified_summary_yld_gain_multiple_2_all %>% 
  mutate(tillage_class  = "All tillage",
         Index = (1+4),
         label2 = paste0( tillage_class , "(", n, ")"))

df_modified_summary_yld_gain_multiple_2_all



df_modified_summary_yld_gain_multiple_2 <- df_modified_summary_yld_gain_multiple_2 %>% 
  mutate(label2 = paste0( tillage_class , "(", n, ")"))
df_modified_summary_yld_gain_multiple_2

df_modified_summary_yld_gain_multiple_2 <- df_modified_summary_yld_gain_multiple_2 %>% arrange(mean    ) %>% 
  dplyr::mutate(Index = seq(1:4))


df_modified_summary_yld_gain_multiple_2
df_modified_summary_yld_gain_multiple_2 <- rbind(df_modified_summary_yld_gain_multiple_2, df_modified_summary_yld_gain_multiple_2_all)


df_modified_summary_yld_gain_multiple_2 <- df_modified_summary_yld_gain_multiple_2 %>%
  mutate(label2 = fct_reorder(label2, Index)) %>%
  arrange(label2)

df_modified_summary_yld_gain_multiple_2


plot2a <-df_modified_summary_yld_gain_multiple_2 %>% 
  ggplot(aes(y = label2, x = mean)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  
  labs(title = "Sites with 2 soil constraints rated as moderate or severe")+ 
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

plot2a



################################################################################
##  option 1000
str(df_modified)
distinct(df_modified,multiple_constraints)



df_modified_summary_yld_gain_multiple_3 <- df_modified %>% 
  #filter(multiple_constraints == 2) %>% 
  filter(multiple_constraints == 3) %>%
  #filter(multiple_constraints == 4) %>%
  group_by(tillage_class) %>% 
  summarise(mean = mean(relative_yld_change, na.rm = TRUE),
            sd = sd(relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n))  
df_modified_summary_yld_gain_multiple_3

df_modified_summary_yld_gain_multiple_3_all <- df_modified %>% 
  #filter(multiple_constraints == 2) %>% 
  filter(multiple_constraints == 3) %>%
  #filter(multiple_constraints == 4) %>%
  group_by() %>% 
  summarise(mean = mean(relative_yld_change, na.rm = TRUE),
            sd = sd(relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n))  
df_modified_summary_yld_gain_multiple_3_all <- df_modified_summary_yld_gain_multiple_3_all %>% 
  mutate(tillage_class  = "All tillage",
         Index = (1+4),
         label2 = paste0( tillage_class , "(", n, ")"))

df_modified_summary_yld_gain_multiple_3_all



df_modified_summary_yld_gain_multiple_3 <- df_modified_summary_yld_gain_multiple_3 %>% 
  mutate(label2 = paste0( tillage_class , "(", n, ")"))
df_modified_summary_yld_gain_multiple_3

df_modified_summary_yld_gain_multiple_3 <- df_modified_summary_yld_gain_multiple_3 %>% arrange(mean    ) %>% 
  dplyr::mutate(Index = seq(1:4))


df_modified_summary_yld_gain_multiple_3
df_modified_summary_yld_gain_multiple_3 <- rbind(df_modified_summary_yld_gain_multiple_3, df_modified_summary_yld_gain_multiple_3_all)


df_modified_summary_yld_gain_multiple_3 <- df_modified_summary_yld_gain_multiple_3 %>%
  mutate(label2 = fct_reorder(label2, Index)) %>%
  arrange(label2)

df_modified_summary_yld_gain_multiple_3


plot2b <-df_modified_summary_yld_gain_multiple_3 %>% 
  ggplot(aes(y = label2, x = mean)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  
  labs(title = "Sites with 3 soil constraints rated as moderate or severe")+ 
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

