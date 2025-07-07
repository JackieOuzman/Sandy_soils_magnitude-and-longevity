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
#### using Yield gains mean of the treatments ---

## multiple_constraints
str(df_modified)
distinct(df_modified,multiple_constraints )

df_modified <- df_modified %>% 
  mutate(multiple_name1 = as.character(multiple_constraints))


df_modified_summary_yld_gain_multiple <- df_modified %>% 
  group_by(multiple_name1) %>% 
  summarise(mean = mean(yield_gain, na.rm = TRUE),
            sd = sd(yield_gain, na.rm = TRUE),
            n = n()) 

df_modified_summary_yld_gain_multiple$multiple_name1 <- factor(df_modified_summary_yld_gain_multiple$multiple_name1,
                                                          levels = c("1","2","3", "4"),
                                                          labels = c("One constraint", "Two constraint",
                                                                     "Three constraint" , "Four constraint"))



df_modified_summary_yld_gain_multiple
m.meanYG <- metamean(n = n,
                     mean = mean,
                     sd = sd,
                     studlab = multiple_name1,
                     data = df_modified_summary_yld_gain_multiple,
                     sm = "MRAW",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     method.random.ci = "HK",
                     title = "Option 2")
summary(m.meanYG)
### home made plots
dim(df_modified_summary_yld_gain_multiple)
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
  #arrange(SMD) %>% 
  dplyr::mutate(Index = seq(1:4))
forest_plot_input_YG


forest_plot_input_total_YG <- data.frame(
  Index = (4+1), ## This provides an order to the data
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
  filter(label != "One constraint" & label != "All tillage"  ) %>% 
  ggplot(aes(y = label2, x = SMD)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  
  labs(title = "Multiple soil constraints")+ 
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
################################################################################
##  option 1000
str(df_modified)
distinct(df_modified,multiple_constraints)


multiple_constraints4 <- df_modified %>% 
  filter(multiple_constraints == 4) %>%
  #distinct(tillage_class)
  distinct(site_display)
multiple_constraints4
#only one site - Brimption Lake had 4 soil constraints and this site only used mixing

multiple_constraints3 <- df_modified %>% 
  filter(multiple_constraints == 3) %>%
  #distinct(tillage_class)
  distinct(site_display)
multiple_constraints3
#only one site - Brimption Lake had 4 soil constraints and this site only used mixing



df_modified_summary_yld_gain_multiple <- df_modified %>% 
  filter(multiple_constraints == 2) %>% 
  #filter(multiple_constraints == 3) %>%
  #filter(multiple_constraints == 4) %>%
  group_by(tillage_class) %>% 
  summarise(mean = mean(yield_gain, na.rm = TRUE),
            sd = sd(yield_gain, na.rm = TRUE),
            n = n())
df_modified_summary_yld_gain_multiple



m.meanYG <- metamean(n = n,
                     mean = mean,
                     sd = sd,
                     studlab = tillage_class    ,
                     data = df_modified_summary_yld_gain_multiple,
                     sm = "MRAW",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     method.random.ci = "HK",
                     title = "Option 2")
summary(m.meanYG)
### home made plots
dim(df_modified_summary_yld_gain_Phy)
#how many rows of data ?
forest_plot_input_YG <- data.frame(
  #Index = seq(1:4), ## This provides an order to the data
  label = m.meanYG$studlab,
  SMD = m.meanYG$TE,
  LL = m.meanYG$lower,
  UL =  m.meanYG$upper,
  n = m.meanYG$n)

forest_plot_input_YG
forest_plot_input_YG <- forest_plot_input_YG %>% arrange(SMD) %>% 
  dplyr::mutate(Index = seq(1:4))
forest_plot_input_YG


forest_plot_input_total_YG <- data.frame(
  Index = (4+1), ## This provides an order to the data
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


plot2 <- forest_plot_input_YG %>% 
filter(label != "Inversion") %>% 
  ggplot( aes(y = label2, x = SMD)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  
  labs(title = "Sites with 2 constraints rated as moderate or severe issue")+
  #labs(title = "Sites with 3 constraints rated as moderate or severe issue")+
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

################################################################################
## Nutrition
str(df_modified)
distinct(df_modified,Nutrient )

df_modified <- df_modified %>% 
  mutate(Nutrient_name1 = as.character(Nutrient))


df_modified_summary_yld_gain_Nutr <- df_modified %>% 
  group_by(Nutrient_name1) %>% 
  summarise(mean = mean(yield_gain, na.rm = TRUE),
            sd = sd(yield_gain, na.rm = TRUE),
            n = n()) 

df_modified_summary_yld_gain_Nutr$Nutrient_name1 <- factor(df_modified_summary_yld_gain_Nutr$Nutrient_name1,
                                                          levels = c("0","1","2"),
                                                          labels = c("No issue", "Moderate issue","Severe issue" ))



df_modified_summary_yld_gain_Nutr
m.meanYG <- metamean(n = n,
                     mean = mean,
                     sd = sd,
                     studlab = Nutrient_name1,
                     data = df_modified_summary_yld_gain_Nutr,
                     sm = "MRAW",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     method.random.ci = "HK",
                     title = "Option 2")
summary(m.meanYG)
### home made plots
dim(df_modified_summary_yld_gain_Phy)
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
  #arrange(SMD) %>% 
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


plot2 <- ggplot(forest_plot_input_YG, aes(y = label2, x = SMD)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  
  labs(title = "Nutrition constraints")+ 
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
################################################################################
## nutrients option 1000
str(df_modified)
distinct(df_modified,Nutrient)

df_modified_summary_yld_gain_Nutr <- df_modified %>% 
  #filter(Nutrient == 2) %>% 
  filter(Nutrient == 1) %>%
  group_by(tillage_class) %>% 
  summarise(mean = mean(yield_gain, na.rm = TRUE),
            sd = sd(yield_gain, na.rm = TRUE),
            n = n())
df_modified_summary_yld_gain_Nutr



df_modified_summary_yld_gain_Nutr
m.meanYG <- metamean(n = n,
                     mean = mean,
                     sd = sd,
                     studlab = tillage_class    ,
                     data = df_modified_summary_yld_gain_Nutr,
                     sm = "MRAW",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     method.random.ci = "HK",
                     title = "Option 2")
summary(m.meanYG)
### home made plots
dim(df_modified_summary_yld_gain_Phy)
#how many rows of data ?
forest_plot_input_YG <- data.frame(
  #Index = seq(1:4), ## This provides an order to the data
  label = m.meanYG$studlab,
  SMD = m.meanYG$TE,
  LL = m.meanYG$lower,
  UL =  m.meanYG$upper,
  n = m.meanYG$n)

forest_plot_input_YG
forest_plot_input_YG <- forest_plot_input_YG %>% arrange(SMD) %>% 
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


plot2 <- ggplot(forest_plot_input_YG, aes(y = label2, x = SMD)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  
  #labs(title = "Sites with nutrition constraints rated as severe issue")+
  labs(title = "Sites with nutrition constraints rated as moderate issue")+
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
