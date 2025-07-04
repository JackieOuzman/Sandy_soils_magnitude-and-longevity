library(ggplot2)
library(readxl)
library(tidyverse)
library(stringr)

library(dmetar)
library(tidyverse)
library(meta)

library(forcats)

## plot and analsyis for preso




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

#
################################################################################
### Option 2 means ----
#### using Yield gains mean of the treatments ---
str(df)
str(df_modified)

## Repellence
distinct(df_modified,Repellence)

df_modified_summary_yld_gain_Rep <- df_modified %>% 
  filter(Repellence == 1) %>% 
  group_by(tillage_amendments_class) %>% 
  summarise(mean = mean(yield_gain, na.rm = TRUE),
            sd = sd(yield_gain, na.rm = TRUE),
            n = n())

df_modified_summary_yld_gain_Rep
m.meanYG <- metamean(n = n,
                   mean = mean,
                   sd = sd,
                   studlab = tillage_amendments_class,
                   data = df_modified_summary_yld_gain_Rep,
                   sm = "MRAW",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "Option 2")
summary(m.meanYG)
### home made plots
dim(df_modified_summary_yld_gain_Rep)
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
  dplyr::mutate(Index = seq(1:7))
forest_plot_input_YG


forest_plot_input_total_YG <- data.frame(
  Index = (7+1), ## This provides an order to the data
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
  
  labs(title = "Sites with repellence rated moderate")+
  xlab("Yield gain (95% CI)") + 
  ylab("") + 
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

###############################################################################

## Physical
str(df_modified)
distinct(df_modified,Repellence)

df_modified_summary_yld_gain_Phy <- df_modified %>% 
  filter(Physical == 2) %>% 
  group_by(tillage_amendments_class) %>% 
  summarise(mean = mean(yield_gain, na.rm = TRUE),
            sd = sd(yield_gain, na.rm = TRUE),
            n = n())

df_modified_summary_yld_gain_Phy
m.meanYG <- metamean(n = n,
                     mean = mean,
                     sd = sd,
                     studlab = tillage_amendments_class,
                     data = df_modified_summary_yld_gain_Phy,
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
  dplyr::mutate(Index = seq(1:6))
forest_plot_input_YG


forest_plot_input_total_YG <- data.frame(
  Index = (6+1), ## This provides an order to the data
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
  
  labs(title = "Sites with physical rated sereve")+
  xlab("Yield gain (95% CI)") + 
  ylab("") + 
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

## Physical option 1000
str(df_modified)
distinct(df_modified,Repellence)

df_modified_summary_yld_gain_Phy <- df_modified %>% 
  #filter(Physical == 2) %>% 
  group_by(Physical) %>% 
  summarise(mean = mean(yield_gain, na.rm = TRUE),
            sd = sd(yield_gain, na.rm = TRUE),
            n = n())

df_modified_summary_yld_gain_Phy
m.meanYG <- metamean(n = n,
                     mean = mean,
                     sd = sd,
                     studlab = Physical  ,
                     data = df_modified_summary_yld_gain_Phy,
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
  
  labs(title = "Physical")+
  xlab("Yield gain (95% CI)") + 
  ylab("") + 
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


