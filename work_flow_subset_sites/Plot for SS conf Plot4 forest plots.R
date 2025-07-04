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
         tillage_class
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


### Option 2 means ----
#### using InR_yield mean of the treatments ---

str(df_modified)
df_modified_summary <- df_modified %>% group_by(tillage_amendments_class) %>% 
  summarise(mean = mean(lnR_Yield, na.rm = TRUE),
            sd = sd(lnR_Yield, na.rm = TRUE),
            n = n())
                                        
df_modified_summary
m.mean <- metamean(n = n,
                   mean = mean,
                   sd = sd,
                   studlab = tillage_amendments_class   ,
                   data = df_modified_summary,
                   sm = "MRAW",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "Option 2")
summary(m.mean)

mean_transfomred1 <- (0.2553 -1) * (100/100)
mean_transfomred1
mean_transfomred2 <- exp(0.2553)
mean_transfomred2 # the treatment yield is 1.29 times more than the control
#If ln(y) = x, then y = e^x

### I can plot this a more manual way but I need to export some data from the analysis

dim(df_modified_summary)
#how many rows of data ?
forest_plot_input <- data.frame(
  #Index = seq(1:26), ## This provides an order to the data
  label = m.mean$studlab,
  SMD = m.mean$TE,
  LL = m.mean$lower,
  UL =  m.mean$upper,
  n = m.mean$n)

forest_plot_input
forest_plot_input <- forest_plot_input %>% arrange(SMD) %>% 
  dplyr::mutate(Index = seq(1:7))
forest_plot_input


forest_plot_input_total <- data.frame(
  Index = (7+1), ## This provides an order to the data
  label = "All tillage",
  SMD = m.mean$TE.random,
  LL = m.mean$lower.random,
  UL =  m.mean$upper.random,
  n =  sum(m.mean$n))
forest_plot_input_total

forest_plot_input <- rbind(forest_plot_input,forest_plot_input_total)
forest_plot_input


meta::forest(m.mean, 
             sortvar = mean,
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftlabs = c("Author", "g", "SE"))

meta::forest(m.mean, layout = "JAMA")

### comments - I need to modify data set so that the controls are better represented here.
### I think this is ok because I am using the natural log .
### Pull out new total from the results



forest_plot_input <- forest_plot_input %>% 
  mutate(label2 = paste0(label, "(", n, ")"))
forest_plot_input
forest_plot_input$label2
str(forest_plot_input)


forest_plot_input <- forest_plot_input %>%
  mutate(label2 = fct_reorder(label2, Index)) %>%
  arrange(label2)


plot1 <- ggplot(forest_plot_input, aes(y = label2, x = SMD)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  xlab("Natural log of yield gain (95% CI)") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))
plot1

################################################################################
### Option 2 means ----
#### using Yield gains mean of the treatments ---
str(df)
str(df_modified)
df_modified_summary_yld_gain <- df_modified %>% group_by(tillage_amendments_class) %>% 
  summarise(mean = mean(yield_gain, na.rm = TRUE),
            sd = sd(yield_gain, na.rm = TRUE),
            n = n())

df_modified_summary_yld_gain
m.meanYG <- metamean(n = n,
                   mean = mean,
                   sd = sd,
                   studlab = tillage_amendments_class,
                   data = df_modified_summary_yld_gain,
                   sm = "MRAW",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "Option 2")
summary(m.meanYG)
### home made plots
dim(df_modified_summary_yld_gain)
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
  xlab("Yield gain (95% CI)") + 
  ylab(" ") + 
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
### Option 2 means Tillage class----
#### using Yield gains mean of the treatments ---
str(df)
str(df_modified)
df_modified_summary_yld_gain <- df_modified %>% group_by(tillage_class) %>% 
  summarise(mean = mean(yield_gain, na.rm = TRUE),
            sd = sd(yield_gain, na.rm = TRUE),
            n = n())

df_modified_summary_yld_gain
m.meanYG <- metamean(n = n,
                     mean = mean,
                     sd = sd,
                     studlab = tillage_class,
                     data = df_modified_summary_yld_gain,
                     sm = "MRAW",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     method.random.ci = "HK",
                     title = "Option 2")
summary(m.meanYG)
### home made plots
dim(df_modified_summary_yld_gain)
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


plot2 <- ggplot(forest_plot_input_YG, aes(y = label2, x = SMD)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  xlab("Yield gain (95% CI)") + 
  ylab(" ") + 
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
