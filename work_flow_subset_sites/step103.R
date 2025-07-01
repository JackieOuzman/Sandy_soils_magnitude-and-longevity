###Script for sandy soils data analysis
## getting ready for meta analysis


library(ggplot2)
library(readxl)
library(tidyverse)
library(stringr)
library(ggpubr)
library(nlme)

#The data has been manipulated in "Analysis for sandy soils conf 2025 version 2.rmd
# With the output as 
# this should have the reps matched to a control

df <- read.csv("N:/sandy soils conference/data/data for SS prestenation/control_metadata_contraints_withYP_select_sites.csv" )

# Brooker is problematic so lets remove it.
df <- df %>% filter(  site_display == "Brimpton Lake"|
                        #site_display == "Brooker"|
                        site_display == "Buckleboo"|
                        site_display == "Bute"|
                        site_display == "Bute boundary"|
                        site_display == "Cadgee"
)

# keep only one yield output
str(df)
df_modified <- df %>% 
  select(tillage_amendments_class,
         site_display,
         year,
         yield,
         control_yield,
         tillage_amendments_class,
         Physical ) %>% 
  dplyr::mutate(site_year = paste0(site_display,"_", year))

str(df_modified)
df_modified_long <- df_modified %>% pivot_longer(cols = yield:control_yield,
                                                 names_to = c("type"),
                                                 values_to = "yield"
)
df_modified_long

df_modified_summary <- df_modified_long %>% group_by(site_display,type,Physical) %>% 
  summarise(n = n(),
            mean = mean(yield),
            sd = sd(yield))

df_modified_summary

df_modified_summary_wide <-   df_modified_summary %>% pivot_wider(
  names_from = type, values_from = c(n ,mean, sd ))
names(df_modified_summary_wide)


df_modified_summary_wide <- df_modified_summary_wide %>% 
  rename(
         n.e = n_yield,
         mean.e = mean_yield,
         sd.e = sd_yield,
         n.c = n_control_yield,
         mean.c = mean_control_yield,
         sd.c = sd_control_yield)
         
df_modified_summary_wide <- df_modified_summary_wide %>%   
  select(site_display, n.e, mean.e, sd.e, n.c, mean.c, sd.c, Physical )

df_modified_summary_wide <- df_modified_summary_wide %>% filter(!is.na(mean.e))
ungroup(df_modified_summary_wide)
str(df_modified_summary_wide)

m.cont <- metacont(n.e = n.e,
                   mean.e = mean.e,
                   sd.e = sd.e,
                   n.c = n.c,
                   mean.c = mean.c,
                   sd.c = sd.c,
                   studlab = site_display,
                   data = df_modified_summary_wide,
                   sm = "SMD",
                   method.smd = "Hedges",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "Site year")
summary(m.cont)

################################################################################
## how would I plot these results
meta::forest(m.cont, 
             sortvar = TE,
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftlabs = c("Author", "g", "SE"))

meta::forest(m.cont, layout = "JAMA")
### I can plot this a more manual way but I need to export some data from the analysis

dim(df_modified_summary_wide)
#how many rows of data ?
forest_plot_input <- data.frame(
  Index = seq(1:3), ## This provides an order to the data
  label = m.cont$studlab,
  SMD = m.cont$TE,
  LL = m.cont$lower,
  UL =  m.cont$upper)

forest_plot_input_total <- data.frame(
  Index = (3+1), ## This provides an order to the data
  label = "Total",
  SMD = m.cont$TE.random,
  LL = m.cont$lower.random,
  UL =  m.cont$upper.random)
forest_plot_input <- rbind(forest_plot_input,forest_plot_input_total)


plot1 <- ggplot(forest_plot_input, aes(y = label, x = SMD)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  #scale_y_continuous(name = "", breaks=1:4, labels = forest_plot_input$label, trans = "reverse") +
  xlab("SMD (95% CI)") + 
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

# can I analyse the subgroup?

update(m.cont, 
       subgroup = Physical , 
       tau.common = FALSE)

# the results are often reported in a table, but I am not sure how to extract or interperet the results


################################################################################

# can I analyse using multiple regression?
m.gen.reg <- metareg(m.cont, ~Physical)
m.gen.reg

## plot output
bubble(m.gen.reg, studlab = TRUE)

################################################################################


#### different example
library(dmetar)
library(tidyverse)
library(meta)
data(BdiScores)

# We only need the first four columns
glimpse(BdiScores[,1:4])


m.mean <- metamean(n = n,
                   mean = mean,
                   sd = sd,
                   studlab = author,
                   data = BdiScores,
                   sm = "MRAW",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "BDI-II Scores")
summary(m.mean)
################################################################################
### My data
str(df)
df_modified_example2 <- df %>% 
  dplyr::mutate(site_year = paste0(site_display,"_", year)) %>% 
select(site_year,
       tillage_amendments_class,
       yield_gain,
       tillage_amendments_class,
       Physical)
str(df_modified_example2)
df_modified_example2_summary <- df_modified_example2 %>% 
  group_by(site_year, Physical) %>% 
  summarise(n = n(),
            mean = mean(yield_gain ),
            sd = sd(yield_gain ))


df_modified_example2_summary <- df_modified_example2_summary %>% 
  filter(!is.na(mean)) %>% 
  filter(mean != "Inf")
  

m.mean <- metamean(n = n,
                   mean = mean,
                   sd = sd,
                   studlab = site_year,
                   data = df_modified_example2_summary,
                   sm = "MRAW",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "Option 2")
summary(m.mean)
###############################################################################
## how would I plot these results I dont think it works for option 2 analysis
meta::forest(metamean, 
             sortvar = mean,
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftlabs = c("Author", "g", "SE"))

meta::forest(metamean, layout = "JAMA")
