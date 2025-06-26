#install.packages("esc")
#devtools::install_github("MathiasHarrer/dmetar")
#install.packages("dmetar") #not working
#install.packages("meta")

library(tidyverse)
library(esc)
library(dmetar)
library(meta)

#https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/forest.html

# Part 2 example for my data
# 4.2.5 Means I think this is closest to what I will do----

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

meta::forest(m.mean, 
             sortvar = TE,
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftlabs = c("Author", "g", "SE"))
meta::forest(m.mean, layout = "JAMA")
meta::forest(m.mean, layout = "RevMan5")
png(file = "forestplot.png", width = 2800, height = 2400, res = 300)
meta::forest(m.mean, 
             sortvar = TE,
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftlabs = c("Author", "g", "SE"))

dev.off()

# With my data  ----
control_metadata_contraints <- read.csv("N:/sandy soils conference/data/data for SS prestenation/control_metadata_contraints.csv")
names(control_metadata_contraints)

# Step 1 summaries the trial data ---
# Question by site or treatment? 
# Question group by tillage type ?
# Calculate the n = n,  mean = mean,  sd = sd,
# only keep the tillage data
distinct(control_metadata_contraints, tillage_class)

tillage_summary <- control_metadata_contraints %>% 
  filter(tillage_class != "Unmodified") %>%
  filter(!is.na(yield)) %>%
  filter(!is.na(control_yield)) %>% 
  filter(tillage_class != "Unmodified") %>% #this might be amendments etc
  
  group_by(site_display,  Physical) %>% 
  summarise(
    n = n(),
    mean = mean(yield_gain, na.rm = TRUE),
    sd = sd(yield_gain),
    se = sd / sqrt(n))
           
tillage_summary


m.mean <- metamean(n = n,
                   mean = mean,
                   sd = sd,
                   studlab = site_display,
                   data = tillage_summary,
                   sm = "MRAW",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "Sandy soils sites")

m.mean
meta::forest(m.mean, 
             sortvar = mean,
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftlabs = c("Sites", "g", "sd"))
meta::forest(m.mean, layout = "JAMA")
# tillage_summary_mean <- metagen(
#                 TE = mean,
#                  seTE = se,
#                  studlab = site_display,
#                  data = tillage_summary,
#                  sm = "SMD",
#                  fixed = FALSE,
#                  random = TRUE,
#                  method.tau = "REML",
#                  method.random.ci = "HK",
#                  title = "Sandys_soils1")
# 
# tillage_summary_mean
# meta::forest(tillage_summary_mean)
# bubble(tillage_summary_mean, studlab = TRUE)
# 
# metareg(tillage_summary_mean, Physical) # this is the same as above.






## is this what the example data looks like?
BdiScores # yip it close
m.mean_tillage <- metamean(n = n,
                   mean = mean,
                   sd = sd,
                   studlab = tillage_class,
                   data = tillage_summary,
                   sm = "MRAW",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "Tillage type")
summary(m.mean_tillage)

meta::forest(m.mean_tillage, 
             sortvar = TE,
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftlabs = c("Tillage", "g", "SE"))
meta::forest(m.mean_tillage, layout = "JAMA")
meta::forest(m.mean_tillage, layout = "RevMan5")
#png(file = "forestplot.png", width = 2800, height = 2400, res = 300)

meta::forest(m.mean_tillage, layout = "JAMA")
meta::forest(m.mean_tillage, layout = "RevMan5")
