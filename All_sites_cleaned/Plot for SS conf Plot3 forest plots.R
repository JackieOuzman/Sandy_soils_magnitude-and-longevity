library(ggplot2)
library(readxl)
library(tidyverse)
library(stringr)

library(dmetar)
library(tidyverse)
library(meta)

library(forcats)

## plot and analsyis for preso




df <- read.csv("N:/sandy soils conference/data/All_sites_cleaned/control_metadata_contraints_tillage_only_cleaned.csv" )

unique(df$crop_group)
unique(df$tillage_class)
# keep only one yield output
str(df)
names(df)
df_modified <- df %>% 
  dplyr::select(tillage_amendments_class,
         tillage_class,
         site_display,
         year,
         relative_yld_change,
         yield_gain,
         yield,
         control_yield,
         tillage_amendments_class,
         amendments_no_amend
  ) %>% 
  dplyr::mutate(site_year = paste0(site_display,"_", year)) %>% 
  filter(tillage_amendments_class != "Unmodified_amendment")

str(df_modified)




### Option 2 means ----
#### using Relative yield mean of the treatments ---

str(df_modified)
df_modified_summary <- df_modified %>% dplyr::group_by(site_display) %>% 
  dplyr::summarise(mean = mean( relative_yld_change, na.rm = TRUE),
            sd = sd( relative_yld_change, na.rm = TRUE),
            n = n())
                                        
df_modified_summary
m.mean <- metamean(n = n,
                   mean = mean,
                   sd = sd,
                   studlab = site_display,
                   data = df_modified_summary,
                   sm = "MRAW",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "Option 2")
summary(m.mean)
meta::forest(m.mean, 
             sortvar = mean,
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftlabs = c("Author", "g", "SE"))

meta::forest(m.mean, layout = "JAMA")

################################################################################
### Just for mixing tillage 
################################################################################
df_modified_summary_mix <- df_modified %>% 
  dplyr::filter(tillage_class == "Mixing") %>% 
  dplyr::group_by(site_display) %>% 
  dplyr::summarise(mean = mean( relative_yld_change, na.rm = TRUE),
                   sd = sd( relative_yld_change, na.rm = TRUE),
                   n = n())

df_modified_summary_mix
m.mean_mix <- metamean(n = n,
                   mean = mean,
                   sd = sd,
                   studlab = site_display,
                   data = df_modified_summary_mix,
                   sm = "MRAW",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "Option 2")
summary(m.mean_mix)
meta::forest(m.mean_mix, 
             sortvar = mean,
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftlabs = c("Author", "g", "SE"))

meta::forest(m.mean_mix, layout = "JAMA")

################################################################################
### Just for ripping tillage 
################################################################################
df_modified_summary_rip <- df_modified %>% 
  dplyr::filter(tillage_class == "Ripping") %>% 
  dplyr::group_by(site_display) %>% 
  dplyr::summarise(mean = mean( relative_yld_change, na.rm = TRUE),
                   sd = sd( relative_yld_change, na.rm = TRUE),
                   n = n())

df_modified_summary_rip
m.mean_rip <- metamean(n = n,
                       mean = mean,
                       sd = sd,
                       studlab = site_display,
                       data = df_modified_summary_rip,
                       sm = "MRAW",
                       fixed = FALSE,
                       random = TRUE,
                       method.tau = "REML",
                       method.random.ci = "HK",
                       title = "Option 2")
summary(m.mean_rip)
meta::forest(m.mean_rip, 
             sortvar = mean,
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftlabs = c("Author", "g", "SE"))

meta::forest(m.mean_rip, layout = "JAMA")

################################################################################
### Just for combination tillage 
################################################################################
df_modified_summary_comb <- df_modified %>% 
  dplyr::filter(tillage_class == "Combination") %>% 
  dplyr::group_by(site_display) %>% 
  dplyr::summarise(mean = mean( relative_yld_change, na.rm = TRUE),
                   sd = sd( relative_yld_change, na.rm = TRUE),
                   n = n())

df_modified_summary_comb
m.mean_cob <- metamean(n = n,
                       mean = mean,
                       sd = sd,
                       studlab = site_display,
                       data = df_modified_summary_comb,
                       sm = "MRAW",
                       fixed = FALSE,
                       random = TRUE,
                       method.tau = "REML",
                       method.random.ci = "HK",
                       title = "Option 2")
summary(m.mean_cob)
meta::forest(m.mean_cob, 
             sortvar = mean,
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftlabs = c("Author", "g", "SE"))

meta::forest(m.mean_cob, layout = "JAMA")



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
  dplyr::mutate(Index = seq(1:26))
forest_plot_input


forest_plot_input_total <- data.frame(
  Index = (26+1), ## This provides an order to the data
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
  xlab("Relative yield change (95% CI)") + 
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
summary(m.mean)
################################################################################
# forest plot with means and stdev rather than output from meta analysis
str(df_modified)



df_modified_summary_site_tillage <- df_modified %>% dplyr::group_by(tillage_class, site_display) %>% 
  dplyr::summarise(mean = mean( relative_yld_change, na.rm = TRUE),
            sd = sd( relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n) )
df_modified_summary_site_tillage <- ungroup(df_modified_summary_site_tillage)
dim(df_modified_summary_site_tillage)

df_modified_summary_site_tillage <- df_modified_summary_site_tillage %>% arrange(mean) %>% 
  dplyr::mutate(Index = seq(1:40))
df_modified_summary_site_tillage

#### all data

df_modified_summary_all <- df_modified %>% dplyr::group_by() %>% 
  dplyr::summarise(mean = mean( relative_yld_change, na.rm = TRUE),
            sd = sd( relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n) )
df_modified_summary_all <- ungroup(df_modified_summary_all)
df_modified_summary_all
df_modified_summary_all <- df_modified_summary_all %>% dplyr::arrange() %>% 
  dplyr::mutate(Index = 41)

df_modified_summary_tillage <- df_modified %>% dplyr::group_by(tillage_class) %>% 
  dplyr::summarise(mean = mean( relative_yld_change, na.rm = TRUE),
            sd = sd( relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n) )
df_modified_summary_tillage <- ungroup(df_modified_summary_tillage)
df_modified_summary_tillage
df_modified_summary_tillage <- df_modified_summary_tillage %>% dplyr::arrange() %>% 
 dplyr::mutate(Index = c( (41+1),(41+2), (41+3) ))

df_modified_summary_all <- df_modified_summary_all %>% 
  dplyr::mutate(tillage_class = "All tillage",
         site_display = "All sites")
df_modified_summary_all
df_modified_summary_tillage <- df_modified_summary_tillage %>% 
  dplyr::mutate(site_display = tillage_class)

df_modified_summary_tillage

df_modified_summary_site_tillage


df_modified_summary_v2 <- rbind(df_modified_summary_all,df_modified_summary_tillage, df_modified_summary_site_tillage )

df_modified_summary_v2 <- df_modified_summary_v2 %>% 
  dplyr::mutate(label2 = paste0(site_display, "(", n, ")"))
df_modified_summary_v2



df_modified_summary_v2 <- df_modified_summary_v2 %>%
  mutate(label2 = fct_reorder(label2, Index)) %>%
  #mutate(label2 = fct_reorder(label2)) %>%
  arrange(label2)

tail(df_modified_summary_v2)


plot2_mix <- df_modified_summary_v2 %>% 
  filter(tillage_class  == "Mixing" | tillage_class  == "All tillage" ) %>% 
  ggplot( aes(y = label2, x = mean)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  xlab("Relative yield change (SE)") + 
  ylab(" ") + 
  xlim(-20, 150)+
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black") ) 
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank())
plot2_mix

plot2_rip <- df_modified_summary_v2 %>% 
  filter(tillage_class  == "Ripping" | tillage_class  == "All tillage" ) %>% 
  ggplot( aes(y = label2, x = mean)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  xlab("Relative yield change (SE)") + 
  ylab(" ") +
  xlim(-20, 150)+
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black") ) 
#axis.title.x=element_blank(),
#axis.text.x=element_blank(),
#axis.ticks.x=element_blank())
plot2_rip

plot2_comb <- df_modified_summary_v2 %>% 
  filter(tillage_class  == "Combination" | tillage_class  == "All tillage" ) %>% 
  ggplot( aes(y = label2, x = mean)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  xlab("Relative yield change (SE)") + 
  ylab(" ") + 
  xlim(-20, 150)+
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black") ) 
#axis.title.x=element_blank(),
#axis.text.x=element_blank(),
#axis.ticks.x=element_blank())
plot2_comb



###############################################################################
### meta analysis by site by with just mixing or ripping no amendments 

str(df_modified)
df_modified_summary_no_amendments <- df_modified %>% 
  dplyr::filter(amendments_no_amend == "no_amendment") %>% 
  dplyr::group_by(site_display) %>% 
  dplyr::summarise(mean = mean( relative_yld_change, na.rm = TRUE),
                   sd = sd( relative_yld_change, na.rm = TRUE),
                   n = n())

df_modified_summary_no_amendments
m.mean <- metamean(n = n,
                   mean = mean,
                   sd = sd,
                   studlab = site_display,
                   data = df_modified_summary_no_amendments,
                   sm = "MRAW",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "Option 2")
summary(m.mean)
meta::forest(m.mean, 
             sortvar = mean,
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftlabs = c("Author", "g", "SE"))

meta::forest(m.mean, layout = "JAMA")

