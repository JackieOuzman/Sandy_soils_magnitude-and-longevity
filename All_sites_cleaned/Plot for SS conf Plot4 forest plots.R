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



# keep only one yield output
str(df)
df_modified <- df %>% 
  dplyr::select(tillage_amendments_class,
         site_display,
         year,
         yield_gain,
         relative_yld_change,
         yield,
         control_yield,
         tillage_amendments_class,
         tillage_class,
         amendments_no_amend 
  ) %>% 
  dplyr::mutate(site_year = paste0(site_display,"_", year)) %>% 
  filter(tillage_amendments_class != "Unmodified_amendment")

str(df_modified)




### Option 2 means ----
#### using  mean of the treatments ---

str(df_modified)
df_modified_summary <- df_modified %>% dplyr::group_by(tillage_amendments_class) %>% 
  dplyr::summarise(mean = mean(relative_yld_change     , na.rm = TRUE),
            sd = sd(relative_yld_change     , na.rm = TRUE),
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

meta::forest(m.mean, 
             sortvar = mean,
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftlabs = c("Author", "g", "SE"))

meta::forest(m.mean, layout = "JAMA")

### I can plot this a more manual way but I need to export some data from the analysis



##############################################################################
## Detailed tillage type tillage_amendments_class
# forest plot with means and stdev rather than output from meta analysis
str(df_modified)



df_modified_summar_tillage <- df_modified %>% dplyr::group_by(tillage_amendments_class) %>% 
  dplyr::summarise(mean = mean( relative_yld_change, na.rm = TRUE),
            sd = sd( relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n) )
df_modified_summar_tillage <- ungroup(df_modified_summar_tillage)
dim(df_modified_summar_tillage)

df_modified_summar_tillage <- df_modified_summar_tillage %>% dplyr::arrange(mean) %>% 
  dplyr::mutate(Index = seq(1:7))


df_modified_summary_all <- df_modified %>% dplyr::group_by() %>% 
  dplyr::summarise(mean = mean( relative_yld_change, na.rm = TRUE),
            sd = sd( relative_yld_change, na.rm = TRUE),
            n = n(),
            SE = sd/sqrt(n) )
df_modified_summary_all <- ungroup(df_modified_summary_all)
df_modified_summary_all
df_modified_summary_all <- df_modified_summary_all %>% dplyr::arrange() %>% 
  dplyr::mutate(Index = (7+1))


df_modified_summary_all <- df_modified_summary_all %>% 
  mutate(tillage_amendments_class = "All tillage")

df_modified_summary_all
df_modified_summar_tillage


df_modified_summary_v2 <- rbind(df_modified_summary_all,df_modified_summar_tillage )
df_modified_summary_v2

df_modified_summary_v2$tillage_amendments_class <- factor(df_modified_summary_v2$tillage_amendments_class,
                                                levels = c("Inversion_no_amendment",
                                                           "Combination_amendment",
                                                           "Ripping_no_amendment",
                                                           "Combination_no_amendment",
                                                           "Ripping_amendment",
                                                           "Mixing_no_amendment",
                                                           "Mixing_amendment",
                                                           "All tillage"),
                                                labels = c("Inversion only", 
                                                           "Combination & amendment",
                                                           "Ripping only",
                                                           "Combination only",
                                                           "Ripping & amendment",
                                                           "Mixing only",
                                                           "Mixing & amendment",
                                                           "All tillage"
                                                           ))



df_modified_summary_v2 <- df_modified_summary_v2 %>% 
  mutate(label2 = paste0(tillage_amendments_class  , "(", n, ")"))
df_modified_summary_v2



df_modified_summary_v2 <- df_modified_summary_v2 %>%
  dplyr::mutate(label2 = fct_reorder(label2, Index)) %>%
  #mutate(label2 = fct_reorder(label2)) %>%
  dplyr::arrange(label2)


plot2 <- df_modified_summary_v2 %>% 
  ggplot( aes(y = label2, x = mean)) +
  geom_point(shape = 18, size = 6) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.3) +
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
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black") ) 
#axis.title.x=element_blank(),
#axis.text.x=element_blank(),
#axis.ticks.x=element_blank())
plot2


##############################################################################
## Detailed  amendments only
# forest plot with means and stdev rather than output from meta analysis
str(df_modified)



df_modified_summar_tillage <- df_modified %>% dplyr::group_by(amendments_no_amend) %>% 
  dplyr::summarise(mean = mean( relative_yld_change, na.rm = TRUE),
                   sd = sd( relative_yld_change, na.rm = TRUE),
                   n = n(),
                   SE = sd/sqrt(n) )
df_modified_summar_tillage <- ungroup(df_modified_summar_tillage)
dim(df_modified_summar_tillage)

df_modified_summar_tillage <- df_modified_summar_tillage %>% dplyr::arrange(mean) %>% 
  dplyr::mutate(Index = seq(1:2))


df_modified_summary_all <- df_modified %>% dplyr::group_by() %>% 
  dplyr::summarise(mean = mean( relative_yld_change, na.rm = TRUE),
                   sd = sd( relative_yld_change, na.rm = TRUE),
                   n = n(),
                   SE = sd/sqrt(n) )
df_modified_summary_all <- ungroup(df_modified_summary_all)
df_modified_summary_all
df_modified_summary_all <- df_modified_summary_all %>% dplyr::arrange() %>% 
  dplyr::mutate(Index = (2+1))


df_modified_summary_all <- df_modified_summary_all %>% 
  mutate(amendments_no_amend = "All tillage")

df_modified_summary_all
df_modified_summar_tillage


df_modified_summary_v2 <- rbind(df_modified_summary_all,df_modified_summar_tillage )
df_modified_summary_v2

df_modified_summary_v2$amendments_no_amend <- factor(df_modified_summary_v2$amendments_no_amend,
                                                          levels = c("no_amendment",
                                                                     "amendment",
                                                                     "All tillage"),
                                                          labels = c("no amendment",
                                                                     "amendment",
                                                                     "All tillage"
                                                          ))



df_modified_summary_v2 <- df_modified_summary_v2 %>% 
  mutate(label2 = paste0(amendments_no_amend  , "(", n, ")"))
df_modified_summary_v2



df_modified_summary_v2 <- df_modified_summary_v2 %>%
  dplyr::mutate(label2 = fct_reorder(label2, Index)) %>%
  #mutate(label2 = fct_reorder(label2)) %>%
  dplyr::arrange(label2)


plot2 <- df_modified_summary_v2 %>% 
  ggplot( aes(y = label2, x = mean)) +
  geom_point(shape = 18, size = 6) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.3) +
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
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black") ) 
#axis.title.x=element_blank(),
#axis.text.x=element_blank(),
#axis.ticks.x=element_blank())
plot2
