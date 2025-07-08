library(ggplot2)
library(readxl)
library(tidyverse)
library(stringr)

## plot and analsyis for preso




df <- read.csv("N:/sandy soils conference/data/data for SS prestenation/control_metadata_contraints_tillage_only.csv" )


# keep only one yield output
str(df)
df_modified <- df %>% 
  select(tillage_amendments_class,
         tillage_class,
         site_display,
         year,
         yield_gain,
         relative_yld_change,
         yield,
         control_yield,
         tillage_amendments_class,
         ) %>% 
  dplyr::mutate(site_year = paste0(site_display,"_", year)) %>% 
  filter(tillage_amendments_class != "Unmodified_amendment")

str(df_modified)
################################################################################

df_modified_summary <- df_modified %>% group_by(tillage_amendments_class) %>% 
  summarise(n = n(),
            mean = mean(relative_yld_change,na.rm = TRUE),
            sd = sd(relative_yld_change,na.rm = TRUE),
            SE = sd/sqrt(n) )
ungroup(df_modified_summary)

df_modified_total <- df_modified %>% group_by() %>%
  summarise(
    n = n(),
    mean = mean(relative_yld_change, na.rm = TRUE),
    sd = sd(relative_yld_change, na.rm = TRUE),
    SE = sd / sqrt(n)
  ) 

df_modified_total <- df_modified_total %>%  mutate(tillage_amendments_class = "Total")
df_modified_summary
df_modified_total
df_modified_summary <- rbind(df_modified_summary, df_modified_total)
df_modified_summary
df_modified_summary$tillage_amendments_class <- factor(df_modified_summary$tillage_amendments_class, 
                                                       levels = c(
                                                         "Total",
                                                         "Inversion_no_amendment",
                                                         "Combination_no_amendment",
                                                         "Combination_amendment",
                                                         "Mixing_no_amendment", 
                                                         "Mixing_amendment",
                                                         "Ripping_no_amendment",
                                                         "Ripping_amendment" 
                                                         ),
                                                       labels = c(
                                                         "All tillage",
                                                         "Inversion only",
                                                         "Combination only",
                                                         "Combination with amendment", 
                                                         "Mixing only",
                                                         "Mixing with amendment",
                                                         "Ripping only", 
                                                         "Ripping with amendment" 
                                                                  ))

df_modified_summary <- df_modified_summary %>% mutate(plot_helper = case_when(
  tillage_amendments_class ==  "All tillage" ~ "Total",
  .default = "tillage_type"))
                                                      
df_modified_summary

plot2_yld_gain <- df_modified_summary %>%
  ggplot(aes(y = mean, x = tillage_amendments_class, fill = plot_helper)) +
  geom_col() +
  scale_fill_manual(values = c("grey", "blue")) +
    geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE),
                width = .2,
                position = position_dodge(.9)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "none")+
  xlab("") +
  ylab("Relative yield change") #+
  #labs(title = " gain above the control for tillage treatments")
plot2_yld_gain



#################################################################################
str(df_modified)
df_modified_summary <- df_modified %>% group_by(tillage_class) %>% 
  summarise(n = n(),
            mean = mean(relative_yld_change,na.rm = TRUE),
            sd = sd(relative_yld_change,na.rm = TRUE),
            SE = sd/sqrt(n) )
ungroup(df_modified_summary)

df_modified_total <- df_modified %>% group_by() %>%
  summarise(
    n = n(),
    mean = mean(relative_yld_change, na.rm = TRUE),
    sd = sd(relative_yld_change, na.rm = TRUE),
    SE = sd / sqrt(n)
  ) 

df_modified_total <- df_modified_total %>%  mutate(tillage_class = "Total")
df_modified_summary
df_modified_total
df_modified_summary <- rbind(df_modified_summary, df_modified_total)
arrange (df_modified_summary , mean)
df_modified_summary$tillage_class <- factor(df_modified_summary$tillage_class, 
                                                       levels = c(
                                                         "Total",
                                                         "Inversion",
                                                         "Combination",
                                                         "Ripping",
                                                         "Mixing" 
                                                       ),
                                                       labels = c(
                                                         "Total",
                                                         "Inversion",
                                                         "Combination",
                                                         "Ripping", 
                                                         "Mixing" 
                                                       ))

df_modified_summary <- df_modified_summary %>% mutate(plot_helper = case_when(
  tillage_class ==  "Total" ~ "Total",
  .default = "tillage_type"))

df_modified_summary

plot2_yld_gain <- df_modified_summary %>%
  ggplot(aes(y = mean, x = tillage_class, fill = plot_helper)) +
  geom_col() +
  scale_fill_manual(values = c("grey", "blue")) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE),
                width = .2,
                position = position_dodge(.9)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "none")+
  xlab("") +
  ylab("Relative yield change") #+
#labs(title = " gain above the control for tillage treatments")
plot2_yld_gain


