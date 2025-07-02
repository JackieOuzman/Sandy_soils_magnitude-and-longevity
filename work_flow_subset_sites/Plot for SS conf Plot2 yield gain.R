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
         site_display,
         year,
         yield_gain,
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
            mean = mean(yield_gain,na.rm = TRUE),
            sd = sd(yield_gain,na.rm = TRUE),
            SE = sd/sqrt(n) )
ungroup(df_modified_summary)

df_modified_total <- df_modified %>% group_by() %>%
  summarise(
    n = n(),
    mean = mean(yield_gain, na.rm = TRUE),
    sd = sd(yield_gain, na.rm = TRUE),
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
                                                         "Ripping_no_amendment",
                                                         "Mixing_amendment",
                                                         "Ripping_amendment" 
                                                         ),
                                                       labels = c(
                                                         "All tillage",
                                                         "Inversion only",
                                                         "Combination only",
                                                         "Combination with amendment", 
                                                         "Mixing only",
                                                         "Ripping only", 
                                                         "Mixing with amendment",        
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
  ylab(" Yield gain t/ha") #+
  #labs(title = "Yield gain above the control for tillage treatments")
plot2_yld_gain



################################################################################
##Natural log of the yield response
#lnR_Yield = log(Yield / Yield_ref)


str(df_modified)

df_modified <- df_modified %>% 
  mutate(lnR_Yield = log(yield / control_yield))
################################################################################
#Heap of sites with no yield- not sure what that is about?
df_modified <- df_modified %>% filter(yield!= 0.0)
df_modified <- df_modified %>% filter(control_yield>  0.0)
################################################################################

df_modified_Natural_log_summary <- df_modified %>% group_by(tillage_amendments_class) %>% 
  summarise(n = n(),
            mean = mean(lnR_Yield,na.rm = TRUE),
            sd = sd(lnR_Yield,na.rm = TRUE),
            SE = sd/sqrt(n) )
ungroup(df_modified_Natural_log_summary)


Natural_log_summary_total <- df_modified %>% group_by() %>%
  summarise(
    n = n(),
    mean = mean(lnR_Yield, na.rm = TRUE),
    sd = sd(lnR_Yield, na.rm = TRUE),
    SE = sd / sqrt(n)
  ) 

Natural_log_summary_total <- Natural_log_summary_total %>%  mutate(tillage_amendments_class = "Total")

df_modified_Natural_log_summary
Natural_log_summary_total

df_modified_Natural_log_summary <- rbind(df_modified_Natural_log_summary, Natural_log_summary_total)
df_modified_Natural_log_summary
df_modified_Natural_log_summary$tillage_amendments_class <- factor(df_modified_Natural_log_summary$tillage_amendments_class, 
                                                       levels = c(
                                                         "Total",
                                                         "Inversion_no_amendment",
                                                         "Combination_no_amendment",
                                                         "Combination_amendment",
                                                         "Mixing_no_amendment", 
                                                         "Ripping_no_amendment",
                                                         "Mixing_amendment",
                                                         "Ripping_amendment" 
                                                       ),
                                                       labels = c(
                                                         "All tillage",
                                                         "Inversion only",
                                                         "Combination only",
                                                         "Combination with amendment", 
                                                         "Mixing only",
                                                         "Ripping only", 
                                                         "Mixing with amendment",        
                                                         "Ripping with amendment" 
                                                       ))

df_modified_Natural_log_summary <- df_modified_Natural_log_summary %>% mutate(plot_helper = case_when(
  tillage_amendments_class ==  "All tillage" ~ "Total",
  .default = "tillage_type"))

df_modified_Natural_log_summary



plot2_NaturalLog <- df_modified_Natural_log_summary %>%
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
  ylab(" Natural log of yield gain. log(yield / control_yield)") 

plot2_NaturalLog









################################################################################






# plot1 <- ggplot(df_modified_summary, aes(y = tillage_amendments_class, x = mean)) +
#   geom_point(shape = 18, size = 5) +  
#   geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
#   #geom_errorbarh(aes(ymin=mean-SE, ymax=mean-SE), height = 0.25) +
#   geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
#   #scale_y_continuous(name = "", breaks=1:4, labels = forest_plot_input$label, trans = "reverse") +
#   xlab("Yield gain") + 
#   ylab(" ") + 
#   theme_bw() +
#   theme(panel.border = element_blank(),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         axis.line = element_line(colour = "black"),
#         axis.text.y = element_text(size = 12, colour = "black"),
#         axis.text.x.bottom = element_text(size = 12, colour = "black"),
#         axis.title.x = element_text(size = 12, colour = "black"))
# plot1
