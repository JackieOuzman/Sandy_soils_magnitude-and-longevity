library(ggplot2)
library(readxl)
library(tidyverse)
library(stringr)

## plot and analsyis for preso

### we grouped our data into 4 And classed into 4 classes:
# Mixing without amendments
# Mixing with amendments
# 
# Ripping without amendments
# Ripping with amendments



df <- read.csv("N:/sandy soils conference/data/data for SS prestenation/control_metadata_contraints_withYP_select_sites.csv" )

## How many site?
df %>% distinct(site_display)


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
         yield_gain,
         yield,
         control_yield,
         tillage_amendments_class,
         ) %>% 
  dplyr::mutate(site_year = paste0(site_display,"_", year)) %>% 
  filter(tillage_amendments_class != "Unmodified_amendment")

str(df_modified)

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
                                                         "Mixing_no_amendment", 
                                                         "Ripping_no_amendment",
                                                         "Mixing_amendment",
                                                         "Ripping_amendment" 
                                                         ),
                                                       labels = c(
                                                         "Total",
                                                         "Mixing only",
                                                         "Ripping only", 
                                                         "Mixing with amendment",        
                                                        "Ripping with amendment" 
                                                                  ))

df_modified_summary

plot2 <- df_modified_summary %>%
  ggplot(aes(y = mean, x = tillage_amendments_class)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,
                position=position_dodge(.9)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        
  xlab("") + 
  ylab(" Yield gain t/ha") + 
  labs(title = "Yield gain above the control for tillage treatments")  
plot2

plot1 <- ggplot(df_modified_summary, aes(y = tillage_amendments_class, x = mean)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = mean-SE, xmax = mean+SE), height = 0.25) +
  #geom_errorbarh(aes(ymin=mean-SE, ymax=mean-SE), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  #scale_y_continuous(name = "", breaks=1:4, labels = forest_plot_input$label, trans = "reverse") +
  xlab("Yield gain") + 
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
