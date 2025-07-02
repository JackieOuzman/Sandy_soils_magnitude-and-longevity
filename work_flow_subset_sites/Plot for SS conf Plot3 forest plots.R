library(ggplot2)
library(readxl)
library(tidyverse)
library(stringr)

library(dmetar)
library(tidyverse)
library(meta)

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

df_modified <- df_modified %>% 
  mutate(lnR_Yield = log(yield / control_yield))
################################################################################
#Heap of sites with no yield- not sure what that is about?
df_modified <- df_modified %>% filter(yield!= 0.0)
df_modified <- df_modified %>% filter(control_yield>  0.0)
################################################################################


### meta anlaysis option 2 means
df_modified
df_modified_summary <- df_modified %>% group_by(site_display) %>% 
  summarise(mean = mean(lnR_Yield, na.rm = TRUE),
            sd = sd(lnR_Yield, na.rm = TRUE),
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

### I can plot this a more manual way but I need to export some data from the analysis

dim(df_modified_summary)
#how many rows of data ?
forest_plot_input <- data.frame(
  Index = seq(1:26), ## This provides an order to the data
  label = m.mean$studlab,
  SMD = m.mean$TE,
  LL = m.mean$lower,
  UL =  m.mean$upper)
forest_plot_input

forest_plot_input_total <- data.frame(
  Index = (26+1), ## This provides an order to the data
  label = "Total",
  SMD = m.mean$TE.random,
  LL = m.mean$lower.random,
  UL =  m.mean$upper.random)
forest_plot_input_total

forest_plot_input <- rbind(forest_plot_input,forest_plot_input_total)
forest_plot_input

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
