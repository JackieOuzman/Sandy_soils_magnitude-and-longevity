library(ggplot2)
library(readxl)
library(tidyverse)
library(stringr)
library(ggpubr)


################################################################################
# Import the data created in Analysis for sandys soils conf 2025 version 2 ----------


control_metadata_contraints <- read.csv("N:/sandy soils conference/data/data for SS prestenation/control_metadata_contraints.csv")
names(control_metadata_contraints)

str(control_metadata_contraints$crop)

# what crop?
crops <- control_metadata_contraints %>% distinct(crop) 
crops %>% arrange(crop)

#What tillage
tillage <- control_metadata_contraints %>% distinct(soil_modification) 
tillage %>% arrange(soil_modification)

list_tillage_sites <- control_metadata_contraints %>% distinct(site_display, soil_modification) %>%  filter(soil_modification != "Unmodified")
  
write_csv(list_tillage_sites,"N:/sandy soils conference/data/data for SS prestenation/list_tillage_sites.csv" ) 

################################################################################
# Plot of control yield vs treatment yield -----

# Option 1
control_vs_treatment_option1 <- control_metadata_contraints %>%
  filter(!is.na(yield)) %>%
  ggplot(mapping = aes(control_yield, yield)) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed",
              colour = "blue",
              linewidth = 1.5) +
  geom_point(alpha = 0.01) +
  geom_smooth(method = lm, se = FALSE) +
  scale_x_continuous(breaks = seq(0, 10, by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 10, by = 0.5)) +
  theme_bw() +
  labs(x = "control yield t/ha", y = "treatment yield t/ha")
  

ggsave(control_vs_treatment_option1,
       device = "png",
       filename = "control_vs_treatment_option1.png",
       path= "N:/sandy soils conference/plots/",
       width=8.62,
       height = 6.28,
       dpi=600
) 


#what is the average yield gain?
str(control_metadata_contraints)
str(control_metadata_contraints$yield_gain)
mean(control_metadata_contraints$yield_gain, na.rm = TRUE)

check <- control_metadata_contraints %>% select(control_yield, yield, yield_gain , relative_yld_change) %>% 
  filter(yield>0.0) %>% 
  filter(control_yield>0.0)

summary_stats <- control_metadata_contraints %>% 
  filter(yield>0.0) %>% 
  group_by() %>% 
  summarise(mean_yld_gain = mean(yield_gain, na.rm = TRUE),
            n = n(),
            sd = sd(yield_gain),
            std_error = sd /sqrt(n))
summary_stats

# # so what might be driving this ?
# # Option 2 coloured by crop type
# control_metadata_contraints %>%
#   filter(!is.na(yield)) %>%
#   ggplot(mapping = aes(control_yield, yield, colour = crop)) +
#   geom_abline(intercept = 0, slope = 1, linetype="dashed", linewidth = 1.5)+
#   geom_point(alpha= 0.4) +
#   #geom_smooth(method = lm, se = FALSE) +
#   scale_x_continuous(breaks=seq(0,10,by=0.5))+
#   scale_y_continuous(breaks=seq(0,10,by=0.5))+
#   theme_bw()+
#   labs(title = "Control yield - all data - no summary\nnote each site, treatment, year and rep is matched to control",
#        x = "control yield t/ha", y = "treatment yield t/ha")
# 
# # Option 3 coloured by crop type as facet
# control_metadata_contraints %>%
#   filter(!is.na(yield)) %>%
#   ggplot(mapping = aes(control_yield, yield)) +
#   geom_abline(intercept = 0,
#               slope = 1,
#               linetype = "dashed",
#               linewidth = 1.5) +
#   geom_point(alpha = 0.4) +
#   geom_smooth(method = lm, se = FALSE) +
#   scale_x_continuous(breaks = seq(0, 10, by = 0.5)) +
#   scale_y_continuous(breaks = seq(0, 10, by = 0.5)) +
#   facet_wrap(. ~ crop) +
#   theme_bw() +
#   labs(title = "Control yield - all data - no summary\nnote each site, treatment, year and rep is matched to control", x = "control yield t/ha", y = "treatment yield t/ha")
# 
# # Option 3 coloured by tillage type as facet
# control_metadata_contraints %>%
#   filter(!is.na(yield)) %>%
#   ggplot(mapping = aes(control_yield, yield)) +
#   geom_abline(intercept = 0,
#               slope = 1,
#               linetype = "dashed",
#               linewidth = 1.5) +
#   geom_point(alpha = 0.4) +
#   geom_smooth(method = lm, se = FALSE) +
#   scale_x_continuous(breaks = seq(0, 10, by = 0.5)) +
#   scale_y_continuous(breaks = seq(0, 10, by = 0.5)) +
#   facet_wrap(. ~ tillage_class) +
#   theme_bw() +
#   labs(title = "Control yield - all data - no summary\nnote each site, treatment, year and rep is matched to control", x = "control yield t/ha", y = "treatment yield t/ha")
