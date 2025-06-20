
names(summary_control_data_all)

summary_control_data_all %>% 
  filter(amendment_all ==  "none"  ) %>% 
  filter(soil_modification == "Rip") %>% 
  #filter(yr_post_amelioration == c(0,1,2,3)) %>% 
  
  ggplot(mapping = aes(yield_gain)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 1, fill = "white") +
  facet_wrap(.~ as.factor(yr_post_amelioration))+
  theme(plot.background = element_rect(fill = "skyblue1"))+
  labs(title = "Yield gains",
       subtitle = "Subset of data Rip only with no soil modification",
       x = "yield gain t/ha", y = "frequency of occurrence")


summary_control_data_all %>% 
  filter(amendment_all ==  "none"  ) %>% 
  filter(soil_modification == "Rip") %>% 
  filter(yr_post_amelioration == c(0,1,2,3)) %>% 
  
  ggplot(mapping = aes(control_yield, yield)) +
  geom_point(alpha= 0.4) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size = 0.5)+
  #geom_smooth(data = no_amendment, method = lm, se = FALSE, colour = "black", size = 0.5) +
  labs(title = "Rip only - Control yield (subset of data with no amendment)",
       subtitle = "note each site, treatment, year and rep is matched to control",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ as.factor(yr_post_amelioration))+
  theme(plot.background = element_rect(fill = "skyblue1"))




#what are the sites with NA years?  
NA_yr_post_amelioration <- summary_control_data_all %>% 
  filter(is.na(yr_post_amelioration))
names(NA_yr_post_amelioration)

distinct(NA_yr_post_amelioration,site )



####################################################################################################################################

summary_control_data_all %>% 
filter(amendment_all ==  "none"  ) %>% 
  filter(soil_modification == "Spade") %>% 
  filter(yr_post_amelioration == c(0,1,2,3)) %>% 
  
  ggplot(mapping = aes(control_yield, yield)) +
  geom_point(alpha= 0.4) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size = 0.5)+
  #geom_smooth(data = no_amendment, method = lm, se = FALSE, colour = "black", size = 0.5) +
  labs(title = "Spade only - Control yield (subset of data with no amendment)",
       subtitle = "note each site, treatment, year and rep is matched to control",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ as.factor(yr_post_amelioration))+
  theme(plot.background = element_rect(fill = "skyblue2"))


summary_control_data_all %>% 
  filter(amendment_all ==  "none"  ) %>% 
  filter(soil_modification == "Spade") %>% 
  #filter(yr_post_amelioration == c(0,1,2,3)) %>% 
  
  ggplot(mapping = aes(yield_gain)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 1, fill = "white") +
  facet_wrap(.~ as.factor(yr_post_amelioration))+
  theme(plot.background = element_rect(fill = "skyblue2"))+
  labs(title = "Yield gains",
       subtitle = "Subset of data Spade only with no soil modification",
       x = "yield gain t/ha", y = "frequency of occurrence")


#########ANIMALS###########################################################################################################################

names(summary_control_data_all)
unique(summary_control_data_all$amendment_code1) #animal
amendment_not_coded_none <- summary_control_data_all %>% 
  filter(amendment_code1 == "none")

unique(amendment_not_coded_none$Descriptors)

amendment_not_coded_check <- summary_control_data_all %>% 
  filter(amendment_code1 == "check")

unique(amendment_not_coded_check$Descriptors)


summary_control_data_all %>%  
  filter(!is.na(yield)) %>% 
  filter(!is.na(control_yield)) %>% 
  
  filter(soil_modification != "Unmodified") %>% 
  
  filter(amendment_code1 == "animal") %>% 
  filter(yr_post_amelioration == c(0,1,2,3)) %>% 
  
  ggplot( mapping = aes(control_yield, yield,)) +
  geom_point(aes(colour= amendment_code1),alpha= 0.4) +
  geom_smooth(method = lm, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Animal amendment only. Control yield - no summary. BUT ANY soil modification \nnote each site, treatment, year and rep is matched to control
      \nFacet wrap is years since amelioration",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ as.factor(yr_post_amelioration))+
  theme(plot.background = element_rect(fill = "skyblue2"))






summary_control_data_all %>%  
  
  filter(!is.na(yield)) %>% 
  filter(!is.na(control_yield)) %>% 
  
  filter(soil_modification != "Unmodified") %>% 
  
  filter(amendment_code1 == "animal") %>% 
  #filter(yr_post_amelioration == c(0,1,2,3)) %>% 
  
  ggplot(mapping = aes(yield_gain)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 1, fill = "white") +
  facet_wrap(.~ as.factor(yr_post_amelioration))+
  theme(plot.background = element_rect(fill = "skyblue2"))+
  labs(title = "Yield gains for Animal amendment only",
       subtitle = "Subset of data amendment with ANY soil modification",
       x = "yield gain t/ha", y = "frequency of occurrence")

