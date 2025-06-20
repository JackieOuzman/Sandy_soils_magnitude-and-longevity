ggplot(data = summary_control_data_all, mapping = aes(control_yield, yield,)) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", colour = "red", size = 1.8)+
  geom_point(alpha= 0.4) +
  geom_smooth(method = lm, se = FALSE) +
  scale_x_continuous(breaks=seq(0,10,by=0.5))+
  scale_y_continuous(breaks=seq(0,10,by=0.5))+
  theme_bw()#+
  #labs(title = "Control yield - all data - no summary\nnote each site, treatment, year and rep is matched to control",
       x = "control yield t/ha", y = "treatment yield t/ha")


#######################################################################################################################

summary_control_data_all %>% 
  filter(amendment_all ==  "none"  ) %>% 
  filter(soil_modification != "Unmodified") %>% 
  
  
  ggplot(mapping = aes(control_yield, yield)) +
  geom_point(aes(colour= soil_modification),alpha= 0.4) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size = 0.5)+
  geom_smooth(data = summary_control_data_all %>% 
                filter(amendment_all ==  "none"  ) %>% 
                filter(soil_modification != "Unmodified"), 
              
              
              method = lm, se = FALSE, colour = "black", size = 0.5) +
  
  
  
  labs(title = "No amendment",
       #subtitle = "note each site, treatment, year and rep is matched to control",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ soil_modification)+
  theme(plot.background = element_rect(fill = "#BFD5E3"),
        legend.position="none")


names(summary_control_data_all)

summary_control_data_all %>% 
  filter(amendment_all ==  "none"  ) %>% 
  filter(soil_modification != "Unmodified") %>% 
  group_by(soil_modification) %>% 
  summarise(mean_yield_gain = round(mean(yield_gain, na.rm = TRUE),2),
            count = n())
  
#######################################################################################################################

summary_control_data_all %>% 
  filter(amendment_all !=  "none"  ) %>% 
  filter(soil_modification != "Unmodified") %>% 
  
  
  ggplot(mapping = aes(control_yield, yield)) +
  geom_point(aes(colour= soil_modification),alpha= 0.4) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size = 0.5)+
  geom_smooth(data = summary_control_data_all %>% 
                filter(amendment_all !=  "none"  ) %>% 
                filter(soil_modification != "Unmodified"), 
              
              
              method = lm, se = FALSE, colour = "black", size = 0.5) +
  
  
  
  labs(#title = "Amendment used with soil modification",
       #subtitle = "note each site, treatment, year and rep is matched to control",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ soil_modification)+
  theme(plot.background = element_rect(fill = "#BFD5E3"),
        legend.position="none")


names(summary_control_data_all)

summary_control_data_all %>% 
  filter(amendment_all !=  "none"  ) %>% 
  filter(soil_modification != "Unmodified") %>% 
  group_by(soil_modification) %>% 
  summarise(mean_yield_gain = round(mean(yield_gain, na.rm = TRUE),2),
            count = n())

#######################################################################################################################


summary_control_data_all$amendment_code1 <-
  factor(summary_control_data_all$amendment_code1,
         levels = c("animal", "plant", "fertiliser","non organic", "mixed" ))
unique(summary_control_data_all$amendment_code1)

summary_control_data_all %>%  
  filter(amendment_code1 != "none") %>%  
  #filter(amendment_code1 != "non organic") %>% 
  #filter(amendment_code1 != "mixed" ) %>% 
  
  filter(!is.na(yield)) %>% 
  filter(!is.na(control_yield)) %>% 
  
  ggplot( mapping = aes(control_yield, yield)) +
  geom_point(aes(colour= amendment_code1),alpha= 0.4) +
  geom_smooth(method = lm, se = FALSE, na.rm = TRUE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(
  #title = "Control yield - all data - no summary\nnote each site, treatment, year and rep is matched to control
  #  \nFacet wrap is filtered data amendments",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ amendment_code1)


summary_control_data_all %>%  
  filter(amendment_code1 != "none") %>%  
  #filter(amendment_code1 != "non organic") %>% 
  #filter(amendment_code1 != "mixed" ) %>% 
  
  filter(!is.na(yield)) %>% 
  filter(!is.na(control_yield)) %>% 
  group_by(amendment_code1) %>% 
  summarise(mean_yield_gain = round(mean(yield_gain, na.rm = TRUE),2),
            count = n())


#######################################################################################################################


summary_control_data_all$amendment_code1 <-
  factor(summary_control_data_all$amendment_code1,
         levels = c("animal", "plant", "fertiliser","non organic", "mixed" ))
unique(summary_control_data_all$amendment_code1)

summary_control_data_all %>%  
  filter(amendment_code1 != "none") %>%  
  #filter(amendment_code1 != "non organic") %>% 
  #filter(amendment_code1 != "mixed" ) %>% 
  
  filter(soil_modification == "Unmodified") %>% 
  
  filter(!is.na(yield)) %>% 
  filter(!is.na(control_yield)) %>% 
  
  ggplot( mapping = aes(control_yield, yield)) +
  geom_point(aes(colour= amendment_code1),alpha= 0.4) +
  geom_smooth(method = lm, se = FALSE, na.rm = TRUE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(
    #title = "Control yield - all data - no summary\nnote each site, treatment, year and rep is matched to control
    #  \nFacet wrap is filtered data amendments",
    x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ amendment_code1)


summary_control_data_all %>%  
  filter(amendment_code1 != "none") %>%  
  #filter(amendment_code1 != "non organic") %>% 
  #filter(amendment_code1 != "mixed" ) %>% 
  
  filter(soil_modification == "Unmodified") %>% 
  
  filter(!is.na(yield)) %>% 
  filter(!is.na(control_yield)) %>% 
  group_by(amendment_code1) %>% 
  summarise(mean_yield_gain = round(mean(yield_gain, na.rm = TRUE),2),
            count = n())
#######################################################################################################################

# No amendemnts
names(summary_control_data_all)

test <- summary_control_data_all %>%
  filter(!is.na(yr_post_amelioration )) %>% 
  #filter(yr_post_amelioration == c(0,1,2,3)) %>% 
  filter(amendment_all ==  "none"  ) %>% 
  filter(soil_modification != "Unmodified") %>% 
  group_by(soil_modification, yr_post_amelioration) %>% 
  summarise(mean_yield_gain = round(mean(yield_gain, na.rm = TRUE),2),
            count = n())
test

summary_control_data_all %>%
  filter(!is.na(yr_post_amelioration )) %>% 
  filter(amendment_all ==  "none"  ) %>% 
  filter(soil_modification != "Unmodified") %>% 
  #filter(yr_post_amelioration == c(0,1,2,3)) %>% 
  
ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  #geom_point(alpha = 0.1) +
  geom_boxplot(outlier.shape = NA,alpha = 0.2 )+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  facet_wrap(.~soil_modification)+
  labs(
      x = "years post amelioration", y = "yield gain t/ha")
  

# data_for_box_plots <- summary_control_data_all %>%
#   filter(!is.na(yr_post_amelioration )) %>% 
#   filter(amendment_all ==  "none"  ) %>% 
#   filter(soil_modification != "Unmodified")
# 
# data_for_box_plots %>% 
# ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
#   #geom_point(alpha = 0.1) +
#   geom_boxplot(outlier.shape = NA,alpha = 0.2,  )+
#   
#   facet_wrap(.~soil_modification)+
#   labs(
#     x = "years post amelioration", y = "yield gain t/ha")

#######################################################################################################################
str(summary_control_data_all$yr_post_amelioration)

error_check <- summary_control_data_all %>%
  filter(!is.na(yr_post_amelioration )) %>% 
  #filter(yr_post_amelioration == c(0,1,2,3)) %>%
  filter(amendment_all ==  "none"  ) %>% 
  filter(soil_modification == "Spade") %>% 
  filter(soil_modification != "Unmodified") %>% 
  select(soil_modification, ID, Descriptors, yield_gain, Amelioration_Year, yr_post_amelioration)  
  
error_check <- error_check %>%  filter(yr_post_amelioration == 0 | yr_post_amelioration == 1)



error_check_2 <- summary_control_data_all %>%
  filter(!is.na(yr_post_amelioration )) %>% 
  #filter(yr_post_amelioration == c(0,1,2,3)) %>%
  filter(amendment_all ==  "none"  ) %>% 
  filter(soil_modification == "Spade") %>% 
  filter(soil_modification != "Unmodified") %>% 
  select(soil_modification, ID, Descriptors, yield_gain, Amelioration_Year, yr_post_amelioration) %>% 
  group_by(soil_modification, yr_post_amelioration) %>% 
  summarise(mean_yield_gain = round(mean(yield_gain, na.rm = TRUE),2),
            count = n())
error_check_2

names(error_check)
