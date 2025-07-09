library(ggplot2)
library(readxl)
library(tidyverse)
library(stringr)

#library(dmetar)

#library(meta)

library(forcats)
library(FSA) 
library(agricolae)
library(multcomp)
library(lsmeans)
library(multcompView)
library(Rmisc)
library(car)
library(DescTools)


df <- read.csv("N:/sandy soils conference/data/data for SS prestenation/control_metadata_contraints_tillage_only_subset.csv" )
str(df)
unique(df$tillage_class)
order <- c("Mixing", "Ripping",  "Combination", "Inversion" )

df$tillage_class <- factor(df$tillage_class,levels = order)

###############################################################################
data_summary_all_analysis <- df %>% 
  dplyr::group_by(tillage_class) %>%
  dplyr::summarise(mean=mean(relative_yld_change, na.rm = TRUE), 
                   sd=sd(relative_yld_change, na.rm = TRUE),
                   count = n(),
                   std_error = sd/(sqrt(count))
                   
  ) %>%
  arrange(desc(mean))

data_summary_all_analysis
###############################################################################
#### ANOVA #####

model = lm( relative_yld_change ~ tillage_class,
            data=df)

anova_yld <- Anova(model, type="II") # Can use type="III"
anova_yld
p_value_ANOVA <- anova_yld[1,4]
F_value_ANOVA <- anova_yld[1,3]

### add these values into the summary data

data_summary_all_analysis <- data_summary_all_analysis %>% 
  mutate(p_value_ANOVA  = p_value_ANOVA,
         F_value_ANOVA  = F_value_ANOVA)

#Add in the significance ***
data_summary_all_analysis <- data_summary_all_analysis %>%
  mutate(ANOVA_sign_0.1 = case_when(
    p_value_ANOVA < 0.001 ~ "****",
    p_value_ANOVA <= 0.01 ~  "***",
    p_value_ANOVA <= 0.05 ~  "**",
    p_value_ANOVA <= 0.1 ~  "*",
    p_value_ANOVA >  0.1 ~  "ns",
    TRUE ~ "check"
    
  ))
data_summary_all_analysis


########################################################################################################################################################
#### agricolae_LSD


model_sand = lm( relative_yld_change ~ tillage_class,
                 data=df)

agricolae_LSD_output_sand <- (LSD.test(model_sand, "tillage_class",   # outer parentheses print result
                                       alpha = 0.1,      
                                       p.adj="none"))      # see ?p.adjust for options"none" is t-student.



agricolae_LSD_output_sand
#Extract the LSD value from the anlsysis and add it to the summary data

LSD_value_1 <- agricolae_LSD_output_sand$statistics$LSD #this becomes NULL if there is not values


#get the 'max value' aka make it a value and make a df 
LSD_value_1 <- max(LSD_value_1)
LSD_value_df <- data.frame(LSD_value_1)

LSD_value_df <- LSD_value_df %>% 
  dplyr::mutate(
    LSD_max = case_when(
      LSD_value_1 > 0 ~ as.character(LSD_value_1),
      TRUE ~ "not reported"
    ))


LSD <- LSD_value_df[1,2]
LSD
#Extract the LSD letters from the anlsysis and add it to the summary data

agricolae_LSD_output_sand_df <- as.data.frame(agricolae_LSD_output_sand[[5]]) #get the fith item in the list
agricolae_LSD_output_sand_df
agricolae_LSD_output_sand_df$tillage_class <- rownames(agricolae_LSD_output_sand_df) #move rwo names into a clm for joining

agricolae_LSD_output_sand_df <- agricolae_LSD_output_sand_df %>% 
  mutate(LSD = LSD)

data_summary_all_analysis <- left_join(data_summary_all_analysis,agricolae_LSD_output_sand_df)
str(data_summary_all_analysis)
data_summary_all_analysis <- dplyr::select(data_summary_all_analysis, -relative_yld_change) %>% 
  dplyr::rename(groups_LSD = groups)


#################################################################################
#### agricolae_HSD.test

tukey_agricolae <- (HSD.test(model_sand, "tillage_class", alpha = 0.1))

# I want to access the groups but its part of a list


tukey_agricolae_df <- as.data.frame(tukey_agricolae[[5]]) #get the fith item in the list
tukey_agricolae_df$tillage_class <- rownames(tukey_agricolae_df) #move row names into a clm for joining

data_summary_all_analysis <- left_join(data_summary_all_analysis,tukey_agricolae_df)
data_summary_all_analysis <- dplyr::select(data_summary_all_analysis, -relative_yld_change) %>% 
  dplyr::rename(groups_HSD_Tukey = groups)



################################################################################
#### Plots 
ANOVA_results <- data_summary_all_analysis
str(ANOVA_results)

### make a new clm for plotting letters if the ANOVA is significant
ANOVA_results <- ANOVA_results %>% 
  dplyr::mutate(groups_LSD_display = case_when(
    ANOVA_sign_0.1 == "ns" ~ "",
    TRUE ~ groups_LSD  ))

ANOVA_results



ANOVA_results <- ANOVA_results %>% 
  dplyr::mutate(LSD_display = case_when(
    ANOVA_sign_0.1 == "ns" ~ paste0("") ,
    TRUE ~ paste0("LSD = ", signif(LSD, digits = 4))))



# barplot with letters from LSD
plot_LSD <- ANOVA_results %>%  
  ggplot( aes(x = factor(tillage_class), y = mean)) + 
  geom_bar(stat = "identity",  alpha = 0.5)  +
  geom_errorbar(aes(ymin=mean-std_error, ymax=mean+std_error), width = 0.1) +
  theme_classic() + 
  
  #scale_y_continuous(breaks=seq(0,max_yld,by=0.5), limits = c(0, max_yld))+
  
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.1, 0.75)) +
  
  geom_text(aes(label=groups_LSD_display), 
            position = position_dodge(0.80), 
            size = 3,
            vjust=-0.5, hjust=-0.3, 
            colour = "gray25")+
  
  #theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(
    axis.text.x=element_text(angle=45,hjust=1, size = 14),
    axis.text.y=element_text(size = 14),
    plot.title = element_text(size = 20))+
  labs(x="", 
       y="Yield t/ha", 
       #title = paste0(a,": ", b),
       #subtitle = ANOVA_results$LSD_display
       ) 
plot_LSD


