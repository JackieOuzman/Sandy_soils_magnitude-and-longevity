library(ggplot2)
library(readxl)
library(tidyverse)
library(stringr)

library(forcats)
library(FSA) 
library(agricolae)
library(multcomp)
library(lsmeans)
library(multcompView)
library(Rmisc)
library(car)
library(DescTools)

library(dmetar)
library(tidyverse)
library(meta)


## plot and analsyis for preso




df <- read.csv("N:/sandy soils conference/data/All_sites_cleaned/control_metadata_contraints_tillage_only_cleaned.csv" )



# keep only one yield output
str(df)
unique(df$crop_group)
unique(df$tillage_class)

df_modified <- df %>% 
  dplyr::select(tillage_amendments_class,
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

df_modified_summary <- df_modified %>% dplyr::group_by(tillage_amendments_class) %>% 
  dplyr::summarise(n = n(),
            mean = mean(relative_yld_change,na.rm = TRUE),
            sd = sd(relative_yld_change,na.rm = TRUE),
            SE = sd/sqrt(n) )
ungroup(df_modified_summary)

df_modified_total <- df_modified %>% dplyr::group_by() %>%
  dplyr::summarise(
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
                                                         #"Inversion_no_amendment",
                                                         "Combination_no_amendment",
                                                         "Combination_amendment",
                                                         "Mixing_no_amendment", 
                                                         "Mixing_amendment",
                                                         "Ripping_no_amendment",
                                                         "Ripping_amendment" 
                                                         ),
                                                       labels = c(
                                                         "All tillage",
                                                         #"Inversion only",
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
df_modified_summary <- df_modified %>% dplyr::group_by(tillage_class) %>% 
  dplyr::summarise(n = n(),
            mean = mean(relative_yld_change,na.rm = TRUE),
            sd = sd(relative_yld_change,na.rm = TRUE),
            SE = sd/sqrt(n) )
ungroup(df_modified_summary)

df_modified_total <- df_modified %>% dplyr::group_by() %>%
  dplyr::summarise(
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
                                                        # "Inversion",
                                                         "Combination",
                                                         "Ripping",
                                                         "Mixing" 
                                                       ),
                                                       labels = c(
                                                         "Total",
                                                        # "Inversion",
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
  ylab("Mean relative yield change (+/- standard error)") #+
#labs(title = " gain above the control for tillage treatments")
plot2_yld_gain
df_modified_summary

###############################################################################
#### agricolae_LSD

model_sand = lm( relative_yld_change ~ tillage_class,
                 data=df)

agricolae_LSD_output_sand <- (LSD.test(model_sand, "tillage_class",   # outer parentheses print result
                                       alpha = 0.1,      
                                       p.adj="none"))      # see ?p.adjust for options"none" is t-student.

agricolae_LSD_output_sand
#Extract the LSD value from the anlsysis and add it to the summary data

LSD_value_1 <- agricolae_LSD_output_sand$statistics$LSD #this becomes NULL if there is not values
LSD_value_1
###############################################################################
model = lm( relative_yld_change ~ tillage_class,
            data=df)
anova_yld <- Anova(model, type="II") # Can use type="III"
anova_yld


###############################################################################

## meta analysis on tillage
df_modified_summary <- df_modified %>% dplyr::group_by(tillage_class) %>% 
  dplyr::summarise(n = n(),
            mean = mean(relative_yld_change,na.rm = TRUE),
            sd = sd(relative_yld_change,na.rm = TRUE),
            SE = sd/sqrt(n) )
ungroup(df_modified_summary)


df_modified_summary
m.mean <- metamean(n = n,
                   mean = mean,
                   sd = sd,
                   studlab = tillage_class,
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

dim(df_modified_summary)
#how many rows of data ?
forest_plot_input <- data.frame(
  Index = seq(1:5), ## This provides an order to the data
  label = m.mean$studlab,
  SMD = m.mean$TE,
  LL = m.mean$lower,
  UL =  m.mean$upper)
forest_plot_input

forest_plot_input_total <- data.frame(
  Index = (5+1), ## This provides an order to the data
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


df_modified_summary
m.mean <- metamean(n = n,
                   mean = mean,
                   sd = sd,
                   studlab = tillage_class,
                   data = df_modified_summary,
                   sm = "MRAW",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "Option 2")
summary(m.mean)
# Then run the meta-regression
