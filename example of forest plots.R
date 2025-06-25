#Forest plots in R
#https://rpubs.com/mbounthavong/forest_plots_r


## Load libraries
library(gridExtra)
library(ggplot2)

library(dplyr)
library(rcompanion)
install.packages(rcompanion)

## Example data frame
dat <- data.frame(
  Index = c(1, 2, 3, 4), ## This provides an order to the data
  label = c("Age (65 and older versus <65)", "Male versus Female", "High income versus Low income", "High school or higher versus No High school"),
  OR = c(1.00, 2.00, 3.00, 0.50),
  LL = c(0.25, 0.90, 2.25, 0.2),
  UL = c(1.75, 3.10, 3.75, 0.8),
  CI = c("0.25, 1.75", "0.90, 3.10", "2.25, 3.75", "0.20, 0.80")
)
dat


## Plot forest plot
plot1 <- ggplot(dat, aes(y = Index, x = OR)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks=1:4, labels = dat$label, trans = "reverse") +
  xlab("Odds Ratio (95% CI)") + 
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




# test with my data.

example_forest <- control_metadata_contraints %>%
  group_by(tillage_class) %>%
  summarise(
    mean = mean(yield_gain, na.rm = TRUE),
    lower_ci = t.test(yield_gain, conf.level = 0.95, na.rm = TRUE)$conf.int[1],
    upper_ci = t.test(yield_gain, conf.level = 0.95, na.rm = TRUE)$conf.int[2]
  )



example_forest <- example_forest %>%
  mutate(tillage_class_factor = factor(
    tillage_class,
    levels =
      c("Unmodified", "Other", "Ripping_Mixing", "Ripping", "Mixing" ),
    labels = c("Unmodified", "Other", "Ripping and Mixing", "Ripping", "Mixing" )
  ))

str(example_forest)

plot2 <- example_forest %>% 
  ggplot(aes(y = tillage_class_factor, x = mean)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  xlab("Yield gain (95% CI)") + 
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
plot2






