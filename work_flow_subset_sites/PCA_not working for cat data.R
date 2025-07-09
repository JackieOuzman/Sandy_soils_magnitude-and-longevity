library(ggplot2)
library(readxl)
library(tidyverse)
library(stringr)

library(dmetar)
library(tidyverse)
library(meta)

library(forcats)



# install.packages("FactoMineR")
# install.packages("vcd")
# install.packages("factoextra")

  
library(FactoMineR)
library(vcd)
library(factoextra)



df <- read.csv("N:/sandy soils conference/data/data for SS prestenation/control_metadata_contraints_tillage_only_subset.csv" )
str(df)


## worked example with factorial data
data(Arthritis)

arthritis_data <- Arthritis[,c(2,3,5)]
arthritis_mca <- MCA(arthritis_data, 
                     ncp = 2, 
                     graph = FALSE)
arthritis_mca
fviz_mca_biplot(arthritis_mca, 
                repel = TRUE, 
                ggtheme = theme_minimal())

arthritis_data2 <- Arthritis[,c(2,3,4,5)]
arthiris_famd <- FAMD(arthritis_data2,
                      ncp = 2, 
                      graph = TRUE)

arthiris_famd 

## worked example with factorial data
test <- df %>% select(relative_yld_change,
                      tillage_class, 
                      amendments_no_amend,
                      Decile_group:post_tillage_group)
summary(df$relative_yld_change)
test <- test %>% 
  mutate(relative_yld_change_group = case_when(
    relative_yld_change < 1.0 ~ "no_change",
    between (relative_yld_change, 1.0001,5.0)  ~ "small_change",
    between (relative_yld_change, 5.0001,26)  ~ "medium_change",
    relative_yld_change > 26.0001  ~ "High_change",
    .default = "no_classed"
  ))
unique(test$relative_yld_change_group)


test_famd <- FAMD(test,
                      ncp = 2, 
                      graph = TRUE)
