library(tidyverse)

#https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/forest.html

# 3.2 Measures & Effect Sizes in Observational Designs--------------------------


# Set seed of 123 for reproducibility 
# and take a random sample (n=50).
set.seed(123)
sample <- rnorm(n = 50, mean = 20, sd = 5)

# Calculate the mean
mean(sample)


# Calculate the standard error
sd(sample)/sqrt(50)

# 3.3 Proportions -----------

# To calculate a proportion p , we have to divide the number of individuals k
# falling into a specific subgroup by the total sample size n

# We define the following values for k and n:
k <- 25
n <- 125

# Calculate the proportion
p <- k/n
p

# Calculate the standard error
sqrt((p*(1-p))/n)



# 3.2.3 Correlations ------------------------------------------------

# Simulate two continuous variables x and y
set.seed(12345)
x <- rnorm(20, 50, 10)
y <- rnorm(20, 10, 3)

# Calculate the correlation between x and y
r <- cor(x,y)
r
# Calculate Fisher's z
z <- 0.5*log((1+r)/(1-r))
z


# 3.3 Effect Sizes in Experimental Designs -----------
## 3.3.1.1 Between-Group Mean Difference --------------

# Generate two random variables with different population means
set.seed(123)
x1 <- rnorm(n = 20, mean = 10, sd = 3)
x2 <- rnorm(n = 20, mean = 15, sd = 3)

# Calculate values we need for the formulas
s1 <- sd(x1)
s2 <- sd(x2)
n1 <- 20
n2 <- 20


# Calculate the mean difference
MD <- mean(x1) - mean(x2)
MD


# Calculate s_pooled
s_pooled <- sqrt(
  (((n1-1)*s1^2) + ((n2-1)*s2^2))/
    ((n1-1)+(n2-1))
)

# Calculate the standard error
se <- s_pooled*sqrt((1/n1)+(1/n2))
se

# 3.3.1.2 Between-Group Standardized Mean Difference --------------------------
# Load esc package
install.packages("esc")
library(esc)

# Define the data we need to calculate SMD/d
# This is just some example data that we made up
grp1m <- 50   # mean of group 1
grp2m <- 60   # mean of group 2
grp1sd <- 10  # sd of group 1
grp2sd <- 10  # sd of group 2
grp1n <- 100  # n of group1
grp2n <- 100  # n of group2

# Calculate effect size
esc_mean_sd(grp1m = grp1m, grp2m = grp2m, 
            grp1sd = grp1sd, grp2sd = grp2sd, 
            grp1n = grp1n, grp2n = grp2n)

# 3.3.1.3 Within-Group (Standardized) Mean Difference --------------------------
# Define example data needed for effect size calculation
x1 <- 20    # mean at t1
x2 <- 30    # mean at t2
sd1 <- 13   # sd at t1
n <- 80     # sample size
r <- 0.5    # correlation between t1 and t2

# Caclulate the raw mean difference
md_within <- x2 - x1

# Calculate the smd:
# Here, we use the standard deviation at t1
# to standardize the mean difference
smd_within <- md_within/sd1
smd_within

# Calculate standard error
se_within <- sqrt(((2*(1-r))/n) + 
                    (smd_within^2/(2*n)))
se_within


3.3.2 Risk & Odds Ratios -----------------------------------------------------
#Not for my data
  
# As it says in the name, a risk ratio (also known as the relative risk) is 
# a ratio of two risks. Risks are essentially proportions (see Chapter 3.2.2). 
# They can be calculated when we are dealing with binary, or dichotomous, outcome data.

# 3.4 Effect Size Correction ---------------------------------------------------
##  3.4.1 Small Sample Bias ----------------------------------------------------
# Define uncorrected SMD and sample size n

SMD <- 0.5
n <- 30

# Convert to Hedges g
g <- hedges_g(SMD, n)
g


# 3.4.2 Unreliability ---
# Define uncorrected correlation and SMD with their standard error
r_xy <- 0.34
se_r_xy <- 0.09
smd <- 0.65
se_smd <- 0.18

# Define reliabilities of x and y
r_xx <- 0.8
r_yy <- 0.7

# Correct SMD for unreliability in x
smd_c <- smd/sqrt(r_xx)
smd_c

se_c <- se_smd/sqrt(r_xx)
se_c
# Correct correlation for unreliability in x and y
r_xy_c <- r_xy/(sqrt(r_xx)*sqrt(r_yy))
r_xy_c
se_c <- se_r_xy/(sqrt(r_xx)*sqrt(r_yy))
se_c

# 3.4.3 Range Restriction -------
# Define correlation to correct
r_xy <- 0.34
se_r_xy <- 0.09

# Define restricted and unrestricted SD
sd_restricted <- 11
sd_unrestricted <- 18

# Calculate U
U <- sd_unrestricted/sd_restricted

# Correct the correlation
r_xy_c <- (U*r_xy)/sqrt((U^2-1)*r_xy^2+1)
r_xy_c
# Correct the standard error
se_r_xy_c <- (r_xy_c/r_xy)*se_r_xy
se_r_xy_c



# 4.1 The Fixed-Effect and Random-Effects Model ---
# 4.1.1 The Fixed-Effect Model

# Load dmetar, esc and tidyverse (for pipe)
#devtools::install_github("MathiasHarrer/dmetar")
#install.packages("dmetar") #not working
library(dmetar)
library(esc)
library(tidyverse)
#install.packages("meta")
library(meta)

# Load data set from dmetar
data(SuicidePrevention)


# Calculate Hedges' g and the Standard Error
# - We save the study names in "study".
# - We use the pmap_dfr function to calculate the effect size
#   for each row.
SP_calc <- pmap_dfr(SuicidePrevention, 
                    function(mean.e, sd.e, n.e, mean.c,
                             sd.c, n.c, author, ...){
                      esc_mean_sd(grp1m = mean.e,
                                  grp1sd = sd.e,
                                  grp1n = n.e,
                                  grp2m = mean.c,
                                  grp2sd = sd.c,
                                  grp2n = n.c,
                                  study = author,
                                  es.type = "g") %>% 
                        as.data.frame()}) 

# Let us catch a glimpse of the data
# The data set contains Hedges' g ("es") and standard error ("se")
glimpse(SP_calc)

# Calculate the inverse variance-weights for each study
SP_calc$w <- 1/SP_calc$se^2

# Then, we use the weights to calculate the pooled effect
pooled_effect <- sum(SP_calc$w*SP_calc$es)/sum(SP_calc$w)
pooled_effect


# 4.1.2 The Random-Effects Model ----



data(ThirdWave)
glimpse(ThirdWave)


m.gen <- metagen(TE = TE,
                 seTE = seTE,
                 studlab = Author,
                 data = ThirdWave,
                 sm = "SMD",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 method.random.ci = "HK",
                 title = "Third Wave Psychotherapies")
summary(m.gen)
m.gen$TE.random
m.gen$TE.fixed


# Use metcont to pool results.
m.cont <- metacont(n.e = n.e,
                   mean.e = mean.e,
                   sd.e = sd.e,
                   n.c = n.c,
                   mean.c = mean.c,
                   sd.c = sd.c,
                   studlab = author,
                   data = SuicidePrevention,
                   sm = "SMD",
                   method.smd = "Hedges",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "Suicide Prevention")
summary(m.cont)

str(DepressionMortality)

m.bin <- metabin(event.e = event.e, 
                 n.e = n.e,
                 event.c = event.c,
                 n.c = n.c,
                 studlab = author,
                 data = DepressionMortality,
                 sm = "RR",
                 method = "MH",
                 MH.exact = TRUE,
                 fixed = TRUE,
                 random = TRUE,
                 method.tau = "PM",
                 method.random.ci = "HK",
                 title = "Depression and Mortality")
summary(m.bin)

# 4.2.5 Means I think this is closest to what I will do----

glimpse(BdiScores[,1:4])
m.mean <- metamean(n = n,
                   mean = mean,
                   sd = sd,
                   studlab = author,
                   data = BdiScores,
                   sm = "MRAW",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "BDI-II Scores")

summary(m.mean)
meta::forest(m.mean, 
             sortvar = TE,
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftlabs = c("Author", "g", "SE"))


meta::forest(m.mean, layout = "JAMA")
meta::forest(m.mean, layout = "RevMan5")


png(file = "forestplot.png", width = 2800, height = 2400, res = 300)

meta::forest(m.mean, 
             sortvar = TE,
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftlabs = c("Author", "g", "SE"))

dev.off()
