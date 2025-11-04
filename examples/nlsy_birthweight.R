# This example studies the relationship between low birth weight and test scores

# clear environment and load packages
rm(list=ls())
library(tidyverse)
library(fixest)

# Load dataset
load(url("https://github.com/tomvogl/econ121/raw/main/data/nlsy_kids.rds"))

# Decribe the data. Test scores go from 0 to 100, with a mean in the 40s.
summary(nlsy_kids)

# Birth weight is in logs, which is a little complicated to interpret
# Let's convert to ounces
nlsy_kids$bw <- exp(nlsy_kids$lnbw)
# could have used nlsy_kids <- mutate(nlsy_kids, bw =  exp(lnbw))

# Have a look at the summary statistics
# For reference, below 88 ounces is considered low birth weight,
# and below 53 ounces is very low birth weight.
summary(nlsy_kids$bw)

# More information on the quantiles of bw
quantile(nlsy_kids$bw, 
         probs = c(.01, .05, .1, .25, .5, .75, .9, .95, .99),
         na.rm = TRUE)

# Let's give ourselves a sense of how birth weight relates to the
# composite test score by plotting mean test scores by birthweight.
# I create a binned version of birthweight to make the plot less noisy.
nlsy_kids |>
  drop_na(bw, comp_score_11to14) |>
  mutate(bw_bin = case_when(bw>=140 ~ 140, # If birth weight is >= 140, assign 140
                            bw>=130 ~ 130, # If birth weight is bet 130 and 139.99, assign 130
                            bw>=120 ~ 120, # and so on...
                            bw>=110 ~ 110,
                            bw>=100 ~ 100,
                            bw>=90 ~ 90,
                            bw>=80 ~ 80,
                            TRUE ~ 70,)) |> # If birth weight is <80, assign 70
  group_by(bw_bin) |> 
  summarise(mean_test = mean(comp_score_11to14)) |>
  ggplot(aes(x=bw_bin,y=mean_test)) +
    geom_line() +
    geom_point()

# There is a strong relationship! But how much of this is 
# due to family characteristics? Let's generate separate
# plots for mothers with <12, 12, and >12 years of schooling.
nlsy_kids |>
  drop_na(bw, comp_score_11to14, momed) |>
  mutate(momedlevel = case_when(momed<12 ~ "<HS",
                                momed==12 ~ "HS",
                                momed>12 ~ ">HS"),
         bw_bin = case_when(bw>=140 ~ 140, 
                            bw>=130 ~ 130, 
                            bw>=120 ~ 120, 
                            bw>=110 ~ 110,
                            bw>=100 ~ 100,
                            bw>=90 ~ 90,
                            bw>=80 ~ 80,
                            TRUE ~ 70,)) |>
  group_by(bw_bin, momedlevel) |> 
  summarise(mean_test = mean(comp_score_11to14)) |>
  ggplot(aes(x=bw_bin,y=mean_test,color=momedlevel)) +
    geom_line() +
    geom_point()
# Maternal education is clearly associated with test scores. At the same time,
# these plots don't look that much flatter than the full sample plot above.
# How much of the relationship is attributable to maternal characteristics
# rather than child health per se? We will use fixed effects to find out.

# For simplicity, let's generate a very low birth weight indicator, 
# based on the 53 ounce threshold.
nlsy_kids$vlow_bw <- ifelse(nlsy_kids$bw<53,1,0)

# Let's look at the structure of the panel data
nlsy_kids |>
  arrange(mom_id) |> # sort by mom_id so siblings are next to each other
  head() # could have also used glimpse(), which would have rotated the table on its side

# OLS with robust standard errors
feols(comp_score_11to14 ~ vlow_bw, data = nlsy_kids, vcov = 'hetero')

# OLS with clustered standard errors -- this is the correct standard error
feols(comp_score_11to14 ~ vlow_bw, data = nlsy_kids, vcov = ~mom_id)
# SEs are a little larger when we take into account intrafamily correlation.
# But we are still concerned that by including between-family variation
# in our estimation, the coefficient on vlow_bw may be biased.

# First, let's look at the "between" variation - the
# between effects model.
# Let's first create a data frame of family averages
nlsy_families <-
  nlsy_kids |>
    drop_na(comp_score_11to14, vlow_bw) |>
    group_by(mom_id) |>
    summarise(mean_test = mean(comp_score_11to14),
              mean_vlow_bw = mean(vlow_bw))
# Now estimate OLS using the family averages
feols(mean_test ~ mean_vlow_bw, data = nlsy_families, vcov = 'hetero')
# The "between" estimate is very similar to the pooled estimate above

# Now let's look at the "within" variation - the fixed
# effects model:
feols(comp_score_11to14 ~ vlow_bw | mom_id, data = nlsy_kids)
# The estimated coefficient shrinks by about 1/3, consistent with upward bias  
# from between-family variation.

# Let's try adding some control variables. We will add black,
# hispanic, and momed as examples of control variables that 
# DO NOT vary within family. We will add male and first born
# as examples of covariates that DO vary within family. Since
# adding control variables changes the sample size, we will 
# rerun the models without control variables in the smaller
# sample.

# OLS with and without control variables. First, define a subsample with
# non-missing dependent and independent variables.
nlsy_subset <- nlsy_kids |> drop_na(comp_score_11to14, vlow_bw, hispanic, black, momed, male, firstborn)
# Now run regressions  
feols(comp_score_11to14 ~ vlow_bw + hispanic + black +  momed + male + firstborn, data = nlsy_subset, vcov = ~mom_id)
feols(comp_score_11to14 ~ vlow_bw + male + firstborn, data = nlsy_subset, vcov = ~mom_id)
feols(comp_score_11to14 ~ vlow_bw, data = nlsy_subset, vcov = ~mom_id)
# Adding the family-level control variables reduces the estimates.
# Adding just the individual-level control variables doesn't do much.

# FE with and without control variables. The family-level control
# variables are dropped because they are collinear with the mother
# fixed effects. Again, the individual-level control variables
# don't much change the estimates on vlow_bw.
feols(comp_score_11to14 ~ vlow_bw + hispanic + black +  momed + male + firstborn | mom_id, data = nlsy_subset)
feols(comp_score_11to14 ~ vlow_bw + male + firstborn | mom_id, data = nlsy_subset)
feols(comp_score_11to14 ~ vlow_bw | mom_id, data = nlsy_subset)
