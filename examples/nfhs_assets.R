# this script estimates the relationship between age and asset 
# ownership in India, using the National Family Health Survey

# clear environment and load packages
rm(list=ls())
library(tidyverse)
library(fixest)

# load nlsy79.rds
load(url("https://github.com/tomvogl/econ121/raw/main/data/nfhs4.rds"))

# filter data to include only 20-80 year old household heads
nfhs4 <- nfhs4 |> filter(age >= 20 & age <= 80)

# note that the survey has sampling weights. these are to
# adjust for the fact that the sample was not a simple
# random sample. subpopulations that were undersampled receive
# larger sampling weights, so that weighted statistics provide
# unbiased estimates of population parameters. if p is the
# probability that a household was included in the sample,
# then that household's weight is proportional to 1/p. the
# actual number is (1/p)*(sample size)*(1 million), but since
# all weighted statistics rescale by the sum of the weights,
# it is conceptually the same as weighting by 1/p. we will want
# to use sampling weights to estimate population parameters.
summary(nfhs4$weight)

# the key variable is the asset score, which indicates
# the household's percentile rank in the national asset 
# distribution.
summary(nfhs4$assets)

# let's plot the asset score by age. first group the data by age.
nfhs4_by_age <- 
  nfhs4 %>% 
  group_by(age) %>%
  summarize(mean = weighted.mean(assets, w=weight))

# now plot the means by age. we see a rising but concave relationship.
ggplot(nfhs4_by_age, aes(x=age, y=mean)) + 
  geom_point()

# estimate a regression with a quadratic, with and without weighting by 
# sampling weight. get slightly diffent coefficients, larger SEs.
# so the weighted estimate is NOT more precise. it is also not
# necessarily the average coefficient in the population. it IS
# an estimate of the answer we would get from a census of the population.
feols(assets ~ age + I(age^2), data = nfhs4, vcov = 'hetero')
feols(assets ~ age + I(age^2), data = nfhs4, vcov = 'hetero', weights = ~weight)

# we should actually estimate this taking the clustered sampling into
# account. villages were sampled, and then individuals within them.
# we need cluster-robust SEs. the SEs become larger, consistent with
# positive intra-cluster correlations.
feols(assets ~ age + I(age^2), data = nfhs4, vcov = ~clustnum, weights = ~weight)

# even though the relationship is non-linear, it may still be
# useful to use a single slope as a summary of a population pattern.
# here it probably makes sense to weight, since the single slope has no
# "structural" interpretation. it is just a description of a pattern in
# the population.
feols(assets ~ age, data = nfhs4, vcov = ~clustnum, weights = ~weight)

# heterogeneity between urban and rural areas using subset
feols(assets ~ age, data = nfhs4, vcov = ~clustnum, weights = ~weight, subset = ~rural==0)
feols(assets ~ age, data = nfhs4, vcov = ~clustnum, weights = ~weight, subset = ~rural==1)

# heterogeneity between urban and rural areas using split (equivalent to above)
feols(assets ~ age, data = nfhs4, vcov = ~clustnum, weights = ~weight, split = ~rural)

# the urban/rural subsamples are independent, so we can compute
# a t-statistic for the difference in slopes "by hand"
(0.001958-0.002472)/sqrt(0.000051^2+0.000089^2)

# alternatively, we can use an interaction term.

#first generate the interaction term
nfhs4 <- nfhs4 |> mutate(ageXrural = age*rural)

# now run the model and save the results as model1 for later
model1 <- feols(assets ~ age + rural + ageXrural, data = nfhs4, vcov = ~clustnum, weights = ~weight)
model1
# same t-statistic, subject to a bit of rounding error in our calculations. 
# assets rise less steeply with age in rural areas.

# we could also do this more elegantly using R's functionality for
# interaction terms, substituting "age * rural" for "age + rural + ageXrural".
# but this syntax is more complicated to use with the hypotheses() function 
# below. just to show you how it's done:
feols(assets ~ age * rural, data = nfhs4, vcov = ~clustnum, weights = ~weight)

# if we wanted to obtain the slope for rural areas from the interacted model,
# we would take a linear combination of the coefficient on age plus coefficient 
# on the ruralXage interaction term. we use the hypotheses() package from the
# marginaleffects package. the fist argument is the name of the estimated 
# model. the second argument is the hypothesis we wish to test. if we specify 
# "age + ageXrural = 0" as our hypothesis, then the function will report back
# the point estimate for the slope, plus the standard error and test statistics
# for the test against 0.
hypotheses(model1, "age + ageXrural = 0")
# same as we got when we estimated a separate regression for rural areas!

# note: in this sample, we could not compute the t-statistic 
# by hand if we were interested in male/female differences in
# slopes. the primary sampling units are clusters, so the male
# and female subsamples are not independent. our only option
# here is to use an interaction term. here we will take advantage
# of R's elegant functionality for interaction terms:
feols(assets ~ age * male, data = nfhs4, vcov = ~clustnum, weights = ~weight)
# no significant difference in slopes between male- and 
# female-headed households!

# now let's suppose we are interested in the ratio of the urban
# slope to the rural slope. we use the delta method, which we can do 
# using the same hypotheses() function as before.
hypotheses(model1,"age / (age + ageXrural) = 0")
# this is nice for interpretation. we can say that the slope in 
# urban areas is 26% higher than the slope in rural areas, and that 
# the proportional difference is statistically significant (because 
# the 95% CI for the ratio excludes 1). we can add a formal test
# of that hypothesis by changing the hypotheses() syntax from
# "= 0" to "= 1":
hypotheses(model1,"age / (age + ageXrural) = 1")
# the p-value is <0.001, confirming out conclusion that the ratio
# is significantly different from 1. note that the estimate is now
# 0.26 rather than 1.26. that's because the function sets the Estimate
# to equal the left-hand side minus the right-hand side.

# Let's get some insight into why the slope is steeper in urban areas.
# First, let's draw the scatter plots by age, just like last time, but
# now separating the sample by urban/rural.
nfhs4_by_age_rural <-
  nfhs4 %>% 
  group_by(age, rural) %>%
  summarize(mean = weighted.mean(assets,w=weight))
ggplot(nfhs4_by_age_rural, aes(x=age, y=mean, color=rural)) +
  geom_point()
# This is not important for class, but you might notice that the color scale for
# rural is continuous. That's because R can't tell it's a categorical
# variable. To change it to a categorical (aka factor) variable, we can use 
# the factor() function as follows.
ggplot(nfhs4_by_age_rural, aes(x=age, y=mean, color=factor(rural))) +
  geom_point()

# The nonlinear functions have different levels but similar shapes! The 
# different linear slopes must be coming from the distribution of age. 
# Let's draw kernel density plots for urban and rural areas. We will
# learn more about density estimation later in the course, but you
# can think of it as a flexible histogram. We use geom_density()
# from ggplot. 'bw' (bandwidth) sets how smooth the density estimator 
# is, and 'alpha' sets the darkness of the area under the curve.
ggplot(nfhs4, aes(x=age, weight = weight, fill=factor(rural))) +
  geom_density(bw = 2, alpha = 0.4)
# Here, you can see that rural areas have more mass at older ages, 
# where the regression function is flatter.