# uncomment if these packages are not installed
# install.packages(c('tidyverse','fixest'))

# this script estimates unweighted and weighted averages using data from the
# National Longitudinal Survey of Youth '79

# clear environment and load packages
rm(list=ls())
library(tidyverse)
library(fixest)

# load nlsy79.rda
load(url("https://github.com/tomvogl/econ121/raw/main/data/nlsy79.rds"))

# note that the survey has sampling weights. these are to
# adjust for the fact that the sample was not a simple
# random sample, due to both stratification and nonresponse.
# if p is the probability that a person was sampled,
# then their weight is set proportional to 1/p. the
# actual number is (1/p)*(100), but since all weighted 
# statistics rescale by the sum of the weights, it is
# conceptually the same as weighting by 1/p. we will want
# to use sampling weights to estimate population parameters.
summary(nlsy79$perweight)
# let's plot a histogram of the weight variable
ggplot(nlsy79, aes(x=perweight)) +
  geom_histogram()
# we can see that the distribution is bimodal. this is
# due to minority oversampling. the dispersion around
# each mode is due to non-response.

# let's compare weighted and unweighted averages of labor income
nlsy79 %>%
  drop_na(laborinc18) %>%
  summarize(mean = mean(laborinc18),
            wtmean = weighted.mean(laborinc18, w = perweight),
            numobs = n())
# the weighted average is about 8000 higher than the unweighted! 
# this is because a disadvantaged group was oversampled. the 
# weighted average is the unbiased estimator of the population mean.

# now let's run regressions to see the relationship between performance
# on the AFQT, which is like an IQ test, and labor income.
feols(laborinc18 ~ afqt81, data = nlsy79, vcov = 'hetero')
feols(laborinc18 ~ afqt81, data = nlsy79, vcov = 'hetero', weights = ~perweight)
# a 1-point increase in the AFQT score is associated with $820-$840 more
# in annual earnings. the unweighted and weighted point estimates are similar,
# but the standard error is larger in the weighted regression. the increase
# in standard errors is consistent with a loss of efficiency. since the point
# estimates are similar, we might prefer the more precise unweighted estimator.

# the slope on AFQT is more interpretable if we know something about the 
# distribution of AFQT:
summary(nlsy79$afqt81)
# these are unweighted statistics, but they are at least a quick
# way to understand that the score is scaled between 1 and 99. if we wanted 
# to be fully careful, we would want to compute weighted statistics.
# we can estimate weighted averages using the function weighted.mean().
# tidyverse does not have an analogous weighted.sd() function, 
# so let's create that function from scratch:
weighted_sd <- function(x, w) {
  weighted_mean <- sum(x * w) / sum(w)
  weighted_variance <- sum(w * (x - weighted_mean)^2) / sum(w)
  sqrt(weighted_variance)
}
# now estimate the weighted mean and sd of AFQT
nlsy79 %>%
  drop_na(afqt81) %>%
  summarize(wtmean = weighted.mean(afqt81, w = perweight),
            wtsd = weighted_sd(afqt81,w=perweight),
            numobs = n())
# so a 1-standard deviation increase in AFQT is associated with
# an increase in earnings of 29 times the coefficient!

# now let's try running the same regressions for gender and labor income
feols(laborinc18 ~ male, data = nlsy79, vcov = 'hetero')
feols(laborinc18 ~ male, data = nlsy79, vcov = 'hetero', weights = ~perweight)
# men earn $26,000 - $36,000 more than women. the point estimates are quite 
# different, suggesting that the weighted and unweighted estimators are answering
# different questions. the standard error for the weighted estimator is still
# larger, but we still might be inclined to select the weighted estimator to 
# guarantee representativeness. we face a tradeoff between bias and variance.

# two extra comments. first, if we wanted to proceed with this analysis, we would
# probably want to try to understand *why* the unweighted and weighted estimators
# differ. what heterogeneity is driving this pattern? second, we would typically
# make consistent choices throughout the analysis. so if we chose the weighted 
# estimator for the gender regression, we would probably want to use it for the 
# AFQT regression too. this is a stylistic point and not a scientific one.
