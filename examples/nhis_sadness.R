# analysis of self-reported sadness in the national health interview survey.

# clear environment
rm(list = ls())

# no scientific notation
options(scipen = 999)

# load packages
library(tidyverse)
library(fixest)
library(marginaleffects) # to estimate marginal effects

# read in data
load(url("https://github.com/tomvogl/econ121/raw/main/data/nhis2010.rds"))

# summarize
summary(nhis2010)

# drop observations with sadness missing/NA.
nhis2010 <- nhis2010 |> drop_na(asad)

# generate a variable that equals one if ever sad, zero otherwise.
table(nhis2010$asad)
nhis2010$anysad <- ifelse(nhis2010$asad != "None of the time", 1, 0)
table(nhis2010$anysad)

# SOME DESCRIPTIVE GRAPHS

# first, sadness by education. I will draw the markers proportional
# to the number of observations in each cell to emphasize that there
# are very few individuals with less than 10 years of education.
table_by_edyrs <-
  nhis2010 |>
  drop_na(edyrs) |>
  group_by(edyrs) |>
  summarize(mean_anysad = mean(anysad),
            numobs = n())

ggplot(table_by_edyrs, aes(x = edyrs, y = mean_anysad, size=numobs)) +
    geom_point()

# next, sadness by age and sex. here, I won't keep track of cell size.
table_by_age_sex <-
  nhis2010 |>
  drop_na(age, male) |>
  group_by(age, male) |>
  summarize(mean_anysad = mean(anysad))

ggplot(table_by_age_sex, aes(x = age, y = mean_anysad, color=factor(male))) +
    geom_line()

# finally, sadness by marital status.
table_by_marital <-
  nhis2010 |>
  drop_na(marstat) |>
  group_by(marstat) |>
  summarize(mean_anysad = mean(anysad))

ggplot(table_by_marital, aes(x = marstat, y = mean_anysad)) + 
  geom_bar(stat = "identity")
# "identity" plots the y we specified, instead of a count or proportion.

# COMPARING OLS, LOGIT, AND PROBIT

# estimate a linear probability model, a logit, and a probit.
# then generate predicted probabilities for each of these approaches.
# then compare the predicted probabilities. we use the feglm() function
# from the fixest package.

ols_model <- feols(anysad ~ edyrs + age + male + black + hisp + asian + other + marstat, 
                   data = nhis2010, 
                   vcov = 'hetero')
ols_model
nhis2010$p_ols <- predict(ols_model, nhis2010, type="response")

probit_model <- feglm(anysad ~ edyrs + age + male + black + hisp + asian + other + marstat, 
                      data = nhis2010,
                      vcov = 'hetero', 
                      family = 'probit')
probit_model
nhis2010$p_probit <- predict(probit_model, nhis2010, type="response")

logit_model <- feglm(anysad ~ edyrs + age + male + black + hisp + asian + other + marstat, 
                     data = nhis2010,
                     vcov = 'hetero', 
                     family = 'logit')
logit_model
nhis2010$p_logit <- predict(logit_model, nhis2010, type="response")

# summarize predicted values from all three models - very similar!!
nhis2010 |>
  select(p_ols, p_probit, p_logit) |>
  summary()

# correlation matrix of predicted values from all three models - very close to 1!!
# need to drop_na() because otherwise cor() returns an error
nhis2010 |>
  select(p_ols, p_probit, p_logit) |>
  drop_na(p_ols, p_probit, p_logit) |>
  cor()

# note: the above comparisons of the three predictions are NOT something
#       I will ask you to do on problem sets and exams.

# MARGINAL EFFECTS

# now let's compute average marginal effects. we use the avg_slopes()
# function from the marginaleffects package. note how similar they
# are to the linear probability model, which is already expressed
# in marginal effects.
avg_slopes(probit_model)
avg_slopes(logit_model)
# note if R is using scientific notation, you can turn it off using:
# options(scipen = 999)


# ODDS RATIOS

# finally, we can also estimate odds ratios in the logit setting.
# the simplest way to obtain them is:
exp(coef(logit_model))
# and for the 95% confidence intervals:
exp(confint(logit_model))
# we can save them as objects and use cbind() to bind them together:
oddsratio <- exp(coef(logit_model))
ci <- exp(confint(logit_model))
cbind(oddsratio, ci)
# to obtain standard errors, we would need to use the delta method.
# but usually researchers test hypotheses using the original logit 
# coefficient and standard error, not the transformed odds ratio.

# these ORs are especially convenient for binary independent variables.
# for instance, men have 32% lower odds of reporting sadness than women.