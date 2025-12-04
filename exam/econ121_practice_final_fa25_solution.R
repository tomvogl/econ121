# Solution to Practice Final Exam FA25

# Clear environment
rm(list = ls())

# Load all packages from the course
library(tidyverse)
library(fixest)
library(marginaleffects)

# Load dataset
load(url("https://github.com/tomvogl/econ121/raw/main/data/migration.rds"))

#####
# 1 #
#####
# We need to include subdistrict dummies because the probability
# of receiving a visa varies across subdistricts. We need to cluster
# standard errors at the locality level because the data were collected.

# Create factor version of subdistrict so we do not have to keep on
# writing factor(subdistrict).
migration <- migration %>% mutate(subdistrict.f = factor(subdistrict))

# Run regression of post-lottery ln(income) on winner and subdistrict dummies.
feols(ln_income_post ~ winner + subdistrict.f,
      data = migration,
      vcov = ~locality)

#####
# 2 #
#####
# The coefficient on winner is 0.51, implying that winning a visa 
# raises income by 51 percent. The t-statistic is 13.7, and the p
# value is less than 0.001, so the effect is statistically significant.

#####
# 3 #
#####
# Run a regression of pre-lottery ln(income) on winner and subdistrict dummies.
feols(ln_income_pre ~ winner + subdistrict.f,
      data = migration,
      vcov = ~locality)

#####
# 4 #
#####
# Run the same regression as in 1 but with predetermined covariates.
feols(ln_income_post ~ winner + muslim + age + literate + edyrs + height + 
                       children + ln_income_pre + married_pre + subdistrict.f,
      data = migration,
      vcov = ~locality)

#####
# 5 #
#####
# There were no significant differences between winners and 
# losers in average log income before the lottery. Furthermore, the 
# reduced form regression is robust to the inclusion of all predetermined 
# covariates. This suggests that the lottery was effectively randomized, 
# and there are no concerns about selection bias.

#####
# 6 #
#####
# Re-estimate the previous model and store it in memory with a name.
model1 <-
  feols(ln_income_post ~ winner + muslim + age + literate + edyrs + height + 
                         children + ln_income_pre + married_pre + subdistrict.f,
        data = migration,
        vcov = ~locality)

# Now use the hypotheses() function to compute a linear combo of coefs.
hypotheses(model1, "winner - literate = 0")

#####
# 7 #
#####
# The critic's parallel trends assumption suggests a difference-in-
# differences design. The data format lends itself to a first-
# differences formulation: regress the change in ln(income) on the 
# change in winner status. Since nobody was a winner in the pre-lottery
# period, this is equivalent to regressing the change in ln(income) on 
# the winner dummy.
feols(ln_income_post - ln_income_pre ~ winner,
      data = migration,
      vcov = ~locality)

#####
# 8 #
###### 
# Linear probability model.
feols(migrated ~ winner + subdistrict.f,
      data = migration,
      vcov = ~locality)

# For the probit and logit, we should compute marginal effects because we
# wish to compare the results with the linear probability model. The syntax
# for computing cluster-robust standard errors in the mfx package is complicated
# so it was fine to use iid standard errors here.
probit_model <-
  feglm(migrated ~ winner + subdistrict.f,
        data = migration,
        vcov = ~locality,
        family = 'probit')
avg_slopes(probit_model)

logit_model <-
  feglm(migrated ~ winner + subdistrict.f,
        data = migration,
        vcov = ~locality,
        family = 'logit')
avg_slopes(logit_model)

#####
# 9 #
#####
# All three models find marginal effects of 0.58, implying that winning 
# a visa raises international migration by 58 percentage points. In all 
# 3 cases, the p value is less than 0.001, so the effect is highly 
# statistically significant. In the language of instrumental variables,
# the regression is the first-stage regression.

######
# 10 #
######
# To estimate the IV effect using the previous questions' results:

# IV coef = reduced form coef / first stage coef
# => IV coef = Q1 coef / Q8 coef

# Using the LPM first stage (Q8), the IV estimator of the effect of migrating on income is
0.512366/0.576345

# Note: Probit and logit results do not apply to the IV framework,
# especially not in the unscaled version without looking at marginal effects

######
# 11 #
######
# Compute the IV estimator using TSLS. We should use the same choices
# of control variables and standard errors as we used in the first stage
# and reduced form regressions.
feols(ln_income_post ~ subdistrict.f | migrated ~ winner,
      data = migration,
      vcov = ~locality)

######
# 12 #
######
# The estimate in question 11 is 0.89 log points, implying that migrating
# causes an 89% increase in income. The effect is significant, with a 
# p-value less than 0.001.

# The TSLS estimate in question 11 is very similar to the ratio in question
# 10, but not exactly the same. If they were computed using the same sample,
# they would be identical, but the first-stage and reduced-form regressions
# were estimated on slightly larger samples than the TSLS regression.

######
# 13 #
######
# To assess whether never takers and always takers exist in this 
# context, we need to figure out whether any lottery losers
# migrate (always takers) and whether any lottery winners do
# not migrate (never takers).

# On way to do this is to re-run the first-stage regression 
# without subdistrict dummies, so that intercept is meaningful.
feols(migrated ~ winner,
      data = migration,
      vcov = ~locality)
# Or we can create a table
migration |>
  drop_na(migrated) |>
  group_by(winner) |>
  summarize(migration_rate = mean(migrated))
# You only needed to do one of these.

######
# 14 #
######
# The results from question 13 suggest that 19 percent of the 
# lottery losers migrated, so there are always takers. Additionally,
# 4.5 percent of the winners migrated, so there are never takers. 
# So we should call the effect in question 1 a "reduced form" effect.
# If there weren't always takers, we could have said "intent to treat."

######
# 15 #
######
# The presence of always takers implies that we can possibly interpret
# the effect as a local average treatment effect. This is the average 
# effect of migrating among workers who migrate if and only if they 
# win the lottery.

######
# 16 #
######
# For the LATE interpretation, we need 3 assumptions:
# 1. Independence, i.e. that winning the lottery is randomly assigned. 
#    The estimates in questions 4 and 5 provide some evidence that the 
#    lottery outcome is unrelated to predetermined variables
# 2. Exclusion restriction, i.e. that winning the lottery only affects
#    income through its effect on migrating. We do not have evidence on
#    the validity of this assumptiuon.
# 3. Monotonicity, i.e. that nobody migrates when they LOSE the lottery
#    but does not migrate when they WIN the lottery. We do not have
#    evidence on the validity of this assumption.

######
# 17 #
######
# To generate the pre-lottery income quartile variable, you could have checked
summary(migration$ln_income_pre)
# and then generated the quartile variable from the 1st, median, and 3rd.
# I will instead use ntile(), which is a little simpler if you know it.
migration <- 
  migration |>
  mutate(quartile = ntile(ln_income_pre, 4))
# Now we need to estimate the means of ln_income_post and migrated by quartile, 
# separately for winners and losers:
means_table <-
  migration |>
  drop_na(quartile, ln_income_post, migrated) |>
  group_by(quartile, winner) |>
  summarize(mean_ln_income = mean(ln_income_post),
            mean_migrated = mean(migrated)) 
# Now we need to plot these means by quartile, separately for
# winners and losers:
ggplot(means_table, aes(x = quartile, y = mean_ln_income, group = winner, color = factor(winner))) +
  geom_point() + 
  geom_line()
ggplot(means_table, aes(x = quartile, y = mean_migrated, group = winner, color = factor(winner))) +
  geom_point() + 
  geom_line()

######
# 18 #
######
# The first graph, with mean ln(income) on the vertical axis, 
# represents the "reduced form" effect for each quartile.
# The second graph, with migration on the vertical axis,
# represents the "first stage" effect for each quartile.
# Since the reduced form effect shrinks across quartiles and the 
# first stage effect grows across quartiles, this implies that  
# the IV estimated return to migration shrinks across quartiles.

# We cannot test for differences in returns by comparing 
# coefficients and standard errors across separate models
# for the 1st and 4th quintiles. The sampling design implies
# that individuals are not independent within locality, so 
# the test would need to take into account the covariance
# of the estimated effects.

######
# 19 #
######
# To estimate the effect on the probability of getting married,
# we should focus on people who were not already married before  
# the lottery. We estimate a TSLS regression on this subsample:
feols(married_post ~ subdistrict.f | migrated ~ winner,
      data = migration,
      subset = ~married_pre==0,
      vcov = ~locality)
# If you failed to restrict to unmarried people, you would have 
# underestimated the effect. You got partial credit in this case.

# For comparison, we compute the share of initially unmarried 
# people who got married after the lottery:
migration |>
  filter(married_pre==0) |>
  summarize(marriage_rate = mean(married_post))

######
# 20 #
######
# Migration decreases probability of marrying by 17 percentage points.
# The p-value is below 0.01, implying statistical significance. The
# effect is large, 39 percent of the marriage rate!
  

