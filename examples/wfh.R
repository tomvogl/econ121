# This example uses data on call center workers at a Chinese firm
# to study the consequences of work from home. In 2010, the firm 
# ran a pilot program that allowed the call center workers to work
# from home. Interested workers signed up for a lottery, and the 
# firm randomly chose approximately half of the lottery participants. 
# Lottery winners were expected to work at home for the duration of 
# the program, except if they lost access to a private room or an 
# internet connection. Lottery losers were expected to work at the call center.

# The dataset contains weekly panel data on the 439 workers who were eligible
# for the program. The dataset contains one observation per week per worker,
# with 37 weeks of data during the program and 48 weeks preceding it.

# Load R packages
library(tidyverse)
library(fixest)
  
# Load the dataset
load(url("https://github.com/tomvogl/econ121/raw/main/data/wfh.rds"))

# Drop obs with missing calls data
wfh <- wfh %>% drop_na(calls)

# Run an OLS regression of calls per week on WFH. This result will
# be biased, but it will be useful to compare with our IV estimate
# of the causal effect. I cluster standard errors at the individual 
# level because it is unreasonable to assume that errors are 
# independent within worker.
feols(calls ~ wfh, 
      data = wfh, 
      vcov = ~personid)
# WFH is associated with 10 more calls per week, but the association
# is not statistically significant at conventional levels. Why is
# this association unlikely to reflect a causal effect? One issue
# is that workers who selected into the lottery may have different
# productivity from those who didn't. Another issue is that the data
# contain data from before and after the firm started allowing WFH.
# If productivity changed over time for reasons unrelated to WFH,
# that change would show up in the coefficient on WFH.

# Now let's run a regression of calls per week on winning the
# lottery. This is a "reduced form" effect of winning the 
# lottery. We include only # observations from the program period, 
# and only workers who participated in the lottery. Winning the 
# lottery is random only within this group. 
feols(calls ~ winner, 
      data = wfh,
      subset = ~(lottery==1 & prog_period==1),
      vcov = ~personid)
# Winning the lottery raises calls per week by 30. This effect
# is statistically significant at the 5% level.

# Because the lottery is randomized, it should not be correlated
# with worker characteristics the preceded the program. So we
# should not have to worry about omitted variables bias. But
# we can still check by including pre-program covariates,
feols(calls ~ winner + age + female + college + children + commute, 
      data = wfh,
      subset = ~(lottery==1 & prog_period==1),
      vcov = ~personid)
# The estimated effect of winning the lottery is the same. We
# conclude that the effect is robust to controlling for pre-program 
# covariates.

# Above, we checked that the estimated treatment effect is robust
# to exogenous covariates. Another way to check whether the lottery
# was random is to check whether winners and losers had similar 
# calls per week in the pre-period. This is a type of "balance check."
feols(calls ~ winner, 
      data = wfh, 
      subset = ~(lottery==1 & prog_period==0), 
      vcov = ~personid)
# There is no significant difference in productivity between
# winners and losers in the pre-period. That is consistent
# with successful randomization.

# So far, we have just seen "reduced form" estimates. What is
# the "first stage?" We can run the "first stage" regression:
feols(wfh ~ winner, 
      data = wfh, 
      subset = ~(lottery==1 & prog_period==1), 
      vcov = ~personid)
# Winning the lottery raises the probability of WFH by 87 percentage
# points on average. Among lottery winners, we sum the constant term
# and the coefficient on winner to find that 87% of weeks involved 
# WFH. Among lottery losers, the constant term indicates that 
# 0.06% of weeks involved WFH.

# The "first stage" results suggest that the lottery very closely
# approximated a randomized experiment with partial noncompliance.
# In such an experiment, there are no "always takers," so the 
# intercept in the first stage should be zero. Here, it is close to
# zero, but not exactly. This has implications for the language
# we use. If there are no "always takers" we refer to the first
# estimated effect above as "intent to treat" rather than 
# "reduced form."

# We can compute an instrumental variables estimate of the effect
# of work from home on productivity by dividing the reduced
# form coefficient by the first stage coefficient. The first stage 
# coefficient is 0.871746. The reduced form coefficient is 29.97503. 
# The IV estimate of the effect of WFH is therefore:
29.97503/0.871746

# We can also estimate the IV coefficient using TSLS.
feols(calls ~ 1 | wfh ~ winner,
      data = wfh,
      subset = ~(lottery==1 & prog_period==1),
      vcov = ~personid)

# There are *almost* no "always takers" here. If there were none,
# we could interpret the IV estimate as a TOT. With "always
# takers," we should interpret the IV estimate as a LATE, if
# monotonicity is satisfied.

# Now let's assess the robustness of the IV coefficient to
# controlling for pre-program covariates:
# We can also estimate the IV coefficient using TSLS.
feols(calls ~ age + female + college + children + commute | wfh ~ winner,
      data = wfh,
      subset = ~(lottery==1 & prog_period==1),
      vcov = ~personid)