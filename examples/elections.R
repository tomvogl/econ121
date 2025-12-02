# this R script estimates the political party incumbency
# advantage in US House elections

rm(list=ls())
library(tidyverse)
library(fixest)
load(url("https://github.com/tomvogl/econ121/raw/main/data/elections.rds"))

# the running variable in this RD analysis is the
# democratic vote margin of victory: the vote share
# of the democrat minus the vote share of the other
# top-two candidate. the democrat wins for all values
# greater than zero

# the dependent variable is the democratic vote share
# in the next election

# we also have two predetermined variables: the
# democratic vote share in the previous election and 
# the democrat's years of political experience

summary(elections)

# histogram of the democratic vote margin of victory
# make sure that the bars don't overlap zero
ggplot(elections, aes(x=difdemshare)) + 
       geom_histogram(binwidth=0.025, boundary=-1)

# drop uncontested elections (where difdemshare is 1 or -1)
elections <- elections |> filter(abs(difdemshare)!=1)

# generate a binning variable for the figures, with
# each bin 0.05 wide. then generate local means for 
# each of the bins
local_means <- 
  elections |>
  mutate(bin = floor(difdemshare*20)/20+0.025) |>
  group_by(bin) |>
  summarize(demsharenext_mean = mean(demsharenext),
            demshareprev_mean = mean(demshareprev),
            demofficeexp_mean = mean(demofficeexp))

# look at the means of the outcome variable and the predetermined
# variable. the outcome changes discontinuously at zero. the predetermined
# variables do not.
ggplot(local_means, aes(bin, demsharenext_mean)) +
  geom_point()
ggplot(local_means, aes(bin, demshareprev_mean)) +
  geom_point()
ggplot(local_means, aes(bin, demofficeexp_mean)) +
  geom_point()

# now let's run our main RD, using a global 4th order polynomial
# to approximate the conditional expectation function. we allow
# the shape of the polynomial to be different above and below
# the victory threshold. we will cluster by district-decade,
# since district boundaries are redrawn every 10 years, but
# this detail is not important for the course.
poly <-
  feols(demsharenext ~ right + 
                       difdemshare + difdemshare2 + difdemshare3 + difdemshare4 +
                       rdifdemshare + rdifdemshare2 + rdifdemshare3 + rdifdemshare4,
        data = elections,
        vcov = ~statedisdec)
summary(poly)
# so a democratic victory now causes a 7 point increase in
# the next election's democratic vote share.
# let's generate a predicted value
elections$demsharenext_hat_poly <- predict(poly, elections)

# let's check whether this result is robust to including the 
# predetermined variables as controls
poly_control <- 
  feols(demsharenext ~ right + difdemshare + difdemshare2 + difdemshare3 + difdemshare4 +
                       rdifdemshare + rdifdemshare2 + rdifdemshare3 + rdifdemshare4 +
                       demshareprev + demofficeexp,
        data = elections, 
        vcov = ~statedisdec)
summary(poly_control)
# doesn't seem to matter much, which is promising. let's generate 
# a predicted value, holding demshareprev and demofficeexp at their means.
elections_temp <- # new data frame that replaces control vars with their means
  elections %>%
  mutate(demshareprev = mean(demshareprev),
         demofficeexp = mean(demofficeexp))
elections$demsharenext_hat_poly_control <- predict(poly_control, elections_temp)
rm(elections_temp)

# the above result suggests that demshareprev demofficeexp don't 
# change discontinuously at zero, but we can also check directly
feols(demshareprev ~ right + 
                     difdemshare + difdemshare2 + difdemshare3 + difdemshare4 +
                     rdifdemshare + rdifdemshare2 + rdifdemshare3 + rdifdemshare4,
      data = elections,
      vcov = ~statedisdec)

feols(demofficeexp ~ right + 
        difdemshare + difdemshare2 + difdemshare3 + difdemshare4 +
        rdifdemshare + rdifdemshare2 + rdifdemshare3 + rdifdemshare4,
      data = elections,
      vcov = ~statedisdec)

# the global polynomial might be misleading, let's check for 
# local mean differences and also run a local linear regression
# we use a bandwidth of 0.1
locmean <- 
  feols(demsharenext ~ right,
        data = elections,
        subset = ~(abs(difdemshare)<0.1),
        vcov = ~statedisdec)
summary(locmean)

loclin <- 
  feols(demsharenext ~ right + difdemshare + rdifdemshare,
        data = elections,
        subset = ~(abs(difdemshare)<0.1),
        vcov = ~statedisdec)
summary(loclin)
# let's generate a predicted value
elections$demsharenext_hat_loclin <- predict(loclin, elections)
# set the prediction to NA if it is outside the bandwidth
elections <- 
  elections |>
  mutate(demsharenext_hat_loclin = if_else(abs(difdemshare)<.1, demsharenext_hat_loclin, NA))

# let's re-plot the binned means, this time with our various predictions
ggplot() +
  geom_point(data = local_means, aes(bin, demsharenext_mean)) +
  geom_line(data = elections, aes(difdemshare, demsharenext_hat_poly), color = "blue") +
  geom_line(data = elections, aes(difdemshare, demsharenext_hat_poly_control), color = "navy") +
  geom_line(data = elections, aes(difdemshare, demsharenext_hat_loclin), color = "tomato")
                







