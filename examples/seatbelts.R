# This do file analyzes a panel of states over time
# to measure the effect of seat belt laws on motor vehicle
# fatality rates and seat belt usage between 1983 and 1997.

# load packages
library(tidyverse)
library(fixest)

# clear environment
rm(list = ls())

# read in dataset
sb <- read_csv("https://github.com/tomvogl/econ121/raw/main/data/seatbelts.csv")

# generate log income per capita
sb$lnincome <- log(sb$income)

# summarize
summary(sb)
# fatalityrate is the number of fatalities per million traffic miles
# vmt is the number of traffic miles driven, in milliions
# sb_usage is the seat belt usage rate, from 0 to 1

##############################
# VISUALIZE POLICY VARIATION #
##############################
ggplot() +
  geom_line(data = sb, aes(x = year,y = primary, color = "Primary")) +
  geom_line(data = sb, aes(x = year,y = secondary, color = "Secondary"), linetype = 2) +
  labs(x = "Year", y = "Seat belts law") +
  facet_wrap(~ state) +
  scale_color_manual(name="Type of law", values = c("Primary" = "purple", "Secondary" = "orange")) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())

#####################################
# BASELINE MODEL WITH NO COVARIATES #
#####################################
# two-way fixed effects model (state FE and year FE)
feols(sb_usage ~ primary + secondary | fips + year, data = sb)
feols(fatalityrate ~ primary + secondary | fips + year, data = sb)
# significant positive effects on seat belt usage.
# positive but not significant effects on fatality rate.

# first difference model
feols(d(sb_usage) ~ d(primary) + d(secondary) | year,
      data = sb, panel.id = ~fips+year)
feols(d(fatalityrate) ~ d(primary) + d(secondary) | year,
      data = sb, panel.id = ~fips+year)
# significant positive effects on seat belt usage.
# significant positive effects on fatality rate.
# the change in significance for fatality rate is mainly 
# coming from smaller standard errors. this is consistent
# with the FD model being efficient because of serial correlation.

################################################
# INCLUDE TIME-VARYING, STATE-LEVEL COVARIATES #
################################################
# two-way fixed effects
feols(sb_usage ~ primary + secondary + speed65 + speed70 + 
                 drinkage21 + ba08 + lnincome + age | fips + year, data = sb)
feols(fatalityrate ~ primary + secondary + speed65 + speed70 + 
                     drinkage21 + ba08 + lnincome + age | fips + year, data = sb)

# first differences
feols(d(sb_usage) ~ d(primary) + d(secondary) + d(speed65) + d(speed70) + 
                    d(drinkage21) + d(ba08) + d(lnincome) + d(age) | year,
      data = sb, panel.id = ~fips+year)
feols(d(fatalityrate) ~ d(primary) + d(secondary) + d(speed65) + d(speed70) + 
                        d(drinkage21) + d(ba08) + d(lnincome) + d(age) | year,
      data = sb, panel.id = ~fips+year)
