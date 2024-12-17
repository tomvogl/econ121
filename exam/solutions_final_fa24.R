# clear environment
rm(list = ls())

# load packages
library(fixest)
library(tidyverse)
library(marginaleffects)

## Question 1 ##
## Load the dataset
load(url("https://github.com/tomvogl/econ121/raw/main/data/project_star.rds"))
## Standardize: Subtract the mean and divide by the standard deviation. You could have also used scales().
project_star <- project_star %>% mutate(stest_std = (stest - mean(stest, na.rm = TRUE))/sd(stest, na.rm = TRUE))
## Because schools are sampled, we need to cluster standard errors at
## the school level. You would have also gotten full credit if you 
## clustered at the classroom level.
feols(stest_std ~ smale + swhite + sfree, data = project_star, vcov = ~schoolid)

## Question 2 ##
## Boys score .20 SD less than girls, and free lunch students score .48 SD
## lower than non-free lunch students, on average. Both of these results
## have p-values less than .05, so they are statistically different from 
## zero at conventional significance levels. In addition, white students 
## score .14 SD more than non-white students, but this difference is not
## statistically significant. (If you had clustered at the classroom level
## or neglected to cluster, you would have found a significant difference 
## for gender too.)

## Question 3 ##
## Define the outcome variable.
project_star <- project_star %>% mutate(sbelow = if_else(stest < 400, 1, 0))
## Estimate the logit model.
logit_model <- feglm(sbelow ~ smale + swhite + sfree, data = project_star, vcov = ~schoolid, family = 'logit')
## Estimate odds ratios and CIs. The CIs were not required, but it is good practice to compute them.
cbind(exp(coef(logit_model)), exp(confint(logit_model)))
## Estimate average marginal effects.
avg_slopes(logit_model)

## Question 4 ##
## The largest risk factor is free lunch eligibility. Holding constant gender
## and race, free lunch eligibility is associated with a tripling of the odds
## of being below grade level, or a 7 percentage point higher probability of 
## being below grade level.

## Question 5 ##
## First we generate dummy variables for the two treatments.
project_star <- project_star %>% mutate(small = if_else(cltype==1,1,0),
                                        aide = if_else(cltype==3,1,0))
## Now we run the regression. We need to include school fixed effects  
## because randomization was stratified by school. I have continued to 
## cluster standard errors at the school level, but you could have also 
## clustered at the classroom level on the basis of the design, which 
## randomized at the classroom level.
feols(stest_std ~ small + aide | schoolid, data = project_star, vcov = ~schoolid)

## Question 6 ##
## The results imply that being in a small class (relative to a regular size
## class) raises test scores by 0.21 standard deviations, roughly 45% of
## the free lunch gap. This difference is statistically significant
## below the 0.001 level, and because of the randomized design, it can be 
## interpreted as the causal effect of being in a small class. In contrast,
## the results suggest that adding a teacher's aide to a regular size class 
## has no effect on test scores.

## Question 7 ##
## Generate dummy for inexperienced teacher, plus interactions with treatments.
project_star <- project_star %>% mutate(inexp = ifelse(texp==0,1,0),
                                        smallXinexp = small*inexp,
                                        aideXinexp = aide*inexp)
## Run regression
interact_model <- feols(stest_std ~ small + aide + inexp + smallXinexp + aideXinexp | schoolid, 
                        data = project_star, 
                        vcov = ~schoolid)
summary(interact_model)
## Estimate ratio and SE
hypotheses(interact_model,"(smallXinexp + small) / small = 1")

## Question 8 ##
## The coefficient on smallXinexp is significantly positive (p<0.01),    
## indicating that the effect of "small" class type is larger when the
## teacher inexperienced. The ratio of effects for inexperienced to 
## experienced teachers is 4.5, implying that the effect for inexperienced 
## teachers is 4.5 times that for experienced teachers.

## Question 9 ##
## To estimate the effect of adding one student, we rely on
## two-stage least squares, using the experimental assignment to the
## "small" class type as the instrument for the number of students.
## Because assignment to the "small" class type was stratified by school,
## we include school fixed effects (or dummies) as exogenous controls. 
## As above, you could have clustered SEs at the class or school level.
## Below, I show a few ways to run the equivalent TSLS regression.
feols(stest_std ~ 1 | schoolid | csize ~ small, data = project_star)
feols(stest_std ~ factor(schoolid) | csize ~ small, data = project_star)

## Question 10 ##
## The TSLS estimate implies that increasing class size by one student
## causes the test score to decline by 0.026 standard deviations. It is
## statistically significant, although you did not need to mention it
## for full credit. The most important assumption is that being assigned 
## to the "small" class type only affects test scores through its effects
## on the number of students in the class. You would have also gotten
## full credit for saying that we must assume linearity of class size
## effects.

## Question 11 ##
## Load the dataset
load(url("https://github.com/tomvogl/econ121/raw/main/data/maimonides.rds"))
## Run the regression. Here it is again appropriate to cluster
## at the school level, since there can be several classrooms
## per school. At the same time, the description of the sampling 
## scheme was not completely explicit, and you would have gotten
## full credit if you did not cluster and explained in the comments  
## that you prefer to cluster only when there is explicit clustered 
## sampling or assignment. You needed a full explanation, however.
feols(avg_verb ~ disadv, data = maimonides, vcov = ~school_id)

## Question 12 ##
## A 1 percentage point increase in the disadvantaged share is associated
## with a .33 point decrease in the average verbal score. Weighting by the 
## number of students would shrink the standard error while not substantially
## changing the coefficient if the individual relationship between student 
## disadvantage and student performance were (i) homogeneous and (ii) homoskedastic,
## and if (iii) the performance of a student did not depend on the composition of
## her classmates. You got full credit if you answered just (i) and (ii).

## Question 13 ##
## Create running variable
maimonides <- maimonides %>% mutate(x = grade_size - 41)
## Binned data: I use bins of 5, such that [-4,0] and [1,5]
## are separate, and I center the value at the middle of 
## the interval. I keep bins between -25 and 25.
binned_data <-
  maimonides %>%
  mutate(bin = floor(x/5)*5 + 2.5) %>% 
  filter(abs(bin)<25) %>%
  group_by(bin) %>%
  summarize(mean_class = mean(class_size),
            mean_verb = mean(avg_verb, na.rm=TRUE))
## Graphs
ggplot(binned_data, aes(x=bin,y=mean_class)) +
  geom_point()
ggplot(binned_data, aes(x=bin,y=mean_verb)) +
  geom_point()
	   
## Question 14 ##
## In the first graph, class size discontinuously declines at the 40-
## student cutoff, suggesting that Maimonides' rule reduces class size
## when it binds in grades close to the cutoff. This is the first-stage 
## relationship.
##
## In the second graph, test scores seem to increase at the cutoff
## cutoff, but less precisely than for the first-stage relationship.
## This suggests that Maimonides' rule may raise average test scores
## when it binds in grades close to the cutoff. This is the reduced form 
## relationship.

## Question 15 ##
## Define the local subsample and generate the variables for the RD regression
maimonides_local <- 
  maimonides %>%
  subset(x>=-25 & x<=25) %>% ## keep local polynomial sample
  mutate(D = ifelse(x>=0,1,0),
         Dx = D*x)
## First stage regression
feols(class_size ~ D + x + Dx, data = maimonides_local, vcov = ~school_id)
## Reduced form regression
feols(avg_verb ~ D + x + Dx, data = maimonides_local, vcov = ~school_id)

## Question 16 ##
## The regression results imply that crossing the 40-student cutoff
## decreases average class size by 15.1 students and raises average
## scores by 2.3 points. The first-stage result is high statistically
## significant, but the reduced-form result is only marginally
## significant, with a p value of 0.11. (You didn't need to comment
## on statistical significance for full credit.) These results have 
## a causal interpretation if...
##
##   (i) potential outcomes are continuous in x at 0
## or
##   (ii) schools and parents have imperfect control over grade size.
##
## You only needed to write one of these assumptions. We can assess the
## assumption(s) validity by
##
##   (i) testing for a discontinuity in a predetermined characteristic
## or
##   (ii) evaluating the continuity of the histogram of x.

## Question 17 ##
feols(disadv ~ D + x + Dx, data = maimonides_local, vcov = ~school_id)
## or
ggplot(maimonides_local, aes(x=x)) + 
  geom_histogram(binwidth=1, boundary=-25)

## Question 18 ##
## We divide the reduced form effect by the first stage effect:
## 2.29 / (-15.01) = -0.15.
## Thus, a 1-student increase decreases average tests scores by 0.15 points. 
## In this fuzzy RD, the average effect is specific to compliers near the cutoff.
## Here, compliers are schools with roughly 40 4th-graders that add classes 
## only if the rule forces them to do so. In addition to the assumption from 
## Question 16, we also need to assume that crossing the cutoff only affects
## test scores through its effect on class size (exclusion restriction) and
## that crossing the cutoff does not increase class size in any school (monotonicity).

## Question 19 ##
feols(avg_verb ~  x + Dx | class_size ~ D, data = maimonides_local, vcov = ~school_id)

feols(avg_verb ~ class_size, data = maimonides, vcov = ~school_id)

## Question 20 ##
## Two-stage least squares finds a negative .15 point effect of 
## an extra student, consistent with the calculation in Question 18.
## However, the effect is only marginally significant with a p-value of
## 0.12. In contrast, OLS finds a positive .13 point effect. The difference 
## implies an upward bias in the OLS result. One possible reason for such a
## bias is that school administrators may decrease class size in schools with 
## many low-performing students.

	   


	   





