
#########################################################################
#                                                                       #
#  ANOVA in R                                                           #
#                                                                       #
#########################################################################

library(ggplot2)
library(car)

#  Recall the ANOVA table from linear regression, which displays the    #
#  variation that can be explained by the regression model, the         #
#  variation that cannot be explained by the regression model, and      #
#  the overall variation in the response variable, along with other     #
#  associated quantities.                                               #
#                                                                       #
#     Source      SS    df    MS                                        #
#     Regression  SSR   dfR   MSR                                       #
#     Error       SSE   dfE   MSE                                       #
#     Total       SST   dfT                                             #
#                                                                       #
#  The F statistic that is commonly shown in the ANOVA table tests      #
#  the hypothesis that all of the parameters are equal to zero. When    #
#  a linear regression model contains only categorical explanatory      #
#  variables, the parameters do not represent slopes; instead they      #
#  represent the differences in the mean values of the response         #
#  variable between the given level and the baseline level of the       #
#  explanatory variable. Thus, the hypothesis that all of the           #
#  parameters are equal to zero is equivalent to the hypothesis that    #
#  the means of the different levels of the explanatory variables are   #
#  the same. ANOVA is a methodology used for testing whether the means  #
#  of a response variable over different populations differ.            #
#                                                                       #
#  If there is only one explanatory variable that is categorical,       #
#  using the ANOVA quantities to test the equivalence of the mean       #
#  values of the response variable is called one-way ANOVA. The method  #
#  of incorporating information from another variable that is           #
#  unrelated to the explanatory variable of interest but can control    #
#  some of the variation included in the unexplained variation          #
#  measures is called ANCOVA (Analysis of Covariance). When multiple    #
#  categorical explanatory variables are of interest, the amount of     #
#  variation explained can be measured and tested separately for each   #
#  variable using two-way ANOVA or factorial ANOVA.                     #
#                                                                       #
#  ANOVA can be approached from two different directions, but the       #
#  testing quantities and procedures are the same. Practitioners will   #
#  either state the hypotheses in terms of the means or give the ANOVA  #
#  model to be used. The ANOVA model is equivalent to a linear          #
#  regression model under the ANOVA conditions, but different notation  #
#  is used. Regardless of the approach used, the underlying             #
#  assumptions of ANOVA procedures are the same: all of the             #
#  observations are independent and are normally distributed with       #
#  unknown means and a common variance.                                 #                                                         #

###################
#  One-way ANOVA  #
###################

#  One-way ANOVA is used to test whether there is a significant         #
#  difference between the means of a response variable from different   #
#  populations defined by one explanatory variable of interest. The     #
#  model notation for one-way ANOVA is                                  #
#     X_ij = mu + alpha_i + epsilon_ij                                  #
#  where mu is the mean of all the observations over all of the         #
#  populations, alpha_i is the difference in the overall mean and the   #
#  i-th group mean, and epsilon_ij is the difference between each       #
#  group mean and the j-th individual observation value.                #
#                                                                       #
#  Using the hypothesis testing approach, the hypotheses for one-way    #
#  ANOVA are                                                            #
#     Ho: mu_1 = mu_2 = ... = mu_k                                      #
#     Ha: at least two of the means are not equal                       #
#                                                                       #
#  When the categorical variable of interest has two levels, the        #
#  ANOVA procedure is equivalent to the two-sided two-sample t-test     #
#  comparing two means assuming equal variances.                        #
#                                                                       #
#  One-way ANOVA procedures rely on the underlying assumptions given    #
#  above, but are generally robust against normality and constant       #
#  variance violations when the sample sizes in all populations are     #
#  equal. If normality holds but constant variance does not, whether    #
#  the one-way ANOVA test remains appropriate depends on the            #
#  relationship of the variances. If the variance of the larger group   #
#  is bigger, the ANOVA test tends to be conservative and err on the    #
#  side of determining no differences between the population means.     #
#                                                                       #
#  The assumptions can be checked through residuals plots. The          #
#  constant variance assumption can also be checked using Levene's      #
#  test, which tests the null hypothesis that the variances of the      #
#  groups are the same. The function leveneTest() in the car package    #
#  can be used to conduct this test by inputting the desired formula    #
#  and data.                                                            #
#                                                                       #
#  Recall the chief logistics officer for the small craft brewery who   #
#  is evaluating their delivery efficiency by measuring the delivery    #
#  time at randomly selected locations. Suppose that the variable of    #
#  interest is whether the location is a city or non-city location.     #

## Delivery time data
dtime <- c(16.7,11.5,12.0,14.9,13.8,18.1,8.0,17.8,21.5,40.3,21.0,
           13.5,19.8,24.0,29.0,15.4,19.0,9.5,35.1,17.9,52.3,18.8,19.8,10.8)
loctype <- c("N","N","N","N","N","C","N","C","C","C","C","N","C","C",
             "C","N","C","N","N","N","C","C","C","N")
deliv <- data.frame(dtime,loctype)

## Plot data
ggplot(deliv, aes(x=loctype, y=dtime)) + 
    stat_summary(fun.y=mean, geom="point") +
    stat_summary(fun.y=mean, geom="line", aes(group=1)) +
    stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)

## Residual plot
lt.mod <- lm(dtime ~ loctype, data=deliv)
ggplot(lt.mod, aes(x=loctype, y=.resid)) + geom_point() + 
    geom_hline(yintercept=0, linetype="dashed")

## Levene's test
leveneTest(dtime ~ loctype, data=deliv)

## QQ plot
X <- data.frame(resid = residuals(lt.mod))
y <- quantile(X$resid, c(0.25, 0.75)) 
x <- qnorm(c(0.25, 0.75))  
slope <- diff(y)/diff(x) 
int <- y[1] - slope*x[1]   
ggplot(X, aes(sample = resid)) + stat_qq() + 
    geom_abline(intercept=int, slope=slope) 

## ANOVA test for delivery time differences in location
lt.test <- aov(dtime ~ loctype, data=deliv)
lt.test
summary(lt.test)

## ANOVA test for delivery time differences in location
anova(lt.mod)

## T-test for delivery time differences in location
t.test(dtime ~ loctype, data=deliv, var.equal=TRUE)

#  Suppose that the logistics officer is interested in differences      #
#  between the stocking the driver must do at each location.            #

## Delivery time data
stock <- c("DO","PS","DO","PS","DO","PS","DO","FS","FS","FS","PS",
           "PS","FS","DO","PS","DO","FS","DO","FS","PS","FS","PS","FS",
           "DO")
deliv <- data.frame(deliv,stock)

## Plot data
ggplot(deliv, aes(x=stock, y=dtime)) + 
    stat_summary(fun.y=mean, geom="point") +
    stat_summary(fun.y=mean, geom="line", aes(group=1)) +
    stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)

## Residual plot
st.mod <- lm(dtime ~ stock, data=deliv)
ggplot(st.mod, aes(x=stock, y=.resid)) + geom_point() + 
    geom_hline(yintercept=0, linetype="dashed")

## Levene's test
leveneTest(dtime ~ stock, data=deliv)

## QQ plot
X <- data.frame(resid = residuals(st.mod))
y <- quantile(X$resid, c(0.25, 0.75)) 
x <- qnorm(c(0.25, 0.75))  
slope <- diff(y)/diff(x) 
int <- y[1] - slope*x[1]   
ggplot(X, aes(sample = resid)) + stat_qq() + 
    geom_abline(intercept=int, slope=slope) 

## ANOVA test for delivery time differences by stocking type
st.test <- aov(dtime ~ stock, data=deliv)
summary(st.test)

#  If the ANOVA test indicates that the population means are            #
#  different, it is common to ask which of the means are different      #
#  from each other. The various pairs cannot be evaluated               #
#  independently because the overall confidence decreases               #
#  significantly. Several simultaneous tests have been proposed, we     #
#  will focus on Tukey's honestly significant difference (HSD).         #
#  Tukey's method produces confidence intervals for each pair of        #
#  populations that have a family-wise significance of the level        #
#  desired. If these intervals contain zero, there is not significant   #
#  evidence that the particular pair is different.                      #
#                                                                       #
#  The function TukeyHSD() will produce the family-wise confidence      #
#  intervals calculated using Tukey's method. The only required input   #
#  is the ANOVA test object. The family-wise confidence level can also  #
#  be specified to a level other than 95%.                              #

## Tukey's HSD intervals
TukeyHSD(st.test)

#  Care is required in reporting differences among population means.    #
#  In cases when population 1 and population 2 are not different and    #
#  population 2 and population 3 are not different, but population 1    #
#  and population 3 are different, it is important to include all of    #
#  these relationships in the conclusions.                              #
#                                                                       #
#  Suppose that a department is experimenting with possible course      #
#  designs - lecture, discussion, and reading - and selects a focus     #
#  group of 12 students. Each student is randomly assigned to one of    #
#  the course designs under consideration. After two weeks, each        #
#  student is given a multiple choice test and the department wants to  #
#  determine if there is a difference in the test scores between the    #
#  course designs.                                                      #

## Course design data
score <- c(15,17,18,14,15,19,14,12,12,10,10,12)
type <- c("L","L","L","L","D","D","D","D","R","R","R","R")
cd.data <- data.frame(score,type)

## Plot data
ggplot(cd.data, aes(x=type, y=score)) + 
    stat_summary(fun.y=mean, geom="point") +
    stat_summary(fun.y=mean, geom="line", aes(group=1)) +
    stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)

## Residual plot
cd.mod <- lm(score ~ type, data=cd.data)
ggplot(cd.mod, aes(x=type, y=.resid)) + geom_point() + 
    geom_hline(yintercept=0, linetype="dashed")

## Levene's test
leveneTest(score ~ type, data=cd.data)

## QQ plot
X <- data.frame(resid = residuals(cd.mod))
y <- quantile(X$resid, c(0.25, 0.75)) 
x <- qnorm(c(0.25, 0.75))  
slope <- diff(y)/diff(x) 
int <- y[1] - slope*x[1]   
ggplot(X, aes(sample = resid)) + stat_qq() + 
    geom_abline(intercept=int, slope=slope) 

## ANOVA test for test score differences by course design type
cd.test <- aov(score ~ type, data=cd.data)
summary(cd.test)

## Tukey's HSD intervals
TukeyHSD(cd.test)

#########################################################################

install.packages("effects")

library(effects)
library(ggplot2)
library(car)

####################
#  One-way ANCOVA  #
####################

#  One-way ANCOVA is an extension of one-way ANOVA. The objective of    #
#  one-way ANCOVA is still to determine whether there are significant   #
#  differences between the means of a response variable over the        #
#  groups defined by a categorical variable of interest. The added      #
#  component of one-way ANCOVA is that various quantitative variables   #
#  called covariates can be added to the model to help control the      #
#  unexplained variance. Good candidates for covariates are variables   #
#  that have a linear relationship with the response variable but have  #
#  no relationship with the explanatory variable of interest. The two   #
#  added assumptions are that the explanatory variable and covariate    #
#  are independent and that the covariate has the same slope over all   #
#  populations.                                                         #
#                                                                       #
#  Adding covariates to a one-way ANOVA analysis has several benefits.  #
#  First, it decreases the unexplained variation, which is the          #
#  denominator of the test statistic. Thus, there is more power for     #
#  explanatory variable of interest to show a significant difference.   #
#  Adding a covariate can help to uncover significant differences       #
#  among the populations if they are masked by other factors and can    #
#  help to identify a better reason for differences between             #
#  populations that seem significant.                                   #
#                                                                       #
#  The hypotheses for one-way ANCOVA are identical to the hypotheses    #
#  for one-way ANOVA. The one-way ANCOVA model uses Y_ijk to denote     #
#  the response variable and X_jk to denote the value of the j-th       #
#  covariate for the k-th observation.                                  #
#     Y_ijk = mu + beta_j X_jk + alpha_i + epsilon_ijk                  #
#                                                                       #
#  From the regression approach, since there are now variables in the   #
#  model besides the explanatory variable of interest, the overall F    #
#  test is no longer equivalent to the one-way ANCOVA hypotheses. To    #
#  account for the additional information, but to test only the         #
#  explanatory variable of interest, the regression quantities in       #
#  general ANOVA table are split so that the explanatory variable of    #
#  interest and the covariate(s) are measured separately.               #
#                                                                       #
#     Source      SS     df     MS      F     p-value                   #
#     CV          SSCV   dfCV   MSCV    FCV   pCV                       #
#     Effect A    SSA    dfA    MSA     FA    pA                        #
#     Error       SSE    dfE    MSE                                     #
#     Total       SST    dfT                                            #

#  Suppose a company is testing whether their newly developed drug is   #
#  effective for increasing memory recall. They recruit 10 patients     #
#  and randomly assign them to either the new drug or a placebo. After  #
#  a certain amount of time, each patient is given a memory test and    #
#  their score is recorded. Research has shown that IQ affects memory   #
#  recall, so the company decides to use the patient's IQ score as a    #
#  covariate.                                                           #

## Memory study data
mem <- c(7.0,7.9,5.4,10.0,11.7,11.0,9.4,14.8,17.5,16.3)
treat <- c("P","P","P","P","P","D","D","D","D","D")
IQ <- c(112,111,105,115,119,116,109,121,118,123)
mem.data <- data.frame(mem, treat, IQ)

## Plot means of explanatory variable
ggplot(mem.data, aes(x=treat, y=mem)) + 
    stat_summary(fun.y=mean, geom="point") +
    stat_summary(fun.y=mean, geom="line", aes(group=1)) +
    stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)

## Plot covariate
ggplot(mem.data, aes(x=IQ, y=mem)) + geom_point()

ggplot(mem.data, aes(x=IQ, y=mem, colour=treat)) + geom_point() +
    geom_smooth(method=lm, se=FALSE)

## ANOVA on covariate
summary(aov(IQ ~ treat, data=mem.data))

## Levene's test
leveneTest(mem ~ treat, data=mem.data)

## QQ plot
mod <- lm(mem ~ IQ + treat, data=mem.data)
X <- data.frame(resid = residuals(mod))
y <- quantile(X$resid, c(0.25, 0.75)) 
x <- qnorm(c(0.25, 0.75))  
slope <- diff(y)/diff(x) 
int <- y[1] - slope*x[1]   
ggplot(X, aes(sample = resid)) + stat_qq() + 
    geom_abline(intercept=int, slope=slope) 

## ANCOVA test for memory recall differences by treatment
acov.test <- aov(mem ~ IQ + treat, data=mem.data)
summary(acov.test)

acov.test2 <- aov(mem ~ treat + IQ, data=mem.data)
summary(acov.test2)

Anova(acov.test, type="III")
Anova(acov.test2, type="III")

## ANCOVA test with interaction term
acov2.test <- aov(mem ~ IQ*treat, data=mem.data)
summary(acov2.test)

## ANOVA test for memory recall differences by treatment
aov.test <- aov(mem ~ treat, data=mem.data)
summary(aov.test)

#  Recall the logistics officer for the craft brewery who is            #
#  interested in knowing whether the average delivery time changes      #
#  depending on the stocking the driver must do at the location.        #
#  Recall that when using one-way ANOVA, a significant difference was   #
#  found. Suppose that the officer decides to incorporate the distance  #
#  that the driver must travel at the location to make the delivery as  #
#  a covariate.                                                         #

## Delivery time data
dtime <- c(16.7,11.5,12.0,14.9,13.8,18.1,8.0,17.8,21.5,40.3,21.0,
           13.5,19.8,24.0,29.0,15.4,19.0,9.5,35.1,17.9,52.3,18.8,19.8,10.8)
stock <- c("DO","PS","DO","PS","DO","PS","DO","FS","FS","FS","PS",
           "PS","FS","DO","PS","DO","FS","DO","FS","PS","FS","PS","FS",
           "DO")
dist <- c(560,220,340,80,150,330,110,210,605,688,215,255,462,448,
          776,200,132,36,770,140,810,450,635,150)
deliv <- data.frame(dtime,stock,dist)

## Plot covariate
ggplot(deliv, aes(x=dist, y=dtime, colour=stock)) + 
    geom_point() + geom_smooth(method=lm, se=FALSE)

## ANOVA on covariate
summary(aov(dist ~ stock, data=deliv))

#  An experimental study by Baumann and Jones investigated three        #
#  different reading comprehension instruction methods - one was        #
#  standard and two were experimental. Before starting the 66 subjects  #
#  on their randomly assigned programs, each was given two pre-tests.   #
#  After the program each was given three post-tests. Suppose that the  #
#  second pre-test and first post-test are designed to measure similar  #
#  reading comprehension components. The objective of this experiment   #
#  is to determine whether there is a difference in the resulting       #
#  reading comprehension over the three instructional methods.          #

## Baumann data (car package)
Baumann[1:10,]

## Plot means of explanatory variable
ggplot(Baumann, aes(x=group, y=post.test.1)) + 
    stat_summary(fun.y=mean, geom="point") +
    stat_summary(fun.y=mean, geom="line", aes(group=1)) +
    stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)

## Plot covariate
ggplot(Baumann, aes(x=pretest.2, y=post.test.1)) + geom_point()

ggplot(Baumann, aes(x=pretest.2, y=post.test.1, colour=group)) + 
    geom_point() + geom_smooth(method=lm, se=FALSE)

## ANOVA on covariate
summary(aov(pretest.2 ~ group, data=Baumann))

## Levene's test
leveneTest(post.test.1 ~ group, data=Baumann)

## QQ plot
mod <- lm(post.test.1 ~ pretest.2 + group, data=Baumann)
X <- data.frame(resid = residuals(mod))
y <- quantile(X$resid, c(0.25, 0.75)) 
x <- qnorm(c(0.25, 0.75))  
slope <- diff(y)/diff(x) 
int <- y[1] - slope*x[1]   
ggplot(X, aes(sample = resid)) + stat_qq() + 
    geom_abline(intercept=int, slope=slope) 

## ANCOVA test for memory recall differences by treatment
acov.test <- aov(post.test.1 ~ pretest.2 + group, data=Baumann)
summary(acov.test)

## ANCOVA test with interaction term
acov2.test <- aov(post.test.1 ~ pretest.2*group, data=Baumann)
summary(acov2.test)

## ANOVA test for memory recall differences by treatment
aov.test <- aov(post.test.1 ~ group, data=Baumann)
summary(aov.test)

#  When the one-way ANCOVA test yields a significant result for the     #
#  explanatory variable of interest, it is common to further            #
#  investigate which of the population means are different. If the      #
#  covariate also yields a significant result, the original means do    #
#  not accurately reflect the effect of the explanatory variable of     #
#  interest as they incorporate the effect of the covariate as well.    #
#                                                                       #
#  To analyze the pairwise differences without the effect of the        #
#  covariate, the means must first be adjusted to remove the influence  #
#  of any other variables included in the model. The resulting means    #
#  are called the marginal means and can be found by applying the       #
#  effect() function from the effects package to the anova output.      #

tapply(Baumann$post.test.1, Baumann$group, mean)
effect("group", acov.test)

#  The marginal means are used for Tukey's HSD analysis. An optional    #
#  argument in the TukeyHSD() function designates which variables to    #
#  compare pairwise. In the case of ANCOVA, only the explanatory        #
#  variable of interest should be analyzed in this way.                 #

TukeyHSD(acov.test, which="group")

#########################################################################

library(ggplot2)
library(car)

###################
#  Two-way ANOVA  #
###################

#  Two-way ANOVA (or factorial ANOVA) incorporates a second             #
#  explanatory variable of interest into ANOVA procedures. The          #
#  assumptions required for two-way ANOVA are the same as those         #
#  required by one-way ANOVA: all of the observations are independent   #
#  and are normally distributed with unknown means and a common         #
#  variance.                                                            #
#                                                                       #
#  Using the regression model approach to ANOVA, there are two          #
#  possible models for two-way ANOVA - the additive model and the       #
#  interactive model. The more appropriate model depends on whether     #
#  all the levels of one explanatory variable of interest have the      #
#  same pattern of response variable means over the other explanatory   #
#  variable of interest or not.                                         #
#                                                                       #
#  The additive two-way ANOVA model is                                  #
#     X_ijk = mu + alpha_i + beta_j + epsilon_ijk                       #
#  where mu is the overall mean of the response variable values,        #
#  alpha_i is the difference between the overall mean and the i-th      #
#  group mean, beta_j is the difference between the overall mean and    #
#  the j-th group mean, and epsilon_ijk is the difference between the   #
#  k-th observation and the mean in the ij-th group.                    #
#                                                                       #
#  The interactive two-way ANOVA model is                               #
#     X_ijk = mu + alpha_i + beta_j + gamma_ij + epsilon_ijk            #
#  where mu, alpha_i, beta_j, and epsilon_ijk are all the same          #
#  quantities as in the additive model  and gamma_ij is the adjustment  #
#  to the mean in the ij-th group due to the interaction between the    #
#  two explanatory variables of interest.                               #
#                                                                       #
#  The two-way ANOVA hypotheses are not fundamentally different from    #
#  the one-way ANOVA hypotheses, but there are more sets of hypotheses  #
#  to test. When the two-way ANOVA is additive, the two sets of         #
#  hypotheses are                                                       #
#     Ho: alpha_1 = alpha_2 = ... = alpha_I = 0                         #
#     Ha: at least one of the alphas is not zero                        #
#  and                                                                  #
#     Ho: beta_1 = beta_2 = ... = beta_J = 0                            #
#     Ha: at least one of the betas is not zero                         #
#                                                                       #
#  When the two-way ANOVA is interactive, the third set of hypotheses   #
#  is                                                                   #
#     Ho: gamma_ij = 0 for all i,j                                      #
#     Ha: at least one of the gammas is not zero                        #
#                                                                       #
#  In order to test the effect of the two explanatory variables         #
#  separately, they must be measured separately in the ANOVA table.     #
#  The general two-way ANOVA table is                                   #
#     Source        SS     df     MS      F     p-value                 #
#     Effect A      SSA    dfA    MSA     FA    pA                      #
#     Effect B      SSB    dfB    MSB     FB    pB                      #
#     Interaction   SSAB   dfAB   MSAB    FAB   pAB                     #
#     Error         SSE    dfE    MSE                                   #
#     Total         SST    dfT                                          #
#                                                                       #
#  Typically in two-way ANOVA, the interaction between the two          #
#  explanatory variables is tested first to determine which type of     #
#  model is most appropriate. The type of model influences which        #
#  follow-up analyses are appropriate.                                  #
#                                                                       #
#  In two-way ANOVA, having equal sample sizes in each group            #
#  combination of the explanatory variables of interest is called a     #
#  balanced design. Using a balanced design for two-way ANOVA is        #
#  strongly preferred.                                                  #
#                                                                       #
#  A study on the effect of vitamin C on tooth growth was studied in    #
#  a random sample of 60 Guinea pigs. Each Guinea pig was given a       #
#  particular dose of vitamin C - 0.5, 1, or 2 mg/day - in one of two   #
#  manners - orange juice or ascorbic acid (VC). At the end of the      #
#  study, the lengths of the cells responsible for tooth grown were     #
#  measured.                                                            #

## Tooth growth data
ToothGrowth[1:15,]
tg.data <- ToothGrowth
tg.data$dose <- as.factor(tg.data$dose)

## Plot means of explanatory variables
ggplot(tg.data, aes(x=dose, y=len, colour=supp)) + 
    stat_summary(fun.y=mean, geom="point") +
    stat_summary(fun.y=mean, geom="line", aes(group=supp)) +
    stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)

## Levene's test
leveneTest(len ~ dose*supp, data=tg.data)

## QQ plot
mod <- lm(len ~ dose*supp, data=tg.data)
X <- data.frame(resid = residuals(mod))
y <- quantile(X$resid, c(0.25, 0.75)) 
x <- qnorm(c(0.25, 0.75))  
slope <- diff(y)/diff(x) 
int <- y[1] - slope*x[1]   
ggplot(X, aes(sample = resid)) + stat_qq() + 
    geom_abline(intercept=int, slope=slope) 

## ANOVA test for length by dose and method
tw.aov.test <- aov(len ~ dose*supp, data=tg.data)
summary(tw.aov.test)

#  Since the interaction term is significant, multiple comparisons are  #
#  not appropriate for either explanatory variable of interest.         #
#                                                                       #
#  Botanists are testing how yield is affected by the variety of        #
#  tomato plant and the planted density (in thousands of plants per     #
#  hectare). Each combination of the three different varieties and      #
#  four different densities were applied to three tomato plants and     #
#  the yield was recorded.                                              #

## Tomato plant data
yield <- c(10.5,9.2,7.5,12.8,13.3,11.2,12.1,12.6,14,10.8,9.1,12.5,
           8.1,8.6,10.1,12.7,13.7,11.5,14.4,15.4,13.7,11.3,12.5,14.5,
           16.1,15.3,17.5,16.6,18.5,19.2,20.8,18,21,17.2,18.9,18.4)
vari <- c(rep(c("H","I","P"), each=12))
den <- rep(rep(c("10","20","30","40"),each=3),3)
tom.data <- data.frame(yield, vari, den)

## Plot means of explanatory variables
ggplot(tom.data, aes(x=den, y=yield, colour=vari)) + 
    stat_summary(fun.y=mean, geom="point") +
    stat_summary(fun.y=mean, geom="line", aes(group=vari)) +
    stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)

## Levene's test
leveneTest(yield ~ vari*den, data=tom.data)

## QQ plot
mod <- lm(yield ~ vari*den, data=tom.data)
X <- data.frame(resid = residuals(mod))
y <- quantile(X$resid, c(0.25, 0.75)) 
x <- qnorm(c(0.25, 0.75))  
slope <- diff(y)/diff(x) 
int <- y[1] - slope*x[1]   
ggplot(X, aes(sample = resid)) + stat_qq() + 
    geom_abline(intercept=int, slope=slope) 

## ANOVA test for yield by variety and density
tw.aov.test <- aov(yield ~ vari*den, data=tom.data)
summary(tw.aov.test)

#  Since the interaction effect is not significant, Tukey's HSD can be  #
#  used to make multiple comparisons on each explanatory variable of    #
#  interest independently.                                              #

## Tukey's HSD intervals for variety
TukeyHSD(tw.aov.test, which="vari")

ggplot(tom.data, aes(x=vari, y=yield)) + 
    stat_summary(fun.y=mean, geom="point") +
    stat_summary(fun.y=mean, geom="line", aes(group=1)) 

## Tukey's HSD intervals for density
TukeyHSD(tw.aov.test, which="den")

ggplot(tom.data, aes(x=den, y=yield)) + 
    stat_summary(fun.y=mean, geom="point") +
    stat_summary(fun.y=mean, geom="line", aes(group=1)) 

#  Recall that one of the assumptions for two-way ANOVA is that all     #
#  the observations are independent. There are cases when it may be     #
#  difficult to design an experiment with truly random individuals.     #
#  In this case, individuals can be grouped together in equal size      #
#  blocks based on similar traits and the block to which the            #
#  individual belongs can be used as one of the explanatory variables.  #
#  Since the main objective of the analysis is to test the primary      #
#  explanatory variable of interest, additive models are typically      #
#  used when incorporating block as a factor.                           #
#                                                                       #
#  Suppose that another botanist decides to study clover accumulation   #
#  over four different sowing rates. He is able to source four plots    #
#  of land for the experiment, but each has been grazed differently     #
#  prior to the experiment, which may affect clover accumulation. The   #
#  botanist decided to use a randomized block design and used each of   #
#  the sowing rates on a randomly selected quarter of each plot.        #

## Clover data
accum <- c(155,255,505,632,123,406,564,416,68,416,662,379,62,75,
           362,564)
plotnum <- c(rep(c("1","2","3","4"), each=4))
s.rate <- rep(c("3.6","6.6","10.2","13.5"),4)
cl.data <- data.frame(accum,plotnum,s.rate)

## Plot means of explanatory variables
ggplot(cl.data, aes(x=s.rate, y=accum)) + 
    stat_summary(fun.y=mean, geom="point") +
    stat_summary(fun.y=mean, geom="line", aes(group=1)) +
    stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)

ggplot(cl.data, aes(x=s.rate, y=accum, colour=plotnum)) + 
    stat_summary(fun.y=mean, geom="point") +
    stat_summary(fun.y=mean, geom="line", aes(group=plotnum)) 

## Levene's test
leveneTest(accum ~ s.rate, data=cl.data)

## QQ plot
mod <- lm(accum ~ s.rate + plotnum, data=cl.data)
X <- data.frame(resid = residuals(mod))
y <- quantile(X$resid, c(0.25, 0.75)) 
x <- qnorm(c(0.25, 0.75))  
slope <- diff(y)/diff(x) 
int <- y[1] - slope*x[1]   
ggplot(X, aes(sample = resid)) + stat_qq() + 
    geom_abline(intercept=int, slope=slope) 

## ANOVA test for accumulation for sowing rate and block
tw.aov.test <- aov(accum ~ s.rate + plotnum, data=cl.data)
summary(tw.aov.test)

## Tukey's HSD intervals for sowing rate
TukeyHSD(tw.aov.test, which="s.rate")

#########################################################################

##########################
#  ANOVA considerations  #
##########################

#  How many explanatory variables of interest are there?                #
#  Are there any related covariates?                                    #
#  Are the individuals independent over all populations?                #
#  Does the normality assumption hold?                                  #
#  Does the constant variance assumption hold?                          #
#  Is the design balanced?                                              #
#  Are the factors fixed or random?                                     #

#########################################################################