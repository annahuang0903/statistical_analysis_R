
#########################################################################
#                                                                       #
#  Regression in R                                                      #
#                                                                       #
#########################################################################

library(ggplot2)

#  Understanding the relationships between variables is a valuable      #
#  skill that can be leveraged to describe the effect of one or more    #
#  variables on another, to estimate average values for varying         #
#  subpopulations, and to predict future values. All regression models  #
#  use explanatory variables to better understand the response          #
#  variable.                                                            #
#                                                                       #
#  There are many types of regression models that apply to various      #
#  different types of data. The two main categories of regression       #
#  models are linear regression and logistic regression. Linear         #
#  regression models attempt to understand and predict values from a    #
#  continuous response variable while logistic regression models        #
#  attempt to understand and predict values from a response variable    #
#  with specified levels, most commonly an indicator variable.          #
#                                                                       #
#  Any combination of types of variables can be used as explanatory     #
#  variables. Regression models can be built with all continuous        #
#  explanatory variables that can be transformed and combined further,  #
#  with all categorical explanatory variables turned into sets of       #
#  dummy variables, or a combination of continuous and categorical      #
#  explanatory variables that can be transformed and combined further.  #

#######################
#  Linear regression  #
#######################

#  Linear regression is typically separated into two subcategories -    #
#  simple linear regression and multiple linear regression. Simple      #
#  linear regression uses only one explanatory variable while multiple  #
#  linear regression uses several explanatory variables. Hence, simple  #
#  linear regression is a special case of multiple linear regression.   #
#  However, the underlying assumptions and background of linear         #
#  regression are most straight-forward in two dimensions, so simple    #
#  linear regression is usually introduced first.                       #
#                                                                       #
#  First, assume that the explanatory and response variables are        #
#  continuous variables. The idea behind linear regression is that      #
#  there is an underlying, but unknown, population relationship         #
#  between the explanatory and response variables. Since it is          #
#  unreasonable to expect that each individual in the population        #
#  sharing the same value(s) for the explanatory variable(s) will       #
#  have the same value for the response variable, this underlying       #
#  population relationship describes the relationship between the       #
#  explanatory variable(s) and the mean of the response variable in     #
#  each subpopulation.                                                  #
#                                                                       #
#  In the simple linear regression case, we assume that there is a      #
#  straight line relationship with unknown slope and intercept values   #
#  that relate the mean of the response values at each value of the     #
#  explanatory variable and the explanatory variable:                   #
#     mu_y|x = beta_0 + beta_1 * x                                      #
#                                                                       #
#  In order to express the model in terms of the values of the          #
#  response variables themselves, an error term is added that serves    #
#  to shift an individual's value of the response variable away from    #
#  the population mean of their subpopulation. Making this adjustment   #
#  yields the regression model:                                         #
#     y = beta_0 + beta_1 * x + epsilon                                 #
#                                                                       #
#  In order to appropriately use linear regression, there are four      #
#  assumptions that must hold.                                          #
#     1) The errors have a mean of zero.                                #
#     2) The errors have a constant variance.                           #
#     3) The errors are independent.                                    #
#     4) The errors are normally distributed.                           #
#                                                                       #
#  As all of the population quantities in the linear regression model   #
#  are unknown, a sample is drawn from the population and used to       #
#  estimate them. The prediction equation is:                           #
#     y.hat = b_0 + b_1 * x                                             #
#                                                                       #
#  The population error terms are estimated by finding the difference   #
#  between the observed value of the response variable and the          #
#  predicted value of the response variable, or the residuals:          #
#     e = y - y.hat                                                     #
#                                                                       #
#  The usual method that is used to determine the estimates or          #
#  coefficients (b_0 and b_1) is called least squares estimation        #
#  because the sum of the squared residuals (or SSE) is minimized.      #
#                                                                       #
#  The first step of linear regression analysis is to create and        #
#  evaluate any relevant scatterplots.  For example, the chief          #
#  logistics officer for a small craft brewery is evaluating their      #
#  delivery efficiency and records the number of cases delivered        #
#  and the delivery time, which includes unloading, transportation to   #
#  the appropriate area, and any necessary stocking time.               #

## Delivery time data
cases <- c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
dtime <- c(16.7,11.5,12.0,14.9,13.8,18.1,8.0,17.8,79.2,21.5,40.3,21.0,
           13.5,19.8,24.0,29.0,15.4,19.0,9.5,35.1,17.9,52.3,18.8,19.8,10.8)
deliv <- data.frame(cases,dtime)

## Scatterplot
dtime.plot <- ggplot(deliv, aes(x=cases, y=dtime)) + geom_point()
dtime.plot

#  If it seems as though linear regression would be appropriate for     #
#  these data, the linear model can be fit using the function lm().     #
#  This function requires the model that the user would like fit and    #
#  the data in which the variables are found.                           #

## Fitting simple linear regression model
dtime.fit <- lm(dtime ~ cases, data=deliv)
dtime.fit

#  The coefficients of the model are printed when a linear model is     #
#  run, but they can also be called separately and saved. It is also    #
#  possible to print and save the fitted or predicted values (y.hat)    #
#  and the residuals (e).                                               #

## Saving the coefficients
dtime.coef <- dtime.fit$coefficients
dtime.coef

dtime.coef2 <- coef(dtime.fit)
dtime.coef2

## Saving the predicted values
dtime.pred <- dtime.fit$fitted.values
dtime.pred

dtime.pred2 <- fitted(dtime.fit)
dtime.pred2

## Saving the residuals
dtime.res <- dtime.fit$residual
dtime.res

dtime.res2 <- residuals(dtime.fit)
dtime.res2

## Adding fitted regression line to scatterplot
dtime.plot + geom_smooth(method=lm, colour="black")
dtime.plot + geom_abline(intercept=dtime.coef[1], slope=dtime.coef[2])

#  In the case of multiple linear regression, the background and        #
#  assumptions remain the same, but are generalized to account for      #
#  multiple explanatory variables.                                      #
#                                                                       #
#  There is still a relationship between the means of the response      #
#  values in each subpopulation and the explanatory variables:          #
#     mu_y|(x_1,...,x_k) = beta_0 + beta_1 * x_1 + ... + beta_k * x_k   #
#                                                                       #
#  The regression model is expanded to account for additional           #
#  explanatory variables:                                               #
#     y = beta_0 + beta_1 * x_1 + ... + beta_k * x_k + epsilon          #
#                                                                       #
#  The prediction equation reflects the regression model:               #
#     y.hat = b_0 + b_1 * x_1 + ... + b_k * x_k                         #
#                                                                       #
#  The residuals are still found by subtracting the predicted value     #
#  from the observed value and are still used as the basis of least     #
#  squares estimation.                                                  #
#                                                                       #
#  However, with multiple explanatory variables come other              #
#  considerations, such as the usefulness of the different variables,   #
#  the correct functional form of each variable, possible interactions  #
#  between the variables, redundancy of any of the variables, and       #
#  overfitting the model.                                               #
#                                                                       #
#  Suppose that in addition to the number of cases, the logistics       #
#  officer also has data on the distance in feet that the driver must   #
#  transport the cases at each location.                                #

## Additional data 
dist <- c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,
          776,200,132,36,770,140,810,450,635,150)
deliv <- data.frame(deliv,dist)

## Scatterplots
dtime.plot1 <- ggplot(deliv, aes(x=cases, y=dtime)) + geom_point()
dtime.plot1

dtime.plot2 <- ggplot(deliv, aes(x=dist, y=dtime)) + geom_point()
dtime.plot2

## Fitting the multiple linear regression model
dtime.fitm <- lm(dtime ~ cases + dist, data=deliv)
dtime.fitm <- lm(dtime ~ ., data=deliv)
dtime.fitm

#  The overall F test indicates whether the model, as a whole, is       #
#  useful. The null hypothesis is that all of the slope parameters      #
#  (beta_1, ., beta_k) are all equal to zero and the alternative        #
#  hypothesis is that at least one of the slope parameters is not       #
#  equal to zero. The overall F test is typically shown in the          #
#  Analysis of Variance (ANOVA) table as the test statistic is          #
#  determined from decomposition of variance elements.                  #
#                                                                       #
#  The total variation in the response variable (total sum of squares)  #
#  can be separated into the variation that can be explained by the     #
#  regression model (regression sum of squares) and the variation that  #
#  remains unexplained (error sum of squares) or SST = SSR + SSE.       #
#  These values, along with their degrees of freedom (n-1, k, n-k-1)    #
#  and their mean squares (the sum of squares divided by the degrees    #
#  of freedom) are the elements that make up the ANOVA table.           #
#  Typically, the total mean square (MST) is not shown in the table     #
#  to highlight the fact that while the sum of squares and degrees      #
#  of freedom have an additive relationship, the mean squares do not.   #
#                                                                       #
#  The overall F test statistic is found by dividing the regression     #
#  mean square by the error mean square and follows an F distribution   #
#  with k and n-k-1 degrees of freedom.                                 #

#########################################################################

library(ggplot2)
library(car)

cases <- c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
dtime <- c(16.7,11.5,12.0,14.9,13.8,18.1,8.0,17.8,79.2,21.5,40.3,21.0,
           13.5,19.8,24.0,29.0,15.4,19.0,9.5,35.1,17.9,52.3,18.8,19.8,10.8)
dist <- c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,
          776,200,132,36,770,140,810,450,635,150)
deliv <- data.frame(cases,dist,dtime)

dtime.fit <- lm(dtime ~ cases, data=deliv)
dtime.fitm <- lm(dtime ~ cases + dist, data=deliv)

###################################
#  Linear regression (continued)  #
###################################

#  The function anova() will produce an ANOVA table for a given linear  #
#  regression model. The default table produced separates the ANOVA     #
#  table such that there is a line for each explanatory variables       #
#  instead of one line for the regression as a whole. This function     #
#  can be used to produce the consolidated regression line found in     #
#  the general ANOVA table by adding the null model (with no            #
#  explanatory variables) as an input.                                  #

## ANOVA table with separated lines
anova(dtime.fitm)

## Regression line from typical ANOVA table
dt.null <- lm(dtime~1, data=deliv)
anova(dt.null, dtime.fitm)

#  When using simple linear regression, the ANOVA table generated by    #
#  the anova() function will be the typical ANOVA table.                #

## ANOVA table for SLR
anova(dtime.fit)

#  The function aov() will produce the same results as the anova()      #
#  function.                                                            #
#                                                                       #
#  The overall F test gives an indication if the current model is       #
#  significant overall, but does not give any information about which   #
#  of the variables are providing significant information. The          #
#  individual variables' significance is tested using the individual    #
#  t-test. The null hypothesis for this test is that the true slope of  #
#  the variable being tested (beta_j) is zero and the alternative       #
#  hypothesis is that the true slope is non-zero. The test statistic    #
#  is the standardized coefficient, which follows a t distribution      #
#  with n-k-1 degrees of freedom.                                       #
#                                                                       #
#  The individual t-test for each explanatory variable can be           #
#  displayed by using the summary() function.                           #

## Summary of fitted model
summary(dtime.fitm)

#  Also shown in the summary output are the coefficient of              #
#  determination (R^2) and the adjusted coefficient of determination.   #
#  The coefficient of determination is found by dividing SSR by SST     #
#  (or by subtracting the quotient of SSE and SST from one), which      #
#  yields the proportion of the total variation in the response         #
#  variable that can be explained by the variation explained by the     #
#  regression model. The downside of this quantity is that it           #
#  increases as additional variables are added to the model,            #
#  regardless of how significant they are in explaining the response    #
#  variable. Thus, the adjusted coefficient of determination penalizes  #
#  for each variable included in the model, by subtracting the          #
#  quotient of MSE and MST from one.                                    #
#                                                                       #
#  Many of the values displayed in the summary can be isolated and      #
#  saved.                                                               #

summary(dtime.fitm)$coefficients
summary(dtime.fitm)$coefficients[,4]
summary(dtime.fitm)$sigma
summary(dtime.fitm)$fstatistic
summary(dtime.fitm)$r.squared
summary(dtime.fitm)$adj.r.squared

#  With multiple linear regression, several different explanatory       #
#  variables are used to estimate the response variable. Multiple       #
#  linear regression is flexible in that additional explanatory         #
#  variables can take several forms. One type of explanatory variables  #
#  that can be used is quadratic functions of another explanatory       #
#  variable, which allows inclusion of explanatory variables that have  #
#  a curved relationship with the response variable.                    #

## Scatterplots of delivery time vs. cases and distance
ggplot(deliv, aes(x=cases, y=dtime)) + geom_point()
ggplot(deliv, aes(x=dist, y=dtime)) + geom_point()

## Adding quadratic terms 
dtime.fit2.1 <- lm(dtime ~ cases + I(cases^2) + dist + I(dist^2), 
                   data=deliv)
summary(dtime.fit2.1)

dtime.fit2.2 <- lm(dtime ~ poly(cases,2) + poly(dist,2), data=deliv)
summary(dtime.fit2.2)

#  A major issue that can be present when using multiple explanatory    #
#  variables is multicollinearity. Explanatory variables are            #
#  multicollinear when they are related to each other. As is expected,  #
#  adding a quadratic term of a linear term already present in the      #
#  model also adds multicollinearity to the model. Multicollinearity    #
#  can be assessed with the Variance Inflation Factor (VIF), which is   #
#  a measure of how much of the variation in each explanatory variable  #
#  can be explained by the other explanatory variables in the model.    #

## Assessing multicollearity
vif(dtime.fit2.1)
vif(dtime.fit2.2)
vif(dtime.fitm)

#  Another type of explanatory variable that can be added to a          #
#  multiple linear regression model is an interaction. Interaction      #
#  terms reflect the effect of one explanatory variable on the          #
#  relationship between the other explanatory variable and the          #
#  response variable.                                                   #

## Adding interaction terms 
dtime.fit3.1 <- lm(dtime ~ cases + dist + cases*dist, data=deliv)
dtime.fit3.1 <- lm(dtime ~ cases*dist)
summary(dtime.fit3.1)

dtime.fit3.2 <- lm(dtime ~ poly(cases,2) + poly(dist,2) + 
                       cases*dist, data=deliv)
summary(dtime.fit3.2)

#########################################################################

library(ggplot2)

cases <- c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
dtime <- c(16.7,11.5,12.0,14.9,13.8,18.1,8.0,17.8,79.2,21.5,40.3,21.0,
           13.5,19.8,24.0,29.0,15.4,19.0,9.5,35.1,17.9,52.3,18.8,19.8,10.8)
dist <- c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,
          776,200,132,36,770,140,810,450,635,150)
deliv <- data.frame(cases,dist,dtime)

###################################
#  Linear regression (continued)  #
###################################

#  In addition to quantitative variables and their functions, sets of   #
#  dummy variables can be added to regression models to incorporate     #
#  categorical variables into linear regression. For a categorical      #
#  variable that has only two levels, one dummy (indicator) variable    #
#  is used. This dummy variable takes the value zero for one of the     #
#  levels (the baseline) and the value one for the other level. Once    #
#  a dummy variable is defined, it is added to the regression model     #
#  like any quantitative variable.                                      #
#                                                                       #
#  For a categorical variable that has more than two levels, a set of   #
#  dummy variables is required to represent the variable in the         #
#  regression model. For a categorical variable with m levels, a set    #
#  of m-1 dummy variables is created. The level made up of all zeros    #
#  is the baseline level.                                               #
#             x_1   x_2   x_3                                           #
#   Level 1     0     0     0                                           #
#   Level 2     1     0     0                                           #
#   Level 3     0     1     0                                           #
#   Level 4     0     0     1                                           #
#                                                                       #
#  Suppose that the logistics officer also has information on whether   #
#  the location requires the use of an elevator for the delivery or     #
#  not. Additionally, the office knows whether the driver delivers      #
#  only, partially stocks, or fully stocks at each location.            #

## Additional data
elev <- c("N","N","N","N","N","E","N","E","E","E","N","E","N","E","E",
          "N","E","E","N","E","N","E","E","E","N")
stock <- c("DO","PS","DO","PS","DO","PS","DO","FS","FS","FS","DO","PS",
           "PS","FS","DO","PS","DO","FS","DO","FS","PS","PS","PS","PS",
           "DO")
deliv <- data.frame(deliv,elev,stock)

## Scatterplots
ggplot(deliv, aes(x=elev, y=dtime)) + geom_point()
ggplot(deliv, aes(x=stock, y=dtime)) + geom_point()

## Adding dummy variables to model
dtime.fitd <- lm(dtime ~ cases + dist + elev + stock, data=deliv)
dtime.fitd

## Modifying baseline level
deliv$elev <- relevel(deliv$elev, ref="N")

dtime.fitd <- lm(dtime ~ cases + dist + elev + stock, data=deliv)
dtime.fitd

#  One major difference between quantitative variables and sets of      #
#  dummy variables is the interpretation of the corresponding           #
#  coefficients. While the coefficients corresponding to quantitative   #
#  variables represent a slope, the coefficients corresponding to       #
#  dummy variables represent the difference in the response variable    #
#  due to the level represented by that particular dummy variable in    #
#  relation to the baseline level.                                      #
#                                                                       #
#  Another major difference between quantitative and categorical        #
#  variables is how their significance in the model is tested. When a   #
#  categorical variable has more than two levels, a set of dummy        #
#  variables represents the variable. Thus, the set of dummy variables  #
#  must be tested together using the partial F test. The partial F      #
#  test determines whether a subset of variables as a whole is useful   #
#  to the model. The null hypothesis for this test is that all of the   #
#  parameters associated with the variables in the subset are equal to  #
#  zero and the alternative hypothesis is that at least one of the      #
#  parameters in the subset is not equal to zero. The partial F test    #
#  uses quantities from the full model, which includes all of the       #
#  variables in the subset, and from the reduced model, which assumes   #
#  the null hypothesis to be true and thus excludes all of the          #
#  variables in the subset being tested. To conduct the partial F       #
#  test, both the full and reduced models need to be defined and        #
#  provided as inputs to the anova() function.                          #

## Testing one dummy variable
summary(dtime.fitd)

## Testing sets of dummy variables
full <- dtime.fitd
red <- lm(dtime ~ cases + dist + elev, data=deliv)

anova(red,full)

#  Interactions can exist between quantitative and categorical          #
#  variables. In this case, the quantitative variable is multiplied     #
#  by each dummy variable in the set. When testing this type of         #
#  interaction, a partial F test must be used to test the set of        #
#  interaction terms.                                                   #
#                                                                       #
#  Recall that one of the considerations introduced when adding         #
#  multiple explanatory variables is the risk of overfitting the data.  #
#  It is best practice to simplify the model as much as possible while  #
#  keeping useful variables. One common way to simplify a regression    #
#  model is to remove variables that are not significant from the       #
#  model. Variables are typically removed one at a time to account for  #
#  the fact that relationships between the explanatory variables can    #
#  cause p-values to change.                                            #

## Combine all types of variables
summary(lm(dtime ~ cases + I(cases^2) + dist + I(dist^2) + 
               cases*dist + elev + stock, data=deliv))

## Remove the most insigificant variables
summary(lm(dtime ~ cases + I(cases^2) + dist + I(dist^2) + 
               cases*dist + elev + stock, data=deliv))

#########################################################################

install.packages("leaps")

library(leaps)
library(ggplot2)

cases <- c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
dtime <- c(16.7,11.5,12.0,14.9,13.8,18.1,8.0,17.8,79.2,21.5,40.3,21.0,
           13.5,19.8,24.0,29.0,15.4,19.0,9.5,35.1,17.9,52.3,18.8,19.8,10.8)
dist <- c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,
          776,200,132,36,770,140,810,450,635,150)
elev <- c("N","N","N","N","N","E","N","E","E","E","N","E","N","E","E",
          "N","E","E","N","E","N","E","E","E","N")
stock <- c("DO","PS","DO","PS","DO","PS","DO","FS","FS","FS","DO","PS",
           "PS","FS","DO","PS","DO","FS","DO","FS","PS","PS","PS","PS",
           "DO")
deliv <- data.frame(cases,dist,elev,stock,dtime)

###################################
#  Linear regression (continued)  #
###################################

#  If a data set has a large number of potential explanatory            #
#  variables, model building procedures can be used to narrow down      #
#  to the most promising explanatory variables. There are two main      #
#  categories of model building procedures - iterative and comparative. #
#  The iterative procedures iteratively add and/or remove variables     #
#  to/from the model based on their individual significance. The        #
#  comparative procedures use various measures to compare all the       #
#  potential model combinations and identify those that are most        #
#  promising with respect to the specified comparative measure. Even    #
#  with a small number of potential explanatory variables, these        #
#  model building procedures can be used to identify potential best     #
#  models to investigate further.                                       #
#                                                                       #
#  The iterative model building processes - forward, backward, and      #
#  stepwise - are run using the step() function. This function          #
#  requires the model to begin using, the minimum model that the user   #
#  will accept, the full model including all the variables that the     #
#  user wants to consider for inclusion in the model, and which type    #
#  of iterative procedure to use.                                       #

## Define minimum and maximum allowable models
nm <- lm(dtime ~ 1, data=deliv)
fm <- lm(dtime ~ ., data=deliv)

## Stepwise selection
step(nm, scope=list(lower=nm, upper=fm), direction="both")

## Backward selection
step(fm, scope=list(lower=nm, upper=fm), direction="backward")

#  The comparative model building procedures are run using the          #
#  regsubsets() function in the leaps package. The inputs required are  #
#  the formula for the full model, the data set to be used, and the     #
#  number of models of each size the user would like. The summary       #
#  function can be used to extract the models identified along with     #
#  several measures.                                                    #

## Identify the 10 best models of each size
bestm <- regsubsets(dtime ~ ., data=deliv, nbest=10)
summary(bestm)

## Extract the measures associated with each model
mod <- summary(bestm)$which
mod.adjr2 <- summary(bestm)$adjr2
mod.cp <- summary(bestm)$cp
mod.rss <- summary(bestm)$rss

## Create data frame of realistic best models and measures
bestm.comp <- data.frame(mod, mod.adjr2, mod.cp, mod.rss)
bestm.comp

bestm.comp <- bestm.comp[which(bestm.comp$stockFS==bestm.comp$stockPS),]
bestm.comp

## Determine the best models in terms of Mallows Cp
bestm.comp[order(bestm.comp$mod.cp),]

#  Once the best model(s) have been identified, the assumptions must    #
#  be checked. Recall the four assumptions:                             #
#     1) The errors have a mean of zero.                                #
#     2) The errors have a constant variance.                           #
#     3) The errors are independent.                                    #
#     4) The errors are normally distributed.                           #
#                                                                       #
#  As the residuals (difference between the observed and predicted      #
#  values) estimate the errors, the residuals from any model under      #
#  consideration should be calculated and analyzed. Various plots       #
#  created with the residuals are used to verify the model assumptions. #
#  The residuals are plotted against the predicted values, against      #
#  each explanatory variable in the model, and against time or          #
#  observation number, if applicable. Additionally a Q-Q plot or        #
#  normal probability plot of the residuals is evaluated.               #
#                                                                       #
#  The Q-Q plot will show a straight-line relationship if the           #
#  residuals follow a normal distribution. The points may not follow    #
#  the reference line exactly, but should not strongly deviate from it. #
#                                                                       #
#  The remaining residual plots should all show a random scattering     #
#  of the residuals around zero in a horizontal band. If they are not   #
#  centered around zero, then the mean zero assumption is violated.     #
#  If the residuals show any sort of fanning in and/or fanning out      #
#  pattern, the constant variance assumption is violated. If the        #
#  residuals show any sort of pattern over time or observation number,  #
#  then the independence assumption is violated.                        #

## Potential model
mod1 <- lm(dtime ~ cases + dist, data=deliv)

## Residuals vs. predicted plot
ggplot(mod1, aes(x=.fitted, y=.resid)) + geom_point() + 
    geom_hline(yintercept=0, linetype="dashed")

## Residuals vs. explanatory variables plot
ggplot(mod1, aes(x=cases, y=.resid)) + geom_point() +
    geom_hline(yintercept=0, linetype="dashed")

ggplot(mod1, aes(x=dist, y=.resid)) + geom_point() +
    geom_hline(yintercept=0, linetype="dashed")

## Q-Q plot
X <- data.frame(resid = residuals(mod1))

y <- quantile(X$resid, c(0.25, 0.75)) 
x <- qnorm( c(0.25, 0.75))  
slope <- diff(y)/diff(x) 
int <- y[1] - slope*x[1]   

ggplot(X, aes(sample = resid)) + stat_qq() + 
    geom_abline(intercept=int, slope=slope) 

## Potential model
mod2 <- lm(dtime ~ cases + dist + stock, data=deliv)

## Residuals vs. predicted plot
ggplot(mod2, aes(x=.fitted, y=.resid)) + geom_point() + 
    geom_hline(yintercept=0, linetype="dashed")

## Residuals vs. explanatory variables plot
ggplot(mod2, aes(x=cases, y=.resid)) + geom_point() +
    geom_hline(yintercept=0, linetype="dashed")

ggplot(mod2, aes(x=dist, y=.resid)) + geom_point() +
    geom_hline(yintercept=0, linetype="dashed")

ggplot(mod2, aes(x=stock, y=.resid)) + geom_point() +
    geom_hline(yintercept=0, linetype="dashed")

## Q-Q plot
X <- data.frame(resid = residuals(mod2))

y <- quantile(X$resid, c(0.25, 0.75)) 
x <- qnorm( c(0.25, 0.75))  
slope <- diff(y)/diff(x) 
int <- y[1] - slope*x[1]   

ggplot(X, aes(sample = resid)) + stat_qq() + 
    geom_abline(intercept=int, slope=slope) 

#  If the assumptions of a model are not met, there are several         #
#  possible courses of action - variables may need to be added to or    #
#  removed from the model, the functional form of a variable included   #
#  in the model may need to be adjusted, and/or transformations can     #
#  be made to either the response variable or any of the explanatory    #
#  variables.                                                           #

## Potential model
mod3 <- lm(dtime ~ cases + dist + I(dist^2), data=deliv)

## Residuals vs. predicted plot
ggplot(mod3, aes(x=.fitted, y=.resid)) + geom_point() + 
    geom_hline(yintercept=0, linetype="dashed")

## Residuals vs. explanatory variables plot
ggplot(mod3, aes(x=cases, y=.resid)) + geom_point() +
    geom_hline(yintercept=0, linetype="dashed")

ggplot(mod3, aes(x=dist, y=.resid)) + geom_point() +
    geom_hline(yintercept=0, linetype="dashed")

ggplot(mod3, aes(x=I(dist^2), y=.resid)) + geom_point() +
    geom_hline(yintercept=0, linetype="dashed")

## Q-Q plot
X <- data.frame(resid = residuals(mod3))

y <- quantile(X$resid, c(0.25, 0.75)) 
x <- qnorm( c(0.25, 0.75))  
slope <- diff(y)/diff(x) 
int <- y[1] - slope*x[1]   

ggplot(X, aes(sample = resid)) + stat_qq() + 
    geom_abline(intercept=int, slope=slope) 

#########################################################################

library(car)
library(ggplot2)

cases <- c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
dtime <- c(16.7,11.5,12.0,14.9,13.8,18.1,8.0,17.8,79.2,21.5,40.3,21.0,
           13.5,19.8,24.0,29.0,15.4,19.0,9.5,35.1,17.9,52.3,18.8,19.8,10.8)
dist <- c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,
          776,200,132,36,770,140,810,450,635,150)
elev <- c("N","N","N","N","N","E","N","E","E","E","N","E","N","E","E",
          "N","E","E","N","E","N","E","E","E","N")
stock <- c("DO","PS","DO","PS","DO","PS","DO","FS","FS","FS","DO","PS",
           "PS","FS","DO","PS","DO","FS","DO","FS","PS","PS","PS","PS",
           "DO")
deliv <- data.frame(cases,dist,elev,stock,dtime)

mod3 <- lm(dtime ~ cases + dist + I(dist^2), data=deliv)

###################################
#  Linear regression (continued)  #
###################################

#  In addition to the regression assumptions, outliers and influential  #
#  points should be checked in any model under consideration. Outliers  #
#  are points that are significantly separated from the majority of the #
#  data either vertically (in y) or horizontally (in x). Influential    #
#  points are points that significantly affect the location of the      #
#  regression line when removed from the analysis.                      #
#                                                                       #
#  Outliers in x are typically identified by leverage. Outliers in y    #
#  are typically identified by studentized residuals. Influential      #
#  points can be identified by several quantities such as RStudent      #
#  residuals, DFFITS, DFBETAS, and Cook's D, among others.              #

## Identifying outliers in y
stud.resid <- rstandard(mod3)
stud.resid

## Identifying outliers in x and influential points
summary(influence.measures(mod3))

## Identifying influential points
Rstud <- rstudent(mod3)
Rstud

#  Outliers and influential points can be present in a model for        #
#  several different reasons and not all of them warrant their removal. #
#  These points can reflect a recording error, a measurement error,     #
#  an unusual event, or valid data. Identified points should be         #
#  thoroughly analyzed and a conscious decision should be made to       #
#  leave them or remove them.                                           #

## Davis data set
ggplot(Davis, aes(x=weight, y=repwt)) + geom_point()
which(Davis$weight>160)
Davis[1:15,]

#  Once a model is chosen for analysis, that model can provide          #
#  information regarding the populations of response variable for       #
#  each subpopulation and information that can be used to predict       #
#  future values. The predicted value from a model both estimates       #
#  the average value of the response variable in its subpopulation      #
#  and predicts a future value from the subpopulation. As such, the     #
#  predicted value is the center of both the confidence interval to     #
#  estimate the average and the prediction interval to predict a        #
#  future value. Since there is uncertainty in where the true future    #
#  value will fall in the population of response variable values, the   #
#  prediction interval will always be wider than the confidence         #
#  interval.                                                            #
#                                                                       #
#  Both the confidence interval and prediction interval for a           #
#  particular subpopulation can be found using the predict.lm()         #
#  function. This function requires the object of the model to be       #
#  used and which type of interval is desired. Additionally, the level  #
#  option can be used to specify a confidence level other than 95%.     #
#  By default, this function will provide the interval requested for    #
#  the subpopulations in the data set used to build the model. To       #
#  produce an interval for a different subpopulation, a data frame      #
#  with the values defining the subpopulation should be supplied with   #
#  the newdata option.                                                  #
#                                                                       #
#  Suppose that the logistics officer is interested in estimating the   #
#  average delivery time for locations that order 7 cases and has a     #
#  distance of 560 feet to travel for delivery.                         #

predict.lm(mod3, interval="confidence")

#  Suppose that the logistics officer is interested in a conservative   #
#  estimate for the time required for the next delivery at a location   #
#  that orders 15 cases and has a distance of 452 feet to travel for    #
#  delivery.                                                            #

new_deliv <- data.frame(cases=15, dist=452)
predict.lm(mod3, newdata=new_deliv, interval="prediction")

#  Suppose that the logistics officer is interested in a conservative   #
#  estimate for the time required for the next delivery at a location   #
#  that orders 55 cases and has a distance of 2050 feet to travel for   #
#  delivery.                                                            #

new_deliv2 <- data.frame(cases=55, dist=2050)
predict.lm(mod3, newdata=new_deliv2, interval="prediction")

#  Does this estimate give the logistics officer useful information?    #

#########################################################################

install.packages("lmtest")
install.packages("boot")

library(lmtest)
library(boot)
library(car)
library(ggplot2)

#########################
#  Logistic regression  #
#########################

#  Recall that logistic regression uses explanatory variables to help   #
#  understand a categorical response variable. Typically the response   #
#  variable in logistic regression is binary, but multinomial logistic  #
#  regression can be used if the response variable has more than two    #
#  categories.                                                          #
#                                                                       #
#  One of the main differences between linear regression and logistic   #
#  regression is the assumption that the explanatory variables and      #
#  response variable are linearly related. This assumption does not     #
#  make sense for a grouped response variable. Another major            #
#  difference between linear and logistic regression is the type of     #
#  estimate that results from the estimated regression model. In        #
#  linear regression, the regression model directly estimates the       #
#  value of the response variable. In logistic regression, the          #
#  regression model estimates the probability of the response variable  #
#  being a 1. To be able to use regression techniques for categorical   #
#  response variables, a logit transformation is used to linearize the  #
#  relationship. The logistic regression model can be written in two    #
#  ways:                                                                #
#   ln{P(y)/[1-P(y)]} = beta_0 + beta_1*x_1 + . + beta_k*x_k + epsilon  #
#   P(y) = 1/[1+exp{-(beta_0 + beta_1*x_1 + . + beta_k*x_k + epsilon}]  #
#                                                                       #
#  Logistic regression is run using the glm() function. This function   #
#  requires the regression model, the relevant data, and the            #
#  distribution family, which is binomial for logistic regression.      #
#                                                                       #
#  GLM is an acronym for Generalized Linear Models, which is a large    #
#  umbrella that encompasses many types of modeling. Linear regression  #
#  can also be conducted using the glm() function by specifying the     #
#  family to be Gaussian (normal).                                      #
#                                                                       #
#  Does BMI have an effect on whether people who regularly exercise     #
#  accurately report their weight? Define accurately reporting to be    #
#  within 1 kilogram.                                                   #

## Prepare Davis data
dat.Davis <- Davis
rec.er <- Davis[12,2:3]
dat.Davis[12,2:3] <- rev(rec.er)
dat.Davis <- na.omit(dat.Davis)
dat.Davis$acc <- ifelse(abs(dat.Davis$weight-dat.Davis$repwt)>1,0,1)
dat.Davis$bmi <- dat.Davis$weight/(dat.Davis$height/100)^2
dat.Davis[1:15,]

## Run logistic regression
logr <- glm(acc ~ bmi, data=dat.Davis, family=binomial())
logr

## Logistic regression line
ggplot(dat.Davis, aes(x=weight, y=acc)) + geom_point() + 
    stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

#  With the differences in the nature of logistic regression, the       #
#  overall F test and individual t-test do not apply. Instead, the      #
#  likelihood ratio test is used to determine the significance of       #
#  the overall model and the Wald test is used to determine the         #
#  significance of the individual variables.                            #
#                                                                       #
#  Every regression model, including linear regression models, has a    #
#  likelihood function, which is usually written as the log of the      #
#  likelihood for simpler calculations. The log likelihood is the       #
#  value of the corresponding likelihood function for the particular    #
#  set of data that is being used and is a measure of how much          #
#  information the model explains once it is fit. The deviance of a     #
#  regression model is the log likelihood multiplied by -2. The         #
#  likelihood ratio assumes that the model under consideration is not   #
#  significantly different from a baseline model, which is usually the  #
#  model with only the intercept. This hypothesis is tested by          #
#  comparing the deviance of the model under consideration to the       #
#  deviance of the baseline model. The difference of the deviances      #
#  follows a chi-squared distribution.                                  #
#                                                                       #
#  The function lrtest() in the lmtest package will produce the         #
#  chi-squared test statistic and p-value for the likelihood ratio      #
#  test. The only required input is the output of the model to be       #
#  tested.                                                              #

## Likelihood ratio test for overall significance
lrtest(logr)

#  The hypotheses and test statistic of the Wald test are identical     #
#  to those of the individual t-test . However, the test statistic is   #
#  compared to the standard normal distribution instead of the t        #
#  distribution to determine the p-values. These test-statistics and    #
#  p-values are output by the summary() function.                       #

## Wald test for individual variables
summary(logr)

#  When a set of dummy variables is being used in logistic regression,  #
#  the likelihood ratio test can be used to compare the full and        #
#  reduced models. To conduct a nested likelihood ratio test, the two   #
#  model outputs are provided as inputs.                                #

#  As the form of the model has changed and the relationship between    #
#  the logit function and the explanatory variables is curved, the      #
#  model parameters do not represent slopes. The exponential of the     #
#  parameters represent odds ratios, which is an indication of the      #
#  change in odds that results from a one unit increase in the          #
#  explanatory variable. The odds ratio is the quotient of the odds     #
#  after a change of one unit and the original odds. If the odds ratio  #
#  is greater than one, then the odds of the response variable being    #
#  one increases. The odds are the quotient of the probability of an    #
#  event occurring and the probability of the event not occurring. If   #
#  the exponential of a coefficient is greater than 1, the odds are     #
#  increasing, which implies that the probability of the event          #
#  occurring is increasing. However, the probability of the event       #
#  occurring is not increasing by the value of the exponential of the   #
#  coefficient.                                                         #

## Odds ratio for BMI
exp(logr$coefficients)

## Confidence intervals for odds ratios
exp(confint(logr))

#  As logistic regression is not as dependent on model assumptions as   #
#  linear regression, model diagnostics are not as extensive. In the    #
#  case of many potential explanatory variables, it is important to     #
#  check for possible multicollinearity with VIFs. Outliers and         #
#  influential points can be identified with residuals, leverage, and   #
#  Cook's D. In logistic regression there are two types of residuals    #
#  - deviance residuals and Pearson residuals - both of which can be    #
#  standardized. The function glm.diag() in the boot package will       #
#  calculate these measures.                                            #

## Outlier and influential point measures
resid.Davis <- dat.Davis[,c("acc","bmi")]
resid.Davis$lev <- glm.diag(logr)$h
resid.Davis$st.devr <- glm.diag(logr)$rd
resid.Davis$st.pr <- glm.diag(logr)$rp
resid.Davis$CookD <- glm.diag(logr)$cook
resid.Davis[1:15,]

#  Recall that logistic regression predicts the probability that the    #
#  outcome of the response variable will be a one. If the objective of  #
#  the regression is to yield 0/1 predictions, the user must determine  #
#  a manner in which to turn the predicted probabilities into 0/1       #
#  outcomes. One basic decision rule is to use a cutoff of 0.5,         #
#  however, more sophisticated options have been developed.             #

## Predicted probabilities
dat.Davis$estprob <- fitted(logr)
dat.Davis[1:15,]

#  When the response variable is categorical, but has more than two     #
#  levels, multinomial logistic regression can be used. Multinomial     #
#  logistic regression uses a set of logistic regression models. Each   #
#  of the models compares the probability of one of the levels to the   #
#  probability of a baseline level. Thus, when using a response         #
#  variable with L levels, L-1 logistic regression models are created.  #
#  For example, when using the last level as the baseline level, the    #
#  models are:
#   ln{P(y=1)/P(y=L)} = beta_0,1 + beta_1,1*x_1 + . + beta_k,1*x_k      #
#                             + epsilon                                 #  
#   ln{P(y=2)/P(y=L)} = beta_0,2 + beta_1,2*x_1 + . + beta_k,2*x_k      #
#                             + epsilon                                 #
#  to                                                                   #
#   ln{P(y=L-1)/P(y=L)} = beta_0,L-1 + beta_1,L-1*x_1 + .               #
#                             + beta_k,L-1*x_k + epsilon                #  
#                                                                       #
#  The exponentials of the parameters represent the probability of the  #
#  outcome occurring in one category over the probability of the        #
#  outcome occurring in the baseline category. Unlike logistic          #
#  regression, the numerator and denominator are no longer complements  #
#  of each other, thus these ratios are typically called the relative   #
#  risks instead of odds ratios.                                        #
#                                                                       #
#  The likelihood ratio test is used to determine the significance of   #
#  the overall model and the Wald tests are used to determine the       #
#  significance of the individual variables.                            #
#                                                                       #
#  There are two functions that can be used to run multinomial          #
#  logistic regression - mlogit() in the mlogit package and multinom()  #
#  in the nnet package.                                                 #
#                                                                       #
#  Model diagnostics are not straight forward in multinomial logistic   #
#  regression.                                                          #

#######################
#  Correlation tests  #
#######################

#  If there is interest in determining whether two variables have a     #
#  significant relationship, but there is not a clear direction for     #
#  linear regression, the population correlation between the two        #
#  variables can be tested. There is a parametric test for correlation  #
#  and a non-parametric test for correlation.                           #
#                                                                       #
#  The Pearson correlation test assumes that the two variables under    #
#  consideration are continuous, are approximately normally             #
#  distributed, and have a linear relationship with constant variance.  #
#  The null hypothesis is that the population correlation parameter     #
#  (rho) is equal to zero, which is equivalent to the variables having  #
#  no linear relationship. The three alternative hypotheses are that    #
#  the population correlation parameter is positive, negative, or not   #
#  equal to zero. The test statistic for this test is a function of     #
#  the sample correlation, r, and follows a t distribution with n-2     #
#  degrees of freedom. This test can be conducted using the cor.test()  #
#  function. Users input the two vectors of data, designate the method  #
#  to be the Pearson test, and determine the alternative hypothesis.    #

## Craft beer delivery data
cases <- c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
dtime <- c(16.7,11.5,12.0,14.9,13.8,18.1,8.0,17.8,79.2,21.5,40.3,21.0,
           13.5,19.8,24.0,29.0,15.4,19.0,9.5,35.1,17.9,52.3,18.8,19.8,10.8)

## Pearson correlation test
cor.test(cases, dtime, method="pearson", alternative="two.sided")

#  The non-parametric correlation test is called the Spearman rank      #
#  correlation test. Its only assumptions are that there is a           #
#  monotonic relationship between the two variables that are not        #
#  normally distributed. The hypotheses take the same form as the       #
#  hypotheses for the Pearson correlation test, except that the         #
#  correlation parameter is measuring the relationship between the      #
#  ranks of the data. The test statistic uses the differences of the    #
#  ranks for corresponding pairs of data points and follows the t       #
#  distribution with n-2 degrees of freedom. This test can also be      #
#  conducted using the cor.test() function, specifying the Spearman     #
#  method.                                                              #

## Caterpillar growth and diet data
growth <- c(12,10,8,11,6,7,2,3,3)
tannin <- c(0,1,2,3,4,5,6,7,8)

## Spearman correlation test
cor.test(growth, tannin, method="spearman", alternative="two.sided")

#########################################################################



