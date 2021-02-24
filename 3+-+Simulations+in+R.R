
#########################################################################
#                                                                       #
#  Simulations in R                                                     #
#                                                                       #
#########################################################################

library(car)

#  One of the powerful abilities of R is quick computation, which       #
#  allows users to simulate outcomes and results. Many statistical      #
#  results are based on the idea of repeated sampling, which is a rare  #
#  occurrence in real life. However, R gives users the ability to       #
#  simulate repeated sampling and to view the results.                  #

###########
#  Loops  #
###########

#  There are several ways to run simulations. One of the most basic is  #
#  using a loop, which repeats a certain process over and over. There   #
#  are two types of loops - for loops and while loops. The former is    #
#  used when there is a specific number of times that the process       #
#  should be repeated. While loops continue to repeat until a certain   #
#  condition is satisfied.                                              #
#                                                                       #
#  A for loop is composed of five main pieces - the for statement, the  #
#  index variable, the in statement, the vector of values for which     #
#  the for loop should run, and the code to be repeated. The index      #
#  variable can either be used in the repeated code or not.             #

for(i in 1:5) print(i)

#  If the code to be repeated is more than one line, curly brackets     #
#  are used to define the section of code to be repeated.               #

j <- 10
for(i in 1:5){
    print(j)
    j <- j+1
}

j <- 10
for(i in 1:5){
    j <- j+1
    print(j)
}

#  If a value is being calculated in the loop that needs to be saved,   #
#  it is best practice to create a vector to record the values before   #
#  beginning the loop.                                                  #

j <- 10
new_vect <- NULL
for(i in 1:5){
    new_vect[i] <- j
    j <- j+1
}
new_vect

j <- 10
new_vect <- rep(0,5)
for(i in 1:5){
    new_vect[i] <- j
    j <- j+1
}
new_vect

#  A user may want to do different things within the process being      #
#  repeated within the loop depending on certain values that result.    #
#  In this case the if() and else() functions can be used within the    #
#  loop. These functions are different from the ifelse() function.      #

bmi <- NULL
for(i in 1:200){
    if(Davis[i,1] == "M")
    {bmi[i] <- Davis$repwt[i]/(Davis$repht[i]/100)^2}
    else{bmi[i] <- Davis$weight[i]/(Davis$height[i]/100)^2}
}
bmi

bmi2 <- ifelse(bmi<18.5,"under","not")

#  A while loop is composed of three main pieces - the while statement, #
#  the condition, and the code to be repeated.                          #

j<-10
while(j<12){
    j <- j+1
    print(j)
}

#  No matter which type of loop is being used, users can include        #
#  options in their loops to stop the loop under certain conditions or  #
#  to move onto the next iteration.                                     #

j <- 10
new_vect <- rep(0,5)
for(i in 1:5){
    new_vect[i] <- j
    j <- j+1
    if(j==13){break}
}
new_vect

j <- 10
new_vect <- rep(0,5)
for(i in 1:5){
    if(j==13){next}
    new_vect[i] <- j
    j <- j+1
}
new_vect  #10 11 12 0 0; j stays 13

j <- 10
new_vect <- rep(0,5)
for(i in 1:5){
    j <- j+1
    if(j==13){next}
    new_vect[i] <- j
}
new_vect

j<-10
while(j<20){
    j <- j+1
    print(j)
    if(j==14){break}
}

j<-10
while(j<20){
    j <- j+1
    if(j==14){next}
    print(j)
}

#  Loops can be nested so that one runs inside another.                 #

A <- matrix(1:6, nrow=2, ncol=3, byrow=T)
new_vect <- NULL
k<-1
for(i in 1:3){
    for(j in 1:2){
        new_vect[k]<-A[j,i]
        k<-k+1
    }
}
new_vect

#######################
#  apply() functions  #
#######################

#  Many of the results produced by loops can be accomplished in         #
#  another way and loops are comparatively slow, so many users prefer   #
#  to avoid loops when possible. One of the main ways to avoid using    #
#  loops is using the apply() function. Recall that the apply()         #
#  function applies the specified function to either all the rows or    #
#  all the columns of a matrix and returns a vector or list.            #
#                                                                       #
#  In fact, there are four apply functions - apply(), lapply(),         #
#  sapply(), and tapply(). The lapply() function applies the specified  #
#  function to every element in a list and returns a list. The sapply() #
#  function applies the specified function to every element of a list   #
#  and returns a vector.  The tapply() function applies the specified   #
#  function for defined groups in a vector and returns an array. The    #
#  by() function uses tapply().                                         #

## lapply and sapply
list1 <- list(1:7, c(2,7,9,12)^2, 1/(1:10))
list1
lapply(list1, sum)  #return a list
sapply(list1, sum)  #return a vector

## tapply
tapply(Davis$weight, Davis$sex, mean)  #apply to vectors

#################
#  Replication  #
#################

#  Another useful tool for simulations and alternative to loops is      #
#  the replicate() function. This function evaluates the expression     #
#  specified the specified number of times and returns a matrix with    #
#  a column containing the results from each replication.               #

replicate(5, rnorm(10))
replicate(5, rnorm(10, 2, 1.6))
replicate(5, runif(10))

##############
#  Sampling  #
##############

#  If a simulation requires sampling from a given set of data instead   #
#  of generating random values, the sample() function can be used. The  #
#  inputs for this function are the vector of values from which to      #
#  sample, whether to replace the values, and how many values to draw.  #
#  The defaults are without replacement sampling and the number of      #
#  values in the original vector.                                       #

wtdata <- Davis$weight
sample(wtdata) #sample everything
sample(wtdata, 10) #size, without replacement
sample(wtdata, 10, replace=TRUE)

#  The sample() function can be used within the replicate() function    #
#  to generate repeated samples.                                        #

#########################################################################

#############################
#  Monte Carlo simulations  #
#############################

#  Monte Carlo simulation is the process of using repeated sampling     #
#  to determine some behavior or characteristic. This type of           #
#  simulation is often used to demonstrate and understand the Central   #
#  Limit Theorem.                                                       #
#                                                                       #
#  Suppose that we have a population that is chi-squared distributed    #
#  with four degrees of freedom and we plan to take a random sample     #
#  from that population and use the Central Limit Theorem to test the   #
#  sample mean. One important question is how large of a random sample  #
#  is needed from the population for the Central Limit Theorem to hold. #
#  This question can be answered using Monte Carlo simulation.          #

## First plot the chi-squared population distribution
Xdata2 <- data.frame(X=c(0,20))
dist2 <- ggplot(Xdata2, aes(x=X))
dist2 + stat_function(fun=dchisq, args=list(df=4))

## Determine the number of repeated samples to draw and parameter values
K <- 10000
a <- 4

## Draw 10,000 samples of size 15 from the population distribution
samps <- replicate(K, rchisq(15,a))
samps[,1:5]

## Determine the sample mean from each random sample
means15 <- apply(samps,2,mean)

## Create a QQ plot of the resulting sample means
mean_data <- data.frame(X=means15)
y <- quantile(mean_data$X, c(0.25, 0.75)) 
x <- qnorm(c(0.25, 0.75))  
slope <- diff(y)/diff(x) 
int <- y[1] - slope*x[1] 

ggplot(mean_data, aes(sample = X)) + stat_qq() + 
    geom_abline(intercept=int, slope=slope) + labs(title="n=15")

#  If we want to repeat this process with other sample sizes, we can    #
#  write a for loop instead of writing the same code several times.     #

## Repeat this process with sample sizes of 30, 40, and 50
for (i in c(30,40,50)){
    samps <- replicate(K, rchisq(i,a))
    means <- apply(samps,2,mean)
    assign(paste("means",i,sep=""), means) #assign mean function to "means"
    
    mean_data <- data.frame(X=means)
    y <- quantile(mean_data$X, c(0.25, 0.75)) 
    x <- qnorm(c(0.25, 0.75))  
    slope <- diff(y)/diff(x) 
    int <- y[1] - slope*x[1] 
    
    qqmean <-ggplot(mean_data, aes(sample = X)) + stat_qq() + 
        geom_abline(intercept=int, slope=slope) + labs(title=paste("n=",i,sep=""))
    print(qqmean)
}

#  Once we have determined which sample size yields a sampling          #
#  distribution that is sufficiently approximately normal, we can       #
#  verify the parameters of the sampling distribution. The population   #
#  mean and variance of chi-squared distributions are the df and 2*df,  #
#  respectively.                                                        #

## Estimate the mean of the sampling distribution (which is 4)
mean(means40)

## Estimate the standard devation of the sampling distribution
true_sd <- sqrt(2*4/40)
true_sd
sd(means40)

###################
#  Bootstrapping  #
###################

#  Suppose that for budget purposes we are only able to draw a sample   #
#  of 7 from the chi-squared population distribution. In this case,     #
#  the sample size is too small to be able to reasonably use the        #
#  Central Limit Theorem. Thus, the sampling distribution is unknown.   #
#  In this case, bootstrapping can be used to estimate the variation    #
#  in the sampling distribution.                                        #
#                                                                       #
#  Bootstrapping uses similar ideas as Monte Carlo simulations, but     #
#  instead of drawing samples from a given population, samples are      #
#  taken from the sampled data. Bootstrapping uses with replacement     #
#  sampling to create several simulated samples that are the same size  #
#  as the original sampled data. The parameter of interest is           #
#  estimated by these bootstrap samples and the variability can be      #
#  assessed by using the variability of the bootstrapped statistics.    #
#                                                                       #
#  It is important to know that there are assumptions that need to      #
#  hold for bootstrapping to work. One of these assumptions is that     #
#  the sampled data represent an independent, representative sample     #
#  from the population. Also, bootstrapping will not work as expected   #
#  if the underlying population has very heavy tails.                   #

## Draw the sample of 7 from the chi-squared distribution
samp_data <- rchisq(7, df=4)
samp_data

## Find the sample mean
mean(samp_data)

## Determine the number of bootstrap samples
B<-10000

## Draw the bootstrap samples
boot_samp <- replicate(B, sample(samp_data, replace=T))

## Determine the sample mean from each bootstrap sample
boot_means <- apply(boot_samp,2,mean)

## Estimate the center of the sampling distribution
mean(boot_means)

## Determine the 95% bootstrap confidence interval 
boot_means_sort <- sort(boot_means)
lbp <- B*0.025
ubp <- B*0.975
boot_ci <- boot_means_sort[c(lbp,ubp)]
boot_ci

#  Bootstrapping can also be used to estimate the variability of        #
#  parameters whose variation is difficult or impossible to determine   #
#  theoretically.                                                       #

## Determine the median of the sampled data
median(samp_data)

## Determine the sample median from each bootstrap sample
boot_meds <- apply(boot_samp,2,median)

## Determine the 95% bootstrap confidence interval
boot_meds_sort <- sort(boot_meds)
boot_med_ci <- boot_meds_sort[c(lbp,ubp)]
boot_med_ci

#  Note that the bootstrap confidence intervals are not symmetric       #
#  around the point estimate. In certain cases, the standard deviation  # 
#  of the bootstrapped esimates can be used in place of the standard    #
#  error in the usual normal confidence interval equation if you are    #
#  confident that approximate normality holds.                          #

#########################################################################
