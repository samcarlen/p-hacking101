#######################################
# p-hacking 101
# Sam Carlen
# irrationalactor.substack.com
###############################

library(tidyverse)
library(pacman)
p_load(mosaic,glmnet,MASS)

########################################
# OkCupid data, graphs, and calculations
########################################

set.seed(4)

# Original sample
# Data avaiable at: https://www.kaggle.com/andrewmvd/okcupid-profiles
ok<-read.csv("~/okcupid_profiles.csv")

# distribution of sample means for 1,000 subsamples of male OkCupid users
ok_men <- ok %>%
  filter(sex=="m")

means <- replicate(n=1000,mean(slice_sample(ok_men,n=1000)$age))

means <- tibble(means)

densityplot(means$means,xlab="mean age across samples")


ggplot(means) + geom_density(aes(x=means),size=1) + geom_rug(aes(x=means)) + 
  geom_vline(aes(xintercept=32.8,color="red"),size=2)+
  annotate(geom="text",x=33,y=1,label="Mean age of women (in red)")+
  annotate(geom="text",x=31.8,y=1,label="Sampling distribution for (male) age (black)")

ok %>%
  group_by(sex) %>%
  summarize(mean_age=mean(age),sd_age=sd(age),n=n())

(32.8-32)/sqrt((9.03^2)/24117 + (10^2)/35829)

####################################

# p-hacking demo
# In this demo, our independent variable will be IQ, which follows a normal distribution
# with mean 100 and standard deviation 15 or 16 (depending on the test).
# In this example, assume IQ ~ N(100,15). Let y = IQ.
# Start with with n=100 sample size. Randomly-generated dependent variables
# are listed below, each with either binomial or normal distributions. 
# The specific paramaters and distributions used are specified below. 
# In this example, imagine we want to regress 
# these "variables" against y (IQ) to "study" the correlates of intelligence.
# But to maximize our chances of obtaining significance, let's measure three
# outcome variables: IQ, SAT score, and GRE

set.seed(102)

# y (IQ)
# normally distributed with a mean of 100 and a standard deviation of 15
y1 = rnorm(100,mean=100,sd=15)

# SAT
# normally distributed with a mean of 1051 and a standard deviation of 211
y2 = rnorm(100,mean=1051,sd=211)

# GRE
# normally distributed with a mean of 150 and a standard deviation of 9
y3 = rnorm(100,mean=150,sd=9)


# Regressors:
x1 <- rbinom(100,size=1,prob=0.5)
x2 <- rbinom(100,size=1,prob=0.2)
x3 <- rbinom(100,size=5,prob=0.3)
x4 <- rnorm(100,mean=0,sd=1)
x5 <- rnorm(100,mean=10,sd=2)
x6 <-rnorm(100,mean=100,sd=10)
x7 <- rnorm(100,mean=0,sd=1)
x8 <- rpois(100,lambda=150)
x9 <- rlnorm(100,meanlog=0,sd=1)
x10 <- rbernoulli(100,p=0.1)

#create a data frame
df <- cbind(y1,y2,y3,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
df <- data.frame(df)

# Models
m1 <- lm(y1~.,data=df)
summary(m1)

m2 <- lm(y2~.,data=df)
summary(m2)

m3 <- lm(y3~.,data=df)
summary(m3)

m4 <- lm(y1~x1*x2+x3*x4+x5*x6+x7*x8+x9+x10,data=df)
summary(m4)

m5 <- lm(y2~x1*x2+x3*x4+x5*x6+x7*x8+x9+x10,data=df)
summary(m5)

m6 <- lm(y3~x1*x2+x3*x4+x5*x6+x7*x8+x9+x10,data=df)
summary(m6)

########################################################
