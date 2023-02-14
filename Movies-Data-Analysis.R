#------------------Libraries---------------------------

library(readr)
library(ggplot2)
library(Rmisc)

#-----------------Loading Dataset----------------------

dataset <- read.csv("F:\\1.Masters in Data Science (Fast)\\Stats\\Assignments\\assignment no 4\\movies.csv")
View(dataset)


#---------------- Task 1 ------------------------------
# Task 1: Plot the graph of all the observations of the 'budget' variable and then calculate the its population mean and standard deviation. State the distribution of the data.

hist(dataset$budget)
mean(dataset$budget)
sd(dataset$budget)

# Analysis:
# the budget data shows right skewed distribution 


#---------------- Task 2 -----------------------------
# Task 2: Draw 1000 samples of size n=20 and compute their means (known as sample means). Then plot those 1000 sample means to get a normal distribution. Finally calculate mean and standard deviation of the sampling distribution.

# calculating sample mean with size 20
bud_samp <- rep(NA, 1000)
for(i in 1:1000)
{temp <- sample(dataset$budget, size = 20)
bud_samp[i] <- mean(temp)} # calculating each mean

# plotting sample mean
hist(bud_samp)

# mean and standard deviation of the sampling distribution
mean(bud_samp)
sd(bud_samp)


#----------------- Task 3 ---------------------------
# Task 3: Compare the mean of sampling distribution with the mean of population and the standard deviation of the sampling distribution with the standard deviation predicted by Central Limit Theorem (CLT).

# Compare the mean of sampling distribution with the mean of population

# population mean
mean(dataset$budget)

# sampling distribution mean
mean(bud_samp)

# Analysis:
# the population mean lies towards right tail of distribution as we take sample the mean gets shifted towards left side a little and the more we increase the size the more this will resemble the normal distribution

# Compare the standard deviation of the sampling distribution with the standard deviation predicted by Central Limit Theorem (CLT).

# stand dev of sampling dist
sd(bud_samp)

# stand dev predicted by clt
sd(dataset$budget)/sqrt(500)

# Analysis:
# as the size of sample increases standard deviation gets smaller since it's sd(x)/ sqrt(n), so for very very large n sd becomes very small and collapses on top of population mean


#----------------- Task 4 ---------------------------
# Task 4: Now increase the sample size from n=20, and do task 2 and 3 again for sample size n=50, n=100 and n=500. Discuss the impact of sample size on sampling distribution.

# calculating sample mean with size 50
bud_samp <- rep(NA, 1000)
for(i in 1:1000)
{temp2 <- sample(dataset$budget, size = 50)
bud_samp[i] <- mean(temp2)}  # calculating each mean

# plotting sample mean
hist(bud_samp)

# mean and standard deviation of the sampling distribution
mean(bud_samp)
sd(bud_samp)

# stand dev predicted by clt
sd(dataset$budget)/sqrt(50)


# calculating sample mean with size 100
bud_samp <- rep(NA, 1000)
for(i in 1:1000)
{temp3 <- sample(dataset$budget, size = 100)
bud_samp[i] <- mean(temp3)} # calculating each mean

# plotting sample mean
hist(bud_samp)

# mean and standard deviation of the sampling distribution
mean(bud_samp)
sd(bud_samp)

# stand dev predicted by clt
sd(dataset$budget)/sqrt(100)


# calculating sample mean with size 500
bud_samp <- rep(NA, 1000)
for(i in 1:1000)
{temp3 <- sample(dataset$budget, size = 500)
bud_samp[i] <- mean(temp3)} # calculating each mean

# plotting sample mean
hist(bud_samp)

# mean and standard deviation of the sampling distribution
mean(bud_samp)
sd(bud_samp)

# stand dev predicted by clt
sd(dataset$budget)/sqrt(500)

# Analysis:
# as the size increases the sample mean comes closer to population mean and standard deviation according CLT collapses on the population mean and we get a normal distribution

#----------------- Task 5 ---------------------------

# Task 5: For n=500 within what range of budget can we expect the population mean to be, with 95% and 99% confidence?
exp_cnt = 500
dt = NULL
i = 1
for(i in 1:exp_cnt){
  tg = dataset[sample(nrow(dataset), 500),]
  tg = cbind(i, tg)
  dt = rbind(dt, tg)
}

# gives count, mean, sd, sd error of mean and confidence interval
tgc <- summarySE(dt, measurevar = "budget", groupvars = c("i"), conf.interval = 0.95)

p1 = ggplot(tgc, aes(x=i, y=budget)) +
  geom_errorbar(aes(ymin=budget-ci, ymax=budget+ci), width=.1)+
  geom_point()+geom_abline(slope = 0, intercept = mean(dataset$budget))+ggtitle("Confidence interval")

p2 = ggplot(tgc, aes(x=i, y=budget)) +
  geom_errorbar(aes(ymin=budget-se, ymax=budget+se), width=.1)+
  geom_point()+geom_abline(slope = 0, intercept = mean(dataset$budget))+ggtitle("sampling error")

multiplot(p1, p2)

cap = sum(tgc$budget+tgc$ci >= mean(dataset$budget) & tgc$budget)
cap
print(cap/exp_cnt)


exp_cnt = 500
dt = NULL
i = 1
for(i in 1:exp_cnt){
  tg = dataset[sample(nrow(dataset), 500),]
  tg = cbind(i, tg)
  dt = rbind(dt, tg)
}

# gives count, mean, sd, sd error of mean and confidence interval
tgc <- summarySE(dt, measurevar = "budget", groupvars = c("i"), conf.interval = 0.99)

p1 = ggplot(tgc, aes(x=i, y=budget)) +
  geom_errorbar(aes(ymin=budget-ci, ymax=budget+ci), width=.1)+
  geom_point()+geom_abline(slope = 0, intercept = mean(dataset$budget))+ggtitle("Confidence interval")

p2 = ggplot(tgc, aes(x=i, y=budget)) +
  geom_errorbar(aes(ymin=budget-se, ymax=budget+se), width=.1)+
  geom_point()+geom_abline(slope = 0, intercept = mean(dataset$budget))+ggtitle("sampling error")

multiplot(p1, p2)

cap = sum(tgc$budget+tgc$ci >= mean(dataset$budget) & tgc$budget)
cap
print(cap/exp_cnt)


# Analysis:
# with 95% confidence interval we are certain that around 495 samples mean will contain population mean and 99% confidence interval will have around 499 samples mean containing population mean
# for 95% confidence interval sample means between range of 2.4e+07 to 2.7e+07 will contain population mean, for 99% confidence interval sample means between range of 2.3e+07 to 3.0e+07 will contain population mean
