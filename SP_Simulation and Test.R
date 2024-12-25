# SIMULATION AND TEST

# Population VS Sample
set.seed (x) # guarantee that the same random values are produced each time you run the code
runif(n=1, min=0, max=9)
runif(n=1000, min=0, max=100)


# Prediction Stock Market
days <- 200
changes <- rnorm(200,mean=1.001,sd=0.005)
plot(cumprod(c(20,changes)),type='l',ylab="Price",
     xlab="day",main="BAYZ closing price (sample path)")


runs <- 100000
#simulates future movements and returns the closing price on day 200
generate.path <- function(){
  days <- 200
  changes <- rnorm(200,mean=1.001,sd=0.005)
  sample.path <- cumprod(c(20,changes))
  closing.price <- sample.path[days+1] #+1 because we add the opening price
  return(closing.price)
}
mc.closing <- replicate(runs,generate.path())
quantile(mc.closing,0.95)
quantile(mc.closing,0.05)


# CASE1

# Expense
set.seed(12)
expense <- rnorm(n=10000, mean=2000, sd= 500)
hist(expense, col='maroon', xlab='Expense ($)', main='Expense')
mtext(paste('mean:', round(mean(expense))), at=c(500), line=-1)
mtext(paste('stdev:', round(sd(expense))), at=c(500), line=-2)
mtext(paste('median:', round(median(expense))), at=c(500), line=-3)


# Unexpected
set.seed(12)
unexpected <- rnorm(n=10000, mean=20, sd=300)
hist (unexpected, col='navy', xlab='Unexpected Income/Expense ($)', main='Unexpected Income or Expenses')
mtext(paste('mean:', round(mean(unexpected))), at=c(-700), line=-1)
mtext(paste('stdev:', round(sd(unexpected))), at=c(-700), line=-2)
mtext(paste('median:', round(median(unexpected))), at=c(-700), line=-3)


# Income
Shape_rate <- function(mean, sd) {
  scale <- sd^2/mean
  shape <- mean/scale
  rate <- 1/scale
  return (c(scale, shape, rate))
}
set.seed(12)
params <- Shape_rate(mean=3200, sd=2000)
shape <- params[2]
rate <- params[3]

income <- rgamma(n=10000, shape=shape, rate=rate)
hist(income, col='cyan', xlab='income ($)', main='income')

mean_income <- mean(income)
stdev_income <- sd(income)
median_income <- median(income)

pos <- 10000

mtext(paste('mean:', round(mean(income))), at=c(pos), line=-1)
mtext(paste('stdev:', round(sd(income))), at=c(pos), line=-2)
mtext(paste('median:', round(median(income))), at=c(pos), line=-3)


# Credit
set.seed(12)
library("mc2d")
survey <- rnorm(10000, mean=5000, sd=1500)
credit <- rempiricalD(n=10000, survey)
hist(credit, col='darkolivegreen', xlab='credit($)', main='credit', breaks=10)

mean_credit <- mean(credit)
stdev_credit <- sd(credit)
median_credit <- median(credit)

mtext(paste('mean:', round(mean(credit))), at=c(200), line=-1)
mtext(paste('stdev:', round(sd(credit))), at=c(200), line=-2)
mtext(paste('median:', round(median(credit))), at=c(200), line=-3)


# Interest
set.seed(12)
interest <- runif(10000)*(1.5-0.3)+0.3
hist(interest, col='brown', xlab='Interest Rate (%)', main='Interest Rate')
mtext(paste('mean:', round(mean(interest),2)), at=c(0.6), line=-1)
mtext(paste('stdev:', round(sd(interest),2)), at=c(0.6), line=-2)
mtext(paste('median:', round(median(interest),2)), at=c(0.6), line=-3)


# Saving 
saving <- (income - expense - credit + unexpected) * (1 + (interest/100))
hist(saving, xlab='Saving ($)', main='Saving', prob=TRUE, xlim=c(-3000, 10000), breaks=24)
mtext(paste('mean:', round(mean(saving))), at=c(6000), line=-1)
mtext(paste('stdev:', round(sd(saving))), at=c(6000), line=-2)
mtext(paste('median:', round(median(saving))), at=c(6000), line=-3)


# CASE1_MODIFIED
saving <- (income-expense+unexpected) * (1+(interest/100))
hist(saving, xlab='Saving($)', main='Saving', prob=TRUE, xlim = c(-3000, 10000), breaks=24)
mtext(paste('mean:', round(mean(saving))), at=c(6000), line=-1)
mtext(paste('stdev:', round(sd(saving))), at=c(6000), line=-2)
mtext(paste('median:', round(median(saving))), at=c(6000), line=-3)

