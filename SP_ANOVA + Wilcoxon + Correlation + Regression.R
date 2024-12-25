# 2nd part ANOVA + Wilcoxon + Correlation + Regression

# one way
install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
crop.data <- read.csv("C:/Users/khali/Downloads/cropdataa.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "numeric"))
summary(crop.data)
one.way <- aov(yield ~ fertilizer, data = crop.data)
summary(one.way)
tuckey.oneway <- TukeyHSD(one.way)
tuckey.oneway

one.way <- aov(yield ~ density, data = crop.data)
summary(one.way)
tuckey.oneway <- TukeyHSD(one.way)
tuckey.oneway

one.way <- aov(yield ~ block , data = crop.data)
summary(one.way)
tuckey.oneway <- TukeyHSD(one.way)
tuckey.oneway




# two way
two.way <- aov(yield ~ fertilizer + density, data = crop.data)
summary(two.way)
interaction <- aov(yield ~ fertilizer*density, data = crop.data)
summary(interaction)

library(AICcmodavg)

model.set <- list(one.way, two.way, interaction)
model.names <- c("one.way", "two.way", "interaction")

aictab(model.set, modnames = model.names)



# WILCOXON
data<- read.csv("C:/Users/khali/Downloads/therapy.csv")
standardTherapy <- data$Standard.Therapy
newTherapy <- data$New.Therapy
wilcox.test(standardTherapy, newTherapy)
wilcox.test(standardTherapy, newTherapy, alternative = "two.sided", conf.int = TRUE)




# CORRELATION
data <- data.frame(assignment = c(77,90,86,92,89,99),
                   test = c(91,80,79,85,87,81))

correlation_pearson <- cor(data$assignment, data$test, method = "pearson")
correlation_test_pearson <- cor.test(data$assignment, data$test, method = "pearson")

print(correlation_pearson)
print(correlation_test_pearson)

library("ggpubr")

ggscatter(data, x = "assignment", y = "test", add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", xlab = "Assignment", ylab = "Test result")




# LINEAR REGRESSION
install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# Apply the lm() function.
relation <- lm(y~x)
print(relation)
print(summary(relation))

# Give the chart file a name.
png(file = "linearregression.png")

# Plot the chart.
plot(y,x,col = "blue",main = "Height & Weight Regression", abline(lm(x~y)),
     cex = 1.3,pch = 16,xlab = "Weight in Kg",ylab = "Height in cm")

# Save the file.
dev.off()

# Find weight of a person with height 170.
a <- data.frame(x = 170)
result <- predict(relation,a)
print(result)

# Evaluation
print(summary(relation))




# MULTIPLE LINEAR REGRESSION
input <- mtcars[,c("mpg","disp","hp","wt")]

# Create the relationship model
model <- lm(mpg~disp+hp+wt, data = input)

# Show the model
print(model)

print(summary(model))




# LOGISTIC REGRESSION WITH R
input <- mtcars[,c("am","cyl","hp","wt")]
print(head(input))
am.data = glm(formula = am ~ cyl + hp + wt, data = input, family = binomial)
print(summary(am.data))


