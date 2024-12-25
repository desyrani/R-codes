# Assignment2


# PERFORM ONE-SAMPLE T-TEST

# Read the DataSet 
yearly_sales <- read.csv("C:/Users/khali/Downloads/yearly_sales.csv")

# Filter the dataset for male customers
male_customers <- yearly_sales[yearly_sales$gender == "M",]

# Calculate sample size
n <- nrow(male_customers)

# Calculate mean
mean_sales <- mean(male_customers$sales_total)

# Calculate standart deviation
sd_sales <- sd(male_customers$sales_total)

# Calculate standard error of the mean 
sem_sales <- sd_sales / sqrt(n)

# Define the hypothesized mean 
hypothesized_mean <- 200 

# Calculate t-value
t_value <- (mean_sales - hypothesized_mean) / sem_sales

# Degree of freedom
df <- n-1


# Perform One-Sample T-Test
one_sample_Ttest <- t.test(male_customers$sales_total, mu = hypothesized_mean)

# Print the results
print(one_sample_Ttest)

# Manual calculation of confidence interval
alpha <- 0.05
t_critical <- qt(1 - alpha/2, df)
ci_lower <- mean_sales - (t_critical * sem_sales)
ci_upper <- mean_sales + (t_critical * sem_sales)

# Print all
cat("Sample Size: ", n, "\n")
cat("Mean: ", mean_sales, "\n")
cat("Standard Deviation: ", sd_sales, "\n")
cat("Standard Error of the Mean: ", sem_sales, "\n")
cat("t-value: ", t_value, "\n")
cat("Degree of Freedom: ", df, "\n")
cat("Critical t-value: ", t_critical, "\n")
cat("Confidence Interval: (", ci_lower, ", ", ci_upper, ") \n")







# PERFORM TWO-SAMPLE T-TEST

# Create a new column to calssify customers based on their number of orders
yearly_sales$Order_Group <- ifelse(yearly_sales$num_of_orders > 2, "More than 2", "2 or Fewer")

# Split the data into two groups
group_more_than_2 <- yearly_sales[yearly_sales$Order_Group == "More than 2",]
group_2_or_fewer <- yearly_sales[yearly_sales$Order_Group == "2 or Fewer",]

# Calculate sample sizes
n1 <- nrow(group_more_than_2)
n2 <- nrow(group_2_or_fewer)

# Calculate means
mean1 <- mean(group_more_than_2$sales_total)
mean2 <- mean(group_2_or_fewer$sales_total)

# Calculate standard deviations
sd1 <- sd(group_more_than_2$sales_total)
sd2 <- sd(group_2_or_fewer$sales_total)

# Calculate standard errors
sem1 <- sd1 / sqrt(n1)
sem2 <- sd2 / sqrt(n2)

# Calculate pooled standard error
se_pooled <- sqrt((sd1^2 / n1) + (sd2^2 / n2))

# Calculate t_value
t_value <- (mean1 - mean2) / se_pooled

# Degrees of freedom
df <- ((sd1^2 / n1) + (sd2^2 / n2))^2 / ((sd1^2 / n1)^2 / (n1 - 1) + (sd2^2 / n2)^2 / (n2 - 1))


# Perform Two-Sample T-Test
two_sample_Ttest <- t.test(sales_total ~ Order_Group, data = yearly_sales)

# Print the results
print(two_sample_Ttest)

# Manual calculation of confidence interval
alpha <- 0.05
t_critical <- qt(1 - alpha/2, df)
ci_lower <- (mean1-mean2) - (t_critical * se_pooled)
ci_upper <- (mean1 - mean2) + (t_critical * se_pooled)


# Print all
cat("Sample Size Group 1 (More than 2 orders): ", n1, "\n")
cat("Sample Size Group 2 (2 or Fewer orders): ", n2, "\n")
cat("Mean Group 1: ", mean1, "\n")
cat("Mean Group 2: ", mean2, "\n")
cat("Standard Deviation Group 1: ", sd1, "\n")
cat("Standard Deviation Group 2: ", sd2, "\n")
cat("Standard Error of Mean Group 1: ", sem1, "\n")
cat("Standard Error of Mean Group 2: ", sem2, "\n")
cat("Pooled Standard Error: ", se_pooled, "\n")
cat("t-value: ", t_value, "\n")
cat("Degrees of Freedom: ", df, "\n")
cat("Critical t-value: ", t_critical, "\n")
cat("Confidence Interval: (", ci_lower, ", ", ci_upper, ")\n")





