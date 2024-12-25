# DATA RESHAPING
# Create vector objects.
city <- c("Tampa", "Seattle", "HartFord", "Denver")
state <- c("FL", "WA", "CT", "CO")
zipcode <- c(33602, 98104, 06161, 80294)

# Combine above three vectors into one data frame.
address <- cbind(city, state, zipcode)

# Print a header.
cat("# # # # The First data frame\n")

#Print the data frame.
print(address)

# Create another data frame with similar coloumns
new.address <- data.frame(
  city = c("Lowry", "Charlotte"),
  state = c("CO", "FL"),
  zipcode = c("80230", "33949"),
  stringAsFactors = FALSE
)

# Print a header.
cat("# # # The Second data frame\n")

# Print the data frame.
print(new.address)

# Combine rows from both the data frames.
all.addresses <- rbind(address,new.address)

# Print a header.
cat("# # # The Combined data frame\n")

# Print the result.
print(all.address)

#### 


library(MASS)
merged.Pima <- merge(x = Pima.te, y = Pima.tr,
    by.x = c("bp", "bmi"),
    by.y = c("bp", "bmi")
)
print(merged.Pima)
nrow(merged.Pima)

library(MASS)
print(ships)

library(reshape2)
molten.ships <- melt(ships, id = c("type", "year"))
print(molten.ships)

recasted.ship <- dcast(molten.ships, type+year~variable, sum)
print(recasted.ship)

####


# Get and print current working directory.
print (getwd())

# Set current working directory.
setwd("C:/Users/khali/Downloads")

# Get and print current working directory.
print(getwd())

data <- read.csv("input.csv")
print (data)

# Create a data frame.
data <- read.csv("input.csv")
retval <- subset(data, as.Date(start_date) > as.Date("2014-01-01"))

# Write filtered data into a new fule.
write.csv(retval, "output.csv", row.names = FALSE)
newdata <- read.csv("output.csv")
print(newdata)


print(is.data.frame(data))
print(ncol(data))
print(nrow(data))

# Get the person detail having max salary.
retval <- subset(data, salary == max(salary))
print(retval)

info <- subset(data, slary > 600 & dept == "IT")
print(info)

retval <- subset(data, as.Date(start_date) > as.Date("2014-01-01"))
print(retval)

# Get the max salary from data frame.
sal <- max(data$salary)
print(sal)

retval <- subset(data, dept == "IT")
print (retval)

