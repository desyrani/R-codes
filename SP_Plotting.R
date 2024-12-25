#PLOTTING
x <- c(2, 4, 6, 8)
y <- c(1, 3, 5, 7)
plot (x,y)
plot (x, y,col="blue", cex=2, pch=15)

# High level charts
head (WorldPhones)
WP1951 <- WorldPhones[1, ]
WP1951
barplot(WP1951)
barplot(WP1951, cex.names = 0.75, cex.axis = 0.75, main = "Numbers of
Telephones in 1951")
dotchart(WP1951, xlab = "Numbers of Phones (’000s)")

VADeaths
barplot(VADeaths, beside = TRUE, legend = TRUE, ylim = c(0, 90), ylab =
          "Deaths per 1000", main = "Death rates in Virginia")
dotchart(VADeaths, xlim = c(0, 75), xlab = "Deaths per 1000", main =
           "Death rates in Virginia", cex = 0.8)

# Bar chart
# What does these do?
colors = c("green","orange","brown")
months <- c("Mar","Apr","May","Jun","Jul")
regions <- c("East","West","North")

# What does these do?
Values <- matrix(c(2,9,3,11,9,4,8,7,3,12,5,2,8,10,11), nrow =
                   3, ncol = 5, byrow = TRUE)

# What does these do?
png(file = "barchart_stacked.png")

# What does these do?
barplot(Values, main = "total revenue", names.arg = months,
        xlab = "month", ylab = "revenue", col = colors)

# What does these do?
legend("topleft", regions, cex = 1.3, fill = colors)

# What does these do?
dev.off()


# Scatterplot
input <- mtcars[,c('wt','mpg')]
png(file = "scatterplot.png")
plot(x = input$wt,y = input$mpg,
     xlab = "Weight",
     ylab = "Milage",
     xlim = c(2.5,5),
     ylim = c(15,30),
     main = "Weight vs Milage"
)
dev.off()
# Give the chart file a name.
png(file = "scatterplot_matrices.png")
# Plot the matrices between 4 variables giving 12 plots.
# One variable with 3 others and total 4 variables.
pairs(~wt+mpg+disp+cyl,data = mtcars,
      main = "Scatterplot Matrix")
# Save the file.
dev.off()

# Line chart
v <- c(7,12,28,3,41)
plot(v,type = "o")
plot(v,type = "o", col = "red", xlab = "Month", ylab = "Rain fall", main = "Rain fall chart")
t <- c(14,7,6,19,3)
lines(t, type = "o", col = "blue")


# High level charts
groupsizes <- c(18, 30, 32, 10, 10)
labels <- c("A", "B", "C", "D", "F")
pie(groupsizes, labels, col = c("grey40", "white", "grey", "black", grey90"))


# Pie chart
library(plotrix)
x <- c(21, 62, 10,53)
lbl <- c("London","New York","Singapore","Mumbai")
pie3D(x,labels = lbl,explode = 0.1,
main = "Pie Chart of Countries ")

x <- c(21, 62, 10, 53)
labels <- c("London", "New York", "Singapore", "Mumbai")

pie(x,labels)

pie(x, labels, main = "City pie chart", col = rainbow(length(x)))
pie(x, labels = piepercent, main = "City pie chart",col = rainbow(length(x)))
legend("topright", c("London","New York","Singapore","Mumbai"), cex = 0.8,
fill = rainbow(length(x)))


# Histogram
v <- c(9,13,21,8,36,22,12,41,31,33,19)
hist(v,xlab = "Weight",col = "yellow",border = "blue")
hist(v,xlab = "Weight",col = "green",border = "red",
xlim = c(0,40), ylim = c(0,5), breaks = 5)



