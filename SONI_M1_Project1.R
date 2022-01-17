if(!require('vcd',character.only = TRUE)) install.packages('vcd')
library(vcd)

# creating Vectors
sales <- c(7,11,15,20,19,11,18,10,6,22)
temperature = c(69,81,77,84,80,97,87,70,65,90)
sales
temperature 

# plotting scatter graph for sales ~ temperature
plot(sales,temperature)

# getting mean of temperature
mean_temperature <- mean(temperature)
mean_temperature

# Removing From Index 15
sales <- c(sales[-3])
sales
# Insert 16 at 2 index
sales <- append(sales,16,2)
sales 

# creating Vectors of String/Names
names <- c("Tom","Dick","Harry")
names

# creating Matrix of 2 column and 5 row
matrixx <- matrix(c(1:10),nrow=5,byrow=FALSE)
matrixx

# creating dataFrame from sales, temperature
icSales <- data.frame('Sales'= sales,'Temperature' = temperature)
icSales

print(str(icSales))

# finding summary of Data Frame
summaryx = summary(icSales)
summaryx

# Opening Student.csv file
student_data = read.csv("./Student.csv")
student_data
# Printing Variables of Student.csv
names(student_data)


# ref
# https://www.tutorialspoint.com/r/index.htm
# Book: R in Action Data Analysis and graphics with R by Robert I. Kabacoff

