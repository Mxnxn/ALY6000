# check if package exist and installing package ‘vcd’ 
if(!require('vcd',character.only = TRUE)) install.packages('vcd')
# importing ‘vcd’
library(vcd)

# creating Vectors
sales <- c(7,11,15,20,19,11,18,10,6,22)
temperature = c(69,81,77,84,80,97,87,70,65,90)

# plotting scatter graph for sales ~ temperature
library(ggplot2)
ggplot(data.frame(sales,temperature), aes(x = temperature , y=sales , color = ..y..)) +
  geom_point(size=4) +
  labs(
    title = "Fig.1 Sales ~ Temperature", # adds title
    x = "Temperature (°F)", # x-axis label
    y = "Sales\n", # y-axis label
    color = "Sales" # color legend
  ) + scale_color_gradient(low="pink", high="red") + theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

meanOfTemperature <- mean(temperature) # Mean of temperature Vector

# Removing From Index which is 3 as Indexing starts from 1 in ‘R’
sales <- c(sales[-3])
# Insert 16 at index 2
sales <- append(sales,16,2)

# allocationg Vector of strings in <names> Vector
names <- c("Tom","Dick","Harry")

mMatrix <- matrix(c(1:10),nrow=5,byrow=FALSE) # creating 5x2 Matrix

# creating dataFrame from sales, temperature named <icSales>
icSales <- data.frame('Sales'= sales,'Temperature' = temperature)
print(str(icSales))

# finding summary of Data Frame <icSales>
mSummary = summary(icSales)
# to open Student.csv file read.csv is built in function provided by ‘R’
student_data = read.csv("./Student.csv") # change CWD for this command
# two functions names() and col_names() which are built-in in ‘R’
names(student_data) # Printing Variables of Student.csv
