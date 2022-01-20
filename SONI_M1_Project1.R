# check if package exist
if(!require('vcd',character.only = TRUE)) install.packages('vcd')
# importing vcd
library(vcd)

# creating Vectors
sales <- c(7,11,15,20,19,11,18,10,6,22)
temperature = c(69,81,77,84,80,97,87,70,65,90)
sales
temperature 

# plotting scatter graph for sales ~ temperature
library(ggplot2)
ggplot(data.frame(sales,temperature), aes(x = sales, y = temperature, color = ..y..)) +
  geom_point(size=4) +
  labs(
    title = "Fig.1 Sales ~ Tempareture", # adds title
    x = "Sales", # x-axis label
    y = "Temperature (F)", # y-axis label
    color = "Temperature" # color legend
  ) + 
  scale_color_gradient(low="blue", high="orange") +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
  )
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


matrixx <- matrix(c(1:10),nrow=5,byrow=FALSE) # creating Matrix of 2 column and 5 row
matrixx

# creating dataFrame from sales, temperature
icSales <- data.frame('Sales'= sales,'Temperature' = temperature)
icSales

print(str(icSales))

# finding summary of Data Frame
summaryx = summary(icSales)
summaryx


student_data = read.csv("./Student.csv") # Opening Student.csv file
names(student_data) # Printing Variables of Student.csv


# ref
# https://www.tutorialspoint.com/r/index.htm
# Book: R in Action Data Analysis and graphics with R by Robert I. Kabacoff

