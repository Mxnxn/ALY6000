
# 1. Install one or more packages
#install.packages(c('FSAdata','FSA','dplyr','plotrix','moments','ggplot2'))

# 2. Importing
library('FSAdata')
library('magrittr')
library('FSA')
library('plotrix')
library('moments')
library('dplyr')

# 3. Loading Dataset into <dataset> 
try(data("BullTroutRML2"))

# 4. Displaying last and first 3 rows
headtail(BullTroutRML2, n=3)

# Taking out unique from lake column
uniques = unique(BullTroutRML2$lake)

# 5. Filtering with 1st unique value
harrisonLakeData = BullTroutRML2 %>%  filter(BullTroutRML2$lake == uniques[1])

# For seperate observations on both Era
#data of 1977 
dfOf1977 = harrisonLakeData %>%  filter(harrisonLakeData$era == "1977-80")
#data of 1997
dfOf1997 = harrisonLakeData %>%  filter(harrisonLakeData$era == "1997-01")

# 6. Displaying last and first 5 rows
headtail(harrisonLakeData,n=5)

# 7. Displaying Structure of Dataset
str(data.frame(harrisonLakeData))

# 8. Displaying summary of Dataset
summary(data.frame(harrisonLakeData))

# 9. Plotting Scatter Plot
plot(harrisonLakeData$age  ~  harrisonLakeData$fl,
     ylab="Age (yrs)",
     xlab="Fork Length (mm)",
     pch=19,
     ylim=c(0,15),
     xlim=c(0,500),
     col = "blue",
     cex= 1,
     main = "Plot 1: Harrison Lake Trout")

# 10. Plotting Harrison Lake Trout
hist(x= harrisonLakeData$age,col='red',border='white',breaks=8,ylim= c(0,15), xlim = c(0,15),xlab="Age (yrs)",main="Plot 2: Harrison Age Distribution")

# 11. Density Plot in green shade
library('ggplot2')
Age =harrisonLakeData$age/mean(harrisonLakeData$age)
ggplot(data=harrisonLakeData, aes(y=age)) +geom_density()+
  geom_point(data=harrisonLakeData, aes(y=age, x=fl,size=Age),alpha = Age,colour = "#00aa00") +
  labs(
    title = "Plot 7: Harrison Density Shaded by Era", # adds title
    x = "Fork Length (mm)", # x-axis label
    y = "Age (yrs)\n", # y-axis label
    color = "Sales" # color legend
  ) + lims(x = c(0,500),y=c(0,15))

# 12.creating tmp and displaying headtail where n =3 
tmp = headtail(harrisonLakeData,n=3)
print(tmp)

# 13. Era column of <tmp>
print(tmp$era)

# 14. Creating <pchs> for ploting characters + and x
pchs = c(3,4)

# 15. creating <cols> for coloring in red and gray60
cols = c('red','gray60')

# 16. converting <tmp> era values to 1,2,3 by its value using as.numeric
tmp$era = as.numeric(tmp$era)

# 17. Initializing <cols> to <tmp> era value
cols = tmp$era

# 18. Plotting graph of <tmp> by different era color
#par(mfrow=c(2,2),bg="#ffffff") # for colxrow layout
plot(tmp$age ~ tmp$fl,ylim=c(0,15), xlim=c(0,500),xlab="Fork Lenght (mm)",ylab="Age (yrs)",col=cols,pch=pchs)
title('Plot 4: Symbol & Color by Era')

# 19. Drawing regression line on Plot:4
plot(tmp$age ~ tmp$fl,ylim=c(0,15), xlim=c(0,500),xlab="Fork Lenght (mm)",ylab="Age (yrs)",col=cols,pch=pchs)
abline(lm(tmp$age ~ tmp$fl),)
title('Plot 5: Regression Overlay')

# 20. Plotting Legends of Plot 4, Plot 5
plot(tmp$age ~ tmp$fl,ylim=c(0,15), xlim=c(0,500),xlab="Fork Lenght (mm)",ylab="Age (yrs)",col=cols,pch=pchs)
abline(lm(tmp$age ~ tmp$fl),)
legend(1,15, legend=c("in 1977-80", "in 1997-01"),
       col=cols, pch=pchs, cex=0.8)
title('Plot 6: Legend Overlay')

# 21. Github Repo
# https://github.com/Mxnxn/Intro-w-R/tree/master/week_2_ALY6000



