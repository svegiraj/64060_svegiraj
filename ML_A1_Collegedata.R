##Importing college dataset taken from github
setwd("C:/Users/Ayyappa/OneDrive/Desktop/Machine Learning")
coldat <- read.csv("College_data.csv")
coldat

##printing descriptive statistics

#mode for the categorical value Private
sort(table(coldat$Private),decreasing = TRUE) [1] 

##To display a table of various descriptive statistics of Enrolled students
library(pastecs)
options(scipen=100) ## To avaoid the scientific notation and change the format of display
options(digits=2)
stat.desc(coldat$Enroll)

##To display top 6 records of the college data
head(coldat)

##To display last 6 records of the college data
tail(coldat)

##rank transformation of PhD students
rank(head(coldat$PhD)) 

##histogram for the student faculty ratio
hist(coldat$S.F.Ratio,col=5, breaks=15, xlab = "Student/faculty ratio", main = "")

##Scatter plot
##High tuition correlates to high graduation rate
plot(Grad.Rate ~ Outstate, data = coldat)


