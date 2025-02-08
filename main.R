#import the data set into r

data <- read.csv("./data/ecommerce_user")

View(data)
summary(data)


#plot for correlation between different variables 
library(ggplot2)

ggplot(data, aes(data$Avg..Session.Length,data$Yearly.Amount.Spent)) +
  geom_point(color="orange") +
  ggtitle("Correlation between Session length and Yearly amount spent") +
  xlab("Avarage Session Length") +
  ylab("Yearly Spent Amount")


#plot correlation between length of membership and yearly amount spent
ggplot(data, aes(data$Length.of.Membership,data$Yearly.Amount.Spent)) +
  geom_point(color = "black")

#create a pairplot between different variables
pairs(data[c("Avg..Session.Length",
             "Length.of.Membership",
             "Yearly.Amount.Spent"
             )])


#explore the selected variables 
ggplot(data, aes(data$Length.of.Membership)) +
  geom_histogram(color = "white",
                 binwidth = .5,
                 fill = "orange")

hist(data$Yearly.Amount.Spent)
ggplot(data, aes(Yearly.Amount.Spent)) +
  geom_histogram(color = "white",
                 binwidth = 50,
                 fill = "orange")


#So we can see that both data are normally distributed
#Now we can do model fitting for our linear regression
