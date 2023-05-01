#install packages

install.packages("readr")
install.packages("dplyr")
install.packages("cluster")
install.packages("ggfortify")
install.packages("ggplot2")

# required libraries
library(cluster)
library(dplyr)
library(readr)
library(ggfortify)
library(ggplot2)
dataM <- read_csv("E:/10th Semester/Data science/Final/Project/Mall_Customers.csv")
View(Mall_Customers)

#To see all Attributes
ls(dataM)
head(dataM)
summary(dataM)
#to show the relationship between Age and Annual Income
scatter <- ggplot(data = dataM, aes(x = Age, y = 'Annual Income (k$)'))
scatter + geom_point(aes(color = Genre, shape = Genre)) +
  theme_bw() +
  xlab("Age") +
  ylab("Annual Income") +
  ggtitle("Age-Annual Income")
#to show the relationship between Age and Spending Score
scatter <- ggplot(data = dataM, aes(x = Age, y = 'Spending Score (1-100)'))
scatter + geom_point(aes(color = Genre, shape = Genre)) +
  theme_bw() +
  xlab("Age") +
  ylab("Spending Score (1-100)") +
  ggtitle("Age-Spending Score (1-100)")
#to show the relationship between Annual Income  and Spending Score
scatter <- ggplot(data = dataM, aes(x = 'Annual Income (k$)', y = 'Spending Score (1-100)'))
scatter + geom_point(aes(color = Genre, shape = Genre)) +
  theme_bw() +
  xlab("Annual Income (k$)") +
  ylab("Spending Score (1-100)") +
  ggtitle("Annual Income (k$)-Spending Score (1-100)")

#Convert genre to numeric
dataM$Genre = factor(dataM$Genre,
                        levels = c("Male","Female"),
                        labels = c(1,0))

dataM$Genre= as.numeric(dataM$Genre)

#Customer id has no effect on dataset
dataM1 = select(dataM,c(2,3,4,5))

# scale data
scale_dataM1 =scale(dataM1)
#distance
dataM1_dist=dist(scale_dataM1)
dataM1_dist
#wss plot function
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}
#wss plot to choose max no of cluster
wssplot(scale_dataM1)
# k means cluster
KM = kmeans(scale_dataM1,3)
#cluster plot
autoplot(KM,scale_dataM1,frame=TRUE)

#mean value
km <- kmeans(scale_dataM1, centers = 3, nstart = 25)
km

plot(scale_dataM1)


