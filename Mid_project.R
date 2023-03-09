library(readxl)
dataset <- read_excel("E:/10th Semester/Data science/Lab Mid/Project/Dataset_midterm.xlsx")
View(dataset)

#finding the shape of the dataset
summary(dataset)
nrow(dataset)
length(dataset) 
dim(dataset)
names(dataset)
str(dataset)

#Show the attributes name
attributes(dataset)#deatailed attributes
ls(dataset)#only attribute names

#the types of data for all attributes
typeof(dataset$id)
typeof(dataset$Age)
typeof(dataset$`weight(kg)`)
typeof(dataset$Delivery_number)
typeof(dataset$Delivery_time)
typeof(dataset$Blood)
typeof(dataset$Heart)
typeof(dataset$Caesarian)

dataset=edit(dataset)
dataset[-4,]#del 
dataset[ -c(2) ] #col del

total_missing_value = colSums(is.na(dataset)) #find number of missing value
total_missing_value

#Finding the specific row number of Null values
which(is.na(dataset$Age))
which(is.na(dataset$`weight(kg)`))
which(is.na(dataset$Delivery_number))
which(is.na(dataset$Delivery_time))
which(is.na(dataset$Heart))
which(is.na(dataset$Caesarian))
which(is.na(dataset$Blood))

remove_missing_value <- na.omit(dataset)# remove missing value
dataset1<-remove_missing_value
dataset1


#mean for Age,Weight,Delivery_number Delivery_time Blood and Caesarian attributes
mean(dataset1$Age)
mean(dataset1$`weight(kg)`)
mean(dataset1$Delivery_number)
mean(dataset1$Delivery_time)
mean(dataset1$Caesarian)
mean(dataset1$Heart)

#median for Age,Weight,Delivery_number Delivery_time Blood and Caesarian attributes
install.packages("DescTools")
library(DescTools)
modeValue_Age<- Mode(dataset1$Age)
modeValue_Age
modeValue_weight<- Mode(dataset1$`weight(kg)`)
modeValue_weight
modeValue_DN<- Mode(dataset1$Delivery_number)
modeValue_DN
modeValue_DT<- Mode(dataset1$Delivery_time)
modeValue_DT
modeValue_Case<- Mode(dataset1$Caesarian)
modeValue_Case
modeValue_Heart<- Mode(dataset1$Heart)
modeValue_Heart

# mode for Age,Weight,Delivery_number Delivery_time Blood and Caesarian attributes
median(dataset1$Age)
median(dataset1$`weight(kg)`)
median(dataset1$Delivery_number)
median(dataset1$Delivery_time)
median(dataset1$Caesarian)
median(dataset1$Heart)
# Categorical to numeric conversion(Blood value)
dataset1$Blood = factor(dataset1$Blood,
                       levels = c("high","normal","low"),
                       labels = c(2,1,0))
d2<-dataset1$Blood
d2
dataset1$Blood= as.numeric(dataset1$Blood)
mean(dataset1$Blood,na.rm = TRUE) #Mean
median(dataset1$Blood,na.rm = TRUE) #median
modeValue <- Mode(dataset1$Blood)#mode
modeValue
#Replacing missing value with mean
dataset$Age[is.na(dataset$Age)] = mean(dataset1$Age, na.rm = TRUE)

dataset$`weight(kg)`[is.na(dataset$`weight(kg)`)] = mean(dataset1$`weight(kg)`, na.rm = TRUE)
dataset$Delivery_number[is.na(dataset$Delivery_number)] = mean(dataset1$Delivery_number, na.rm = TRUE)
dataset$Delivery_time[is.na(dataset$Delivery_time)] = mean(dataset1$Delivery_time, na.rm = TRUE)
dataset$Heart[is.na(dataset$Heart)] = mean(dataset1$Heart, na.rm = TRUE)
dataset$Caesarian[is.na(dataset$Caesarian)] = mean(dataset1$Caesarian, na.rm = TRUE)
dataset$Blood[is.na(dataset$Blood)] = mean(dataset1$Blood, na.rm = TRUE)

total_missing_value = colSums(is.na(dataset)) #find number of missing value
total_missing_value
#Replacing missing value with median
dataset$Age[is.na(dataset$Age)] = median(dataset1$Age, na.rm = TRUE)
dataset$`weight(kg)`[is.na(dataset$`weight(kg)`)] = median(dataset1$`weight(kg)`, na.rm = TRUE)
dataset$Delivery_number[is.na(dataset$Delivery_number)] = median(dataset1$Delivery_number, na.rm = TRUE)
dataset$Delivery_time[is.na(dataset$Delivery_time)] = median(dataset1$Delivery_time, na.rm = TRUE)
dataset$Heart[is.na(dataset$Heart)] = median(dataset1$Heart, na.rm = TRUE)
dataset$Caesarian[is.na(dataset$Caesarian)] = median(dataset1$Caesarian, na.rm = TRUE)
dataset$Blood[is.na(dataset$Blood)] = median(dataset1$Blood, na.rm = TRUE)

#Replacing missing value with mode
dataset$Age[is.na(dataset$Age)] = Mode(dataset1$Age, na.rm = TRUE)
dataset$`weight(kg)`[is.na(dataset$`weight(kg)`)] = Mode(dataset1$`weight(kg)`, na.rm = TRUE)
dataset$Delivery_number[is.na(dataset$Delivery_number)] = Mode(dataset1$Delivery_number, na.rm = TRUE)
dataset$Delivery_time[is.na(dataset$Delivery_time)] = Mode(dataset1$Delivery_time, na.rm = TRUE)
dataset$Heart[is.na(dataset$Heart)] = Mode(dataset1$Heart, na.rm = TRUE)
dataset$Caesarian[is.na(dataset$Caesarian)] =Mode(dataset1$Caesarian, na.rm = TRUE)
dataset$Blood[is.na(dataset$Blood)] = Mode(dataset1$Blood, na.rm = TRUE)

#Calucalting Standard Deviation
sd(dataset1$Age)
sd(dataset1$`weight(kg)`)
sd(dataset1$Delivery_number)
sd(dataset1$Delivery_time)
sd(dataset1$Heart)
sd(dataset1$Caesarian)
sd(dataset1$Blood)

#to find ranges of all attributes
install.packages("dplyr")
library(dplyr)

range(dataset1$Age)
range(dataset1$`weight(kg)`)
range(dataset1$Delivery_number)
range(dataset1$Delivery_time)
range(dataset1$Blood)
range(dataset1$Heart)
range(dataset1$Caesarian)

#detecting outlier for blood
index_vec = c(1:68)
plot(index_vec,dataset1$Blood)
dataset1$Blood[dataset1$Blood<=500] <- NA

#detecting outlier for Weight
index_vec = c(1:68)
plot(index_vec,dataset1$`weight(kg)`)
dataset1$`weight(kg)`[dataset1$`weight(kg)`<=5000] <- NA

#detecting outlier for Heart
index_vec = c(1:68)
plot(index_vec,dataset1$Heart)
dataset1$Heart[dataset1$Heart<=500] <- NA









