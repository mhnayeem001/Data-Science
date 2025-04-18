install.packages("dplyr")
library(dplyr)

#importing the csv file
mydata<- read.csv("E:/8th Sem/MIDTERM/DATA SCIENCE/project/Titanic mid project data.csv",header = TRUE, sep = ",")
View(mydata)

#correcting invalid values
mydata$who <- gsub("mannn", "man", mydata$who)
mydata

#correcting values of who attributes
mydata <- mydata %>%
  mutate(
    who = case_when(
      Gender == "male" & who %in% c("woman", "child") ~ "man",
      Gender == "female" & who %in% c("man", "child") ~ "woman",
      TRUE ~ who
    )
  )
mydata

#checking child values
filter(mydata,who=="child")
alvi<-filter(mydata,age<16)

#factor function- for making level of categorical data
mydata <- mydata %>%
  mutate(
    Gender = factor(Gender, levels = c("male", "female"), labels = c(1, 2)),
    embarked   = factor(embarked, levels = c("C","Q","S"), labels = c(1,2,3)),
    class    = factor(class , levels = c("First", "Second", "Third"), labels = c(1, 2, 3)),
    who = factor(who, levels = c("man", "woman","child"), labels = c(1, 2,3)),
    alone = factor(alone, levels = c("TRUE", "FALSE"), labels = c(1, 2))
  )
mydata

#to know the structure of the data set
str(mydata)

#Finding numeric columns
numeric_columns <- sapply(mydata, is.numeric)
numeric_columns

#detecting null values for the data set
is.na(mydata)

#counting number of null values in each column
colSums(is.na(mydata))

#measure of central tendency and spread
vars<-c("age","sibsp","parch","survived")
summary(mydata[vars])

#to find the verience
varience<-var(mydata$sibsp)# there are missing values
varience

#to find the standard deviation
s<-mydata$age
sd(s)