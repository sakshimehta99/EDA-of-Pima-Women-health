install.packages("FSA")
library("FSA")
install.packages("FSAdata")
library("FSAdata")
install.packages("magrittr")
library("magrittr")
install.packages("dplyr")
library("dplyr")
install.packages("tidyr")
library("tidyr")
install.packages("plyr")
library("plyr")
install.packages("plotrix")
library("plotrix")
install.packages("ggplot2")
library("ggplot2")
install.packages("psych")
library("psych")
install.packages("tidyverse")
library("tidyverse")
install.packages("tidyverse", dependencies = TRUE)
install.packages("data.table")
library("data.table")

#loading and importing the dataset

Diabetes <- read.csv(file.choose())

#calculating total number of records
nrow(Diabetes)

#calculating mean & median value of age of women 
round(mean(Diabetes$Age))
median(Diabetes$Age)
min(Diabetes$Age)
max(Diabetes$Age)
#raw dataset
structure(Diabetes)

#skin thickness
Diabetes$SkinThickness <- ifelse(Diabetes$SkinThickness == 0, round(mean(Diabetes$SkinThickness)),Diabetes$SkinThickness)

Diabetes



# datatset with outcome 1

Diabetes1 <- filter(Diabetes, Outcome==1)
Diabetes1
Diabetes2 <- filter(Diabetes, Outcome==0)
Diabetes2
counts <- count(Diabetes1$Age)
counts


#line graph 
ggplot(Diabetes2, aes(x=BMI, y=Age)) +
  geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=2) +
  #theme_ipsum() +
  ggtitle("Glucose vs BMI")

#bar plot (age 21-30)

first <- head(counts,10)
first
bp <- ggplot(first,aes(x,freq))+ 
  geom_bar(stat="identity", colour="Blue", fill="Light Blue")+
  scale_x_continuous(breaks = seq(min(21), max(30), by=1))+
  labs(title ="Classification according to age", x="Age",y="Number of people with Diabetes")+
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25)
  theme(axis.text.x = element_text(size=10))
bp

#bar plot (age 56-70)
last <- tail(counts,10)
last
up <- ggplot(last,aes(x,freq))+ 
  geom_bar(stat="identity", colour="Blue", fill="Light Blue")+
  scale_x_continuous(breaks = seq(min(56), max(70), by=1))+
  labs(title ="Classification according to age", x="Age",y="Number of people with Diabetes")+
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25)
  theme(axis.text.x = element_text(size=10))

up

##bar plot all age groups

alls <- ggplot(counts,aes(x,freq))+ 
  geom_bar(stat="identity", colour="Blue", fill="Light Blue")+
  #scale_x_continuous(breaks = seq(min(21), max(30), by=1))+
  labs(title ="Classification according to age", x="Age",y="Number of people with Diabetes")+
  theme(axis.text.x = element_text(size=6))
alls




#pie chart of the Diabetesetics and non Diabetesetic


  ifDiabetes <- count(Diabetes$Outcome)
  ifDiabetes
  tot<- sum(ifDiabetes$freq)
  tot
  ifDiabetes$freper <- round(ifDiabetes$freq*100/tot)
  
  outcomes <- c(ifDiabetes$freper)
  labels <- c("Non-Diabetic", "Diabetic")
  
  pie(ifDiabetes$freq, labels=ifDiabetes$freper,
   main = "Piechart", col = rainbow(length(ifDiabetes$freq)))
  legend("topright",c(labels), cex = 1.2, fill = rainbow(length(ifDiabetes$freq)))
  
  #CREATES pregnancy vs outcome linegraph
  ggplot(data=Diabetes, aes(x=Pregnancies, y=Outcome)) +
    stat_summary(fun = sum, na.rm = TRUE, color = 'Blue', geom ='line')+
    scale_x_continuous(breaks = seq(min(0), max(20), by=3))+
    scale_y_continuous(breaks = seq(min(0), max(50), by=5))
  
  
  #adding attribute glucose that is relative to the maximum value of glucose
  
 gmax<- max(Diabetes$Glucose)
gmax  

Diabetes$new_glucose <- Diabetes$Glucose*100/gmax
View(Diabetes$new_glucose)

df1 <- data.frame(Diabetes$Glucose,Diabetes$new_glucose)
df1