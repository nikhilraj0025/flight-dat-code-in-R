flight <- read.csv("C:/Users/AKHIL/Desktop/R csv/Flight1987.csv")
View(flight)


flight <- flight[,c(-1)]
View(flight)

library(dplyr)
flight <- mutate(flight,DepDelay1=ifelse(DepDelay>15,1,0))
View(flight)


flight$DepDelay1 <- factor(flight$DepDelay1)
flight$DepDelay1

flight$Origin <- factor(flight$Origin)
flight$Origin

flight$Dest <- factor(flight$Dest)
flight$Dest

fligt$FlightNum <- factor(flight$FlightNum)
flight$FlightNum

flight$UniqueCarrier <- factor(flight$UniqueCarrier)
flight$UniqueCarrier

flight <- flight[,c(-13)]
View(flight)

set.seed(121)
flight_s <- sample(2 ,nrow(flight),replace=TRUE,prob=c(.95,.5))
Train_s  <- flight[flight_s==1,]
Test_s <- flight[flight_s==2,]

table(Train_s$DepDelay1)
Train_s;Test_s
dim(flight)
dim(Train_s)
dim(Test_s)
filter_f <- filter(Train_s,DepDelay1==1)
filter_f
flt <- rbind(Train_s,filter_f)
flt


contents <- table(flt$DepDelay1)
contents

##Model1
model_flight <- glm(DepDelay1~ArrDelay+ActualElapsedTime+CRSElapsedTime,family = binomial,data = flt)
summary(model_flight)
predval <- predict(model_flight,flt,type = "response")
predval
pred_actual_df1 <- data.frame(predval,Test_s$DepDelay1)
pred_actual_df1
##converting probability to 1 and 0
pred_actual_df1 <- mutate(pred_actual_df1,predval=ifelse(predval> .5,1,0))
pred_actual_df1

colnames(pred_actual_df1) <- c("predict","actual")
##Confusion matrix
tab <- table(pred_actual_df1$predict,pred_actual_df1$actual)
tab

##To find accuracy
accuracy <- sum(diag(tab)/sum(tab))
accuracy


contents <- table(flight$Train_s)
contents

##Pie chart for flight delay for top 10 destinations
flight1 <- c(1434,1430,1392,1385,1320,1376,684,583,569,549)
destination <- c('JAN','SLC','FAI','CAE','EWR','SLC','STT','CID','LAX','SPN')
flight1_pie <- pie(flight1,labels=destination,main='flights delay')

sum_flight1 <- sum(flight1)
perc_flight1 <- round((flight1/sum_flight1)*100)
perc_flight1
perc_count_label <- paste(destination,perc_flight1,'%',flight1)
perc_count_label
flight1_PIE_PER <- pie(flight1,labels=perc_count_label,main='flight delay in % for top 10 countries')

##Pie chart for flight delay of top 10 origin
flight1 <- c(1434,1430,1392,1385,1320,1376,684,583,569,549)
Origin <- c('MLU','MSP','ANC','AGS','MCO','ANC','STX','PIA','SFO','GUM')
flight1_pie <- pie(flight1,labels=Origin,main='flights delay')

sum_flight1 <- sum(flight1)
perc_flight1 <- round((flight1/sum_flight1)*100)
perc_flight1
perc_count_label <- paste(Origin,perc_flight1,'%',flight1)
perc_count_label
flight1_PIE_PER <- pie(flight1,labels=perc_count_label,main='flight delay in % for top 10 countries')


