library(data.table)
library(earth)
library(forecast)
library(reshape)
library(plyr)

readData <- function(){
  #Load the data with the correct categorical classes
  class <- c('factor', 'factor', 'Date', 'numeric', 'factor', 'factor','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric' )
  sales <- read.csv('combinedData.csv', colClasses = class)
}


sales <- readData(); summary(sales)
str(sales)

train <- subset(sales, Date <= '2012-02-24',select=colnames(sales))
test <- subset(sales, Date > '2012-02-24',select=colnames(sales))
print(paste0('Percentage Train = ', round(nrow(train)/nrow(sales), 2), '; Percentage Test = ', round(nrow(test)/nrow(sales), 2)))

##Model 1: Linear Regression with All Variables--------------------------------------------------------------------------
m1 <- lm(Weekly_Sales ~ ., data = train)
summary(m1)

#test RMSE
predict<-predict(m1, newdata = test)
RMSE.m1<-sqrt(mean((test$Weekly_Sales-predict)^2))
RMSE.m1
#12175.25
summary(sales)

#Check Validity of Assumptions
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))

#Model 2: Linear Model with Time Series Component------------------------------------------------------------------------
tr.store <- unique(tr.d$Store)
runTSLM <- function(train, test){
  for(j in unique(train$Store)){#for every store
    y <- ts(train[train$Store==j, "Weekly_Sales"], frequency = 52)
    #convert weekly sales of that store into a time series object
    model <- tslm(y ~ trend + season)
    fc <- forecast(model, h = nrow(test[test$Store==j, ]))
    test[test$Store==j,'Weekly_Sales'] <- as.numeric(fc$mean)
    #fcast$mean is the result of the forecast (the application of the model to the future)
  }
  test
}

train.df <- data.frame(Date = rep(unique(train$Date), length(unique(test$Store))),
                       Store = rep(unique(test$Store), each = length(unique(train$Date))),
                       Dept =0)
predict.df <- data.frame(Date = rep(unique(test$Date), length(unique(test$Store))),
                         Store = rep(unique(test$Store), each = length(unique(test$Date))), Dept=0)

test.depts <- unique(test$Dept)
test.depts <- test.depts[length(test.depts):1]
test$Dept

for(d in unique(test$Dept)){
  tr.d <- train.df
  tr.d$Dept=d
  tr.d <- join(tr.d, train[train$Dept == d, c('Store','Date','Weekly_Sales', 'IsHoliday', 'Type', 'Size', 'Temperature','Fuel_Price','MarkDown1','MarkDown2','MarkDown3','MarkDown4','MarkDown5','CPI','Unemployment')])
#for the particular department, sales in the store on that date.
  pr.d <- predict.df
  pr.d$Weekly_Sales <- 0

  pr.d$Dept=d
  
  #remove na
  tr.d[is.na(tr.d)]<-0 
  tr.d$IsHoliday[is.na(tr.d$IsHoliday)] <- 'FALSE'
  tr.d$Type[is.na(tr.d$Type)] <- "A"
  result <- runTSLM(tr.d, pr.d)
  
if(d==1){
  final<-result
}else{
  final<-rbind(final, result)
}
}
str(final)
final$Dept<-factor(final$Dept)
names(final)[names(final) == "Weekly_Sales"] <- "Weekly_Sales_predicted"
#Test: RMSE
predict.m4 <- test
predict.m4<-join(predict.m4, final, type='inner', by=c('Date', 'Store', 'Dept'))
test.m4.error <- predict.m4$Weekly_Sales - predict.m4$Weekly_Sales_predicted
RMSE.m4.test <- sqrt(mean((test.m4.error)^2)); RMSE.m4.test #3345.987
summary(abs(test.m4.error))
#RMSE 3k, the best model so far

#--model 3---Multivairabte Time Series model with other variables-------------------------------------------------------------------------------
train.df <- data.frame(Date = rep(unique(train$Date), length(unique(test$Store))),
                       Store = rep(unique(test$Store), each = length(unique(train$Date))),
                       Dept =0)
predict.df <- data.frame(Date = rep(unique(test$Date), length(unique(test$Store))),
                         Store = rep(unique(test$Store), each = length(unique(test$Date))), Dept=0)

library(vars)
tr.store <- unique(tr.d$Store)

runVARS <- function(train, test){
  for(j in unique(train$Store)){#for every store
#is for a particular store, particular department. Hence do not need to include type and size into the model
    #build var model:
    input<-cbind(CPI=train$CPI, Unemployment=train$Unemployment, Sales=train$Weekly_Sales)
    ts(input, frequency=52)
    #Lag optimisation
    VARselect(input, lag.max = 10, type = "both")
    #Vector autoregression with lags set according to results of lag optimisation. 
    var = VAR(input, p=2)
    #Forecasting
    fc <- predict(var, n.ahead = nrow(test[test$Store==j, ]))
    print(fc)
    test[test$Store==j,'Weekly_Sales'] <- as.numeric(fc$fcst$Sales[, 'fcst'])
  }
  test
}


for(d in unique(test$Dept)){
  d=5
  tr.d <- train.df
  tr.d$Dept=d
  tr.d <- join(tr.d, train[train$Dept == d, c('Store','Date','Weekly_Sales', 'IsHoliday', 'Type', 'Size', 'Temperature','Fuel_Price','MarkDown1','MarkDown2','MarkDown3','MarkDown4','MarkDown5','CPI','Unemployment')])
  #for the particular department, sales in the store on that date.
  pr.d <- predict.df
  pr.d$Weekly_Sales <- 0
  
  pr.d$Dept=d
  #tr.d got NA.
  
  
    for(j in tr.store){
   m=mean(tr.d$Weekly_Sales[tr.d$Store==j], na.rm = TRUE)
   tr.d$Weekly_Sales[is.na(tr.d$Weekly_Sales)] <- m}
  
  tr.d[is.na(tr.d)]<-0
  tr.d$IsHoliday[is.na(tr.d$IsHoliday)] <- 'FALSE'
  tr.d$Type[is.na(tr.d$Type)] <- "A"
  result <- runVARS(tr.d, pr.d)
  
  if(d==1){
    final<-result
  }else{
    final<-rbind(final, result)
  }
}
str(final)
final$Dept<-factor(final$Dept)
names(final)[names(final) == "Weekly_Sales"] <- "Weekly_Sales_predicted"
#Test: RMSE
predict.m4 <- test
predict.m4<-join(predict.m4, final, type='inner', by=c('Date', 'Store', 'Dept'))
test.m4.error <- predict.m4$Weekly_Sales - predict.m4$Weekly_Sales_predicted
RMSE.m4.test <- sqrt(mean((test.m4.error)^2)); RMSE.m4.test #3345.987
summary(abs(test.m4.error))
#RMSE 15322

#---------------------MARS------------------
#speed up mars by changing thresh, fask.k and degree
mars<-earth(Weekly_Sales~., degree = 1, data = train, fast.k=5, thresh=0.01, minspan=1)
summary(mars)
predict.mars<-predict(mars, newdata = test)
RMSE.mars<-sqrt(mean((test$Weekly_Sales-predict.mars)^2))
RMSE.mars
#14648.6 Importance: only department

#see if get better result with default fast.k and thresh
mars1<-earth(Weekly_Sales~., degree = 1, data = train, minspan=1)
summary(mars1)
predict.mars<-predict(mars1, newdata = test)
RMSE.mars1<-sqrt(mean((test$Weekly_Sales-predict.mars)^2))
RMSE.mars1
#12674, Importance: department, Size, store/markdown.

#change degree, minspan
mars2<-earth(Weekly_Sales~., degree = 2, data = train)
summary(mars2)
predict.mars2<-predict(mars2, newdata = test)
RMSE.mars2<-sqrt(mean((test$Weekly_Sales-predict.mars2)^2))
RMSE.mars2
#16057, got worse when degree is 2.

#add 10 fold
mars3<-earth(Weekly_Sales~., degree = 1, data = train, nfold = 10, fast.k=5)
summary(mars3)
predict.mars<-predict(mars3, newdata = test)
RMSE.mars3<-sqrt(mean((test$Weekly_Sales-predict.mars)^2))
RMSE.mars3
#taking too long to terminate