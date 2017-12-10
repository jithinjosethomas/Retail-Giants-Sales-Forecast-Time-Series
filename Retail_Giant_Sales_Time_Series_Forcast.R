#################  Retail Giant Sales Forecasting #################

#Install the libraries
install.packages('forecast', dependencies = TRUE)

#Loading the required libraries
library(forecast)
library(tseries)
require(graphics)
library(dplyr)
library(stats)
library(lubridate)
library(caret)
library(car)
library(MASS)
library(GGally)
library(ggplot2)
library(caTools)
library(scales)

################ BUSINESS PROBLEM ##################

#“Global Mart” is an online store super giant having worldwide operations. 
#It takes orders and delivers across the globe and deals with all the major
#product categories - consumer, corporate & home office.

#Now as a sales/operations manager, you want to finalise the plan for the next 6 months.  
#So, you want to forecast the sales and the demand for the next 6 months, that would help 
#you manage the revenue and inventory accordingly.

#The store caters to 7 different market segments and in 3 major categories.
#You want to forecast at this granular level, so you subset your data into 21 (7*3) buckets 
#before analysing these data.

#But not all of these 21 market buckets are important from the store’s point of view. 
#So you need to find out 2 most profitable (and consistent) segment from these 21 
#and forecast the sales and demand for these segments.


#Loading the file into R
superstore_data <- read.csv("Global Superstore.csv", header = T, sep = ',')

#Understanding Dimensions
View(superstore_data)
dim(superstore_data)  #51290 rows #24 columns

#Structure of the dataset
str(superstore_data)

#printing first few rows
head(superstore_data)

#Exploring the data
summary(superstore_data)

#Checking missing values
sapply(superstore_data, function(x) sum(is.na(x)))

#41296 NA values present in postal code, 
#Postal code represents the district
#in our final data, we will impute it with 0
superstore_data[which(is.na(superstore_data$Postal.Code)),12] <- 0

#Check Again, after imputing
sum(is.na(superstore_data)) # 0 NA's

#Standardizing date
superstore_data$Order.Date<- dmy(superstore_data$Order.Date)
superstore_data$Ship.Date<- dmy(superstore_data$Ship.Date)

#Check for unique values
nrow(unique(superstore_data)) # all unique entries

#Check for market segments # Regions
superstore_data$Market <- as.factor(superstore_data$Market)
levels(superstore_data$Market) #"Africa" "APAC"   "Canada" "EMEA"   "EU"     "LATAM"  "US"#

#Check for segments # Product segments
superstore_data$Segment <- as.factor(superstore_data$Segment)
levels(superstore_data$Segment) #"Consumer"    "Corporate"   "Home Office"#

#Combining market and segment into new column Market_Segment
superstore_data$Market_Segment <- paste(superstore_data$Market, superstore_data$Segment, sep="_")
#Correcting Date
superstore_data$Month <- month(as.POSIXlt(superstore_data$Order.Date, format="%d-%m-%Y"))
superstore_data$Year <- year(as.POSIXlt(superstore_data$Order.Date, format="%d-%m-%Y"))
# superstore_data$Year <- format(as.Date(superstore_data$Order.Date), "%Y-%m")


################### EDA ###################

#Check for outliers using boxplot in Sales
boxplot(superstore_data$Sales) 
quantile(superstore_data$Sales,seq(0,1,0.01))
#Outliers are present in Sales, so imputation is necessary
#For Sales
#Lets try to cap with 95th percentile values at the top
superstore_data$Sales <- squish(superstore_data$Sales, quantile(superstore_data$Sales, c(.01, .95)))
#All good !!!


#Check for outliers using boxplot in Quantity
boxplot(superstore_data$Quantity) 
quantile(superstore_data$Quantity,seq(0,1,0.01))
#Outliers are present in Quantity, so imputation is necessary
#For Quantity
#Lets try to cap with 95th percentile values at the top
superstore_data$Quantity <- squish(superstore_data$Quantity, quantile(superstore_data$Quantity, c(.01, .95)))
#All good !!!


#Check for outliers using boxplot in Discount
boxplot(superstore_data$Discount) 
#Outliers are present in Discount, so imputation is necessary
quantile(superstore_data$Discount,seq(0,1,0.01))
#For Discount
#Lets try to cap with 95th percentile values at the top
superstore_data$Discount <- squish(superstore_data$Discount, quantile(superstore_data$Discount, c(.01, .95)))
#All good !!!

#Check for outliers using boxplot in Profit
boxplot(superstore_data$Profit) 
quantile(superstore_data$Profit,seq(0,1,0.01))
#For profit
#Lets try to cap with 95th and below the 5th percentile values
superstore_data$Profit <- squish(superstore_data$Profit, quantile(superstore_data$Profit, c(.05, .95)))
#All good !!!

#Check for outliers using boxplot in Shipping Cost
boxplot(superstore_data$Shipping.Cost) 
#Outliers are present in Shipping Cost, so imputation is necessary
quantile(superstore_data$Shipping.Cost,seq(0,1,0.01))
#For Shipping Cost
#Lets try to cap with 95th and below the 5th percentile values
superstore_data$Shipping.Cost <- squish(superstore_data$Shipping.Cost, quantile(superstore_data$Shipping.Cost, c(.05, .95)))
#All good !!!


################## Multivariate Analysis #######################

corr_matrix <- subset(superstore_data,select=c(Sales,Quantity,Discount,Profit,Shipping.Cost))
cor(corr_matrix)

#Changing all value to positive 
superstore_data$Profit <- superstore_data$Profit - min(superstore_data$Profit)

store_MS_group <- group_by(superstore_data, Market_Segment=Market_Segment,Month = Month,Year=Year)
View(store_MS_group)
store_MS_df <- summarise(store_MS_group,Sales = sum(Sales),Quantity= sum(Quantity),Profit = sum(Profit))
View(store_MS_df)


#Order each market segment by month and year
store_MS_df <- store_MS_df[order(store_MS_df$Market_Segment, store_MS_df$Year,store_MS_df$Month),]

#To find coefficient of variation to compare the top2 market segments
store_MS_cov_group <- group_by(store_MS_df,Market_Seg=Market_Segment)
store_MS_cov_df <- summarise(store_MS_cov_group, Total_Profit = sum(Profit),Mean_profit =mean(Profit),SD_profit =sd(Profit))
store_MS_cov_df$COV <- store_MS_cov_df$SD_profit/store_MS_cov_df$Mean_profit                              #COV = (sd(Profit, na.rm=TRUE)/mean(Profit, na.rm=TRUE))*100)


View(store_MS_cov_df)
#Order by Profit and COV to find and compare the top most market segments
store_MS_cov_df <- store_MS_cov_df[order(-store_MS_cov_df$Total_Profit, store_MS_cov_df$COV),]
View(store_MS_cov_df)


#Top 2 segments on analysis are
#APAC_Consumer with total profit of 654616.148 and COV of 0.4430291
#EU_Consumer with total profit of 599659.444 and COV of 0.4881849

#Plots for multivariate analysis
##################################
bp_profit <- ggbarplot(store_MS_cov_df, x = "Market_Seg", y = "Total_Profit",
                       fill = "Market_Seg",               # change fill color by cyl
                       color = "white",            # Set bar border colors to white
                       sort.val = "asc",           # Sort the value in ascending order
                       sort.by.groups = TRUE,      # Sort inside each group
                       x.text.angle = 90           # Rotate vertically x axis texts
)
bp_profit + font("x.text", size = 8)

bp_cov <- ggbarplot(store_MS_cov_df, x = "Market_Seg", y = "COV",
                    fill = "Market_Seg",               # change fill color by cyl
                    color = "white",            # Set bar border colors to white
                    sort.val = "asc",           # Sort the value in ascending order
                    sort.by.groups = FALSE,      # Sort inside each group
                    x.text.angle = 90           # Rotate vertically x axis texts
)
bp_cov + font("x.text", size = 8)

##################################

#Get details of the 2 Profitable segments we are interested in
Profit_Consumer_df <- store_MS_df[store_MS_df$Market_Segment %in% c("APAC_Consumer","EU_Consumer"), ] 

Profit_Consumer_group <- group_by(Profit_Consumer_df, Months = Month,Year=Year)
Profit_df <- summarise(Profit_Consumer_group, 
                         Sales = sum(Sales),Quantity= sum(Quantity),Profit = sum(Profit))

#Order interested profit data by month and year
Profit_df <- Profit_df[order(Profit_df$Year,Profit_df$Months),]

dim(Profit_df)  #48 rows #5 columns


#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later
#Use timeser<-ts(<name# | of the variable>) to create a time series
total_timeser_Sales <- ts(Profit_df$Sales)
total_timeser_Quantity <- ts(Profit_df$Quantity)
inProfitSeg <- Profit_df[1:42,]
timeserSales <- ts(inProfitSeg$Sales)
timeserQuantity <- ts(inProfitSeg$Quantity)
cols1 <- c("red", "blue")
labels1 <- c("Raw", "Smoothed")
ylab1 <- c("Interested Store Sales")
xlab1 <- c("Months from 2011 to 2014")
title1 <- c("Interested Store Sales: 2011 to 2014")

#Plot Sales
plot(timeserSales, main=title1, xlab = xlab1, ylab = ylab1, col=cols1[1])

ylab2 <- c("Interested Store Sales Quantity")
title2 <- c("Interested Store Sales Quantity: 2011 to 2014")

#Plot Quantity
plot(timeserQuantity, main=title2, xlab = xlab1, 
     ylab = ylab2, col=cols1[1])

#Smoothing the series for Sales and Quantity - Moving Average Smoothing

w <-1
smoothedseriesSales <- stats::filter(timeserSales, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

smoothedseriesQuantity <- stats::filter(timeserQuantity, 
                                     filter=rep(1/(2*w+1),(2*w+1)), 
                                     method='convolution', sides=2)


#Smoothing left end of the time series for Sales and Quantity 

diffSales <- smoothedseriesSales[w+2] - smoothedseriesSales[w+1]
for (i in seq(w,1,-1)) {
  smoothedseriesSales[i] <- smoothedseriesSales[i+1] - diffSales
}

diffQuantity <- smoothedseriesQuantity[w+2] - smoothedseriesQuantity[w+1]
for (i in seq(w,1,-1)) {
  smoothedseriesQuantity[i] <- smoothedseriesQuantity[i+1] - diffQuantity
}


#Smoothing right end of the time series for Sales and Quantity 

n <- length(smoothedseriesSales)
diffSales <- smoothedseriesSales[n-w] - smoothedseriesSales[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseriesSales[i] <- smoothedseriesSales[i-1] + diffSales
}

n <- length(smoothedseriesQuantity)
diffQuantity <- smoothedseriesQuantity[n-w] - smoothedseriesQuantity[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseriesQuantity[i] <- smoothedseriesQuantity[i-1] + diffQuantity
}



#Plot the smoothed time series for Sales and Quantity 
#Plot Sales TS
plot(timeserSales, main=title1, xlab = xlab1, 
     ylab = ylab1, col=cols1[1])
#Plot Sales TS smoothed
lines(smoothedseriesSales, col="blue", lwd=2)

#Plot Quantity TS
plot(timeserQuantity, main=title2, xlab = xlab1, 
     ylab = ylab2, col=cols1[1])
#Plot Quantity TS smoothed
lines(smoothedseriesQuantity, col="blue", lwd=2)




#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

timevals_mon <- inProfitSeg$Months
timevals_yr <- inProfitSeg$Year
smootheddf_ProfitSeg <- as.data.frame(cbind(timevals_mon,timevals_yr, as.vector(smoothedseriesSales),as.vector(smoothedseriesQuantity)))
colnames(smootheddf_ProfitSeg) <- c('Month','year', 'Sales','Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
#Sales
plot(timeserSales, main=title1, xlab = xlab1, 
     ylab = ylab1, col=cols1[1])

lmfitSales <- lm(Sales ~ sin(0.4*Month) * poly(Month,4) + cos(0.4*Month) * poly(Month,4)
                 + Month, data=smootheddf_ProfitSeg)

global_pred_sales <- predict(lmfitSales, Month=timevals_mon,Year=timevals_yr)
summary(global_pred_sales)
lines(global_pred_sales, col="green", lwd=2)
lines(smoothedseriesSales, col="blue", lwd=2) 
legend('topright', c("Original","Smoothened", "Predicted"), lty=1, col=c('red', 'blue', 'green',' brown'), bty='n', cex=.75)


#Quantity
plot(timeserQuantity, main=title2, xlab = xlab1, 
     ylab = ylab2, col=cols1[1])

lmfitQuantity <- lm(Quantity ~ sin(0.4*Month) * poly(Month,4) + cos(0.4*Month) * poly(Month,4)
                    + Month, data=smootheddf_ProfitSeg)
global_pred_Quantity <- predict(lmfitQuantity, Month=timevals_mon,Year=timevals_yr)
summary(global_pred_Quantity)
lines(global_pred_Quantity, col='green', lwd=2)# Predicted sales values
lines(smoothedseriesQuantity, col="blue", lwd=2) #Smoothed Sales values
legend('topright', c("Original","Smoothened", "Predicted"), lty=1, col=c('red', 'blue', 'green',' brown'), bty='n', cex=.75)


#Now, let's look at the locally predictable series
#We will model it as an ARIMA series

#Sales
local_predSales <- timeserSales-global_pred_sales
plot(local_predSales, col='red', type = "l")
acf(local_predSales)
acf(local_predSales, type="partial")

p=2
q=2
order <- c(p,0,q)
armafitSales <- Arima(local_predSales,order=order,method="ML")

tsdiag(armafitSales)
armafitSales

#Quantity
local_predQuantity <- timeserQuantity-global_pred_Quantity
plot(local_predQuantity, col='red', type = "l")
acf(local_predQuantity)
acf(local_predQuantity, type="partial")
p=2
q=2
order <- c(p,0,q)
armafitQuantity <- Arima(local_predQuantity,order=order,method="ML")


tsdiag(armafitQuantity)
armafitQuantity


#We'll check if the residual series is white noise

resiSales <- local_predSales-fitted(armafitSales)

adf.test(resiSales,alternative = "stationary")
kpss.test(resiSales)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outProfitSeg <- Profit_df[43:48,]
timevals_outmm <- outProfitSeg$Months
timevals_outyr <- outProfitSeg$Year

global_pred_outSales <- predict(lmfitSales,data.frame(Month =timevals_outmm,Year=timevals_outyr))
global_pred_outQuantity <- predict(lmfitQuantity,data.frame(Month =timevals_outmm,Year=timevals_outyr))

fcastSales <- global_pred_outSales
fcastQuantity <- global_pred_outQuantity

#Now, let's compare our prediction with the actual values, using MAPE
fcastSales
as.data.frame(fcastSales)

MAPE_class_Sales <- accuracy(fcastSales,outProfitSeg$Sales)[5]
MAPE_class_Sales

MAPE_class_Quantity <- accuracy(fcastQuantity,outProfitSeg$Quantity)[5]
MAPE_class_Quantity

#Let's also plot the predictions along with original values, to
#Get a visual feel of the fit

class_sales_pred <- c(ts(global_pred_sales),ts(global_pred_outSales))
plot(total_timeser_Sales, col = "black", main = "Sales prediction vs Actual values")
lines(class_sales_pred, col = "red")
legend('topleft', c("Original", "Predicted"), lty=1, col=c('black', 'red'), bty='n', cex=.75)

class_quantity_pred <- c(ts(global_pred_Quantity),ts(global_pred_outQuantity))
plot(total_timeser_Quantity, col = "black", main="Quantity prediction vs Actual values")
lines(class_quantity_pred, col = "red")
legend('topleft', c("Original", "Predicted"), lty=1, col=c('black', 'red'), bty='n', cex=.75)

#So, that was classical decomposition, now let's do an ARIMA fit

#Sales

p=2
q=2
order <- c(p,0,q)
autoarimaSales <- Arima(timeserSales,order=order,method="ML")

autoarimaSales
tsdiag(autoarimaSales)
plot(autoarimaSales$x, col="black")
lines(fitted(autoarimaSales), col="red")

#Quantity

p=2
q=2
order <- c(p,0,q)
autoarimaQuantity <- Arima(timeserQuantity,order=order,method="ML")

autoarimaQuantity
tsdiag(autoarimaQuantity)
plot(autoarimaQuantity$x, col="black")
lines(fitted(autoarimaQuantity), col="red")

#Again, let's check if the residual series is white noise

#Sales
resi_auto_arimaSales <- timeserSales - fitted(autoarimaSales)
adf.test(resi_auto_arimaSales,alternative = "stationary")
kpss.test(resi_auto_arimaSales)

#Quantity
resi_auto_arimaQuantity <- timeserQuantity - fitted(autoarimaQuantity)
adf.test(resi_auto_arimaQuantity,alternative = "stationary")
kpss.test(resi_auto_arimaQuantity)

#Also, let's evaluate the model using MAPE

#Sales
fcast_auto_arimaSales <- predict(autoarimaSales, n.ahead = 6)
MAPE_auto_arimaSales <- accuracy(fcast_auto_arimaSales$pred,outProfitSeg$Sales)[5]
MAPE_auto_arimaSales

#Quantity
fcast_auto_arimaQuantity <- predict(autoarimaQuantity, n.ahead = 6)
MAPE_auto_arimaQuantity <- accuracy(fcast_auto_arimaQuantity$pred,outProfitSeg$Quantity)[5]
MAPE_auto_arimaQuantity

#Lastly, let's plot the predictions along with original values, to
#Get a visual feel of the fit

#Sales
auto_arima_predSales <- c(fitted(autoarimaSales),ts(fcast_auto_arimaSales$pred))
plot(total_timeser_Sales, col = "black", main="Sales Actual vs Predicted Values")
lines(auto_arima_predSales, col = "red")
legend('topleft', c("Original","Predicted"), lty=1, col=c('black', 'red'), bty='n', cex=.75)

#Quantity
auto_arima_predQuantity <- c(fitted(autoarimaQuantity),ts(fcast_auto_arimaQuantity$pred))
plot(total_timeser_Quantity, col = "black", main="Quantity Actual vs Predicted Values")
lines(auto_arima_predQuantity, col = "red")
legend('topleft', c("Original","Predicted"), lty=1, col=c('black', 'red'), bty='n', cex=.75)

