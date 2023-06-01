#Import Library
library(readxl)
library(pastecs)
library(tidyverse)
library(car)
library(zoo)

### Task 2
#Import dataset
df <- read_excel("K194141728.xlsx", sheet = "balance_sheet")
ratio <- read_excel("K194141728.xlsx", sheet = "ratio")

# Pick and transform needed variables 
df["lev"] = df["Liabilities"]/ df["Total assets"]
df["tang"] = df["Fixed assets"]/ df["Total assets"]
df["inv"] = df["Inventories"]/ df["Current assets"]
df["roa"] = ratio["ROA"]/100

#List of independent and dependent variables
col = c("Quarter", "lev", "tang", "inv", "roa")

#Create dataframe
df = df[col]

###Task 3
#Create dataframe before and after pandemic
df_before = df[1:42,] 
df_after = df[43:51,]

#Descriptive statistics
summary(df_before)
summary(df_after)

#Explore Data Analysis
boxplot(df_before$lev, df_after$lev, main = "Boxplots of leverage",
                        xlab = "Before and after pandamic",
                        ylab = "The Values of leverage ratio")
boxplot(df_before$tang, df_after$tang, main = "Boxplots of tangibility",
        xlab = "Before and after pandamic",
        ylab = "The Values of tangibility")
boxplot(df_before$inv, df_after$inv, main = "Boxplots of invntories/current assets",
        xlab = "Before and after pandamic",
        ylab = "The Values of invntories/current assets")
boxplot(df_before$roa, df_after$roa, main = "Boxplots of profitibility",
        xlab = "Before and after pandamic",
        ylab = "The Values of ROA")
#Standard dviation 
stat.desc(df_before)[13,]
stat.desc(df_after)[13,]

###Task 4
#box & whisker plot and histogram of the leverage
ggplot(df, aes(x= lev)) +
  geom_boxplot(col="black",
               fill="gray") +
  ggtitle("Boxplot of leverage")

ggplot(df, aes(x= lev)) + 
  geom_histogram(bins =20,col="red",
                 fill="green",
                 binwidth = 0.05)+
  ggtitle("Histogram of leverage")


  
### Task 5 
##5.1
# Model multiple regression
model <-lm(lev ~ roa + tang + inv, data = df)
summary(model)
# Check Linearity 
plot(lev ~ roa, data= df,main =paste("Corr:",cor(df$lev, df$roa)))
plot(lev ~ tang, data= df,main =paste("Corr:",cor(df$lev, df$tang)))
plot(lev ~ inv, data= df,main =paste("Corr:",cor(df$lev, df$inv)))


# Check Multicollinearity
vif(lm(lev ~ roa + tang + inv, data = df))

# Check important assumptions 
par(mfrow=c(2,2))
plot(model)

shapiro.test(resid(model)) # Null hypothesis is normality
#Shapiro-Wilk normality test

library(lmtest) #Null hypothesis is homoskedasticity
bptest(model) 

##5.2 
# Create covid dummy variable (before covid: 0, after covid: 1) 
df["covid"] = 0
df[43:51,"covid"] = 1
# Model with the interaction between Covid-19 dummy variable and the independent variables
model_dummy <-lm(lev ~ roa + tang + inv + roa*covid +tang*covid +inv*covid, data = df)
summary(model_dummy)

# Check important assumptions 
par(mfrow=c(2,2))
plot(model_dummy)

# Null hypothesis is normality
#Shapiro-Wilk normality test
shapiro.test(resid(model_dummy))

#Null hypothesis is homoskedasticity
library(lmtest) 
bptest(model_dummy) 

##5.3 
#define new observation
new = df[,3:5]

#use the fitted model to predict the value for the new observation
pred = predict(model, newdata = new)
print(pred)
#RMSE 
library(Metrics)
rmse(df$lev, pred)


###6 
#import lib 
library(forecast) #forecast, accuracy
library(tseries) #adf.test
library(lmtest) #coeftest
library(stats) #Box.test
par(mfrow=c(1,1))

# Transform the variable into time series 
#(the beginning quarter starts from q3/2019 and the frequency is 4 quarters each year)
ts = ts(df$lev,start = c(2009,3),frequency = 4)
# Visualize time series data
autoplot(ts)

#Check stationary
adf.test(ts, k=4) #Since p is greater than significance level, the Series is NON Stationary

# Decompose time series and check it again
ts_d1 = diff(ts, differences = 1)
adf.test(ts_d1, k=4) # p-value is still greater than significance level, the Series is NON Stationary

#Second Difference
ts_d2 = diff(ts, differences = 2)
adf.test(ts_d2, k=4)
autoplot(ts_d2)
# ==> d =2 , the d in term ARIMA(p,2,q)

Pacf(ts_d2) # => p =4

Acf(ts_d2) # => q = 0


## Finally, I have appropriate p,d,q for the ARIMA model. That is: ARIMA(4,2,0)
# Model
mod = Arima(ts, order= c(4,2,0))
# Summary model
summary(mod)
# Check p-value 
coeftest(mod) # The model is completely statistically significant

# AutoCorrelation of Residuals test 
acf(mod$residuals)
pacf(mod$residuals)
Box.test(mod$residuals,lag=12,type='Ljung-Box')

# Forecast next 4 quarters in 2022 and 2023
forecast(mod, h =4)
# Visualization the forecast
autoplot(forecast(mod,h=4))





