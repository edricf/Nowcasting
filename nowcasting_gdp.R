# Load Libraries
library(cansim)
library(astsa)
library(tidyverse)
library(forecast)
library(xts)
library(lubridate)
library(pdfetch)

######################## Gather Data
X2raw = get_cansim_vector( c( 
  'monthly Canadian-Dollar Effective Exchange Rate Index' = 'v111666277',
  'monthly retail trade' = 'v52367097',
  'monthly government reserves'='v122396',
  "monthly GDP (basic prices)" = "v65201210" ,
  "quarterly GDP (market prices)" = "v62305752",
  'quarterly consumption 1' = 'v62305724',
  'quarterly consumption 2' = 'v62305730',
  'quarterly investment 1' ='v62305733',
  'quarterly investment 2' = 'v62305739',
  'quarterly investment 3' = 'v62305742',
  'quarterly government 1' = 'v62305731',
  'quarterly government 2' = 'v62305740',
  'quarterly government 3' = 'v62305741',
  'quarterly government 4' = 'v62305742',
  'quarterly exports'='v62305745',
  'quarterly imports'='v62305748'),
  start_time = "1900-01-01")  %>% 
  normalize_cansim_values()


### Quartely data ###
filter_cansim <- function(vec_code) {
  filtered_data = X2raw %>%  
    filter( VECTOR == vec_code ) %>% 
    # find year & quarter
    mutate( Y = year( Date ), Q = quarter( Date ),
            index = yearqtr( Y + Q/4 ) ) %>%  
    xts( x=.$VALUE, order.by =.$index) 
  return(filtered_data)
} 
# Quarterly GDP
Q_total <- filter_cansim('v62305752')

# Quarterly Consumptions
Q_consump_1 <- filter_cansim('v62305724')
Q_consump_2 <- filter_cansim('v62305730')
total_consumption <- Q_consump_1 + Q_consump_2

# Quarterly Investments
Q_invest_1 <- filter_cansim('v62305733')
Q_invest_2 <- filter_cansim('v62305739')
Q_invest_3 <- filter_cansim('v62305742')
total_investment <- Q_invest_1 + Q_invest_2 + Q_invest_3

# Quarterly Government Expenditures
Q_gov_1 <- filter_cansim('v62305731')
Q_gov_2 <- filter_cansim('v62305740')
Q_gov_3 <- filter_cansim('v62305741')
Q_gov_4 <- filter_cansim('v62305742')
total_gov <- Q_gov_1 + Q_gov_2 + Q_gov_3 - Q_gov_4

# Quarterly Net Exports
Q_exports <- filter_cansim('v62305745')
Q_imports <- filter_cansim('v62305748')
Q_net_exp <- Q_exports- Q_imports

### Monthly data

## Monthly GDP
M_gdp = X2raw %>%  
  filter( VECTOR == "v65201210" ) %>% 
  # find year, quarter, month, and month-in-quarter
  mutate( Y = year( Date ), Q = quarter( Date ), 
          index = yearqtr( Y + Q/4 ), 
          M = month( Date ),
          MinQ = paste( "M", M%%3, sep="" ) ) %>%  
  # spread monthly data into 3 columns one for each month-in-quarter
  pivot_wider(id_cols = index, names_from = MinQ, 
              values_from = VALUE ) %>% 
  # take lag for M0
  mutate( M0 = lag(M0) ) %>% 
  xts( x=.[,c("M0","M1","M2")], order.by =.$index) 

## Monthly TSX closing prices
tsx_data <- pdfetch_YAHOO('^GSPTSE', fields='close', from=as.Date('1997-01-01'), to=as.Date('2019-08-31'), interval='1mo')
tsx_data <- data.frame(date=index(tsx_data), coredata(tsx_data))
names(tsx_data)= c('date', 'VALUE')
tsx_data <- tsx_data %>% 
  # find year, quarter, month, and month-in-quarter
  mutate( Y = year( date ), Q = quarter( date ), 
          index = yearqtr( Y + Q/4 ), 
          M = month( date ),
          MinQ = paste( "TSX", M%%3, sep="" ) ) %>%  
  # spread monthly data into 3 columns one for each month-in-quarter
  pivot_wider(id_cols = index, names_from = MinQ, 
              values_from = VALUE ) %>% 
  # take lag for TSX0
  mutate(TSX0 = lag(TSX0) ) %>% 
  xts( x=.[,c("TSX0","TSX1","TSX2")], order.by =.$index)  


## Canadian-Dollar Effective Exchange Rate Index (CERI)/ CEER
M_fx = X2raw %>%  
  filter( VECTOR == "v111666277" ) %>% 
  # find year, quarter, month, and month-in-quarter
  mutate( Y = year( Date ), Q = quarter( Date ), 
          index = yearqtr( Y + Q/4 ), 
          M = month( Date ),
          MinQ = paste( "fx", M%%3, sep="" ) ) %>%  
  # spread monthly data into 3 columns one for each month-in-quarter
  pivot_wider(id_cols = index, names_from = MinQ, 
              values_from = VALUE ) %>% 
  # take lag for M0
  mutate( fx0 = lag(fx0) ) %>% 
  xts( x=.[,c("fx0","fx1","fx2")], order.by =.$index)
# Impute CEER
M_fx[149:nrow(M_fx)-1,]<-c(98.20373,120.11,117.93, 114.71,116.59,117.13,115.82,115.99,117.32,117.47,118.13,117.15,114.95,115.02,115.88,114.59,114.59,114.49,115.8,117.36,116.94)
## Replaced by CEER BY 2018-01-01 
# Source https://www.bankofcanada.ca/rates/exchange/canadian-effective-exchange-rates/

### Monthly retail trade sales
M_trade = X2raw %>%  
  filter( VECTOR == "v52367097" ) %>% 
  # find year, quarter, month, and month-in-quarter
  mutate( Y = year( Date ), Q = quarter( Date ), 
          index = yearqtr( Y + Q/4 ), 
          M = month( Date ),
          MinQ = paste( "T", M%%3, sep="" ) ) %>%  
  # spread monthly data into 3 columns one for each month-in-quarter
  pivot_wider(id_cols = index, names_from = MinQ, 
              values_from = VALUE ) %>% 
  # take lag for M0
  mutate( T0 = lag(T0) ) %>% 
  xts( x=.[,c("T0","T1","T2")], order.by =.$index) 


### International Reserves
M_reserves = X2raw %>%  
  filter( VECTOR == "v122396" ) %>% 
  # find year, quarter, month, and month-in-quarter
  mutate( Y = year( Date ), Q = quarter( Date ), 
          index = yearqtr( Y + Q/4 ), 
          M = month( Date ),
          MinQ = paste( "D", M%%3, sep="" ) ) %>%  
  # spread monthly data into 3 columns one for each month-in-quarter
  pivot_wider(id_cols = index, names_from = MinQ, 
              values_from = VALUE ) %>% 
  # take lag for M0
  mutate( D0 = lag(D0) ) %>% 
  xts( x=.[,c("D0","D1","D2")], order.by =.$index)

### Merge data ###
merged_data  <- merge(Q_total, total_consumption, total_investment, total_gov, Q_net_exp)
merged_data <- merge(merged_data, M_gdp, join ='inner') ## Universal Var
merged_data <- merge(merged_data, M_trade, join='inner') ## Use for Consumption
merged_data <- merge(merged_data, tsx_data, join='inner') ## Use for Investment
merged_data <- merge(merged_data, M_fx, join='inner') ## Use for Net Export
merged_data <- merge(merged_data, M_reserves, join='inner') ## Use for Gov Exp
# Convert our data in Millions for Numerical Stability
merged_data<- merged_data/10^9
merged_df <- data.frame(date=index(merged_data), coredata(merged_data)) # for plotting/EDA
head(merged_data)

## Create function for plotting linear trend
plot_trend <- function(x,y, xlab,ylab,title) {
  plot(y=y, x=x, pch=20,
       xlab=xlab, ylab=ylab, main=title)
  abline(lm(y~x), col='red')
}

par(mfrow=c(1,1))
plot_trend(merged_df$T0, merged_df$total_consumption, 'Monthly Retail Trade (Start of Quarter)',
           'Quarterly Consumption', title='Quarterly Consumption VS. Retail Trade')

plot_trend(merged_df$TSX0, merged_df$total_investment, 'TSX Index (Start of Quarter)',
           'Quarterly  Investments', title='Quarterly Investments VS. TSX')

plot_trend(merged_df$fx0, merged_df$Q_net_exp, 'CAD Effective Exchange Rate Index, monthly average (Start of Quarter)',
           'Quarterly Net Exports', title='Quarterly Net Exports VS. CERI')

plot_trend(merged_df$D0, merged_df$total_gov, 'Monthly International Reserves (start of Quarter)',
           'Quarterly Gov Expenditures', title='Quarterly Gov Expenditures VS. International Reserves')


#################################### Model Fitting #####################################
### Fit Model ###
## Create function for model diagnostics
diagnostic <- function(stdres, res, title) {
  par(mfrow=c(2,2))
  plot(stdres, main=title) # residual plot
  qqnorm(stdres) # normal QQ-plot
  acf(stdres,na.action=na.pass) # residual ACF
  Box.test(stdres, lag=20) # Ljung-Box test (H=20) 
  
  lags=3:20; p.values=rep(0,length(lags))
  for(i in 1:length(lags)){
    p.values[i]=Box.test(stdres, lags[i], type = "Ljung-Box")$p.value
  }
  plot(lags, p.values, ylim=c(0,1), main="Ljung-Box p-values"); abline(h=.05, lty=2)
} 
# Baseline Total GDP Model
par(mfrow=c(2,2))
acf(diff(log(merged_data[,'Q_total'])), na.action = na.pass, main='Differenced Quarterly Log GDP ACF')
pacf(diff(log(merged_data[,'Q_total'])), na.action = na.pass, main='Differenced Quarterly Log GDP PACF')
plot(log(merged_data[,'Q_total']), main='Log Quarterly GDP')
plot(diff(log(merged_data[,'Q_total'])), main='Differenced Log Quarterly GDP')
auto.arima(log(merged_data[,'Q_total']),d=1, xreg=log(merged_data[,c('M0', 'M1', 'M2')]))
tot_gdp.fit <- Arima(log(merged_data[,'Q_total']), order=c(1,1,1), xreg=log(merged_data[,c('M0', 'M1', 'M2')]), include.drift=TRUE)

sarima(log(merged_data[,'Q_total']),1,1,1, xreg=merged_data[,c('M0', 'M1', 'M2')],details=F)

Arima(log(merged_data[,'Q_total']), order=c(1,1,1), include.drift=TRUE)$aic
res = tot_gdp.fit$residuals ## Model residuals
stdres = tot_gdp.fit$residuals / sqrt(tot_gdp.fit$sigma2)
stdres = ts(data.frame(stdres),freq=4,start = 1997)
diagnostic(stdres,res,'Plot of Standardized Residuals for Total GDP')
# Arima(1,1,1) with external regessors

# Consumption Model
par(mfrow=c(2,2))
acf(diff(log(merged_data[,'total_consumption'])), na.action = na.pass, main='Differenced Quaterly Log Consumption ACF')
pacf(diff(log(merged_data[,'total_consumption'])), na.action = na.pass, main='Differenced Quarterly Log Consumption PACF')
plot(log(merged_data[,'total_consumption']), main= 'Quarterly Log Consumption')
plot(diff(log(merged_data[,'total_consumption'])), main='Differenced Quart Log Consum')
auto.arima(log(merged_data[,'total_consumption']), d=1,xreg=log(merged_data[,c('M0', 'M1', 'M2','T0','T1','T2')]))
consump.fit <-Arima(log(merged_data[,'total_consumption']), order=c(0,1,0), xreg=log(merged_data[,c('M0', 'M1', 'M2','T0','T1','T2')]), include.drift=TRUE)
consump.fit  # Arima 0,1,0
Arima(log(merged_data[,'total_consumption']), order=c(0,1,0),include.drift=TRUE)$aic
res = consump.fit$residuals
stdres = consump.fit$residuals / sqrt(consump.fit$sigma2)
stdres = ts(data.frame(stdres),freq=4,start = 1997)
diagnostic(stdres,res,'Plot of Standardized Residuals for Consumption')
# Use Arima(0,1,0) with external regressors


# Investment Model
par(mfrow=c(2,2))
acf(diff(log(merged_data[,'total_investment'])), main='Differenced Quarterly Log Investments ACF', na.action = na.pass)
pacf(diff(merged_data[,'total_investment']), main='Differenced Quarterly Log Investments PACF', na.action = na.pass)
plot((log(merged_data[,'total_investment'])), main='Log Quarterly Investments')
plot(diff(log(merged_data[,'total_investment'])), main='Differenced Log Investments')
auto.arima(log(merged_data[,'total_investment']), d=1,xreg=log(merged_data[,c('M0', 'M1', 'M2', 'TSX0','TSX1','TSX2')]))
invest.fit <- Arima(log(merged_data[,'total_investment']), order=c(0,1,0), xreg=log(merged_data[,c('M0', 'M1', 'M2', 'TSX0', 'TSX1', 'TSX2')]), include.drift=TRUE)
invest.fit
Arima(log(merged_data[,'total_investment']), order=c(0,1,0), include.drift=TRUE)$aic
res = invest.fit$residuals
stdres = invest.fit$residuals / sqrt(invest.fit$sigma2)
stdres = ts(data.frame(stdres),freq=4,start = 1997)
diagnostic(stdres,res,'Plot of Standardized Residuals for Investments Model')
# Use Arima(0,1,0) with external regressors

# Government Model
par(mfrow=c(2,2))
acf(diff(log(merged_data[,'total_gov'])), na.action = na.pass, main='Differenced Log Government EXP ACF')
pacf(diff(log(merged_data[,'total_gov'])), na.action = na.pass, main='Differenced Log Government EXP PACF')
plot(log(merged_data[,'total_gov']), main='Quarterly Government EXP')
plot(diff(log(merged_data[,'total_gov'])), main='Differenced Government EXP')
auto.arima(log(merged_data[,'total_gov']),d=1,xreg=log(merged_data[,c('M0','M1','M2','D0','D1','D2')]))
gov.fit <- Arima(log(merged_data[,'total_gov']), order=c(2,1,0), xreg=log(merged_data[,c('M0', 'M1', 'M2')]), include.drift = T)
gov.fit
Arima(log(merged_data[,'total_gov']), order=c(2,1,0), include.drift = T)$aic
res = gov.fit$residuals
stdres = gov.fit$residuals / sqrt(gov.fit$sigma2)
stdres = ts(data.frame(stdres),freq=4,start = 1997)
diagnostic(stdres,res,'Plot of Standardized Residuals for Government EXP')
# Use Arima(2,1,0) with external regressors

# Net Exports Model
par(mfrow=c(2,2))
acf(diff(scale(merged_data[,'Q_net_exp'])), na.action = na.pass, main='Differenced Scaled Net Exports ACF')
pacf(diff(scale(merged_data[,'Q_net_exp'])), na.action = na.pass, main='Differenced Scaled Net Exports PACF')
plot((merged_data[,'Q_net_exp']), main='Scaled Net Exports')
plot(diff(merged_data[,'Q_net_exp']), main='Differenced Scaled Net Exports')
auto.arima(scale(merged_data[,'Q_net_exp']), d=1, xreg=scale(merged_data[,c('M0','M1','M2')]))
exports.fit <- Arima(scale(merged_data[,'Q_net_exp']),order=c(0,1,0),xreg=scale(merged_data[,c('M0', 'M1', 'M2','fx0','fx1','fx2')]))
exports.fit
Arima(scale(merged_data[,'Q_net_exp']),order=c(0,1,0))$aic
res = exports.fit$residuals
stdres = exports.fit$residuals / sqrt(exports.fit$sigma2)
stdres = ts(data.frame(stdres),freq=4,start = 1997)
diagnostic(stdres,res,'Plot of Standardized Residuals for Net Exports Model')

######################### Cross Validation using MAPE and MSE ##########
# We Validate our data using its last 20 Quarters
c_val <- function(x,y,ord,num_pred,lg) {
  y_fe=rep(NA,90)
  for(i in (91 - 20:1) ){
    yt = y[1:i]; xt = x[1:i,num_pred]  # subset of y,x
    
    # fit regression with AR(1) residuals
    out = Arima( yt, order = ord, xreg = xt, include.drift=T) 
    
    # 1-step-ahead forecast erros using predict() function
    y_fe[i+1] = forecast( out, xreg = x[i+1,num_pred] )$mean
  }
  # Cross-Validated Performance 
  if(lg==TRUE) {
    y_pred=exp(y_fe)
    MSPE = mean( (exp(y) - exp(y_fe))^2, na.rm = TRUE )
    MAPE = mean( abs(exp(y) - exp(y_fe))/abs(exp(y)), na.rm = TRUE )
  }
  else {
    y_pred=y_fe
    MSPE = mean( (y - y_pred)^2, na.rm = TRUE )
    MAPE = mean( abs(y - y_pred)/abs(y), na.rm = TRUE )
  }
  return(data.frame(MSPE, MAPE, y_pred))  
}

### Total GDP ARIMA (1,1,1) CROSS VALIDATION
head(merged_data)
y= log(merged_data$Q_total)
x= log(merged_data[,c('M0','M1','M2')])
c_val(x,y, c(1,1,1),c('M0'),lg=TRUE) # Cross vals scores for start of quarter
nowcasts_tot <- c_val(x,y, c(1,1,1),c('M0','M1'),lg=TRUE) # cross val scores after 1st quarter
nowcasts_tot
c_val(x,y, c(1,1,1),c('M0', 'M1', 'M2'),lg=TRUE) # cross val scores after 2nd quarter

### Consumption Model ARIMA (0,1,0) CROSS VALIDATION with monthly retail
y =log(merged_data$total_consumption)
x= log(merged_data[,c('M0','M1','M2','T0','T1','T2')])
c_val(x,y, c(0,1,0),c('M0','T0'),lg=TRUE)
nowcasts_consump <-c_val(x,y, c(0,1,0),c('M0','M1', 'T0','T1'),lg=TRUE)
nowcasts_consump
c_val(x,y, c(0,1,0),c('M0', 'M1', 'M2', 'T0','T1','T2'),lg=TRUE)

### Investment Model ARIMA (0,1,0) CROSS VALIDATION with monthly retail
y =log(merged_data$total_investment)
x= log(merged_data[,c('M0','M1','M2','TSX0','TSX1','TSX2')])
c_val(x,y, c(0,1,0),c('M0','TSX0'),lg=TRUE)
nowcasts_inv <- c_val(x,y, c(0,1,0),c('M0','M1', 'TSX0','TSX1'),lg=TRUE)
nowcasts_inv
c_val(x,y, c(0,1,0),c('M0', 'M1', 'M2', 'TSX0','TSX1','TSX2'),lg=TRUE)


### Government Model ARIMA (2,1,0) CROSS VALIDATION with monthly retail
y =log(merged_data$total_gov)
x= log(merged_data[,c('M0','M1','M2','D0','D1','D2')])
c_val(x,y, c(2,1,0),c('M0','D0'),lg=TRUE)
nowcasts_gov <- c_val(x,y, c(2,1,0),c('M0','M1', 'D0','D1'),lg=TRUE)
nowcasts_gov
c_val(x,y, c(2,1,0),c('M0', 'M1', 'M2', 'D0','D1','D2'),lg=TRUE)

### Net Exports ARIMA (0,1,0) CROSS VALIDATION with monthly retail
y =scale(merged_data$Q_net_exp)
x= scale(merged_data[,c('M0','M1','M2','fx0','fx1','fx2')])
c_val(x,y, c(0,1,0),c('M0','fx0'),lg=FALSE)
nowcasts_exp <- c_val(x,y, c(0,1,0),c('M0','M1','M2'),lg=FALSE)
nowcasts_exp
c_val(x,y, c(0,1,0),c('M0','M1','M2','fx0','fx1','fx2'),lg=FALSE)


##################### create function to plot 1st month Nowcast
plot_nowcasts <- function(component, ylab, nowcasts, legend_pos, scale=FALSE) {
  if(scale==TRUE){
  merged_df[,component] = scale(merged_df[,component])
  }
  merged_df[,component][72:91]
  nowcasts$date <-merged_df$date
  nowcasts <- nowcasts[,c('date','y_pred')][72:91,]
  plot(nowcasts,type='b', pch=20, col='red', xlab='Date', ylab=ylab, main=paste('1st Month Nowcasts and Actual Values (', ylab,')',sep=''))
  lines(merged_df[,c('date',component)], pch=20, col='blue',type='b')
  legend(legend_pos,1,95,legend=c('1st month nowcasts','actual values'), col=c('red','blue'), lty=1:2,cex=0.8)
}

head(merged_df)
par(mfrow=c(2,2))
plot_nowcasts('total_consumption','Consumption', nowcasts_consump, 'topleft')
plot_nowcasts('total_gov', 'Government', nowcasts_gov, 'bottomright')
plot_nowcasts('total_investment', 'Investments', nowcasts_inv, 'topright')
plot_nowcasts('Q_net_exp', 'Net Exports', nowcasts_exp, 'topright', scale=TRUE)
plot_nowcasts('Q_total','Total GDP',nowcasts_tot, 'topleft')

## Calculate how large each component is
merged_df$total_consumption[91]
merged_df$total_gov[91]
merged_df$total_investment[91]
merged_df$Q_net_exp[91]

