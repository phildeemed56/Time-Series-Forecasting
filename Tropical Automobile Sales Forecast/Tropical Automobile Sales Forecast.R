# Author:  Philip Mensah


# Start file

cat("\014")
setwd('')
rm(list=ls())
set.seed(773)

############################################################
# INSTALL/LOAD PACKAGES ####################################
############################################################

# Load fpp3 (automatically installs if needed)
# This "grand" package contains most packages and datasets needed to run the code
if (!require("fpp3")) install.packages("fpp3")

# Load gridExtra (automatically installs if needed)
# This facilitates the creation of side-by-side plots
if (!require("gridExtra")) install.packages("gridExtra")

# Load tseries (automatically installs if needed)
# This contains the Jarque-Bera test function
if (!require("tseries")) install.packages("tseries")

# Load car (automatically installs if needed)
# This contains the Durbin-Watson test function
if (!require("car")) install.packages("car")

# Load hablar (automatically installs if needed)
# This contains the convert function
if (!require("hablar")) install.packages("hablar")

(.packages())


############################################################


# Load CSV file: US employment in retail trade only, monthly, not seasonally adjusted
data <- readr::read_csv("PROJECT_2.csv")
head(data)

# Recognize the data as time series
data <- data %>%
  mutate(QUARTER = yearquarter(date)) %>%
  select(-date) %>%
  as_tsibble(index = QUARTER) %>%
  mutate(y = logsales)
head(data)


# Plotting the series
plotdata <- data %>%
  autoplot(y) +
  labs(title="Log of Sales", x="Quarter") +
  geom_line(size=0.7) + geom_point(size=1.0)

plotdata




# Classical decomposition 
decomposition <- data %>%
  model(classical_decomposition(`y`, type="additive"))
components(decomposition) %>%
  autoplot() +
  labs(y = "Log of Sales",
       title = "Classical decomposition of the Log of Sales")

# Seasons plots 
data %>%
  gg_subseries(`y`) +
  labs(y = "Log of sales",
       title = "Seasons plot: Log of Sales")

data %>%
  gg_season(`y`) +
  labs(y = "Log of Sales",
       title = "Seasons plot: Log of Sales")




# Residualization to detrend and to seasonally demean 
fit.dtds1 <- data %>%
  model(TSLM(`y` ~ 1+ trend()+season()))
report(fit.dtds1)

plot.fit1 <- augment(fit.dtds1) %>% 
  ggplot(aes(x=QUARTER)) +
  geom_line(aes(y = `y`, colour = "Data"), size=1) +
  geom_line(aes(y = .fitted, colour = "Fitted"), size=1, alpha=0.75) +
  labs(title="Log of Sales") +
  scale_color_manual(values=c(Data="black",Fitted="darkcyan")) +
  guides(colour = guide_legend(title = NULL)) +
  theme(legend.position = c(0.1, 0.9))

plot.resid1 <- augment(fit.dtds1) %>% 
  ggplot(aes(x=QUARTER)) +
  geom_line(aes(y = .resid, colour = "Detrended & Seasonally Adj"), size=1) +
  geom_hline(yintercept=0,lty='dashed') +
  labs(title="Residualized Log Sales",
       y="Residual") +
  scale_color_manual(values=c(`Detrended & Seasonally Adj`="darkred")) +
  theme(legend.position = "none")

grid.arrange(plot.fit1, plot.resid1, ncol=1)






# Inspect if residualized series is stationary
augment(fit.dtds1) %>% 
  gg_season(.resid) +
  labs(y = "Log of Sales Made",
       title = "Seasons plot: Log Sales")

augment(fit.dtds1) %>% 
  gg_lag(.resid, geom = "point") +
  labs(y="Residual", x="",
       title="Lags plot: Residualized Log Sales") 


plot.resid.diff1 <- augment(fit.dtds1) %>% 
  mutate(`Quarterly Growth in Residuals` = difference(.resid,1)) %>%
  ggplot(aes(x=QUARTER)) +
  geom_line(aes(y = `Quarterly Growth in Residuals`, colour = "d1"), size=1) +
  geom_hline(yintercept=0,lty='dashed') +
  labs(title="Quarterly Growth of Residualized Log Sales",
       y="diff1(Residual)") +
  scale_color_manual(values=c(`d1`="black")) +
  theme(legend.position = "none")

plot.resid.diff12 <- augment(fit.dtds1) %>% 
  mutate(`Annual Growth in Residuals` = difference(.resid,12)) %>%
  ggplot(aes(x=QUARTER)) +
  geom_line(aes(y = `Annual Growth in Residuals`, colour = "d12"), size=1) +
  geom_hline(yintercept=0,lty='dashed') +
  labs(title="Annual Growth of Residualized Log Sales",
       y="diff12(Residual)") +
  scale_color_manual(values=c(`d12`="navyblue")) +
  theme(legend.position = "none")

grid.arrange(plot.resid1, plot.resid.diff1, plot.resid.diff12, ncol=1)


# Using HEGY to Test if residualized series is stationary
uroot::hegy.test(ts(augment(fit.dtds1)$.resid, frequency=4), deterministic = c(1,0,0), lag.method="AIC", maxlag=36) 



# Using ADF to Test if residualized series is stationary
urca::summary(urca::ur.df(augment(fit.dtds1)$.resid, type = c("none"), selectlags="AIC")) 



# Plot correlogram for residualized series
augment(fit.dtds1) %>% 
  gg_tsdisplay(.resid, plot_type='partial', lag=36) +
  labs(title="Correlogram of Residualized Log Sales", y="Residual")




############################################################

# Inspect first-differenced residualized series
data1 <- augment(fit.dtds1) %>% 
  mutate(`Quarterly Growth in Residuals` = difference(.resid,1))

data1 %>%
  gg_tsdisplay(`Quarterly Growth in Residuals`, plot_type='partial', lag=36) +
  labs(title="Quarterly Growth of First Differenced Residualized Log Sales", y=" ")


# Using ADF to Test if first differenced residualized series is stationary
urca::summary(urca::ur.df(data1$`Quarterly Growth in Residuals`[!is.na(data1$`Quarterly Growth in Residuals`)], 
                          type = c("none"), selectlags="AIC")) 


# Using HEGY to Test if first differenced residualized series is stationary
uroot::hegy.test(ts(data1$`Quarterly Growth in Residuals`[!is.na(data1$`Quarterly Growth in Residuals`)], frequency = 4), deterministic = c(1,0,0), lag.method="AIC", maxlag=36)




##################################
# Modeling Quarterly Growth of Log of Sales

data <- data %>%
  mutate(`Quarterly Growth in Log of Sales` = difference(`y`,1))

data

arma.7 <- data %>%
  model(ARIMA(`Quarterly Growth in Log of Sales` ~ 0 + trend() + season() + pdq(3,1,1) + PDQ(1,0,1), 
              ic="aicc", stepwise=FALSE, approximation=FALSE))
report(arma.7)


# Time plots of transformed data
plotdata3 <- data %>%
  ggplot(aes(y=`y`, x=QUARTER)) +
  labs(title="Log of Sales",
       y="Log Sales", x="QUARTER") +
  geom_line(color='dodgerblue4', size=1)

plotdata4 <- data %>%
  ggplot(aes(y=`Quarterly Growth in Log of Sales`, x=QUARTER)) +
  labs(title="Quarterly Growth Rate in Log Sales",
       y="Quarterkt Growth Rate in Log Sales", x="QUARTER") +
  geom_line(size=1, color="darkred") 

grid.arrange(plotdata3, plotdata4, ncol=1)



# Diagnostics
residuals(arma.7) %>% 
  gg_tsdisplay(.resid, plot_type='partial', lag=36) +
  labs(title="Quarterly Growth of Log Sales", y="Residuals")

residuals(arma.7) %>% features(.resid, box_pierce, lag = 36, dof = nrow(coef(arma.7))) %>% select(bp_stat,bp_pvalue)
residuals(arma.7) %>% features(.resid, ljung_box, lag = 36, dof = nrow(coef(arma.7))) %>% select(lb_stat,lb_pvalue)




# Unit root tests
uroot::hegy.test(ts(data$`y`, frequency=4), 
                 deterministic = c(1,0,0), lag.method="AIC", maxlag=36) 

uroot::hegy.test(ts(data$`Quarterly Growth in Log of Sales`, frequency=4),
                 deterministic = c(1,0,0), lag.method="AIC", maxlag=36) 


urca::summary(urca::ur.df(data$`Quarterly Growth in Log of Sales`[!is.na(data$`Quarterly Growth in Log of Sales`)], 
                          type = c("none"), selectlags="AIC")) 








############################################################
# Forecasting Performance: h=1 #############################
############################################################


n <- ceiling(nrow(data)*0.9);   n    # size of the training sample
data$QUARTER[n]                        # last training observation
data$QUARTER[n+1]                      # first holdout observation

# Hold-out sample
data.holdout1 <- data %>%
  filter_index(paste(data$QUARTER[n+1]) ~ .)
p <- nrow(data.holdout1);   p         # size of the hold-out sample



############################################################

# Reestimate on the training sample and
train.arma1 <- data %>% 
  filter_index(. ~ paste(data$QUARTER[n])) %>%
  model("arma.1"  = ARIMA(`Quarterly Growth in Log of Sales` ~ 0 + pdq(3,1,1) + PDQ(1,0,1) + trend() + season()))
        
# Produce the first 1-step-ahead forecast
forecast.y1 <- train.arma1 %>% forecast(h=1) 



# Recursively add a data point one at a time and re-estimate the models
for (q in as.character(data.holdout1$QUARTER[-p])) {
  
  append.train.arma1 <- data %>%
    filter_index( . ~ paste(q)) %>%
    model("arma.1"  = ARIMA(`Quarterly Growth in Log of Sales` ~ 0 + pdq(3,1,1) + PDQ(1,0,1) + trend() + season()))
          
  append.forecast.y1 <- append.train.arma1 %>% forecast(h=1)
  
  forecast.y1 <- bind_rows(forecast.y1, append.forecast.y1) 
  
}


# Evaluate forecast accuracy in the hold-out sample
forecast.y1 %>%
  accuracy(filter_index(data, paste(data$QUARTER[n+1]) ~ .))

# Plot forecasts
forecast.y1 %>%
  autoplot(filter_index(data, paste(data$QUARTER[n-4]) ~ paste(data$QUARTER[n])), level=c(99), size=1) +
  autolayer(filter_index(data, paste(data$QUARTER[n+1]) ~ .), 
            .vars=`Quarterly Growth in Log of Sales`, color = "black", linetype="longdash", size=1, alpha=0.5) +
  labs(title = "1-step-ahead Forecasts for Log Sales" ) +
  theme(legend.position = c(0.1,0.8))

