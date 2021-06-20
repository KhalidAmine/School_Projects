###############################################################################
# TAKE HOME EXAM
#
# Student + SNR: 
#    - Khalid Amine  (2045967)
#
# Data Science Methods in Finance (323080-M-6)
# Tilburg University
###############################################################################

#### setting up working environment ####
dev.off(dev.list()["RStudioGD"])
rm(list=ls()) ## libs
cat("\014")

filepath <- rstudioapi::getSourceEditorContext()$path
dirpath  <- dirname(rstudioapi::getSourceEditorContext()$path)

setwd(dirpath)

# Package names 
packages <- c("ISLR", "glmnet", "ggplot2", "readxl", "forecast", "MASS", "dplyr", 
              "tibble", "zoo", "jtools", "sjmisc", "matrixStats", "PerformanceAnalytics", 
              "reshape2", "groupdata2", "tidyr", "caret", "ggfortify","Metrics", 
              "gbm", "PortfolioAnalytics", "devtools", "corrr", "rattle", "rpart",
              "skimr", "tensorflow", "keras", "doBy", "chron", "xtable", "caTools")

# Install packages not yet installed 
installed_packages <- packages %in% rownames(installed.packages()) 

if (any(installed_packages == FALSE)) {   
  install.packages(packages[!installed_packages]) 
} 

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#----------------------------------------------------------------------------#
###### some functions
# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

# constructformula for RMSE 
RMSE <- function(predicted, actual){
  sqrt(mean((predicted-actual)^2))
}

#----------------------------------------------------------------------------#

# load in data
df <- readRDS("2021_Electricity_data.RDS")


##### Question 1: Report summary graphs for the data
### Part 1.1
# making a separate date variable, without hour 
df$date_exclhour <- substring(df$date, 1,10 )
df$date_exclhour <- as.Date(df$date_exclhour)

# make seperate hour variable, without the date itself
df$hour <- substring(df$date, 12,19)
df$hour <- chron(times=df$hour)   # converting it into a time variable

# make a weekend identifier
df$weekdays <- weekdays(df$date)
df$weekend <- ifelse(df$weekdays== "Saturday" | df$weekdays== "Sunday", 1,0)

# arrange df again 
df <- df %>%
  select(date_exclhour, hour, date, weekend, dutch_power, german_power, belgium_power, norway_power
         , dutch_load, dutch_generation_forecast, solar, wind_off_shore, wind_on_shore)

# calculate average hourly prices over a day
q1.1_1 <- df %>% 
  group_by(hour) %>%
    mutate(avg_phour_NL= mean(dutch_power), avg_phour_DE= mean(german_power), 
           avg_phour_BE= mean(belgium_power), avg_phour_NW= mean(norway_power)) %>%
      select(hour,avg_phour_NL, avg_phour_DE, avg_phour_BE, avg_phour_NW)
  
q1.1_1 <- q1.1_1[!duplicated(q1.1_1),]

# !! neglect the date variable in the table, this is made in order to be able to plot ONLY time in ggplot !!
q1.1_1_plot <- q1.1_1
q1.1_1_plot %>% mutate(hour = as.character(hour))
q1.1_1_plot$hour <- as.POSIXct(paste0("2016-09-23", q1.1_1_plot$hour), tz = "GMT")

# Plot average hourly prices over a day of all countries
ggplot(q1.1_1_plot, aes(x = hour)) +
  geom_line(aes(y = avg_phour_NL, color = "Netherlands"), size = 0.5) +
  geom_line(aes(y = avg_phour_DE, color = "Germany"), size = 0.5) +
  geom_line(aes(y = avg_phour_BE, color = "Belgium"), size = 0.5) +
  geom_line(aes(y = avg_phour_NW, color = "Norway"), size = 0.5) +
  labs(x = "Hour", y = "Average price", color = "Legend", subtitle = "2018 | 2019 | 2020") + 
  ggtitle("Average hourly prices over a day - across countries") +
  scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "4 hour")

# export table for question 1 part 1 in latex
print(xtable(q1.1_1, type = "latex"), file = "Q1.1_1.tex")

# correlation matrix ~ countries 
cor_matrix <- cor(q1.1_1[,-1])
print(xtable(cor_matrix, type = "latex"), file = "cor.tex")

## calculate average hourly prices over a day dutch market with and without weekdays 
# average full week prices every hour
q1.1_2 <- df %>%
  group_by(hour) %>%
    mutate(avg_phour_fullweek= mean(dutch_power)) %>%
      select(hour,avg_phour_fullweek)
q1.1_2 <- q1.1_2[!duplicated(q1.1_2), ]  
  
# average weekend prices every hour
mergewithq1.1_2 <- df %>%
  group_by(hour, weekend) %>%
    mutate(avg_phour_onlyweekend= mean(dutch_power)) %>%
      select(hour,weekend, avg_phour_onlyweekend)
mergewithq1.1_2 <- mergewithq1.1_2[!duplicated(mergewithq1.1_2), ]    
mergewithq1.1_2 <- mergewithq1.1_2 %>%
  filter(weekend==1) 

# merge them together and plot graph
q1.1_2 <- merge(q1.1_2, mergewithq1.1_2, by="hour")
q1.1_2 <- q1.1_2 %>% select(hour, avg_phour_fullweek, avg_phour_onlyweekend )

# !! neglect the date variable in the table, this is made in order to be able to plot ONLY time in ggplot !!
q1.1_2_plot <- q1.1_2
q1.1_2_plot$hour <- as.POSIXct(paste0("2016-09-23", q1.1_2_plot$hour), tz = "GMT")
 
ggplot(q1.1_2_plot, aes(x = hour)) +
  geom_line(aes(y = avg_phour_fullweek, color = "Full week"), size = 0.7) +
  geom_line(aes(y = avg_phour_onlyweekend, color = "Weekend"), size = 0.7) +
  labs(x = "Hour", y = "Average price", color = "Legend", subtitle = "Netherlands (2018 | 2019 | 2020)") + 
  ggtitle("Average hourly prices over a day - full week against weekend") +
  scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "4 hour")

# export table for question 1 part 2 in latex
print(xtable(q1.1_2, type = "latex"), file = "Q1.1_2.tex")


### Part 1.2
# compute volatility of hourly prices over a day taking into account full week in Netherlands
q1.2 <- df %>%
  group_by(hour) %>%
    mutate(avg_volatility_NL= sd(dutch_power)) %>%
      select(hour,avg_volatility_NL)
q1.2 <- q1.2[!duplicated(q1.2), ]  

# compute volatility of hourly prices over a day taking only weekends into account in Netherlands
mergewithq1.2 <- df %>%
  group_by(hour, weekend) %>%
    mutate(avg_vol_onlyweekend= sd(dutch_power)) %>%
      select(hour,weekend, avg_vol_onlyweekend)
mergewithq1.2 <- mergewithq1.2[!duplicated(mergewithq1.2), ]    
mergewithq1.2 <- mergewithq1.2 %>%
  filter(weekend==1) 

# merge them together and plot graph
q1.2 <- merge(q1.2, mergewithq1.2, by="hour")
q1.2 <- q1.2 %>% select(hour, avg_volatility_NL, avg_vol_onlyweekend)

# need to put volatility in percentage
hour <- q1.2$hour
q1.2_inperc <- q1.2[,2:3]/nrow(q1.2) *100
q1.2_inperc <- cbind(hour, q1.2_inperc)

# !! neglect the date variable in the table, this is made in order to be able to plot ONLY time in ggplot !!
q1.2_plot <- q1.2_inperc
q1.2_plot$hour <- as.POSIXct(paste0("2016-09-23", q1.2$hour ), tz = "GMT")

# plot volatility of hourly prices over a day in netherlands
ggplot(q1.2_plot, aes(x = hour)) +
  geom_line(aes(y = avg_volatility_NL, color = "Full week"), size = 0.6) +
  geom_line(aes(y = avg_vol_onlyweekend, color = "Weekend"), size = 0.6) +
  labs(x = "Hour", y = "Volatility in %", color = "Legend", subtitle = "Netherlands (2018 | 2019 | 2020)") + 
  ggtitle("Volatility of hourly prices over a day - full week against weekend") +
  scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "4 hour")

# export table for question 1.2 in latex
q1.2_inperc$hour <- as.character(q1.2$hour)
print(xtable(q1.2_inperc, type = "latex"), file = "q1.2.tex")


### Part 1.3
# make a year and day variable 
df$year <- substring(df$date, 1,4)
df$day <- substring(df$date, 9,10)

# compute daily averages per day and month in each year 
q1.3 <- df %>%
  group_by(date_exclhour) %>%
    mutate(avg_dailyp= mean(dutch_power)) %>%
      select(year, day, date_exclhour, avg_dailyp)
q1.3 <- q1.3[!duplicated(q1.3), ]  

# compute average daily prices over the year 
q1.3$monthday <- substring(q1.3$date_exclhour, 6,10)
q1.3 <- q1.3 %>%
  group_by(monthday) %>% 
    mutate(avg_dayprice= mean(avg_dailyp)) %>%
      select(monthday, avg_dayprice)
q1.3 <- q1.3[!duplicated(q1.3), ]  

# create day variable and plot graph
q1.3$days <- 1:366
q1.3 <- q1.3[,-1]
q1.3 <- q1.3[-366,]

ggplot(q1.3) +
  aes(x = days, y = avg_dayprice) +
  geom_line(size = 0.5, colour = "#0c4c8a") +
  labs(x = "days", y = "Average over hourly prices per day", 
       title = "Average daily prices over a year ~ averaged over the three years ",
       subtitle= "Netherlands (2018 | 2019 | 2020)")+
  theme_minimal()


### part 1.4 
# make a dummy variable that identifies the months April - September
df$month <- substring(df$date, 6,7)
df$apr_septdummy <- ifelse(df$month=="04" | df$month=="05" | df$month=="06"
                           | df$month=="07" | df$month=="08" | df$month=="09", 1,0)

# compute average hourly solar generation over a day
q1.4 <- df %>%
  group_by(hour, apr_septdummy) %>%
    mutate(avg_solar= mean(solar)) %>%
      select(hour,avg_solar, apr_septdummy)
q1.4 <- q1.4[!duplicated(q1.4), ]  

# Plot average hourly solar generation over a day: April- September vs October-March
q1.4_1 <- q1.4 %>% filter(apr_septdummy==0) %>% rename(avg_solar_okt_march= avg_solar)
q1.4_2 <- q1.4 %>% filter(apr_septdummy==1) %>% rename(avg_solar_apr_sept= avg_solar)
q1.4 <- merge(q1.4_1, q1.4_2, by="hour") 
q1.4 <- q1.4 %>% select(hour, avg_solar_okt_march, avg_solar_apr_sept)

# !! neglect the date variable in the table, this is made in order to be able to plot ONLY time in ggplot !!
q1.4_plot <- q1.4
q1.4_plot$hour <- as.POSIXct(paste0("2016-09-23", q1.4_plot$hour ), tz = "GMT")

ggplot(q1.4_plot, aes(x = hour)) +
  geom_line(aes(y = avg_solar_apr_sept, color = "April-September"), size = 0.8) +
  geom_line(aes(y = avg_solar_okt_march, color = "October-March"), size = 0.8) +
  labs(x = "hours", y = "solar power generation in MWh", color = "Legend") + 
  ggtitle("Average hourly solar generation over a day") +
  scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "4 hour")

# export table for question 1.4 in latex
q1.4$hour <- as.character(q1.4$hour)
print(xtable(q1.4, type = "latex"), file = "q1.4.tex")


### part 1.5
# compute daily averages of solar and off-shore wind per day and month in each year 
q1.5 <- df %>%
  group_by(date_exclhour) %>%
    mutate(avg_dailysolar= mean(solar), avg_dailyoffshorewind= mean(wind_off_shore)) %>%
      select(year, day, date_exclhour, avg_dailysolar, avg_dailyoffshorewind)
q1.5 <- q1.5[!duplicated(q1.5), ]  

# compute average daily prices over the year 
q1.5$monthday <- substring(q1.5$date_exclhour, 6,10)
q1.5 <- q1.5 %>%
  group_by(monthday) %>% 
    mutate(avg_dsolar= mean(avg_dailysolar), avg_doffshorewind=mean(avg_dailyoffshorewind)) %>%
      select(monthday, avg_dsolar, avg_doffshorewind)
q1.5 <- q1.5[!duplicated(q1.5), ]  

# create day variable and plot graph
q1.5$days <- 1:366

ggplot(q1.5, aes(x = days)) +
  geom_line(aes(y = avg_dsolar, color = "Solar"), size = 0.4) +
  geom_line(aes(y = avg_doffshorewind, color = "Off-shore wind"), size = 0.4) +
  labs(x = "days", y = "average over hourly generation per day in MWh", 
       color = "Legend", subtitle = "average over the three years (2018 | 2019 | 2020)") + 
  ggtitle("Average daily solar and off-shore wind generation over a year") 



##### Question 2: Prepare the data set for making daily price predictions
### part 2.1
# Prepare the data
q2 <- df %>%
  select(date_exclhour, hour, dutch_power, german_power, belgium_power, norway_power, 
         dutch_load, dutch_generation_forecast, solar, wind_off_shore, wind_on_shore)
q2$id_hour <- 1:24
q2 <- q2 %>% select(-hour) 

q2.1 <- reshape(q2, idvar = "date_exclhour", timevar = "id_hour", direction = "wide", sep="_")

# exporting table to latex
print(xtable(q2.1, type = "latex"), file = "q2.1.tex")


### part 2.2
# Split the data into a dataframe with the outcomes (((n − 1) × 24 of hourly electricity prices) 
# and the features ((n − 1) × 24p) features lagged by 1 day.

## here I just take the dutch price variables, since these are already day-ahead electricity prices (y variables)
q2.2_y <- q2.1 %>% select(date_exclhour,dutch_power_1, dutch_power_2, dutch_power_3, dutch_power_4,
                          dutch_power_5, dutch_power_6, dutch_power_7, dutch_power_8
                          , dutch_power_9, dutch_power_10, dutch_power_11, dutch_power_12
                          , dutch_power_13, dutch_power_14, dutch_power_15, dutch_power_16, 
                          dutch_power_17, dutch_power_18, dutch_power_19, dutch_power_20, 
                          dutch_power_21, dutch_power_22, dutch_power_23, dutch_power_24) %>% 
                    rename(date=date_exclhour)

        # remove first date variable since otherwise wont correspond with the lagged variables 
q2.2_y <- q2.2_y[-1,]

## here I just take the features and lag them otherwise will be using feature info (x variables)
date <- q2.1$date_exclhour
date <- date[-1]

q2.2_x <- q2.1 %>% select(-date_exclhour)
q2.2_x <- q2.2_x %>% mutate_all(lag)
q2.2_x <- q2.2_x[-1,]
names(q2.2_x) <- paste(names(q2.2_x), "lag", sep="_")

q2.2_x <- cbind(date, q2.2_x)


### part 2.3: Clean the data
## Get rid of the features in q2.2_x that have too many 0 entries
# I use a 15% cut-off, meaning that if a feature has more than 15% 0's, then remove
# it is known already that we have 1095 observations for each feature 
number_0_inperc <- (data.frame(zero_values=colSums(q2.2_x == 0))) /1095    # mostly solar that has 0 values
keep_factors <- number_0_inperc < 0.15

q2.2_x <- q2.2_x[, keep_factors] # removed solar in dark hours --> make inference

## Split the sample into a training set of 2.5 years and a test set of 0.5 years.
# 2.5 years is approx 918 days, and 0.5 years is approx 182 days, so i allocate precisely last 182 days to test set
# start: 2018-01-02, end: 2020-12-29 / training set: 2018-01-02 -> 2020-07-02 , test set: 2020-07-03 -> 2020-12-29

# separate the date variable
date <- q2.2_x$date

# training set
x_train <- q2.2_x %>%
  filter(date<= "2020-07-02") %>% 
    select(-date)

x_test <- q2.2_x %>%
  filter(date> "2020-07-02") %>% 
    select(-date)

# test set 
y_train <- q2.2_y %>%
  filter(date<= "2020-07-02")%>% 
    select(-date)

y_test <- q2.2_y %>%
  filter(date> "2020-07-02") %>% 
    select(-date)


## Normalize the data so that all features have mean 0 and a standard deviation of 1.
x_train <- scale(x_train, center = TRUE, scale = TRUE)
x_test <- scale(x_test, center = TRUE, scale = TRUE)


##### Question 3: Predicting electricity prices in the first hour of the day
# i make a separate dataframe for this question to make it easier for myself 
# i only take the variables needed to predict the first hour of the day 
x_train <- as_tibble(x_train)
x_test <- as_tibble(x_test)

# need to take price first hour of the day to use is as dependent variable
train_q3 <- cbind(dutch_power_1=y_train[,1], x_train)
test_q3 <- cbind(dutch_power_1=y_test[,1], x_test)

### part 3.1: Build a benchmark model 
# train the model in training set to predict price for first hour of the day (1) 
# using the price in the last hour of the previous day (and a constant) as a predictor.
q3.1_lmmodel <- lm(dutch_power_1 ~ dutch_power_24_lag, data=train_q3)
summary(q3.1_lmmodel)    # in-sample R-squared: 0.7908

# test the model on the test set and compute the OOS RMSE 
benchmarklm_q3.1 <- predict(q3.1_lmmodel, test_q3)
  
eval_results(test_q3$dutch_power_1, benchmarklm_q3.1, test_q3)
                  # RMSE of 8.211028 and R-squared of -0.09165744


### part 3.2: Machine learning models
# split training data in train and validation set
set.seed(73)
split <- round(nrow(train_q3) * 0.8)   # 2 years (730 days) of 2.5 years (912 days) corresponds to 80%
# Create training set 
train_q3.1 <- train_q3[1:split, ]
# Create validation set
validation_q3.1 <- train_q3[(split+1):nrow(train_q3), ]

# ==> training the models, I cannot use cross validation since this will break the autocorrelation
# I will thus have to manually train some models and evaluate them on the validation set to 
# see which hyperparameters are optimal 


## First model: Lasso
# fit the model, manually try different lambda parameters 
lasso_q3 <- caret::train(dutch_power_1 ~ ., data = train_q3.1,
                       method = "glmnet",family = "gaussian", 
                       tuneGrid = expand.grid(alpha=1, lambda=0.23), # best lambda parameter for validation (0.23), i tried different parameters
                       trControl= trainControl(method="none"))
            # lambda parameters tried: 0.01, 0.14,0.23,0.55, 1 ==> 0.23 had lowest validation set RMSE

# training performance rmse of Lasso
lasso_predtrn <- predict(lasso_q3, train_q3.1)
rmselasso_trn <- eval_results(train_q3.1$dutch_power_1, lasso_predtrn, train_q3.1)
rmselasso_trn
              # in-sample R-squared: 0.7513485
              # in-sample RMSE: 5.387478

# Validation performance rmse and rsquared of Lasso
lasso_predval <- predict(lasso_q3, validation_q3.1)
rmselasso_val <- eval_results(validation_q3.1$dutch_power_1, lasso_predval, validation_q3.1)
rmselasso_val
              # R-squared validation set: 0.7244118
              # RMSE validation set: 3.504528

# test set performance rmse and r-squared lasso
lasso_predtest <- predict(lasso_q3, test_q3)
rmselasso_test <- eval_results(test_q3$dutch_power_1, lasso_predtest, test_q3)
rmselasso_test
              # R-squared test set: 0.1190801
              # RMSE test set: 7.376028


## Second model: Partial Least Squares
# fitting the pls model, trying different ncomp values
pls.fit <- caret::train(dutch_power_1 ~ ., data = train_q3.1,
                 method = "pls",
                 tuneGrid= expand.grid(ncomp = 10), # I tried a lot of parameters -> 10 is optimal in validation set!
                 trControl = trainControl(method="none"))
              # ncomp tried: 1,5,10,15,20,40 ==> optimal (based on validation set): 10 

# training rmse of PLS
plspred_trn <- predict(pls.fit, train_q3.1)
rmsepls_trn <- eval_results(train_q3.1$dutch_power_1, plspred_trn, train_q3.1)
rmsepls_trn
          # in-sample R-squared: 0.7747967
          # in-sample RMSE: 5.127167

# Validation rmse of PLS
plspred_val <- predict(pls.fit, validation_q3.1)
rmsepls_val <- eval_results(validation_q3.1$dutch_power_1, plspred_val, validation_q3.1)
rmsepls_val
          # R-squared validation set: 0.6716787 
          # RMSE validation set: 3.825153

# evaluate model on test set 
plspred_test <- predict(pls.fit, test_q3)
rmsepls_test <- eval_results(test_q3$dutch_power_1, plspred_test, test_q3)
rmsepls_test
          # R-squared test set: 0.05883781
          # RMSE test set: 7.624065


## Third model: Random Forest
# fitting the RF model, trying different parameters values
rf_model <- caret::train(dutch_power_1 ~ ., data = train_q3.1,
                        method = "ranger",
                        tuneGrid= expand.grid(mtry = 150, #170 optimal in val set 
                                              min.node.size= 10, #5 optimal in val set 
                                              splitrule="variance"), # I tried a lot of parameters -> 10 is optimal!
                        trControl = trainControl(method="none"))
                  # mtry tested: 10, 30, 60, 100, 150, 170, 190 ==> 150 optimal (based on val set)
                  # min.node.siz tested: 1,3,5,10,15,20,25,30 ==> 5 optimal (based on val set)

# training rmse of RF
rfpred_trn <- predict(rf_model, train_q3.1)
rmserf_trn <- eval_results(train_q3.1$dutch_power_1, rfpred_trn, train_q3.1)
rmserf_trn
            # in-sample R-squared: 0.9437658
            # in-sample RMSE: 2.562066

# Validation rmse of RF
rfpred_val <- predict(rf_model, validation_q3.1)
rmserf_val <- eval_results(validation_q3.1$dutch_power_1, rfpred_val, validation_q3.1)
rmserf_val
            # R-squared validation set: 0.1840763 
            # RMSE validation set: 6.030093

# evaluate model on test set 
rfpred_test <- predict(rf_model, test_q3)
rmserf_test <- eval_results(test_q3$dutch_power_1, rfpred_test, test_q3)
rmserf_test
            # R-squared test set: -0.2053268
            # RMSE test set: 8.627933



##### Question 4: Predicting hourly electricity prices

### part 4.1: Build a benchmark model
# 24 linear regressions to predict the 24 hourly dutch electricity prices over the day 
# using the price in the last hour of the previous day (and a constant) as a predictor.
lm_models_q4<- lapply(1:length(y_train), function(x) lm(y_train[,x] ~ dutch_power_24_lag , 
                                                        data=x_train))
summaries_lmmodels <- lapply(lm_models_q4, summary)

in_sampleR2 <- sapply(summaries_lmmodels, function(x) c(r_sq = x$r.squared))

## Plot the in-sample R2 for the 24 models in one graph
in_sampleR2 <- as_tibble(in_sampleR2)
in_sampleR2 <- in_sampleR2 %>% rename(lm = value)

in_sampleR2_plot <- as_tibble(in_sampleR2)
in_sampleR2_plot$hours <- 1:24

ggplot(in_sampleR2_plot) +
 aes(x = hours, y = lm) +
 geom_line(size = 0.92, colour = "#0c4c8a") +
 labs(x = "Hour", y = "R-squared", title = "In-sample R-squared - linear model ", subtitle = "benchmark model") + theme_minimal()

# make table and export to latex
print(xtable(in_sampleR2_plot, type = "latex"), file = "q4.1_R2.tex")


## Plot a corresponding graph for the out-of-sample RMSE
# make OOS predictions with the 24 models 
OOS_predlm <- sapply(lm_models_q4, function(x) {predict(x,x_test)})
OOS_predlm <- as_tibble(OOS_predlm)

# compute RMSE per prediction 
OOS_rmselm <- (OOS_predlm - y_test)^2
OOS_rmselm <- sapply(OOS_rmselm, function(x) {sqrt(mean(x))})
OOS_rmselm <- as_tibble(OOS_rmselm)
OOS_rmselm <- OOS_rmselm %>% rename(lm = value)

OOS_rmselm_plot <- OOS_rmselm
OOS_rmselm_plot$hours <- 1:24

ggplot(OOS_rmselm_plot) +
 aes(x = hours, y = lm) +
 geom_line(size = 1L, colour = "#0c4c8a") +
 labs(x = "Hour", y = "RMSE", title = "OOS RMSE - Linear model ", subtitle = "benchmark model") +
 theme_minimal()

# make table and export to latex
print(xtable(OOS_rmselm_plot, type = "latex"), file = "q4.1_rmse.tex")


### part 4.2: Machine learning models
## preparing data to use for this question 

# making train and validation set for the features 
set.seed(73)
split <- round(nrow(x_train) * 0.8)   
x_train_q4 <- x_train[1:split, ]
x_val_q4 <- x_train[(split+1):nrow(x_train), ]

# making train and validation set for the y variables
set.seed(73)
split <- round(nrow(y_train) * 0.8)   
y_train_q4 <- y_train[1:split, ]
y_val_q4 <- y_train[(split+1):nrow(y_train), ]

## need to use following datasets for this question 
# training set 
x_train_q4
y_train_q4

# validation set 
x_val_q4
y_val_q4

# test set 
x_test
y_test

# train and val set together in order to perform time-series cv 
x_train
y_train

# first define our TScv method, we will start at training set and validate in validation set 
# I will use this for all the models, in order to tune the hyperparameters in the val set 
myTimeControl <- trainControl(method = "timeslice", initialWindow = 730,
                              horizon = 183,
                              fixedWindow = FALSE,
                              allowParallel = TRUE, verboseIter = TRUE)


### First model: 24 lasso models 
## fitting the 24 models on the training set 
# train 24 models and for each model it will take the best tuning parameter from the list defined
lasso_q4 <- lapply(1:length(y_train), 
                   function(x) caret::train(y=y_train[,x],x= x_train, 
                                            method="glmnet", family = "gaussian",  
                                            tuneGrid = expand.grid(alpha=1, lambda=seq(0.01,1,0.05)), 
                                            trControl = myTimeControl))

## in-sample R2 LASSO
# extract in-sample R2 for each model for which optimal lambda was used 
R2_lassos_q4.2 <- as_tibble(sapply(1:length(lasso_q4), function (x) lasso_q4[[x]][["resample"]]$Rsquared))
R2_lassos_q4.2 <- R2_lassos_q4.2 %>% rename(lasso = value)

## OOS RMSE LASSO 
# predict on test set with 24 lasso's
OOS_pred_lassoq4 <- as_tibble(sapply(lasso_q4, function(x) {predict(x,x_test)}))

# compute RMSE per prediction 
OOS_rmse_lasso <- (OOS_pred_lassoq4 - y_test)^2
OOS_rmse_lasso <- as_tibble(sapply(OOS_rmse_lasso, function(x) {sqrt(mean(x))}))
OOS_rmse_lasso <- OOS_rmse_lasso %>% rename(lasso=value)

# table for this model to report on the slides and extract to latex
optimal_lambdas_lasso <- as_tibble(sapply(1:length(lasso_q4), function (x) lasso_q4[[x]][["bestTune"]][["lambda"]]))
lasso_q4table <- cbind(optimal_lambdas_lasso, R2_lassos_q4.2, OOS_rmse_lasso)
print(xtable(lasso_q4table, type = "latex"), file = "q4_lasso.tex")


### Second model: 24 Partial Least Squares models 
## fitting the 24 models on the training set 
# train 24 models and for each model it will take the best tuning parameter from the list defined
pls_q4 <- lapply(1:length(y_train), 
                   function(x) caret::train(y=y_train[,x],x= x_train,  method="pls",   
                                            tuneGrid = expand.grid(ncomp=seq(1,30,2)), 
                                            trControl = myTimeControl))

## in-sample R2 PLS
# extract in-sample R2 for each pls model for which optimal ncomp was used 
R2_PLS_q4.2 <- as_tibble(sapply(1:length(pls_q4), function (x) pls_q4[[x]][["resample"]]$Rsquared))
R2_PLS_q4.2 <- R2_PLS_q4.2 %>% rename(pls = value)

## OOS RMSE PLS 
# predict on test set with 24 lasso's
OOS_pred_PLSq4 <- as_tibble(sapply(pls_q4, function(x) {predict(x,x_test)}))

# compute RMSE per prediction 
OOS_rmse_PLSq4 <- (OOS_pred_PLSq4 - y_test)^2
OOS_rmse_PLSq4 <- as_tibble(sapply(OOS_rmse_PLSq4, function(x) {sqrt(mean(x))}))
OOS_rmse_PLSq4 <- OOS_rmse_PLSq4 %>% rename(pls=value)

# table for this model to report on the slides and extract to latex
optimal_ncomps_pls <- as_tibble(sapply(1:length(pls_q4), function (x) pls_q4[[x]][["bestTune"]][["ncomp"]]))
pls_q4table <- cbind(optimal_ncomps_pls, R2_PLS_q4.2, OOS_rmse_PLSq4)
print(xtable(pls_q4table, type = "latex"), file = "q4_pls.tex")


### Third model: 24 random forest models 
## fitting the 24 models on the training set 
# train 24 models and for each model it will take the best tuning parameter from the list defined
rf_q4 <- lapply(1:length(y_train), 
                 function(x) caret::train(y=y_train[,x],x= x_train,  method="ranger",   
                                          tuneGrid = expand.grid(mtry=c(10,100,180), 
                                                                 min.node.size= c(2,12,20),
                                                                 splitrule="variance"),
                                          trControl = myTimeControl))

## in-sample R2 Random forest
# extract in-sample R2 for each pls model for which optimal ncomp was used 
R2_RF_q4.2 <- as_tibble(sapply(1:length(rf_q4), function (x) rf_q4[[x]][["resample"]]$Rsquared))
R2_RF_q4.2 <- R2_RF_q4.2 %>% rename(rf = value)

## OOS RMSE Random forest 
# predict on test set with 24 lasso's
OOS_pred_RFq4 <- as_tibble(sapply(rf_q4, function(x) {predict(x,x_test)}))

# compute RMSE per prediction 
OOS_rmse_RFq4 <- (OOS_pred_RFq4 - y_test)^2
OOS_rmse_RFq4 <- as_tibble(sapply(OOS_pred_RFq4, function(x) {sqrt(mean(x))}))
OOS_rmse_RFq4 <- OOS_rmse_RFq4 %>% rename(rf=value)

# table for this model to report on the slides and extract to latex
optimal_mtry_rf <- as_tibble(sapply(1:length(rf_q4), function (x) rf_q4[[x]][["bestTune"]][["mtry"]]))
optimal_node_rf <- as_tibble(sapply(1:length(rf_q4), function (x) rf_q4[[x]][["bestTune"]][["min.node.size"]]))
rf_q4table <- cbind(optimal_mtry_rf, optimal_node_rf, R2_RF_q4.2, OOS_rmse_RFq4)
print(xtable(rf_q4table, type = "latex"), file = "q4_RF.tex")


### PLOTTING Q4 MODELS 
## plotting the in-sample R-squared: linear, Lasso, pls and RF
# putting everything in 1 table 
Q4_R2table <- cbind(in_sampleR2, R2_lassos_q4.2, R2_PLS_q4.2, R2_RF_q4.2)
Q4_R2table$hour<- 1:24

# plotting in-sample R2 
ggplot(Q4_R2table, aes(x = hour)) +
  geom_line(aes(y = lm, color = "Linear model"), size = 0.5) +
  geom_line(aes(y = lasso, color = "Lasso"), size = 0.5) +
  geom_line(aes(y = pls, color = "Partial Least Squares"), size = 0.5) +
  geom_line(aes(y = rf, color = "Random Forest"), size = 0.5) +
  labs(x = "hour", y = "R-squared", color = "Legend", subtitle = "ML models against benchmark") + 
  ggtitle("In-sample R-squared")


## plotting the OOS RMSE: linear, Lasso, pls and RF
# putting everything in 1 table
Q4_OOSrmse <- cbind(OOS_rmselm,OOS_rmse_lasso, OOS_rmse_PLSq4, OOS_rmse_RFq4)
Q4_OOSrmse$hour<- 1:24

# plotting OOS RMSE 
ggplot(Q4_OOSrmse, aes(x = hour)) +
  geom_line(aes(y = lm, color = "Linear model"), size = 1) +
  geom_line(aes(y = lasso, color = "Lasso"), size = 1) +
  geom_line(aes(y = pls, color = "Partial Least Squares"), size = 1) +
  geom_line(aes(y = rf, color = "Random Forest"), size = 1) +
  labs(x = "hour", y = "RMSE", color = "Legend", subtitle = "ML models against benchmark") + 
  ggtitle("Out-of-sample RMSE")




##### Question 5: Trading strategy

### part 5.1: Benchmark strategy
## compute the hour of the day with the lowest and highest average price.
# first take in training set of each hour the average price 
avg_price_train <- colMeans(y_train)

# take the column with the minimum value, this is where we will charge our battery 
charge_benchmark <- which.min(avg_price_train)  
                          # dutch_power_5: at 5 we charge 

# take the column with the maximum value, this is where we will discharge our battery
discharge_benchmark <- which.max(avg_price_train)
                          # dutch_power_20: at 20 we discharge

# construct this strategy in the test set and report cumulative profit 
benchmark_strategy_profits <- y_test$dutch_power_20 - y_test$dutch_power_5
benchmark_strategy_cumprofit <- cumsum(benchmark_strategy_profits)

# plot of daily cumulative profit benchmark 
benchmark_strategy_cumprofit_plot <- as_tibble(benchmark_strategy_cumprofit)
benchmark_strategy_cumprofit_plot <- benchmark_strategy_cumprofit_plot %>% rename(benchmark=value)
benchmark_strategy_cumprofit_plot$day <- 1:182

ggplot(benchmark_strategy_cumprofit_plot) +
 aes(x = day, y = benchmark) +
 geom_line(size = 1L, colour = "#0c4c8a") +
 labs(x = "Day", y = "Balance", title = "Cumulative profit - test data", subtitle = "Benchmark strategy") +
 theme_minimal()

# compute and plot daily volatility of benchmark strategy in test set (with moving window)
benchmark_volatility_averagedaily <- sd(benchmark_strategy_profits) / length(benchmark_strategy_profits)
benchmark_volatility_averagedaily

benchmark_strategy_volatility <- runsd(benchmark_strategy_cumprofit, k=3)
benchmark_strategy_volatility_plot <- as_tibble(benchmark_strategy_volatility)
benchmark_strategy_volatility_plot$day <- 1:182

ggplot(benchmark_strategy_volatility_plot) +
  aes(x = day, y = value) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  labs(x = "Day", y = "sigma", title = "Daily volatility of benchmark model - test set ") +
  theme_minimal()



### part 5.2: Machine learning models

## First model: Lasso models
# Use the 24 hourly forecasts from the Lasso model from Question 4.2 to predict 
# hour with the lowest and the hour with the highest price for the subsequent day

# for each day, which hour has lowest price ==> charge 
chargevalue_lasso <- apply(OOS_pred_lassoq4, 1, min)
whencharge_lasso <- apply(OOS_pred_lassoq4, 1, which.min)

# for each day, which hour has highest price ==> discharge
dischargevalue_lasso <- apply(OOS_pred_lassoq4, 1, max)
whendischarge_lasso <-apply(OOS_pred_lassoq4, 1, which.max)

# Charge your battery at the hour with the lowest predicted price and discharge 
# at the hour with the highest price. If the highest price comes before the 
# lowest price, do not charge your battery at all on this day.

# need to make a loop which takes for each day the difference between the 
# predicted discharge and charge day in order to get the returns each day and construct cumulative profit
lasso_returns_test <- list()
  for(t in 1:nrow(y_test)){
    lasso_returns_test[[t]] <- ifelse(y_test[t,whendischarge_lasso[t]] > y_test[t,whencharge_lasso[t]],
                          y_test[t,whendischarge_lasso[t]] - y_test[t,whencharge_lasso[t]],0)
}
lasso_returns_test <- unlist(lasso_returns_test)

# cumulative profit and daily volatility of Lasso strategy in test data
lasso_cumprofit <- cumsum(lasso_returns_test)
lasso_vol_averagedaily <- sd(lasso_returns_test) / length(lasso_returns_test)


# setting up in table in order to plot it at the end
lasso_cumprofit_plot <- as_tibble(lasso_cumprofit)
lasso_cumprofit_plot <- lasso_cumprofit_plot %>% rename(lasso= value)


## Second model: PLS models
# Use the 24 hourly forecasts from the Lasso model from Question 4.2 to predict 
# hour with the lowest and the hour with the highest price for the subsequent day

# for each day, which hour has lowest price ==> charge 
chargevalue_pls <- apply(OOS_pred_PLSq4, 1, min)
whencharge_pls <- apply( OOS_pred_PLSq4, 1, which.min)

# for each day, which hour has highest price ==> discharge
dischargevalue_pls <- apply(OOS_pred_PLSq4, 1, max)
whendischarge_pls <-apply(OOS_pred_PLSq4, 1, which.max)

# Charge your battery at the hour with the lowest predicted price and discharge 
# at the hour with the highest price. If the highest price comes before the 
# lowest price, do not charge your battery at all on this day.

# need to make a loop which takes for each day the difference between the 
# predicted discharge and charge day in order to get the returns each day and construct cumulative profit
pls_returns_test <- list()
for(t in 1:nrow(y_test)){
  pls_returns_test[[t]] <- ifelse(y_test[t,whendischarge_pls[t]] > y_test[t,whencharge_pls[t]],
                                    y_test[t,whendischarge_pls[t]] - y_test[t,whencharge_pls[t]],0)
}
pls_returns_test <- unlist(pls_returns_test)

# cumulative profit and daily volatility of Lasso strategy in test data
pls_cumprofit <- cumsum(pls_returns_test)
pls_vol_averagedaily <- sd(pls_returns_test) / length(pls_returns_test)



# setting up in table in order to plot it at the end
pls_cumprofit_plot <- as_tibble(pls_cumprofit)
pls_cumprofit_plot <- pls_cumprofit_plot %>% rename(pls= value)


## Third model: RF models
# Use the 24 hourly forecasts from the Lasso model from Question 4.2 to predict 
# hour with the lowest and the hour with the highest price for the subsequent day

# for each day, which hour has lowest price ==> charge 
chargevalue_rf <- apply(OOS_pred_RFq4, 1, min)
whencharge_rf <- apply(OOS_pred_RFq4, 1, which.min)

# for each day, which hour has highest price ==> discharge
dischargevalue_rf <- apply(OOS_pred_RFq4, 1, max)
whendischarge_rf <-apply(OOS_pred_RFq4, 1, which.max)

# Charge your battery at the hour with the lowest predicted price and discharge 
# at the hour with the highest price. If the highest price comes before the 
# lowest price, do not charge your battery at all on this day.

# need to make a loop which takes for each day the difference between the 
# predicted discharge and charge day in order to get the returns each day and construct cumulative profit
rf_returns_test <- list()
for(t in 1:nrow(y_test)){
  rf_returns_test[[t]] <- ifelse(y_test[t,whendischarge_rf[t]] > y_test[t,whencharge_rf[t]],
                                  y_test[t,whendischarge_rf[t]] - y_test[t,whencharge_rf[t]],0)
}
rf_returns_test <- unlist(rf_returns_test)

# cumulative profit and daily volatility of Lasso strategy in test data
rf_cumprofit <- cumsum(rf_returns_test)
rf_vol_averagedaily <- sd(rf_returns_test) / length(rf_returns_test)


# setting up in table in order to plot it at the end
rf_cumprofit_plot <- as_tibble(rf_cumprofit)
rf_cumprofit_plot <- rf_cumprofit_plot %>% rename(rf= value)


## plotting cumulative profits against each other (benchmark vs ML models)
Q5.2_cumprofits <- cbind(benchmark_strategy_cumprofit_plot, lasso_cumprofit_plot,
                         pls_cumprofit_plot, rf_cumprofit_plot)

ggplot(Q5.2_cumprofits, aes(x = day)) +
  geom_line(aes(y = benchmark, color = "Benchmark"), size = 1) +
  geom_line(aes(y = lasso, color = "Lasso"), size = 1) +
  geom_line(aes(y = pls, color = "PLS"), size = 1) +
  geom_line(aes(y = rf, color = "Random Forest"), size = 1) +
  labs(x = "days", y = "Balance", color = "Legend") + 
  ggtitle("cumulative profit - test data")



### part 5.3: Build your own trading strategy
# the random forest model performed the best, so will use that model
# we will take instead of just 1 hour, 2 different hours in which we charge and discharge 

## Identify which hours to charge and discharge, according to prediction 
# charge 
whencharge_ownstrat <- apply(OOS_pred_RFq4, 1, which.minn, n=30)
whenaverage_lowestprice <- apply(whencharge_ownstrat, 1, mean)
                # on average at 6 prices at lowest  ==> first charge
                # second lowest hour approximately at 13 ==> second charge 

# discharge
whendischarge_ownstrat <- apply(OOS_pred_RFq4, 1, which.maxn, n=40)
whenaverage_highestprice <- apply(whendischarge_ownstrat, 1, mean)
                # first discharge: 10 
                # second discharge: 18


## computing profit from this trading strategy
# define when to charge and discharge every day
whencharge_ownstrat_1 <- rep(6, times=nrow(y_test))
whendischarge_ownstrat_1 <- rep(10, times=nrow(y_test))

whencharge_ownstrat_2 <- rep(13, times=nrow(y_test))
whendischarge_ownstrat_2 <- rep(18, times=nrow(y_test))

# charge battery on low price and discharge on next high price, repeat for every day 
ownstrat_returns_test1 <- list()
for(t in 1:nrow(y_test)){
  ownstrat_returns_test1[[t]] <- ifelse(y_test[t,whendischarge_ownstrat_1[t]] > y_test[t,whencharge_ownstrat_1[t]],
                                 y_test[t,whendischarge_ownstrat_1[t]] - y_test[t,whencharge_ownstrat_1[t]],0)
}
ownstrat_returns_test1 <- unlist(ownstrat_returns_test1)


ownstrat_returns_test2 <- list()
for(t in 1:nrow(y_test)){
  ownstrat_returns_test2[[t]] <- ifelse(y_test[t,whendischarge_ownstrat_2[t]] > y_test[t,whencharge_ownstrat_2[t]],
                                       y_test[t,whendischarge_ownstrat_2[t]] - y_test[t,whencharge_ownstrat_2[t]],0)
}
ownstrat_returns_test2 <- unlist(ownstrat_returns_test2)

total_profit_strategy= ownstrat_returns_test1 + ownstrat_returns_test2


# cumulative profit and daily volatility of Lasso strategy in test data
ownstrategy_cumprofit <- cumsum(total_profit_strategy)
ownstrategy_vol_averagedaily <- sd(total_profit_strategy) / length(total_profit_strategy) * 100

# construct plot of my strategy 
ownstrat_plot <- as_tibble(ownstrategy_cumprofit)
ownstrat_plot$day <- 1:182

ggplot(ownstrat_plot) +
  aes(x = day, y = value) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  labs(x = "Day", y = "Balance", title = "Cumulative profit - Own trading strategy") +
  theme_minimal()










