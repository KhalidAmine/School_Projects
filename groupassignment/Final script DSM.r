################################################################################
# Group assignment
#
# Student + SNR: 
#  - Khalid Amine               2045967
#  - Marc Stam                  2016797
#
# Data Science Methods in Finance (323080-M-6)
# Tilburg University
################################################################################

#### setting up working environment ####
dev.off(dev.list()["RStudioGD"])
rm(list=ls()) ## libs
cat("\014")

filepath <- rstudioapi::getSourceEditorContext()$path
dirpath  <- dirname(rstudioapi::getSourceEditorContext()$path)

setwd(dirpath)

# Package names 
packages <- c("ISLR", "glmnet", "ggplot2", "readxl", "forecast", "MASS", 
              "dplyr", "tibble", "zoo", "jtools", "sjmisc", 
              "matrixStats", "PerformanceAnalytics", "reshape2", 
              "groupdata2", "tidyr")

# Install packages not yet installed 
installed_packages <- packages %in% rownames(installed.packages()) 

if (any(installed_packages == FALSE)) {   
  install.packages(packages[!installed_packages]) 
} 

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


#----------------------------------------------------------------------------#

###### Question 1 ######

## 1) the number of stocks with missing and non-missing values by year (e.g., graphically)
# load in the data
df <-readRDS("2020_CRPS_DataScience.RDS")
head(df)


# Creating year variable
df$year <- substring(df$Date, 5,8)
years <- unique(df$year)

uniques <- df %>% 
  group_by(permno,year) %>% 
  summarize(count=n())

# check for total stocks per year
total <- matrix(nrow=2019,ncol=1)
for (i in 1:2019){
  total[i,1] <-  nrow(uniques[uniques$year == i,])
}

# keep only 1957-2019
total <- total[-c(1:1956),]

# now check for missing:
missing <- matrix(nrow=2019,ncol=1)
for (i in 1:2019){
  missing[i,1] <-  nrow(uniques[uniques$year == i & uniques$count < 12,])
}

missing <- missing[-c(1:1956),]

# for 1957 only 10 months of data
missing[1] <- nrow(uniques[uniques$year == 1957 & uniques$count < 10,])

nonmissing <- total - missing

yearrr <- c(1957:2019)
allstocks <- cbind(yearrr, nonmissing, missing, total)
allstocks <- as_tibble(allstocks)

# plotting number of stocks with missing and non-missing values by year
ggplot(allstocks, aes(x = yearrr)) +
  geom_line(aes(y = nonmissing, color = "nonmissings"), size = 1.5) +
  geom_line(aes(y = missing, color = "missings"), size = 1.5) +
  labs(x = "Year", y = "# of observations", color = "Legend") + 
  ggtitle(" Annual (non)-missing observations for stocks")



## 2) mean returns (reported monthly in percentage points)
# checking date variable
str(df$Date)

# Transform data to time-series
df_transformed <- dcast(df, Date~permno, value.var="retadj.1mn")
yearmonth <- df_transformed$Date

# make separate dataframe and compute monthly mean returns per permno 
df_ret <- df_transformed[,-1]
monthly_avg <- rowMeans(df_ret, na.rm = TRUE)
monthly_avg <- as.data.frame(monthly_avg)

#creating new dataframe for summary at end
df_Q1 <- cbind(df_transformed$Date, monthly_avg)


## 3) standard deviation of returns (monthly)
# compute standard deviations
df_ret <- data.matrix(df_ret)
monthly_stdev <- rowSds(df_ret, na.rm = TRUE)

# add in final dataframe
df_Q1 <- cbind(df_Q1,monthly_stdev)


## 4) mean log market cap
df$ln_marketcap <- log(df$ME)
# transforming dataset based on ln_marketcap and computing mean of log market cap 
panel_marketcap <- dcast(df, Date~permno, value.var="ln_marketcap")
panel_marketcap2 <- panel_marketcap[,-1]
mean_marketcap <- rowMeans(panel_marketcap2, na.rm = TRUE)

# Add in final dataframe 
df_Q1 <- cbind(df_Q1, mean_marketcap)


## 5) the standard deviation of log market cap
# compute standard deviations
panel_marketcap2 <- data.matrix(panel_marketcap2)
marketcap_stdev <- rowSds(panel_marketcap2, na.rm = TRUE)

# add in final dataframe
df_Q1 <- cbind(df_Q1,marketcap_stdev)


## For 2-5), report the time-series averages of the cross-sectional means in a table
# mean of mean returns and standard deviations, since no missing values
ts_mean_return <-  mean(df_Q1$monthly_avg)
ts_stdev_returns <-  mean(df_Q1$monthly_stdev)

# after calculation of return means, need to delete missing values of market cap
df_Q1 <- na.omit(df_Q1)

ts_mean_mktcap <-  mean(df_Q1$mean_marketcap)
ts_stdev_mktcap <-  mean(df_Q1$marketcap_stdev)

# 4 mean values in data frame
df2_q1 <- cbind(ts_mean_return,ts_stdev_returns,ts_mean_mktcap,ts_stdev_mktcap)



###### Question 2 ######

## portfolio 1 
df_port1 <- as.data.frame(df_ret)
na_counter <- rowSums(is.na(df_port1))
df_port1 <- cbind(na_counter,df_port1)

seperator <- 1/(24608-lag(na_counter))
df_port1 <- cbind(seperator,df_port1)
df_port1_new <- df_port1[-(1:2)]

ew_return <- rowSums(df_port1_new, na.rm = TRUE)*seperator


## portfolio 2
# getting the weights in the portfolio
panel_ME <- dcast(df, Date~permno, value.var="ME")
total_mcap <- rowSums(panel_ME[2:24609], na.rm = T)
panel_ME <- cbind(total_mcap, panel_ME)
df_port2 <- as.data.frame(panel_ME)
na_counter2 <- rowSums(is.na(df_port1))
df_port2 <- cbind(na_counter2,df_port2)

new_dfport2 <- (df_port2[4:24611]/(df_port2$total_mcap))
totalweight <- rowSums(new_dfport2[2:24608],na.rm = TRUE)
new_dfport2 <- cbind(totalweight,new_dfport2)

# now multiply lagged weights with returns 
mkt_weighted_ret <-lag(new_dfport2[2:24609]) * df_port1_new
mktw_return <- rowSums(mkt_weighted_ret, na.rm=TRUE)    # market weighted portfolio

# putting the series together

# cut first 76 months due to missing risk free and missing market factor
dftemp <- cbind(ew_return[-c(1:76)], mktw_return[-c(1:76)])
dftemp <- as.data.frame(dftemp)

dftemp$ew_return <- (dftemp$V1 +1)
dftemp$mktw_return <- (dftemp$V2 +1)

dftemp$ew_cumulative <- cumprod(dftemp$ew_return)
dftemp$mktw_cumulative <- cumprod(dftemp$mktw_return)

# final dataframe for plots
df_Q2 <- cbind(as.character(yearmonth[-c(1:76)]), dftemp)
df_Q2 <- as_tibble(df_Q2)
head(df_Q2)


# importing the ff factors
ff <- read.csv("ff factors.CSV", header=FALSE, comment.char="#")

# getting rid of first text filled rows
ff1 <- ff[-c(1:3),]
# getting rid of annual factor data 
ff2 <- ff1[-c(679:750),]
ff3 <- as_tibble(ff2)
# getting the right format & creating the market factor (emrp + rf)
ff3$V2 <- as.numeric(ff3$V2)/100
ff3$V3 <- as.numeric(ff3$V3)/100
ff3$V4 <- as.numeric(ff3$V4)/100
ff3$V5 <- as.numeric(ff3$V5)/100
ff3$V6 <- as.numeric(ff3$V6)/100
ff3$V7 <- as.numeric(ff3$V7)/100
ff3$V8 <- ff3$V2 + ff3$V7 

# getting the cumulative returns for market factor
ff3$V8 <- ff3$V8 +1
ff3$V8 <- cumprod(ff3$V8)

# rename variables
colnames(ff3)[2] <- "excess market ret"
colnames(ff3)[3] <- "smb"
colnames(ff3)[4] <- "hml"
colnames(ff3)[5] <- "rmw"
colnames(ff3)[6] <- "cma"
colnames(ff3)[7] <- "rf"
colnames(ff3)[8] <- "market factor"

# data is aligned. now bind to cumulative returns frame:
df_Q2 <- cbind(df_Q2,ff3[,-1])
# getting cumulative emrp
colnames(df_Q2)[1] <- "date"
colnames(df_Q2)[10] <- "mkt_factor_cum"
df_Q2[is.na(df_Q2)] <- 0 # replace NA's with 0(maybe not needed????)
df_Q2 <- df_Q2[,-1]      # remove date since we will be transforming it to xts 

# transform to panel time series and make plot 
# (only need to plot cumulative return of the two portfolios together with the market factor)
df_Q2 <- as_tibble(df_Q2)
head(df_Q2)
df_Q2$ew_cumulative <- as.numeric(as.character(df_Q2$ew_cumulative))
df_Q2$mktw_cumulative <- as.numeric(as.character(df_Q2$mktw_cumulative))
head(df_Q2) # now everything is double instead of character... 
df_Q2_plot <- df_Q2 %>%
  select(ew_cumulative, mktw_cumulative, `market factor`)

ts_Q2 <- xts(df_Q2_plot, order.by = seq(as.Date("1963-07-01"), 
                                        length.out=nrow(df_Q2_plot), by = "month"), df_Q2_plot)

plot.xts(ts_Q2, main="cumulative returns", yaxis.right=FALSE)
addLegend("topleft", 
          legend.names=c("Equally weighted portfolio", "Market weighted portfolio", 
                         "Market factor"), 
          lty=c(1,1,1),lwd=c(2,2,2), ncol=1,bg="white", bty="o")


##### Question 3 #####
# first construct dataframe with only weights and returns, so to use it afterwards, 
# need also sum of marketcap and then compute for each stock the % contribution 
# it has in the market cap to be able to make groups
df_Q3 <- df %>%
  select(Date, permno, ME) %>%
  group_by(Date) %>%
  mutate(total_mcap = sum(ME,na.rm=T)) %>%
  group_by(Date, permno) %>%
  mutate(perc_of_marketcap = (ME/total_mcap) *100) %>%
  select(-ME, -total_mcap)

df_Q3 <- cbind(df_Q3, returns= df$retadj.1mn)

## create 100 portfolios and exclude last 5 years (2019-2015)
df_Q3final <- cbind(year = df$year,df_Q3)
df_Q3final <- df_Q3final %>%
  filter(year<2015)

#first need to lag market cap, to construct portfolios 
df_Q3final <- df_Q3final %>%
  group_by(permno) %>%
  mutate(L.marketcap = lag(perc_of_marketcap)) 

# since all stocks in this date will have NA, no meaning in having it 
df_Q3final <- df_Q3final %>% 
  filter(Date != "Mar 1957") %>% # since all stocks in this date will have NA, no meaning in having it 
  select(Date, permno, L.marketcap, returns)

# constructing the 100 portfolios 
df_Q3final <- df_Q3final %>%
  group_by(Date) %>% 
  arrange(L.marketcap) %>%
  group(n=100, method="n_fill", force_equal=TRUE, col_name="portfolionumber") 


# need to make each portfolio value weighted, thus taking weighted mean of each stock
df_Q3final <- df_Q3final %>% 
  group_by(Date, portfolionumber) %>% 
  mutate(vw_return = weighted.mean(returns, L.marketcap, na.rm=TRUE))  


# making it more structured
final_Q3 <- df_Q3final %>%
  select(Date, vw_return, portfolionumber)

# remove duplicates 
final_Q3 <- final_Q3[!duplicated(final_Q3),]

#transform to wide dataset in order to have structured portfolios next to each other 
Q3_df <- dcast(final_Q3, Date~portfolionumber, value.var="vw_return")

# in a panel time-series with dates as indicators
portfolios_Q3 <- xts(Q3_df[,-1], order.by = seq(as.Date("1957-04-01"), 
                                                length.out=nrow(Q3_df), 
                                                by = "month"), Q3_df)

plot(portfolios_Q3[,1], type= "l")# plot of portfolio 1 (with lowest weights )
plot(portfolios_Q3[,20]) # plot portf 20


# now we have to compute annualized sharpe ratios for each portfolio
# setting up the risk free rates
riskfree <- df_Q2$rf
riskfree <- do.call(cbind, replicate(100, riskfree, simplify=FALSE))
# getting the monthly excess returns
excessreturn <- portfolios_Q3[-c(1:15),]-riskfree
exp.return <- (colMeans(excessreturn, na.rm=T)) 
r.free <- 0.0001 
# getting the monthly volatility
volatility <- colSds(excessreturn,na.rm=TRUE)

# getting the annualized Sharpe Ratios
sr_monthly <- exp.return/volatility
sr_annual <- (sr_monthly * sqrt(12))
plot(sr_annual, main = "Sharpe Ratios for Size Based Portfolios", xlab = "percentile", ylab = "Annual Sharpe Ratio")

###### Question 4 ######
#install.packages("matrixStats")
library(matrixStats)

returnmatrix <- portfolios_Q3
# monthly sharpe ratio for each portfolio
sr_monthly <- data.matrix(excessreturn/volatility)

#install.packages("PortfolioAnalytics")
library(PortfolioAnalytics)

#install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
library(IntroCompFinR)

# METHOD 1: MAXIMUM SHARPE RATIO

returnmatrix[is.na(returnmatrix)] <- 0
expectedreturn <- (colMeans(returnmatrix)) 
r.free <- 0.0001 
covariancematrix <- data.matrix(cov(returnmatrix))
bestport <- tangency.portfolio(expectedreturn, covariancematrix, r.free, shorts = TRUE)
summary(bestport)

#PLOT 4.1: weight allocation to each portfolio in optimal scenario. Short sales are allowed.
plot(bestport)

gmin.port = globalMin.portfolio(expectedreturn, covariancematrix)
ef <- efficient.frontier(expectedreturn, covariancematrix, alpha.min=-2.5,
                         alpha.max=3, nport=100,shorts = TRUE)

#PLOT 4.2:  monthly returns and volatility. Constructed a mean-variance plot with tangency and GMV
attributes(ef)
plot(ef)
plot(ef, plot.assets=TRUE, col="steelblue", pch=20)
points(gmin.port$sd, gmin.port$er, col="black", pch=16, cex=2)
points(bestport$sd, bestport$er, col="red", pch=16, cex=2)
text(gmin.port$sd, gmin.port$er, labels="GMV", pos=2)
text(bestport$sd, bestport$er, labels="TANGENCY", pos=2)
sr.tan = (bestport$er - r.free)/bestport$sd
abline(a=r.free, b=sr.tan, col="black", lwd=2)

# convert to annual
optimum <- c(bestport$sd*sqrt(12),bestport$er*12)
is_optimal_sr <- optimum[2]/optimum[1]

# PLOT 4.3: compare optimal linear combination SR with individual portfolio SR's
plot(sr_annual, main = "Sharpe Ratios for Size Based Portfolios", xlab = "percentile",
     ylab = "Annual Sharpe Ratio", ylim = c(-2.5,4))
points(x = 50, y = is_optimal_sr, col = "red", pch = 16)
text(x = 50, y = 3, labels="Optimal Linear Combination", pos=4)

# check if optimized weights sum up to 1
sum((bestport$weights))
optimalweights <- bestport$weights

# 1. optimal weights on size based portfolios 
is_optimal_sr

# 2. market 
excessretmarket <- mean(df_Q2$`excess market ret`[-c(619:678)], na.rm=TRUE)
sd_market <- sd(df_Q2$`excess market ret`[-c(619:678)], na.rm=TRUE)
sr_market <- (excessretmarket/sd_market) * sqrt(12)

# 3. equally weighted portfolio
excessretew <- mean(ew_return[c(77:694)]-df_Q2$rf[-c(619:678)], na.rm=TRUE)
sd_ew <- sd(ew_return[c(77:694)]-df_Q2$rf[-c(619:678)], na.rm=TRUE)
sr_ew <- excessretew/sd_ew * sqrt(12)

##############################################################################################
# create portfolios again + optimize with the pre specified weights 
# repeat step at Q3 but now for last 5 years
df_Q3 <- df %>%
  select(Date, permno, ME) %>%
  group_by(Date) %>%
  mutate(total_mcap = sum(ME,na.rm=T)) %>%
  group_by(Date, permno) %>%
  mutate(perc_of_marketcap = (ME/total_mcap) *100) %>%
  select(-ME, -total_mcap)

df_Q3 <- cbind(df_Q3, returns= df$retadj.1mn)

## create 100 portfolios and exclude last 5 years (2019-2015)
df_Q3final <- cbind(year = df$year,df_Q3)
df_Q3final <- df_Q3final %>%
  filter(year>=2015)

#first need to lag market cap, to construct portfolios  
df_Q3final <- df_Q3final %>%
  group_by(permno) %>%
  mutate(L.marketcap = lag(perc_of_marketcap)) 

# since all stocks in this date will have NA, no meaning in having it 
df_Q3final <- df_Q3final %>% 
  filter(Date != "Mar 1957") %>% # since all stocks in this date will have NA, no meaning in having it 
  select(Date, permno, L.marketcap, returns)

# constructing the 100 portfolios 
df_Q3final <- df_Q3final %>%
  group_by(Date) %>% 
  arrange(L.marketcap) %>%
  group(n=100, method="n_fill", force_equal=TRUE, col_name="portfolionumber") 


# need to make each portfolio value weighted, thus taking weighted mean of each stock
df_Q3final <- df_Q3final %>% 
  group_by(Date, portfolionumber) %>% 
  mutate(vw_return = weighted.mean(returns, L.marketcap, na.rm=TRUE))  


# making it more structured
final_Q3 <- df_Q3final %>%
  select(Date, vw_return, portfolionumber)

# remove duplicates 
final_Q3 <- final_Q3[!duplicated(final_Q3),]

#transform to wide dataset in order to have structured portfolios next to each other 
Q3_df <- dcast(final_Q3, Date~portfolionumber, value.var="vw_return")

# in a panel time-series with dates as indicators
portfolios_Q4 <- xts(Q3_df[,-1], order.by = seq(as.Date("2015-01-01"), 
                                                length.out=nrow(Q3_df), 
                                                by = "month"), Q3_df)

##############################################################################################
# continue with Q4
riskfree2 <- df_Q2$rf[-c(1:618)]
riskfree2 <- do.call(cbind, replicate(100, riskfree2, simplify=FALSE))

excessreturn2 <- portfolios_Q4 - riskfree2
volatility2 <- colSds(excessreturn2,na.rm=T)
sr_monthly2 <- excessreturn2/volatility2

# now we have to use the tangency weights from the in-sample optimizer
optimalweights2<- do.call("rbind", replicate(60, optimalweights, simplify = FALSE))
monthly_sr2 <-  rowSums(optimalweights2*sr_monthly2, na.rm=TRUE)

# start of the comparison (in vs.out of sample)
# 1. optimal weights on size based portfolios
oos_optimal_sr <- mean(monthly_sr2) * sqrt(12)

# 2. market 
excessretmarket2 <- mean(df_Q2$`excess market ret`[-c(1:618)], na.rm=TRUE)
sd_market2 <- sd(df_Q2$`excess market ret`[-c(1:618)], na.rm=TRUE)
sr_market2 <- excessretmarket2/sd_market2 * sqrt(12)

# 3. equally weighted portfolio
excessretew2 <- mean(ew_return[-c(1:694)]-df_Q2$rf[-c(1:618)], na.rm=TRUE)
sd_ew2 <- sd(ew_return[-c(1:694)]-df_Q2$rf[-c(1:618)], na.rm=TRUE)
sr_ew2 <- excessretew2/sd_ew2 * sqrt(12)

# final step: table
df_Q4 <- matrix(nrow=2,ncol=4)
colnames(df_Q4) <- c("period", "optimal weighted", "market","equally weighted")

df_Q4[1,1] <- "in sample"
df_Q4[2,1] <- "out of sample"
df_Q4[1,2] <- is_optimal_sr
df_Q4[2,2] <- oos_optimal_sr
df_Q4[1,3] <- sr_market
df_Q4[2,3] <- sr_market2
df_Q4[1,4] <- sr_ew
df_Q4[2,4] <- sr_ew2
df_Q4
# performance order in sample: optimal weighted > equally weighted > market
# performance order out of sample: market > optimal weighted > equally weighted

###### Question 5 ######
#install.packages("ggfortify")
install.packages("pca3d")

library(ggfortify)
library(pca3d)
insample <- (sr_monthly)
oosample <- (sr_monthly2)

# period 1: in sample
# missing values are problematic for pca -> replace by zero
insample[is.na(insample)] <- 0

# now plot first 3 PC's
insample.pca <- prcomp(insample[1:678,1:100])
summary(insample.pca)
insample.pca$sdev
pca3d(insample.pca, col = c("red"))

# period 2: out of sample
oosample[is.na(oosample)] <- 0
oosample.pca <- prcomp(oosample)
summary(oosample.pca)
pca3d(oosample.pca, col = "darkblue")

###### Question 6 ######

insample <- as.data.frame(insample)
oosample <- as.data.frame(oosample)

# PC1
srpc1_is <- colMeans(insample[1], na.rm=T) * sqrt(12)
srpc1_oos <- colMeans(oosample[1], na.rm=T) * sqrt(12)

# PC2
srpc2_is <- colMeans(insample[2], na.rm=T) * sqrt(12)
srpc2_oos <- colMeans(oosample[2], na.rm=T) * sqrt(12)

# PC3
srpc3_is <- colMeans(insample[3], na.rm=T) * sqrt(12)
srpc3_oos <- colMeans(oosample[3], na.rm=T) * sqrt(12)

# final step: table
df_Q6 <- matrix(nrow=2,ncol=4)
colnames(df_Q6) <- c("period", "PC1", "PC2","PC3")

df_Q6[1,1] <- "in sample"
df_Q6[2,1] <- "out of sample"
df_Q6[1,2] <- srpc1_is
df_Q6[2,2] <- srpc1_oos
df_Q6[1,3] <- srpc2_is
df_Q6[2,3] <- srpc2_oos
df_Q6[1,4] <- srpc3_is 
df_Q6[2,4] <- srpc3_oos
df_Q6
# performance order: PC1 > PC2 > PC3: more return for small cap portfolio (smb factor premium)

###### Question 7 ######
#install.packages("devtools")
library(devtools)

#install_github("kassambara/factoextra")
library(factoextra)

# we need to extract the rotation matrix for in sample and out of sample period
rotation_is <- insample.pca$rotation
rotation_oos <- oosample.pca$rotation

# we take the last 60 values from the in sample period to construct the correlation matrix
rotation_is <- rotation_is[1:100,1:3]
rotation_oos <- rotation_oos[1:100, 1:3]

# construct correlation matrix
correlationframe <- cbind(rotation_is,rotation_oos)

install.packages("corrr")
library(corrr)
pca_correlations <- correlate(correlationframe)
colnames(pca_correlations) <- c("is_dim1", "is_dim2", "is_dim3","oos_dim1", "oos_dim2", "oos,dim3")

biplot(insample.pca, main = "in sample biplot")
biplot(oosample.pca, main = "out-of-sample biplot")

###### Question 8 ######

# we go back to the original dataset: "df"
df2 <- df
monthlabel <- factor(df2$Date)

# converting non-numeric variables to numeric
df2$shrcd <- as.numeric(df2$shrcd)
df2$exchcd <- as.numeric(df2$exchcd)

# getting all medians: for each variable, for each month
median_shrcd <- as.vector(by(data = df2$shrcd, INDICES = monthlabel, FUN = median,na.rm=T))
median_exchcd <- as.vector(by(data = df2$exchcd, INDICES = monthlabel, FUN = median,na.rm=T))
median_cfacpr <- as.vector(by(data = df2$cfacpr, INDICES = monthlabel, FUN = median,na.rm=T))
median_cfacshr <- as.vector(by(data = df2$cfacshr, INDICES = monthlabel, FUN = median,na.rm=T))
median_shrout <- as.vector(by(data = df2$shrout, INDICES = monthlabel, FUN = median,na.rm=T))
median_prc <- as.vector(by(data = df2$prc, INDICES = monthlabel, FUN = median,na.rm=T))
median_vol <- as.vector(by(data = df2$vol, INDICES = monthlabel, FUN = median,na.rm=T))
median_retx  <- as.vector(by(data = df2$retx , INDICES = monthlabel, FUN = median,na.rm=T))
median_retadj.1mn <- as.vector(by(data = df2$retadj.1mn, INDICES = monthlabel, FUN = median,na.rm=T))
median_ME <- as.vector(by(data = df2$ME, INDICES = monthlabel, FUN = median,na.rm=T))
median_port.weight <- as.vector(by(data = df2$port.weight, INDICES = monthlabel, FUN = median,na.rm=T))
median_ln_marketcap <- as.vector(by(data = df2$ln_marketcap, INDICES = monthlabel, FUN = median,na.rm=T))

# replacing the NA's by median: for each variable, for each month
df2 <- cbind(as.numeric(monthlabel),df2)
colnames(df2)[1] <- "monthlabel"

# match medians with identifier (for each month to store the different cross-sectional medians)
matchvector <- (c(unique(monthlabel)))

# make loop for all variables
# first the frame with the identifier
nafilling <- cbind(matchvector,median_shrcd,median_exchcd,median_cfacpr, median_cfacshr, median_shrout, median_prc,
                   median_vol, median_retx, median_retadj.1mn, median_ME, median_port.weight,median_ln_marketcap)
check <- (c(unique(monthlabel)))

# making the loop
for (i in 1:754){
  check[i] <- (df2$monthlabel[i] == nafilling[i,1])
  df2$shrcd[is.na(df2$shrcd[c(check[i]==TRUE)])] <- nafilling[i,2]
  df2$exchcd[is.na(df2$exchcd[c(check[i]==TRUE)])] <- nafilling[i,3]
  df2$cfacpr[is.na(df2$cfacpr[c(check[i]==TRUE)])] <- nafilling[i,4]
  df2$cfacshr[is.na(df2$cfacshr[c(check[i]==TRUE)])] <- nafilling[i,5]
  df2$shrout[is.na(df2$shrout[c(check[i]==TRUE)])] <- nafilling[i,6]
  df2$prc[is.na(df2$prc[c(check[i]==TRUE)])] <- nafilling[i,7]
  df2$vol[is.na(df2$vol[c(check[i]==TRUE)])] <- nafilling[i,8]
  df2$retx[is.na(df2$retx[c(check[i]==TRUE)])] <- nafilling[i,9]
  df2$retadj.1mn[is.na(df2$retadj.1mn[c(check[i]==TRUE)])] <- nafilling[i,10]
  df2$ME[is.na(df2$ME[c(check[i]==TRUE)])] <- nafilling[i,11]
  df2$port.weight[is.na(df2$port.weight[c(check[i]==TRUE)])] <- nafilling[i,12]
  df2$ln_marketcap[is.na(df2$ln_marketcap[c(check[i]==TRUE)])] <- nafilling[i,13]
}
sum(is.na(df2))
# all NA's are filled in.

# now scaling df in new frame from -1 to 1:

#install.packages("scales")
library(scales)
#install.packages("data.table")
library(data.table)

head(df2)
scaleddf <- df2
scaleddf <- data.table(scaleddf)

# scaling in cross-section
monthlabel <- scaleddf$monthlabel
retadj.1mn <- scaleddf$retadj.1mn
test123 <- scaleddf %>% select(-Date,-shrcd,- permno, -exchcd,-year,-ln_marketcap,-retadj.1mn)
head(as_tibble(test123))

# scaling all predictor features
test123 <- test123 %>% group_by(monthlabel) %>% mutate(monthlabel = monthlabel, shrout = rescale(shrout, c(-1,1)))
test123 <- test123 %>% group_by(monthlabel) %>% mutate(monthlabel = monthlabel, prc = rescale(prc, c(-1,1)))
test123 <- test123 %>% group_by(monthlabel) %>% mutate(monthlabel = monthlabel, vol = rescale(vol, c(-1,1)))
test123 <- test123 %>% group_by(monthlabel) %>% mutate(monthlabel = monthlabel, ME = rescale(ME, c(-1,1)))
test123 <- test123 %>% group_by(monthlabel) %>% mutate(monthlabel = monthlabel, port.weight = rescale(port.weight, c(-1,1)))

####################################################
merge1 <- scaleddf %>%  select(Date,permno,shrcd,exchcd,year,ln_marketcap)
test123 <- as_tibble(test123)
merge2 <- test123 %>% select( -monthlabel)
scaleddf <- cbind(monthlabel,retadj.1mn,merge1, merge2)

###### Question 9 ######

# from now we work with scaleddf
# repeat from question 1

## 2) mean returns (reported monthly in percentage points)
# checking date variable
str(scaleddf$Date)
# Transform data to panel data
df_transformed2 <- dcast(scaleddf, Date~permno, value.var="retadj.1mn")

# make separate dataframe and compute monthly mean returns per permno 
df_ret2 <- df_transformed2[,-1]
monthly_avg2 <- rowMeans(df_ret2, na.rm = TRUE)
monthly_avg2 <- as.data.frame(monthly_avg2)

#creating new dataframe for summary at end
df_Q9 <- cbind(df_transformed2$Date, monthly_avg2)

## 3) standard deviation of returns (monthly,cross-section)

df_ret2 <- data.matrix(df_ret2)
monthly_stdev2 <- rowSds(df_ret2, na.rm = TRUE)

# add in final dataframe
df_Q9 <- cbind(df_Q9,monthly_stdev2)

## 4) mean log market cap
# transforming dataset based on ln_marketcap and computing mean of log market cap 
panel_marketcap3 <- dcast(scaleddf, Date~permno, value.var="ln_marketcap")
# to correct for very small values: log(<0.01) will result in -inf observations
panel_marketcap3[panel_marketcap3 < 0.01] <- 0
mean_marketcap3 <-rowMeans(panel_marketcap3[,-1], na.rm = TRUE)

# Add in final dataframe 
df_Q9 <- cbind(df_Q9, mean_marketcap3)

## 5) the standard deviation of log market cap

panel_marketcap3 <- data.matrix(panel_marketcap3[,-1])
marketcap_stdev2 <- rowSds(panel_marketcap3, na.rm = TRUE)

# add in final dataframe
df_Q9 <- cbind(df_Q9,marketcap_stdev2)

# mean of mean returns and standard deviations, since no missing values
ts_mean_return2 <-  mean(df_Q9$monthly_avg2)
ts_stdev_returns2 <-  mean(df_Q9$monthly_stdev2)

ts_mean_mktcap2 <-  mean(df_Q9$mean_marketcap3, na.rm=T)
ts_stdev_mktcap2 <-  mean(df_Q9$marketcap_stdev2, na.rm=T)

# 4 mean values in data frame
df2_q9 <- cbind(ts_mean_return2,ts_stdev_returns2,ts_mean_mktcap2,ts_stdev_mktcap2)

# now we can start the comparison of Q9 and Q1 samples
df3_q9 <- rbind(df2_q9,df2_q1)
names_ <- (as.vector(c(1:2)))
names_[1] <- "Summary statistics Q9"
names_[2] <- "Summary statistics Q1"
df3_q9 <- cbind(names_,df3_q9)
df3_q9
# comparison is right, check market cap mean and sd

###### Question 10 ######

# step 1. add some extra factors to this model to improve predictive power #
extrafactors <- read_excel("extra factors for dsm2.xlsx")
lead.month <- c(2:755)        # identification number used for merging
extrafactors <- cbind(lead.month,extrafactors)
extrafactors[is.na(extrafactors)] <- 0
extrafactors[1,2] <-"03/31/1957"

# scaling the factors
########################################################################################
extrafactors <- data.matrix(extrafactors)
extrafactors[,3] <- rescale(extrafactors[,3], c(-1,1))
extrafactors[,4] <- rescale(extrafactors[,4], c(-1,1))
extrafactors[,5] <- rescale(extrafactors[,5], c(-1,1))
extrafactors[,6] <- rescale(extrafactors[,6], c(-1,1))
extrafactors[,7] <- rescale(extrafactors[,7], c(-1,1))
extrafactors[,8] <- rescale(extrafactors[,8], c(-1,1))
extrafactors[,9] <- rescale(extrafactors[,9], c(-1,1))
extrafactors[,10] <- rescale(extrafactors[,10], c(-1,1))
extrafactors[,11] <- rescale(extrafactors[,11], c(-1,1))
extrafactors[,12] <- rescale(extrafactors[,12], c(-1,1))
extrafactors[,13] <- rescale(extrafactors[,13], c(-1,1))
extrafactors[,14] <- rescale(extrafactors[,14], c(-1,1))
extrafactors[,15] <- rescale(extrafactors[,15], c(-1,1))
extrafactors[,16] <- rescale(extrafactors[,16], c(-1,1))
extrafactors[,17] <- rescale(extrafactors[,17], c(-1,1))
extrafactors[,18] <- rescale(extrafactors[,18], c(-1,1))
extrafactors[,19] <- rescale(extrafactors[,19], c(-1,1))
extrafactors[,20] <- rescale(extrafactors[,20], c(-1,1))
extrafactors[,21] <- rescale(extrafactors[,21], c(-1,1))
extrafactors[,22] <- rescale(extrafactors[,22], c(-1,1))
extrafactors[,23] <- rescale(extrafactors[,23], c(-1,1))
extrafactors[,24] <- rescale(extrafactors[,24], c(-1,1))
extrafactors[,25] <- rescale(extrafactors[,25], c(-1,1))
extrafactors[,26] <- rescale(extrafactors[,26], c(-1,1))
extrafactors[,27] <- rescale(extrafactors[,27], c(-1,1))
extrafactors[,28] <- rescale(extrafactors[,28], c(-1,1))
extrafactors[,29] <- rescale(extrafactors[,29], c(-1,1))
extrafactors[,30] <- rescale(extrafactors[,30], c(-1,1))

extrafactors <- as.data.frame(extrafactors)
########################################################################################

# step 2. load original dataset in panel format and modify structure
dataq10 <- scaleddf %>% as_tibble() 

#first need to lead return, to construct portfolios  
dataq10 <- dataq10 %>%
  group_by(permno) %>%
  rename(returns_t = retadj.1mn) %>%
  mutate(lag.returns = lag(returns_t), lead.month = lead(monthlabel), 
         lead.Date = lead(Date), lead.returns = lead(returns_t))

## month_t+1 variable as stated in appendix 
dataq10$day <- 31

# dropping irrelevant features
drops <- c("retx", "monthlabel", "value", "shrcd", "exchcd", 
           "port.weight", "lead.Date")
dataq10 <- dataq10[ , !(names(dataq10) %in% drops)]

# add 1. volume/marketcap (vol/ME)
dataq10$me_per_vol <- dataq10$vol/dataq10$ME
# add 2. adding volatility from time  t=1
vol_t <- df_Q9$monthly_stdev2[c(2:755)]
vol_t <- cbind(lead.month,vol_t)
dataq10 <- merge(dataq10,vol_t)

# rearranging the columns, same format as in appendix assignment
dataq10 <- as_tibble(dataq10)
dataq10 <- dataq10 %>% select(Date, year, lead.month, permno, lead.returns, cfacpr, cfacshr ,
                              shrout , prc,  vol  , returns_t,   ME, lag.returns, day, me_per_vol, vol_t)

# NA test (should be 0)
sum(is.na(dataq10))
# these NA's arise from taking a lead and a lag variable; drop
dataq10 <- na.omit(dataq10)

# merge with extra factors: this takes <1 minute 
date <- dataq10$Date
dataq10 <- dataq10 %>% select(-Date)
dataq10 <- merge(dataq10,extrafactors)
dataq10 <- dataq10 %>% select(-Date)
dataq10 <- cbind(date, dataq10)

# remove irrelevant variable csp
dataq10 <- as_tibble(dataq10)
dataq10 <- dataq10 %>%
  select(-csp)

# step 3. making split in train_validation and testing sample  
train_validation <-  dataq10 %>%
  filter(year < 2015 & year >= 1990) %>%
  select(- year, -day) 
train_validation <- as.data.frame(train_validation)
head(train_validation)

test <- dataq10 %>%
  filter(year >=2015) %>% 
  select(- year, -day)
test <- as.data.frame(test)
head(test)

# --------------------------------------------------------------------------- #
## breaking up model into training and validation set 
set.seed(73)
# 70/30 split
split <- round(nrow(train_validation) * 0.7)
# Create training set (starting from 1963 until 1999)
train <- train_validation[1:split, ]
# Create validation set (starting from 1999 until end 2014)
validation <- train_validation[(split+1):nrow(train_validation), ]

# dropping year and filling NA with 0
drops2 <- c("year")
train <- train[ , !(names(train) %in% drops2)]
validation <- validation[ , !(names(validation) %in% drops2)]
# set up is done: now continue with building the models

# --------------------------------------------------------------------------- #
# code used for evaluating model performance
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square)
}

#### -------------------- 1. LASSO/RIDGE model ------------------------------- ####
## fit lasso/ridge model 
# then we evaluate model in validation set using RMSE as measurement of error
# method "none" in trainControl will fit model in entire training set
# we will train different models with different hyperparameters in training set and 
# choose te best model based on validation set performance

#install.packages("caret")
library(caret)

# 3 different LASSO models on training set 

lasso1 <- train(lead.returns ~ . - permno - date -lead.month, data = train,
                method = "glmnet",family = "gaussian", 
                tuneGrid = expand.grid(alpha=1, lambda=0.02), 
                trControl= trainControl(method="none"))

lasso2 <- train(lead.returns ~ . - permno - date -lead.month, data = train, 
                method = "glmnet",family = "gaussian", 
                tuneGrid = expand.grid(alpha=1, lambda=0.012),  #best parameter
                trControl= trainControl(method="none"))

lasso3 <- train(lead.returns ~ . - permno - date -lead.month, data = train,
                method = "glmnet",family = "gaussian", 
                tuneGrid = expand.grid(alpha=1, lambda=0.5),
                trControl= trainControl(method="none"))

## training performance rmse of Lasso's
#lasso 1
predlasso1 <- predict(lasso1, train)
rmselasso1_trn <- eval_results(train$lead.returns, predlasso1, train)
rmselasso1_trn

#lasso 2
predlasso2 <- predict(lasso2, train)
rmselasso2_trn <- eval_results(train$lead.returns, predlasso2, train)
rmselasso2_trn

#lasso 3
predlasso3 <- predict(lasso3, train)
rmselasso3_trn <- eval_results(train$lead.returns, predlasso3, train)
rmselasso3_trn

## Validation performance rmse of Lasso's
#lasso 1
predlasso1 <- predict(lasso1, validation)
rmselasso1 <- eval_results(validation$lead.returns, predlasso1, validation)
rmselasso1

#lasso 2
predlasso2 <- predict(lasso2, validation)
rmselasso2 <- eval_results(validation$lead.returns, predlasso2, validation)
rmselasso2

#lasso 3
predlasso3 <- predict(lasso3, validation)
rmselasso3 <- eval_results(validation$lead.returns, predlasso3, validation)
rmselasso3


# 3 different RIDGE models 
ridge1 <- train(lead.returns ~ . - permno - date -lead.month, data = train,
                method = "glmnet",family = "gaussian", 
                tuneGrid= expand.grid(alpha=0, lambda=0.001), 
                trControl= trainControl(method="none"))

ridge2 <- train(lead.returns ~ . - permno - date -lead.month, data = train,
                method = "glmnet",family = "gaussian", 
                tuneGrid=  expand.grid(alpha=0, lambda=0.07), # best parameter
                trControl= trainControl(method="none"))

ridge3 <- train(lead.returns ~ . - permno - date -lead.month, data = train,
                method = "glmnet",family = "gaussian", 
                tuneGrid= expand.grid(alpha=0, lambda=0.09),
                trControl= trainControl(method="none"))

## training performance rmse of Ridge
#Ridge 1
predridge1 <- predict(ridge1, train)
rmseridge1_trn <- eval_results(train$lead.returns, predridge1, train)
rmseridge1_trn

#Ridge 2
predridge2 <- predict(ridge2, train)
rmseridge2_trn <- eval_results(train$lead.returns, predridge2, train)
rmseridge2_trn 

#Ridge 3
predridge3 <- predict(ridge3, train)
rmseridge3_trn <- eval_results(train$lead.returns, predridge3, train)
rmseridge3_trn

## Validation performance rmse of Ridge
#Ridge 1
predridge1 <- predict(ridge1, validation)
rmseridge1 <- eval_results(validation$lead.returns, predridge1, validation)
rmseridge1

#Ridge 2
predridge2 <- predict(ridge2, validation)
rmseridge2 <- eval_results(validation$lead.returns, predridge2, validation)
rmseridge2 

#Ridge 3
predridge3 <- predict(ridge3, validation)
rmseridge3 <- eval_results(validation$lead.returns, predridge3, validation)
rmseridge3

# designing the table 
df_Q10 <- as.data.frame(matrix(nrow=8, ncol = 6))
colnames(df_Q10)[1] <- "Model name"
colnames(df_Q10)[2] <- "Best obtained tuning parameter"
colnames(df_Q10)[3] <- "RMSE Validation"
colnames(df_Q10)[4] <- "R-squared Validation"
colnames(df_Q10)[5] <- "RMSE training"
colnames(df_Q10)[6] <- "R-squared training"
df_Q10[1,1] <- "LASSO"
df_Q10[2,1] <- "Ridge"
df_Q10[3,1] <- "PLS"
df_Q10[4,1] <- "PCR"
df_Q10[5,1] <- "GBM"
df_Q10[6,1] <- "GBM"
df_Q10[7,1] <- "GBM"
df_Q10[8,1] <- "GBM"

# storing best LASSO and Ridge model 

# tuning parameters
df_Q10[1,2] <- "lambda = 0.012"
df_Q10[2,2] <- "lambda = 0.07"
# RMSE validation
df_Q10[1,3] <- rmselasso1[1]
df_Q10[2,3] <-rmseridge2[1]
# R-squared validation
df_Q10[1,4] <- rmselasso1[2]
df_Q10[2,4] <-rmseridge2[2]
# RMSE training
df_Q10[1,5] <- rmselasso1_trn[1]
df_Q10[2,5] <-rmseridge2_trn[1]
# R-squared training
df_Q10[1,6] <- rmselasso1_trn[2]
df_Q10[2,6] <-rmseridge2_trn[2]

#### -------------------- 2. PLS model ------------------------------- ####

# fitting the pls model (very poor model)
pls.fit <- train(lead.returns ~ . - permno - date,      
                 data = train,
                 method = "pls",
                 tuneGrid= expand.grid(ncomp = 1), # we tried a lot of models -> 1 is optimal!
                 trControl = trainControl(method="none"))

## training rmse of PLS
plspred1 <- predict(pls.fit, train)
rmsepls1_trn <- eval_results(train$lead.returns, plspred1, train)
rmsepls1_trn

## Validation rmse of PLS
plspred1 <- predict(pls.fit, validation)
rmsepls1 <- eval_results(validation$lead.returns, plspred1, validation)
rmsepls1

# fitting the pcr model
pcr.fit <- train(lead.returns ~ . - permno - date,      
                 data = train,
                 method = "pcr",
                 tuneGrid= expand.grid(ncomp = 1), # we tried a lot of models -> 1 is optimal!
                 trControl = trainControl(method="none"))

## train rmse of PCR
pcrpred1= predict(pcr.fit, train)
rmsepcr1_trn <- eval_results(train$lead.returns, pcrpred1, train)
rmsepcr1_trn

## Validation rmse of PCR
pcrpred1= predict(pcr.fit, validation)
rmsepcr1 <- eval_results(validation$lead.returns, pcrpred1, validation)
rmsepcr1

# storing best PLS and PCR models
df_Q10[3,2] <- "ncomp = 1"
df_Q10[4,2] <- "ncomp = 1"
# RMSE validation
df_Q10[3,3] <- rmsepls1[1]
df_Q10[4,3] <-rmsepcr1[1]
# R-squared validation 
df_Q10[3,4] <- rmsepls1[2]
df_Q10[4,4] <-rmsepcr1[2]
# RMSE training
df_Q10[3,5] <- rmsepls1_trn[1]
df_Q10[4,5] <-rmsepcr1_trn[1]
# R-squared training
df_Q10[3,6] <- rmsepls1_trn[2]
df_Q10[4,6] <-rmsepcr1_trn[2]

#### -------------------- 3. Tree-based models ------------------------------- ####

### 2. GBM model

## 2 simple GBM model
library(gbm)
gbm.fit <- train(lead.returns ~ . - permno - date,      
                 data = train,
                 method = "gbm",
                 tuneGrid = expand.grid(n.trees = 400, interaction.depth = 2,
                                        shrinkage=0.01,
                                        n.minobsinnode=5),
                 trControl = trainControl(method="none"))

# after n.trees = 400 not a lot of improvement -> n.tree = 400
# interaction.depth should be 2
# shrinkage = 0.01
# n.minobsinnode = 5

gbmpred1 <- predict(gbm.fit, train)
rmsegbm1_trn <- eval_results(train$lead.returns, gbmpred1, train)
rmsegbm1_trn

## Validation rmse of GBM
gbmpred1 <- predict(gbm.fit, validation)
rmsegbm1 <- eval_results(validation$lead.returns, gbmpred1, validation)
rmsegbm1

summary(gbm.fit)

# storing again
df_Q10[5,2] <- "n.trees = 400"
df_Q10[6,2] <- "interaction.depth = 2"
df_Q10[7,2] <- "shrinkage = 0.01"
df_Q10[8,2] <- "n.minobsinnode = 5"
df_Q10[5,3] <- rmsegbm1[1]
df_Q10[5,4] <- rmsegbm1[2]
df_Q10[5,5] <- rmsegbm1_trn[1]
df_Q10[5,6] <- rmsegbm1_trn[2]
###### Question 11 ######

df_Q10

###### Question 12 ######

# we chose ridge3 model (insert variable importance plot):

plot(varImp(gbm.fit))  

###### Question 13 ######

# we take our best model and predict out-of-sample returns (ridge)
testing<- data.matrix(test)
testing <- testing[,-c(1)]
head(testing)

# fitting the best model: GBM
testgbm <- train(lead.returns ~ . - permno-lead.month,      
                 data = testing,
                 method = "gbm",
                 tuneGrid = expand.grid(n.trees = 400, interaction.depth = 2,
                                        shrinkage=0.01,
                                        n.minobsinnode=5),
                 trControl = trainControl(method="none"))

gbmpredictions <- predict(testgbm, testing)
testing <- cbind(testing,gbmpredictions)
head(testing)

# convert to panel
testing <- as.data.frame(testing)
gbmtestpanel <- dcast(testing, lead.month~permno, value.var="gbmpredictions")
gbmtestpanel[,2:4731][is.na(gbmtestpanel[2:4731])] <- 0

# create portfolio on stocks that performed >1% on average (monthly)
# drop if colmeans < 0
newdf <- ifelse(colMeans(gbmtestpanel)>0.01,gbmtestpanel,NA)
newdf <- as.data.frame(newdf)
newdf <- newdf[ , colSums(is.na(newdf)) == 0]
ew_returnsmonthly <- as.vector(rowMeans(newdf[2:36]))
cum_ew_monthly <- ew_returnsmonthly + 1
cum_ew_monthly <- cumprod(cum_ew_monthly)
plot(cum_ew_monthly,type="l",col="red")

# calculate numbers for Sharpe Ratio
newdf <- as.matrix(newdf[2:36])
volatility_ew <-colSds(newdf)
volatility_ew <- mean(volatility_ew)
exp_return_ew <- mean(ew_returnsmonthly)-r.free
ew_graphpoint <- cbind(exp_return_ew, volatility_ew)

# putting it in the efficient frontier plot
plot(ef)
plot(ef, plot.assets=TRUE, col="steelblue", pch=20)
points(gmin.port$sd, gmin.port$er, col="black", pch=16, cex=2)
points(bestport$sd, bestport$er, col="red", pch=16, cex=2)
points(ew_graphpoint[2],ew_graphpoint[1], col = "darkgreen", pch=16, cex=2)
text(ew_graphpoint[2],ew_graphpoint[1], labels="GBM", pos=3)
text(gmin.port$sd, gmin.port$er, labels="GMV", pos=2)
text(bestport$sd, bestport$er, labels="TANGENCY", pos=2)
sr.tan = (bestport$er - r.free)/bestport$sd
abline(a=r.free, b=sr.tan, col="black", lwd=2)

# compute annual SR of ew lasso portfolio
sr_gbm_ew <- sqrt(12)*(ew_graphpoint[1]/ew_graphpoint[2])

# putting it in the Sharpe Ratio plot
plot(sr_annual, main = "Sharpe Ratio Comparison", xlab = "Portfolio", ylab = "Annual Sharpe Ratio", ylim = c(-2.5,4))
points(x = 50, y = is_optimal_sr, col = "red", pch = 16)
text(x = 50, y = 3, labels="Tangency portfolio (OW)", pos=4)
points(x = 40, y = sr_gbm_ew, col = "darkgreen", pch = 16)
text(x = 40, y = 1.4, labels="GBM portfolio (EW)", pos=4)
points(x = 30, y = df_Q4[2,3], col = "gold", pch = 16)
text(x = 30, y = 1.15,labels= "Market portfolio", pos=2)

# plotting cumulative performance
# for market 
q13cummarket <- df_Q2$`market factor`[c(619:678)]
plot(q13cummarket, type = "l")

# for tangency portfolio (cum performance over last 5 years)
optimalweights3 <- do.call("rbind", replicate(59, optimalweights, simplify = FALSE))
optweight_ret <- optimalweights3*excessreturn2[-c(1)]
totaloptweight_ret <- rowSums(optweight_ret,na.rm=T)
totaloptweight_ret_cum <- totaloptweight_ret+1
totaloptweight_ret_cum2 <- cumprod(totaloptweight_ret_cum)*100
plot(totaloptweight_ret_cum2, type= "l")

# storing the 3 lines in dataframe to obtain final plot
df_Q13 <- cbind(cum_ew_monthly[2:60]*100,q13cummarket[2:60],totaloptweight_ret_cum2)
ts_Q13 <- xts(df_Q13, order.by = seq(as.Date("2015-02-01"), 
                                     length.out=nrow(df_Q13), by = "month"), df_Q13)

plot.xts(ts_Q13, main="cumulative returns (%)", yaxis.right=FALSE)
addLegend("topleft", 
          legend.names=c("GBM EW portfolio", "Market weighted portfolio", 
                         "Optimal weighted portfolio"), 
          lty=c(1,1,1),lwd=c(2,2,2), ncol=1,bg="white", bty="o")

###### Question 14 ######

# create dataframe with all needed information (df_Q2 & ff3)
df_Q14 <- ff3[c(620:678),]
df_Q14 <- df_Q14[,-8]
df_Q14 <- cbind(df_Q14,ew_returnsmonthly[1:59],totaloptweight_ret)

# model 1: regressing lasso prediction model on FF 5 factors
#1. subtract risk-free rate from lasso predicted return portfolio
df_Q14$ew_returnsmonthly <- df_Q14$ew_returnsmonthly - df_Q14$rf
#2. create the linear model
linearmodel1 <- lm(ew_returnsmonthly~smb+hml+rmw+cma+`excess market ret`, data = df_Q14)
summary(linearmodel1)
# we find a significant SMB factor: our best performing stocks have a high loading on SMB

# model 2: regressing optimized portfolio on FF 5 factors
linearmodel2 <- lm(totaloptweight_ret~smb+hml+rmw+cma+`excess market ret`, data = df_Q14)
summary(linearmodel2)

#### 3. extra question:
# Calculate the statistical power you have in rejecting the null of a
# zero intercept against the alternative of a positive alpha.

## 3.1 statistical power in rejecting alpha = 0 for linearmodel1 
# For H1: Alpha > 0
res1 <- summary(linearmodel1)
pt(coef(res1)[1,3], linearmodel1$df, lower = FALSE)

      # --> Using a significance level of 1%, we REJECT the null hypothesis 
        # of a zero intercept against the alternative of a positive alpha.

## 3.2 statistical power in rejecting alpha = 0 for linearmodel2 
# For H1: Alpha > 0
res2 <- summary(linearmodel2)
pt(coef(res2)[1,3], linearmodel2$df, lower = FALSE)

      # --> Using a significance level of 1%, we do not reject the null hypothesis 
        # of a zero intercept against the alternative of a positive alpha.









