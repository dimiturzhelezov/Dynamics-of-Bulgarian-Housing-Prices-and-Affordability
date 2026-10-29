#This is my Econometrics project.
#I will use and ARDL model to model HAI(Housing Affordability Index) against several variables. The idea is to see how
#affordability is affected by housing and macroeconomic variables on country level.
#The second set of models will test the cointegration relationship of House Prices per square meter
#against net salaries per major region of Bulgaria - HAI1_Sofia, HAI1_Varna, HAI1_Plovdiv, HAI1_Burgas, HAI1_Ruse, Stara Zagora
#using two panel cointegraion models - Mean Group and Pooled Mean Group models


#First, I have to load the Stata files in R:
#install.packages("haven") #This package is used to read the Stata .dta format datasets
library("haven")

#Before that, I should clear the loaded datasets from the exercizes
rm(list=ls())

#Now let's load the time series data
ts = read_dta("data/dataset1.dta")

#I need to transform my time variable
attr(ts$tq, "format.stata") #Shows my time variable has quarterly periodicity

class(ts$tq) #numeric

head(ts$tq)

#zoo helps transform irregular indexed variables, in my case my quarterly time variable
library("zoo")

ts_q= as.numeric(ts$tq)

ts$tq = as.yearqtr(ts_q/4 + 1960) 

#Check to see if this worked
str(ts$tq)
#It now shows that my variable start from 1995Q1, since that is my first period of observation

#First, we should plot the variables of interest, in our case:
library("tidyverse")

#Let's create a copy of our data, in case we need to change 
ts1 = ts

#Before the graph, I will add from my Excel the HAIs of the cities, since they are not in my current data set
ts_HAIs = read.csv("data/Project.csv", sep = ";", stringsAsFactors = FALSE)
#
ts_HAIs$tq = as.yearqtr(ts_HAIs$tq)

#left join the two
ts1 = left_join(ts1, ts_HAIs, by = "tq")

#rename the variables
ts1 = ts1 %>% rename(HAI1_Sofia_hh = Sofia..Capital....household)
ts1 = ts1 %>% rename(HAI1_Burgas_hh = Burgas...household)
ts1 = ts1 %>% rename(HAI1_Varna_hh = Varna...household)
ts1 = ts1 %>% rename(HAI1_Stara_Zagora_hh = Stara.Zagora...household)
ts1 = ts1 %>% rename(HAI1_Ruse_hh = Ruse...household)
ts1 = ts1 %>% rename(HAI1_Plovdiv_hh = Plovdiv...household)
#
ts1 = ts1 %>% rename(HAI1_Sofia = Sofia..Capital.)
ts1 = ts1 %>% rename(HAI1_Burgas = Burgas)
ts1 = ts1 %>% rename(HAI1_Varna = Varna)
ts1 = ts1 %>% rename(HAI1_Stara_Zagora = Stara.Zagora)
ts1 = ts1 %>% rename(HAI1_Ruse = Ruse)
ts1 = ts1 %>% rename(HAI1_Plovdiv = Plovdiv)


str(ts1$HAI1_Burgas)
str(ts1$HAI1_Varna)
str(ts1$HAI1_Stara_Zagora)
str(ts1$HAI1_Plovdiv)
str(ts1$HAI1_Ruse)
str(ts1$HAI1_capita)
#The variables are numeric

#Now let's graph them
ts1_long = ts1 %>%
  pivot_longer(cols = c(HAI1_Burgas, HAI1_Varna, HAI1_Sofia, HAI1_Ruse, HAI1_Stara_Zagora, HAI1_Plovdiv, HAI1_capita),
               names_to = "variable",
               values_to = "value") %>% filter(tq >= as.yearqtr("2010 Q1"))   # keep only 2010Q1+

ts1_long = ts1_long[ts1_long$tq >= as.yearqtr("2010 Q1"), ]


ggplot(ts1_long, aes(x = tq, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black") +
  labs(title = "HAIs capita",
       x = "Quarter",
       y = "Value",
       color = "Variable") +
  theme_minimal() +
  theme(legend.position = "bottom")

#We can also plot the rest of our variables we will use for our model:
#CCI - Construction Cost Index,
#Employment Rate,
#end-of-quarter Mortgage Rate; disclaimer - HAI uses weighted average Mortgage Rate
#LTIR - Long-Term Interest Rate
#CNRB = Construction of New Buildings - Number of residential buildings
#These are the original variables I used in the model
ts4_long = ts1 %>%
  pivot_longer(cols = c(ln_HAI1_capita,
                        ln_HAI2_capita, 
                        ln_Employ_Rate, 
                        ln_Mortgage_IR, 
                        ln_LTIR,
                        ln_Constr_cost_index,
                        lnConst_new_build_num_res_dwells),
               names_to = "variable",
               values_to = "value") %>% filter(tq >= as.yearqtr("2007 Q1"))   # keep only 2007Q1+

ts4_long = ts4_long[ts4_long$tq >= as.yearqtr("2007 Q1"), ]


ggplot(ts4_long, aes(x = tq, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "Variables of interest",
       x = "Quarter",
       y = "lnValue",
       color = "Variable") +
  theme_minimal() +
  theme(legend.position = "bottom")

#Plot the log of the variables separately to decide on a unit root test:

#HAI1_capita - Zivot Andrews
ggplot(ts1, aes(x = tq, y = ln_HAI1_capita ))+
  geom_line()

#HAI2_capita - Zivot Andrews
ggplot(ts1, aes(x = tq, y = ln_HAI2_capita ))+
  geom_line()

#Employment Rate - Zivot Andrews
ggplot(ts1, aes(x = tq, y = ln_Employ_Rate ))+
  geom_line()

#CCI - Zivot Andrews
ggplot(ts1, aes(x = tq, y = ln_Constr_cost_index ))+
  geom_line()

#Const_new_build_num_res_build - Check both
ggplot(ts1, aes(x = tq, y = ln_Const_new_build_num_res_build ))+
  geom_line()

#Mortgage Rate - Check both
ggplot(ts1, aes(x = tq, y = ln_Mortgage_IR ))+
  geom_line()

#LTIR - Zivot Andrews
ggplot(ts1, aes(x = tq, y = ln_LTIR ))+
  geom_line()


#At this point we can start our analysis by testing for stationarity

#We have to transform the variables in log form (as per the paper I am following), but I have
#already done that before, so let's check them:
str(ts1$ln_HAI1_capita) #numeric


#We test using ADF and Zivot Andrews unit root tests - ZA allow for structural breaks in the series,
#which we observe in several variables - HAI, LTIR, CCI, Employment Rate
#The rest do not exhibit a break, so an ADF will do just fine - CNRB, mortgage rate
library("urca")

#Zivot-Andrews Tests - lag=4 is specified where the series exhibit seasonality

#HAI1 - nonstationary
summary(ur.za(ts1$ln_HAI1_capita, "both", lag=4))

#HAI2 - nonstationary
summary(ur.za(ts1$ln_HAI2_capita, "both", lag=4))

#LTIR - nonstationary
summary(ur.za(ts1$ln_LTIR, "both"))

#CCI - nonstationary
summary(ur.za(ts1$ln_Constr_cost_index, "both"))

#Employment Rate - nonstationary 
summary(ur.za(ts1$ln_Employ_Rate, "both"))

#Unemployment Rate - nonstationary 
summary(ur.za(ts1$ln_Unemploy_Rate, "both", lag=4))

#Mortgage Rate - nonstationary
summary(ur.za(ts1$ln_Mortgage_IR, "both"))

#CNRB - nonstationary 
summary(ur.za(ts1$ln_Const_new_build_num_res_build, "both", lag = 4))


#ADF Tests

#CNRB - stationary
summary(ur.df(na.omit(ts1$ln_Const_new_build_num_res_build), type = c("trend"), selectlags = c("BIC")))

#Mortgage Rate - nonstationary
summary(ur.df(na.omit(ts1$ln_Mortgage_IR), type = c("trend"), selectlags = c("BIC")))

#As we can see, none of them are stationary, except ln_Const_new_build_num_res_build for ADF - unreliable


#Zivot-Andrews Tests for I(1) - we switch from trend to intercept, since differencing removes deterministic trends
#that are not persistent

#HAI1 - stationary at 1%
summary(ur.za(diff(ts1$ln_HAI1_capita), "intercept"))

#HAI2 - stationary at 1%
summary(ur.za(diff(ts1$ln_HAI2_capita), "intercept"))

#LTIR - stationary at 1%
summary(ur.za(diff(ts1$ln_LTIR), "intercept"))

#CCI - stationary at 1%
summary(ur.za(diff(ts1$ln_Constr_cost_index), "intercept"))

#Employment Rate - stationary at 1%
summary(ur.za(diff(ts1$ln_Employ_Rate), "intercept"))

#Unemployment Rate - stationary at 1%
summary(ur.za(diff(ts1$ln_Unemploy_Rate), "intercept"))

#Mortgage Rate - stationary at 1%
summary(ur.za(diff(ts1$ln_Mortgage_IR), "intercept"))

#CNRB - stationary at 1%
summary(ur.za(diff(ts1$ln_Const_new_build_num_res_build), "intercept"))

#First difference are all stationary for the ZA tests


#ADF Tests

#CNRB - stationary at 1%
summary(ur.df(na.omit(diff(ts1$ln_Const_new_build_num_res_build)), type = c("drift"), selectlags = c("BIC")))

#Mortgage Rate - stationary at 1%
summary(ur.df(na.omit(diff(ts1$ln_Mortgage_IR)), type = c("drift"), selectlags = c("BIC")))

#stationary for the first difference ADF test as well

#stationarity after first difference - let's check for I(1)
#is modelled well with ARDL - Autoregressive Distributed Lag cointegration model.
#It implements the bounds test (F & t statistics)
#to check cointegration relationships
#between the variables, regardless of whether they are I(0) or I(1) (But not I(>1))
#It allows for dynamics(lags) of independent variables in the long-run equation, 
#as well as for the short-term equation (error correction equation).

#let's check for correlations first

cor_table = ts1 %>% select(ln_HAI1_capita,
      ln_Unemploy_Rate,
      ln_Mortgage_IR,
      ln_LTIR,
      ln_Constr_cost_index,
      ln_Const_new_build_num_res_build)
cor_table

cor = cor(cor_table, use = "complete.obs")
cor


#High correlation is expected among such macroeconomic variables

#We need to test for cointegration - for ARDL, we need the bounds test

library(ARDL)
#
model1 = auto_ardl(ln_HAI1_capita ~ ln_Employ_Rate +
                   ln_Mortgage_IR +
                   ln_LTIR +
                   ln_Constr_cost_index +
                   ln_Const_new_build_num_res_build, 
                   data = ts1,
                   selection = "BIC",
                   max_order = 4)

#model1$top_orders
model1$top_orders
#
model200013 = model1$best_model
#
summary(model200013)

#Unrestricted Error Correction Model
uecm200013 = uecm(model200013)
#
summary(uecm200013)

#Bounds test for cointegration
bounds_f_test(model200013, case = 3)
#RECM - Short-run model
recm200013 = recm(uecm200013, case = 3)
summary(recm200013)
#ect=-0.407 -> error correction exists, each quarter corrects 40.7% of disequilibrium

library(lmtest)
bgtest(model200013, order = 4 )
#No serial correlation

bptest(model200013)
#No heteroskedasticity

library(tseries)

jarque.bera.test(model200013$residuals)
#Normally distributed residuals

shapiro.test(model200013$residuals)

#install.packages("strucchange")
library(strucchange)

cusum_test = efp(
  formula = ln_HAI1_capita ~ ln_Employ_Rate +
    ln_Mortgage_IR +
    ln_LTIR +
    ln_Constr_cost_index +
    ln_Const_new_build_num_res_build,
  data = ts1,
  type = "Rec-CUSUM"
)

plot(cusum_test)

mosum_test = efp(
  ln_HAI1_capita ~ ln_Employ_Rate +
    ln_Mortgage_IR +
    ln_LTIR +
    ln_Constr_cost_index +
    ln_Const_new_build_num_res_build,
  data = ts1,
  type = "Rec-MOSUM"
)

plot(mosum_test)

#The breaches in the first cusum test are short-lived, happening around covid - so expected.
#There is stability in the long-run, however, the model is prone to shocks, as there are
#many structural breaks in the parameters


library(car)
vif(model200013)

#High multicollinearity - expected, probably why there are insignificant variables


library(sandwich)

V_nw = NeweyWest(model200013, lag = 4, prewhite = FALSE, adjust = TRUE)

multipliers(model200013, type = "lr", vcov_matrix = V_nw)




