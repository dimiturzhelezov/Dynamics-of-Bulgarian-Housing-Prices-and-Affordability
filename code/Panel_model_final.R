#Senior Thesis - Panel ARDL: house prices and average salaries
#6 cities + Total, quarterly 2010Q1-2024Q1

library("haven")
library("tidyverse")
library("zoo")
library("plm")
library("Westerlund")
library("ardlverse")
library("tseries")
library("strucchange")

rm(list = ls())
options(scipen = 999)


#Data
panel_data = read_dta("data/paneldataset.dta")



#City names from the Stata labels
panel_data = panel_data %>%
  mutate(city = as.character(as_factor(city)))



#Keep the quarters with both HP and salary
panel = panel_data %>%
  filter(!is.na(city)) %>%
  filter(!is.na(HP)) %>%
  filter(!is.na(AVG_Monthly_Salary)) %>%
  select(city, time, HP, AVG_Monthly_Salary) %>%
  arrange(city, time)



#time is the Stata quarter number (140 = 1995Q1), so we make a real quarter too
panel = panel %>%
  mutate(quarter = as.yearqtr(1960 + time/4),
         q = quarters(quarter))



panel = as.data.frame(panel)



#Every city should have 57 quarters
table(panel$city)



#pivot_longer for graphs
panel_long = panel %>%
  pivot_longer(cols = c(HP, AVG_Monthly_Salary),
               names_to = "series",
               values_to = "value")



#graph the plots
ggplot(panel_long, aes(x = as.Date(quarter), y = value, color = series)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ city) +
  labs(x = "Quarter", y = "BGN", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")



ggsave("output/figures/hp_salary_by_city.png", width = 9, height = 6)



#Differences for every city
panel_ur = panel %>%
  group_by(city) %>%
  mutate(d_HP = HP - dplyr::lag(HP),
         d_Salary = AVG_Monthly_Salary - dplyr::lag(AVG_Monthly_Salary)) %>%
  ungroup()



#Share of d_Salary explained by the quarter (shows the seasonality)
summary(lm(d_Salary ~ q, data = panel_ur))$r.squared



#Remove each city's average change in every quarter (same as using quarter dummies)
#d_Salary_sa - seasonally adjusted
panel_ur = panel_ur %>%
  group_by(city, q) %>%
  mutate(d_Salary_sa = d_Salary - mean(d_Salary, na.rm = TRUE)) %>%
  ungroup()



#Panel data format for the tests
panel_p = pdata.frame(as.data.frame(panel_ur), index = c("city", "time"))
panel_d = panel_p[!is.na(panel_p$d_HP), ]



#Cross-sectional dependence (Pesaran CD test)
#H0: the cities are not correlated with each other
#I test the differences, because trending levels can look correlated even when they are not
pcdtest(d_HP ~ 1, data = panel_d, test = "cd")
pcdtest(d_Salary_sa ~ 1, data = panel_d, test = "cd")
#Strong cross-sectional dependence between cities.
#IPS assumes independent cities, so I also run it on demeaned series as a robustness check



#Panel unit root tests (Im-Pesaran-Shin)
#Preferred over Levin-Lin-Chu because it allows each city its own autoregressive coefficient
#H0: all cities have a unit root
#H1: there exists at least one stationary city



#Levels (with trend)
purtest(HP ~ trend, data = panel_p, test = "ips", lags = "AIC", pmax = 4)
purtest(AVG_Monthly_Salary ~ trend, data = panel_p, test = "ips", lags = "AIC", pmax = 4)
#Series are non-stationary



#Differences (with intercept)
purtest(d_HP ~ 1, data = panel_d, test = "ips", lags = "AIC", pmax = 4)
purtest(d_Salary ~ 1, data = panel_d, test = "ips", lags = "AIC", pmax = 4)
#raw: fails due to seasonality
purtest(d_Salary_sa ~ 1, data = panel_d, test = "ips", lags = "AIC", pmax = 4)
#with quarter dummies - stationary
#Levels have a unit root, differences are stationary -> both I(1)



#Robustness: IPS on demeaned series (removes the common national movement)
panel_dm = panel_ur %>%
  group_by(time) %>%
  mutate(HP_dm = HP - mean(HP),
         Salary_dm = AVG_Monthly_Salary - mean(AVG_Monthly_Salary),
         d_HP_dm = d_HP - mean(d_HP),
         d_Salary_dm = d_Salary_sa - mean(d_Salary_sa)) %>%
  ungroup()



panel_dm_p = pdata.frame(as.data.frame(panel_dm), index = c("city", "time"))
panel_dm_d = panel_dm_p[!is.na(panel_dm_p$d_HP_dm), ]



purtest(HP_dm ~ trend, data = panel_dm_p, test = "ips", lags = "AIC", pmax = 4)
purtest(Salary_dm ~ trend, data = panel_dm_p, test = "ips", lags = "AIC", pmax = 4)
purtest(d_HP_dm ~ 1, data = panel_dm_d, test = "ips", lags = "AIC", pmax = 4)
purtest(d_Salary_dm ~ 1, data = panel_dm_d, test = "ips", lags = "AIC", pmax = 4)
#Both series are I(1), with and without demeaning



#Panel cointegration test (Westerlund, 2007)
#H0: no cointegration (no error correction)
#Gt, Ga: at least some cities cointegrated; Pt, Pa: all cities cointegrated

#Why I prefer Westerlund and not Pedroni (what I used in the original thesis):
#Pedroni tests the residuals of a static regression (HP on salary).
#Westerlund tests the error correction term directly - the same term
#that the PMG and MG models estimate. So the test matches the model.
#Pedroni assumes short-run and long-run dynamics follow the same pattern
#(common factor restriction). If this is not the case, Pedroni can miss cointegration.
#Westerlund does not need this assumption, and it also has bootstrap p-values that stay valid when the cities are
#correlated. Pedroni assumes the cities are independent, but my CD test shows they are not.
#The group-mean statistics (Gt, Ga) allow each city its own speed of
#adjustment, which matches the MG model chosen by the Hausman test.



#westerlund_test needs a time index that goes 1, 2, 3, ...
panel = panel %>%
  group_by(city) %>%
  mutate(t_numeric = row_number()) %>%
  ungroup() %>%
  as.data.frame()



#Bootstrap p-values, because the cities are correlated (CD test)
#1 lag, to match the dynamics of the ARDL models below
set.seed(123)
westerlund_lag1 = westerlund_test(data = panel,
                                  yvar = "HP",
                                  xvars = "AVG_Monthly_Salary",
                                  idvar = "city",
                                  timevar = "t_numeric",
                                  constant = TRUE,
                                  trend = FALSE,
                                  lags = 1,
                                  leads = 0,
                                  bootstrap = 1000)



westerlund_lag1$boot_pvals
#All 4 tests support the existence of cointegration



#Bounds test for every city (Pesaran, Shin & Smith, 2001)
#Error correction form: d_HP = constant + phi*l_HP + beta*l_Salary + short_run*d_Salary + error
#F-test: H0 = phi = beta = 0 (no long-run relationship)
#t-test: H0 = phi = 0 (no error correction)
#Case 3: unrestricted intercept, no trend; k = 1 regressor (salary)

pss_5 = pss_critical_values(k = 1, case = 3, level = "5%")
pss_5



panel_bounds = panel %>%
  group_by(city) %>%
  mutate(l_HP = dplyr::lag(HP),
         l_Salary = dplyr::lag(AVG_Monthly_Salary),
         d_HP = HP - l_HP,
         d_Salary = AVG_Monthly_Salary - l_Salary) %>%
  ungroup() %>%
  filter(!is.na(d_HP))



bounds_results = data.frame()



for (this_city in unique(panel_bounds$city)) {
  data_city = panel_bounds %>% filter(city == this_city)
  
  full_model = lm(d_HP ~ l_HP + l_Salary + d_Salary, data = data_city)
  restricted_model = lm(d_HP ~ d_Salary, data = data_city)
  
  F_stat = anova(restricted_model, full_model)$F[2]
  t_stat = summary(full_model)$coefficients["l_HP", "t value"]
  
  bounds_results = rbind(bounds_results,
                         data.frame(city = this_city,
                                    F_stat = F_stat,
                                    t_stat = t_stat,
                                    row.names = NULL))
}



bounds_results
#I compare with pss_5:
#F above the I(1) bound -> cointegration, below the I(0) bound -> no cointegration, in between -> inconclusive
#t more negative than the I(1) bound -> cointegration, above the I(0) bound -> no cointegration
#Result: F above the I(1) bound in all cities; t beyond the I(1) bound only in Plovdiv and Ruse
#-> cointegration in some, but not all, cities




#PMG and MG models
#d_HP = constant + ec*(l_HP - long_run*Salary) + short_run*d_Salary + error



#PMG - common long-run, city-specific short-run
pmg_model = panel_ardl(HP ~ AVG_Monthly_Salary,
                       data = panel,
                       id = "city",
                       time = "time",
                       p = 1,
                       q = 0,
                       estimator = "pmg",
                       maxiter = 1000,
                       tol = 1e-10)



summary(pmg_model)



#MG - everything city-specific, then averaged
mg_model = panel_ardl(HP ~ AVG_Monthly_Salary,
                      data = panel,
                      id = "city",
                      time = "time",
                      p = 1,
                      q = 0,
                      estimator = "mg")



summary(mg_model)



#Hausman: H0 = PMG is fine (MG goes first)
hausman_test(mg_model, pmg_model)
#Result: Chi2 = 36.2065, p = 0.000 -> MG is preferred



#Diagnostics on the PMG and MG residuals



#Data in the same order as the models: by city, then time, first quarter dropped
panel_ecm = panel %>%
  group_by(city) %>%
  mutate(l_HP = dplyr::lag(HP),
         d_HP = HP - l_HP,
         d_Salary = AVG_Monthly_Salary - dplyr::lag(AVG_Monthly_Salary)) %>%
  ungroup() %>%
  filter(!is.na(d_HP)) %>%
  as.data.frame()



cities = unique(panel_ecm$city)



#Residuals from the ardlverse models
panel_ecm$res_pmg = unlist(pmg_model$residuals)
panel_ecm$res_mg = unlist(mg_model$residuals)



#Check: residual + fitted should give back d_HP (residuals are in the right order)
all.equal(as.numeric(unlist(pmg_model$residuals) + unlist(pmg_model$fitted)), panel_ecm$d_HP)
all.equal(as.numeric(unlist(mg_model$residuals) + unlist(mg_model$fitted)), panel_ecm$d_HP)



#Tests per city
#Serial correlation: Breusch-Godfrey, 4 lags, H0 = no autocorrelation
#Heteroskedasticity: Breusch-Pagan, H0 = constant variance
#Normality: Jarque-Bera, H0 = normal residuals
#Stability: OLS-CUSUM, H0 = stable model
#Seasonality: residuals on quarter dummies, H0 = no seasonality left
city_tests = function(e, data_city) {
  n = length(e)
  X = data_city[, c("l_HP", "AVG_Monthly_Salary", "d_Salary")]
  
  #Breusch-Godfrey: regress residuals on the regressors and 4 of their own lags
  e_lags = sapply(1:4, function(k) c(rep(0, k), e[1:(n - k)]))
  bg_lm = n*summary(lm(e ~ as.matrix(X) + e_lags))$r.squared
  bg_p = 1 - pchisq(bg_lm, df = 4)
  
  #Breusch-Pagan: regress squared residuals on the regressors
  bp_lm = n*summary(lm(e^2 ~ as.matrix(X)))$r.squared
  bp_p = 1 - pchisq(bp_lm, df = ncol(X))
  
  #Jarque-Bera
  jb_p = jarque.bera.test(e)$p.value
  
  #OLS-CUSUM of the residuals
  cusum_p = unname(sctest(efp(e ~ 1, type = "OLS-CUSUM"))$p.value)
  
  #Seasonality
  seasonal_p = anova(lm(e ~ data_city$q))$"Pr(>F)"[1]
  
  c(serial_corr_p = bg_p, heterosk_p = bp_p, normality_p = jb_p,
    cusum_p = cusum_p, seasonality_p = seasonal_p)
}



#Run the tests for every city
diagnostics = function(residual_column) {
  results = data.frame()
  for (this_city in cities) {
    data_city = panel_ecm %>% filter(city == this_city)
    tests = city_tests(data_city[[residual_column]], data_city)
    results = rbind(results, data.frame(city = this_city, t(tests), row.names = NULL))
  }
  results
}



pmg_diagnostics = diagnostics("res_pmg")
pmg_diagnostics



mg_diagnostics = diagnostics("res_mg")
mg_diagnostics
#p < 0.05 means the problem is present
#Result: heteroskedasticity and non-normal residuals in most cities, stable (CUSUM),
#autocorrelation in Plovdiv and Sofia (MG), also Ruse and Stara Zagora (PMG),
#leftover seasonality in Plovdiv



#CUSUM plots (MG)
par(mfrow = c(2, 4))



for (this_city in cities) {
  e = panel_ecm$res_mg[panel_ecm$city == this_city]
  plot(efp(e ~ 1, type = "OLS-CUSUM"), main = paste("CUSUM -", this_city))
}



par(mfrow = c(1, 1))



#Cross-sectional dependence of the residuals (Pesaran CD test)
#H0: the residuals of different cities are not correlated
cd_test = function(residual_column) {
  residual_matrix = sapply(cities, function(this_city) panel_ecm[[residual_column]][panel_ecm$city == this_city])
  correlations = cor(residual_matrix)
  N = ncol(residual_matrix)
  T = nrow(residual_matrix)
  CD = sqrt(2 * T / (N * (N - 1))) * sum(correlations[upper.tri(correlations)])
  p = 2 * pnorm(-abs(CD))
  cat("CD =", round(CD, 3), "  p-value =", signif(p, 4), "\n")
  cat("Average correlation =", round(mean(correlations[upper.tri(correlations)]), 3), "\n")
}



cd_test("res_pmg")
cd_test("res_mg")
#Result: CD about 5, p < 0.001 -> residuals are still correlated across cities



#Residual plots
ggplot(panel_ecm, aes(x = as.Date(quarter), y = res_mg)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ city, scales = "free_y") +
  labs(x = "Quarter", y = "Residual", title = "MG residuals by city") +
  theme_minimal()


