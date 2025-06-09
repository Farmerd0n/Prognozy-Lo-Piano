
#libraries
if (!require("readxl")) {
  install.packages("readxl")
  library(readxl)
}
if (!require("tidyr")) {
  install.packages("tidyr")
  library(tidyr)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("lmtest")) {
  install.packages("lmtest")
  library(lmtest)
}
if (!require("zoo")) {
  install.packages("zoo")
  library(zoo)
}
if (!require("tseries")) {
install.packages("tseries")
library(tseries)
}
if (!require("ARDL")) {
install.packages("ARDL")
library(ARDL)
}
if (!require("dLagM")) {
  install.packages("dLagM")
  library(dLagM)
}
if (!require("strucchange")) {
  install.packages("strucchange")
  library(strucchange)
}
if (!require("dynlm")) {
  install.packages("dynlm")
  library(dynlm)
}
if (!require("car")) {
  install.packages("car")
  library(car)
}
if (!require("sandwich")) {
  install.packages("sandwich")
  library(sandwich)
}


# data load
data <- read_excel("Data_F.xlsx", sheet = "Arkusz1")
#WAŻNE ZMNIESZAM O 1 CAŁOŚĆ HICP_mm
data$HICP_mm <- data$HICP_mm - 1
# data_typ
str(data$TIME)
data$TIME <- as.Date(paste0(data$TIME, "-01"))
data_ts <- zoo(data[, -1], order.by = as.Date(data$TIME))

# data vis

ggplot(data, aes(x = TIME, y = HICP_mm)) +
  geom_line() +                 # linia pokazująca trend w czasie
  labs(title = "Values of HICP_mm over time",
       x = "Time",
       y = "HICP_mm") +
  theme_minimal()


ggplot(data, aes(x = Unemployment, y = HICP_mm)) +
  geom_point() +
  ggtitle("Unemployment vs HICP") +
  theme_minimal()

ggplot(data, aes(x = Pensions, y = HICP_mm)) +
  geom_point() +
  ggtitle("Pensions vs HICP") +
  theme_minimal()

ggplot(data, aes(x = Healthcare, y = HICP_mm)) +
  geom_point() +
  ggtitle("Healthcare vs HICP") +
  theme_minimal()

ggplot(data, aes(x = Budget_Balance, y = HICP_mm)) +
  geom_point() +
  ggtitle("Budget Balance vs HICP") +
  theme_minimal()

ggplot(data, aes(x = New_Housing, y = HICP_mm)) +
  geom_point() +
  ggtitle("New Housing vs HICP") +
  theme_minimal()

ggplot(data, aes(x = Industry_Orders_mm, y = HICP_mm)) +
  geom_point() +
  ggtitle("Industry Orders vs HICP") +
  theme_minimal()

ggplot(data, aes(x = Current_Consumer_Confidence_Indicator, y = HICP_mm)) +
  geom_point() +
  ggtitle("Consumer Confidence vs HICP") +
  theme_minimal()

ggplot(data, aes(x = Trade_Balance, y = HICP_mm)) +
  geom_point() +
  ggtitle("Trade Balance vs HICP") +
  theme_minimal()

# statistical tests
adf.test(data_ts$HICP_mm)
adf.test(data_ts$Unemployment) #nie stacjonarny
adf.test(data_ts$Pensions) #
adf.test(data_ts$Healthcare)
adf.test(data_ts$Budget_Balance) #nie stacjonarny
adf.test(data_ts$New_Housing)
adf.test(data_ts$Industry_Orders_mm)
adf.test(data_ts$Current_Consumer_Confidence_Indicator) #nie stacjonarny
adf.test(data_ts$Trade_Balance) #nie stacjonarny

# diff check for non stationary
adf.test(diff(data_ts$Unemployment))
adf.test(diff(data_ts$Budget_Balance))
adf.test(diff(data_ts$Current_Consumer_Confidence_Indicator))
adf.test(diff(data_ts$Trade_Balance))

# adding diff
diff_Unemployment <- diff(data_ts$Unemployment)
diff_Budget_Balance <- diff(data_ts$Budget_Balance)
diff_Current_Consumer_Confidence_Indicator <- diff(data_ts$Current_Consumer_Confidence_Indicator)
diff_Trade_Balance <- diff(data_ts$Trade_Balance)
data_trim <- data_ts[-1, ]
data_trim$diff_Unemployment <- diff_Unemployment
data_trim$diff_Budget_Balance <- diff_Budget_Balance
data_trim$diff_Current_Consumer_Confidence_Indicator <- diff_Current_Consumer_Confidence_Indicator
data_trim$diff_Trade_Balance  <- diff_Trade_Balance 
# model ARDL with diff on variables
model <- auto_ardl(HICP_mm ~ Pensions + Healthcare + New_Housing+Industry_Orders_mm+diff_Unemployment+diff_Budget_Balance+diff_Current_Consumer_Confidence_Indicator+diff_Trade_Balance, data = data_trim, max_order = 4)
# model ARDL without diff on variables
model <- auto_ardl(HICP_mm ~ Pensions + Healthcare + New_Housing+Industry_Orders_mm+Unemployment+Budget_Balance+Current_Consumer_Confidence_Indicator+Trade_Balance, data = data_ts, max_order = 5)
# ARDL and prep for ECM model
model_best<-model$best_model
summary(model_best)
data_df <- data[-c(1, 2), ]
fitted_values_mod1 <- fitted(model_best)[, "HICP_mm"]
fitted_df_mod1 <- data.frame(
  TIME = index(fitted_values_mod1),
  HICP_pred = coredata(fitted_values_mod1)
)
merged_df_mod1 <- merge(fitted_df_mod1, data_df[, c("TIME", "HICP_mm")], by = "TIME", all.x = TRUE)
ggplot(merged_df_mod1, aes(x = TIME)) +
  geom_line(aes(y = HICP_mm, color = "True Data"), size = 1) +
  geom_line(aes(y = HICP_pred, color = "ARDL"), size = 1) +
  labs(title = "HICP_mm vs. ARDL",
       x = "Time",
       y = "HICP_mm") +
  scale_color_manual(name = "Legenda",
                     values = c("True Data" = "blue", "ARDL" = "red")) +
  theme_minimal()


# Error Correction model
bounds_f_test(model_best, case = 3)
ecm_model <- uecm(model_best)
summary(ecm_model)
data_df <- data[-c(1, 2), ]
fitted_values <- fitted(ecm_model)[, "HICP_mm"]
fitted_df <- data.frame(
  TIME = index(fitted_values),      
  HICP_pred = coredata(fitted_values)  
)
merged_df <- merge(fitted_df, data_df[, c("TIME", "HICP_mm")], by = "TIME", all.x = TRUE)
ggplot(merged_df, aes(x = TIME)) +
  geom_line(aes(y = HICP_mm, color = "True Data"), size = 1) +
  geom_line(aes(y = HICP_pred, color = "ECM"), size = 1) +
  labs(title = "HICP_mm vs model ECM",
       x = "Time",
       y = "HICP_mm") +
  scale_color_manual(name = "Legend",
                     values = c("True Data" = "blue", "ECM" = "red")) +
  theme_minimal()

# ARDL without extreme p value variables
model_dynlm <- dynlm(HICP_mm ~ 
                        L(HICP_mm, 1) +   
                        Healthcare +     # Current
                        L(Pensions, 1) + # LAG 1
                        Unemployment + L(Unemployment, 1) + L(Unemployment, 2) + # Current+LAG 1,2 
                        Budget_Balance + L(Budget_Balance, 1) + L(Budget_Balance, 2) + # Current+LAG 1,2
                        Current_Consumer_Confidence_Indicator + # Current
                        L(New_Housing, 1) + # LAG 1
                        L(Trade_Balance, 1) + # LAG 1
                        L(Industry_Orders_mm, 1), # LAG 1
                      data = data_ts)

summary(model_dynlm)

# AIC and BIC 

AIC(model_dynlm) #BEST
AIC(model_best)
AIC(ecm_model)

BIC(model_dynlm) #BEST
BIC(model_best)
BIC(ecm_model)

# model_dynlm visualization

data_df <- data[-c(1, 2), ]
fitted_values_mod2 <- fitted(model_dynlm)[, "HICP_mm"]
fitted_df_mod2 <- data.frame(
  TIME = index(fitted_values_mod2),
  HICP_pred = coredata(fitted_values_mod2)
)
merged_df_mod2 <- merge(fitted_df_mod2, data_df[, c("TIME", "HICP_mm")], by = "TIME", all.x = TRUE)
ggplot(merged_df_mod2, aes(x = TIME)) +
  geom_line(aes(y = HICP_mm, color = "True Data"), size = 1) +
  geom_line(aes(y = HICP_pred, color = "DYNLM"), size = 1) +
  labs(title = "HICP_mm vs. DYNLM",
       x = "Time",
       y = "HICP_mm") +
  scale_color_manual(name = "Legenda",
                     values = c("True Data" = "blue", "DYNLM" = "red")) +
  theme_minimal()

# TESTS

dwtest(model_dynlm)  # Durbin-Watson
bgtest(model_dynlm, order=1)  # Breusch-Godfrey order 1
bptest(model_dynlm) # Breusch-Pagan - FAILED
jarque.bera.test(residuals(model_dynlm)) # Jarque-Bera - FAILED
sctest(model_dynlm, type = "OLS-CUSUM") #Stab
sctest(model_dynlm, type = "OLS-CUSUMQ") #Stab

# Correction for p-value and standard errors from heteroscedasciticty 
robust_se <- vcovHC(model_dynlm, type = "HC1") 
coeftest(model_dynlm, vcov = robust_se)

# VIF
vif(model_dynlm)

# FIXING UNEMPLOYMENT VIF - old one is still best

model_dynlmU0 <- dynlm(HICP_mm ~ 
                       L(HICP_mm, 1) +   
                       Healthcare +     # Current
                       L(Pensions, 1) + # LAG 1
                       Unemployment + # Current 
                       Budget_Balance + L(Budget_Balance, 1) + L(Budget_Balance, 2) + # Current+LAG 1,2
                       Current_Consumer_Confidence_Indicator + # Current
                       L(New_Housing, 1) + # LAG 1
                       L(Trade_Balance, 1) + # LAG 1
                       L(Industry_Orders_mm, 1), # LAG 1
                     data = data_ts)

summary(model_dynlmU0)

model_dynlmU1 <- dynlm(HICP_mm ~ 
                         L(HICP_mm, 1) +   
                         Healthcare +     # Current
                         L(Pensions, 1) + # LAG 1
                         L(Unemployment, 1) + # LAG 1 
                         Budget_Balance + L(Budget_Balance, 1) + L(Budget_Balance, 2) + # Current+LAG 1,2
                         Current_Consumer_Confidence_Indicator + # Current
                         L(New_Housing, 1) + # LAG 1
                         L(Trade_Balance, 1) + # LAG 1
                         L(Industry_Orders_mm, 1), # LAG 1
                       data = data_ts)

summary(model_dynlmU1)

model_dynlmU2 <- dynlm(HICP_mm ~ 
                         L(HICP_mm, 1) +   
                         Healthcare +     # Current
                         L(Pensions, 1) + # LAG 1
                         L(Unemployment, 2) + # LAG 2 
                         Budget_Balance + L(Budget_Balance, 1) + L(Budget_Balance, 2) + # Current+LAG 1,2
                         Current_Consumer_Confidence_Indicator + # Current
                         L(New_Housing, 1) + # LAG 1
                         L(Trade_Balance, 1) + # LAG 1
                         L(Industry_Orders_mm, 1), # LAG 1
                       data = data_ts)

summary(model_dynlmU2)

model_dynlmA1 <- dynlm(HICP_mm ~ 
                         L(HICP_mm, 1) +   
                         Healthcare +     # Current
                         L(Pensions, 1) + # LAG 1
                         L(Budget_Balance, 1) + L(Budget_Balance, 2) + # LAG 1,2
                         Current_Consumer_Confidence_Indicator + # Current
                         L(New_Housing, 1) + # LAG 1
                         L(Trade_Balance, 1) + # LAG 1
                         L(Industry_Orders_mm, 1), # LAG 1
                       data = data_ts)

summary(model_dynlmA1)

# AIC and BIC for best contenders

AIC(model_dynlm) #BEST
AIC(model_dynlmU0)
AIC(model_dynlmU1)
AIC(model_dynlmU2)
AIC(model_dynlmA1)

BIC(model_dynlm) 
BIC(model_dynlmU0)
BIC(model_dynlmU1)
BIC(model_dynlmU2)
BIC(model_dynlmA1) #BEST

summary(model_dynlm)
summary(model_dynlmA1)
