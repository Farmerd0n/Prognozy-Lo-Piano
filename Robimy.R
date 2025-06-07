
#libreria 
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

#data load
data <- read_excel("Data_F.xlsx", sheet = "Arkusz1")
#data_typ
str(data$TIME)
data$TIME <- as.Date(paste0(data$TIME, "-01"))
data_ts <- zoo(data[, -1], order.by = as.Date(data$TIME))

#data vis

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

#statistical tests
adf.test(data_ts$HICP_mm)
adf.test(data_ts$Unemployment) #nie stacjonarny
adf.test(data_ts$Pensions) #
adf.test(data_ts$Healthcare)
adf.test(data_ts$Budget_Balance) #nie stacjonarny
adf.test(data_ts$New_Housing)
adf.test(data_ts$Industry_Orders_mm)
adf.test(data_ts$Current_Consumer_Confidence_Indicator) #nie stacjonarny
adf.test(data_ts$Trade_Balance) #nie stacjonarny

#diff check for non stationary
adf.test(diff(data_ts$Unemployment))
adf.test(diff(data_ts$Budget_Balance))
adf.test(diff(data_ts$Current_Consumer_Confidence_Indicator))
adf.test(diff(data_ts$Trade_Balance))

#adding diff
diff_Unemployment <- diff(data_ts$Unemployment)
diff_Budget_Balance <- diff(data_ts$Budget_Balance)
diff_Current_Consumer_Confidence_Indicator <- diff(data_ts$Current_Consumer_Confidence_Indicator)
diff_Trade_Balance <- diff(data_ts$Trade_Balance)
data_trim <- data_ts[-1, ]
data_trim$diff_Unemployment <- diff_Unemployment
data_trim$diff_Budget_Balance <- diff_Budget_Balance
data_trim$diff_Current_Consumer_Confidence_Indicator <- diff_Current_Consumer_Confidence_Indicator
data_trim$diff_Trade_Balance  <- diff_Trade_Balance 
#model ARDL
model <- auto_ardl(HICP_mm ~ Pensions + Healthcare + New_Housing+Industry_Orders_mm+diff_Unemployment+diff_Budget_Balance+diff_Current_Consumer_Confidence_Indicator+diff_Trade_Balance, data = data_trim, max_order = 4)

summary(model)

summary(model$best_model)
lmtest::dwtest(model$best_model)
lmtest::bptest(model$best_model)
coeftest(model$best_model, vcov = vcovHC(model$best_model, type = "HC1"))

tseries::jarque.bera.test(residuals(model$best_model))

