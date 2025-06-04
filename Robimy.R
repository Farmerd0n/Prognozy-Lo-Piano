
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

plot(data$Unemployment , data$HICP_mm)
plot(data$Pensions , data$HICP_mm)
plot(data$Healthcare , data$HICP_mm)
plot(data$Budget_Balance , data$HICP_mm)
plot(data$New_Housing , data$HICP_mm)
plot(data$Industry_Orders_mm , data$HICP_mm)
plot(data$Current_Consumer_Confidence_Indicator , data$HICP_mm)
plot(data$Trade_Balance , data$HICP_mm)

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
model <- auto_ardl(HICP_mm ~ Pensions + Healthcare + New_Housing+Industry_Orders_mm+Unemployment+Budget_Balance+Current_Consumer_Confidence_Indicator+Trade_Balance, data = data_ts, max_order = 5)
model$top_orders
model<-model$best_model
summary(model)


bounds_f_test(model, case = 3)
ecm_model <- uecm(model)
summary(ecm_model)
# Test na autokoremodel# Test na autokorelację reszt (Breusch-Godfrey)
lmtest::bgtest(ecm_model)

# Test normalności (Jarque-Bera)
tseries::jarque.bera.test(residuals(ecm_model))

# Test heteroskedastyczności (Breusch-Pagan)
lmtest::bptest(ecm_model)

