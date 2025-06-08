
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
if (!require("strucchange")) {
  install.packages("strucchange")
  library(strucchange)
}


if (!require(car)) {
  install.packages("car")
  library(car)
}

if (!require("corrplot")) {
  install.packages("corrplot")
  library(corrplot)
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

# CHOOSING INDEPENDENT VARIABLES
X_vars <- data[, c("Unemployment", "Pensions", "Healthcare", "Budget_Balance", 
                   "New_Housing", "Industry_Orders_mm", 
                   "Current_Consumer_Confidence_Indicator", "Trade_Balance")]

# dependent variable
Y <- data$HICP_mm

# Calculating correlations
correlations <- sapply(X_vars, function(x) cor(x, Y, use = "complete.obs"))

# Abbreviated variable names (adapted to the chart)
short_names <- c("Unempl.", "Pensions", "Health", "Budget", "New_Hous", 
                 "Ind_Orders", "Confidence", "Trade")

# Creating a chart
barplot(correlations,
        names.arg = short_names,
        main = "Correlation with HICP_mm",
        ylab = "Correlation Coefficient",
        col = "skyblue",
        las = 2)



# Zmienna zależna i niezależne
vars_all <- data[, c("HICP_mm", "Unemployment", "Pensions", "Healthcare", 
                     "Budget_Balance", "New_Housing", "Industry_Orders_mm", 
                     "Current_Consumer_Confidence_Indicator", "Trade_Balance")]

# Calculating the correlation matrix
cor_matrix <- cor(vars_all, use = "complete.obs")

# Shortened names
short_names_all <- c("HICP_mm", "Unempl.", "Pensions", "Health", 
                     "Budget", "New_Hous", "Ind_Orders", "Confidence", "Trade")

# Setting short names as kolnames and rownames
colnames(cor_matrix) <- short_names_all
rownames(cor_matrix) <- short_names_all

# Correlation chart
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         number.cex = 0.7)














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

coeftest(ecm_model, vcov = vcovHC(ecm_model, type = "HC1"))

tseries::jarque.bera.test(residuals(ecm_model))

library(strucchange)
sctest(ecm_model, type = "CUSUM")

# VIF
vif_values <- vif(ecm_model)

# PRINT RESULTS
print(vif_values)







