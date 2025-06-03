
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



#data load
data <- read_excel("Data_F.xlsx", sheet = "Arkusz1")

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



