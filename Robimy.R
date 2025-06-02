
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


#data load
data <- read_excel("DATA.xlsx", sheet = "11BEZUK", col_names = FALSE)

# Transpozycja: zamieniamy wiersze z kolumnami
data_t <- as.data.frame(t(data))

# Pierwszy wiersz staje się nagłówkami
colnames(data_t) <- data_t[1, ]
data_t <- data_t[-1, ]

# Teraz mamy ramkę danych: w kolumnach kraje, w wierszach kolejne miesiące

# Dodaj kolumnę daty (np. z pierwszego wiersza)
data_t$TIME <- seq(as.Date("1996-01-01"), by = "month", length.out = nrow(data_t))


