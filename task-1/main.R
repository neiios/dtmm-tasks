library(psych)
library(tidyverse)

# 1. Failo nuskaitymas

getwd()

setwd(file.path(getwd(), "task-1"))

# Failo nuskaitymas ir praleistų reikšmių užpildymas <Na>
data <- read.csv("data.csv", na.strings=c(""))

# 2. Teisingų kintamųjų reikšmių nustatymas

convert_to_numeric <- function(variable, pattern){
  # Nereikalingų simbolių pašalinimas
  numeric_variable <- gsub(pattern, "", variable)
  # Konvertavimas į skaitinę reikšmę
  as.numeric(as.character(numeric_variable))
}

# 'Expenses' stulpelis
data$Expenses <- convert_to_numeric(data$Expenses, pattern = " Dollars|,")

# 'Profit' stulpelis
data$Revenue <- convert_to_numeric(data$Revenue, pattern = "\\$|,")

# 'Growth' stulpelis
data$Growth <- convert_to_numeric(data$Growth, pattern = "\\%")

# 3. Aprašomoji statistika ir duomenų priešanalizė

head(data, 5)

str(data)

summary(data)

# 4. praleistų reikšmių užpildymas
