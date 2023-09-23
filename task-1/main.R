library(psych)
library(tidyverse)

# 1. Failo nuskaitymas
getwd()
setwd(file.path(getwd(), "task-1"))

# Failo nuskaitymas ir praleistų reikšmių užpildymas <Na>
data <- read.csv("data.csv", na.strings=c(""))

# 2. Teisingų kintamųjų reikšmių nustatymas
convert_to_numeric <- function(df) {

  format_number <- function(variable, pattern) {

    numeric_variable <- gsub(pattern, "", variable)
    
    numeric_variable <- gsub(",", "", numeric_variable)
    
    as.numeric(as.character(numeric_variable))
  }
  
  # 'Expenses' stulpelis
  df$Expenses <- format_number(df$Expenses, pattern = " Dollars")
  
  # 'Profit' stulpelis
  df$Revenue <- format_number(df$Revenue, pattern = "\\$|,")
  
  # 'Growth' stulpelis
  df$Growth <- format_number(df$Growth, pattern = "\\%")

  return(df) 
  
}

# 4. Praleistų reikšmių užpildymas

# 4.1. Faktinis užpildymas
fill_missing_states <- function(df) {
  states_and_cities <- list(
    "New York" = "NY",
    "Newport Beach" = "CA",
    "San Francisco" = "CA",
    "Chicago" = "IL",
    "Alpharetta" = "GA"
  )
  
  for(city in names(states_and_cities)) {
    df$State[is.na(df$State) & df$City == city] <- states_and_cities[[city]]
  }
  
  return(df)
}

# 4.2. Užpildymas naudojant mediana
median_imputation_filling <- function(df) {

  industries <- unique(df$Industry)

  metrics <- c("Employees", "Revenue", "Expenses", "Growth")
  
  for (industry in industries) {
    for(metric in metrics) {
      median_val <- median(df[df$Industry==industry, metric], na.rm = TRUE)
      df[is.na(df[[metric]]) & df$Industry==industry, metric] <- median_val
    }
  }
  
  return(df)
  
}

adjust_profit <- function(df) {
  
  df[is.na(df$Profit), "Profit"]<-df[is.na(fin$Profit), "Revenue"] - 
    df[is.na(df$Profit), "Expenses"]
  
  return(df)
}

adjust_expenses <- function(df) {
  
  df[is.na(df$Expenses), "Expenses"]<-df[is.na(fin$Expenses), "Revenue"] - 
    df[is.na(df$Expenses), "Profit"]
  
  return(df)
}



fin <- data
fin <- convert_to_numeric(fin)

head(fin, 5)
str(fin)
summary(fin)

empty_values_test <- mapply(anyNA, fin)
empty_values_test

data[!complete.cases(data),]

fin <- fill_missing_states(fin)
fin <- median_imputation_filling(fin)
fin <- adjust_profit(fin)
fin <- adjust_expenses(fin)

empty_values_test <- mapply(anyNA, fin)
empty_values_test



