# install.packages("psych")
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("Hmisc")
# install.packages("stringi")

library("psych")
library("tidyverse")
library("Hmisc")
library("corrplot")
library("xtable")
library("stargazer")
library("ggplot2")

getwd()
setwd(file.path(getwd(), "task-1"))

source("utils.R")

original_data <- read.csv("data.csv", na.strings = c(""))

# Duomenų priešanalizė 
fin <- original_data
fin <- convert_to_numeric(fin)

# Praleistų reikšmių identifikavimas
empty_values_test <- mapply(anyNA, fin)
empty_values_test

original_data[!complete.cases(original_data), ]

# Faktinis užpildymas
fin <- fill_missing_states(fin)
fin <- fin %>% # fill third value if other two are present
  mutate(
    Profit = ifelse(is.na(Profit) & !is.na(Revenue) & !is.na(Expenses), Revenue - Expenses, Profit),
    Revenue = ifelse(is.na(Revenue) & !is.na(Profit) & !is.na(Expenses), Profit + Expenses, Revenue),
    Expenses = ifelse(is.na(Expenses) & !is.na(Profit) & !is.na(Revenue), Revenue - Profit, Expenses)
  )

# Užpildymas mediana
fin <- median_imputation_filling(fin)

# Praleistų reikšmių pašalinimas
fin <- fin[!is.na(fin$Industry), ]
fin <- fin[!is.na(fin$Inception), ]
fin <- fin[, -1] # drop first column

# Praleistų reikšmių identifikavimas
mapply(anyNA, fin)
fin[!complete.cases(fin), ]

# Aprašomoji statistika 
head(fin, 5)
str(fin)
summary(select_if(fin, is.numeric))
apply(select_if(fin, is.numeric), 2, sd)
source("tex-tables.R")

# Išskirčių identifikavimas
source("outliers.R")

# Duomenų normavimas
min_max_normalization <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

zscore_normalization <- function(x) {
  return((x - mean(x)) / sd(x))
}

fin_min_max_norm <- fin
fin_min_max_norm[, 3:4] <- min_max_normalization(fin[, 4])
fin_min_max_norm[, 7:10] <- apply(fin[, 7:10], 2, min_max_normalization)

summary(select_if(fin_min_max_norm, is.numeric))
apply(select_if(fin_min_max_norm, is.numeric), 2, sd)

fin_zscore_norm <- fin
fin_zscore_norm[, 3:4] <- zscore_normalization(fin[, 4])
fin_zscore_norm[, 7:10] <- apply(fin[, 7:10], 2, zscore_normalization)

summary(select_if(fin_zscore_norm, is.numeric))
apply(select_if(fin_zscore_norm, is.numeric), 2, sd)

# Koreliacijos
df_numeric <- select_if(fin_zscore_norm, is.numeric)
rcorr(as.matrix(df_numeric))
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(df_numeric)

