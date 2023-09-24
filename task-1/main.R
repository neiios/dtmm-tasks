# install.packages("psych")
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("Hmisc")
# install.packages("stringi")

library("psych")
library("tidyverse")
library("Hmisc")
library("corrplot")

getwd()
setwd(file.path(getwd(), "task-1")) # TODO: doesn't work in rstudio?

source("utils.R")

original_data <- read.csv("data.csv", na.strings = c(""))

fin <- original_data
fin <- convert_to_numeric(fin)

head(fin, 5)
str(fin)
summary(fin)

empty_values_test <- mapply(anyNA, fin)
empty_values_test

original_data[!complete.cases(original_data), ]

fin <- fill_missing_states(fin)
fin <- median_imputation_filling(fin)
fin <- adjust_profit(fin)
fin <- adjust_expenses(fin)

fin <- fin[!is.na(fin$Industry), ]

empty_values_test <- mapply(anyNA, fin)
empty_values_test

plot_outliers_by_industry(fin, "Revenue")
identify_outliers_by_industry(fin, "Revenue")
fin <- remove_extreme_outliers_by_industry(fin, "Revenue")

plot_outliers_by_industry(fin, "Expenses")
identify_outliers_by_industry(fin, "Expenses")
fin <- remove_extreme_outliers_by_industry(fin, "Expenses")

plot_outliers_by_industry(fin, "Growth")
identify_outliers_by_industry(fin, "Growth")
fin <- remove_extreme_outliers_by_industry(fin, "Growth")

# NOTE: doesnt make much sense but at least we can test removal
# plot_outliers_by_industry(fin, "Inception")
# identify_outliers_by_industry(fin, "Inception")
# fin <- remove_extreme_outliers_by_industry(fin, "Inception")

min_max_normalization <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

zscore_normalization <- function(x) {
  return((x - mean(x)) / sd(x))
}

fin_norm <- fin
fin_norm[, 5] <- min_max_normalization(fin[, 5])
fin_norm[, 8:11] <- apply(fin[, 8:11], 2, min_max_normalization)

find_correlations(fin_norm)
