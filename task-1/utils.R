convert_to_numeric <- function(df) {
    format_number <- function(variable, pattern) {
        numeric_variable <- gsub(pattern, "", variable)
        numeric_variable <- gsub(",", "", numeric_variable)
        as.numeric(as.character(numeric_variable))
    }

    df$Expenses <- format_number(df$Expenses, pattern = " Dollars")
    df$Revenue <- format_number(df$Revenue, pattern = "\\$|,")
    df$Growth <- format_number(df$Growth, pattern = "\\%")

    return(df)
}

fill_missing_states <- function(df) {
    states_and_cities <- list(
        "New York" = "NY",
        "Newport Beach" = "CA",
        "San Francisco" = "CA",
        "Chicago" = "IL",
        "Alpharetta" = "GA"
    )

    for (city in names(states_and_cities)) {
        df$State[is.na(df$State) & df$City == city] <- states_and_cities[[city]]
    }

    return(df)
}

median_imputation_filling <- function(df) {
    industries <- unique(df$Industry)

    metrics <- c("Employees", "Revenue", "Expenses", "Profit", "Growth")

    for (industry in industries) {
        for (metric in metrics) {
            median_val <- median(df[df$Industry == industry, metric], na.rm = TRUE)
            df[is.na(df[[metric]]) & df$Industry == industry, metric] <- median_val
        }
    }

    return(df)
}
