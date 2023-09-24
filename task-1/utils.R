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

    metrics <- c("Employees", "Revenue", "Expenses", "Growth")

    for (industry in industries) {
        for (metric in metrics) {
            median_val <- median(df[df$Industry == industry, metric], na.rm = TRUE)
            df[is.na(df[[metric]]) & df$Industry == industry, metric] <- median_val
        }
    }

    return(df)
}

adjust_profit <- function(df) {
    df[is.na(df$Profit), "Profit"] <- df[is.na(fin$Profit), "Revenue"] -
        df[is.na(df$Profit), "Expenses"]

    return(df)
}

adjust_expenses <- function(df) {
    df[is.na(df$Expenses), "Expenses"] <- df[is.na(fin$Expenses), "Revenue"] -
        df[is.na(df$Expenses), "Profit"]

    return(df)
}

plot_outliers_by_industry <- function(df, metric) {
    ggplot(df, aes_string(x = "Industry", y = metric)) +
        geom_boxplot(aes(color = Industry)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        guides(color = FALSE)
}

identify_outliers_by_industry <- function(df, metric) {
    outliers <- list()

    industries <- unique(df$Industry)

    for (industry in industries) {
        industry_df <- df[df$Industry == industry, ]

        Q1 <- quantile(industry_df[[metric]], 0.25, na.rm = TRUE)
        Q3 <- quantile(industry_df[[metric]], 0.75, na.rm = TRUE)
        H <- Q3 - Q1

        inner_barrier <- c(Q1 - 1.5 * H, Q3 + 1.5 * H)
        outer_barrier <- c(Q1 - 3 * H, Q3 + 3 * H)

        extreme_outliers <- industry_df[industry_df[[metric]] < outer_barrier[1] | industry_df[[metric]] > outer_barrier[2], ]
        mild_outliers <- industry_df[industry_df[[metric]] < inner_barrier[1] | industry_df[[metric]] > inner_barrier[2], ]

        outliers[[industry]] <- list(extreme_outliers = extreme_outliers, mild_outliers = mild_outliers)
    }

    return(outliers)
}

remove_extreme_outliers_by_industry <- function(df, metric) {
    outliers <- identify_outliers_by_industry(df, metric)
    industries <- unique(df$Industry)

    for (industry in industries) {
        extreme_outliers <- outliers[[industry]]$extreme_outliers
        df <- df[!(row.names(df) %in% row.names(extreme_outliers)), ]
    }
    return(df)
}

find_correlations <- function(df) {
    df_numeric <- select(df, Employees, Revenue, Expenses, Profit, Growth)

    rcorr(as.matrix(df_numeric))

    source("http://www.sthda.com/upload/rquery_cormat.r")
    rquery.cormat(df_numeric)
}
