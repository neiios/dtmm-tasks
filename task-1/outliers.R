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

identify_outliers_by_industry(fin, "Employees")
fin <- remove_extreme_outliers_by_industry(fin, "Employees")

identify_outliers_by_industry(fin, "Revenue")
fin <- remove_extreme_outliers_by_industry(fin, "Revenue")

identify_outliers_by_industry(fin, "Expenses")
fin <- remove_extreme_outliers_by_industry(fin, "Expenses")

identify_outliers_by_industry(fin, "Profit")
fin <- remove_extreme_outliers_by_industry(fin, "Profit")

hist(fin$Employees,
    xlab = "Darbuotojų skaičius",
    ylab = "Kiekis",
    col = "steelblue",
    main = "",
    breaks = 100,
)

boxplot(fin$Revenue, fin$Expenses, fin$Profit,
    ylab = "Dollars",
    col = "steelblue"
)

summary(select_if(fin, is.numeric))
apply(select_if(fin, is.numeric), 2, sd)
