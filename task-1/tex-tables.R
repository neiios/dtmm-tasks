sink("tables.tex")
for (column in c("Inception", "Employees", "Revenue", "Expenses", "Profit", "Growth")) {
    table <- fin %>%
        group_by(Industry) %>%
        summarise(
            Min = min(.data[[column]]),
            Mean = mean(.data[[column]]),
            Max = max(.data[[column]]),
            SD = sd(.data[[column]])
        )

    second_table <- fin %>%
        group_by(Industry) %>%
        summarise(
            `1Q` = quantile(.data[[column]], 0.25),
            Median = median(.data[[column]]),
            `3Q` = quantile(.data[[column]], 0.75),
        )

    print(
        xtable(
            table,
            caption = sprintf("Pagrindinė lentelė rodikliui \\enquote{%s}", column),
            label = sprintf("table:%s-part1", tolower(column)),
            auto = TRUE,
            digits = 2,
        ),
        caption.placement = "top",
        table.placement = "htbp",
        booktabs = TRUE,
        include.rownames = FALSE,
    )

    print(
        xtable(
            second_table,
            caption = sprintf("Kvantilių lentelė rodikliui \\enquote{%s}", column),
            label = sprintf("table:%s-part2", tolower(column)),
            auto = TRUE,
            digits = 2,
        ),
        caption.placement = "top",
        table.placement = "htbp",
        booktabs = TRUE,
        include.rownames = FALSE,
    )
}
sink()
