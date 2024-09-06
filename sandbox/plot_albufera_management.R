plot_albufera_management <- function(management_df = albufera_management) {
  management_df$date <- as.Date(paste("2000",
                                      management_df$mm,
                                      management_df$dd,
                                      sep = "-"))

  management_df |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = height_cm, color = tancat)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(variety ~ .) +
    ggplot2::scale_x_date(date_labels = "%m/%d", date_breaks = "1 month") +
    ggplot2::scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
    ggplot2::labs(x = "Day of the Year",
                  y = "Height (cm)",
                  title = "Height vs Day of the Year")
}

plot_albufera_management2 <- function(management_df = albufera_management) {
  management_df$day <- as.Date(paste("2000",
                                      management_df$mm,
                                      management_df$dd,
                                      sep = "-"))
  management_df$seed_day <-
    as.numeric( management_df$day - as.Date("2000-04-20") )

  management_df |>
    ggplot2::ggplot(ggplot2::aes(x = seed_day, y = height_cm, color = tancat)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(variety ~ .) +
    ggplot2::scale_x_continuous() +
    ggplot2::scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
    ggplot2::labs(x = "Seed Day",
                  y = "Height (cm)",
                  title = "Height vs Day of the Year")
}
