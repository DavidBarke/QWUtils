control_chart_params <- function(
  data, type = c("xbar_R", "xbar_s", "R", "s", "p")
) {
  params <- list(n = {
    summarised_table <- dplyr::group_by(data, sample) %>%
      summarise(n = dplyr::n()) %>%
      summarise(mean_n = mean(n))
    summarised_table$mean_n
  })

  R <- function(x) diff(range(x))

  switch(
    type,
    "xbar_s" = {
      summarised_table <- group_by(data, sample) %>%
        summarise(x_bar = mean(value), s = sd(value)) %>%
        summarise(x_bar_bar = mean(x_bar), s_bar = mean(s))
      params$x_bar_bar <- summarised_table$x_bar_bar
      params$s_bar <- summarised_table$s_bar
    },
    "s" = {
      summarised_table <- group_by(data, sample) %>%
        summarise(s = sd(value)) %>%
        summarise(s_bar = mean(s))
      params$s_bar <- summarised_table$s_bar
    },
    "p" = {
      params$p_bar <- mean(data$value)
    },
    "xbar_R" = {
      summarised_table <- group_by(data, sample) %>%
        summarise(x_bar = mean(value), R = R(value)) %>%
        summarise(x_bar_bar = mean(x_bar), R_bar = mean(R))
      params$x_bar_bar <- summarised_table$x_bar_bar
      params$R_bar <- summarised_table$R_bar
    },
    "R" = {
      summarised_table <- group_by(data, sample) %>%
        summarise(R = R(value)) %>%
        summarise(R_bar = mean(R))
      params$R_bar <- summarised_table$R_bar
    }
  )

  params
}
