#' Title
#'
#' @param data
#'
#' @return
#'
#' @examples
plot_std_curve_linear <- function(data) {
  if (!(methods::is(data, "std_calc") |
        methods::is(data, "lm") |
        methods::is(data, "std_curve"))) {
    stop("Input must be the output from either std_curve_fit() or std_curve_calc().")
  }

  if (methods::is(data, "std_calc")) {
    std_calc <- data
    r_squared <- summary(std_calc[["std_curve"]])[["r.squared"]]
    raw_data <- std_calc[["std_curve"]][["model"]]
    std_curve <- std_calc[["std_curve"]]
    pred_data <- std_calc[["std_calc_data"]]

    formula_label <- paste0(
      "R<sup>2</sup> = ",
      round(r_squared, 3),
      "<br>",
      std_paste_formula(std_curve),
      "<br>Calculated Unknowns:"
    )
  } else if (methods::is(data, "std_curve")) {
    std_curve <- data
    r_squared <- summary(std_curve)[["r.squared"]]
    raw_data <- std_curve[["model"]]
    formula_label <- paste0(
      "R<sup>2</sup> = ",
      round(r_squared, 3),
      "<br>",
      std_paste_formula(std_curve)
    )
  }

  var_names <- colnames(raw_data)
  annotation_positions <- data.frame(
    x = lerp_vec(raw_data[, 1], 0.01),
    y = lerp_vec(raw_data[, 2], 0.8)
  )



  plt <- raw_data %>%
    ggplot2::ggplot(ggplot2::aes_string(var_names[1], var_names[2])) +
    ggplot2::geom_point() +
    ggplot2::geom_line(
      data = linear_mod_predictor(std_curve),
      colour = "gray40"
    ) +
    ggplot2::theme_classic() +
    ggtext::geom_richtext(
      data = annotation_positions,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        label = formula_label
      ),
      fill = NA,
      label.color = NA,
      label.padding = grid::unit(rep(0, 4), "pt"),
      vjust = -0.1,
      hjust = 0
    )

  if (methods::is(data, "std_calc")) {
    plt <- plt +
      ggplot2::geom_segment(
        data = pred_data,
        ggplot2::aes_string(
          x = var_names[1],
          xend = var_names[1],
          y = 0,
          yend = var_names[2]
        ),
        linetype = "dashed"
      ) +
      ggplot2::geom_point(
        data = pred_data,
        shape = 4,
        size = 5
      ) +
      ggpp::geom_table(
        data = annotation_positions,
        label = list(pred_data),
        mapping = ggplot2::aes(x = .data$x, y = .data$y),
        hjust = 0,
        vjust = 1.1
      )
  }

  plt
}
