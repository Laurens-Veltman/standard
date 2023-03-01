#' Create a Standard Curve From Known Data
#'
#' @param data A `data.frame` that contains the columns for concentration and
#'   observed response for the standard curve.
#' @param conc Name of the column that contains the concentration for the
#'   standard curve.
#' @param resp Name of the column that contains the response values for the
#'   standard curve.
#'
#' @return A linear model ( [lm()][stats::lm] ) object to be used as a standard
#'   curve, for use with `standard::std_curve_calc()` `broom::augment()` or
#'   `stats::predict()`.
#' @export
#'
#' @examples
#' library(standard)
#'
#' # Protein concentrations of the standards used in the assay
#' prot <- c(
#'   0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000,
#'   0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000
#' )
#'
#' # absorbance readings from the standards used in the assay
#' abs <- c(
#'   0.329, 0.352, 0.349, 0.379, 0.417, 0.491, 0.668, 0.956,
#'   0.327, 0.341, 0.355, 0.383, 0.417, 0.446, 0.655, 0.905
#' )
#' assay_data <- data.frame(
#'   Protein = prot,
#'   Absorbance = abs
#' )
#'
#' # unknown concentrations
#' unk <- c(0.554, 0.568, 0.705)
#'
#'
#' assay_data |>
#'   std_curve_fit(Protein, Absorbance) |>
#'   plot()
std_curve_fit <- function(data, conc, resp) {

  ## do lots of quasiquotation magic to make the formula work with any of the
  ## user-supplied columns

  # enquoting the given columns, so they can be used in a function
  in_conc <- rlang::enquo(conc)
  in_resp <- rlang::enquo(resp)
  # use quo_name, sym and expr to define the forumla for the model from the
  # user supplied columns
  .f <- rlang::expr(
    !!rlang::sym(rlang::quo_name(in_conc)) ~
      !!rlang::sym(rlang::quo_name(in_resp))
  )

  # fit the actual linear model with the data and the created formula
  std_curve <- stats::lm(.f, data = data)

  # return the model
  class(std_curve) <- c("std_curve", class(std_curve))

  std_curve
}


#' Use a Standard Curve to Calculate Unknown Values
#'
#' @param std_curve A linear model, created with either `lm()` or
#'   `standard::std_curve_fit()`
#' @param unknowns A numeric vector of unknown values, which the standard curve
#' @param simplify TRUE returns a numeric vector, FALSE returns a `tibble::tibble()`.
#'   will be used to predict their values.
#' @return a [tibble][tibble::tibble-package] with a column for the unknown
#'   values, and a column `.fitted` for the predicted values, based on the
#'   standard curve.
#' @export
#' @importFrom rlang .data :=
#'
#' @examples
#' library(standard)
#'
#' # Protein concentrations of the standards used in the assay
#' prot <- c(
#'   0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000,
#'   0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000
#' )
#'
#' # absorbance readins from the standards used in the assay
#' abs <- c(
#'   0.329, 0.352, 0.349, 0.379, 0.417, 0.491, 0.668, 0.956,
#'   0.327, 0.341, 0.355, 0.383, 0.417, 0.446, 0.655, 0.905
#' )
#' assay_data <- data.frame(
#'   Protein = prot,
#'   Absorbance = abs
#' )
#'
#' # unknown concentrations
#' unk <- c(0.554, 0.568, 0.705)
#'
#'
#' assay_data |>
#'   std_curve_fit(Protein, Absorbance) |>
#'   std_curve_calc(unk) |>
#'   plot()
std_curve_calc <- function(std_curve, unknowns, simplify = TRUE) {
  stopifnot(is.vector(unknowns))

  variable_names <- colnames(std_curve$model)

  unk <- tibble::tibble(
    !!variable_names[2] := unknowns
  )

  unk$.fitted <- unname(stats::predict(std_curve, newdata = unk))

  if (simplify) {
    output <- unk$.fitted
  } else {
    colnames(unk) <- variable_names
    output <- unk
  }

  output
}

#' Extract and Paste Formula From Standard Curve
#'
#' @param std_curve object of class std_curve, the output of `std_curve_fit()`
#' @param digits Number of decimal places to round numbers in the formula to.
#'
#' @return a string of the extracted formula from the standard curve
#' @export
#'
#' @examples
#' library(standard)
#'
#' # Protein concentrations of the standards used in the assay
#' prot <- c(
#'   0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000,
#'   0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000
#' )
#'
#' # absorbance readins from the standards used in the assay
#' abs <- c(
#'   0.329, 0.352, 0.349, 0.379, 0.417, 0.491, 0.668, 0.956,
#'   0.327, 0.341, 0.355, 0.383, 0.417, 0.446, 0.655, 0.905
#' )
#' assay_data <- data.frame(
#'   Protein = prot,
#'   Absorbance = abs
#' )
#'
#' # unknown concentrations
#' unk <- c(0.554, 0.568, 0.705)
#'
#'
#' assay_data |>
#'   std_curve_fit(Protein, Absorbance) |>
#'   std_paste_formula()
std_paste_formula <- function(std_curve, digits = 3) {
  numbers <- stats::coef(std_curve)

  axis_names <- colnames(std_curve[["model"]])
  sign <- ifelse(numbers[["(Intercept)"]] < 0, "-", "+")

  paste(
    axis_names[1],
    "=",
    round(numbers[[axis_names[2]]], digits = digits),
    "*",
    axis_names[2],
    sign,
    round(abs(numbers[["(Intercept)"]]), digits = digits)
  )
}

#' Plot a Standard Curve
#'
#' @param data Result of `std_curve_pred()` or `std_curve_fit()`.
#'
#' @return a [ggplot2][ggplot2::ggplot] plot with the standard curve and unkowns
#'   plotted, whch can be further customised using `ggplot` options.
#' @export
#' @importFrom rlang .data !!
#'
#' @examples
#' library(standard)
#'
#' # Protein concentrations of the standards used in the assay
#' prot <- c(
#'   0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000,
#'   0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000
#' )
#'
#' # absorbance readins from the standards used in the assay
#' abs <- c(
#'   0.329, 0.352, 0.349, 0.379, 0.417, 0.491, 0.668, 0.956,
#'   0.327, 0.341, 0.355, 0.383, 0.417, 0.446, 0.655, 0.905
#' )
#' assay_data <- data.frame(
#'   Protein = prot,
#'   Absorbance = abs
#' )
#'
#' # unknown concentrations
#' unk <- c(0.554, 0.568, 0.705)
#'
#'
#' assay_data |>
#'   std_curve_fit(Protein, Absorbance) |>
#'   std_curve_calc(unk) |>
#'   plot()
std_curve_plot <- function(data) {
  if (!(methods::is(data, "lm") |
    methods::is(data, "std_curve"))) {
    stop("Input must be a linear model of the result of `std_curve_fit().`")
  }

    std_curve <- data
    r_squared <- summary(std_curve)[["r.squared"]]
    raw_data <- std_curve[["model"]]
    formula_label <- paste0(
      "R<sup>2</sup> = ",
      round(r_squared, 3),
      "<br>",
      std_paste_formula(std_curve)
    )

  var_names <- colnames(raw_data)
  annotation_positions <- data.frame(
    x = lerp_vec(raw_data[, 1], 0.01),
    y = lerp_vec(raw_data[, 2], 0.8)
  )



  plt <- raw_data |>
    ggplot2::ggplot(
      ggplot2::aes(!!rlang::sym(var_names[1]), !!rlang::sym(var_names[2]))
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(
      colour = "gray40",
      method = "lm",
      formula = "y ~ x",
      se = FALSE
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

  plt
}

#' Generic Function for Plotting Fitted Standard Curves
#'
#' @param x output of `std_surve_fit()`
#' @param ... Additional arguments to be passed to `std_curve_plot()`
#'
#' @return ggplot2 plot
#' @export
plot.std_curve <- function(x, ...) {
  standard::std_curve_plot(x, ...)
}
