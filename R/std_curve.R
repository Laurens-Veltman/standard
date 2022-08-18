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
#' assay_data %>%
#'   std_curve_fit(Protein, Absorbance) %>%
#'   plot()
std_curve_fit <- function(data, conc, resp) {

  # do lots of quasiquotation magic to make the formula work with any of the
  # user-supplied columns
  # enquoting the given columns, so they can be used in a function
  in_conc <- rlang::enquo(conc)
  in_resp <- rlang::enquo(resp)
  # use quo_name, sym and expr to define the forumlar for the model from the
  # user supplied columns
  .f <- rlang::expr(
    !!dplyr::sym(rlang::quo_name(in_conc)) ~
      !!dplyr::sym(rlang::quo_name(in_resp))
  )

  # fit the actual linear model with the data and the created forumla
  std_curve <- stats::lm(.f, data = data)

  # return the model
  class(std_curve) <- c("std_curve", "lm", "oldClass")

  std_curve
}


#' Use a Standard Curve to Calculate Unknown Values
#'
#' @param std_curve A linear model, created with either `lm()` or
#'   `standard::std_curve_fit()`
#' @param unknowns A numeric vector of unknown values, which the standard curve
#'   will be used to predict their values.
#' @param digits Number of decimal places for calculations.
#'
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
#' assay_data %>%
#'   std_curve_fit(Protein, Absorbance) %>%
#'   std_curve_calc(unk) %>%
#'   plot()
std_curve_calc <- function(std_curve, unknowns, digits = 3) {
  stopifnot(is.vector(unknowns))

  variable_names <- colnames(std_curve$model)

  unk <- tibble::tibble(
    !!variable_names[2] := unknowns
  )

  calculated_data <- purrr::quietly(broom::augment)(std_curve, newdata = unk)$result %>%
    dplyr::select(
      !!variable_names[2],
      !!variable_names[1] := .data$.fitted
    )

  calculated_data[, 2] <- round(calculated_data[, 2],
    digits = digits
  )

  output <- list(
    std_curve = std_curve,
    std_calc_data = calculated_data
  )

  output <- structure(output, class = "std_calc")

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
#' assay_data %>%
#'   std_curve_fit(Protein, Absorbance) %>%
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

#' Printing Results of `std_curve_calc()`
#'
#' @param x object of class `std_calc`, the output of `std_curve_calc`
#' @param ... additional arguments to be passed to or from methods.
#' @export
#'
print.std_calc <- function(x, ...) {
  print(x[["std_calc_data"]])
}

#' Convert `std_calc` to data frame
#'
#' @param x object of class `std_calc`, the output of `std_curve_calc()`
#' @param row.names Optional vector of rownames.
#' @param optional logical. If TRUE, setting row names and converting column
#'   names (to syntactic names: see make.names) is optional. Note that all of
#'   R's base package as.data.frame() methods use optional only for column names
#'   treatment, basically with the meaning of data.frame(*, check.names =
#'   !optional). See also the make.names argument of the matrix method.
#' @param ... additional arguments to be passed to or from methods.
#'
#' @return data.frame
#' @export
as.data.frame.std_calc <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x[["std_calc_data"]], row.names = row.names, optional = optional, ...)
}


#' Generic function for subsetting output of `std_curve_fit()`
#'
#' @param x object of class `std_curve`, the output of `std_curve_fit()`
#' @param i row index
#' @param j column index
#'
#' @return column of tibble
#' @export
`[.std_calc` <- function(x, i, j) {
  x[i, j]
}

#' Plot a Standard Curve
#'
#' @param data Result of `std_curve_pred()` or `std_curve_fit()`.
#'
#' @return a [ggplot2][ggplot2::ggplot] plot with the standard curve and unkowns
#'   plotted, whch can be further customised using `ggplot` options.
#' @export
#' @importFrom rlang .data
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
#' assay_data %>%
#'   std_curve_fit(Protein, Absorbance) %>%
#'   std_curve_calc(unk) %>%
#'   plot()
std_curve_plot <- function(data) {
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
    ggplot2::ggplot(
      ggplot2::aes_string(var_names[1], var_names[2])
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

#' Generic Function for Plotting Standard Curve Calculations
#'
#' @param x output of `std_curve_calc()`
#' @param ... Additional arguments to be passed to `std_curve_plot()`
#'
#' @return ggplot2 plot
#' @export
plot.std_calc <- function(x, ...) {
  standard::std_curve_plot(x, ...)
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
