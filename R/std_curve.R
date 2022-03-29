#' Create a Standard Curve From Known Data
#'
#' @param data A `data.frame` that contains the columns for concentration and
#'   observed response for the standard curve.
#' @param conc Name of the column that contains the concentration for the
#'   standard curve.
#' @param resp Name of the column that contains the response values for the
#'   standard curve.
#' @param curve
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
std_curve_fit <- function(data, conc, resp, curve = "linear") {
  if (curve == "linear") {
    std_curve <- std_func_linear(
      data = data,
      conc = {{ conc }},
      resp = {{ resp }}
      )
    class(std_curve) <- c("std_mod_linear", class(std_curve))
  } else if (curve == "log") {
    std_curve <- std_func_logis(
      data = data,
      conc = {{ conc }},
      resp = {{ resp }}
      )
    class(std_curve) <- c("std_mod_log", class(std_curve))
  }

  class(std_curve) <- c("std_curve", class(std_curve))
  std_curve
}

#' Generate Points for Plotting a Standard Curve
#'
#' @param std_curve A model generated from `std_curve_fit()`
#' @importFrom rlang := .data
#' @return
#'
#' @examples
calc_std_curve <- function(std_curve) {
  data <- quiet_broom_augment(std_curve)


  if (methods::is(std_curve, "std_mod_linear")) {
    name_x <- colnames(data)[2]
    vec_x <- dplyr::pull(data, !!name_x)
    min_x <- min(vec_x)
    max_x <- max(vec_x)

    df <- tibble::tibble(
      !!name_x :=  seq(min_x, max_x, length.out = 100)
    )

    df <- quiet_broom_augment(std_curve, newdata = df) %>%
      dplyr::select(
        !!name_x,
        .data$.fitted
      )
    df
  } else if (methods::is(std_curve, "std_mod_log")) {
    name_x <- colnames(data)[1]
    name_y <- colnames(data)[2]
    vec_x <- dplyr::pull(data, !!name_x)
    min_x <- min(vec_x)
    max_x <- max(vec_x)

    df <- tibble::tibble(
      !!name_x :=  seq(min_x, max_x, length.out = 100)
    )

    df <- quiet_broom_augment(std_curve, newdata = df) %>%
      dplyr::select(
        !!name_x,
        !!name_y := .data$.fitted
      )
    df
  }


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

  variable_names <- colnames(quiet_broom_augment(std_curve))

  if (methods::is(std_curve, "std_mod_linear")) {
    unk <- tibble::tibble(
      !!variable_names[2] := unknowns
    )
    calculated_data <- quiet_broom_augment(std_curve, newdata = unk) %>%
      dplyr::select(
        !!variable_names[2],
        !!variable_names[1] := .data$.fitted
      )
  }
  if (methods::is(std_curve, "std_mod_log")) {
    unk <- tibble::tibble(
      !!variable_names[1] := unknowns
    )
    calculated_data <- quiet_broom_augment(std_curve, newdata = unk) %>%
      dplyr::select(
        !!variable_names[1],
        !!variable_names[2] := .data$.fitted
      )
  }


  calculated_data[, 2] <- round(calculated_data[, 2], digits = digits)

  output <- list(
    std_curve = std_curve,
    std_calc_data = calculated_data
  )

  class(output) <- c("std_calc", class(std_curve))

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
  if (!(methods::is(data, "std_calc") | methods::is(data, "std_curve"))) {
    stop("Input must be the output from either std_curve_fit() or std_curve_calc().")
  }

  if (methods::is(data, "std_mod_linear")) {
    plot_std_curve_linear(data)
  } else if (methods::is(data, "std_mod_log")) {
    plot_std_curve_log(data)
  }


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
