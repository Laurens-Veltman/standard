test_that("Basic Standard Curve Workflow", {
  ideal_results <- tibble::tibble(
    abs = c(0.554, 0.568, 0.705),
    prot = c(0.361, 0.384, 0.609)
  )

  prot <- c(
    0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000,
    0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000
  )

  abs <- c(
    0.329, 0.352, 0.349, 0.379, 0.417, 0.491, 0.668, 0.956,
    0.327, 0.341, 0.355, 0.383, 0.417, 0.446, 0.655, 0.905
  )

  assay_data <- tibble::tibble(
    prot = prot,
    abs = abs
  )

  unk <- c(0.554, 0.568, 0.705)

  calculated <- standard::std_curve_fit(assay_data, prot, abs) |>
    standard::std_curve_calc(unk)

  expect_equal(ideal_results, round(calculated$std_calc_data, 3))
})
