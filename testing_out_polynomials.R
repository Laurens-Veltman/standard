df <- tibble::tribble(
  ~prot,                 ~abs,
  0,                      0,
  245.995423340961,       0.29867374005305,
  500,                    0.50026525198939,
  997.711670480549,       0.801591511936339,
  1500,                   0.972413793103448,
  1997.71167048055,       1.10079575596817
)

MyNewDRCfun <- function(){

  fct <- function(x, parm) {
    # function code here
  }
  ssfct <- function(data){
    # Self-starting code here
  }
  names <- c()
  text <- "Descriptive text"

  ## Returning the function with self starter and names
  returnList <- list(fct = fct, ssfct = ssfct, names = names, text = text)
  class(returnList) <- "drcMean"
  invisible(returnList)
}

test_function <- function(X, a, b, c, d) {
  a + b*X + (c*X^2) + (d*X^3)
}
second_order_polynomial <- function(X, a, b, c) {
  - a + b*X - (c*X^2)
}
fourth_order_polynomial <- function(X, a, b, c, d, e) {
  - a + b*X - (c*X^2) + d*X^3 + d*X^4
}

some_new_function <- function() {
  fct <- function(x, parm) {
    test_function(x, parm[1], parm[2], parm[3], parm[4])
  }

  ssfct <- function(data) {
    d <- 100
    c <- 750
    b <- 10
    a <- 0

    return(c(a, b, c, d))
  }

  names <- c("a", "test", "c", "d")
  text <- "Third Order Polynomial"

  returnList <- list(fct = fct, ssfct = ssfct, names = names, text = text)
  class(returnList) <- "drcMean"
  invisible(returnList)
}


drc::drm(abs ~ prot, fct = some_new_function(), data = df#,
         # start = list(a = -3, b = 1000, c = -750, d = 1400)

         ) |> plot(log = "")


mod <- nls(abs ~ test_function(prot, a, b, c, d),
    data = df,
    start = list(a = 0, b = 2000, c = -2000, d = 2000))


plot(df)
broom::augment(mod, newdata = data.frame(prot = seq(0, 2000))) |>
  lines()


mod2 <- nls(abs ~ second_order_polynomial(prot, a, b, c),
           data = df,
           start = list(a = 0, b = 0, c = 0))
plot(df)
broom::augment(mod2, newdata = data.frame(prot = seq(0, 2000))) |>
  lines()


plot(df)
mod3 <- nls(abs ~ SSasymp(prot, Asym, R0, lrc),
    data = df)

broom::augment(mod3, newdata = data.frame(prot = seq(0, 2000))) |>
  lines()
summary(mod3)

broom::augment(mod3, newdata = data.frame(prot = 0.6))



mod4 <- nls(prot ~ test_function(abs, a, b, c, d),
           data = df,
           start = list(a = 0, b = 2000, c = -2000, d = 2000))


plot(df)

broom::augment(mod4, newdata = data.frame(abs = 0.6))

broom::tidy(mod4)

mod5 <- nls(prot ~ SSA,
            data = df)

mod5 <- drc::drm(abs ~ prot, fct = drc::AR.2(), data = df)

plot(mod5, log = "")

broom::augment(mod5, newdata = data.frame(abs = 0.6))
broom::augment(mod5, data = data.frame(abs = seq(6)))

broom::tidy(mod5)

drc::ED(mod5, 0.6 / 1.25 * 100)




