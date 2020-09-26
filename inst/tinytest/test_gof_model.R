calc_beta_times_covariates_for_linear_models <- function() {
  X <- 1:10
  d = data.frame(y = 2 * X, x = X)
  fit <- glm(y ~ x, data = d, family = poisson())
  btx <- calc_beta_times_covariates(model = fit)
  expect_equivalent(
    btx,
    coef(fit)["(Intercept)"] + coef(fit)["x"] * X)
}
calc_beta_times_covariates_for_linear_models()

calc_beta_times_covariates_non_linear_gen_error <- function() {
  # fit stolen from example(nls)
  fit <- nls(
    formula = density ~ SSlogis(log(conc), Asym, xmid, scal),
    data = DNase)
  expect_error(calc_beta_times_covariates(model = fit))
}
calc_beta_times_covariates_non_linear_gen_error()

extract_Y_name_from_model <- function() {
  X <- 1:10
  d = data.frame(y = 2 * X, x = X)
  fit <- lm(y ~ x, data = d)
  expect_equal(get_Y_name(model = fit), "y")
}
extract_Y_name_from_model()

extract_Y_name_wo_lhs_gen_error <- function() {
  expect_error(
    get_Y_name(model = formula("~x")),
    pattern = "Formula.*does not contain a left hand side")
}
extract_Y_name_wo_lhs_gen_error()

refit_model_uses_new_data_to_update <- function() {
  X <- 1:10
  d = data.frame(y = 2 * X, x = X)
  fit <- lm(y ~ x, data = d)

  new_d <- data.frame(y = 3 * X, x = X)
  update_fit <- refit_model(model = fit, data = new_d)
  expect_equal(
    coefficients(object = update_fit),
    c("(Intercept)" = 0, "x" = 3))
}
refit_model_uses_new_data_to_update()

Rn1_calculates_marked_emp_process <- function() {
  Y <- 1:10
  Y_hat <- 10 * runif(n = 10)
  expect_equal(
    Rn1(
      Y = Y,
      Y_hat = Y_hat,
      order_beta_time_covariates = 1:10),
    cumsum(Y - Y_hat) / sqrt(10)
    )
  expect_equal(
    Rn1(
      Y = Y,
      Y_hat = Y_hat,
      order_beta_time_covariates = 10:1),
    cumsum(rev(Y - Y_hat)) / sqrt(10)
    )
}
Rn1_calculates_marked_emp_process()

calculate_cramer_von_mises_for_marked_emp_process <- function() {
  e <- rnorm(10)
  expect_equal(Rn1_CvM(e), mean(e^2))
}
calculate_cramer_von_mises_for_marked_emp_process()

calculate_kolmogorov_smirnov_for_marked_emp_process <- function() {
  e <- rnorm(10)
  expect_equal(Rn1_KS(e), max(abs(e)))
}
calculate_kolmogorov_smirnov_for_marked_emp_process()
