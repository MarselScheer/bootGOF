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
