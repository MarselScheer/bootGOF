GOF_model_info_extractor_all_methods_are_abstract <- function() { # nolint
  mif <- GOF_model_info_extractor$new()
  expect_error(
    mif$yhat(model = list()),
    pattern = "Abstract")
  expect_error(
    mif$y_minus_yhat(model = list()),
    pattern = "Abstract")
  expect_error(
    mif$beta_x_covariates(model = list()),
    pattern = "Abstract")

}
GOF_model_info_extractor_all_methods_are_abstract()

GOF_lm_info_extractor_one_covariate <- function() { # nolint
  fit <- lm(y~x, data = data.frame(y = 2 * (1:10), x = 1:10))
  mif <- GOF_lm_info_extractor$new()
  expect_equivalent(
    mif$yhat(model = fit),
    2 * (1:10))
  expect_equivalent(
    mif$y_minus_yhat(model = fit),
    replicate(n = 10, expr = 0))
  expect_equivalent(
    mif$beta_x_covariates(model = fit),
    2 * (1:10))
}
GOF_lm_info_extractor_one_covariate()


GOF_glm_info_extractor_one_covariate <- function() { # nolint
  Y <- 2 * (1:10) # nolint
  X <- 1:10 # nolint
  fit <- glm(y~x, data = data.frame(y = Y, x = X), family = poisson())
  beta <- coef(fit)
  b_times_x <- beta["(Intercept)"] + beta["x"] * X
  mif <- GOF_glm_info_extractor$new()
  expect_equivalent(
    mif$yhat(model = fit),
    exp(b_times_x))
  expect_equivalent(
    mif$y_minus_yhat(model = fit),
    Y - exp(b_times_x))
  expect_equivalent(
    mif$beta_x_covariates(model = fit),
    b_times_x)
}
GOF_glm_info_extractor_one_covariate()
