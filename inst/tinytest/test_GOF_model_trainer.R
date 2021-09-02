GOF_model_trainer_all_methods_abstract <- function() {
  mt <- GOF_model_trainer$new()
  expect_error(
    mt$refit(),
    pattern = "Abstract")
}
GOF_model_trainer_all_methods_abstract()

GOF_lm_trainer_refits_model <- function() {
  mt <- GOF_lm_trainer$new()
  d <- data.frame(Y = 1:10, X = 1:10)
  fit <- lm(Y~X, data = d)
  d_new <- d
  d_new$Y <- -(1:10)
  fit_new <- mt$refit(model = fit, data = d_new)
  expect_equivalent(
    coef(fit_new),
    c(0, -1))
}
GOF_lm_trainer_refits_model()

GOF_glm_trainer_refits_model <- function() {
  mt <- GOF_glm_trainer$new()
  d <- data.frame(Y = 1:10, X = 1:10)
  fit <- glm(Y~X, data = d, family = Gamma())
  d_new <- d
  d_new$Y <- (101:110)
  fit_new <- mt$refit(model = fit, data = d_new)
  expect_equivalent(
    coef(fit_new),
    coef(glm(Y~X, data = d_new, family = Gamma())))
}
GOF_glm_trainer_refits_model()
