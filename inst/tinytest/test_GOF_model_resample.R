GOF_model_resample_works <- function() {
  d <- data.frame(y = 101:110, x = 1:10)
  ms_mock <- R6::R6Class(
    inherit = GOF_model_simulator,
    public = list(
      resample_y = function(model) {
        1:10
      }))$new()
  mt <- GOF_lm_trainer$new()
  mr <- GOF_model_resample$new(gof_model_simulator = ms_mock, gof_model_trainer = mt)
  fit <- lm(y~x, data = d)
  fit_new <- mr$resample(model = fit, data = d, y_name = "y")
  expect_equivalent(
    coef(fit_new),
    c(0, 1))
}
GOF_model_resample_works()
