GOF_model_simulator_all_methods_abstract <- function() {
  ms <- GOF_model_simulator$new()
  expect_error(
    ms$resample_y(),
    pattern = "Abstract")
}
GOF_model_simulator_all_methods_abstract()

GOF_lm_sim_param_simulates <- function() {
  set.seed(1)
  ms <- GOF_lm_sim_param$new()
  X <- 1:1000
  Y <- X + rnorm(1000, sd = 0.1)
  d <- data.frame(y = Y, x = X)
  fit <- lm(y~x, data = d)
  expect_true(
    # epsilon in Y and in resample_y() have a variance
    # of 0.1^2, so the difference has an expected
    # variance of 2 * 0.1^2
    var(ms$resample_y(model = fit) - Y) <= 2 * 0.1^2 + 0.03
  )
}
GOF_lm_sim_param_simulates()

GOF_lm_sim_param_wild_rademacher_simulates <- function() {
  set.seed(1)
  ms <- GOF_lm_sim_wild_rademacher$new()
  X <- 1:10
  Y <- X + rnorm(10)
  d <- data.frame(y = Y, x = X)
  fit <- lm(y~x, data = d)
  mockery::stub(
    where = ms$resample_y,
    what = "rrademacher",
    how = rep(1, 10)
  )
  expect_equivalent(
    ms$resample_y(model = fit),
    Y
  )
  mockery::stub(
    where = ms$resample_y,
    what = "rrademacher",
    how = rep(-1, 10)
  )
  expect_equivalent(
    ms$resample_y(model = fit),
    predict.lm(fit) - residuals.lm(fit)
  )
}
GOF_lm_sim_param_wild_rademacher_simulates()

GOF_glm_sim_param_simulates <- function() {
  set.seed(1)
  N <- 100
  X1 <- rnorm(N)
  X2 <- rnorm(N)
  d <- data.frame(
    sth = rpois(n = N, lambda = exp(4 + X1 * 2 + X2 * 6)),
    x1 = X1, x2 = X2)
  fit <- glm(sth ~ ., data = d, family = poisson())
  ms <- GOF_glm_sim_param$new()
  d_sim <- d
  d_sim$sth <- ms$resample_y(model = fit)
  fit_sim <- glm(sth ~ ., data = d_sim, family = poisson())
  expect_true(
    sum(abs(coef(fit) - coef(fit_sim))) <= 0.001
  )
}
GOF_glm_sim_param_simulates()

GOF_glm_sim_param_wild_rademacher_simulates <- function() {
  set.seed(1)
  ms <- GOF_glm_sim_wild_rademacher$new()
  X <- 1:10
  Y <- X + rnorm(10)
  d <- data.frame(y = Y, x = X)
  fit <- glm(y~x, data = d, family = Gamma())
  mockery::stub(
    where = ms$resample_y,
    what = "rrademacher",
    how = rep(1, 10)
  )
  expect_equivalent(
    ms$resample_y(model = fit),
    Y
  )
  mockery::stub(
    where = ms$resample_y,
    what = "rrademacher",
    how = rep(-1, 10)
  )
  expect_equivalent(
    ms$resample_y(model = fit),
    predict.glm(fit, type = "response") -
      residuals.glm(fit, type = "response")
  )
}
GOF_glm_sim_param_wild_rademacher_simulates()
