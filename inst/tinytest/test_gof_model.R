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

resampling_Rn1 <- function() {
  set.seed(1)
  d <- data.frame(y = rnorm(10), x = 1:10)
  fit <- lm(y~x, data = d)
  o_btx <- order(predict(fit, type = "response"))
  sim_y <- function() {rnorm(n = 10)}
  ref_m <- function(model, data) {
    lm(y~x, data = data)
  }
  out <- resample_Rn1(
    data = d,
    model = fit,
    order_beta_time_covariates = o_btx,
    Y_name = "y",
    simulate_Y = sim_y,
    refit_model = ref_m,
    B = 2)

  set.seed(1)
  rnorm(10)
  d$y <- rnorm(10)
  fit1 <- lm(y~x, data = d)
  rn1_1 <- Rn1(
    Y = d$y,
    Y_hat = predict(fit1),
    order_beta_time_covariates = o_btx)
  d$y <- rnorm(10)
  fit2 <- lm(y~x, data = d)
  rn1_2 <- Rn1(
    Y = d$y,
    Y_hat = predict(fit2),
    order_beta_time_covariates = o_btx)
  expect_equal(out, list(rn1_1, rn1_2))

}
resampling_Rn1()

gof_model_calculates_pvalue_KS <- function() {
  d <- data.frame(y = 1:10, x1 = 1:10)
  fit <- glm(
    formula = y~x1,
    data = d,
    family = poisson())

  mockery::stub(
              where = gof_model,
              what = 'Rn1',
              how = c(1, -97, 19))
  mockery::stub(
              where = gof_model,
              what = 'resample_Rn1',
              how = lapply(1:100, function(i) sample(seq_len(i))))
  out <- gof_model(
    data = d,
    model = fit,
    type = "parametric",
    statistic = Rn1_KS)
  expect_equal(out$pvalue, 0.03)

}
gof_model_calculates_pvalue_KS()

gof_model_calculates_pvalue_CvM <- function() {
  d <- data.frame(y = 1:10, x1 = 1:10)
  fit <- glm(y~x1, data = d, family = poisson())

  rn1_stub <- c(1, -97, 19)
  rn1_boot_stub <- lapply(1:100, function(i) sample(seq_len(i)))
  mockery::stub(
              where = gof_model,
              what = 'Rn1',
              how = rn1_stub)
  mockery::stub(
              where = gof_model,
              what = 'resample_Rn1',
              how = rn1_boot_stub)
  out <- gof_model(
    data = d,
    model = fit,
    type = "parametric",
    statistic = Rn1_CvM)
  expect_equal(
    out$pvalue,
    mean(Rn1_CvM(rn1_stub) < sapply(rn1_boot_stub, Rn1_CvM)))

}
gof_model_calculates_pvalue_CvM()

gof_model_expect_small_pvalue <- function() {
  set.seed(1)
  X1 <- rnorm(100)
  X2 <- rnorm(100)
  d <- data.frame(
    y = rpois(n = 100, lambda = exp(4 + X1 * 2 + X2 * 6)),
    x1 = X1)
  fit <- glm(y~x1, data = d, family = poisson())
  out_p <- gof_model(
    data = d,
    model = fit,
    type = "parametric",
    statistic = Rn1_KS,
    B = 100)
  expect_equal(out_p$pvalue, 0)

  X1 <- rnorm(100)
  d <- data.frame(
    y = rnorm(n = 100, mean = 4 + X1^2),
    x1 = X1)
  fit <- lm(y~x1, data = d)
  out_w <- gof_model(
    data = d,
    model = fit,
    type = "wild",
    statistic = Rn1_CvM,
    B = 100)

  expect_equal(out_w$pvalue, 0)
}
gof_model_expect_small_pvalue()

gof_model_expect_non_small_pvalue <- function() {
  set.seed(1)
  X1 <- rnorm(100)
  d <- data.frame(
    y = rpois(n = 100, lambda = exp(4 + X1 * 2)),
    x1 = X1)
  fit <- glm(y~x1, data = d, family = poisson())
  out <- gof_model(
    data = d,
    model = fit,
    type = "parametric",
    statistic = Rn1_KS,
    B = 100)
  expect_equal(out$pvalue, 0.74)

  X1 <- rnorm(100)
  d <- data.frame(
    y = rnorm(n = 100, mean = 4 + X1 + X1^2),
    x1 = X1)
  fit <- lm(y~x1 + I(x1^2), data = d)
  out_w <- gof_model(
    data = d,
    model = fit,
    type = "wild",
    statistic = Rn1_CvM,
    B = 100)

  expect_equal(out_w$pvalue, 0.8)
}
gof_model_expect_non_small_pvalue()
