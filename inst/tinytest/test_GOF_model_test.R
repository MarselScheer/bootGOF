Rn1_fun <- function(r, o) {
  cumsum(r[o])
}
GOF_model_test_necessary_input <- function() {
  expect_error(
    GOF_model_test$new(),
    pattern = "nmb_boot_samples")
  expect_error(
    GOF_model_test$new(nmb_boot_samples = 0),
    pattern = ".*nmb_boot_samples.*>= 1")
  expect_error(
    GOF_model_test$new(
      nmb_boot_samples = 1),
    pattern = "model")
  expect_error(
    GOF_model_test$new(
      model = "dummy",
      nmb_boot_samples = 1),
    pattern = "data")
  expect_error(
    GOF_model_test$new(
      model = "dummy",
      nmb_boot_samples = 1,
      data = "dummy"),
    pattern = "y_name")
  expect_error(
    GOF_model_test$new(
      model = "dummy",
      nmb_boot_samples = 1,
      data = "dummy",
      y_name = "dummy"),
    pattern = "Rn1_statistic")
  expect_error(
    GOF_model_test$new(
      model = "dummy",
      nmb_boot_samples = 1,
      data = "dummy",
      y_name = "dummy",
      Rn1_statistic = "dummy"),
    pattern = "gof_model_info_extractor")
  expect_error(
    GOF_model_test$new(
      model = "dummy",
      nmb_boot_samples = 1,
      data = "dummy",
      y_name = "dummy",
      Rn1_statistic = "dummy",
      gof_model_info_extractor = "dummy"),
    pattern = "gof_model_resample")
}
GOF_model_test_necessary_input()

GOF_model_test_calc_Rn1_org <- function() {
  set.seed(1)
  X <- rnorm(10)
  Y <- 5 * X + rnorm(10)
  d <- data.frame(y = Y, x = X)
  fit <- lm(y ~ x, data = d)
  mt <- GOF_model_test$new(
    model = fit,
    data = d,
    y_name = "y",
    Rn1_statistic = "dummy",
    nmb_boot_samples = 10,
    gof_model_info_extractor = GOF_lm_info_extractor$new(),
    gof_model_resample = "dummy")
  expect_equal(
    mt$get_Rn1_org(),
    Rn1_fun(r = residuals(fit), o = order(X))
  )
}
GOF_model_test_calc_Rn1_org()

GOF_model_test_calc_Rn1_boot <- function() {
  set.seed(1)
  X <- rnorm(10)
  Y <- 5 * X + rnorm(10)
  d <- data.frame(y = Y, x = X)
  fit <- lm(y ~ x, data = d)


  model_resample_mock <- list(rnorm(10), rnorm(10), rnorm(10))
  MODEL_MOCK_NMB <- 0
  lm_sim_para_mock <- R6::R6Class(
    public = list(
      resample_y = function(model) {
        MODEL_MOCK_NMB <<- MODEL_MOCK_NMB + 1
        model_resample_mock[[MODEL_MOCK_NMB]]
      }))$new()

  mt <- GOF_model_test$new(
    model = fit,
    nmb_boot_samples = 3,
    data = d,
    y_name = "y",
    Rn1_statistic = "dummy",
    gof_model_info_extractor = GOF_lm_info_extractor$new(),
    gof_model_resample = GOF_model_resample$new(
      gof_model_simulator = lm_sim_para_mock,
      gof_model_trainer = GOF_lm_trainer$new()
    )
  )

  d1 <- d
  d1$y <- model_resample_mock[[1]]
  fit1 <- lm(y~x, data = d1)
  d2 <- d
  d2$y <- model_resample_mock[[2]]
  fit2 <- lm(y~x, data = d2)
  d3 <- d
  d3$y <- model_resample_mock[[3]]
  fit3 <- lm(y~x, data = d3)
  out <- mt$get_Rn1_boot()
  expect_equal(
    out,
    list(
      Rn1_fun(r = residuals(fit1), o = order(X)),
      Rn1_fun(r = residuals(fit2), o = order(X)),
      Rn1_fun(r = residuals(fit3), o = order(X))
    ))
}
GOF_model_test_calc_Rn1_boot()

GOF_model_test_calc_pvalue <- function() {
  set.seed(1)
  X <- rnorm(10)
  Y <- 5 * X + rnorm(10)
  d <- data.frame(y = Y, x = X)
  fit <- lm(y ~ x, data = d)
  KS <- Rn1_KS$new()
  mt <- GOF_model_test$new(
    model = fit,
    data = d,
    y_name = "y",
    Rn1_statistic = KS,
    nmb_boot_samples = 10,
    gof_model_info_extractor = GOF_lm_info_extractor$new(),
    gof_model_resample = GOF_model_resample$new(
      gof_model_simulator = GOF_lm_sim_param$new(),
      gof_model_trainer = GOF_lm_trainer$new()))
  out <- mt$get_pvalue()
  expect_equal(
    out,
    {
      stat_org <- KS$calc_statistic(mt$get_Rn1_org())
      stat_boot <- sapply(mt$get_Rn1_boot(), KS$calc_statistic)
      mean(stat_org < stat_boot)
    }
  )
}
GOF_model_test_calc_pvalue()



GOF_model_test_expect_small_pvalue <- function() {
  set.seed(1)
  X1 <- rnorm(100)
  X2 <- rnorm(100)
  d <- data.frame(
    y = rpois(n = 100, lambda = exp(4 + X1 * 2 + X2 * 6)),
    x1 = X1)
  fit <- glm(y~x1, data = d, family = poisson())
  mt <- GOF_model_test$new(
    model = fit,
    data = d,
    y_name = "y",
    Rn1_statistic = Rn1_KS$new(),
    nmb_boot_samples = 100,
    gof_model_info_extractor = GOF_glm_info_extractor$new(),
    gof_model_resample = GOF_model_resample$new(
      gof_model_simulator = GOF_glm_sim_param$new(),
      gof_model_trainer = GOF_glm_trainer$new()))

  expect_equal(mt$get_pvalue(), 0)

  X1 <- rnorm(100)
  d <- data.frame(
    y = rnorm(n = 100, mean = 4 + X1^2),
    x1 = X1)
  fit <- lm(y~x1, data = d)
  ie <- GOF_lm_info_extractor$new()
  mt <- GOF_model_test$new(
    model = fit,
    data = d,
    y_name = "y",
    Rn1_statistic = Rn1_KS$new(),
    nmb_boot_samples = 100,
    gof_model_info_extractor = ie,
    gof_model_resample = GOF_model_resample$new(
      gof_model_simulator = GOF_sim_wild_rademacher$new(
        gof_model_info_extractor = ie
      ),
      gof_model_trainer = GOF_lm_trainer$new()))

  expect_equal(mt$get_pvalue(), 0)
}
GOF_model_test_expect_small_pvalue()

GOF_model_test_expect_non_small_pvalue <- function() {
  set.seed(1)
  X1 <- rnorm(100)
  d <- data.frame(
    y = rpois(n = 100, lambda = exp(4 + X1 * 2)),
    x1 = X1)
  fit <- glm(y~x1, data = d, family = poisson())
  mt <- GOF_model_test$new(
    model = fit,
    data = d,
    y_name = "y",
    Rn1_statistic = Rn1_KS$new(),
    nmb_boot_samples = 100,
    gof_model_info_extractor = GOF_glm_info_extractor$new(),
    gof_model_resample = GOF_model_resample$new(
      gof_model_simulator = GOF_glm_sim_param$new(),
      gof_model_trainer = GOF_glm_trainer$new()))

  expect_equal(mt$get_pvalue(), 0.74)

  X1 <- rnorm(100)
  d <- data.frame(
    y = rnorm(n = 100, mean = 4 + X1 + X1^2),
    x1 = X1)
  fit <- lm(y~x1 + I(x1^2), data = d)
  ie <- GOF_lm_info_extractor$new()
  mt <- GOF_model_test$new(
    model = fit,
    data = d,
    y_name = "y",
    Rn1_statistic = Rn1_CvM$new(),
    nmb_boot_samples = 100,
    gof_model_info_extractor = ie,
    gof_model_resample = GOF_model_resample$new(
      gof_model_simulator = GOF_sim_wild_rademacher$new(
        gof_model_info_extractor = ie
      ),
      gof_model_trainer = GOF_lm_trainer$new()))

  expect_equal(mt$get_pvalue(), 0.8)
}
GOF_model_test_expect_non_small_pvalue()
