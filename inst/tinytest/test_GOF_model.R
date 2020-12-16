dummy_lm_model <- function() {
  set.seed(1)
  X <- rnorm(10)
  Y <- 5 * X + rnorm(10)
  d <- data.frame(y = Y, x = X)
  fit <- lm(y ~ x, data = d)
  ret <- list(fit = fit, X = X, Y = Y, d = d)
  return(ret)
}

dummy_glm_model <- function() {
  set.seed(1)
  X <- 1:10
  Y <- 1:10
  d <- data.frame(y = Y, x = X)
  fit <- glm(y ~ x, data = d, family = poisson())
  ret <- list(fit = fit, X = X, Y = Y, d = d)
  return(ret)
}

GOF_model_resample_dummy <- R6::R6Class(
  classname = "dummy",
  public = list(
    initialize = function(gof_model_simulator, gof_model_trainer) {
    }))

GOF_model_test_dummy <- R6::R6Class(
  classname = "dummy",
  public = list(
    initialize = function(model,
                          data,
                          nmb_boot_samples,
                          y_name,
                          Rn1_statistic,
                          gof_model_info_extractor,
                          gof_model_resample) {
    }))

GOF_model_uses_lm_info_extractor <- function() {
  dummy_lm <- dummy_lm_model()

  inject_lm_info_extractor <- FALSE
  GOF_model_test_spy <- R6::R6Class(
    classname = "GOF_model_test",
    public = list(
      initialize = function(model,
                            data,
                            nmb_boot_samples,
                            y_name,
                            Rn1_statistic,
                            gof_model_info_extractor,
                            gof_model_resample) {
        inject_lm_info_extractor <<- inherits(
          x = gof_model_info_extractor,
          what = "GOF_lm_info_extractor")
      }))

  GOF_model(
    model = dummy_lm$fit,
    simulator_type = "parametric",
    gof_model_resample_class = GOF_model_resample_dummy,
    gof_model_test_class = GOF_model_test_spy)
  expect_true(inject_lm_info_extractor)
}
GOF_model_uses_lm_info_extractor()

GOF_model_uses_lm_trainer <- function() {
  dummy_lm <- dummy_lm_model()

  inject_lm_trainer <- FALSE
  GOF_model_resample_spy <- R6::R6Class(
    classname = "GOF_model_resample",
    public = list(
      initialize = function(gof_model_simulator, gof_model_trainer) {
        inject_lm_trainer <<- inherits(
          x = gof_model_trainer,
          what = "GOF_lm_trainer")
      }))

  GOF_model(
    model = dummy_lm$fit,
    simulator_type = "parametric",
    gof_model_test_class = GOF_model_test_dummy,
    gof_model_resample_class = GOF_model_resample_spy)
  expect_true(inject_lm_trainer)
}
GOF_model_uses_lm_trainer()

GOF_model_uses_lm_parametric_simulator <- function() {
  dummy_lm <- dummy_lm_model()

  inject_lm_param_simulator <- FALSE
  GOF_model_resample_spy <- R6::R6Class(
   classname = "GOF_model_resample",
    public = list(
      initialize = function(gof_model_simulator, gof_model_trainer) {
        inject_lm_param_simulator <<- inherits(
          x = gof_model_simulator,
          what = "GOF_lm_sim_param")
      }))

  GOF_model(
    model = dummy_lm$fit,
    simulator_type = "parametric",
    gof_model_test_class = GOF_model_test_dummy,
    gof_model_resample_class = GOF_model_resample_spy)
  expect_true(inject_lm_param_simulator)
}
GOF_model_uses_lm_parametric_simulator()

GOF_model_uses_lm_rademacher_simulator <- function() {
  dummy_lm <- dummy_lm_model()

  inject_lm_rademacher_simulator <- FALSE
  GOF_model_resample_spy <- R6::R6Class(
   classname = "GOF_model_resample",
    public = list(
      initialize = function(gof_model_simulator, gof_model_trainer) {
        inject_lm_rademacher_simulator <<- inherits(
          x = gof_model_simulator,
          what = "GOF_lm_sim_wild_rademacher")
      }))

  GOF_model(
    model = dummy_lm$fit,
    simulator_type = "semi_parametric_rademacher",
    gof_model_test_class = GOF_model_test_dummy,
    gof_model_resample_class = GOF_model_resample_spy)
  expect_true(inject_lm_rademacher_simulator)
}
GOF_model_uses_lm_rademacher_simulator()

GOF_model_uses_unknow_simulator_type <- function() {
  expect_error(
    GOF_model(simulator_type = "sthelse"),
    pattern = "simulator_type.*failed")
}
GOF_model_uses_unknow_simulator_type()

GOF_model_uses_glm_parametric_simulator <- function() {
  dummy_glm <- dummy_glm_model()

  inject_glm_param_simulator <- FALSE
  GOF_model_resample_spy <- R6::R6Class(
   classname = "GOF_model_resample",
    public = list(
      initialize = function(gof_model_simulator, gof_model_trainer) {
        inject_glm_param_simulator <<- inherits(
          x = gof_model_simulator,
          what = "GOF_glm_sim_param")
      }))

  GOF_model(
    model = dummy_glm$fit,
    simulator_type = "parametric",
    gof_model_test_class = GOF_model_test_dummy,
    gof_model_resample_class = GOF_model_resample_spy)
  expect_true(inject_glm_param_simulator)
}
GOF_model_uses_glm_parametric_simulator()

GOF_model_uses_glm_info_extractor <- function() {
  dummy_glm <- dummy_glm_model()

  inject_glm_info_extractor <- FALSE
  GOF_model_test_spy <- R6::R6Class(
    classname = "GOF_model_test",
    public = list(
      initialize = function(model,
                            data,
                            nmb_boot_samples,
                            y_name,
                            Rn1_statistic,
                            gof_model_info_extractor,
                            gof_model_resample) {
        inject_glm_info_extractor <<- inherits(
          x = gof_model_info_extractor,
          what = "GOF_glm_info_extractor")
      }))

  GOF_model(
    model = dummy_glm$fit,
    simulator_type = "parametric",
    gof_model_resample_class = GOF_model_resample_dummy,
    gof_model_test_class = GOF_model_test_spy)
  expect_true(inject_glm_info_extractor)
}
GOF_model_uses_glm_info_extractor()

GOF_model_uses_glm_trainer <- function() {
  dummy_glm <- dummy_glm_model()

  inject_glm_trainer <- FALSE
  GOF_model_resample_spy <- R6::R6Class(
    classname = "GOF_model_resample",
    public = list(
      initialize = function(gof_model_simulator, gof_model_trainer) {
        inject_glm_trainer <<- inherits(
          x = gof_model_trainer,
          what = "GOF_glm_trainer")
      }))

  GOF_model(
    model = dummy_glm$fit,
    simulator_type = "parametric",
    gof_model_test_class = GOF_model_test_dummy,
    gof_model_resample_class = GOF_model_resample_spy)
  expect_true(inject_glm_trainer)
}
GOF_model_uses_glm_trainer()


GOF_model_expect_small_pvalue <- function() {
  set.seed(1)
  X1 <- rnorm(100)
  X2 <- rnorm(100)
  d <- data.frame(
    y = rpois(n = 100, lambda = exp(4 + X1 * 2 + X2 * 6)),
    x1 = X1)
  fit <- glm(y~x1, data = d, family = poisson())
  mt <- GOF_model(
    model = fit,
    data = d,
    nmb_boot_samples = 100,
    simulator_type = "parametric",
    y_name = "y",
    Rn1_statistic = Rn1_KS$new())

  expect_equal(mt$get_pvalue(), 0)

  X1 <- rnorm(100)
  d <- data.frame(
    y = rnorm(n = 100, mean = 4 + X1^2),
    x1 = X1)
  fit <- lm(y~x1, data = d)
  mt <- GOF_model(
    model = fit,
    data = d,
    nmb_boot_samples = 100,
    simulator_type = "semi_parametric_rademacher",
    y_name = "y",
    Rn1_statistic = Rn1_KS$new())

  expect_equal(mt$get_pvalue(), 0)
}
GOF_model_expect_small_pvalue()

GOF_model_expect_non_small_pvalue <- function() {
  set.seed(1)
  X1 <- rnorm(100)
  d <- data.frame(
    y = rpois(n = 100, lambda = exp(4 + X1 * 2)),
    x1 = X1)
  fit <- glm(y~x1, data = d, family = poisson())
  mt <- GOF_model(
    model = fit,
    data = d,
    nmb_boot_samples = 100,
    simulator_type = "parametric",
    y_name = "y",
    Rn1_statistic = Rn1_KS$new())

  expect_equal(mt$get_pvalue(), 0.74)

  X1 <- rnorm(100)
  d <- data.frame(
    y = rnorm(n = 100, mean = 4 + X1 + X1^2),
    x1 = X1)
  fit <- lm(y~x1 + I(x1^2), data = d)
  mt <- GOF_model(
    model = fit,
    data = d,
    nmb_boot_samples = 100,
    simulator_type = "semi_parametric_rademacher",
    y_name = "y",
    Rn1_statistic = Rn1_CvM$new())

  expect_equal(mt$get_pvalue(), 0.8)
}
GOF_model_expect_non_small_pvalue()

GOF_model_error_for_glm_semi_parametric <- function() {
  set.seed(1)
  X1 <- rnorm(100)
  d <- data.frame(
    y = rpois(n = 100, lambda = exp(4 + X1 * 2)),
    x1 = X1)
  fit <- glm(y~x1, data = d, family = poisson())
  expect_error(
    GOF_model(
      model = fit,
      data = d,
      nmb_boot_samples = 100,
      simulator_type = "semi_parametric_rademacher",
      y_name = "y",
      Rn1_statistic = Rn1_KS$new()),
    pattern = "Ordinary Least Square estimate necessary for semi_parameteric_rademacher"
  )
}
GOF_model_error_for_glm_semi_parametric()
