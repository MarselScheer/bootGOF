##' Extracts the scalar product of beta with the covariates
##'
##' @param model fit inherited from class 'lm'.
##' @return vector of scalar products of the estimated beta
##'   with the covariates stored in \code{model}
calc_beta_times_covariates <- function(model) {
  checkmate::assert_class(x = model, classes = "lm")
  ret <- predict.lm(model, type = "response")
  return(ret)
}

##' Extracts the target-name used in the model definition
##'
##' @param model an object that returns a formula using formula().
##' @return The left hand side of the formula extracted from
##'   \code{model}
get_Y_name <- function(model) {
  frml <- formula(x = model)
  if (length(frml) != 3) {
    stop(sprintf(
      fmt = "Formula '%s' contained in the model does not contain a left hand side",
      as.character(frml)))
  }

  ret <- as.character(frml[2])
  return(ret)
}

##' Refits a model using new data
##'
##' @param model to be refitted using \code{data}
##' @param data used to refit \code{model}
##' @return \code{model} but reffitted using \code{data}
refit_model <- function(model, data) {
  update(object = model, data = data)
}
##' Calculates the marked empirical process
##'
##' @param Y the dependent variable
##' @param Y_hat estimate of the dependent variable
##' @param order_beta_time_covariates order of the scalar product
##'   of the estimated beta and the covariates.
##' @return the marked empirical process evaluated at every
##'   position of the (ordered) scalar of the estimated beta
##'   and the covariate
Rn1 <- function(Y, Y_hat, order_beta_time_covariates) {
  err <- Y - Y_hat
  err <- err[order_beta_time_covariates]
  ret <- cumsum(err) / sqrt(length(Y))
  return(ret)
}

##' Cramer von Mises distance for a marked empirical process
##'
##' @param Rn1_vec marked empirical process evaluated at every
##'   position of the scalar of the estimated beta and the covariate
##' @return Cramer von Mises distance of \code{Rn1_vec}
##' @export
Rn1_CvM <- function(Rn1_vec) {
  ret <- mean(Rn1_vec^2)
  return(ret)
}
##' Kolmogorov-Smirnov distance for a marked empirical process
##'
##' @param Rn1_vec marked empirical process evaluated at every
##'   position of the scalar of the estimated beta and the covariate
##' @return Kolmogorov-Smirnov distance of \code{Rn1_vec}
##' @export
Rn1_KS <- function(Rn1_vec) {
  ret <- max(abs(Rn1_vec))
  return(ret)
}
##' Bootstraps the marked empirical process
##'
##' It simulates the dependent variable, refits the model and
##' calculates the marked emp. process
##' @param data that was used to fit \code{model}
##' @param model that is bootstrapped
##' @param order_beta_time_covariates order of the scalar product
##'   of the estimated beta and the covariates.
##' @param Y_name name of the dependent variables
##' @param simulate_Y function that simulates the dependent
##'   according to \code{model} and returns a vector
##' @param refit_model function that refits the \code{model}
##'   on the bootstrapped data
##' @param B number of bootstraps
##' @return list with \code{B} entries, every entry is a
##'   marked empirical process
resample_Rn1 <- function(data,
                                model,
                                order_beta_time_covariates,
                                Y_name,
                                simulate_Y,
                                refit_model,
                                B = 1000) {
  ret <- lapply(
    X = seq_len(B),
    FUN = function(bootstrap_index) {
      data[[Y_name]] <- simulate_Y()
      fit_boot <- refit_model(model = model, data = data)
      Rn1_boot <- Rn1(
        Y = data[[Y_name]],
        Y_hat = predict(object = fit_boot, type = "response"),
        order_beta_time_covariates = order_beta_time_covariates)
      return(Rn1_boot)
    })
  return(ret)
}

##' Performs a Goodness-of-fit test for GLMs or LSE based models.
##'
##' @param data that was used to fit \code{model}
##' @param model that is bootstrapped
##' @param type string. "parameteric" or "wild" in order
##'   to perform the parameteric bootstrap for GLMs or
##'   semi-parametric (wild) bootstrap for least square estimate
##'   based model
##' @param statistic function that is the functional for the
##'   marked empirical process. The package provides \link{KS}
##'   and \link{CvM}.
##' @param B number of bootstraps
##' @return a pvalue for the Goodness-of-fit of \code{model}
##' @export
gof_model <- function(data,
                      model,
                      type = c("parametric", "wild"),
                      statistic,
                      B) {
  Y_name <- get_Y_name(model = model)
  Y <- data[[Y_name]]
  Y_hat <- predict(object = model, type = "response")
  btx <- calc_beta_times_covariates(model = model)
  o_btx <- order(btx)
  Rn1_org <- Rn1(
    Y = Y,
    Y_hat = Y_hat,
    order_beta_time_covariates = o_btx)

  stat_org <- statistic(Rn1_org)
  if (type == "parametric") {
    sim_Y <- function() simulate(object = model)[, "sim_1"]
  } else if (type == "wild") {
    eps <- Y_hat - Y
    N <- length(eps)
    sim_Y <- function() {
      r <- 2 * rbinom(n = N, size = 1, prob = 0.5) - 1
      Y_hat + r * eps
    }
  }

  Rn1_boot <- resample_Rn1(
    data = data,
    model = model,
    order_beta_time_covariates = o_btx,
    Y_name = Y_name,
    simulate_Y = sim_Y,
    refit_model = refit_model,
    B = B
  )
  stat_boot <- sapply(Rn1_boot, statistic)
  ret <- list(pvalue = mean(x = stat_org < stat_boot))
  return(ret)
}
