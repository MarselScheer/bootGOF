##' R6 Class representing generator of the dependent variable
##'
##' R6 does not offer interfaces. Hence all methods
##' are considered as abstract.
GOF_model_simulator <- R6::R6Class(
  classname = "GOF_model_simulator",
  public = list(
    ##' @description Abstract function that resamples the dependent
    ##'   variable
    ##' @param model fitted model
    ##' @return resamples the dependent variable according to
    ##'   the \code{model}
    resample_y = function(model) {
      stop("Abstract method. Needs to be implemented")
    }))

##' Implements the "interface" GOF_model_simulator for
##' for linear models
GOF_lm_sim_param <- R6::R6Class(
  classname = "GOF_lm_sim_param",
  ##' @description resamples the dependent variables based
  ##'   on the parameteric nature defined by \code{model}
  ##' @param model see \link{GOF_model_simulator}
  ##' @return see \link{GOF_model_simulator}
  public = list(
    resample_y = function(model) {
      simulate(model)[,1]
    }))
##' Generates Rademacher distributed random variables
##'
##' @param n number of random variables to be generated
##' @return vector of values following the Rademacher distribution
rrademacher <- function(n) {
  ret <- 2 * rbinom(n = n, size = 1, prob = 0.5) - 1
  return(ret)
}


##' Implements the "interface" GOF_model_simulator for
##' for linear models
GOF_lm_sim_wild_rademacher <- R6::R6Class(
  classname = "GOF_lm_sim_wild_rademacher",
  public = list(
    ##' @description a wild bootstrap using Rademacher random
    ##'   variables to resample the dependent variable
    ##' @param model see \link{GOF_model_simulator}
    ##' @return see \link{GOF_model_simulator}
    resample_y = function(model) {
      eps <- residuals.lm(object = model, type = "response")
      yhat <- predict.lm(object = model, type = "response")
      r <- rrademacher(n = length(eps))
      ret <- yhat + r * eps
      return(ret)
    }))

##' Implements the "interface" GOF_model_simulator for
##' for generalized linear models
GOF_glm_sim_param <- R6::R6Class(
  classname = "GOF_glm_sim_param",
  public = list(
    ##' @description resamples the dependent variables based
    ##'   on the parameteric nature defined by \code{model}
    ##' @param model see \link{GOF_model_simulator}
    ##' @return see \link{GOF_model_simulator}
    resample_y = function(model) {
      simulate(model)[,1]
    }))
