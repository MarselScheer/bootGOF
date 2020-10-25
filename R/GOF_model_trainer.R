##' R6 Class representing a trainer for fitting models
##'
##' R6 does not offer interfaces. Hence all methods
##' are considered as abstract.
##' @export
GOF_model_trainer <- R6::R6Class(
  classname = "GOF_model_trainer",
  public = list(
    ##' @description Abstract function refits the model to
    ##'   a new data set
    ##' @param model fitted model
    ##' @param data used for refitting the model
    ##' @return \code{model} refitted on \code{data}
    refit = function(model, data) {
      stop("Abstract method. Needs to be implemented")
    }))


##' @title Implements the "interface" GOF_model_trainer for
##'   for linear models
##' @export
GOF_lm_trainer <- R6::R6Class(
  classname = "GOF_lm_trainer",
  public = list(
    ##' @description see \link{GOF_model_trainer}
    ##' @param model see \link{GOF_model_trainer}
    ##' @param data see \link{GOF_model_trainer}
    ##' @return see \link{GOF_model_trainer}
    refit = function(model, data) {
      update(object = model, data = data)
    }))

##' @title Implements the "interface" GOF_model_trainer for
##'   for generalized linear models
##' @export
GOF_glm_trainer <- R6::R6Class(
  classname = "GOF_glm_trainer",
  public = list(
    ##' @description see \link{GOF_model_trainer}
    ##' @param model see \link{GOF_model_trainer}
    ##' @param data see \link{GOF_model_trainer}
    ##' @return see \link{GOF_model_trainer}
    refit = function(model, data) {
      update(object = model, data = data)
    }))
