##' @title R6 Class representing model information
##'
##' @description R6 does not offer interfaces. Hence all methods
##' are considered as abstract.
##' @export
GOF_model_info_extractor <- R6::R6Class( # nolint
  classname = "GOF_model_info_extractor",
  public = list(
    ##' @description Abstract function that estimates/predicts the
    ##'   the dependent variable in \code{model}
    ##' @param model fitted model
    ##' @return estimate/prediction of the dependent variable
    ##'   fitted by \code{model}
    yhat = function(model) {
      stop("Abstract method. Needs to be implemented")
    },
    ##' @description abstract function that calculates the residuals
    ##'   on the scale of the dependent variable.
    ##' @param model fitted model
    ##' @return residuals on the scale of the dependent variable
    y_minus_yhat = function(model) {
      stop("Abstract method. Needs to be implemented")
    },
    ##' @description abstract function that calculates the inner product
    ##'   of estimated parameters and the independent variables.
    ##' @param model fitted model
    ##' @return inner product of the estimated parameters and the
    ##'   independent variables.
    beta_x_covariates = function(model) {
      stop("Abstract method. Needs to be implemented")
    })
  )

##' @title Implements the "interface" GOF_model_info_extractor
##'   for linear models
##' @description This class is specialized in extracting various information
##'   from an object of class "lm"
##' @export
GOF_lm_info_extractor <- R6::R6Class( # nolint
  classname = "GOF_lm_info_extractor",
  inherit = GOF_model_info_extractor,
  public = list(
    ##' @description see \link{GOF_model_info_extractor}
    ##' @param model see \link{GOF_model_info_extractor}
    ##' @return see \link{GOF_model_info_extractor}
    yhat = function(model) {
      predict.lm(object = model)
    },
    ##' @description see \link{GOF_model_info_extractor}
    ##' @param model see \link{GOF_model_info_extractor}
    ##' @return see \link{GOF_model_info_extractor}
    y_minus_yhat = function(model) {
      residuals.lm(object = model)
    },
    ##' @description see \link{GOF_model_info_extractor}
    ##' @param model see \link{GOF_model_info_extractor}
    ##' @return see \link{GOF_model_info_extractor}
    beta_x_covariates = function(model) {
      predict.lm(object = model)
    }))


##' @title Implements the "interface" GOF_model_info_extractor for
##'   for generalized linear models
##' @description This class is specialized in extracting various information
##'   from an object of class "glm"
##' @export
GOF_glm_info_extractor <- R6::R6Class( # nolint
  classname = "GOF_glm_info_extractor",
  inherit = GOF_model_info_extractor,
  public = list(
    ##' @description see \link{GOF_model_info_extractor}
    ##' @param model see \link{GOF_model_info_extractor}
    ##' @return see \link{GOF_model_info_extractor}
    yhat = function(model) {
      predict.glm(object = model, type = "response")
    },
    ##' @description see \link{GOF_model_info_extractor}
    ##' @param model see \link{GOF_model_info_extractor}
    ##' @return see \link{GOF_model_info_extractor}
    y_minus_yhat = function(model) {
      residuals.glm(object = model, type = "response")
    },
    ##' @description see \link{GOF_model_info_extractor}
    ##' @param model see \link{GOF_model_info_extractor}
    ##' @return see \link{GOF_model_info_extractor}
    beta_x_covariates = function(model) {
      predict.glm(object = model, type = "link")
    }))
