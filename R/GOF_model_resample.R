##' R6 Class representing the resampling scheme for
##' Goodness-of-fit-tests for (linear) models
##' @export
GOF_model_resample <- R6::R6Class(
  classname = "GOF_model_resample",
  public = list(
    ##' @description constructor.
    ##' @param gof_model_simulator an instance that implements \link{GOF_model_simulator}
    ##' @param gof_model_trainer an instance that implements \link{GOF_model_trainer}
    ##' @return No explicit return
    initialize = function(gof_model_simulator, gof_model_trainer) {
      private$model_simulator <- gof_model_simulator
      private$model_trainer <- gof_model_trainer
    },
    ##' @description resamples the dependent variable in \code{data} and refits
    ##'   \code{model} to that new data set
    ##' @param model fitted model based on \code{data}
    ##' @param data used to fit \code{model}
    ##' @param y_name string specifying the name of the dependent variable
    ##' @return a resampled version of \code{model}
    resample = function(model, data, y_name) {
      data_new <- data
      data_new[[y_name]] <- private$model_simulator$resample_y(model = model)
      ret <- private$model_trainer$refit(model = model, data = data_new)
      return(ret)
    }),
  private = list(
    model_simulator = NULL,
    model_trainer = NULL
  )
)
