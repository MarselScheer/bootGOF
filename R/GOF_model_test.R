##' @title R6 Class representing the Goodness-of-Fit test
##'   for (linear) models.
##' @description This class can test the null hypothesis that data follows
##'   a particular linear model, i.e. classical linear models, generalized
##'   linear models or models of the type \eqn{m(\beta^\top X) + \epsilon}.
##' @export
GOF_model_test <- R6::R6Class(
  classname = "GOF_model_test",
  public = list(
    ##' @param model a fitted model
    ##' @param data used to fit \code{model}
    ##' @param nmb_boot_samples integer specifying the number of bootstrap
    ##'   samples to perform
    ##' @param y_name string specifying the name of the dependent variable in
    ##'   in \code{data}
    ##' @param Rn1_statistic statistic used to map the marked empirical
    ##'   process to the real line. Needs to be an instance of the class
    ##'   that implements \link{Rn1_statistic}
    ##' @param gof_model_info_extractor an instance that implements
    ##'   \link{GOF_model_info_extractor} in order to apply it to
    ##'   \code{model}
    ##' @param gof_model_resample an instance that implements
    ##'   \link{GOF_model_resample} in order to apply it to
    ##'   \code{model}
    ##' @return An instance of the Class
    initialize = function(model,
                          data,
                          nmb_boot_samples,
                          y_name,
                          Rn1_statistic,
                          gof_model_info_extractor,
                          gof_model_resample) {
      checkmate::assert_count(x = nmb_boot_samples, positive = TRUE)
      private$model_org <- model
      private$data_org <- data
      private$y_name <- y_name
      private$Rn1_statistic <- Rn1_statistic
      private$nmb_boot_samples <- nmb_boot_samples
      private$model_info_extractor <- gof_model_info_extractor
      private$model_resample <- gof_model_resample
      private$order_beta_dot_X_org <- order(
        private$model_info_extractor$beta_x_covariates(model = private$model_org)
      )
      private$Rn1_statistic <- Rn1_statistic
    },
    ##' @description calculates the marked empricial process for \code{model}
    ##' @return vector ordered by the inner product of the estimated
    ##'   parameter and the independent variables
    get_Rn1_org = function() {
      if (is.null(private$Rn1_org)) {
        private$Rn1_org <- private$Rn1(
          y_minus_yhat = private$model_info_extractor$y_minus_yhat(
            model = private$model_org
          ),
          order_beta_x_covariates = private$order_beta_dot_X_org)
      }
      return(private$Rn1_org)
    },
    ##' @description calculates the marked empricial process for the
    ##'   resampled versions of \code{model}
    ##' @return list of length \code{nmb_boot_samples} where every element
    ##'   is a vector ordered by the inner product of the estimated
    ##'   parameter and the dependent variables
    get_Rn1_boot = function() {
      if (is.null(private$Rn1_boot)) {
        private$resample_Rn1()
      }
      return(private$Rn1_boot)
    },
    ##' @description p-value for Goodness-of-Fit-test for \code{model}
    ##' @return p-value for the null hypothesis that the dependent variable
    ##'   was generated according to \code{model}
    get_pvalue = function() {
      stat_org <- private$Rn1_statistic$calc_statistic(Rn1 = self$get_Rn1_org())
      stat_boot <- sapply(
        X = self$get_Rn1_boot(),
        FUN = private$Rn1_statistic$calc_statistic
      )
      mean(stat_org < stat_boot)
    }),
  private = list(
    nmb_boot_samples = NULL,
    model_info_extractor = NULL,
    model_resample = NULL,
    Rn1_statistic = NULL,
    Rn1_boot = NULL,
    Rn1_org = NULL,
    model_org = NULL,
    data_org = NULL,
    y_name = NULL,
    order_beta_dot_X_org = NULL,
    Rn1 = function(y_minus_yhat, order_beta_x_covariates) {
      ret <- cumsum(x = y_minus_yhat[order_beta_x_covariates])
      return(ret)
    },
    resample_Rn1 = function() {
      f <- function(dummy) {
        fit_boot <- private$model_resample$resample(
          model = private$model_org,
          data = private$data_org,
          y_name = private$y_name)
        Rn1_boot <- private$Rn1(
          y_minus_yhat = private$model_info_extractor$y_minus_yhat(model = fit_boot),
          order_beta_x_covariates = private$order_beta_dot_X_org)
        return(Rn1_boot)
      }
      private$Rn1_boot <- lapply(X = 1:private$nmb_boot_samples, FUN = f)
    }))
