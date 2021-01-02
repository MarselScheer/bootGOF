##' @title Convenience function for creating a GOF-test for statistical models
##'
##' @description Simplifies the creation of an instance of
##'   \link{GOF_model_test}
##' @param model of class 'lm' or 'glm'. Caution with MASS::glm.nb, see
##'   vignette 'New-Models' for more details.
##' @param data see \link{GOF_model_test}
##' @param nmb_boot_samples see \link{GOF_model_test}
##' @param simulator_type either "parameteric" or "semi_parameteric_rademacher"
##' @param y_name see \link{GOF_model_test}
##' @param Rn1_statistic see \link{GOF_model_test}
##' @param gof_model_resample_class no need to change this parameter. Here the
##'   class used for resampling the model (\link{GOF_model_resample})
##'   is injected. This parameter simply makes it easier to test the
##'   convenience function properly.
##' @param gof_model_test_class no need to change this parameter. Here the
##'   class used for performing the GOF test (\link{GOF_model_test})
##'   is injected. This parameter simply makes it easier to test the
##'   convenience function properly.
##' @export
##' @return instance of \link{GOF_model_test}
GOF_model <- function(model,
                      data,
                      nmb_boot_samples,
                      simulator_type,
                      y_name,
                      Rn1_statistic,
                      gof_model_resample_class = GOF_model_resample,
                      gof_model_test_class = GOF_model_test
                      ) {
  checkmate::assert_subset(
    x = simulator_type,
    choices = c("parametric", "semi_parametric_rademacher"))
  checkmate::assert_multi_class(x = model, classes = c("lm", "glm"))
  if (inherits(x = model, what = "negbin")) {
    warning("The GOF-test requires to refit the model. Refitting MASS::glm.nb can be problematic, see vignette New-Models")
  }



  simulators <- list(
    lm = list(
      parametric = GOF_lm_sim_param,
      semi_parametric_rademacher = GOF_sim_wild_rademacher),
    glm = list(
      parametric = GOF_glm_sim_param,
      semi_parametric_rademacher = list(
        new = function() stop(
          paste(
            "Ordinary Least Square estimate necessary for semi_parameteric_rademacher.",
            "But MLE is used for GLMs."
          )))
    )
  )

  if (inherits(x = model, what = "glm")) {
    ms <- simulators[["glm"]][[simulator_type]]$new()
    mt <- GOF_glm_trainer$new()
    mie <- GOF_glm_info_extractor$new()
  } else if (inherits(x = model, what = "lm")) {
    mt <- GOF_lm_trainer$new()
    mie <- GOF_lm_info_extractor$new()
    
    if (simulator_type == "parametric") {
      ms <- simulators[["lm"]][[simulator_type]]$new()
    } else {
      ms <- simulators[["lm"]][[simulator_type]]$new(
        gof_model_info_extractor = mie
      )
    }
  }

  model_resample <- gof_model_resample_class$new(
    gof_model_simulator = ms,
    gof_model_trainer = mt
  )

  ret <- gof_model_test_class$new(
    model = model,
    data = data,
    nmb_boot_samples = nmb_boot_samples,
    y_name = y_name,
    Rn1_statistic = Rn1_statistic,
    gof_model_info_extractor = mie,
    gof_model_resample = model_resample)
  return(ret)
}
