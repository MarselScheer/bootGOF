GOF_model_test_injector <- function(model,
                                    data,
                                    nmb_boot_samples,
                                    simulator_type,
                                    y_name,
                                    Rn1_statistic,
                                    gof_model_resample_class,
                                    gof_model_test_class) {
  checkmate::assert_subset(
    x = simulator_type,
    choices = c("parametric", "semi_parametric_rademacher"))

  simulators <- list(
    lm = list(
      parametric = GOF_lm_sim_param,
      semi_parametric_rademacher = GOF_lm_sim_wild_rademacher),
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
    ms <- simulators[["lm"]][[simulator_type]]$new()
    mt <- GOF_lm_trainer$new()
    mie <- GOF_lm_info_extractor$new()
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
