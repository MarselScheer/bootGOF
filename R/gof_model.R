pseudo_gof_model <- function(data,
                      model,
                      m = 1000,
                      calc_beta_times_covariates
                      calc_Y_hat
                      get_Y_name
                      refit_model,
                      simulate_Y,
                      type = c("parametric", "wild"),
                      statistic = c("KS", "CvM")) {

  checkmate::assert_subset(type)
  checkmate::assert_subset(statistic)

  Y_name <- get_Y_name(model = model)
  if(checkmate::check_false(Y_name %in% names(data))) {
    stop(sprintf("According to get_Y_name() the name of Y should be %s. But cannot find it in the data-object"))
  }

  if (missing(calc_beta_times_covariates)) {
    calc_beta_times_covariates <- GOF:::calc_beta_times_covariates
  }
  metric_fun <- ifelse(statistic == "KS", GOF:::KS, GOF:::CvM)

  Y <- data[[Y_name]]
  b_times_x <- calc_beta_times_covariates(model = model)
  Y_hat <- calc_Y_hat(model = model)
  metric <- metric_fun(
    Rn1 = Rn1(
      Y = Y,
      Y_hat = Y_hat,
      beta_times_covariates = b_times_x
    )
  )


  data_boot <- data

  metric_boot <- sapply(seq_len(B), function(boot_iter) {
    data_boot[[Y_name]] <- simulate_Y()

    model_boot <- refit_model(model = model, data = data_boot)

    return(metric(model = model_boot))
  })

  out <- list(
    metric = metric,
    metric_boot = metric_boot,
    pvalue = mean(metric_boot > metric)
  )

  return(out)
}
