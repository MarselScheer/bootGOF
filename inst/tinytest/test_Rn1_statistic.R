Rn1_statistic_all_methods_are_abstract <- function() {
  stat <- Rn1_statistic$new()
  expect_error(
    stat$calc_statistic(Rn1 = 1:100),
    pattern = "Abstract")

}
Rn1_statistic_all_methods_are_abstract()

Rn1_KS_calculates_Kolomogorov_Smirnoff <- function() {
  stat <- Rn1_KS$new()
  r <- runif(10)
  expect_equal(
    stat$calc_statistic(Rn1 = r),
    max(r))
}
Rn1_KS_calculates_Kolomogorov_Smirnoff()

Rn1_CvM_calculates_Cramer_von_Mises <- function() {
  stat <- Rn1_CvM$new()
  r <- runif(10)
  expect_equal(
    stat$calc_statistic(Rn1 = r),
    mean(r^2))
}
Rn1_CvM_calculates_Cramer_von_Mises()
