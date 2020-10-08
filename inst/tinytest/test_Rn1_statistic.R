CRn1_statistic_all_methods_are_abstract <- function() {
  stat <- CRn1_statistic$new()
  expect_error(
    stat$calc_statistic(Rn1 = 1:100),
    pattern = "Abstract")

}
CRn1_statistic_all_methods_are_abstract()

CRn1_KS_calculates_Kolomogorov_Smirnoff <- function() {
  stat <- CRn1_KS$new()
  r <- runif(10)
  expect_equal(
    stat$calc_statistic(Rn1 = r),
    max(r))
}
CRn1_KS_calculates_Kolomogorov_Smirnoff()

CRn1_CvM_calculates_Cramer_von_Mises <- function() {
  stat <- CRn1_CvM$new()
  r <- runif(10)
  expect_equal(
    stat$calc_statistic(Rn1 = r),
    mean(r^2))
}
CRn1_CvM_calculates_Cramer_von_Mises()
