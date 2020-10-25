##' @title R6 Class representing statistics for marked
##'   empirical processes
##'
##' @description R6 does not offer interfaces. Hence all methods
##'   are considered as abstract.
##' @export
Rn1_statistic <- R6::R6Class(
  classname = "Rn1_statistic",
  public = list(
    ##' @description Abstract function that calculates the statistic
    ##'   for a given marked empirical process
    ##' @param Rn1 marked empirical process as a double vector
    ##' @return statistic based on \code{Rn1}
    calc_statistic = function(Rn1) {
      stop("Abstract method. Needs to be implemented")
    })
  )

##' @title Kolmogorov-Smirnov-statistic for marked empirical
##'   process
##'
##' @description Implements the "interface" Rn1_statistic
##' @export
Rn1_KS <- R6::R6Class(
  classname = "Rn1_KS",
  inherit = Rn1_statistic,
  public = list(
    ##' @description calculates the Kolmogorov-Smirnov-statistic
    ##' @param Rn1 see \link{Rn1_statistic}
    ##' @return see \link{Rn1_statistic}
    calc_statistic = function(Rn1) {
      ret <- max(abs(Rn1))
      return(ret)
    })
)

##' @title Cramer-von-Mises-statistic for marked empirical
##'   process
##'
##' @description Implements the "interface" Rn1_statistic
##' @export
Rn1_CvM <- R6::R6Class(
  classname = "Rn1_CvM",
  inherit = Rn1_statistic,
  public = list(
    ##' @description calculates the calculates the Cramer-von-Mises
    ##'   statistic
    ##' @param Rn1 see \link{Rn1_statistic}
    ##' @return see \link{Rn1_statistic}
    calc_statistic = function(Rn1) {
      ret <- mean(Rn1^2)
      return(ret)
    })
  )
