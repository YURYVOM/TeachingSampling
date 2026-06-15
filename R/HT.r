#' @export
#'
#' @title
#' Horvitz-Thompson Estimator of the Population Total
#' @description
#' Computes the Horvitz-Thompson (HT) estimator of the population total for
#' one or more variables of interest, given the sample observations and their
#' first-order inclusion probabilities.
#' @return
#' A numeric vector or matrix with the estimated total for each variable
#' of interest.
#' @details
#' The Horvitz-Thompson estimator is defined as:
#' \deqn{\hat{t}_{y,\pi} = \sum_{k \in s} \frac{y_k}{\pi_k}}
#' where \eqn{\pi_k} is the first-order inclusion probability of unit \eqn{k}.
#' This estimator is design-unbiased for any fixed-size sampling design.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param y Vector or matrix of values of the variable(s) of interest for
#'   units in the sample.
#' @param Pik Vector of first-order inclusion probabilities for each unit
#'   in the sample.
#'
#' @references
#' Horvitz, D.G. and Thompson, D.J. (1952). A generalization of sampling
#' without replacement from a finite universe.
#' \emph{Journal of the American Statistical Association}, 47, 663-685.\cr
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.
#'
#' @seealso \code{\link{VarHT}}, \code{\link{E.SI}}, \code{\link{E.piPS}}
#'
#' @examples
#' # Population N = 5, sample size n = 2
#' N <- 5
#' n <- 2
#' p <- c(0.13, 0.2, 0.15, 0.1, 0.15, 0.04, 0.02, 0.06, 0.07, 0.08)
#' y <- c(32, 34, 46, 89, 35)
#' Ind <- Ik(N, n)
#' pik <- as.vector(Pik(p, Ind))
#' # Select first sample (units 1 and 2)
#' sam <- c(1, 2)
#' HT(y[sam], pik[sam])

HT <- function(y, Pik) {
  y   <- t(as.matrix(y))
  pik <- as.matrix(Pik)
  result <- y %*% (1/Pik)
  result
}