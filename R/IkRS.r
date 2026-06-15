#' @export
#'
#' @title
#' Sample Membership Indicator Matrix for All Possible Sample Sizes
#' @description
#' Constructs the indicator matrix of the complete sampling support, stacking
#' the indicator matrices for all sample sizes from 1 to \code{N}. This
#' covers every possible non-empty subset of the population.
#' @return
#' A binary matrix with \eqn{2^N} rows (one per non-empty subset, including
#' the empty set as the first row of zeros) and \code{N} columns. Entry
#' \eqn{(s, k) = 1} if unit \eqn{k} belongs to subset \eqn{s}.
#' @details
#' This function calls \code{\link{Ik}} for each possible sample size
#' \eqn{n = 1, \ldots, N} and stacks the results. It is intended for small
#' populations only (\code{N <= 10}) due to the exponential growth of the
#' support size.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param N Population size. Recommended \code{N <= 10}.
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{Ik}}, \code{\link{SupportRS}}
#'
#' @examples
#' # Full indicator matrix for N = 3
#' IkRS(3)
#' # Number of rows: 1 (empty) + 3 + 3 + 1 = 8 = 2^3
#' nrow(IkRS(3))

IkRS <- function(N) {
  sam <- matrix(0, ncol = N, nrow = 1)
  for (k in 1:N) {
    sam <- rbind(sam, Ik(N, k))
  }
  sam
}