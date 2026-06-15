#' @export
#'
#' @title
#' Sample Membership Indicator Matrix
#' @description
#' Constructs the indicator matrix of the sampling support for a fixed-size
#' without-replacement design. Each row corresponds to one possible sample
#' and each column to one population unit.
#' @return
#' A binary matrix of dimension \code{choose(N, n) x N}, where entry
#' \eqn{(s, k) = 1} if unit \eqn{k} belongs to sample \eqn{s}, and 0
#' otherwise.
#' @details
#' The full enumeration of all \code{choose(N, n)} possible samples is
#' computationally feasible only for small populations. For \code{N > 15}
#' this function will be very slow. It is intended primarily for theoretical
#' illustrations and teaching purposes.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param N Population size. Recommended \code{N <= 15}.
#' @param n Sample size.
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{Pik}}, \code{\link{Pikl}}, \code{\link{Support}}
#'
#' @examples
#' # All possible samples of size n = 2 from N = 4 units
#' N <- 4
#' n <- 2
#' Ik(N, n)
#' # Number of rows equals choose(N, n) = 6
#' nrow(Ik(N, n)) == choose(N, n)

Ik <- function(N, n) {
  Q <- Support(N, n, ID = FALSE)
  I <- matrix(0, choose(N, n), N)
  for (i in 1:n) {
    for (j in 1:choose(N, n)) {
      for (k in 1:N) {
        if (Q[j, i] == k)
          I[j, k] <- 1
      }
    }
  }
  I
}