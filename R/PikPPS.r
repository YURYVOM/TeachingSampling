#' @export
#'
#' @title
#' Inclusion Probabilities Proportional to Size
#' @description
#' Computes first-order inclusion probabilities proportional to an auxiliary
#' size variable \code{x} for a without-replacement sample of size \code{n}.
#' A sequential truncation algorithm ensures all probabilities are at most 1.
#' @return
#' A numeric vector of length \code{N} with the first-order inclusion
#' probability for each unit in the population. Values are in \code{(0, 1]}.
#' @details
#' The initial probabilities \eqn{\pi_k = n x_k / \sum x} may exceed 1 for
#' large units. The algorithm iteratively sets those probabilities to 1 and
#' redistributes the remaining sample size among the other units until all
#' probabilities are valid. The result satisfies \eqn{\sum \pi_k = n}.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param n Desired sample size.
#' @param x Vector of length \code{N} with positive auxiliary size values
#'   for each unit in the population.
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{S.piPS}}, \code{\link{PikSTPPS}}, \code{\link{PikHol}}
#'
#' @examples
#' data('Lucy')
#' attach(Lucy)
#' N   <- nrow(Lucy)
#' n   <- 400
#' Pik <- PikPPS(n, Employees)
#' # Check: sum equals n
#' sum(Pik)
#' # All values are valid probabilities
#' all(Pik > 0 & Pik <= 1)

PikPPS <- function(n, x) {
  pik <- n * x/sum(x)
  while ((sum(pik > 1)) != 0) {
    s      <- which(pik >= 1)
    new    <- (1:length(pik))[-s]
    pik[s] <- 1
    txnew  <- sum(x[s])
    for (k in new) {
      pik[k] <- (n - length(s)) * x[k]/(sum(x) - txnew)
    }
  }
  pik
}