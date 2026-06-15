#' @export
#'
#' @title
#' Sample Probabilities under With-Replacement Sampling
#' @description
#' Computes the probability of each possible outcome in the with-replacement
#' sampling support, given unit selection probabilities \code{pk}.
#' @return
#' A numeric vector of length \code{choose(N+m-1, m)} with the probability
#' of each distinct unordered outcome in the with-replacement support.
#' @details
#' For each distinct unordered outcome (multiset) in the support enumerated
#' by \code{\link{nk}}, the probability is computed as a multinomial
#' probability:
#' \deqn{p(s) = \frac{m!}{\prod_k n_k!} \prod_k p_k^{n_k}}
#' where \eqn{n_k} is the number of times unit \eqn{k} appears in outcome
#' \eqn{s} and \eqn{p_k} is the selection probability of unit \eqn{k}.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param N Population size.
#' @param m Number of draws (sample size with replacement).
#' @param pk Vector of length \code{N} with selection probabilities for each
#'   unit. Must sum to 1.
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{nk}}, \code{\link{SupportWR}}, \code{\link{S.PPS}}
#'
#' @examples
#' # N = 3 units, m = 2 draws, equal probabilities
#' N  <- 3
#' m  <- 2
#' pk <- c(1/3, 1/3, 1/3)
#' p  <- p.WR(N, m, pk)
#' sum(p)  # must equal 1

p.WR <- function(N, m, pk) {
  p  <- rep(0, N)
  I  <- nk(N, m)
  N  <- dim(I)[1]
  for (i in 1:N) {
    ni   <- c(I[i, ])
    p[i] <- dmultinom(ni, prob = pk)
  }
  p
}