#' @export
#'
#' @title
#' Simple Random Sampling With Replacement
#' @description
#' Draws a simple random sample of size \code{m} with replacement from a
#' finite population of size \code{N}. Returns the frequency of selection
#' for each unit drawn at least once.
#' @return
#' A vector of population indices of length \code{m}, where each element is
#' the index of a selected unit. Units may appear more than once.
#' @details
#' The number of times each unit is selected follows a multinomial
#' distribution with equal probabilities \eqn{1/N}. The function uses a
#' sequential binomial draw approach. Use \code{\link{E.WR}} to estimate
#' population totals.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param N Population size.
#' @param m Number of draws (sample size with replacement).
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{E.WR}}, \code{\link{S.SI}}, \code{\link{S.PPS}}
#'
#' @examples
#' data('Lucy')
#' attach(Lucy)
#' N   <- nrow(Lucy)
#' m   <- 400
#' sam <- S.WR(N, m)
#' y   <- data.frame(Income = Income[sam], Expenditure = Expenditure[sam])
#' E.WR(N, m, y)

S.WR <- function(N, m) {
  nk <- rep(0, N)
  for (k in 1:N) {
    suma  <- sum(nk)
    nk[k] <- rbinom(1, (m - suma), (1/(N - k + 1)))
  }
  x   <- which(nk > 0)
  w   <- nk[x]
  sam <- rep(x[1], w[1])
  if (length(x) == 1) return(sam)
  if (length(x) > 1) {
    for (i in 2:length(x)) {
      sam <- c(sam, rep(x[i], w[i]))
    }
  }
  sam
}