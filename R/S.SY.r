#' @export
#'
#' @title
#' Systematic Sampling
#' @description
#' Draws a systematic sample from a finite population of size \code{N} using
#' a fixed sampling interval \code{a}. A random start \code{r} is chosen
#' uniformly from \code{1} to \code{a}, and every \code{a}-th unit thereafter
#' is selected.
#' @return
#' A vector containing the population indices of the selected units.
#' @details
#' The random start \code{r} is drawn from \code{sample(a, 1)}, and then
#' units \eqn{r, r+a, r+2a, \ldots} are selected. If \code{N} is not a
#' multiple of \code{a}, the sample size varies by one unit depending on the
#' random start. Use \code{\link{E.SY}} to estimate population totals.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param N Population size.
#' @param a Sampling interval (skip). The expected sample size is
#'   approximately \code{N/a}.
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{E.SY}}, \code{\link{S.SI}}
#'
#' @examples
#' data('Lucy')
#' attach(Lucy)
#' N   <- nrow(Lucy)
#' a   <- 10
#' sam <- S.SY(N, a)
#' y   <- data.frame(Income = Income[sam], Expenditure = Expenditure[sam])
#' E.SY(N, a, y)

S.SY <- function(N, a) {
  r <- sample(a, 1)
  c <- N - a * floor(N/a)
  if (r <= c)
    n <- floor((N/a)) + 1
  else
    n <- floor(N/a)
  sam <- matrix(0, n, 1)
  for (k in 1:n) {
    sam[k] <- r + (a * (k - 1))
  }
  sam
}