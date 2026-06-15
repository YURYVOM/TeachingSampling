#' @export
#'
#' @title
#' Poisson Sampling
#' @description
#' Draws a Poisson sample from a finite population of size \code{N}.
#' Each unit \eqn{k} is independently selected with its own inclusion
#' probability \eqn{\pi_k}.
#' @return
#' A vector of length \code{N} where selected units contain their population
#' index and non-selected units contain \code{0}.
#' @details
#' Poisson sampling is a generalisation of Bernoulli sampling that allows
#' unequal inclusion probabilities. The sample size is random. To extract
#' the selected indices, use \code{sam[sam != 0]}.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param N Population size.
#' @param Pik Vector of length \code{N} containing the first-order inclusion
#'   probability for each unit in the population. Values must be in \code{(0, 1]}.
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{E.PO}}, \code{\link{PikPPS}}, \code{\link{S.piPS}}
#'
#' @examples
#' data('Lucy')
#' attach(Lucy)
#' N   <- nrow(Lucy)
#' n   <- 400
#' Pik <- PikPPS(n, Employees)
#' sam <- S.PO(N, Pik)
#' sam <- sam[sam != 0]
#' y   <- data.frame(Income = Income[sam], Expenditure = Expenditure[sam])
#' E.PO(y, Pik[sam])

S.PO <- function(N, Pik) {
  sam <- matrix(0, N, 1)
  U   <- runif(N)
  for (k in 1:N) {
    if (U[k] <= Pik[k])
      sam[k] <- k
  }
  return(sam)
}