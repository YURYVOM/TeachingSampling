#' @export
#'
#' @title
#' Bernoulli Sampling
#' @description
#' Draws a Bernoulli sample from a finite population of size \code{N}.
#' Each unit is independently selected with the same inclusion probability
#' \code{prob}.
#' @return
#' A vector of length \code{N} where selected units contain their population
#' index and non-selected units contain \code{0}.
#' @details
#' The sample size under Bernoulli sampling is random, following a
#' Binomial(\code{N}, \code{prob}) distribution. To extract the selected
#' indices, use \code{sam[sam != 0]}.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param N Population size.
#' @param prob Scalar. Inclusion probability, must satisfy \code{0 < prob <= 1}.
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{E.BE}}, \code{\link{S.PO}}, \code{\link{S.SI}}
#'
#' @examples
#' # Population of size N = 100, inclusion probability 10%
#' N    <- 100
#' prob <- 0.1
#' sam  <- S.BE(N, prob)
#'
#' # Extract selected indices
#' selected <- sam[sam != 0]
#' length(selected)  # random, around 10
#'
#' # Using Lucy data
#' data('Lucy')
#' N    <- nrow(Lucy)
#' prob <- 0.05
#' sam  <- S.BE(N, prob)
#' sam  <- sam[sam != 0]
#' y    <- data.frame(Income = Lucy$Income[sam])
#' E.BE(y, prob)

S.BE <- function(N, prob) {
  sam <- matrix(0, N, 1)
  U   <- runif(N)
  for (k in 1:N) {
    if (U[k] <= prob)
      sam[k] <- k
  }
  return(sam)
}