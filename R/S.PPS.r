#' @export
#'
#' @title
#' Probability Proportional to Size With-Replacement Sampling
#' @description
#' Draws a with-replacement sample of size \code{m} from a finite population
#' using probabilities proportional to an auxiliary size variable \code{x}.
#' @return
#' A matrix with \code{m} rows and two columns:
#' \itemize{
#'   \item Column 1 (\code{sam}): population indices of the selected units.
#'   \item Column 2 (\code{pk}): selection probability of each draw.
#' }
#' @details
#' At each draw, unit \eqn{k} is selected with probability
#' \eqn{p_k = x_k / \sum x}. Since sampling is with replacement, the same
#' unit may appear more than once. Use \code{\link{E.PPS}} or \code{\link{HH}}
#' to estimate population totals from this sample.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param m Number of draws (sample size with replacement).
#' @param x Vector of length \code{N} containing positive auxiliary size
#'   values for each unit in the population.
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{E.PPS}}, \code{\link{HH}}, \code{\link{S.piPS}}
#'
#' @examples
#' data('Lucy')
#' attach(Lucy)
#' m   <- 400
#' res <- S.PPS(m, Employees)
#' sam <- res[, 1]
#' pk  <- res[, 2]
#' y   <- data.frame(Income = Income[sam], Expenditure = Expenditure[sam])
#' E.PPS(y, pk)

S.PPS <- function(m, x) {
  N     <- length(x)
  pk    <- x/sum(x)
  cumpk <- cumsum(pk)
  U     <- runif(m)
  ints  <- cbind(c(0, cumpk[-N]), cumpk)
  sam   <- rep(0, m)
  for (i in 1:m) {
    sam[i] <- which(U[i] > ints[, 1] & U[i] < ints[, 2])
  }
  return(cbind(sam, pk[sam]))
}