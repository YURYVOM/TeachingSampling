#' @export
#'
#' @title
#' Stratified Probability Proportional to Size Without-Replacement Sampling
#' @description
#' Draws a stratified sample where within each stratum units are selected
#' using a probability proportional to size without-replacement (piPS) design.
#' @return
#' A matrix with \code{sum(nh)} rows and two columns, sorted by population
#' index:
#' \itemize{
#'   \item Column 1: population indices of the selected units.
#'   \item Column 2: first-order inclusion probabilities of the selected units.
#' }
#' @details
#' Within each stratum \eqn{h}, the function calls \code{\link{S.piPS}} to
#' draw \code{nh[h]} units with probabilities proportional to \code{x}.
#' The global population indices are preserved in the output.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param S Vector of length \code{N} identifying the stratum membership of
#'   each unit in the population.
#' @param x Vector of length \code{N} containing positive auxiliary size
#'   values for each unit in the population.
#' @param nh Integer vector of length \code{H} specifying the sample size
#'   within each stratum.
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{S.piPS}}, \code{\link{S.STSI}}, \code{\link{E.STpiPS}},
#'   \code{\link{PikSTPPS}}
#'
#' @examples
#' data('Lucy')
#' attach(Lucy)
#' N  <- nrow(Lucy)
#' n1 <- 70; n2 <- 100; n3 <- 200
#' nh <- c(n1, n2, n3)
#' res <- S.STpiPS(Level, Employees, nh)
#' head(res)
#' sam <- res[, 1]
#' Pik <- res[, 2]
#' y   <- data.frame(Income = Income[sam], Expenditure = Expenditure[sam])
#' E.STpiPS(y, Pik, Level[sam])

S.STpiPS <- function(S, x, nh) {
  S   <- as.factor(S)
  S   <- as.factor(as.integer(S))
  res <- matrix(NA, nrow = sum(nh), ncol = 2)
  cum <- cumsum(nh)
  for (k in 1:length(nh)) {
    h     <- which(S == k)
    res.h <- S.piPS(nh[k], x[h])
    sam.h <- res.h[, 1]
    pik.h <- res.h[, 2]
    if (k == 1) {
      res[1:nh[k], 1] <- h[sam.h]
      res[1:nh[k], 2] <- pik.h
    }
    if (k > 1) {
      res[(cum[k-1]+1):(cum[k]), 1] <- h[sam.h]
      res[(cum[k-1]+1):(cum[k]), 2] <- pik.h
    }
  }
  res[order(res[, 1]), ]
}