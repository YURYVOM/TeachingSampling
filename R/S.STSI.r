#' @export
#'
#' @title
#' Stratified Simple Random Sampling Without Replacement
#' @description
#' Draws a stratified simple random sample without replacement from a finite
#' population. Within each stratum, units are selected by simple random
#' sampling without replacement.
#' @return
#' A sorted vector of population indices of the selected units, of length
#' \code{sum(nh)}.
#' @details
#' The function selects \code{nh[h]} units from stratum \eqn{h} using
#' \code{base::sample}, and returns all selected indices sorted in ascending
#' order. Use \code{\link{E.STSI}} to estimate population totals from this
#' sample.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param S Vector of length \code{N} identifying the stratum membership of
#'   each unit in the population.
#' @param Nh Integer vector of length \code{H} with the population size of
#'   each stratum.
#' @param nh Integer vector of length \code{H} with the sample size of each
#'   stratum. Must satisfy \code{nh[h] <= Nh[h]} for all \code{h}.
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{E.STSI}}, \code{\link{S.SI}}, \code{\link{S.STpiPS}}
#'
#' @examples
#' data('Lucy')
#' attach(Lucy)
#' N  <- nrow(Lucy)
#' Nh <- as.numeric(table(Level))
#' nh <- c(70, 100, 200)
#' sam <- S.STSI(Level, Nh, nh)
#' y   <- data.frame(Income = Income[sam], Expenditure = Expenditure[sam])
#' E.STSI(Level[sam], Nh, nh, y)

S.STSI <- function(S, Nh, nh) {
  S   <- as.factor(S)
  S   <- as.factor(as.integer(S))
  cum <- cumsum(nh)
  sam <- matrix(0, sum(nh))
  for (k in 1:length(nh)) {
    h     <- which(S == k)
    sam.h <- sample(Nh[k], nh[k])
    if (k == 1) sam[1:nh[k]] <- h[sam.h]
    if (k > 1)  sam[(cum[k-1]+1):(cum[k])] <- h[sam.h]
  }
  sort(sam)
}