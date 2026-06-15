#' @export
#'
#' @title
#' Stratified Probability Proportional to Size With-Replacement Sampling
#' @description
#' Draws a stratified with-replacement sample where within each stratum units
#' are selected using probability proportional to size (PPS-WR).
#' @return
#' A data frame with \code{sum(mh)} rows and two columns:
#' \itemize{
#'   \item \code{sam}: population indices of the selected units.
#'   \item \code{pk}: within-stratum selection probabilities of each draw.
#' }
#' @details
#' Within each stratum \eqn{h}, \code{mh[h]} draws are made with
#' probabilities \eqn{p_k = x_k / \sum_{k \in h} x_k}. The same unit may
#' appear more than once within a stratum. Use \code{\link{E.STPPS}} to
#' estimate population totals from this sample.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param S Vector of length \code{N} identifying the stratum membership of
#'   each unit in the population.
#' @param x Vector of length \code{N} containing positive auxiliary size
#'   values for each unit in the population.
#' @param mh Integer vector of length \code{H} specifying the number of
#'   draws within each stratum.
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{S.PPS}}, \code{\link{S.STpiPS}}, \code{\link{E.STPPS}}
#'
#' @examples
#' data('Lucy')
#' attach(Lucy)
#' mh  <- c(20, 30, 50)
#' res <- S.STPPS(Level, Employees, mh)
#' head(res)
#' sam <- res$sam
#' pk  <- res$pk
#' y   <- data.frame(Income = Income[sam], Expenditure = Expenditure[sam])
#' E.STPPS(y, pk, mh, Level[sam])

S.STPPS <- function(S, x, mh) {
  S   <- as.factor(S)
  S   <- as.factor(as.integer(S))
  cum <- cumsum(mh)
  sam <- matrix(0, sum(mh))
  pk  <- matrix(0, sum(mh))
  for (k in 1:length(mh)) {
    h    <- which(S == k)
    Nh   <- length(x[h])
    pkh  <- x[h]/sum(x[h])
    cumpk <- cumsum(pkh)
    U    <- runif(mh[k])
    ints <- cbind(c(0, cumpk[-Nh]), cumpk)
    sam.h <- rep(0, mh[k])
    for (i in 1:mh[k]) {
      sam.h[i] <- which(U[i] > ints[, 1] & U[i] < ints[, 2])
    }
    pk.h <- pkh[sam.h]
    if (k == 1) {
      sam[1:mh[k]]  <- h[sam.h]
      pk[1:mh[k]]   <- pk.h
    }
    if (k > 1) {
      sam[(cum[k-1]+1):(cum[k])] <- h[sam.h]
      pk[(cum[k-1]+1):(cum[k])]  <- pk.h
    }
  }
  data.frame(sam, pk)
}