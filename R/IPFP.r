#' @export
#'
#' @title
#' Iterative Proportional Fitting Procedure (Raking)
#' @description
#' Adjusts a contingency table so that its row and column marginals match
#' known population totals, using the Iterative Proportional Fitting
#' Procedure (IPFP), also known as raking or RAS algorithm.
#' @return
#' A matrix with \code{nrow(Table) + 1} rows and \code{ncol(Table) + 1}
#' columns containing the adjusted cell counts, with an added row of
#' estimated column totals and an added column of estimated row totals.
#' @details
#' The algorithm alternates between row and column adjustments until
#' convergence. At each step, cells in each row (or column) are multiplied
#' by the ratio of the known marginal to the current estimated marginal.
#' Convergence is assessed by the sum of absolute differences between
#' known and estimated marginals.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param Table A matrix or data frame of initial cell counts or weights to
#'   be adjusted.
#' @param Col.knw Numeric vector of known column marginal totals.
#' @param Row.knw Numeric vector of known row marginal totals.
#' @param tol Convergence tolerance. The algorithm stops when the total
#'   absolute deviation between known and estimated marginals is below
#'   \code{tol}. Default is \code{0.0001}.
#'
#' @references
#' Deming, W.E. and Stephan, F.F. (1940). On a least squares adjustment of
#' a sampled frequency table when the expected marginal totals are known.
#' \emph{Annals of Mathematical Statistics}, 11(4), 427-444.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{Domains}}, \code{\link{Wk}}
#'
#' @examples
#' # A 2x2 table to be raked to known marginals
#' Table   <- matrix(c(10, 20, 30, 40), nrow = 2)
#' Row.knw <- c(40, 60)
#' Col.knw <- c(35, 65)
#' IPFP(Table, Col.knw, Row.knw)

IPFP <- function(Table, Col.knw, Row.knw, tol = 0.0001) {
  Table   <- as.matrix(Table)
  Col.est <- colSums(Table)
  Row.est <- rowSums(Table)
  I       <- length(Row.knw)
  J       <- length(Col.knw)
  Est     <- Table
  criterio <- sum(abs(Col.knw - Col.est)) + sum(abs(Row.knw - Row.est))
  while (criterio > tol) {
    for (i in 1:I) {
      for (j in 1:J) {
        Est[i, j] <- Est[i, j] * Row.knw[i]/Row.est[i]
      }
    }
    Col.est  <- colSums(Est)
    Row.est  <- rowSums(Est)
    criterio <- sum(abs(Col.knw - Col.est)) + sum(abs(Row.knw - Row.est))
    for (i in 1:I) {
      for (j in 1:J) {
        Est[i, j] <- Est[i, j] * Col.knw[j]/Col.est[j]
      }
    }
    Col.est  <- colSums(Est)
    Row.est  <- rowSums(Est)
    criterio <- sum(abs(Col.knw - Col.est)) + sum(abs(Row.knw - Row.est))
  }
  p1 <- rbind(Est, Col.est)
  p2 <- cbind(p1, c(Row.est, sum(Row.est)))
  colnames(p2)[J + 1] <- c("Row.est")
  return(p2)
}
