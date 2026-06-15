#' @export
#'
#' @title
#' GREG Generalised Weights
#' @description
#' Computes the generalised regression (GREG) weights for each unit in the
#' sample. These weights incorporate both the sampling design weights and a
#' calibration adjustment based on known population totals of auxiliary
#' variables.
#' @return
#' A numeric vector of length \code{n} with the GREG weight for each unit
#' in the sample.
#' @details
#' The GREG weight for unit \eqn{k} is:
#' \deqn{w_k = \frac{1}{\pi_k} + \mathbf{x}_k^T
#' \left(\sum_s \frac{v_k \mathbf{x}_k \mathbf{x}_k^T}{\pi_k}\right)^{-1}
#' (\mathbf{t}_x - \hat{\mathbf{t}}_{x,\pi})}
#' where \eqn{v_k = 1/(\pi_k c_k)} and \eqn{c_k} is a variance-stabilising
#' constant. The GREG estimator is then \eqn{\hat{t}_{GREG} = \sum_s w_k y_k}.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param x Vector or matrix of auxiliary variables observed in the sample.
#' @param tx Vector of known population totals of the auxiliary variables.
#' @param Pik Vector of first-order inclusion probabilities for each unit
#'   in the sample.
#' @param ck Vector of variance-stabilising constants. Typically \code{ck = 1}
#'   (homoscedastic) or \code{ck = x} (heteroscedastic).
#' @param b0 Logical. If \code{TRUE}, an intercept column is prepended to
#'   \code{x}. Default is \code{FALSE}.
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{GREG.SI}}, \code{\link{E.Beta}}
#'
#' @examples
#' data('Lucy')
#' attach(Lucy)
#' N   <- nrow(Lucy)
#' n   <- 400
#' sam <- S.SI(N, n)
#' Pik <- rep(n/N, n)
#' x   <- as.matrix(Employees[sam])
#' tx  <- sum(Employees)
#' ck  <- rep(1, n)
#' wk  <- Wk(x, tx, Pik, ck)
#' # Check calibration: weighted sum of x equals tx
#' sum(wk * x)

Wk <- function(x, tx, Pik, ck, b0 = FALSE) {
  if (b0 == TRUE)  x <- as.matrix(cbind(1, x))
  if (b0 == FALSE) x <- as.matrix(x)
  tx    <- as.matrix(tx)
  txpi  <- as.matrix(t(x) %*% (1/Pik))
  V     <- 1/(Pik * ck)
  result <- (1/Pik) + ((V * x) %*% solve(t(V * x) %*% x) %*% (tx - txpi))
  return(result)
}