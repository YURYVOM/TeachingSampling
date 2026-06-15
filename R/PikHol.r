#' @export
#'
#' @title
#' Optimal Inclusion Probabilities for Multiple Surveys (Holmberg)
#' @description
#' Computes optimal first-order inclusion probabilities for a population that
#' is surveyed on multiple occasions, minimising a measure of total variance
#' across surveys. This implements the approach of Holmberg (2002) for
#' coordinated sampling over time.
#' @return
#' A numeric vector of length \code{N} with the optimal inclusion probability
#' for each unit in the population.
#' @details
#' For each survey \eqn{k}, the initial inclusion probabilities are computed
#' via \code{\link{PikPPS}}. An optimal composite size measure is then derived
#' by combining the per-survey auxiliary variables through a weighted sum, and
#' the final inclusion probabilities are computed proportional to the square
#' root of this composite. The resulting sample size \code{n.st} is chosen to
#' minimise total variance subject to a relative precision target \code{e}.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param n Integer vector of length \code{p} with the desired sample size
#'   for each of the \code{p} surveys.
#' @param sigma Matrix of dimension \code{N x p} where column \eqn{k} contains
#'   the auxiliary size variable for survey \eqn{k}.
#' @param e Scalar. Relative tolerance parameter controlling the precision
#'   target across surveys.
#' @param Pi Optional matrix of dimension \code{N x p} with initial inclusion
#'   probabilities for each survey. If omitted, \code{\link{PikPPS}} is used.
#'
#' @references
#' Holmberg, A. (2002). A multiparameter perspective on the choice of sampling
#' design in surveys. \emph{Statistics in Transition}, 5(6), 969-994.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{PikPPS}}, \code{\link{PikSTPPS}}
#'
#' @examples
#' data('Lucy')
#' attach(Lucy)
#' # Two surveys with different auxiliary variables
#' sigma <- cbind(Employees, Income)
#' n     <- c(100, 150)
#' pik   <- PikHol(n, sigma, e = 0.1)
#' sum(pik <= 1)  # all valid probabilities

PikHol <- function(n, sigma, e, Pi = NULL) {
  N <- dim(sigma)[1]
  p <- length(n)
  if (is.null(Pi)) {
    Pi <- matrix(NA, nrow = N, ncol = p)
    for (k in 1:p) {
      Pi[, k] <- PikPPS(n[k], sigma[, k])
    }
  }
  A <- matrix(NA, nrow = N, ncol = p)
  for (k in 1:p) {
    A[, k] <- sigma[, k]^2/(sum(((1/Pi[, k]) - 1) * sigma[, k]^2))
  }
  aqk    <- rowSums(A)
  n.st   <- ceiling(((sum(sqrt(aqk)))^2)/((1 + e) * p + (sum(aqk))))
  pikopt <- PikPPS(n.st, sqrt(aqk))
  return(pikopt)
}