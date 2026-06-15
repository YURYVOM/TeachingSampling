#' @export
#'
#' @title
#' Frequency Indicator Matrix for With-Replacement Sampling
#' @description
#' Constructs the indicator matrix of the with-replacement sampling support
#' for a population of size \code{N} and \code{m} draws. Each row corresponds
#' to one possible ordered outcome and each column to one population unit,
#' with entry \eqn{(s, k) = 1} if unit \eqn{k} was selected at least once
#' in outcome \eqn{s}.
#' @return
#' A binary matrix of dimension \code{choose(N+m-1, m) x N}, where entry
#' \eqn{(s, k) = 1} if unit \eqn{k} appears in the \eqn{s}-th outcome of
#' the with-replacement support, and 0 otherwise.
#' @details
#' The with-replacement support is enumerated via \code{\link{SupportWR}}.
#' This function is intended for small populations and few draws only, as the
#' support grows rapidly with \code{N} and \code{m}.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param N Population size. Keep small due to combinatorial growth.
#' @param m Number of draws (sample size with replacement).
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{Ik}}, \code{\link{SupportWR}}, \code{\link{nk}}
#'
#' @examples
#' # With-replacement support: N = 3 units, m = 2 draws
#' N <- 3
#' m <- 2
#' IkWR(N, m)
#' # Number of rows = choose(N + m - 1, m) = choose(4, 2) = 6
#' nrow(IkWR(N, m)) == choose(N + m - 1, m)

IkWR <- function(N, m) {
  Q <- SupportWR(N, m, ID = FALSE)
  I <- matrix(0, choose(N + m - 1, m), N)
  for (i in 1:m) {
    for (j in 1:choose(N + m - 1, m)) {
      for (k in 1:N) {
        if (Q[j, i] == k)
          I[j, k] <- 1
      }
    }
  }
  I
}