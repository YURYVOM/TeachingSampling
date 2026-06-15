#' @export
#'
#' @title
#' Simple Random Sampling Without Replacement
#' @description
#' Draws a simple random sample of size \code{n} without replacement from a
#' finite population of size \code{N} using the sequential algorithm of
#' Fan, Muller and Rezucha (1962).
#' @return
#' A vector of length \code{N} where selected units contain their population
#' index and non-selected units contain \code{0}.
#' @details
#' The sequential algorithm selects units one at a time by comparing a uniform
#' random variate with the conditional inclusion probability at each step,
#' ensuring exactly \code{n} units are selected. To extract the selected
#' indices, filter out the zeros: \code{sam[sam != 0]}.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param N Population size.
#' @param n Sample size. Must satisfy \code{n <= N}.
#' @param e Optional vector of \code{N} uniform random variates in \code{(0,1)}.
#'   If omitted, \code{runif(N)} is used. Useful for reproducibility or
#'   coordinated sampling.
#'
#' @references
#' Fan, C.T., Muller, M.E. and Rezucha, I. (1962). Development of sampling
#' plans by using sequential (item by item) selection techniques and digital
#' computers. \emph{Journal of the American Statistical Association},
#' 57(298), 387-402.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{E.SI}}, \code{\link{S.STSI}}, \code{\link{S.SY}}
#'
#' @examples
#' data('Lucy')
#' attach(Lucy)
#' N   <- nrow(Lucy)
#' n   <- 400
#' sam <- S.SI(N, n)
#' sam <- sam[sam != 0]
#' y   <- data.frame(Income = Income[sam], Expenditure = Expenditure[sam])
#' E.SI(N, n, y)

S.SI <- function(N, n, e = runif(N)) {
  c   <- matrix(0, N, 1)
  dec <- matrix(0, N, 1)
  sam <- matrix(0, N, 1)
  for (k in 1:N) {
    c[k] <- (n - dec[k])/(N - k + 1)
    if (e[k] < c[k]) {
      dec[k:N] <- dec[k] + 1
      sam[k]   <- k
    }
  }
  sam
}