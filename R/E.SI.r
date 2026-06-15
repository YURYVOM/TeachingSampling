#' @export
#'
#' @title
#' Estimation of the Population Total under Simple Random Sampling Without
#' Replacement
#' @description
#' Computes the Horvitz-Thompson estimator of the population total under a
#' simple random sampling without replacement (SI) design.
#' @return
#' A matrix with four rows and one column per variable of interest:
#' \itemize{
#'   \item \code{Estimation}: Estimated population total.
#'   \item \code{Standard Error}: Estimated standard error of the total.
#'   \item \code{CVE}: Estimated coefficient of variation (in percentage).
#'   \item \code{DEFF}: Design effect (always 1 under SI, included for
#'     consistency with other estimators).
#' }
#' @details
#' Under simple random sampling without replacement, the Horvitz-Thompson
#' estimator reduces to \eqn{\hat{t}_y = N \bar{y}_s}, the expansion
#' estimator. The design effect is always 1 because SI is the reference design.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param N Population size.
#' @param n Sample size.
#' @param y Vector, matrix or data frame containing the values of the
#'   variables of interest for every unit in the selected sample.
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{S.SI}}, \code{\link{E.STSI}}, \code{\link{GREG.SI}}
#'
#' @examples
#' data('Lucy')
#' attach(Lucy)
#' N   <- nrow(Lucy)
#' n   <- 400
#' sam <- S.SI(N, n)
#' y   <- data.frame(Income = Income[sam], Expenditure = Expenditure[sam])
#' E.SI(N, n, y)

E.SI <- function(N, n, y) {
  y <- cbind(1, y)
  y <- as.data.frame(y)
  names(y)[1] <- "N"
  Total <- matrix(NA, nrow = 4, ncol = dim(y)[2])
  rownames(Total) <- c("Estimation", "Standard Error", "CVE", "DEFF")
  colnames(Total) <- names(y)
  pik <- matrix(n/N, nrow = n, ncol = 1)
  dk  <- 1/pik
  for (k in 1:dim(y)[2]) {
    ty   <- sum(y[, k] * dk)
    Vty  <- (N^2) * (1 - (n/N)) * var(y[, k])/(n)
    CVe  <- 100 * sqrt(Vty)/ty
    VMAS <- (N^2) * (1 - (n/N)) * var(y[, k])/(n)
    DEFF <- Vty/VMAS
    Total[, k] <- c(ty, sqrt(Vty), CVe, DEFF)
  }
  return(Total)
}