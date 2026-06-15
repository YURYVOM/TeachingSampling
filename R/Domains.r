#' @export
#'
#' @title
#' Domain Indicator Matrix
#' @description
#' Creates a binary indicator matrix that identifies the domain membership
#' of each unit in the sample. Each column corresponds to one domain
#' (level of \code{y}) and each row to one unit.
#' @return
#' A binary matrix of dimension \code{n x D}, where \code{D} is the number
#' of domains (levels of \code{y}). Entry \eqn{(k, d) = 1} if unit \eqn{k}
#' belongs to domain \eqn{d}, and 0 otherwise. Column names are the domain
#' labels.
#' @details
#' This function is useful for domain estimation, where population totals or
#' means must be estimated for subgroups of the population. The indicator
#' matrix can be multiplied element-wise with the variable of interest to
#' restrict estimation to each domain.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param y A vector (factor or coercible to factor) identifying the domain
#'   membership of each unit in the sample.
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{E.SI}}, \code{\link{E.STSI}}
#'
#' @examples
#' data('Lucy')
#' attach(Lucy)
#' N   <- nrow(Lucy)
#' n   <- 400
#' sam <- S.SI(N, n)
#' # Level has 3 domains: Small, Medium, Big
#' dom <- Domains(Level[sam])
#' head(dom)
#' colSums(dom)  # sample sizes per domain

Domains <- function(y) {
  y   <- as.factor(y)
  d   <- as.double(y)
  n   <- length(d)
  Dom <- matrix(0, n, max(d))
  colnames(Dom) <- levels(y)
  for (k in 1:max(d)) {
    Dom[, k] <- as.double(d == k)
  }
  Dom
}