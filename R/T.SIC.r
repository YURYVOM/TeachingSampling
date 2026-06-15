#' @export
#'
#' @title
#' Cluster Totals for Single-Stage Cluster Sampling
#' @description
#' Computes the total of each variable of interest within each cluster
#' (Primary Sampling Unit) in a single-stage cluster sample.
#' @return
#' A matrix with one row per cluster and one column per variable of interest
#' (plus a first column \code{Ni} with the cluster size). Row names are the
#' cluster labels.
#' @details
#' This function aggregates the sample data by cluster, producing the cluster-
#' level totals needed for estimation under single-stage cluster sampling.
#' The output can be passed directly to \code{\link{E.1SI}} or \code{\link{E.SI}}
#' treating each cluster total as an observation.
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param y Vector, matrix or data frame containing the values of the
#'   variables of interest for every unit in the sample.
#' @param Cluster Vector identifying the cluster (PSU) membership of each
#'   unit in the sample.
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{E.1SI}}, \code{\link{E.2SI}}
#'
#' @examples
#' library(dplyr)
#' data('BigCity')
#' UI  <- levels(as.factor(BigCity$PSU))
#' NI  <- length(UI)
#' nI  <- 10
#' sam <- S.SI(NI, nI)
#' sampleI  <- UI[sam[sam != 0]]
#' CityI    <- BigCity[BigCity$PSU %in% sampleI, ]
#' y        <- data.frame(Income = CityI$Income,
#'                        Expenditure = CityI$Expenditure)
#' cluster  <- CityI$PSU
#' T.SIC(y, cluster)

T.SIC <- function(y, Cluster) {
  Cluster <- as.factor(Cluster)
  y       <- cbind(1, y)
  y       <- as.data.frame(y)
  names(y)[1] <- "Ni"
  nI      <- length(levels(Cluster))
  Total   <- matrix(NA, nrow = nI, ncol = dim(y)[2])
  rownames(Total) <- levels(Cluster)
  colnames(Total) <- names(y)
  Cluster <- as.factor(as.integer(Cluster))
  for (k in 1:nI) {
    e      <- which(Cluster == k)
    ye     <- y[e, ]
    ye     <- as.matrix(ye)
    tye    <- colSums(ye)
    Total[k, ] <- tye
  }
  Total <- as.matrix(Total)
  return(Total)
}