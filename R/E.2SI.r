#' @export
#'
#' @title
#' Estimation of the Population Total under Two Stage Simple Random Sampling
#' @description
#' Computes the Horvitz-Thompson estimator of the population total under a
#' two-stage simple random sampling without replacement design, where both
#' Primary Sampling Units (PSUs) and Secondary Sampling Units (SSUs) are
#' selected by simple random sampling without replacement.
#' @return
#' A matrix with four rows and one column per variable of interest:
#' \itemize{
#'   \item \code{Estimation}: Estimated population total.
#'   \item \code{Standard Error}: Estimated standard error of the total.
#'   \item \code{CVE}: Estimated coefficient of variation (in percentage).
#'   \item \code{DEFF}: Design effect with respect to simple random sampling.
#' }
#' @details
#' The variance estimator decomposes into two components: the between-PSU
#' component and the within-PSU component, following the classical two-stage
#' variance decomposition of Sarndal et al. (1992).
#' @author Hugo Andres Gutierrez Rojas <hagutierrezro at gmail.com>
#' @param NI Population size of Primary Sampling Units (PSUs).
#' @param nI Sample size of Primary Sampling Units (PSUs).
#' @param Ni Vector of population sizes of Secondary Sampling Units within
#'   each selected PSU.
#' @param ni Vector of sample sizes of Secondary Sampling Units within
#'   each selected PSU.
#' @param y Vector, matrix or data frame containing the values of the
#'   variables of interest for every unit in the selected sample.
#' @param PSU Vector identifying the PSU membership of each unit in the sample.
#'
#' @references
#' Sarndal, C-E. and Swensson, B. and Wretman, J. (1992),
#' \emph{Model Assisted Survey Sampling}. Springer.\cr
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas
#' y estimacion de parametros}. Editorial Universidad Santo Tomas.
#'
#' @seealso \code{\link{E.1SI}}, \code{\link{E.UC}}
#'
#' @examples
#' library(TeachingSampling)
#' data('BigCity')
#' library(dplyr)
#' Households <- BigCity %>%
#'   group_by(HHID) %>%
#'   summarise(PSU = unique(PSU),
#'             Persons = n(),
#'             Income = sum(Income),
#'             Expenditure = sum(Expenditure))
#'
#' UI <- levels(as.factor(Households$PSU))
#' NI <- length(UI)
#' nI <- 10
#' samI <- S.SI(NI, nI)
#' sampleI <- UI[samI]
#' CityI <- Households[Households$PSU %in% sampleI, ]
#'
#' Ni <- as.numeric(table(CityI$PSU))
#' ni <- ceiling(Ni * 0.2)
#'
#' estima <- data.frame(CityI$Persons, CityI$Income, CityI$Expenditure)
#' area   <- as.factor(CityI$PSU)
#'
#' E.2SI(NI, nI, Ni, ni, estima, area)

E.2SI <- function(NI, nI, Ni, ni, y, PSU) {
  y <- cbind(1, y)
  y <- as.data.frame(y)
  names(y)[1] <- "N"
  PSU <- as.factor(PSU)
  
  Total <- matrix(NA, nrow = 4, ncol = dim(y)[2])
  rownames(Total) <- c("Estimation", "Standard Error", "CVE", "DEFF")
  colnames(Total) <- names(y)
  
  f <- ni/Ni
  F <- nI/NI
  
  for (k in 1:dim(y)[2]) {
    ysum <- tapply(y[, k], PSU, sum)
    s2i  <- tapply(y[, k], PSU, var)
    ti   <- (1/f) * ysum
    ty   <- (1/F) * sum(ti)
    part.1 <- NI^2/nI * (1 - F) * var(ti)
    part.2 <- NI/nI * sum(Ni^2/ni * (1 - f) * s2i)
    Vty    <- part.1 + part.2
    CVe    <- 100 * sqrt(Vty)/ty
    n      <- length(y[, k])
    N      <- (NI/nI) * sum(Ni)
    VMAS   <- (N^2) * (1 - (n/N)) * var(y[, k])/(n)
    DEFF   <- Vty/VMAS
    Total[, k] <- c(ty, sqrt(Vty), CVe, DEFF)
  }
  return(Total)
}