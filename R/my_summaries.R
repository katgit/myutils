
#' Summary for a numeric vector
#'
#' \code{numeric_summary} calculated extended summary
#' for a numeric vector
#'
#' @param x numeric vector
#' @param na.rm whether to include NA values in the calculations
#'
#' @return a named numeric vector with 6 values
#' @import dplyr
#' @export
#'
#' @examples
#'
#' x <- rnorm(100)
#' numeric_summary(x)
#'
numeric_summary <- function(x, na.rm=FALSE){

  if (!is.null(x) && !is.numeric(x))
    stop("'x' must be a numeric vector")

  min=min(x,na.rm=na.rm)
  max=max(x,na.rm=na.rm)
  mean=mean(x,na.rm=na.rm)
  sd=sd(x,na.rm=na.rm)
  length = length(x)
  Nmiss = sum(is.na(x))
  y <- dplyr::tibble(a = x, b = x)


  c(min = min, max = max, mean = mean, sd=sd, length=length, Nmiss=Nmiss)

}

#' Summary for a character vector
#'
#' Display summary for a character value
#'
#' @param x character vector
#' @param na.rm whether to ignore NA values
#'
#'
#' @return a numeric vector with 3 values
#' @export
#'
#' @examples
#'
#' x <- c("Boston", "New York", "Boston", "New Haven")
#' char_summary(x)
#'
char_summary <- function(x, na.rm=FALSE){


  if (!is.null(x) && !is.character(x))
    stop("'x' must be a character vector")


  length = length(x)
  Nmiss = sum(is.na(x))
  Nunique = length(unique(x))

  c(length = length,
    Nmiss = Nmiss,
    Nunique = Nunique )

}
