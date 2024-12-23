#' GPP sample data
#'
#' A subset of data from the original General Population Poll Data.
#' Countries and years have been renamed and group into three fictional
#' countries and years.
#'
#' @format ## `gpp`
#' A data frame with 750 rows and 19 columns:
#' \describe{
#'   \item{country}{Country name}
#'   \item{year}{Year}
#'   \item{gend}{Gender}
#'   \item{age}{Age}
#'   \item{q1x}{Trust in X institution}
#'   \item{q49x}{Justice System evaluation}
#'   ...
#' }
#' @source <https://worldjusticeproject.org/>
"gpp"

#' Rule of Law Index Historical Data
#'
#' Index scores, at country-year level, for all factors and subfactors
#' from 2012 to 2024.
#'
#' @format ## `roli`
#' A data frame with 1,341 rows and 57 columns:
#' \describe{
#'   \item{country}{Country name}
#'   \item{year}{Year}
#'   \item{roli}{Rule of Law Index Score}
#'   \item{f1, f2, f3, f4, f5, f6, f7, f8}{Factor scores}
#'   \item{sfxx}{Subfactor scores}
#'   ...
#' }
#' @source <https://worldjusticeproject.org/>
"roli"