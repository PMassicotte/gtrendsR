#' Geographical Targeting.
#'
#' \itemize{
#'   \item Criteria.ID Unique and persistent assigned ID.
#'   \item Name Best available English name of the geo target.
#'   \item Canonical.Name The constructed fully qualified English name consisting of the target's own name, and that of its parent and country. This field is meant only for disambiguating similar target names-it is not yet supported in LocationCriterionService (use location names or criteria IDs instead).
#'   \item Parent.ID The criteria ID of a parent. This field is included for legacy support, and the IDs may not be consistent across datasets. Canonical Names is the preferred method of constructing hierarchies.
#'   \item Country.Code The ISO-3166-1 alpha-2 country code that is associated with the target.
#'   \item Target.Type Target type (Airpot, City, etc.).
#'   \item Status Is the location currently valid or not. 
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name regions
#' @usage data("regions")
#' @format A data frame with 86460 rows and 7 variables
#' @references \url{https://developers.google.com/adwords/api/docs/appendix/geotargeting} 
NULL