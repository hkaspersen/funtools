#' Paste together unique values
#'
#' Paste together unique values in a column, and collapse with specified symbol.
#'
#' @param x The column holding the vaues
#' @param collapse Delimiter between the unique values, default ","
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@protonmail.com}
#'
#' @export
#'
func_paste <- function(x, collapse = ",") paste(unique(x[!is.na(x)]), collapse = collapse)
