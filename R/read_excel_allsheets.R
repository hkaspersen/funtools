#' Import each excel sheet
#'
#' Import each excel sheet and concatenate to a data frame or tibble.
#'
#' @param filename The name of the excel file
#' @param tibble output as tibble or data frame? (TRUE/FALSE)
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@protonmail.com}
#'
#' @export
#'
#' @importFrom readxl excel_sheets
#' @import tibble
#'
read_excel_allsheets <- function(filename, tibble = TRUE) {
  # set tibble = FALSE if data frames are preferred
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
