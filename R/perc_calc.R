#' Calculate the percent occurrence of 1 and 0 in columns
#'
#' The function calculates the percent occurrence of each column variable for all rows, where one row is one observation.
#'
#' @param df The data frame holding the values. Here, each column need to be one variable, f.ex. presence/absence of a gene, and the rows represent observations. Only values 1/0 are allowed in the data frame. Column names act as variable names in the resulting data frame
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@protonmail.com}
#'
#' @export
#'
perc_calc <- function(df) {
  df %>%
    gather(key, value) %>%
    group_by_all() %>%
    count() %>%
    ungroup() %>%
    mutate(value = if_else(value == "0", "Absent", "Present")) %>%
    spread(value, n, fill = 0) %>%
    mutate(Total = Absent + Present,
           Percent = round(Present / Total * 100, 1))
}
