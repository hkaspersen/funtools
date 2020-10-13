#' Clean MIC-values
#'
#' Clean MIC-value data from SensiTitre
#'
#' @param df Data frame holding the MIC-values, rows = samples, columns = MIC values, first column named "id"
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@protonmail.com}
#'
#' @export
#' @importFrom dplyr mutate
#' @importFrom dplyr recode_factor
#' @importFrom dplyr select
#' @importFrom tidyr spread
#'
clean_mic <- function(df) {
  recode_vector <- c(">1024" = 2048,
                     ">512" = 1024,
                     ">256" = 512,
                     ">128" = 256,
                     ">64" = 128,
                     ">32" = 64,
                     ">16" = 32,
                     ">8" = 16,
                     ">4" = 8,
                     ">2" = 4,
                     ">1" = 2,
                     ">0.5" = 1,
                     ">0,5" = 1,
                     ">0.25" = 0.5,
                     ">0,25" = 0.5,
                     ">0.12" = 0.25,
                     ">0,12" = 0.25,
                     "<8" = 4,
                     "<4" = 2,
                     "<2" = 1,
                     "<1" = 0.5,
                     "<0.5" = 0.25,
                     "<0,5" = 0.25,
                     "<0.25" = 0.12,
                     "<0,25" = 0.12,
                     "<0.12" = 0.06,
                     "<0,12" = 0.06,
                     "<0.06" = 0.03,
                     "<0,06" = 0.03,
                     "<0.03" = 0.015,
                     "<0,03" = 0.015,
                     "<0.015" = 0.0075,
                     "<0,015" = 0.0075,
                     "1024" = 1024,
                     "512" = 512,
                     "256" = 256,
                     "128" = 128,
                     "64" = 64,
                     "32" = 32,
                     "16" = 16,
                     "8" = 8,
                     "4" = 4,
                     "2" = 2,
                     "1" = 1,
                     "0.5" = 0.5,
                     "0,5" = 0.5,
                     "0.25" = 0.25,
                     "0,25" = 0.25,
                     "0.12" = 0.12,
                     "0,12" = 0.12,
                     "0.06" = 0.06,
                     "0,06" = 0.06,
                     "0.03" = 0.03,
                     "0,03" = 0.03,
                     "0.015" = 0.015,
                     "0,015" = 0.015)

  df %>%
    gather(key, value, -id) %>%
    mutate(value2 = recode_factor(value,
                                  !!!recode_vector,
                                  .default = levels(recode_vector)),
           value2 = as.numeric(as.character(value2))) %>%
    select(-value) %>%
    spread(key, value2)
}
