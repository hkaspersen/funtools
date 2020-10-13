#' Create MIC-distribution table
#'
#' Create a MIC-distribution table from clean MIC-data from SensiTitre
#'
#' @param df The data frame holding the MIC-values and grouping data
#' @param group_col the name of the column with the grouping data
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@protonmail.com}
#'
#' @export
#'
#' @import dplyr
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#'
create_mic_dist <- function(df, group_col) {

  res_perc_table <- df %>%
    # convert MIC-values to resistant/nonresistant groups
    mutate(SMX_R = ifelse(SMX > 64, 1, 0),
           TMP_R = ifelse(TMP > 2, 1, 0),
           CIP_R = ifelse(CIP > 0.06, 1, 0),
           TET_R = ifelse(TET > 8, 1, 0),
           MERO_R = ifelse(MERO > 0.12, 1, 0),
           AZI_R = ifelse(AZI > 16, 1, 0),
           NAL_R = ifelse(NAL > 8, 1, 0),
           FOT_R = ifelse(FOT > 0.25, 1, 0),
           CHL_R = ifelse(CHL > 16, 1, 0),
           TAZ_R = ifelse(TAZ > 0.5, 1, 0),
           TGC_R = ifelse(TGC > 0.5, 1, 0),
           COL_R = ifelse(COL > 2, 1, 0),
           AMP_R = ifelse(AMP > 8, 1, 0),
           GEN_R = ifelse(GEN > 2, 1, 0)) %>%
    select(!! sym(group_col), contains("_R")) %>%
    gather(amr, value, -!! sym(group_col)) %>%
    group_by_all() %>%
    count() %>%
    ungroup() %>%
    mutate(value = ifelse(value == 1, "resistant","sensitive")) %>%
    spread(value, n, fill = 0) %>%
    # calculate percentages
    mutate(total= resistant + sensitive,
           percent = round(resistant/total*100, 1),
           amr = sub("_R", "", amr)) %>%
    select(amr, !! sym(group_col), total, percent) %>%
    arrange(!! sym(group_col))


  df %>%
    select(-id) %>%
    gather(amr, value, -!! sym(group_col)) %>%
    group_by_all() %>%
    count() %>%
    ungroup() %>%
    left_join(res_perc_table) %>%
    mutate(perc = round(n/total*100, 1),
           index = 1:n()) %>%
    spread(value, perc) %>%
    select(-c(n,index)) %>%
    group_by(!! sym(group_col), amr) %>%
    summarise_all(list(func_paste)) %>%
    ungroup()
}
