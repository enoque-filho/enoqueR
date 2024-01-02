#' Tabela descritiva usando gtsummary
#'
#' @export

tabela_dataset_descritiva_gts = function(dados){
  saida =
    gtsummary::modify_header( gtsummary::add_n( gtsummary::tbl_summary(
      data      = dados,
      type      = list(where(is.numeric) ~ "continuous2"),
      statistic = list(gtsummary::all_continuous2() ~ c("{min} - {max}", "{median}",  "{mean} ({sd})")))
    ),label ~ "**Variável**")
  return(saida)
}

