#' Tabela descritiva usando gtsummary
#'
#' @export

tabela_dataset_descritiva_gts = function(dados){
  saida =
    gtsummary::tbl_summary(
      data      = dados,
      type      = list(where(is.numeric) ~ "continuous2"),
      statistic = list(gtsummary::all_continuous2() ~ c("{min} - {max}", "{median}",  "{mean} ({sd})"))
  ) %>%
  gtsummary::add_n() %>%
  gtsummary::modify_header(label ~ "**Variável**")
  return(saida)
}

