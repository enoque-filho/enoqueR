#' Resumo das variáveis numéricas
#'
#' @export


#   Tabela com medidas descritivas (mean, sd, etc)

eda_tabela_dataset_resumo_num = function(dados){
  dados = dplyr::select_if(dados, is.numeric)

  resu = rstatix::get_summary_stats(dados) |>
    dplyr::select(variable, n, mean, sd, min, max, q1, median, q3, iqr)
  names(resu) = c("Variável","N", "Média", "Variância", "Mínimo",
                  "Máximo", "Q1", "Mediana", "Q3", "IQR")
  if(nrow(dados) <= 5000){
    teste  = rstatix::shapiro_test(dados, vars = names(dados))
    names(teste) = c("Variável", "W","p")
    saida = dplyr::left_join(resu, teste,
                              by = c("Variável" = "Variável"))
  }
  else{
    saida = resu
  }
  return(saida)
}
