#' Estrutura dos dados
#' Data de atualização 23/09/2022
#' @import dplyr
#' @export

eda_tabela_dataset_estrutura = function(dados){
  dados = data.frame(dados)
  saida =
    dplyr::tibble(Nome = names(dados),
                  Tipo = t(dplyr::summarise_all(dados, class))[,1],
                  N = colSums(!is.na(dados)),
                  `N ausentes` = colSums(is.na(dados)),
                  `% ausentes` = (colSums(is.na(dados))/nrow(dados))*100,
                  `N distintos` = t(dplyr::summarise_all(dados, dplyr::n_distinct))[,1])
  return(saida)
}