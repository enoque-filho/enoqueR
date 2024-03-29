#' Visualizar densidade de variáveis numéricas
#'
#' @import ggplot2
#' @export



eda_visual_dataset_densidade = function(dados, ...){
  saida = dados %>%
    select(where(is.numeric)) %>%
    pivot_longer(everything(), names_to = "variavel") %>%
    ggplot(aes(x = value)) +
    geom_density(...) +
    facet_wrap(~variavel, scales = "free")
  return(saida)
}
