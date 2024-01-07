#' Visualizar variáveis categoricas
#'
#' @import ggplot2
#' @import dplyr
#' @export

eda_visual_dataset_cat = function(dados, ncol = NULL, n_levels = 10){

  saida =
  dados %>%
    select(!where(is.numeric)) %>%
    pivot_longer(everything(), names_to = "variavel") %>%
    count(variavel, value)  %>%
    mutate(lab = paste0(n,' (', round(prop.table(n)*100, 2), '%)')) %>%
    slice_max(order_by = n, by = variavel, n = n_levels) %>%
    ggplot(aes(x = value, y = n, label = lab)) +
    geom_col(fill = "gray50") +
    geom_text(vjust = -0.5, size = 3) +
    labs(y = "Frequência") +
    facet_wrap(~variavel, scales = "free", ncol = ncol)
  return(saida)
  }

