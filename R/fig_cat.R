#' Gráfico univariado para variável qualitativa
#'
#' @export

fig_cat = function(dados, coluna,...){
  saida <-
    count(dados,{{ coluna }}) %>%
    mutate(lab = paste0(n,' (', round(prop.table(n)*100, 2), '%)')) %>%
    ggplot(aes(x = {{ coluna }}, y = n, label = lab)) +
    geom_col(fill = 'gray50') +
    geom_text(vjust = -0.5, size = 3) +
    labs(y = 'Frequência',...)
  return(saida)
}

