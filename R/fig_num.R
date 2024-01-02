#' Gráfico univariado para variável quantitativa
#'
#' @export


fig_num = function(dados, coluna, fill = 'steelblue', bins = 30, alpha = 0.8, ...){
  p1 <- ggplot(dados, aes(y = '', x =  {{ coluna }})) +
    geom_boxplot(fill = fill, alpha = alpha) +
    theme(axis.ticks.x = element_blank(),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text    = element_blank(),
          axis.title.x = element_blank()
    ) +
    labs(y = '')

  p2 <- ggplot(dados, aes(x = {{ coluna }}, after_stat(density))) +
    geom_density(color = fill, linewidth = 0.8) +
    geom_histogram(fill = fill, bins = bins, alpha = alpha) +
    geom_vline(aes(xintercept = mean(   {{ coluna }}, na.rm = TRUE)),
               linetype = 'dashed', color = 'red', linewidth = 0.8) +
    labs(y = 'Densidade',...)

  saida = (p1 / p2) + plot_layout(heights = c(1,5)) &
    scale_x_continuous(
      limits = c(min(pull(dados, {{coluna}}), na.rm = TRUE) - 2,
                 max(pull(dados, {{coluna}}), na.rm = TRUE) + 2))
  return(saida)
}
