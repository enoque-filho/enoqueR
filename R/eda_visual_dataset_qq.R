#' Gráficos qq para um dataset
#'
#'
#'
#' @export

eda_visual_dataset_qq = function(dados, ncols = 2){
  dados = dplyr::select_if(dados, is.numeric)
  vars = names(dados)
  graficos = list()

  for(i in 1:length(vars)){
    d = dplyr::select(dados, var = vars[i])
    graficos[[i]] = ggplot2::ggplot(d, ggplot2::aes(sample = var)) +
      ggplot2::stat_qq() +
      ggplot2::stat_qq_line() +
      ggplot2::labs(x = "Quantis teóricos", y = "Quantis", title = vars[i]) +
      ggplot2::theme(text = ggplot2::element_text(size = 6))
  }
  return(gridExtra::grid.arrange(grobs = graficos, ncols))
}
