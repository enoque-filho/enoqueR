#' Gráficos de dispersão 2x2 usando ggplot2
#'
#'
#' @export



eda_visual_dataset_disper = function(dados, ncols = 2){
  dados = dplyr::select_if(dados, is.numeric)
  variaveis = names(dados)
  n = 1
  plots = list()
  for (i in 1:ncol(dados)){
    for(j in 1:ncol(dados)){
      if(i != j){
        d = dplyr::select(dados, X = variaveis[i], Y = variaveis[j])
        plots[[n]] =
          ggplot2::ggplot(d, ggplot2::aes(x = X, y = Y)) +
          ggplot2::geom_point(color = "black") +
          ggplot2::labs(x = variaveis[i], y = variaveis[j]) +
          ggplot2::theme(text = ggplot2::element_text(size = 6))
        n = n + 1
      }
    }
  }
  return(gridExtra::grid.arrange(grobs = plots, ncols))
}

