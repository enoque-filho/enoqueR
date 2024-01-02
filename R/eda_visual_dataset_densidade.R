#' Visualizar densidade de variáveis numéricas
#'
#' @import ggplot2
#' @export


eda_visual_dataset_densidade = function(dados, ncols = 2){
  dados = dplyr::select_if(dados, is.numeric)
  vars = names(dados)
  graficos = list()

  for(i in 1:length(vars)){
    d = dplyr::select(dados, var = vars[i])
    graficos[[i]] = ggplot(d, aes(x = var)) +
      geom_density() +
      labs(x = vars[i], y = "Densidade")
  }
  return(gridExtra::grid.arrange(grobs = graficos, ncols))
}
