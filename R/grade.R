#' grid.arrange de gridExtra resumido
#'
#'@export


grade = function(graficos, n_colunas){
  return(gridExtra::grid.arrange(grobs = graficos, ncol = n_colunas))
}
