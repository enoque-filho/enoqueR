#'  Função para adicionar rotulos (labelled) a um dataset
#' @export

rotular = function(dados, rotulos){
  for(i in 1:ncol(dados)){
    labelled::var_label(dados[,i]) = rotulos[i]
  }
  return(dados)
}