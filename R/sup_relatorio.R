#' Saidas automaticas para listas (Fonte: Bel Carnielli)
#' @param lista objeto da classe list()
#' @export

sup_relatorio = function(lista){
  for(i in 1:length(lista)){

    if(paste(class(lista[[i]]), collapse = " ") == "list"){
      sup_relatorio(lista[[i]]) # repetir dentro de outra lista
    }
    else{
      if(paste(class(lista[[i]]), collapse = " ") == "data.frame"){
        print( knitr::kable( lista[[i]] ) ) # saida para tabela
      }
      else{
        if(paste(class(lista[[i]]), collapse = " ") == "character"){
          cat(c(lista[[i]],"\n")) # saida para texto
        }
        else{
          print( lista[[i]] ) # saida para gráfico
        }
      }
    }
  }
}



