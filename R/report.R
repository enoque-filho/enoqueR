#' Saidas automaticas para listas (Fonte: Bel Carnielli)
#' @export

report = function(a){
  for(i in 1:length(a)){

    if(paste(class(a[[i]]), collapse = " ") == "list"){
      report(a[[i]]) # repetir dentro de outra lista
    }
    else{
      if(paste(class(a[[i]]), collapse = " ") == "data.frame"){
        print( knitr::kable( a[[i]] ) ) # saida para tabela
      }
      else{
        if(paste(class(a[[i]]), collapse = " ") == "character"){
          cat(c(a[[i]],"\n")) # saida para texto
        }
        else{
          print( a[[i]] ) # saida para gráfico
        }
      }
    }
  }
}



