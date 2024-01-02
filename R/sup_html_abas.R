#' Resultados de listas em abas (HTML)
#' @export

sup_html_abas = function(lista, sobre = "Abas", abas = NULL){

  nomes_abas = NULL
  # Usar nomes da lista como nomes das abas por padrão
  if(is.null(abas)){

    # Se a lista não tiver nome
    if(is.null(names(lista))){
      n = length(lista)
      nomes_abas = paste("Aba ",1:n)
    }
    else{ nomes_abas = names(lista)}
  }
  else{names_abas = abas}

  cat(paste0('## ', sobre, ' {.tabset}',  '\n\n'))

  for(i in 1:length(nomes_abas)){
    cat(paste0("### ", nomes_abas,"\n\n"))
    print( lista[[i]] )
    cat('\n\n')
  }

}




