#' Informações do dataset
#'
#' @export

info = function(dados){
  Info = c("Observações", "Variáveis", "Numérico", "Categórico", "Lógico")
  N = c(dim(dados)[1],
        dim(dados)[2],
        ncol(dplyr::select_if(dados, is.numeric)),
        sum(ncol(dplyr::select_if(dados, is.factor)),
            ncol(dplyr::select_if(dados, is.character))),
        ncol(dplyr::select_if(dados, is.logical)))
  return(info = dplyr::tibble(Info, N))
}
