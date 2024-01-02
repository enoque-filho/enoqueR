#' Tipos de variáveis presentes no conjunto de dados
#'
#'
#' @export

overall_tipos = function(dados){
  saida = data.frame(
    x = c("Numerico",
          "Categorico",
          "Caractere",
          "Logico",
          "Data"),
    n = c(
      ncol(dplyr::select_if(dados, is.numeric)),   # Numérico
      ncol(dplyr::select_if(dados, is.factor)),    # Categórico
      ncol(dplyr::select_if(dados, is.character)), # Caractére
      ncol(dplyr::select_if(dados, is.logical)),   # Lógico
      ncol(select_if(dados, lubridate::is.Date))) # Data
  )
  return(saida)
}
