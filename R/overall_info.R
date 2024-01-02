#' Informações do dataset
#'
#' @export

overall_info = function(dados){
  saida = data.frame(
    x = c(
      "Numero de variaveis",
      "Numero de observacoes",
      "Numero de celulas vazias",
      "Linhas duplicadas",
      "Tamanho de memoria"),
    n = c(
      dim(dados)[2],                   # Número de variáveis
      dim(dados)[1],                   # Número de observações
      sum(is.na(dados)),               # Número de celulas vazias
      nrow(dados[duplicated(dados),]), # Linhas duplicadas
      pryr::object_size(dados))        # Tamanho de memória
  )
  return(saida)
}
