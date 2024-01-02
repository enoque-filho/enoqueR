#' Teste de normalidade para todos as variaveis do dataset:
#' @export

dataset_normalidade = function(dados, ...){
  dados = dplyr::select_if(data.frame(dados), is.numeric)
  variavel = names(dados)

  teste = NULL
  estatistica = NULL
  pvalor = NULL

  for(i in 1:ncol(dados)){
    teste[i]       = teste_normalidade(dados[,i])[1]
    estatistica[i] = round(as.numeric(teste_normalidade(dados[,i])[2]), 3)
    pvalor[i]      = round(as.numeric(teste_normalidade(dados[,i])[3]), 3)
  }
  return(data.frame(variavel, teste, estatistica, pvalor))
}
