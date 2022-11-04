#' Resultados para relatório de análise descritiva
#'
#'
#' @export


analise_descritiva = function(dados){

  dados = data.frame(dados)
  variaveis = names(dados)

  plots   = list()
  tabela = list()
  tabelas = list()
  resultados = list()
  textos  = NULL
  classe  = NULL

  for(i in 1:length(variaveis)){

    vetor = dados[,variaveis[i]]
    classe = class(vetor)

    if(classe %in% c("numeric","double","integer")){

      tabela[[i]] = tab_desc_num(vetor, variaveis[i])
      plots       = visual_desc_num(vetor, variaveis[i])
      textos      = texto_desc_num(vetor, variaveis[i])

      resultados[[i]] = list(tabelas, plots, textos)
    }
    else{

      tabela[[i]] = tab_desc_cat(vetor, variaveis[i])
      plots       = visual_desc_cat(vetor, variaveis[i])
      textos      = texto_desc_cat(vetor, variaveis[i])

    }
    resultados[[i]] = list(plots, textos)

  }

  tabelas = dplyr::bind_rows(tabela)
  names(resultados) = variaveis
  return(list(tabelas, resultados))
}


