#' Visualização de dados para análise descritiva
#'
#' @export
#'
#'

visual_dataset_descritiva = function(dados, cor = "cyan3"){

  dados     = data.frame(dados)
  variaveis = names(dados)
  plots     = list()

  for(i in 1:length(variaveis)){

    vetor = dados[,variaveis[i]]
    classe = class(vetor)

    if(classe %in% c("numeric","double","integer")){
      plots[[i]] = visual_desc_num(vetor, variaveis[i], cor = cor)
    }
    else{
      plots[[i]] = visual_desc_cat(vetor, variaveis[i], cor = cor)
    }
  }
  names(plots) = variaveis
  return(plots)
}
