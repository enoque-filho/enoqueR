#'
#'
#' @export
#'


eda_tabela_dataset_resumo_cat = function(dados){
  dados = dplyr::mutate_all(dplyr::select_if(dados, purrr::negate(is.numeric)), factor)
  variaveis = names(dados)
  Variável = NULL
  Nível = NULL
  n = NULL
  `n %` = NULL
  `n % acumulada` = NULL
  k = 1
  for(i in 1:length(variaveis)){
    nivs = levels(dados[,i])
    npct = 0

    for(j in 1:length(nivs)){
      Variável[k] = variaveis[i]
      Nível[k] = nivs[j]
      n[k] = nrow(dplyr::filter(dados, get(variaveis[i]) == nivs[j]))
      `n %`[k] = (nrow(dplyr::filter(dados, get(variaveis[i]) == nivs[j]))/nrow(dados))*100
      `n % acumulada`[k] = npct +`n %`[k]
      npct = `n % acumulada`[k]
      k = k+1

      if(j == length(nivs)){
        Variável[k] = variaveis[i]
        Nível[k] = "TOTAL"
        n[k] = nrow(dplyr::filter(dados, get(variaveis[i]) %in% nivs))
        `n %`[k] = NA
        `n % acumulada`[k] = NA
        k = k+1}
    }


  }
  saida = dplyr::tibble(Variável, Nível, n, `n %`,`n % acumulada`)
  return(saida)
}
