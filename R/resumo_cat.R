#'
#'
#' @export
#'


resumo_cat = function(dados){
  dados = dplyr::select_if(dados, purrr::negate(is.numeric)) |> dplyr::mutate_all(factor)
  vars = names(dados)
  Variável = NULL
  Nível = NULL
  n = NULL
  `n %` = NULL
  `n % acumulada` = NULL
  k  = 1
  for(i in 1:length(vars)){
    nivs = levels(dados[,i])
    npct = 0
    for(j in 1:length(nivs)){
      Variável[k] = vars[i]
      Nível[k] = nivs[j]
      n[k] = nrow(dplyr::filter(dados, get(vars[i]) == nivs[j]))
      `n %`[k] = (nrow(dplyr::filter(dados, get(vars[i]) == nivs[j]))/nrow(dados))*100
      `n % acumulada`[k] = npct +`n %`[k]
      npct = `n % acumulada`[k]
      k = k+1
      if(j == length(nivs)){
        Variável[k] = vars[i]
        Nível[k] = "TOTAL"
        n[k] = nrow(dplyr::filter(dados, get(vars[i]) %in% nivs))
        `n %`[k] = NA
        `n % acumulada`[k] = NA
        k = k+1}
    }


  }
  saida = dplyr::tibble(Variável, Nível, n, `n %`,`n % acumulada`)
  return(saida)
}
