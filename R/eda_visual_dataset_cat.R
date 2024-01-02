#' Visualizar variáveis categoricas
#'
#' @import ggplot2
#' @import purrr
#' @export

eda_visual_dataset_cat = function(dados, resposta = NULL, ncols = 2, pos = "stack"){
  dados = dplyr::select_if(dados, negate(is.numeric)) |> dplyr::mutate_all(factor)
  graficos = list()

  if(is.null(resposta) == TRUE){
    variaveis = names(dados)
    i = 1
    for(i in 1:length(variaveis)){
      d = dplyr::select(dados, var = variaveis[i])
      graficos[[i]] =
        ggplot(d, aes(x = var, fill = var)) +
        geom_bar(position = pos, color = "black", alpha = 0.8)  +
        labs(x = variaveis[i], y = element_blank()) +
        theme(legend.position = "none", text = element_text(size = 8)) +
        geom_text(stat='count', aes(label=..count..), vjust = 1.5)}
  }
  else{
    variaveis = names(dados)[names(dados)!=resposta]
    for(i in 1:length(variaveis)){
      d = dplyr::select(dados, resp = resposta, var = variaveis[i])
      graficos[[i]] =
        ggplot(d, aes(x = var, fill = resp)) +
        geom_bar(position = pos, color = "black") +
        labs(x = variaveis[i], y = resposta) +
        theme(text = element_text(size = 8)) +
        geom_text(stat='count', aes(label=..count..))
    }
  }
  return(gridExtra::grid.arrange(grobs = graficos, ncols))
}
