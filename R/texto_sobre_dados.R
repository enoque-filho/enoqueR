#' Texto descritivo de conjunto de dados para relatórios
#' @export
texto_sobre_dados = function(dados, nome){

  glue::glue("O conjunto de dados {nome_dados} contém {n} observações e {n_variaveis} variáveis, das quais {n_quantitativas} são quantitativas e {n_qualitativas} são qualitiativas.",
             nome_dados = nome,
             n = nrow(dados),
             n_variaveis = ncol(dados),
             n_quantitativas  = ncol(select_if(dados, is.numeric)),
             n_qualitativas = ncol(select_if(dados, purrr::negate(is.numeric))))
}
