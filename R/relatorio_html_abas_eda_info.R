#' Infomações do dataset em abas
#' @export
#'

relatorio_html_abas_eda_info = function(dados){
  cat("## Informações do conjunto de dados {.tabset} \n\n")
  cat("### Sobre o dataset \n\n")
  Info = c("Observações", "Variáveis", "Numérico", "Categórico", "Lógico", "Tamanho")
  N = c(dim(dados)[1],
        dim(dados)[2],
        ncol(dplyr::select_if(dados, is.numeric)),
        sum(ncol(dplyr::select_if(dados, is.factor)),
            ncol(dplyr::select_if(dados, is.character))),
        ncol(dplyr::select_if(dados, is.logical)),
        paste0(round(c(object.size(dados))/1024,2)," KB"))
  print(dplyr::tibble(Info, N) %>% kbl() %>% kable_styling())

  cat("### Geral\n\n")
  print(eda_tabela_dataset_geral(dados) %>% kbl() %>% kable_styling())

  cat("### Estrutura\n\n")
  print(eda_tabela_dataset_estrutura(dados) %>% kbl() %>% kable_styling())

  cat("### Resumo numéricas\n\n")
  print(eda_tabela_dataset_resumo_num(dados) %>% kbl() %>% kable_styling())

  cat("### Resumo categóricas\n\n")
  print(eda_tabela_dataset_resumo_cat(dados) %>% kbl() %>% kable_styling())
  cat("\n\n")
}
