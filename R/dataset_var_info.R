#' Informações relevantes das variáveis para testes estatísticos
#'
#'@export

dataset_var_info = function(dados){
  saida =
  left_join(
    left_join(
      # classes:
      summarise(dados, across(everything(), class)) %>%
        pivot_longer(everything()),
      # n, n_miss e n_distinct:
      mutate(dados, across(everything(), as.character)) %>%
        pivot_longer(everything()) %>%
        summarise(
          .by        = name,
          n          = sum(!is.na(value)),
          n_miss     = sum(is.na(value)),
          n_distinct = n_distinct(value)
        ), by = "name"
    ),
    # teste de normalidade
    rstatix::shapiro_test(dados, is.numeric),
    by = c('name' = 'variable')
  ) %>%
    rename(variavel = 1, tipo = 2, w = 6)
  return(saida)
}
