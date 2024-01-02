#' Qual teste utlizar em cada par de variáveis de um dataset
#'
#' @export

dataset_bivar_info = function(dados){

  dados    = data.frame(dados)
  p1       = dataset_var_info(dados)
  # Combinações 2 a 2 dos nomes das colunas
  pares    = combn(names(dados), 2) %>% t() %>% data.frame()

  # Funções auxiliares

  aaaa = function(dados, col, cold){a = NULL
  a = NULL
  for(i in 1:nrow(pares)){
    a[i] = p1[pares[i,col], cold]}
  return(a)
  }

  bbbb = function(norm){
    b = norm > 0.05
    saida = ifelse(is.na(b), FALSE, b)
    return(saida)
    }

  saida = pares %>%
    dplyr::mutate(
      var1 = X1,
      var2 = X2,
      classe1 = aaaa(p1,1,"classe"),
      classe2 = aaaa(p1,2,"classe"),
      tipo = ifelse(
        classe1 == classe2 & classe1 %in% c("numeric", "integer"), "num_num",
                    ifelse(classe1 == classe2 & purrr::negate(classe1 %in% c("numeric", "integer")),
                           "cat_cat","num_cat")),
      nl1 = aaaa(p1,1, "n_dist"),
      nl2 = aaaa(p1,2, "n_dist"),
      norm_v1 = aaaa(p1,1, "p_norm"),
      norm_v2 = aaaa(p1,2, "p_norm"),
      teste = dplyr::case_when(
        tipo == "cat_cat" ~ "Qui-quadrado",
        tipo == "num_num" ~ ifelse(norm_v1 <= 0.05 | norm_v2 <= 0.05, "Correlação Spearman", "Correlação Pearson"),
        tipo == "num_cat" ~ifelse(bbbb(norm_v1) | bbbb(norm_v2),
                                  ifelse(nl1 <= 2 | nl2 <= 2,"Teste t", "ANOVA"),
                                  ifelse(nl1 <= 2 | nl2 <= 2,"Mann-Whitney", "Kruskall-Wallis"))
        )
      ) %>%
    dplyr::select(-X1, -X2)

  return(saida)

}

