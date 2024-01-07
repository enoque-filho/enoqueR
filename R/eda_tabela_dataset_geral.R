#' Visão geral de banco de dados
#' Retorna as seguintes informações de um banco de dados:**
#' - Nome das variáveis
#' - Classe das variáveis
#' - Normalidade (caso numeric)
#' - Niveis e frequencia (casso character ou factor)
#' - Numero de observações não ausentes em cada variável

#' @param dados banco de dados
#' @export

eda_tabela_dataset_geral = function(dados){
  saida =
    left_join(
      left_join(
        summarise(dados, across(everything(), class)) %>%
          pivot_longer(everything(), names_to = "variable", values_to = "classe"),
        full_join(
          select(dados, !where(is.numeric)) %>%
            pivot_longer(everything(), names_to = "variable") %>%
            count(variable, value) %>%
            summarise(.by = variable, informacao = paste0(value,": ", n, collapse = ", ")),
          rstatix::shapiro_test(dados, is.numeric) %>%
            mutate(
              .keep = "unused",
              informacao = paste0(ifelse(p>=0.05,"Normal","Não Normal"), ' (W: ', round(statistic, 5), ", p: ", round(p, 5), ")")
            ),
          by = c("variable", "informacao")
        ), by = "variable"
      ),
      summarise(dados, across(everything(), ~sum(!is.na(.x)))) %>%
        pivot_longer(everything(), names_to = "variable", values_to =  "n"),
      by = "variable"
    )
  return(saida)
}


