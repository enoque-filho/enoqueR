#' Tabela de estatísticas descritivas em colunas usando gtsummary
#' https://site.statplace.com.br/blog/gtsummary-tabelas-de-resumo-prontas-para-publicacao/
#'
#' @import dplyr
#' @import gtsummary
#' @export

tbl_resumo = function(dados, digits = NULL, missing = 'ifany'){

  colunas = list(
    # media (sd)
      select(dados, where(is.numeric)) %>%
      tbl_summary(statistic = all_continuous() ~ '{mean} ({sd})',
                  digits = list(everything() ~ digits),
                  missing = missing,
                  missing_text = 'Missing') %>%
        modify_header(stat_0 ~ '**Média (DP)**'),

    # mediana (iqr)
      select(dados, where(is.numeric)) %>%
      tbl_summary(digits = list(everything() ~ digits),
                  missing = missing,
                  missing_text = 'Missing') %>%
      modify_header(stat_0 ~ '**Mediana (IQR)**'),

    # min - max
      select(dados, where(is.numeric)) %>%
      tbl_summary(statistic = all_continuous() ~ '{min} - {max}',
                  digits = list(everything() ~ digits),
                  missing = missing,
                  missing_text = 'Missing') %>%
      modify_header(stat_0 ~ '**Min - Max**'),

    # categóricas n (%)
      select(dados, !where(is.numeric)) %>%
      tbl_summary(digits = list(everything() ~ digits),
                  missing = missing,
                  missing_text = 'Missing') %>%
      modify_header(stat_0 ~ '**N (%)**')
    )

  # Juntando tabelas
  tbl_resumo <-
    tbl_merge(colunas) %>%
  # Fazendo modificações
    modify_spanning_header(everything() ~ NA_character_)  %>%
    modify_footnote(everything() ~ NA_character_) %>%
    modify_table_body(~.x %>% dplyr::relocate('stat_0_4', .after = label)) %>% # categóricas
    modify_table_styling(columns = everything(), rows = !is.na(variable), missing_symbol = '-') %>%
    modify_header(label  ~  '**Variável**') %>%
    bold_labels() %>%
    modify_caption(caption = '**Estatísticas Descritivas**')
  return(tbl_resumo)
}

