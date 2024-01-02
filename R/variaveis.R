#' Listar varias estatísticas
#'
#' @export

variaveis = function(dados){
  # Variáveis
 p1 = dados %>%
    dplyr::select(where(is.numeric)) %>%
    rstatix::get_summary_stats(
      # n
      # Minimo
      # Máximo
      # Mediana
      # Q1
      # Q3
      # IQR
      # Desvio mediano absoluto (MAD)
      # Média
      # Desvio padrão
      # Erro padrão
      # Intervalo de confiança
    ) |>
   dplyr::mutate(
  # Variância
    var = sd^2,
  # Range
    range = max - min,
  # Coeficiente de variação
    cv = var/mean)

p2 = dados %>%
  dplyr::select(dplyr::where(is.numeric)) %>%
  tidyr::pivot_longer(dplyr::everything()) %>%
  dplyr::group_by(variable = name) %>%
  dplyr::summarise(
  # Distintos
    distinct = n_distinct(value),
  # Únicos (%)
  # Ausentes
    ausente = sum(is.na(value)),
  # Ausentes (%)
    ausente_pct = sum(is.na(value))/length(na.omit(value)),
  # Zeros
    zeros = sum(value == 0),
  # Zeros (%)
    zeros_pct = sum(value == 0)/length(na.omit(value)),
  # 5%
    q.5 = quantile(value, .05, na.rm = TRUE),
  # 95%
    q.95 = quantile(value, .95, na.rm = TRUE),
  # Curtose
    curtose = moments::kurtosis(value, na.rm = TRUE),
  # Assimetria
    assimetria = moments::skewness(value, na.rm = TRUE))

# Categóricas -------

p3 = dados %>%
  dplyr::select(dplyr::where(negate(is.numeric))) %>%
  tidyr::pivot_longer(dplyr::everything()) %>%
  dplyr::group_by(variable = name) %>%
  dplyr::summarise(
    # Distintos
    distinct = dplyr::n_distinct(value),
    # Únicos (%)
    # Ausentes
    ausente = sum(is.na(value)),
    # Ausentes (%)
    ausente_pct = sum(is.na(value))/length(na.omit(value)),
    n = length(na.omit(value)),
    min = NA,
    max = NA,
    median = NA,
    q1 = NA,
    q3 = NA,
    iqr = NA,
    mad = NA,
    mean = NA,
    sd = NA,
    se = NA,
    ci = NA,
    var = NA,
    range = NA,
    cv = NA,
    q.5 = NA,
    q.95 = NA,
    curtose = NA,
    assimetria = NA)

  saida =
    merge(p1,p2) %>%
    dplyr::bind_rows(.,p3) %>%
    tidyr::pivot_longer(-variable) %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>%
    data.frame()

  return(saida)
}
