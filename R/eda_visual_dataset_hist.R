#'
#'
#'
#'@export
#'


eda_visual_dataset_hist = function(dados){
  saida = dados %>%
    select(where(is.numeric)) %>%
    pivot_longer(everything(), names_to = "variavel") %>%
    ggplot(aes(x = value)) +
    geom_histogram(color = 'white', fill = 'gray50', bins = 25) +
    facet_wrap(~variavel, scales = "free")
  return(saida)
}
