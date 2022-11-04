#'  Análise univariada automatizada para relatório.
#'
#' @import dplyr
#' @import ggplot2
#' @import gridExtra
#' @import scales
#' @export

analise_univariada  = function(variavel, cor = "#028090"){

  var_name = names(variavel)
  variavel = dplyr::select(variavel, var = var_name)
  nn = sum(!is.na(variavel$var))
  # Resumo
  resumo  = rstatix::get_summary_stats(variavel)
  resumo[1,1] = var_name
  resumo
  # Histograma
  breaks = pretty(range(variavel$var,na.rm = TRUE), n = nclass.Sturges(variavel$var), min.n = 1)
  histograma = ggplot(variavel, aes(x = var)) +
    labs(x = var_name, y = "Contagem", title = paste0(var_name, " (n=",nn,")")) +
    geom_histogram(color = "black", fill = cor, breaks = breaks) +
    theme(text = element_text(size = 6))

  ## boxplot
  boxplot  = ggplot(variavel, aes(var)) +
             geom_boxplot(alpha = 0.7, fill = cor)

  # Densidade
  densidade = ggplot(variavel, aes(x = var)) +
    geom_density(color =  cor) +
    labs(x = var_name, y = "Densidade") +
    theme(text = element_text(size = 6))

  # qqplot
  qq = ggplot(variavel , aes(sample = var)) +
    stat_qq(color = cor) +
    stat_qq_line() +
    labs(x = "Quantis teóricos", y = "Quantis", title = var_name) +
    theme(text = element_text(size = 6))

  # Teste de normalidade
  alpha = 0.05
  tst_norm = "Shapiro-Wilk"
  tbl_norm = rstatix::shapiro_test(variavel, vars = "var")
  res_norm = ifelse(tbl_norm$p < alpha,
                    "foi significativo (`r scales::pvalue(tbl_norm$p)`), logo teste com o pressuposto de normalidade podem ser realizadas (abordagem parâmétrica",
                    "não foi significativo (`r scales::pvalue(tbl_norm$p)`), logo testes com o pressuposto de normalidade não podem ser realizados e deve-se utilzar abordagens não-parâmétricas")


  # Intepretação
 interp = cat("A variável quantitativa `r var_name` possui `r nn` observações. O teste de normalidade de `r tst_norm` `r res_norm.")

 visu = gridExtra::grid.arrange(gridExtra::arrangeGrob(histograma, boxplot, ncol = 1),
                                gridExtra::arrangeGrob(densidade, qq, ncol = 1), ncol=2)

 saida = list(resumo, visu, interp)
 return(saida)
}

