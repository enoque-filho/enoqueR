#' Teste de normalidade
#' @export
#'
teste_normalidade = function(vetor, alpha = 0.05){
  vetor = na.omit(vetor)
  if(length(vetor)<5000){
    teste = "Shapiro-Wilk"
    estatistica = shapiro.test(vetor)$statistic["W"]
    p = round(shapiro.test(vetor)$p.value,4)
  }
  else{
    teste = "Kolmogorov-Smirnov"
    vetor = unique(vetor)
    estatistica = ks.test(vetor, pnorm, mean(vetor), sd(vetor))$statistic["D"]
    p = ks.test(vetor, pnorm, mean(vetor), sd(vetor))$p.value
  }

  txt = paste0("O teste de ", teste, ", com significância de ",alpha,"%, foi realizado e ",
               ifelse(p>=0.05,
                      "não mostrou evidências para rejeitar a hipótese de normalidade dos dados",
                      "mostrou evidêcias para rejeitar a hipótese de normalidade dos dados"),
               " (", names(estatistica),'=',round(estatistica,3),", p=", scales::pvalue(p),")")

  return(c(teste = teste, estatistica, p.valor = p, txt))
}






