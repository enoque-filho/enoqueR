#' Funções para análise descritiva
#'
#' @export


tab_desc_num = function(vetor, nome){
  # Faz tabela de estatisticas descritivas para uma variável quantitativa
  d      = data.frame(var = vetor)
  nn     = sum(!is.na(d$var))
  tabela = rbind(
    data.frame(var = paste0("***",nome, "*** (n=", nn, ")"), n = " "),
    dplyr::mutate(
      tidyr::pivot_longer(
        dplyr::summarise(d,minmax  = paste(min(var,na.rm = TRUE),"-",max(var,na.rm = TRUE)),
                           q1q3    = paste(quantile(var,0.25, na.rm = TRUE),"-", quantile(var,0.75, na.rm = TRUE)),
                           mediana = as.character(median(var, na.rm = TRUE)),
                           mediadp = paste0(round(mean(var,1, na.rm = TRUE))," (", round(sd(var, na.rm = TRUE),1),")")),
        c("minmax", "q1q3", "mediana", "mediadp"), names_to = "var", values_to = "n"),
      var = paste("&ensp;", c("Min - Max", "Q1 - Q3", "Mediana", "Média (DP)")))
  )
  names(tabela) = c("Variável", "Estatística")
  return(tabela)
}

# tab_desc_num(iris$Sepal.Width, "Sepal.Length")

# -------------------------------------------------------------------------


tab_desc_cat = function(vetor, nome){
  # Faz tabela de estatisticas descritivas para uma variável qualitativa
  d      = data.frame(var = vetor)
  nn     = sum(!is.na(d$var))
  tabela = rbind(data.frame(var = paste0("***",nome, "*** (n=", nn, ")"), n = " "),
          dplyr::mutate(dplyr::count(d, var), var = paste("&ensp;", var), n = paste0(n," (",round(n/sum(n)*100,1),"%)")))

  names(tabela) = c("Variável", "Estatística")
  return(tabela)
}


visual_desc_num = function(vetor, nome = "var", cor = "cyan3"){
  d      = data.frame(var = vetor)
  nn     = sum(!is.na(d$var))
  breaks = pretty(range(d$var, na.rm = TRUE), n = nclass.Sturges(d$var), min.n = 1)

  plot = ggplot2::ggplot(d, ggplot2::aes(x = var)) +
    ggplot2::geom_histogram(color  = "black", fill = cor, breaks = breaks, alpha = 0.8) +
    ggplot2::labs(x = nome, y = "Contagem", title = paste0(nome, " (n=",nn,")")) +
    ggplot2::theme(text = ggplot2::element_text(size = 8))
  return(plot)
}

# visual_desc_num(iris$Sepal.Length,"Sepal.Length")

# -------------------------------------------------------------------------

visual_desc_cat = function(vetor, nome = "var", cor = "cyan3"){
  d      = data.frame(var = vetor)
  nn     = sum(!is.na(d$var))
  d2 = dplyr::mutate(
    dplyr::summarise(dplyr::group_by(d, var), n = dplyr::n()), pct = paste0("(",round(n/sum(n)*100,1),"%)"))

  plot =
    ggplot2::ggplot(d2, ggplot2::aes(x = var, y = n)) +
    ggplot2::geom_bar(stat="identity" , fill = cor, alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label =  paste(n, pct)), vjust = -0.3) +
    ggplot2::theme(text = ggplot2::element_text(size = 8), legend.position = "none") +
    ggplot2::labs(x = nome, y = "Contagem", title = paste0(nome, " (n=",nn,")"))# +
    # ggplot2::scale_y_continuous(limits = c(0 ,nn))

  if(nrow(dplyr::count(d2, var)) >=10){ plot = plot + ggplot2::coord_flip()}
  return(plot)
}

# visual_desc_cat(iris$Species,"Espécies de Iris", "cyan4")

# -------------------------------------------------------------------------


texto_desc_num = function(vetor, nome){
  d      = data.frame(var = vetor)
  nn     = sum(!is.na(d$var))
  txt_ausente = ifelse(nn == nrow(d),
                       paste0(" não apresentou valores ausentes (*missing*), todas as ", nn, " linhas do conjunto dedados estão preenchidas"),
                       paste0(" apresentou valores ausentes (*missing*)", nrow(d)-nn, "das linhas não estão preenchidas"))

txt_media_mediana = "[Nada aqui por enquanto]" # A diferença entre a média ([media]) e a mediana ([mediana]) [não é significativa indicando simetria]
texto = paste0(
  paste0("**",nome,"**: a variável ",nome, txt_ausente,". Passamos a avaliar como os valores estão distribuidos:\n\n"),
  paste0("- Os dados variam no intervalo (",paste0(min(d$var, na.rm = TRUE), "-", max(d$var, na.rm = TRUE)),"). Portanto  sua amplitude (diferença entre maior e o menor) foi ", max(d$var, na.rm = TRUE) - min(d$var, na.rm = TRUE),";\n\n"),
  paste0("- Quartis: 25% dos valores foram menores que ",quantile(d$var, 0.25, na.rm = TRUE)," e 25% foram maiores que ", quantile(d$var, 0.75, na.rm = TRUE),". Assim a medida 'central' dos dados se distribuição logo de", IQR(d$var, na.rm = TRUE)," unidades. Esta quantia também é chamada intervalo interquatil;\n\n"),
  paste0("- A mediana obtida foi ", median(d$var, na.rm = TRUE),", que indica que 50% dos dados estão abaixo dese valor e 50% estão acima." ,txt_media_mediana, ";\n\n"),
  paste0("- A variabilidade é medida pelo desvio padrão (", round(sd(d$var, na.rm = TRUE),1),") e indica o quanto os dados variam da média obtida;\n\n"),
  paste0("- o CV (Coeficiete de Variação) (",round(sd(d$var,na.rm = TRUE)/mean(d$var, na.rm = TRUE)*100,1),"%) compara o desvio padrão com a média. O ideal é que este indice seja o mais baixo possivel (idealmente menor que 50%).", ifelse(round(sd(d$var,na.rm = TRUE)/mean(d$var, na.rm = TRUE)*100,1)<50, "Como isso ocorreu os dados tendem a se concentrar perto da média;\n\n","Como isso não ocorreu os dados tendem a se afastar da média")),
  paste0("- ",teste_normalidade(d$var)[4],".\n\n"))
texto
}

# texto_desc_num(iris$Sepal.Length,"Largura da Sepala")

# -------------------------------------------------------------------------
texto_desc_cat = function(vetor, nome){

  d      = data.frame(var = na.omit(vetor))
  nn     = sum(!is.na(d$var))
  p      = rep(1/length(levels(d$var)),length(levels(d$var)))

  estatistica = chisq.test(table(d$var), p = p)$statistic
  pvalor      = chisq.test(table(d$var), p = p)$p.value

  texto = paste0(
  paste0("**",nome,"**: a variável  ",nome, " Foi realizado um teste-quiquadrado de aderência, ",
         "para verificar se as frequeências de cada nivel diferem do esperado (frequências iguais)"),

  paste0(ifelse(estatistica < 0.05, "rejeitamos a hipótese de igualde entre todas as  frequências",
                 "não rejeitamos a hipótese de igualdade entre todas as frequências")),
  paste0("($\\chi^2$=", round(estatistica,3),", p=", round(pvalor,3),"[Vcramer])"),
  " Isso significa que pelo menos uma frequêcia difere de 1/3 33.33%. O efeito foi medido pela estatistica V de Cramer (vcramer) que pode ser cosiderado um efeito grande"
  )
  return(texto)
}

# texto_desc_cat(iris$Species, "Espécies")
####################################################################################


# Análise descritiva usando gtsummary -------------------------------------


tab_desc2 = function(dados){
  saida =
    gtsummary::modify_header( gtsummary::add_n( gtsummary::tbl_summary(
      data      = dados,
      type      = list(where(is.numeric) ~ "continuous2"),
      statistic = list(gtsummary::all_continuous2() ~ c("{min} - {max}", "{median}",  "{mean} ({sd})")))
    ),label ~ "**Variável**")
  return(saida)
}


# -------------------------------------------------------------------------


visual_descritiva = function(dados, cor = "cyan3"){

  dados     = data.frame(dados)
  variaveis = names(dados)
  plots     = list()

  for(i in 1:length(variaveis)){

    vetor = dados[,variaveis[i]]
    classe = class(vetor)

    if(classe %in% c("numeric","double","integer")){
      plots[[i]] = visual_desc_num(vetor, variaveis[i], cor = cor)
    }
    else{
      plots[[i]] = visual_desc_cat(vetor, variaveis[i], cor = cor)
    }
  }
  names(plots) = variaveis
  return(plots)
}

