# Reactive to filter year

ano_filtrado_graph <- reactive({
  
  
  print(input$ano_graph)  
  
  if (input$graph_type == "palma_renda") {
    
    aa <- palma_renda[year == input$ano_graph]
    
  } else if (input$graph_type == "palma_cor") {
    
    aa <- palma_cor[year == input$ano_graph]
    
  } else if (input$graph_type == "dumbell_renda")  {
    
    aa <- dumbell_renda[year == input$ano_graph]
    
  } else if (input$graph_type == "dumbell_cor")  {
    
    aa <- dumbell_cor[year == input$ano_graph]
    
  }
  
})


# Reactive para a modo
modo_filtrado_graph <- reactive({
  
  
  # if (input$graph_type == "palma_renda") {
  #   
  #   aa <- palma_renda[modo == input$modo_todos_graph]
  #   
  # } else if (input$graph_type == "palma_cor") {
  #   
  #   aa <- palma_cor[modo == input$modo_todos_graph]
  #   
  # } else if (input$graph_type == "dumbell_renda")  {
  #   
  #   aa <- dumbell_renda[modo == input$modo_todos_graph]
  #   
  # } else if (input$graph_type == "dumbell_cor")  {
  #   
  #   aa <- dumbell_cor[modo == input$modo_todos_graph]
  #   
  # }
  
  
  ano_filtrado_graph()[mode == input$modo_todos_graph]
  
})  

# Reative to activity
# Reative to time threshold
input_atividade_graph <- reactive({
  
  ifelse(input$graph_type %in% c("palma_renda", "palma_cor"), 
         input$atividade_graph_cum, 
         input$atividade_graph_tmi)
  
})


atividade_filtrada_graph <- reactive({
  
  bb <- modo_filtrado_graph()[atividade == input_atividade_graph()]
  
  return(bb)
  
})

# Reative to time threshold
input_tempo_graph <- reactive({
  
  ifelse(input$modo_todos_graph %in% c("walk", "bicycle"),
         input$tempo_ativo_graph, 
         input$tempo_tp_graph)
  
})

tempo_filtrado_graph <- reactive({
  
  cc <- atividade_filtrada_graph()[tempo_viagem == input_tempo_graph()]
  
})



make_title_plots <- reactive({
  
  # make title plot
  title_plot_graph <- switch(input$graph_type, 
                             "palma_renda" = i18n()$t("renda"), 
                             "palma_cor" = i18n()$t("cor"),
                             "dumbell_renda" = i18n()$t("renda"),
                             "dumbell_cor" = i18n()$t("cor")) 
  
  title_plot_modo <- switch(input$modo_todos_graph, 
                            "public_transport" = i18n()$t("por transporte público"), 
                            "car" = i18n()$t("por carro"), 
                            "walk" = i18n()$t("por caminhada"),
                            "bicycle" = i18n()$t("por bicicleta")) 
  
  title_plot_atividade <- switch(input_atividade_graph(), 
                                 "TT" = switch(input$graph_type,
                                               "palma_renda" = i18n()$t("para trabalho"),
                                               "palma_cor" = i18n()$t("para trabalho"),
                                               "dumbell_renda" = "vazio",
                                               "dumbell_cor" = "vazio"),
                                 "ET" = switch(input$graph_type,
                                               "palma_renda" = i18n()$t("para educação"),
                                               "palma_cor" = i18n()$t("para educação"),
                                               "dumbell_renda" = i18n()$t("à escola mais próxima"),
                                               "dumbell_cor" = i18n()$t("à escola mais próxima")),
                                 "EI" = switch(input$graph_type,
                                               "palma_renda" = i18n()$t("para educação infantil"),
                                               "palma_cor" = i18n()$t("para educação infantil"),
                                               "dumbell_renda" = i18n()$t("à escola infantil mais próxima"),
                                               "dumbell_cor" = i18n()$t("à escola infantil mais próxima")),
                                 "EF" = switch(input$graph_type,
                                               "palma_renda" = i18n()$t("para educação fundamental"),
                                               "palma_cor" = i18n()$t("para educação fundamental"),
                                               "dumbell_renda" = i18n()$t("à escola fundamental mais próxima"),
                                               "dumbell_cor" = i18n()$t("à escola fundamental mais próxima")),
                                 "EM" = switch(input$graph_type,
                                               "palma_renda" = i18n()$t("para educação média"),
                                               "palma_cor" = i18n()$t("para educação média"),
                                               "dumbell_renda" = i18n()$t("à escola média mais próxima"),
                                               "dumbell_cor" = i18n()$t("à escola média mais próxima")),
                                 "ST" = switch(input$graph_type,
                                               "palma_renda" = i18n()$t("para saúde"),
                                               "palma_cor" = i18n()$t("para saúde"),
                                               "dumbell_renda" = i18n()$t("ao equipamento de saúde mais próximo"),
                                               "dumbell_cor" = i18n()$t("ao equipamento de saúde mais próximo")),
                                 "SB" = switch(input$graph_type,
                                               "palma_renda" = i18n()$t("para saúde baixa"),
                                               "palma_cor" = i18n()$t("para saúde baixa"),
                                               "dumbell_renda" = i18n()$t("ao equipamento de saúde baixo mais próximo"),
                                               "dumbell_cor" = i18n()$t("ao equipamento de saúde baixo mais próximo")),
                                 "SM" = switch(input$graph_type,
                                               "palma_renda" = i18n()$t("para saúde média"),
                                               "palma_cor" = i18n()$t("para saúde média"),
                                               "dumbell_renda" = i18n()$t("ao equipamento de saúde médio mais próximo"),
                                               "dumbell_cor" = i18n()$t("ao equipamento de saúde médio mais próximo")),
                                 "SA" = switch(input$graph_type,
                                               "palma_renda" = i18n()$t("para saúde alta"),
                                               "palma_cor" = i18n()$t("para saúde alta"),
                                               "dumbell_renda" = i18n()$t("ao equipamento de saúde alta mais próximo"),
                                               "dumbell_cor" = i18n()$t("ao equipamento de saúde alta mais próximo")),
                                 "CT" = switch(input$graph_type,
                                               "palma_renda" = i18n()$t("para CRAS"),
                                               "palma_cor" = i18n()$t("para CRAS"),
                                               "dumbell_renda" = i18n()$t("ao CRAS mais próximo"),
                                               "dumbell_cor" = i18n()$t("ao CRAS mais próximo"))
                                 
  )
  
  title_plot_df <- data.frame(graph = title_plot_graph,
                              modo = title_plot_modo,
                              atividade = title_plot_atividade)
  
  # print(title_plot_df$graph)
  # print(title_plot_df$modo)
  # print(title_plot_df$atividade)
  
  return(title_plot_df)
  
  
})


# Render graphs
output$output_graph <- renderHighchart({
  
  
  req(input$graph_type)
  
  # GRAPH FOR PALMA RATIO 
  if (input$graph_type %in% c("palma_renda", "palma_cor")) {
    
    new <- tempo_filtrado_graph()[, name_muni := factor(name_muni)]
    
    
    new <- setorder(new, -palma_ratio)
    
    legend_subtitle <- ifelse(input_atividade_graph() == "TT", i18n()$t("empregos"),
                              ifelse(
                                input_atividade_graph() == "ET", i18n()$t("escolas"),
                                ifelse(
                                  input_atividade_graph() == "EI", i18n()$t("escolas de educação infantil"),
                                  ifelse(
                                    input_atividade_graph() == "EF", i18n()$t("escolas de educação fundamental"),
                                    ifelse(
                                      input_atividade_graph() == "EM", i18n()$t("escolas de educação média"),
                                      ifelse(
                                        input_atividade_graph() == "ST", i18n()$t("equipamentos de saúde"),
                                        ifelse(
                                          input_atividade_graph() == "SB", i18n()$t("equipamentos de saúde de baixa complexidade"),
                                          ifelse(
                                            input_atividade_graph() == "SM", i18n()$t("equipamentos de saúde de média complexidade"),
                                            ifelse(
                                              input_atividade_graph() == "SA", i18n()$t("equipamentos de saúde de alta complexidade"),
                                              ifelse(
                                                input_atividade_graph() == "CT", i18n()$t("CRAS"),
                                                input_atividade_graph()))))))))))
    
    
    title_plot <- sprintf("%s %s %s %s %s %s %s %s %s", 
                          i18n()$t("Desigualdade de acesso a"),
                          i18n()$t(legend_subtitle),
                          make_title_plots()$modo, 
                          i18n()$t("em"), 
                          input_tempo_graph(), 
                          i18n()$t("minutos"),
                          i18n()$t("entre grupos de"),
                          make_title_plots()$graph,
                          ifelse(input$selected_language == "en", "groups", "")
    )
    
    legend_plot <- switch(input$graph_type, 
                          "palma_renda" = 
                            sprintf("%s %s %s", 
                                    i18n()$t("Razão entre a média do número de"), 
                                    i18n()$t(legend_subtitle),
                                    i18n()$t("acessíveis pelos 10% mais ricos pelos 40% mais pobres")), 
                          "palma_cor" = 
                            sprintf("%s %s %s", 
                                    i18n()$t("Razão entre a média do número de"), 
                                    i18n()$t(legend_subtitle),
                                    i18n()$t("acessíveis pelos brancos pelos negros"))) 
    
    # print(title_plot)
    # print(legend_plot)
    
    
    hchart(new, "bar", hcaes(x = name_muni, y = palma_ratio),
           name = "Palma Ratio") %>%
      hc_title(text = title_plot,
               align = "left", x = 10) %>%
      hc_subtitle(text = legend_plot,
                  align = "left", x = 10) %>%
      hc_xAxis(opposite = FALSE,
               title = list(text = "")
               , labels = list(
                 # format = "{value}%",
                 style = list(fontSize = 15))
      ) %>%
      hc_yAxis(title = list(text = ifelse(input$graph_type == "palma_renda", 
                                          i18n()$t("Razão de Palma"), 
                                          i18n()$t("Razão de Desigualdade por Cor")))) %>%
      # change bar colors
      hc_colors(colors = "#1D5A79") %>%
      # change font
      hc_chart(style = list(fontFamily = "Roboto Condensed")) %>%
      # add vertical line
      hc_yAxis(plotLines = list(list(color = "#99A3A4", value = 1, width = 2, zIndex = 5, dashStyle = "LongDash"))) %>%
      hc_exporting(enabled = FALSE) %>%
      # add data label at the end of each bar (with values)
      hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE,
                                                  align = "right",
                                                  x = -5,
                                                  style = list(fontSize = 11,
                                                               color = "white",
                                                               textOutline = "0.3px white",
                                                               fontWeight = "regular"))))
    
  } else if (input$graph_type %in% c("dumbell_renda", "dumbell_cor")) {
    
    
    title_plot <- sprintf("%s %s %s %s %s %s", 
                          i18n()$t("Desigualdade de tempo de viagem"),
                          make_title_plots()$modo,
                          make_title_plots()$atividade,
                          i18n()$t("entre grupos de"),
                          make_title_plots()$graph,
                          ifelse(input$selected_language == "en", "groups", "")
    )
    
    legend_plot <- switch(input$graph_type, 
                          "dumbell_renda" = i18n()$t("Média do tempo mínimo de viagem por renda"), 
                          "dumbell_cor" = i18n()$t("Média do tempo mínimo de viagem por cor")) 
    
    
    # arrange by Q1
    
    teste_dumbell <- setorder(atividade_filtrada_graph(), -low)
    
    highchart() %>%
      hc_xAxis(categories = teste_dumbell$name_muni, labels = list(style = list(fontSize = 15))) %>%
      hc_yAxis(min = 0, labels = list(style = list(fontSize = 15)), title = list(text = i18n()$t("Minutos"))) %>%
      hc_chart(inverted = TRUE) %>%
      hc_title(text = title_plot,
               align = "left", x = 25) %>% 
      # change font
      hc_chart(style = list(fontFamily = "Roboto Condensed")) %>%
      hc_subtitle(text = legend_plot,
                  align = "left", x = 25) %>%
      hc_legend(itemStyle = list(fontSize = 15)) %>%
      # add bar
      hc_add_series(data = teste_dumbell,
                    type = "errorbar",
                    color = "#95A5A6",
                    lineWidth = 5,
                    opacity = 0.5,
                    name = "",
                    tooltip = list(enabled = TRUE,
                                   valueDecimals = 0),
                    whiskerWidth = 0) %>%
      # add total
      hc_add_series(data = teste_dumbell$total,
                    type = "scatter",
                    color = "black",
                    name = "Total",
                    size = 5,
                    marker = list(radius = 7),
                    tooltip = list(pointFormat = sprintf("%s: {point.y}", i18n()$t("Valor")),
                                   valueDecimals = 0)) %>%
      # add Q1
      hc_add_series(data = teste_dumbell$low,
                    type = "scatter",
                    color = "#008B45",
                    name = ifelse(input$graph_type == "dumbell_renda", i18n()$t("Pobres Q1"), i18n()$t("Negros")),
                    marker = list(radius = 7, symbol = "circle"),
                    tooltip = list(pointFormat = sprintf("%s: {point.y}", i18n()$t("Valor")),
                                   valueDecimals = 0)) %>%
      # add Q5
      hc_add_series(data = teste_dumbell$high,
                    type = "scatter",
                    color = "#36648B",
                    name = ifelse(input$graph_type == "dumbell_renda", i18n()$t("Ricos Q5"), i18n()$t("Brancos")),
                    marker = list(radius = 7, symbol = "circle"),
                    tooltip = list(pointFormat = sprintf("%s: {point.y}", i18n()$t("Valor")),
                                   valueDecimals = 0))
    
    
  }
  
  
})
