output$graphs <- renderUI({
  
  # Create lists that will give the select options to the respective language
  list_trabalho_graph <- list('Trabalho' = structure(c("TT"), .Names = c(i18n()$t("Trabalho Total"))))
  list_saude_graph <- list('Saúde' = structure(c("ST", "SB", "SM", "SA"), 
                                               .Names = c(i18n()$t("Saúde Total"),
                                                          i18n()$t("Saúde Baixa"),
                                                          i18n()$t("Saúde Média"),
                                                          i18n()$t("Saúde Alta"))))
  list_edu_graph <- list('Educação' = structure(c("ET", "EI", "EF", "EM"), 
                                                .Names = c(i18n()$t("Educação Total"),
                                                           i18n()$t("Educação Infantil"),
                                                           i18n()$t("Educação Fundamental"),
                                                           i18n()$t("Educação Média"))))
  
  list_types_graphs_palma <- list('Razão de Desigualdade' = structure(c("palma_renda", "palma_cor"), 
                                                               .Names = c(i18n()$t("Desigualdade por renda (Razão de Palma)"),
                                                                          i18n()$t("Desigualdade por cor"))))
  list_types_graphs_dumbell <- list('Desigualdade à atividade mais próxima' = structure(c("dumbell_renda", "dumbell_cor"), 
                                                                                        .Names = c(i18n()$t("Desigualdade por renda"),
                                                                                                   i18n()$t("Desigualdade por cor"))))
  
  names(list_trabalho_graph) <-c(i18n()$t("Trabalho"))
  names(list_saude_graph) <-c(i18n()$t("Saúde"))
  names(list_edu_graph) <-c(i18n()$t("Educação"))
  names(list_types_graphs_palma) <-c(i18n()$t("Razão de Desigualdade"))
  names(list_types_graphs_dumbell) <- c(i18n()$t("Desigualdade à atividade mais próxima"))
  
  # vector_indicadores_graph <- structure(c("CMA", "TMI"), .Names = c(i18n()$t("Cumulativo"), i18n()$t("Tempo Mínimo")))
  # vector_types_graphs <- structure(c("palma", "boxplot"), .Names = c(i18n()$t("Razão de Palma"), "Boxplot"))
  
  # Start proper UI here 
  tagList(
    pickerInput(inputId = "graph_type",
                # label = HTML("<h1>Escolha o indicador de acessibilidade: <img src=\"ipea.jpg\" align=\"leftright\" width=\"70\"/></h1>"),
                label = HTML(sprintf("<h1>%s <button id=\"q15_graph\" type=\"button\" class=\"btn btn-light btn-xs\"><i class=\"fa fa-info\"></i></button></h1>", 
                                     i18n()$t("Escolha o gráfico:"))),
                choices = c(list_types_graphs_palma, list_types_graphs_dumbell),
                selected = "palma_renda",
                width = '100%'),
    div(
      # edit2
      bsPopover(id = "q15_graph", 
                title = sprintf("<strong>%s</strong>", i18n()$t("Gráficos")),
                content = HTML(i18n()$t('<ul><li><strong>Razão de Desiguldade</strong> calcula a desigualdade pelo indicador cumulativo para renda - razão entre a acessibilidade média dos 10% mais ricos pela dos 40% mais pobres - e cor - razão entre a acessibilidade média ponderada pela população branca pela população negra.</li><li><strong>Desigualdade à atividade mais próxima</strong> compara a acessibilidade para renda - acessibilidade média da população em geral x acessibilidade pobres x acessibiidade ricos, e para cor - média x brancos x negros.</li></ul>')),
                placement = "left",
                trigger = "hover",
                options = list(container = "body"))
    ),
    radioGroupButtons(inputId = "modo_todos_graph",
                      # label = HTML("<h1>Escolha o indicador de acessibilidade: <img src=\"ipea.jpg\" align=\"leftright\" width=\"70\"/></h1>"),
                      label = h1(i18n()$t("Escolha o modo de transporte:")), 
                      choices = c("<i class=\"fas fa-bus fa-2x\"></i>" = "tp", 
                                  "<i class=\"fas fa-walking fa-2x\"></i>" = "caminhada",
                                  "<i class=\"fas fa-bicycle fa-2x\"></i>" = "bicicleta"),
                      selected = "tp",
                      individual = TRUE,
                      justified = TRUE
    ),
    # img(src='ipea.jpg', align = "right", width = "150"),
    conditionalPanel(condition = "graphs_cma.indexOf(input.graph_type) > -1",
                     pickerInput(inputId = "atividade_graph_cum",
                                 label = HTML(sprintf("<h1>%s: <button id=\"q3_graph\" type=\"button\" class=\"btn btn-light btn-xs\"><i class=\"fa fa-info\"></i></button></h1>", 
                                                      i18n()$t("Escolha a atividade"))),
                                 choices = c(list_trabalho_graph, list_saude_graph, list_edu_graph),
                                 selected = "TT")),
    conditionalPanel(condition = "graphs_tmi.indexOf(input.graph_type) > -1",
                     pickerInput(inputId = "atividade_graph_tmi",
                                 label = HTML(sprintf("<h1>%s: <button id=\"q4_graph\" type=\"button\" class=\"btn btn-light btn-xs\"><i class=\"fa fa-info\"></i></button></h1>", 
                                                      i18n()$t("Escolha a atividade"))),
                                 choices = c(list_saude_graph, list_edu_graph),
                                 selected = "ST")),
    div(
      # edit2
      bsPopover(id = "q3_graph", 
                title = sprintf("<strong>%s</strong>", i18n()$t("Atividades")),
                content = HTML(i18n()$t("<ul><li> Atividades com o sufixo <em>Total</em> representam todas as atividades</li><li> Sufixos da atividade de <b>saúde</b> (<em>Baixa, Média</em> e <em>Alta</em>) representam o nível de atenção dos serviços prestados</li></ul>")),
                placement = "top",
                trigger = "hover",
                options = list(container = "body"))
    ),
    div(
      # edit2
      bsPopover(id = "q4_graph", 
                title = sprintf("<strong>%s</strong>", i18n()$t("Atividades")),
                content = HTML(i18n()$t("<ul><li> Atividades com o sufixo <em>Total</em> representam todas as atividades</li><li> Sufixos da atividade de <b>saúde</b> (<em>Baixa, Média</em> e <em>Alta</em>) representam o nível de atenção dos serviços prestados</li></ul>")),
                placement = "top",
                trigger = "hover",
                options = list(container = "body"))
    ),
    conditionalPanel(condition = "graphs_cma.indexOf(input.graph_type) > -1 && input.modo_todos_graph == 'tp'",
                     sliderInput(inputId = "tempo_tp_graph",
                                 label = h1(i18n()$t("Escolha o tempo de viagem:")),
                                 min = 30, max = 120,
                                 step = 30, value = 30,
                                 animate = animationOptions(interval = 2000),
                                 post = " min",
                                 width = '100%')),
    conditionalPanel(condition = "graphs_cma.indexOf(input.graph_type) > -1 && modos_ativos.indexOf(input.modo_todos_graph) > -1",
                     sliderInput(inputId = "tempo_ativo_graph",
                                 label = h1(i18n()$t("Escolha o tempo de viagem:")),
                                 min = 15, max = 60,
                                 step = 15, value = 15,
                                 animate = animationOptions(interval = 2000),
                                 post = " min",
                                 width = '100%')),
    hr(),
    tagList(i18n()$t('Mais informações podem ser encontradas na seção de resultados do'), 
            shiny::a('Texto para Discussão', href=i18n()$t('https://www.ipea.gov.br/acessooportunidades/publication/2019_td2535/')))
    
  )
  
  
  
})