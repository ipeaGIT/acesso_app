  
# 1) LOAD DATA ----------------------------------------------------------------------------------------

acess <- read_rds("data/acess_wide.rds")

hex <- read_rds("data/hex.rds")

centroids <- read_rds("data/cities_centroids.rds")

limits <- read_rds(("data/cities_limits.rds"))

palma_renda <- read_rds("data/acess_palma_renda.rds") %>% setDT()
palma_cor <- read_rds("data/acess_palma_cor.rds") %>% setDT()

dumbell_renda <- read_rds("data/acess_dumbell_renda.rds") %>% setDT()
dumbell_cor <- read_rds("data/acess_dumbell_cor.rds") %>% setDT()

# Define a server for the Shiny app
function(input, output, session) {
  
  # 2) MODAL WITH LANGUAGE OPTION AT STARTUP ----------------------------------------------------------
  query_modal <- div(id = "modal_lang", 
                     modalDialog(
                       # title = HTML("<h1>Instruções para uso do mapa interativo na aba ao lado &nbsp<i class=\"fas fa-arrow-right\"></i></h1>"),
                       title = HTML("<h1>Lingua // Language</h1>"),
                       renderUI({
                         div(style = "width: 50%;margin: 0 auto;", 
                             pickerInput(inputId = 'selected_language',
                                         # label = "Escolha a lingua:",
                                         choices = c(a = "pt", b = "en"),
                                         choicesOpt = list(content = (c("<p><img src='img/pt.png' width=30px>&nbsp;&nbsp;Português</img></p>",
                                                                        "<p><img src='img/en_new.png' width=30px>&nbsp;&nbsp; English</img></p>"))),
                                         selected = input$selected_language),
                             
                             # actionButton(inputId = "openDetails",
                             #              label = "",
                             #              icon = icon("check"))
                         )
                       }),
                       
                       # includeHTML("www/carousel_2.html"),
                       easyClose = TRUE,
                       size = "m",
                       footer = div(id = "openDetails", class = "btn btn-default action-button shiny-bound-input",
                                    tagList(
                                      modalButton(icon("check"))
                                      # actionButton(inputId = "openDetails", label = "", icon = icon("check"))
                                    )
                       )
                       
                     )
  )
  
  # Show the model on start up ...
  showModal(query_modal)
  
  
  # 3) OBSERVER TO TIMEOUT IF USER IS INACTIVE  (inactive) -----------------------------------------------------
  
  # observeEvent(input$timeOut, { 
  #   print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
  #   showModal(modalDialog(
  #     title = "Timeout",
  #     HTML(paste("Session timeout due to", input$timeOut, "inactivity -", Sys.time()), "<br>"),
  #     HTML("<a href=\".\">Recarregar</a>"),
  #     footer = NULL
  #   ))
  #   session$close()
  # })  
  

# RENDER LANDING PAGE -------------------------------------------------------------------------

  observeEvent(input$openDetails, {
    
    output$landing_page <- renderUI({
      
    if (input$selected_language == "pt") {
      
      includeMarkdown("home_pt.md")
      
    } else if (input$selected_language == "en") {
      
      includeMarkdown("home_en.md")
    }
      
      
    })
    
  })
  
  # Second modal with instrctions
  observeEvent({input$tabs == "tab_mapa"}, once = TRUE, {
    
    if (input$tabs == "tab_mapa") {
      
      showModal(
        div(class = "modal_instructions",
            modalDialog(
              # title = "Teste",
              HTML(sprintf("<h1>%s &nbsp<i class=\"fas fa-arrow-right\"></i></h1>", i18n()$t("Comece selecionando uma cidade"))),
              easyClose = TRUE,
              size = "s",
              footer = NULL
              
            ))
      )
      
    }
    
    
  })
  
  
  
  
  # 3) RENDER 'UI' HERE SO IT CAN UPDATE FOR THE LANGUAGUES ---------------------------------------------
  
  # 3.1 Reactive to select the translator for the active langague -------------
  i18n <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% translator$languages) {
      translator$set_translation_language(selected)
    }
    translator
  })
  
  
  
  # TRANSLATE TAB TITLES ------------------------------------------------------------------------
  
  output$title_map = renderText({
    switch(input$selected_language, "pt"="Mapa", "en"="Map") 
  })  
  
  output$title_graph = renderText({
    switch(input$selected_language, "pt"="Gráficos", "en"="Plots") 
  })  
  
  
  # 3.2 Start of the UI (map) -------------------------------------
  source("modules/map_ui.R", local = TRUE)
  
  
  
  # 3.2 Start of the UI (graphs) -------------------------------------
  source("modules/graphs_ui.R", local = TRUE)
  

  # UI FOR THE DOWNLOAD BUTTONS -----------------------------------------------------------------

  output$ui_download_button <- renderUI({
    
    tagList(
    downloadButton("downloadData", i18n()$t("Baixe os dados (.csv)")),
    downloadButton("downloadPlot", i18n()$t("Baixe o gŕafico (.png)"))
    )
    
  })  

  # UI FOR THE DOWNLOAD BUTTONS - dictionary -----------------------------------------------------------------

  output$ui_download_dic <- renderUI({
    
    downloadButton("downloadDic", i18n()$t("Baixe o dicionário dos dados (.xlsx)"))
    
  })  

  # SERVER-GRAPHS -------------------------------------------------------------------------------
  
  
  # # Reative para indicador
  # indicador_filtrado_graph <- reactive({
  #   
  #   palma[indicador == input$indicador_graph]
  #   
  # })
  # 
  
  # Reactive para a modo
  modo_filtrado_graph <- reactive({
    
    
    if (input$graph_type == "palma_renda") {
      
      aa <- palma_renda[modo == input$modo_todos_graph]
      
    } else if (input$graph_type == "palma_cor") {
      
      aa <- palma_cor[modo == input$modo_todos_graph]
      
    } else if (input$graph_type == "dumbell_renda")  {
      
      aa <- dumbell_renda[modo == input$modo_todos_graph]
      
    } else if (input$graph_type == "dumbell_cor")  {
      
      aa <- dumbell_cor[modo == input$modo_todos_graph]
      
    }
      
    })  
  
  # Reative to activity
  # Reative to time threshold
    input_atividade_graph <- reactive({
    
    if(input$graph_type %in% c("palma_renda", "palma_cor")) {input$atividade_graph_cum} else {input$atividade_graph_tmi}
    
  })
  
  
  atividade_filtrada_graph <- reactive({
    
    bb <- modo_filtrado_graph()[atividade == input_atividade_graph()]
    # bb <- filter(modo_filtrado_graph(), atividade == input$atividade_cum_graph)
    
    # print(input$atividade_cum_graph)
    # print(input$atividade_cum_graph)
    
    # return(bb)
    
    print(paste0("aaa", nrow(bb)))
    
    return(bb)
    
  })
  
  # Reative to time threshold
  input_tempo_graph <- reactive({
    
    if(input$modo_todos_graph %in% c("caminhada", "bicicleta")) {input$tempo_ativo_graph} else {input$tempo_tp_graph}
    
  })
  
  tempo_filtrado_graph <- reactive({
    
    cc <- atividade_filtrada_graph()[tempo_viagem == input_tempo_graph()]
    # cc <- filter(atividade_filtrada_graph(), tempo_viagem == input_tempo_graph())
    
    # print(input_tempo_graph())
    
    # print(nrow(cc))
    # print(ncol(cc))
    
    # return(cc)
    
  })
  
make_title_plots <- reactive({
  
  # make title plot
  title_plot_graph <- switch(input$graph_type, 
                             "palma_renda" = i18n()$t("Desigualdade por renda"), 
                             "palma_cor" = i18n()$t("Desigualdade por cor"),
                             "dumbell_renda" = i18n()$t("Desigualdade por renda"),
                             "dumbell_cor" = i18n()$t("Desigualdade por cor")) 
  
  title_plot_modo <- switch(input$modo_todos_graph, 
                            "tp" = i18n()$t("por transporte público"), 
                            "caminhada" = i18n()$t("por caminhada"),
                            "bicicleta" = i18n()$t("por bicicleta")) 
  
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
                                               "dumbell_cor" = i18n()$t("ao equipamento de saúde alta mais próximo")))
  
  title_plot_df <- data.frame(graph = title_plot_graph,
                              modo = title_plot_modo,
                              atividade = title_plot_atividade)
  
  print(title_plot_df$graph)
  print(title_plot_df$modo)
  print(title_plot_df$atividade)
  
  return(title_plot_df)
  
  
})
  
  
  # Render graphs
  output$output_graph <- renderHighchart({
    
    
    # GRAPH FOR PALMA RATIO 
    if (input$graph_type %in% c("palma_renda", "palma_cor")) {
    
    new <- tempo_filtrado_graph() %>%
      # mutate(sigla_muni = factor(sigla_muni, levels = munis_df$abrev_muni, labels = munis_df$name_muni)) %>%
      mutate(nome_muni = factor(nome_muni))
      # mutate(sigla_muni = forcats::fct_reorder(sigla_muni, palma_ratio))
    
    new <- arrange(new, desc(palma_ratio))
    
    
    title_plot <- sprintf("%s %s %s %s %s %s", make_title_plots()$graph, make_title_plots()$modo, make_title_plots()$atividade, i18n()$t("em"), input_tempo_graph(), i18n()$t("minutos"))
    legend_plot <- switch(input$graph_type, 
                          "palma_renda" = i18n()$t("Razão da acessibilidade cumulativa dos 10% mais ricos pelos 40% mais pobres"), 
                          "palma_cor" = i18n()$t("Razão da acessibilidade cumulativa da população branca pela população negra")) 
    
    print(title_plot)
    print(legend_plot)
    
      
      hchart(new, "bar", hcaes(x = nome_muni, y = palma_ratio),
             name = "Palma Ratio") %>%
        hc_title(text = title_plot,
                 align = "left", x = 50) %>%
        hc_subtitle(text = legend_plot,
                    align = "left", x = 50) %>%
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
                                                    style = list(fontSize = 15,
                                                                 color = "white",
                                                                 textOutline = "0.3px white",
                                                                 fontWeight = "regular"))))
      
    } else if (input$graph_type %in% c("dumbell_renda", "dumbell_cor")) {
      
      
      title_plot <- sprintf("%s %s %s", make_title_plots()$graph, make_title_plots()$modo, make_title_plots()$atividade)
      legend_plot <- switch(input$graph_type, 
                            "dumbell_renda" = i18n()$t("Média do tempo mínimo de viagem por renda"), 
                            "dumbell_cor" = i18n()$t("Média do tempo mínimo de viagem por cor")) 
      
      
      # arrange by Q1
      
      teste_dumbell <- arrange(atividade_filtrada_graph(), -low)
      
      highchart() %>%
        hc_xAxis(categories = teste_dumbell$nome_muni, labels = list(style = list(fontSize = 15))) %>%
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
  
  # prepare data to be downloaded
  prepare_data_download <- reactive({
    
    # select data 
      
    if (input$graph_type %in% c("palma_renda", "palma_cor")) {
      
      
      data_out <- tempo_filtrado_graph()
      
    } 
    else if (input$graph_type %in% c("dumbell_renda", "dumbell_cor")) {
      
      data_out <- atividade_filtrada_graph()
      
    }
    
    # define attributes name based on language
    attributes_lookup <- data.frame(atividade = c("TT", "ST", "SB", "SM", "SA", "ET", "EI", "EF", "EM"), 
                                    nome = c(i18n()$t("trabalho_total"), 
                                             i18n()$t("saude_total"),
                                             i18n()$t("saude_baixa"),
                                             i18n()$t("saude_media"),
                                             i18n()$t("saude_alta"),
                                             i18n()$t("educacao_total"),
                                             i18n()$t("educacao_infantil"),
                                             i18n()$t("educacao_fundamental"),
                                             i18n()$t("educacao_media")))
    
    output_csv_palma <- data_out %>%
      mutate(modo = case_when(
        modo == 'bicicleta' ~ i18n()$t('bicicleta'),
        modo == 'caminhada' ~ i18n()$t('caminhada'),
        modo == 'tp' ~ i18n()$t('tp'))) %>%
      left_join(attributes_lookup, by = "atividade")
    
    
    # define column names based on language
    if (input$graph_type %in% c("palma_renda", "palma_cor")) {
      
      output_csv_palma <- output_csv_palma %>% select(nome_muni, sigla_muni, modo, indicador, atividade = nome, 
                                                      tempo_viagem, pobre, rico, palma_ratio)
      
    colnames(output_csv_palma) <- c(i18n()$t("nome_muni"),
                            i18n()$t("sigla_muni"),
                            i18n()$t("modo"),
                            i18n()$t("indicador"),
                            i18n()$t("atividade"),
                            i18n()$t("tempo_viagem"),
                            "low",
                            "high",
                            "ratio") 
    
    }
    
    else if (input$graph_type %in% c("dumbell_renda", "dumbell_cor")) {
      
      # define column names based on language
      
      output_csv_palma <- output_csv_palma %>% select(nome_muni, sigla_muni, modo, indicador, atividade = nome, total, low, high)
      
      colnames(output_csv_palma) <- c(i18n()$t("nome_muni"),
                              i18n()$t("sigla_muni"),
                              i18n()$t("modo"),
                              i18n()$t("indicador"),
                              i18n()$t("atividade"),
                              "total",
                              "low",
                              "high"
      )
      
    }
    
    return(output_csv_palma)
    
  })

  # DOWNLOAD BUTTON -----------------------------------------------------------------------------
  
  # data
  output$downloadData <- downloadHandler(
    

    
    # generate button with data
    filename = function() {
        
      if (input$graph_type %in% c("palma_renda", "palma_cor")) {
        
        sprintf("acess_%s_%s_%s_%s.csv", i18n()$t(input$graph_type), i18n()$t(input$modo_todos_graph), input_atividade_graph(), input_tempo_graph())
        
      } 
      else if (input$graph_type %in% c("dumbell_renda", "dumbell_cor")) {
        
        file_name <- sprintf("acess_%s_%s_%s.csv", i18n()$t(input$graph_type), i18n()$t(input$modo_todos_graph), input_atividade_graph())
        
      }
      
    },
    content = function(file) {
      
      write.csv(prepare_data_download(), file, row.names = FALSE, quote = FALSE)
    
      }
  
    
  )
  
  # plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      
      
      if (input$graph_type %in% c("palma_renda", "palma_cor")) {
        
        sprintf("acess_%s_%s_%s_%s.png", i18n()$t(input$graph_type), i18n()$t(input$modo_todos_graph), input_atividade_graph(), input_tempo_graph())
        
      } 
      else if (input$graph_type %in% c("dumbell_renda", "dumbell_cor")) {
        
        sprintf("acess_%s_%s_%s.png", i18n()$t(input$graph_type), i18n()$t(input$modo_todos_graph), input_atividade_graph())
        
      }
      
    },
    content = function(file) {
      
      
      if (input$graph_type %in% c("palma_renda", "palma_cor")) {
        
        
        title_plot <- sprintf("%s %s %s %s %s %s", make_title_plots()$graph, make_title_plots()$modo, make_title_plots()$atividade, i18n()$t("em"), input_tempo_graph(), i18n()$t("minutos"))
        legend_plot <- switch(input$graph_type, 
                              "palma_renda" = i18n()$t("Razão da acessibilidade cumulativa dos 10% mais ricos pelos 40% mais pobres"), 
                              "palma_cor" = i18n()$t("Razão da acessibilidade cumulativa da população branca pela população negra")) 
      
      new_save <- tempo_filtrado_graph() %>%
        mutate(nome_muni = factor(nome_muni)) %>%
        mutate(nome_muni = forcats::fct_reorder(nome_muni, palma_ratio))
      
      plot_save <- ggplot(data = new_save)+
        geom_col(aes(y = palma_ratio, x = nome_muni), fill = "#1D5A79") +
        geom_text(aes(y = palma_ratio, x = nome_muni, label = round(palma_ratio,1)), size = 3, position = position_stack(vjust = 0.88), color='gray99') +
        geom_hline(yintercept = 1, color = "grey90", linetype = "dashed") +
        scale_y_continuous(breaks = c(0, 1, 3, 6, 9))+
        coord_flip()+
        theme_ipsum(grid = "X", base_family = "Helvetica")+
        labs(x = "", y = ifelse(input$graph_type == "palma_renda", i18n()$t("Razão de Palma"), i18n()$t("Razão de Desigualdade por Cor")),
             title = title_plot,
             subtitle = legend_plot,
             caption = i18n()$t("Projeto Acesso a Oportunidades - IPEA")
        )+
        theme(plot.title = element_text(size=9, hjust=0),
              plot.subtitle = element_text(size = 7, hjust=0),
              plot.caption = element_text(size=7),
              axis.text.y = element_text(size = 6),
              axis.text.x = element_text(size = 6),
              axis.title.x = element_text(size = 6),
              legend.text = element_text(size = 7),
              plot.margin = unit(c(3,3,3,3), "mm"))
        
      } 
      
      
      else if (input$graph_type %in% c("dumbell_renda", "dumbell_cor")) {
        
        
        title_plot <- sprintf("%s %s %s", make_title_plots()$graph, make_title_plots()$modo, make_title_plots()$atividade)
        legend_plot <- switch(input$graph_type, 
                              "dumbell_renda" = i18n()$t("Média do tempo mínimo de viagem por renda"), 
                              "dumbell_cor" = i18n()$t("Média do tempo mínimo de viagem por cor")) 
        
      new_save <- atividade_filtrada_graph() %>%
        mutate(nome_muni = factor(nome_muni))
      
      # para plotar as legendas
      new_save_legend <- new_save %>% tidyr::gather(tipo, valor, total:high)
      
          plot_save <- ggplot(data = new_save) + 
            geom_dumbbell(aes(x = high, xend = low, y = forcats::fct_reorder(nome_muni, low)), 
                          size=2, color="gray80", alpha=.8, colour_x = "steelblue4", colour_xend = "springgreen4") +
            geom_point(data = new_save_legend, aes(x = valor, y = nome_muni, color = tipo), size = 2)+
          # geom_point(aes(x = total, y = nome_muni), color = "black", size = 2)+
          scale_color_manual(values=c('black', 'steelblue4', 'springgreen4'), 
                             name="", 
                             labels=c('Total', 
                                      ifelse(input$graph_type == "dumbell_renda", i18n()$t("Pobres Q1"), i18n()$t("Negros")), 
                                      ifelse(input$graph_type == "dumbell_renda", i18n()$t("Ricos Q5"), i18n()$t("Brancos")))) +
        theme_ipsum(grid= "X", base_family = "Helvetica") +
        labs(x = i18n()$t("Minutos"), y = "", title = title_plot, subtitle = legend_plot)+
        theme(plot.title = element_text(size=9, hjust=0),
              plot.subtitle = element_text(size = 7, hjust=0),
              plot.caption = element_text(size=7),
              axis.text.y = element_text(size = 6),
              axis.text.x = element_text(size = 6),
              axis.title.x = element_text(size = 7),
              plot.margin = unit(c(3,3,3,3), "mm"),
              legend.text = element_text(size = 7),
              legend.position = "bottom")
      
    }
      
      ggsave(filename = file, plot = plot_save, dpi = 300, width = 16.5, height = 10, units = "cm")
      
    }
  )
  
  
    
  
  # 4) REACTIVE TO FILTER THE CITY -----------------------------------------------------------------
  a_city <- reactive({
    
    if(input$cidade != "") {input$cidade} else {"fake"}
    
    
  })
  
  cidade_filtrada <- reactive({
    
    acess[sigla_muni == a_city()]
    
  })
  
  
  
  # 5) REACTIVE TO FILTER THE MODE -----------------------------------------------------------------
  a <- reactive({
    
    if (a_city() %in% c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec') | input$indicador == "US") {input$modo_todos}
    
    else if(a_city() %in% c('bsb', 'sal', 'man', 'goi', 'bel', 'gua', 'cam', 'slz', 'sgo', 'mac', 'duq', 'cgr', 'nat', 'fake')) {
      
      # # Reactive para a modo para indicador cumulativo
      input$modo_ativo }
    
  })
  
  # Reactive para a modo
  modo_filtrado <- reactive({
    
    cidade_filtrada()[modo == a()]
    
  })
  
  # 6) REACTIVE TO FILTER THE INDICATOR ------------------------------------------------------------
  indicador_filtrado <- reactive({
    
    b_indicador <- ifelse(input$indicador == "US", c("P00|R00|E00|S00"), input$indicador)
    
    modo_filtrado() %>% dplyr::select(id_hex, P001, matches(b_indicador))
    
  })
  
  
  # 7) REACTIVE TO FILTER THE ACTIVITY ------------------------------------------------------------
  # Reactive para a atividade para indicador cumulativo
  atividade_filtrada <- reactive({
    
    indicador_filtrado() %>% dplyr::select(id_hex, P001, matches(input$atividade_cum))
    
  })
  
  
  # # Reactive para a atividade para indicador minimo
  atividade_filtrada_min <- reactive({
    
    indicador_filtrado() %>% dplyr::select(id_hex, P001, matches(input$atividade_min)) %>%
      rename(id_hex = 1, P001 = 2, valor = 3) %>%
      mutate(id = 1:n()) %>%
      mutate(popup = paste0(i18n()$t("<strong>População:</strong> "), P001, i18n()$t("<br><strong>Valor da acessibilidade:</strong> "), round(valor, 0), " ", i18n()$t("minutos")))
    
  })
  
  # Reactive para a atividade para indicador de uso do solo
  atividade_filtrada_us <- reactive({
    
    input_us <- ifelse(input$atividade_us == "ET", "E001",
                       ifelse(input$atividade_us == "EI", "E002",
                              ifelse(input$atividade_us == "EF", "E003",
                                     ifelse(input$atividade_us == "EM", "E004"))))
    
    indicador_filtrado() %>% dplyr::select(id_hex, P001, matches(input_us)) %>%
      rename(id_hex = 1, P001 = 2, valor = 3) %>%
      mutate(id = 1:n()) %>%
      mutate(popup = paste0(i18n()$t("<strong>População:</strong> "), P001, i18n()$t("<br><strong>Valor do indicador:</strong> "), round(valor, 0), " ", i18n()$t("atividades")))
    
  })
  
  atividade_filtrada_min_sf <- reactive({
    
    atividade_filtrada_min() %>% setDT() %>%
      merge(hex, by = "id_hex", all.x = TRUE, sort = FALSE) %>% 
      st_sf(crs = 4326)
    
  })
  
  atividade_filtrada_us_sf <- reactive({
    
    atividade_filtrada_us() %>% setDT() %>%
      merge(hex, by = "id_hex", all.x = TRUE, sort = FALSE) %>% 
      st_sf(crs = 4326)
    
  })
  
  # 8) REACTIVE TO FILTER THE TIME THRESHOLD -------------------------------------------------------
  b <- reactive({
    
    if (a_city() %in% c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec') & input$modo_todos %in% "tp") {input$tempo_tp}
    
    else if  (a_city() %in% c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec') & input$modo_todos %in% c("caminhada", "bicicleta")) {input$tempo_ativo_tp}
    
    else if (a_city() %in% c('bsb', 'sal', 'man', 'goi', 'bel', 'gua', 'cam', 'slz', 'sgo', 'mac', 'duq', 'cgr', 'nat', 'fake')) {input$tempo_ativo}
    
  })
  
  # Reactive para o tempo
  tempo_filtrado <- reactive({
    
    atividade_filtrada() %>% dplyr::select(id_hex, P001, matches(as.character(b()))) %>%
      rename(id_hex = 1, P001 = 2, valor = 3) %>%
      mutate(id = 1:n()) %>%
      # create popup
      mutate(popup = paste0(i18n()$t("<strong>População:</strong> "), P001, i18n()$t("<br><strong>Valor da acessibilidade:</strong> "), round(valor, 1), "%"))
    
    # print(unique(time1$modo))
    
    # print(nrow(time1))
    
    # return(time1)
    
  })
  
  # Reactive para o tempo
  tempo_filtrado_sf <- reactive({
    
    tempo_filtrado() %>% setDT() %>%
      merge(hex, by = "id_hex", all.x = TRUE, sort = FALSE) %>% 
      st_sf(crs = 4326)
    
    
  })
  
  
  
  
  # 9) RENDER BASEMAP -------------------------------------------------------
  # baseMap
  output$map <- renderMapdeck({
    
    mapdeck(location = c(-43.95988, -19.902739), zoom = 3)
    
    
  })
  
  
  # Stop the loading page here !
  waiter_hide()
  
    
  # 10) OBSERVER TO RENDER THE CITY INDICATOR -------------------------------------------------------
  observeEvent({input$cidade},{
    
    
    # Filtrar limites
    limits_filtrado <- filter(limits, abrev_muni == input$cidade)
    
    if (input$cidade != "") {
      
      centroid_go <- filter(centroids, abrev_muni == input$cidade)
      
      if(input$cidade %in% c("spo", "man", "cgr", "bsb")) {
        
        zoom1 <- 9
        
      } else if(input$cidade %in% c("mac", "for", "nat", "rec", "sal", "slz", "bho")) {
        
        zoom1 <- 11
        
      } else {zoom1 <- 10}
      
      # if(input$indicador == "CMA") {
      # 
      #   data1 <- tempo_filtrado()
      #   layer_id1 <- "acess_cum_go"
      #   palette1 <- "inferno"
      #   legend_options1 <- list(title = "Porcentagem de Oportunidades Acessíveis")
      # 
      # } else if (input$indicador == "TMI")  {
      # 
      #   data1 <- atividade_filtrada_min
      #   layer_id1 <- "acess_min_go"
      #   palette1 <- "inferno"
      #   legend_options1 <- list(title = "Porcentagem de Oportunidades Acessíveis")
      # 
      # 
      # }
      
      proxy <- mapdeck_update(map_id = "map") %>%
        mapdeck_view(location = c(centroid_go$lon, centroid_go$lat), zoom = zoom1,
                     duration = 3000, transition = "fly")
      
      if (input$indicador == "CMA") {
        
        proxy %>%
          clear_polygon(layer_id = "acess_min_go") %>%
          clear_legend(layer_id = "acess_min_go") %>%
          add_polygon(
            data = limits_filtrado,
            stroke_colour = "#616A6B",
            stroke_width = 100,
            fill_opacity = 0,
            update_view = FALSE,
            focus_layer = FALSE,
          ) %>%
          add_polygon(
            data = tempo_filtrado_sf(),
            fill_colour = "valor",
            fill_opacity = 200,
            layer_id = "acess_cum_go",
            palette = "inferno",
            update_view = FALSE,
            focus_layer = FALSE,
            # auto_highlight = TRUE,
            tooltip = "popup",
            legend = TRUE,
            legend_options = list(title = i18n()$t("Porcentagem de Oportunidades Acessíveis")),
            legend_format = list( fill_colour = as.integer)
          )
        
        
      } else if (input$indicador == "TMI") {
        
        # create viridis scale in the reverse direction
        # create matrix
        colorss <- colourvalues::color_values_rgb(x = 1:256, "viridis")
        # invert matrix
        colorss <- apply(colorss, 2, rev)[, 1:3]
        
        proxy %>%
          clear_polygon(layer_id = "acess_cum_go") %>%
          clear_legend(layer_id = "acess_cum_go") %>%
          add_polygon(
            data = limits_filtrado,
            stroke_colour = "#616A6B",
            stroke_width = 100,
            fill_opacity = 0,
            update_view = FALSE,
            focus_layer = FALSE
          ) %>%
          add_polygon(
            data = atividade_filtrada_min_sf(),
            fill_colour = "valor",
            fill_opacity = 200,
            layer_id = "acess_min_go",
            palette = colorss,
            update_view = FALSE,
            tooltip = "popup",
            legend = TRUE,
            legend_options = list(title = i18n()$t("Minutos até a oportunidade mais próxima")),
            legend_format = list( fill_colour = as.integer)
          )
        
      } else if (input$indicador == "US") {
        
        # create viridis scale in the reverse direction
        # create matrix
        colorss <- colourvalues::color_values_rgb(x = 1:256, "viridis")
        # invert matrix
        colorss <- apply(colorss, 2, rev)[, 1:3]
        
        proxy %>%
          clear_polygon(layer_id = "acess_cum_go") %>%
          clear_legend(layer_id = "acess_cum_go") %>%
          add_polygon(
            data = limits_filtrado,
            stroke_colour = "#616A6B",
            stroke_width = 100,
            fill_opacity = 0,
            update_view = FALSE,
            focus_layer = FALSE
          ) %>%
          add_polygon(
            data = atividade_filtrada_us(),
            fill_colour = "valor",
            fill_opacity = 200,
            layer_id = "acess_us_go",
            palette = colorss,
            update_view = FALSE,
            tooltip = "popup",
            legend = TRUE,
            legend_options = list(title = i18n()$t("Minutos até a oportunidade mais próxima")),
            legend_format = list( fill_colour = as.integer)
          )
        
      }
      
      
    } 
    
    
    
  })
  
  
  observeEvent({c(input$indicador, 
                  input$modo_todos, input$modo_ativo, 
                  input$atividade_cum, input$atividade_min, 
                  input$tempo_tp, input$tempo_ativo_tp, input$tempo_ativo)},{
                    
                    
                    if (a_city() != "fake") {
                      
                      
                      if (input$indicador == "TMI") {
                        
                        # create viridis scale in the reverse direction
                        # create matrix
                        colorss <- colourvalues::color_values_rgb(x = 1:256, "viridis")
                        # invert matrix
                        colorss <- apply(colorss, 2, rev)[, 1:3]
                        
                        mapdeck_update(map_id = "map") %>%
                          clear_polygon(layer_id = "acess_cum_go") %>%
                          clear_legend(layer_id = "acess_cum_go") %>%
                          add_polygon(
                            data = atividade_filtrada_min_sf(),
                            fill_colour = "valor",
                            fill_opacity = 200,
                            layer_id = "acess_min_go",
                            palette = colorss,
                            update_view = FALSE,
                            tooltip = "popup",
                            legend = TRUE,
                            legend_options = list(title = i18n()$t("Minutos até a oportunidade mais próxima")),
                            legend_format = list( fill_colour = as.integer)
                          )
                        
                      } else 
                        
                        if (input$indicador == "CMA") {
                          
                          mapdeck_update(map_id = "map") %>%
                            clear_polygon(layer_id = "acess_min_go") %>%
                            clear_legend(layer_id = "acess_min_go") %>%
                            add_polygon(
                              data = tempo_filtrado_sf(),
                              fill_colour = "valor",
                              fill_opacity = 200,
                              layer_id = "acess_cum_go",
                              palette = "inferno",
                              update_view = FALSE,
                              focus_layer = FALSE,
                              # auto_highlight = TRUE,
                              tooltip = "popup",
                              legend = TRUE,
                              legend_options = list(title = i18n()$t("Porcentagem de Oportunidades Acessíveis")),
                              legend_format = list( fill_colour = as.integer)
                            )
                          
                        } else 
                          
                          if (input$indicador == "US") {
                          
                          mapdeck_update(map_id = "map") %>%
                            clear_polygon(layer_id = "acess_cum_go") %>%
                            clear_legend(layer_id = "acess_cum_go") %>%
                            add_polygon(
                              data = atividade_filtrada_us(),
                              fill_colour = "valor",
                              fill_opacity = 200,
                              layer_id = "acess_us_go",
                              palette = 'inferno',
                              update_view = FALSE,
                              tooltip = "popup",
                              legend = TRUE,
                              legend_options = list(title = i18n()$t("Minutos até a oportunidade mais próxima")),
                              legend_format = list( fill_colour = as.integer)
                            )
                          
                        }
                      
                      
                    }
                  })
  
  
  # fim <- reactive({
  #   
  #   if(input$indicador == "CMA") {df <- tempo_filtrado() %>% setDT()} 
  #   else if (input$indicadot == "TMI") {df <- atividade_filtrada_min() %>% setDT()}
  #   
  # })
  # 
  # 
  # 
  # fim_v1 <- reactive({
  #   
  #   city_pop <- sum(fim()$P001)
  #   
  #   fim_ordered <- setorder(fim(), valor, P001)
  #   
  #   fim_ordered_cum <- fim_ordered[, cumsum := cumsum(P001)/city_pop]
  #   
  # })
  # 
  # 
  # 
  # 
  # global <- reactiveValues()
  # 
  # 
  # # produce plot for the whole city when we change cities
  # output$plot <- renderPlotly({
  #   
  #   if(input$cidade == "") {return()} else {
  #     
  #     # go <- ggplot(fim_v1())+
  #     #   geom_bar(aes(x = quebra, y = N, fill = global$high), stat = "identity")+
  #     #   scale_fill_manual(values = c("yes"="tomato", "no"="gray" ), guide = FALSE )+
  #     #   theme_minimal()+
  #     #   labs(x = "", y = "")+
  #     #   theme(plot.margin = unit(c(0,0,0,0), "cm"),
  #     #         rect = element_rect(fill = "transparent"))
  #     # 
  #     # ui <- ggplotly(go)
  #     # 
  #     # print(ui)
  #     # 
  #     # return(ui)
  #     
  #     
  #     plotly_go <-  plot_ly(fim_v1(), x = ~cumsum, y = ~valor, type = 'scatter', mode = 'lines') %>%
  #       layout(title = "Indicador Cumulativo",
  #              xaxis = list(title = "% populacao"),
  #              yaxis = list(title = "% oportunidades"))
  #     
  #     return(plotly_go)
  #     
  #     
  #   }
  #   
  # }
  # )
  # 
  # 
  # # observer to update only the highlighted bar on MAP CLICK
  # observeEvent({input$map_polygon_click},{
  #   # print( input$map_polygon_click )
  #   
  #   js <- input$map_polygon_click
  #   lst <- jsonlite::fromJSON( js )
  #   row <- (lst$index) + 1
  #   
  #   
  #   # get problematic value of hist
  #   valor_prob <- fim_v1()[id == row]$valor
  #   cumsum_prob <- fim_v1()[id == row]$cumsum
  #   
  #   
  #   # pegar quebra
  #   
  #   print(valor_prob)
  #   print(cumsum_prob)
  #   
  #   
  #   plotlyProxy("plot") %>%
  #     plotlyProxyInvoke("deleteTraces", list(as.integer(1))) %>%
  #     plotlyProxyInvoke("addTraces", list(x=c(cumsum_prob, cumsum_prob),
  #                                         y=c(valor_prob, valor_prob)))
  #   # marker = list(size = 10,
  #   #               color = 'rgba(255, 182, 193, .9)',
  #   #               line = list(color = 'rgba(152, 0, 0, .8)',
  #   #                           width = 2)))
  #   # plotlyProxyInvoke("addTraces", list(x=c(as.factor(quebra_prob), as.factor(quebra_prob)),
  #   #                                     y=c(as.factor(valor_prob), as.factor(valor_prob)),
  #   #                                     type = 'bar',
  #   #                                     marker = list(color = 'rgb(211,84,0)',
  #   #                                                   line = list(color = 'rgb(8,48,107)',
  #   #                                                               width = 1.5))))
  #   
  # })
  
  
  
}