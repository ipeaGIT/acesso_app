# prepare data to be downloaded
prepare_data_download <- reactive({
  
  
  
  # select data 
  
  if (input$graph_type %in% c("palma_renda", "palma_cor")) {
    
    
    data_out <- tempo_filtrado_graph()
    
  } 
  else if (input$graph_type %in% c("dumbell_renda", "dumbell_cor")) {
    
    data_out <- atividade_filtrada_graph()
    
  }
  
  # print(tempo_filtrado_graph())
  
  # define attributes name based on language
  attributes_lookup <- data.table(atividade = c("TT", "ST", "SB", "SM", "SA", "ET", "EI", "EF", "EM"), 
                                  nome = c(i18n()$t("trabalho_total"), 
                                           i18n()$t("saude_total"),
                                           i18n()$t("saude_baixa"),
                                           i18n()$t("saude_media"),
                                           i18n()$t("saude_alta"),
                                           i18n()$t("educacao_total"),
                                           i18n()$t("educacao_infantil"),
                                           i18n()$t("educacao_fundamental"),
                                           i18n()$t("educacao_media")))
  
  # output_csv_palma <- data_out %>%
  #   mutate(modo = case_when(
  #     modo == 'bicicleta' ~ i18n()$t('bicicleta'),
  #     modo == 'caminhada' ~ i18n()$t('caminhada'),
  #     modo == 'tp' ~ i18n()$t('tp'))) %>%
  #   left_join(attributes_lookup, by = "atividade")
  
  # data_out[, modo := ifelse(modo == 'bicicleta', i18n()$t('bicicleta'),
  #                           ifelse(modo == 'caminhada', i18n()$t('caminhada'),
  #                                  ifelse(modo == 'tp', i18n()$t('tp'), modo)))]
  
  # print(data_out)
  # print(attributes_lookup)
  
  output_csv_palma <- merge(
    data_out, attributes_lookup,
    by = "atividade",
    all.x = TRUE,
    sort = FALSE
  )
  
  # print(output_csv_palma)
  
  
  # define column names based on language
  if (input$graph_type %in% c("palma_renda", "palma_cor")) {
    
    
    if (input$graph_type == c("palma_renda")) {
      
    output_csv_palma <- output_csv_palma[, .(name_muni, abbrev_muni, year, mode, indicador, atividade = nome, 
                                             tempo_viagem, pobre, rico, palma_ratio)]
      
    } else if (input$graph_type == c("palma_cor")) {
      
      
    output_csv_palma <- output_csv_palma[, .(name_muni, abbrev_muni, year, mode, indicador, atividade = nome, 
                                             tempo_viagem, brancos, negros, palma_ratio)]
      
    }
    
    
    print(output_csv_palma)
    
    colnames(output_csv_palma) <- c(i18n()$t("nome_muni"),
                                    i18n()$t("sigla_muni"),
                                    i18n()$t("ano"),
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
    
    output_csv_palma <- output_csv_palma[, .(name_muni, abbrev_muni, year, mode, indicador, atividade = nome, total, low, high)]
    
    colnames(output_csv_palma) <- c(i18n()$t("name_muni"),
                                    i18n()$t("sigla_muni"),
                                    i18n()$t("ano"),
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



# UI FOR THE DOWNLOAD BUTTONS -----------------------------------------------------------------

output$ui_download_button <- renderUI({
  
  tagList(
    downloadButton("downloadData", i18n()$t("Baixe os dados (.csv)")),
    downloadButton("downloadPlot", i18n()$t("Baixe o gŕafico (.png)"))
  )
  
})  

# UI FOR THE DOWNLOAD BUTTONS - dictionary -----------------------------------------------------------------

output$ui_download_dic <- renderUI({
  
  actionButton("downloadDic", i18n()$t("Visite a página com o dicionário e todos os dados"), 
               onclick = if(input$selected_language == "pt") "location.href='https://www.ipea.gov.br/acessooportunidades/dados';" else "location.href='https://www.ipea.gov.br/acessooportunidades/en/dados';")
  
  # downloadButton("downloadDic", i18n()$t("Baixe o dicionário dos dados (.xlsx)"))
  
})  


# data
output$downloadData <- downloadHandler(
  
  
  
  # generate button with data
  filename = function() {
    
    if (input$graph_type %in% c("palma_renda", "palma_cor")) {
      
      sprintf("acess_%s_%s_%s_%s.csv", i18n()$t(input$graph_type), i18n()$t(input$modo_todos_graph), 
              input_atividade_graph(), input_tempo_graph())
      
    } 
    else if (input$graph_type %in% c("dumbell_renda", "dumbell_cor")) {
      
      sprintf("acess_%s_%s_%s.csv", i18n()$t(input$graph_type), 
              i18n()$t(input$modo_todos_graph), input_atividade_graph())
      
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
      
      sprintf("acess_%s_%s_%s_%s.png", i18n()$t(input$graph_type), 
              i18n()$t(input$modo_todos_graph), input_atividade_graph(), input_tempo_graph())
      
    } 
    else if (input$graph_type %in% c("dumbell_renda", "dumbell_cor")) {
      
      sprintf("acess_%s_%s_%s.png", i18n()$t(input$graph_type), 
              i18n()$t(input$modo_todos_graph), input_atividade_graph())
      
    }
    
  },
  content = function(file) {
    
    library(ggplot2)
    library(ggalt) # install.packages("ggalt", dependecies = TRUE)
    
    if (input$graph_type %in% c("palma_renda", "palma_cor")) {
      
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
                                                input_atividade_graph() == "SA", i18n()$t("equipamentos de saúde de alta complexidade"), input_atividade_graph())))))))))
      
      
      title_plot <- sprintf("%s %s %s %s \n%s %s %s %s %s", 
                            i18n()$t("Desigualdade de acesso a"),
                            i18n()$t(legend_subtitle),
                            make_title_plots()$mode, 
                            i18n()$t("em"), 
                            input_tempo_graph(), 
                            i18n()$t("minutos"),
                            i18n()$t("entre grupos de"),
                            make_title_plots()$graph,
                            ifelse(input$selected_language == "en", "groups", "")
      )
      
      legend_plot <- switch(input$graph_type, 
                            "palma_renda" = 
                              sprintf("%s %s \n%s", 
                                      i18n()$t("Razão entre a média do número de"), 
                                      i18n()$t(legend_subtitle),
                                      i18n()$t("acessíveis pelos 10% mais ricos pelos 40% mais pobres")), 
                            "palma_cor" = 
                              i18n()$t("Razão da acessibilidade cumulativa da população branca pela população negra")) 
      
      # new_save <- tempo_filtrado_graph() %>%
      #   mutate(nome_muni = factor(nome_muni)) %>%
      #   mutate(nome_muni = forcats::fct_reorder(nome_muni, palma_ratio))
      
      tempo_filtrado_graph()[, name_muni := factor(name_muni)]
      tempo_filtrado_graph()[, name_muni := forcats::fct_reorder(name_muni, palma_ratio)]
      new_save <- copy(tempo_filtrado_graph())
      
      plot_save <- ggplot(data = new_save)+
        geom_col(aes(y = palma_ratio, x = name_muni), fill = "#1D5A79") +
        geom_text(aes(y = palma_ratio, x = name_muni, label = round(palma_ratio,1)), 
                  size = 3, position = position_stack(vjust = 0.88), color='gray99') +
        geom_hline(yintercept = 1, color = "grey90", linetype = "dashed") +
        # scale_y_continuous(breaks = c(0, 1, 3, 6, 9))+
        coord_flip()+
        theme_bw()+
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
      
      title_plot <- sprintf("%s %s \n%s %s %s %s", 
                            i18n()$t("Desigualdade de tempo de viagem"),
                            make_title_plots()$mode,
                            make_title_plots()$atividade,
                            i18n()$t("entre grupos de"),
                            make_title_plots()$graph,
                            ifelse(input$selected_language == "en", "groups", "")
      )
      
      legend_plot <- switch(input$graph_type, 
                            "dumbell_renda" = i18n()$t("Média do tempo mínimo de viagem por renda"), 
                            "dumbell_cor" = i18n()$t("Média do tempo mínimo de viagem por cor")) 
      
      # 
      # new_save <- atividade_filtrada_graph() %>%
      #   mutate(nome_muni = factor(nome_muni))
      
      atividade_filtrada_graph()[, name_muni := factor(name_muni)]
      new_save <- copy(atividade_filtrada_graph())
      
      # para plotar as legendas
      new_save_legend <- new_save %>% tidyr::gather(tipo, valor, total:high)
      
      plot_save <- ggplot(data = new_save) + 
        geom_dumbbell(aes(x = high, xend = low, y = forcats::fct_reorder(name_muni, low)), 
                      size=2, color="gray80", alpha=.8, colour_x = "steelblue4", colour_xend = "springgreen4") +
        geom_point(data = new_save_legend, aes(x = valor, y = name_muni, color = tipo), size = 2)+
        scale_color_manual(values=c('black', 'steelblue4', 'springgreen4'), 
                           name="", 
                           labels=c('Total', 
                                    ifelse(input$graph_type == "dumbell_renda", i18n()$t("Pobres Q1"), i18n()$t("Negros")), 
                                    ifelse(input$graph_type == "dumbell_renda", i18n()$t("Ricos Q5"), i18n()$t("Brancos")))) +
        theme_bw() +
        labs(x = i18n()$t("Minutos"), y = "", title = title_plot, subtitle = legend_plot)+
        expand_limits(x = 0)+
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
