
# 1) LOAD DATA ----------------------------------------------------------------------------------------

acess <- read_rds("data/acess_wide.rds")

hex <- read_rds("data/hex.rds")

centroids <- read_rds("data/cities_centroids.rds")

limits<- read_rds(("data/cities_limits.rds"))

# Define a server for the Shiny app
function(input, output, session) {
  
  # 2) MODAL WITH INSTRUCTIONS AT STARTUP ----------------------------------------------------------
  query_modal <- modalDialog(
    title = HTML("<h1>Instruções para uso do mapa interativo na aba ao lado &nbsp<i class=\"fas fa-arrow-right\"></i></h1>"),
    # HTML("1) Selecione a cidade na aba ao lado<br>"),
    # HTML("&nbsp;<br>"),
    # # HTML("1) Selecione a cidade na aba <strong>Escolha a cidade</strong><br>"),
    # HTML("2) Modifique as opções de:<br>"),
    # HTML("&nbsp;&nbsp;&nbsp;&nbsp;- Indicador de acessibilidade<br>"),
    # HTML("&nbsp;&nbsp;&nbsp;&nbsp;- Modo de transporte<br>"),
    # HTML("&nbsp;&nbsp;&nbsp;&nbsp;- Atividade<br>"),
    # HTML("&nbsp;&nbsp;&nbsp;&nbsp;- Tempo de viagem<br>"),
      # HTML("<img src=\"https://media.giphy.com/media/mEjT1jbXelaEg/giphy.gif\" alt=\"Smiley face\" height=\"42\" align=\"right\">"),
    # # HTML("O mapa da cidade é gerado para opções predefinidas de indicador, modo, atividade e tempo<br>"),
    includeHTML("www/carousel_2.html"),
    easyClose = FALSE,
    size = "m",
    footer = modalButton("Fechar")
    )

  # Show the model on start up ...
  showModal(query_modal)
  

  # 3) OBSERVER TO TIMEOUT IF USER IS INACTIVE ----------------------------------------------------------

  observeEvent(input$timeOut, { 
    print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
    showModal(modalDialog(
      title = "Timeout",
      HTML(paste("Session timeout due to", input$timeOut, "inactivity -", Sys.time()), "<br>"),
      HTML("<a href=\".\">Recarregar</a>"),
      footer = NULL
    ))
    session$close()
  })  
  
  
  
  a_city <- reactive({
    
    if(input$cidade != "") {input$cidade} else {"fake"}
    
    
  })
  
  # 4) REACTIVE TO FILTER THE CITY -----------------------------------------------------------------
  cidade_filtrada <- reactive({
    
    acess[sigla_muni == a_city()]
    
  })
  
  
  
  # 5) REACTIVE TO FILTER THE MODE -----------------------------------------------------------------
  a <- reactive({
    
    if (a_city() %in% c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec')) {input$modo_todos}
    
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
    
    modo_filtrado() %>% dplyr::select(id_hex, P001, matches(input$indicador))
    
  })

    
  # 7) REACTIVE TO FILTER THE ACTIVITY ------------------------------------------------------------
  # Reactive para a atividade para indicador cumulativo
  atividade_filtrada <- reactive({
    
    indicador_filtrado() %>% dplyr::select(id_hex, P001, matches(input$atividade_cum))
    
  })
  
  
  # # Reactive para a atividade para indicador cumulativo
  atividade_filtrada_min <- reactive({
    
    indicador_filtrado() %>% dplyr::select(id_hex, P001, matches(input$atividade_min)) %>%
      rename(id_hex = 1, P001 = 2, valor = 3) %>%
      mutate(id = 1:n()) %>%
      mutate(popup = paste0("<strong>População:</strong> ", P001, "<br><strong>Valor da acessibilidade:</strong> ", round(valor, 0), " minutos"))
  
    })
  
  atividade_filtrada_min_sf <- reactive({
    
    atividade_filtrada_min() %>% setDT() %>%
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
      mutate(popup = paste0("<strong>População:</strong> ", P001, "<br><strong>Valor da acessibilidade:</strong> ", round(valor, 1), "%"))
    
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
            legend_options = list(title = "Porcentagem de Oportunidades Acessíveis"),
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
            legend_options = list(title = "Minutos até a oportunidade mais próxima"),
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
                          legend_options = list(title = "Minutos até a oportunidade mais próxima"),
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
                            legend_options = list(title = "Porcentagem de Oportunidades Acessíveis"),
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