library(shiny)
library(dplyr)
library(sf)
library(readr)
library(data.table)
library(mapdeck)
library(waiter) # remotes::install_github("JohnCoene/waiter")



# Use GForce Optimisations in data.table operations
options(datatable.optimize=Inf)

# set number of threads used in data.table
data.table::setDTthreads(percent = 100)


# register mapbox api key
my_api <- data.table::fread("mapbox_key.txt", header = F)
# set_token("")
set_token(my_api$V1)




# abrir acessibilidade

acess <- read_rds("data/acess_wide.rds")

hex <- read_rds("data/hex.rds")

centroids <- read_rds("data/cities_centroids.rds")

limits<- read_rds(("data/cities_limits.rds"))

# Define a server for the Shiny app
function(input, output) {
  
  # the modal dialog where the user can enter the query details.
  query_modal <- modalDialog(
    title = h1("Instruções para uso do mapa interativo"),
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
    includeHTML("carousel_2.html"),
    easyClose = FALSE,
    size = "m",
    footer = modalButton("Fechar")
    )

  # Show the model on start up ...
  showModal(query_modal)
  
  
  
  a_city <- reactive({
    
    if(input$cidade != "") {input$cidade} else {"fake"}
    
    
  })
  
  # Reactive expression for the data subsetted to what the user selected
  cidade_filtrada <- reactive({
    acess[sigla_muni == a_city()]
  })
  
  
  
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
  
  # Reactive para o indicador
  indicador_filtrado <- reactive({
    
    modo_filtrado() %>% dplyr::select(id_hex, P001, modo, matches(input$indicador)) %>% setDT()
    
  })

    
  # # Reactive para a atividade para indicador cumulativo
  atividade_filtrada <- reactive({
    indicador_filtrado() %>% dplyr::select(id_hex, P001, matches(input$atividade_cum))
    # modo_filtrado()[atividade == input$atividade_cum]
  })
  
  
  # Atividade filtrada para o indicador minimo
  # # Reactive para a atividade para indicador cumulativo
  atividade_filtrada_min <- reactive({
    indicador_filtrado() %>% dplyr::select(id_hex, P001, matches(input$atividade_min)) %>%
      merge(hex, by = "id_hex", all.x = TRUE) %>% 
      rename(id_hex = 1, P001 = 2, valor = 3, geometry = 4) %>%
      mutate(popup = paste0("<strong>População:</strong> ", P001, "<br><strong>Valor da acessibilidade:</strong> ", round(valor, 0), " minutos")) %>%
      st_sf(crs = 4326)
    
  })
  
  
  b <- reactive({
    
    if (a_city() %in% c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec') & input$modo_todos %in% "tp") {input$tempo_tp}
    
    else if  (a_city() %in% c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec') & input$modo_todos %in% c("caminhada", "bicicleta")) {input$tempo_ativo_tp}
    
    else if (a_city() %in% c('bsb', 'sal', 'man', 'goi', 'bel', 'gua', 'cam', 'slz', 'sgo', 'mac', 'duq', 'cgr', 'nat', 'fake')) {input$tempo_ativo}
    
  })
  
  # Reactive para o tempo
  tempo_filtrado <- reactive({
    
    atividade_filtrada() %>% dplyr::select(id_hex, P001, matches(as.character(b()))) %>%
      merge(hex, by = "id_hex", all.x = TRUE) %>%
      rename(id_hex = 1, P001 = 2, valor = 3, geometry = 4) %>%
      # create popup
      mutate(popup = paste0("<strong>População:</strong> ", P001, "<br><strong>Valor da acessibilidade:</strong> ", round(valor, 1), "%")) %>%
      st_sf(crs = 4326)
    
    
  })

  
    
  # baseMap
  output$map <- renderMapdeck({
    
    mapdeck(location = c(-43.95988, -19.902739), zoom = 3)
    
    
  })
  
  
  waiter_hide()
  
  
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
            data = tempo_filtrado(),
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
            data = atividade_filtrada_min(),
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
                          data = atividade_filtrada_min(),
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
                            data = tempo_filtrado(),
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
  
  
  
}
