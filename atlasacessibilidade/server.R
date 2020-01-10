library(shiny)
library(dplyr)
library(sf)
# library(leaflet)
library(readr)
library(data.table)
library(mapdeck)



# Use GForce Optimisations in data.table operations
options(datatable.optimize=Inf)

# set number of threads used in data.table
data.table::setDTthreads(percent = 100)


# register mapbox api key
my_api <- data.table::fread("mapbox_key.txt", header = F)
set_token(my_api$V1)




# abrir acessibilidade

acess_cum <- read_rds("data/acess_tp_cum_app_sgeo_wide.rds")

acess_min <- read_rds("data/acess_tp_min_app_sgeo_wide.rds")

hex <- read_rds("data/hex_teste.rds")

# linhas <- read_rds("../../../data/linhas_HMcapacidade/linhas_HMcapacidade.rds")

limits <- read_rds("data/cities_centroids.rds")

# Define a server for the Shiny app
function(input, output) {
  
  # Reactive expression for the data subsetted to what the user selected
  cidade_filtrada <- reactive({
    acess_cum[sigla_muni == input$cidade]
  })
  
  # Reactive expression for the data subsetted to what the user selected
  cidade_filtrada_min <- reactive({
    acess_min[sigla_muni == input$cidade]
  })
  
  
    
  # # Reactive para a modo para indicador cumulativo
  modo_filtrado <- reactive({
    
    
    if (input$cidade %in% c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec')) {
      
      modo_filtrado <- cidade_filtrada()[modo == input$modo_todos]
      
      
    } else if(input$cidade %in% c('bsb', 'sal', 'man', 'goi', 'bel', 'gua', 'cam', 'slz', 'sgo', 'mac', 'duq', 'cgr', 'nat')) {
      
      # # Reactive para a modo para indicador cumulativo
      modo_filtrado <- cidade_filtrada()[modo == input$modo_ativo]
      
      
    }
    
  })
  
    
  # # Reactive para a modo para indicador minimo
  modo_filtrado_min <- reactive({
    
    
    if (input$cidade %in% c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec')) {
      
      cidade_filtrada_min()[modo == input$modo_todos]
      
      
    } else if(input$cidade %in% c('bsb', 'sal', 'man', 'goi', 'bel', 'gua', 'cam', 'slz', 'sgo', 'mac', 'duq', 'cgr', 'nat')) {
      
      # # Reactive para a modo para indicador cumulativo
      cidade_filtrada_min()[modo == input$modo_ativo]
      
      
    }
    
  })
    
  
  
  # # Reactive para a atividade para indicador cumulativo
  atividade_filtrada <- reactive({
    modo_filtrado() %>% dplyr::select(id_hex, P001, matches(input$atividade_cum))
    # modo_filtrado()[atividade == input$atividade_cum]
  })
  
  
  # Atividade filtrada para o indicador minimo
  # # Reactive para a atividade para indicador cumulativo
  atividade_filtrada_min <- reactive({
    modo_filtrado_min() %>% dplyr::select(id_hex, P001, matches(input$atividade_min)) %>%
      merge(hex, by = "id_hex", all.x = TRUE) %>% 
      rename(id_hex = 1, P001 = 2, valor = 3, geometry = 4) %>%
      mutate(popup = paste0("<strong>População:</strong> ", P001, "<br><strong>Valor da acessibilidade:</strong> ", round(valor, 0), " minutos")) %>%
      st_sf(crs = 4326)
    
  })
  
  
  # Reactive para o tempo
  tempo_filtrado <- reactive({
    
    if (input$cidade %in% c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec') & input$modo_todos %in% "tp") {
      
      atividade_filtrada() %>% dplyr::select(id_hex, P001, matches(as.character(input$tempo_tp))) %>%
        merge(hex, by = "id_hex", all.x = TRUE) %>%
        rename(id_hex = 1, P001 = 2, valor = 3, geometry = 4) %>%
        # create popup
        mutate(popup = paste0("<strong>População:</strong> ", P001, "<br><strong>Valor da acessibilidade:</strong> ", round(valor, 1), "%")) %>%
        st_sf(crs = 4326)
      
    } else if (input$cidade %in% c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec') & input$modo_todos %in% c("caminhada", "bicicleta")) {
      
      atividade_filtrada() %>% dplyr::select(id_hex, P001, matches(as.character(input$tempo_ativo_tp))) %>%
        merge(hex, by = "id_hex", all.x = TRUE) %>%
        rename(id_hex = 1, P001 = 2, valor = 3, geometry = 4) %>%
        mutate(popup = paste0("<strong>População:</strong> ", P001, "<br><strong>Valor da acessibilidade:</strong> ", round(valor, 1), "%")) %>%
        st_sf(crs = 4326)
      
    } else if(input$cidade %in% c('bsb', 'sal', 'man', 'goi', 'bel', 'gua', 'cam', 'slz', 'sgo', 'mac', 'duq', 'cgr', 'nat')) {
      
      atividade_filtrada() %>% dplyr::select(id_hex, P001, matches(as.character(input$tempo_ativo))) %>%
        merge(hex, by = "id_hex", all.x = TRUE) %>%
        rename(id_hex = 1, P001 = 2, valor = 3, geometry = 4) %>%
        mutate(popup = paste0("<strong>População:</strong> ", P001, "<br><strong>Valor da acessibilidade:</strong> ", round(valor, 1), "%")) %>%
        st_sf(crs = 4326)
      
    }
    
    
  })
  
  
  
  # # Reactive para a cor do mapa
  # colorpal <- reactive({
  #   colorNumeric("inferno", tempo_filtrado()$valor)
  # })
  # 
  # colorpal_min <- reactive({
  #       colorNumeric("inferno", atividade_filtrada_min()$valor)
  # })
  
  
  
  
  # baseMap
  output$map <- renderMapdeck({
    
    # vai <- acess_cum[nome_muni == input$cidade] %>% st_sf(crs = 4326)
    # 
    # bounds <- st_bbox(vai)
    # min_lon <- bounds["xmin"] %>% as.numeric()
    # min_lat <- bounds["ymin"] %>% as.numeric()
    # max_lon <- bounds["xmax"] %>% as.numeric()
    # max_lat <- bounds["ymax"] %>% as.numeric()
    # 
    # # cidade_filtrada() %>%
    #   leaflet(data = vai) %>%
    #     addProviderTiles(provider = providers$CartoDB) %>%
    #     fitBounds(~min_lon, ~min_lat, ~max_lon, ~max_lat)
    
    mapdeck()
  
    
    })
  
  
  centroid_go <- reactive({
    filter(limits, abrev_muni == input$cidade)
  })

  
  
  
  observe({
    
    if(input$cidade %in% c("spo", "man", "cgr", "bsb")) {
      
      zoom1 <- 9
      
    } else if(input$cidade %in% c("mac", "for", "nat", "rec", "sal", "slz", "bho")) {
      
      zoom1 <- 11
    
    } else {zoom1 <- 10}
    
    isolate({
      proxy <- mapdeck_update(map_id = "map") %>%
        mapdeck_view(location = c(centroid_go()$lon, centroid_go()$lat), zoom = zoom1,
                     transition = "fly")
      
    })
    
    
    # linhas_cidade <- linhas %>%
    #   filter(Cidade == input$cidade) %>%
    #   filter(!st_is_empty(.))
    
    # colorpal_linhas <- colorFactor("Accent", linhas_cidade$Modo)
    
    if (input$indicador == "Cumulativo") {
      
      proxy %>%
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
      
      
    } else if (input$indicador == "Oportunidade mais próxima") {
      
      # create viridis scale in the reverse direction
      # create matrix
      colorss <- colourvalues::color_values_rgb(x = 1:256, "viridis")
      # invert matrix
      colorss <- apply(colorss, 2, rev)[, 1:3]
      
      proxy %>%
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
          legend_options = list(title = "Minutos atè a oportunidade mais próxima"),
          legend_format = list( fill_colour = as.integer)
        )
      
    }
      
      
    
    
  })
  
  
  
}
