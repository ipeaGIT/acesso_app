# MAP SERVER

# 1) REACTIVE TO FILTER THE CITY -----------------------------------------------------------------

# First we use a reactive expression to choose the input
# We created a 'fake' city to represent the Brazil map

# a_city <- reactive({
#   
#   if(input$cidade != "") {input$cidade} else {"fake"}
#   
#   
# })

v_city <- reactiveValues()

observeEvent({input$cidade},{
  
  if(input$cidade != "") {
    
    v_city$city <- input$cidade }
  
  else {v_city$city <- NULL}
  
  
  
  print(v_city$city)
  
})



# Filter the city

cidade_filtrada <- reactive({
  
  # only run when city value is not NULL
  req(v_city$city)
  
  acess[sigla_muni == v_city$city]
  
})



# 2) REACTIVE TO FILTER THE MODE -----------------------------------------------------------------

# First we use a reactive expression to choose the input

a <- reactive({
  
  if (v_city$city %in% c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec')) {input$modo_todos}
  
  else if(v_city$city %in% c('bsb', 'sal', 'man', 'goi', 'bel', 'gua', 'cam', 'slz', 'sgo', 'mac', 'duq', 'cgr', 'nat', 'fake')) {
    
    input$modo_ativo }
  
})

# Reactive para a modo
modo_filtrado <- reactive({
  
  cidade_filtrada()[modo == a()]
  
})

# 3) REACTIVE TO FILTER THE INDICATOR --------------------------------------------------------------

indicador_filtrado <- reactive({
  
  modo_filtrado() %>% dplyr::select(id_hex, P001, matches(input$indicador))
  
})


# 4) REACTIVE TO FILTER THE ACTIVITY ---------------------------------------------------------------
# Reactive para a atividade para indicador cumulativo
atividade_filtrada <- reactive({
  
  indicador_filtrado() %>% dplyr::select(id_hex, P001, matches(input$atividade_cum))
  
})


# Reactive para a atividade para indicador tempo minimo
atividade_filtrada_min <- reactive({
  
  indicador_filtrado() %>% dplyr::select(id_hex, P001, matches(input$atividade_min)) %>%
    rename(id_hex = 1, P001 = 2, valor = 3) %>%
    mutate(id = 1:n()) %>%
    mutate(popup = paste0(i18n()$t("<strong>População:</strong> "), P001, i18n()$t("<br><strong>Valor da acessibilidade:</strong> "), round(valor, 0), " ", i18n()$t("minutos")))
  
})


# 5) REACTIVE TO FILTER THE TIME THRESHOLD ---------------------------------------------------------
# This filter is only applied to the cumulative indicator

# Select time threshold
b <- reactive({
  
  if (v_city$city %in% c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec') & input$modo_todos %in% "tp") {input$tempo_tp}
  
  else if  (v_city$city %in% c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec') & input$modo_todos %in% c("caminhada", "bicicleta")) {input$tempo_ativo_tp}
  
  else if (v_city$city %in% c('bsb', 'sal', 'man', 'goi', 'bel', 'gua', 'cam', 'slz', 'sgo', 'mac', 'duq', 'cgr', 'nat', 'fake')) {input$tempo_ativo}
  
})

# Reactive for time threshold
tempo_filtrado <- reactive({
  
  atividade_filtrada() %>% dplyr::select(id_hex, P001, matches(as.character(b()))) %>%
    rename(id_hex = 1, P001 = 2, valor = 3) %>%
    mutate(id = 1:n()) %>%
    # Create tiptool message
    mutate(popup = paste0(i18n()$t("<strong>População:</strong> "), P001, i18n()$t("<br><strong>Valor da acessibilidade:</strong> "), round(valor, 1), "%"))
  
})

# 6) TRANSFORM TO SF -------------------------------------------------------------------------------

atividade_filtrada_min_sf <- reactive({
  
  atividade_filtrada_min() %>% setDT() %>%
    merge(hex, by = "id_hex", all.x = TRUE, sort = FALSE) %>% 
    st_sf(crs = 4326)
  
})


tempo_filtrado_sf <- reactive({
  
  tempo_filtrado() %>% setDT() %>%
    merge(hex, by = "id_hex", all.x = TRUE, sort = FALSE) %>% 
    st_sf(crs = 4326)
  
  
})




# 7) RENDER BRAZIL'S BASEMAP -------------------------------------------------------

output$map <- renderMapdeck({
  
  mapdeck(location = c(-43.95988, -19.902739), zoom = 3)
  
  
})


# Stop the loading page here !
waiter_hide()


# 8) OBSERVER TO RENDER THE CITY INDICATOR -------------------------------------------------------
observeEvent({v_city$city},{
  
  
  # Filter cities limits
  limits_filtrado <- filter(limits, abrev_muni == v_city$city)
  
  centroid_go <- filter(centroids, abrev_muni == v_city$city)
  
  
  # Choose zoom based on city: some cities are bigger than others
  if(v_city$city %in% c("spo", "man", "cgr", "bsb")) {
    
    zoom1 <- 9
    
  } else if(v_city$city %in% c("mac", "for", "nat", "rec", "sal", "slz", "bho")) {
    
    zoom1 <- 11
    
  } else {zoom1 <- 10}
  
  
  # Zoom in on the city when it's choosen
  proxy <- mapdeck_update(map_id = "map") %>%
    mapdeck_view(location = c(centroid_go$lon, centroid_go$lat), zoom = zoom1,
                 duration = 3000, transition = "fly")
  
  # Create map with indicators when the city is first selected
  if (input$indicador == "CMA") {
    
    proxy %>%
      clear_polygon(layer_id = "acess_min_go") %>%
      clear_legend(layer_id = "acess_min_go") %>%
      # Render city limits
      add_polygon(
        data = limits_filtrado,
        stroke_colour = "#616A6B",
        stroke_width = 100,
        fill_opacity = 0,
        update_view = FALSE,
        focus_layer = FALSE,
      ) %>%
      # Render city indicator
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
      # Render city limits
      add_polygon(
        data = limits_filtrado,
        stroke_colour = "#616A6B",
        stroke_width = 100,
        fill_opacity = 0,
        update_view = FALSE,
        focus_layer = FALSE
      ) %>%
      # Render city indicator
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
    
  }
  
  
  
  
  
})


# Observe any change on the atrributes on the city and change the map accordingly
observeEvent({c(input$indicador, 
                input$modo_todos, input$modo_ativo, 
                input$atividade_cum, input$atividade_min, 
                input$tempo_tp, input$tempo_ativo_tp, input$tempo_ativo)},{
                  
                  
                  
                  
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
                    }
                  
                  
                })

