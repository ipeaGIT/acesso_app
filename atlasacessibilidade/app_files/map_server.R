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

# v_city <- reactiveValues(cidade = NULL)




v_city <- reactive({
  
  req(input$cidade)
  # print(input$cidade)
  if(input$cidade != "") input$cidade else NULL
  
})

# observeEvent({input$cidade},{
#   
#   
#   req(input$cidade)
#   # print(input$cidade)
#   # if(input$cidade != "") {
#   # 
#   #   v_city$city <- input$cidade }
#   # 
#   # else {v_city$city <- NULL}
#   v_city$city <- if(input$cidade != "") input$cidade
#   
#   
#   
#   # print(v_city$city)
#   
# })

# Filter the city

cidade_filtrada <- reactive({
  
  # only run when city value is not NULL
  # req(v_city())
  print(v_city$city)
  print(input$cidade)
  
  # open city and hex here!!!!!!!!!!!!
  readRDS(sprintf("data/new/access_%s.rds", v_city()))
  
  # acess[sigla_muni == v_city$city]
  # print(head(readRDS(sprintf("data/new/access_%s.rds", v_city()))))
  
  
})

hex_filtrado <- reactive({
  
  # only run when city value is not NULL
  req(v_city())
  
  # open city and hex here!!!!!!!!!!!!
  readRDS(sprintf("data/new/hex/hex_%s.rds", v_city()))
  
})

# reactive to filter the year -----------------------------------------------------------------

ano_filtrado <- reactive({
  
  # print(table(cidade_filtrada()$year))
  print(a())
  print(sprintf("Year selected: %s", input$ano))
  
  cidade_filtrada()[year == input$ano]
  
  
})





# 2) REACTIVE TO FILTER THE MODE -----------------------------------------------------------------

# First we use a reactive expression to choose the input

a <- reactive({
  
  req(v_city())
  
  if (v_city() %in% c('for', 'spo', 'cur', 'poa', 'bho', 'cam') & input$ano %in% c(2017, 2018, 2019)) 
    
  {input$modo_todos}
  
  else if(v_city() %in% c('rio') & input$ano %in% c(2018, 2019)) {
    
    input$modo_todos }
  
  else if(v_city() %in% c('rec', 'goi') & input$ano %in% c(2019)) {
    
    
    input$modo_todos }
  
  
  # else if(v_city() %in% c('bsb', 'sal', 'man', 'goi', 'bel', 'gua', 'slz', 'sgo', 'mac', 'duq', 'cgr', 'nat', 'fake')) {
    
    
    else {input$modo_ativo }
  
  # print(input$modo_ativo)
  
})

# Reactive para a modo
modo_filtrado <- reactive({
  
  print(nrow(ano_filtrado()))
  print(sprintf("Mode selected: %s", a()))
  
  ano_filtrado()[mode == a()]
  
})


# 3) REACTIVE TO FILTER THE INDICATOR --------------------------------------------------------------

# # update the options from indicators
# observe({
#   
#   # print(input$indicador)
#   req(input$indicador)
#   
#   list_pop_total <- list('População' = structure(c("PT"),
#                                                  .Names = c(i18n()$t("População Total"))))
#   
#   list_pop_sexo <- list('População por sexo' = structure(c("PH", "PM"),
#                                                          .Names = c(i18n()$t("População Homens"),
#                                                                     i18n()$t("População Mulheres"))))
#   
#   list_pop_cor <- list('População por cor' = structure(c("PB", "PN", "PA", "PI"),
#                                                        .Names = c(i18n()$t("População Branca"),
#                                                                   i18n()$t("População Negra"),
#                                                                   i18n()$t("População Asiática"),
#                                                                   i18n()$t("População Indígena"))))
#   
#   list_pop_idade <- list('População por idade' = structure(c("P0005I", "P0614I", "P1518I", "P1924I",
#                                                              "P2539I", "P4069I", "P70I"),
#                                                            .Names = c(i18n()$t("População 0 a 5 anos"),
#                                                                       i18n()$t("População 6 a 14 anos"),
#                                                                       i18n()$t("População 15 a 18 anos"),
#                                                                       i18n()$t("População 19 a 24 anos"),
#                                                                       i18n()$t("População 25 a 39 anos"),
#                                                                       i18n()$t("População 40 a 69 anos"),
#                                                                       i18n()$t("População 70+ anos"))))
#   
#   if (input$indicador == "CMP") {
#     
#     updatePickerInput(
#       session,
#       inputId = "atividade_cma",
#       # label = "teste",
#       selected = "PT",
#       choices = c(list_pop_total, list_pop_sexo, list_pop_cor, list_pop_idade)
#     )
#     
#   }
#   
#   
#   
#   
# })

indicador_filtrado <- reactive({
  
  print(sprintf("go: %s", input$indicador))
  print(nrow(modo_filtrado()))
  
  cols <- c('id_hex', 'P001', grep(input$indicador, colnames(modo_filtrado()), ignore.case = TRUE, value = TRUE))
  
  modo_filtrado()[, ..cols]
  
  # print(head(modo_filtrado()[, ..cols])) # ok
  
  # modo_filtrado() %>% dplyr::select(id_hex, P001, matches(input$indicador))
  
})



# 4) REACTIVE TO FILTER THE ACTIVITY ---------------------------------------------------------------

indicador_ok <- reactive({
  
  # print(input$indicador)
  
  if (input$indicador == "CMA") {
    
    input$atividade_cma  }
  else if (input$indicador == "CMP"){ 
    
    input$atividade_cmp
  }
  
  
})

# Reactive para a atividade para indicador cumulativo
atividade_filtrada_cma <- reactive({
  
  # print(atividade())
  print(sprintf("Indicador ok: %s", indicador_ok()))
  # print(input$atividade_cma)
  # print(input$atividade_cmp)
  
  cols <- c('id_hex', 'P001', grep(indicador_ok(), colnames(indicador_filtrado()), ignore.case = TRUE, value = TRUE))
  
  indicador_filtrado()[, ..cols]
  
  # print(head(indicador_filtrado()[, ..cols]))
  
})


# Reactive para a atividade para indicador tempo minimo
atividade_filtrada_min <- reactive({
  
  if (input$indicador == "TMI") {
    
    req(input$atividade_min)
    print(sprintf("min %s", input$atividade_min))
    
    cols <- c('id_hex', 'P001', grep(input$atividade_min, colnames(indicador_filtrado()), ignore.case = TRUE, value = TRUE))
    
    indicador_filtrado1 <- indicador_filtrado()[, ..cols]
    colnames(indicador_filtrado1) <- c('id_hex', 'P001', 'valor')
    indicador_filtrado1[, id := 1:nrow(indicador_filtrado1)]
    indicador_filtrado1[, popup := paste0(i18n()$t("<strong>População:</strong> "), P001, i18n()$t("<br><strong>Valor da acessibilidade:</strong> "), round(valor, 0), " ", i18n()$t("minutos"))]
    
  }
  
})


# 5) REACTIVE TO FILTER THE TIME THRESHOLD ---------------------------------------------------------
# This filter is only applied to the cumulative indicator

# Select time threshold
b <- reactive({
  
  req(v_city())
  
  # switch (v_city(),
  #   c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec') & input$modo_todos %in% "public_transport"  = input$tempo_tp,
  #   c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec') & input$modo_todos %in% "public_transport"  = input$tempo_tp,
  #   c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec') & input$modo_todos %in% "public_transport"  = input$tempo_tp,
  #   
  # )
  
  if (a() == "public_transport") input$tempo_tp else if(a() %in% c("walk", "bicycle")) input$tempo_ativo
  
  # if (v_city() %in% c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec') & input$modo_todos %in% "public_transport") {input$tempo_tp}
  # 
  # else if  (v_city() %in% c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec') & input$modo_todos %in% c("walk", "bicycle")) {input$tempo_ativo_tp}
  # 
  # else if (v_city() %in% c('bsb', 'sal', 'man', 'goi', 'bel', 'gua', 'cam', 'slz', 'sgo', 'mac', 'duq', 'cgr', 'nat', 'fake')) {input$tempo_ativo}
  # else {input$tempo_tp}
  
})

# Reactive for time threshold
tempo_filtrado <- reactive({
  
  print(sprintf("b: %s", b()))
  
  cols <- c('id_hex', 'P001', grep(b(), colnames(atividade_filtrada_cma()), ignore.case = TRUE, value = TRUE))
  
  atividade_filtrada1 <- atividade_filtrada_cma()[, ..cols]
  colnames(atividade_filtrada1) <- c('id_hex', 'P001', 'valor')
  atividade_filtrada1[, id := 1:nrow(atividade_filtrada1)]
  atividade_filtrada1[, popup := paste0(i18n()$t("<strong>População:</strong> "), P001, i18n()$t("<br><strong>Valor da acessibilidade:</strong> "), round(valor, 1), "%")]
  
  
})

# 6) TRANSFORM TO SF -------------------------------------------------------------------------------

atividade_filtrada_min_sf <- reactive({
  
  req(atividade_filtrada_min())
  
  atividade_filtrada_min_sf1 <- merge(atividade_filtrada_min(), hex_filtrado(), by = "id_hex", all.x = TRUE, sort = FALSE)
  
  atividade_filtrada_min_sf1 <- st_sf(atividade_filtrada_min_sf1, crs = 4326)
  
})



tempo_filtrado_sf <- reactive({
  
  # print(hex_filtrado())
  
  tempo_filtrado_sf1 <- merge(tempo_filtrado(), hex_filtrado(), by = "id_hex", all.x = TRUE, sort = FALSE)
  
  tempo_filtrado_sf1 <- st_sf(tempo_filtrado_sf1, crs = 4326)
  
  tempo_filtrado_sf1 <- tempo_filtrado_sf1 %>% dplyr::filter(!st_is_empty(.))
  
  # print(tempo_filtrado_sf1)
  
  # tempo_filtrado() %>% setDT() %>%
  #   merge(hex, by = "id_hex", all.x = TRUE, sort = FALSE) %>% 
  #   st_sf(crs = 4326)
  
  
})




# 7) RENDER BRAZIL'S BASEMAP -------------------------------------------------------

output$map <- renderMapdeck({
  
  mapdeck(location = c(-43.95988, -19.902739), zoom = 3)
  
  
})


# Stop the loading page here !
waiter_hide()

# reactive to get city limits
limits_filtrado <- reactive({
  
  # Filter cities limits
  limits_filtrado <- limits[abrev_muni == v_city()] %>% st_sf(crs = 4326)
  
  # print(limits_filtrado)
  
})


centroid_go <- reactive({
  
  centroid_go <- centroids[abrev_muni == v_city()]
  
  print(centroid_go)
})



zoom1 <- reactive ({
  
  # Choose zoom based on city: some cities are bigger than others
  if(v_city() %in% c("spo", "man", "cgr", "bsb")) {
    
    zoom1 <- 9
    
  } else if(v_city() %in% c("mac", "for", "nat", "rec", "sal", "slz", "bho")) {
    
    zoom1 <- 11
    
  } else {zoom1 <- 10}
  
  print(zoom1)
  
})


# 8) OBSERVER TO RENDER THE CITY INDICATOR -------------------------------------------------------
observeEvent({v_city()},{
  
  # create viridis scale in the reverse direction
  # create matrix
  colorss <- colourvalues::color_values_rgb(x = 1:256, "viridis")
  # invert matrix
  colorss <- apply(colorss, 2, rev)[, 1:3]
  # add alpha
  colorss <- cbind(colorss, 170)
  
  # create list with values for mapdeck options
  mapdeck_options <- list(
    'layer_id1'       = ifelse(input$indicador %in% c("CMA", "CMP"), "acess_min_go", "acess_cum_go"),
    'data'            = if    (input$indicador %in% c("CMA", "CMP")) tempo_filtrado_sf() else atividade_filtrada_min_sf(),
    'layer_id2'       = ifelse(input$indicador %in% c("CMA", "CMP"), "acess_cum_go", "acess_min_go"),
    'palette1'        = if (input$indicador %in% c("CMA", "CMP")) "inferno" else colorss,
    'legend_options1' = ifelse(input$indicador %in% c("CMA", "CMP"),
                               "Oportunidades Acessíveis",
                               "Minutos até a oportunidade mais próxima")
  )
  
  # print(head(mapdeck_options$data))
  # print(nrow(mapdeck_options$data))
  # print(class(mapdeck_options$data))
  
  # saveRDS(mapdeck_options$data, "data/new/teste.rds")
  
  # Zoom in on the city when it's choosen
  mapdeck_update(map_id = "map") %>%
    mapdeck_view(location = c(centroid_go()$lon, centroid_go()$lat), zoom = zoom1(),
                 duration = 3000, transition = "fly") %>%
    clear_polygon(layer_id = mapdeck_options$layer_id1) %>%
    clear_legend(layer_id = mapdeck_options$layer_id1) %>%
    # # Render city limits
    # add_polygon(
    #   data = limits_filtrado(),
    #   stroke_colour = "#616A6B",
    #   stroke_width = 100,
    #   fill_opacity = 0,
    #   update_view = FALSE,
    #   focus_layer = FALSE,
    # ) %>%
    # Render city indicator
    add_polygon(
      data = mapdeck_options$data,
      fill_colour = "valor",
      fill_opacity = 170,
      layer_id = mapdeck_options$layer_id2,
      palette = mapdeck_options$palette1,
      update_view = FALSE,
      focus_layer = FALSE,
      # auto_highlight = TRUE,
      tooltip = "popup",
      legend = TRUE,
      legend_options = list(title = i18n()$t(mapdeck_options$legend_options1)),
      legend_format = list( fill_colour = as.integer),
      stroke_width = NULL,
      stroke_colour = NULL,
      stroke_opacity = 0
    )
  
  
  
  
  
  
})


# Observe any change on the atrributes on the city and change the map accordingly
observeEvent({c(input$indicador, 
                input$ano,
                input$modo_todos, input$modo_ativo, 
                input$atividade_cma, input$atividade_cmp, input$atividade_min, 
                input$tempo_tp, input$tempo_ativo_tp, input$tempo_ativo)},{
                  
                  # print(nrow(atividade_filtrada_min_sf))
                  
                  
                  if (input$indicador == "TMI") {
                    
                    # create viridis scale in the reverse direction
                    # create matrix
                    colorss <- colourvalues::color_values_rgb(x = 1:256, "viridis")
                    # invert matrix
                    colorss <- apply(colorss, 2, rev)[, 1:3]
                    # add alpha
                    colorss <- cbind(colorss, 170)
                    
                    mapdeck_update(map_id = "map") %>%
                      clear_polygon(layer_id = "acess_cum_go") %>%
                      clear_legend(layer_id = "acess_cum_go") %>%
                      add_polygon(
                        data = atividade_filtrada_min_sf(),
                        fill_colour = "valor",
                        # fill_opacity = 200,
                        layer_id = "acess_min_go",
                        palette = colorss,
                        update_view = FALSE,
                        tooltip = "popup",
                        legend = TRUE,
                        legend_options = list(title = i18n()$t("Minutos até a oportunidade mais próxima")),
                        legend_format = list( fill_colour = as.integer),
                        stroke_width = 0,
                        stroke_colour = NULL,
                        stroke_opacity = 0
                      )
                    
                  } else 
                    
                    if (input$indicador %in% c("CMA", "CMP")) {
                      
                      mapdeck_update(map_id = "map") %>%
                        clear_polygon(layer_id = "acess_min_go") %>%
                        clear_legend(layer_id = "acess_min_go") %>%
                        add_polygon(
                          data = tempo_filtrado_sf(),
                          fill_colour = "valor",
                          fill_opacity = 170,
                          layer_id = "acess_cum_go",
                          palette = "inferno",
                          update_view = FALSE,
                          focus_layer = FALSE,
                          # auto_highlight = TRUE,
                          tooltip = "popup",
                          legend = TRUE,
                          legend_options = list(title = i18n()$t("Oportunidades Acessíveis")),
                          legend_format = list( fill_colour = as.integer),
                          stroke_width = NULL,
                          stroke_colour = NULL,
                          stroke_opacity = 0
                        )
                    }
                  
                  
                })
