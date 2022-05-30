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

# waiter_show(html = tagList(spin_loaders(id = 2, color = "black"), br(), HTML("&nbsp;"), br(),
#                            span("Opening GTFS...", style = "color: black"),
#                            br(),  br(), HTML("&nbsp;")),
#             color = "rgba(233, 235, 240, .5)")

cidade_filtrada <- reactive({
  
  # only run when city value is not NULL
  # req(v_city())
  print(v_city())
  # print(input$cidade)
  
  # open city and hex here!!!!!!!!!!!!
  readRDS(sprintf("data/new/access/access_%s.rds", v_city()))
  
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
  # print(sprintf("a: %s", a()))
  # print(sprintf("Year selected: %s", input$ano))
  # print(sprintf("US: %s", input$demo_ou_us))
  
  cidade_filtrada()[year == input$ano]
  
  # print(nrow(cidade_filtrada()[year == input$ano]))
  
})



# here we should create the observer for the landuse indicator --------------------------------
us_filtrado <- reactive({
  
  print(sprintf("Us deu certo? %s", input$indicador_us))
  
  if (input$indicador_us == "us") {
    
    # open city and hex here!!!!!!!!!!!!
    readRDS(sprintf("data/new/landuse/landuse_%s.rds", v_city()))
    
  } 
  
  
})

us_filtrado_type <- reactive({
  
  req(us_filtrado())
  # print(sprintf("aaiaiai %s", input$demo_ou_us))
  
  
  if (input$demo_ou_us == "demo") {
    
    # get pop variables
    pop <- colnames(us_filtrado())[startsWith(colnames(us_filtrado()), c("P"))]
    # print(pop)
    # get renda variables
    renda <- colnames(us_filtrado())[startsWith(colnames(us_filtrado()), c("R"))]
    # print(renda)
    
    cols <- c('id_hex', 'year', pop, renda)
    # print(cols)
    us_filtrado()[, ..cols]
    
    
  } else if (input$demo_ou_us == "activity") {
    
    # get us variables
    us1 <- colnames(us_filtrado())[startsWith(colnames(us_filtrado()), c("T"))]
    us2 <- colnames(us_filtrado())[startsWith(colnames(us_filtrado()), c("E"))]
    us3 <- colnames(us_filtrado())[startsWith(colnames(us_filtrado()), c("M"))]
    us4 <- colnames(us_filtrado())[startsWith(colnames(us_filtrado()), c("S"))]
    us5 <- colnames(us_filtrado())[startsWith(colnames(us_filtrado()), c("C"))]
    
    
    cols <- c('id_hex', 'year', us1, us2, us3, us4, us5)
    us_filtrado()[, ..cols]
    
  }
  
})

us_filtrado_ano <- reactive({
  
  # nrow(us_filtrado_type()[year == input$ano_us])
  us_filtrado_type()[year == input$ano_us]
  
  
})

# para selecionar o input de uso do solo correto
indicador_us_ok <- reactive({
  
  # print(input$indicador)
  
  if (input$demo_ou_us == "demo") {
    
    input$atividade_demo
    
  } else if (input$demo_ou_us == "activity"){
    
    input$atividade_us
  }
  
  
})

# observeEvent(input$demo_ou_us, {
#   
#   if (input$demo_ou_us == "demo") {
#     
#     vars <- c("gua", "gua")
#     
#   } else if (input$demo_ou_us == "activity") {
#     
#     vars <- c("gu", "gu")
#     
#   }
# 
# updatePickerInput(session = session,
#                   inputId = "atividade_demo",
#                   label = "Teste",
#                   choices = vars)
#   
# })

# filter final indicator for us

us_filtrado_ano_atividade <- reactive({
  
  
  # print(nrow(us_filtrado_ano()))
  # print(colnames(us_filtrado_ano()))
  print(sprintf("Indicador us ok: %s", indicador_us_ok()))
  # print(colnames(us_filtrado_ano()))
  cols <- c("id_hex", indicador_us_ok())
  # print(cols)
  a <- us_filtrado_ano()[, ..cols]
  colnames(a) <- c('id_hex', 'valor')
  # print(head(a))
  a[, id := 1:nrow(a)]
  return(a)
  # atividade_filtrada1[, popup := paste0(i18n()$t("<strong>População:</strong> "), P001, i18n()$t("<br><strong>Valor da acessibilidade:</strong> "), round(valor, 1), "%")]
  
  
  # print(head(a))
  # return(a)
  
  
})

us_filtrado_ano_atividade_sf <- reactive({
  
  
  
  data.table::setkeyv(us_filtrado_ano_atividade(), c('id_hex'))
  a <- us_filtrado_ano_atividade()[hex_filtrado(), on = 'id_hex', geom := i.geom]
  
  # to sf
  a <- st_sf(a, crs = 4326)
  return(a)
  
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
    
    
    input$modo_todos
    
    
    # else if(v_city() %in% c('bsb', 'sal', 'man', 'goi', 'bel', 'gua', 'slz', 'sgo', 'mac', 'duq', 'cgr', 'nat', 'fake')) {
    
    
  } else input$modo_ativo 
  
  # print(sprintf("teste1 %s", input$modo_todos))
  # print(sprintf("teste2 %s", input$modo_ativo))
  
})

output$tp <- reactive({
  a() %in% c("public_transport", "car")
})
outputOptions(output, 'tp', suspendWhenHidden = FALSE)



# Reactive para a modo
modo_filtrado <- reactive({
  
  print(sprintf("ano filtrado nrow: %s", nrow(ano_filtrado())))
  print(sprintf("Mode selected: %s", a()))
  
  ano_filtrado()[mode == a()]
  
})


# 3) REACTIVE TO FILTER THE INDICATOR --------------------------------------------------------------


indicador_filtrado <- reactive({
  
  print(sprintf("go: %s", input$indicador))
  print(sprintf("modo filtrado nrow: %s", nrow(modo_filtrado())))
  
  cols <- c('id_hex', 'P001', grep(input$indicador, colnames(modo_filtrado()), ignore.case = TRUE, value = TRUE))
  
  modo_filtrado()[, ..cols]
  
  # print(head(modo_filtrado()[, ..cols])) # ok
  
})



# 4) REACTIVE TO FILTER THE ACTIVITY ---------------------------------------------------------------

indicador_ok <- reactive({
  
  # print(input$indicador)
  
  if (input$indicador == "CMA") {
    
    input$atividade_cma  
  } else if (input$indicador == "CMP"){ 
    
    input$atividade_cmp
  }
  
  
})

# Reactive para a atividade para indicador cumulativo
atividade_filtrada_cma <- reactive({
  
  # print(atividade())
  print(sprintf("Indicador ok: %s", indicador_ok()))
  # print(input$atividade_cma)
  # print(input$atividade_cmp)
  # print(colnames(indicador_filtrado()))
  
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
  
  if (a() %in% c("public_transport", "car")) input$tempo_tp else if(a() %in% c("walk", "bicycle")) input$tempo_ativo
  
})

# Reactive for time threshold
tempo_filtrado <- reactive({
  
  # print(sprintf("b: %s", b()))
  # print(colnames(atividade_filtrada_cma()))
  
  cols <- c('id_hex', 'P001', grep(b(), colnames(atividade_filtrada_cma()), ignore.case = TRUE, value = TRUE))
  
  
  atividade_filtrada1 <- atividade_filtrada_cma()[, ..cols]
  colnames(atividade_filtrada1) <- c('id_hex', 'P001', 'valor')
  atividade_filtrada1[, id := 1:nrow(atividade_filtrada1)]
  atividade_filtrada1[, popup := paste0(i18n()$t("<strong>População:</strong> "), P001, i18n()$t("<br><strong>Valor da acessibilidade:</strong> "), 
                                        scales::comma(as.integer(valor), big.mark = " "))]
  
  # print(head(atividade_filtrada1))
  
})

# 6) TRANSFORM TO SF -------------------------------------------------------------------------------

atividade_filtrada_min_sf <- reactive({
  
  req(atividade_filtrada_min())
  
  data.table::setkeyv(atividade_filtrada_min(), c('id_hex'))
  atividade_filtrada_min_sf1 <- atividade_filtrada_min()[hex_filtrado(), on = 'id_hex', geom := i.geom]
  
  # to sf
  atividade_filtrada_min_sf1 <- st_sf(atividade_filtrada_min_sf1, crs = 4326)
  
})



tempo_filtrado_sf <- reactive({
  
  # print(hex_filtrado())
  
  
  # merge
  data.table::setkeyv(tempo_filtrado(), c('id_hex'))
  tempo_filtrado_sf1 <- tempo_filtrado()[hex_filtrado(), on = 'id_hex', geom := i.geom]
  
  # to sf
  tempo_filtrado_sf1 <- st_sf(tempo_filtrado_sf1, crs = 4326)
  
  
  
})




# 7) RENDER BRAZIL'S BASEMAP -------------------------------------------------------

output$map <- renderMapdeck({
  
  mapdeck(location = c(-43.95988, -19.902739), 
          zoom = 3,
          style = mapdeck_style("streets"))
  
  
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
  
  # print(centroid_go)
})



zoom1 <- reactive ({
  
  # Choose zoom based on city: some cities are bigger than others
  if(v_city() %in% c("spo", "man", "cgr", "bsb")) {
    
    zoom1 <- 9
    
  } else if(v_city() %in% c("mac", "for", "nat", "rec", "sal", "slz", "bho")) {
    
    zoom1 <- 11
    
  } else {zoom1 <- 10}
  
  # print(sprintf("zoom: %s", zoom1))
  
})


mapdeck_id_clear <- reactiveVal("us_initial")

# 8) OBSERVER TO RENDER THE CITY INDICATOR -------------------------------------------------------
observeEvent({v_city()},{
  
  mapdeck_id <- ifelse(input$indicador_us == "access", "access_initial", "us_initial")
  # mapdeck_id_clear(ifelse(input$indicador_us == "access", "us_initial", "access_initial"))
  
  print(sprintf("Mapdeck id: %s", mapdeck_id))
  print(sprintf("Mapdeck id clear: %s", mapdeck_id_clear()))
  
  
  waiter_show(html = tagList(spin_loaders(id = 2, color = "black")),
              color = "rgba(233, 235, 240, .1)")
  
  # create viridis scale in the reverse direction
  # create matrix
  colorss <- colourvalues::color_values_rgb(x = 1:256, "viridis")
  # invert matrix
  colorss <- apply(colorss, 2, rev)[, 1:3]
  # add alpha
  colorss <- cbind(colorss, 170)
  
  # select variables
  data <- if(input$indicador_us == "access" & input$indicador %in% c("CMA", "CMP")) {
    tempo_filtrado_sf()
  } else if(input$indicador_us == "access" & input$indicador %in% c("TMI")) {
    atividade_filtrada_min_sf()
  } else if (input$indicador_us == "us") {
    us_filtrado_ano_atividade_sf()
  }
  
  legend <- if(input$indicador_us == "access" & input$indicador %in% c("CMA", "CMP")) {
    "Oportunidades Acessíveis"
  } else if(input$indicador_us == "access" & input$indicador %in% c("TMI")) {
    "Minutos até a oportunidade mais próxima"
  } else if (input$indicador_us == "us") {
    "Quantidade"
  }
  
  # create list with values for mapdeck options
  mapdeck_options <- list(
    # 'layer_id1'       = ifelse(input$indicador %in% c("CMA", "CMP"), "acess_min_go", "acess_cum_go"),
    # 'data'            = if(input$indicador %in% c("CMA", "CMP")) tempo_filtrado_sf() else if (input$indicador %in% c("TMI")) ,
    # 'layer_id2'       = ifelse(input$indicador %in% c("CMA", "CMP"), "acess_cum_go", "acess_min_go"),
    'palette1'        = if (input$indicador %in% c("CMA", "CMP")) "inferno" else colorss,
    'legend_options1' = ifelse(input$indicador %in% c("CMA", "CMP"),
                               "Oportunidades Acessíveis",
                               "Minutos até a oportunidade mais próxima")
  )
  
  legend_converter_cma <- function(x) {
    return( scales::comma(as.integer(x), big.mark = " ", accuracy = 100) )
  }
  
  legend_converter <- if (input$indicador_us == "access" & input$indicador %in% c("TMI")) {
    
    as.integer
    
  } else legend_converter_cma
  
  # Zoom in on the city when it's choosen
  mapdeck_update(map_id = "map") %>%
    mapdeck_view(location = c(centroid_go()$lon, centroid_go()$lat), zoom = zoom1(),
                 duration = 4000, transition = "fly") %>%
    clear_polygon(layer_id = mapdeck_id_clear()) %>%
    clear_legend(layer_id = mapdeck_id_clear()) %>%
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
      data = data,
      fill_colour = "valor",
      fill_opacity = 170,
      layer_id = mapdeck_id,
      # layer_id = mapdeck_options$layer_id2,
      palette = mapdeck_options$palette1,
      update_view = FALSE,
      focus_layer = FALSE,
      # auto_highlight = TRUE,
      tooltip = "popup",
      legend = TRUE,
      legend_options = list(title = i18n()$t(legend)),
      legend_format = list( fill_colour = legend_converter),
      stroke_width = NULL,
      stroke_colour = NULL,
      stroke_opacity = 0
    )
  
  mapdeck_id_clear(mapdeck_id)
  
  waiter_hide()
  
})


# Observe any change on the atrributes on the city and change the map accordingly
observeEvent({c(input$indicador_us,
                input$indicador, 
                input$ano,
                input$modo_todos, input$modo_ativo, 
                input$atividade_cma, input$atividade_cmp, input$atividade_min, 
                input$tempo_tp, input$tempo_ativo)},{
                  
                  # print(nrow(atividade_filtrada_min_sf))
                  
                  legend_converter_cma <- function(x) {
                    return( scales::comma(as.integer(x), big.mark = " ", accuracy = 100) )
                  }
                  
                  legend_converter <- if (input$indicador_us == "access" & input$indicador %in% c("TMI")) {
                    
                    as.integer
                    
                  } else legend_converter_cma
                  
                  
                  if (input$indicador_us == "access") {
                    
                    mapdeck_id <- "access_update"
                    print(sprintf("Mapdeck id clear2: %s", mapdeck_id_clear()))
                    
                    if (input$indicador == "TMI") {
                      
                      # create viridis scale in the reverse direction
                      # create matrix
                      colorss <- colourvalues::color_values_rgb(x = 1:256, "viridis")
                      # invert matrix
                      colorss <- apply(colorss, 2, rev)[, 1:3]
                      # add alpha
                      colorss <- cbind(colorss, 170)
                      
                      mapdeck_update(map_id = "map") %>%
                        clear_polygon(layer_id = mapdeck_id_clear()) %>%
                        clear_legend(layer_id = mapdeck_id_clear()) %>%
                        add_polygon(
                          data = atividade_filtrada_min_sf(),
                          fill_colour = "valor",
                          # fill_opacity = 200,
                          layer_id = mapdeck_id,
                          palette = colorss,
                          update_view = FALSE,
                          tooltip = "popup",
                          legend = TRUE,
                          legend_options = list(title = i18n()$t("Minutos até a oportunidade mais próxima")),
                          legend_format = list( fill_colour = legend_converter),
                          stroke_width = 0,
                          stroke_colour = NULL,
                          stroke_opacity = 0
                        )
                      
                    } else 
                      
                      if (input$indicador %in% c("CMA", "CMP")) {
                        
                        mapdeck_update(map_id = "map") %>%
                          clear_polygon(layer_id = mapdeck_id_clear()) %>%
                          clear_legend(layer_id = mapdeck_id_clear()) %>%
                          add_polygon(
                            data = tempo_filtrado_sf(),
                            fill_colour = "valor",
                            fill_opacity = 170,
                            layer_id = mapdeck_id,
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
                    
                    mapdeck_id_clear(mapdeck_id)
                    
                  }
                  
                  
                })

# Observe any change on the atrributes on the city and change the map accordingly
# only for land use
observeEvent({c(input$indicador_us, 
                input$ano_us,
                input$demo_ou_us,
                input$atividade_demo, input$atividade_us)},{
                  
                  # print(nrow(atividade_filtrada_min_sf))
                  
                  legend_converter <- function(x) {
                    return( scales::comma(as.integer(x), big.mark = " ", accuracy = 10) )
                  }
                  
                  if (input$indicador_us == "us") {
                    
                    # print(sprintf("Mapdeck id clear1: %s", mapdeck_id_clear()))
                    
                    
                    
                    mapdeck_id <- "us_update"
                    # mapdeck_id_clear <- ifelse(input$indicador_us == "access", "us_initial", "access_initial")
                    
                    # print(sprintf("Mapdeck id: %s", mapdeck_id))
                    
                    
                    # create viridis scale in the reverse direction
                    # create matrix
                    colorss <- colourvalues::color_values_rgb(x = 1:256, "viridis")
                    # invert matrix
                    colorss <- apply(colorss, 2, rev)[, 1:3]
                    # add alpha
                    colorss <- cbind(colorss, 170)
                    
                    mapdeck_update(map_id = "map") %>%
                      clear_polygon(layer_id = mapdeck_id_clear()) %>%
                      clear_legend(layer_id = mapdeck_id_clear()) %>%
                      add_polygon(
                        data = us_filtrado_ano_atividade_sf(),
                        fill_colour = "valor",
                        fill_opacity = 170,
                        layer_id = mapdeck_id,
                        palette = "inferno",
                        update_view = FALSE,
                        focus_layer = FALSE,
                        # tooltip = "popup",
                        legend = TRUE,
                        legend_options = list(title = i18n()$t("Quantidade")),
                        legend_format = list( fill_colour = legend_converter),
                        stroke_width = NULL,
                        stroke_colour = NULL,
                        stroke_opacity = 0
                      )
                    
                    mapdeck_id_clear(mapdeck_id)
                    
                    
                  }
                  
                  
                })
