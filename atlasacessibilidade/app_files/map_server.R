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

v_city <- reactiveValues(cidade = NULL)

observeEvent(c(input$cidade), {
  
  print(input$cidade)
  
  v_city$cidade <- if(isTruthy(input$cidade) & input$cidade != "") input$cidade else NULL
  # print(v_city$cidade)
  
})

# v_city <- reactive({
#   
#   req(input$cidade)
#   # print(input$cidade)
#   if(input$cidade != "") input$cidade else NULL
#   
# })



# observer to update the city if the circle from the city is clicked
observeEvent(c(input$map_pointcloud_click), {
  
  req(input$map_pointcloud_click)
  
  js <- input$map_pointcloud_click
  lst <- jsonlite::fromJSON( js )
  row <- (lst$index) + 1
  
  # print(row)
  centroids_filter <- centroids[row,]
  centroids_filter <- centroids_filter$abrev_muni
  # print(centroids_filter)
  v_city$cidade <- centroids_filter
  
  
  # update the city input picker
  updatePickerInput(session = session, inputId = "cidade",
                    selected = v_city$cidade)
  
  
})



output$city <- reactive({
  v_city$cidade
  # !is.null(v_city$cidade)
})
outputOptions(output, 'city', suspendWhenHidden = FALSE)



rv <- reactiveValues(prev_bins = NULL)

# observer to change the labels of each year
observeEvent(v_city$cidade, {
  
  
  if (v_city$cidade %in% c("for", "spo", "cam", "bho", "poa", "cur")) {
    
    choices_new <- list(HTML("2017 &nbsp;<i class=\"fas fa-bus\"></i>"),
                        HTML("2018 &nbsp;<i class=\"fas fa-bus\"></i>"),
                        HTML("2019 &nbsp;<i class=\"fas fa-bus\"></i>"))
    
  } else if(v_city$cidade %in% c("rio")) {
    
    choices_new <- list(HTML("2017"),
                        HTML("2018 &nbsp;<i class=\"fas fa-bus\"></i>") ,
                        HTML("2019 &nbsp;<i class=\"fas fa-bus\"></i>") )
    
  } else if(v_city$cidade %in% c("rec", "goi")) {
    
    
    choices_new <- list(HTML("2017"),
                        HTML("2018") ,
                        HTML("2019 &nbsp;<i class=\"fas fa-bus\"></i>"))
    
  } else choices_new <- list("2017", "2018", "2019")
  
  
  
  updateRadioButtons(session = session,
                     inputId = "ano",
                     choiceValues = c("2017", "2018", "2019"),
                     choiceNames = choices_new)
  
  
})

# first, identify previous city

observeEvent(c(v_city$cidade), {
  
  
  # rv$prev_bins <- c(rv$prev_bins, v_city$cidade)
  rv$prev_bins <- c(tail(rv$prev_bins, 1), v_city$cidade)
  
  # print("rv$prev_bins")
  # print(v_city$cidade == rv$prev_bins)
  # print(all(v_city$cidade == rv$prev_bins))
  
  
})

observeEvent(c(input$modo_ativo, input$ano), {
  
  print("GAROTO")
  print(a())
  
  updateRadioGroupButtons(
    session = session,
    inputId = "modo_todos",
    selected = a()
  )
  
})

observeEvent(c(input$modo_ativo), {
  
  # print("GAROTO")
  # 
  # updateRadioGroupButtons(
  #   session = session,
  #   inputId = "modo_todos",
  #   selected = "walk"
  # )
  
})


cidade_filtrada <- reactive({
  
  # only run when city value is not NULL
  req(v_city$cidade)
  # print(v_city$cidade)
  # print(input$cidade)
  
  # open city and hex here!!!!!!!!!!!!
  readRDS(sprintf("data/new/access/access_%s.rds", v_city$cidade))
  
  # acess[sigla_muni == v_city$city]
  # print(head(readRDS(sprintf("data/new/access_%s.rds", v_city$cidade))))
  
  
})

hex_filtrado <- reactive({
  
  # only run when city value is not NULL
  req(v_city$cidade)
  
  # open city and hex here!!!!!!!!!!!!
  readRDS(sprintf("data/new/hex/hex_%s.rds", v_city$cidade))
  
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
  
  # print(sprintf("Us deu certo? %s", input$indicador_us))
  
  if (input$indicador_us == "us") {
    
    # open city and hex here!!!!!!!!!!!!
    readRDS(sprintf("data/new/landuse/landuse_%s.rds", v_city$cidade))
    
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


# get the year of the demo or us
indicador_year_us_ok <- reactive({
  
  # print(input$indicador)
  
  if (input$demo_ou_us == "demo") {
    
    input$ano_demo
    
  } else if (input$demo_ou_us == "activity"){
    
    input$ano_us
  }
  
  
})


us_filtrado_ano <- reactive({
  
  # nrow(us_filtrado_type()[year == input$ano_us])
  us_filtrado_type()[year == indicador_year_us_ok()]
  
  
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
  # print(sprintf("Indicador us ok: %s", indicador_us_ok()))
  # print(colnames(us_filtrado_ano()))
  cols <- c("id_hex", indicador_us_ok())
  # print(cols)
  a <- us_filtrado_ano()[, ..cols]
  colnames(a) <- c('id_hex', 'valor')
  # print(head(a))
  a[, id := 1:nrow(a)]
  # identifica indicador
  a[, indicador := indicador_us_ok()]
  
  
  # make tooltip
  unity <- fcase(
    startsWith(indicador_us_ok(), "P"),    i18n()$t(" pessoas"), 
    startsWith(indicador_us_ok(), "R001"), i18n()$t(" R$"), 
    startsWith(indicador_us_ok(), "R002"), i18n()$t(" Quintil"), 
    startsWith(indicador_us_ok(), "R003"), i18n()$t(" Decil"), 
    startsWith(indicador_us_ok(), "T"),    i18n()$t(" empregos"), 
    startsWith(indicador_us_ok(), "E"),    i18n()$t(" equipamentos de educação"), 
    startsWith(indicador_us_ok(), "S"),    i18n()$t(" equipamentos de saúde"), 
    startsWith(indicador_us_ok(), "C"),    i18n()$t(" cras")
  )
  
  a[, popup := 
      fifelse(startsWith(indicador, "R00"),
              sprintf("<strong>%s:</strong> %s %s", i18n()$t("Valor"), unity, scales::comma(as.integer(valor), big.mark = " ")),
              sprintf("<strong>%s:</strong> %s %s", i18n()$t("Valor"), scales::comma(as.integer(valor), big.mark = " "), unity)
      )]
  
  
  # print(head(a))
  # print(valor)
  # return(a)
  
  return(a)
  
})

us_filtrado_ano_atividade_sf <- reactive({
  
  
  
  data.table::setkeyv(us_filtrado_ano_atividade(), c('id_hex'))
  a <- us_filtrado_ano_atividade()[hex_filtrado(), on = 'id_hex', geom := i.geom]
  
  # to sf
  a <- st_sf(a, crs = 4326)
  return(a)
  
})


# 2) REACTIVE TO FILTER THE MODE -----------------------------------------------------------------



modo_cidade <- reactiveValues(teste = NULL)


a <- reactive({
  
  
  # !all(v_city$cidade == rv$prev_bins)
  
  req(v_city$cidade)
  
  
  if (v_city$cidade %in% c('for', 'spo', 'cur', 'poa', 'bho', 'cam') & input$ano %in% c(2017, 2018, 2019))  {
    
    
    return(input$modo_todos)
    
  } else if(v_city$cidade %in% c('rio') & input$ano %in% c(2018, 2019)) {
    
    return(input$modo_todos)
    
  } else if(v_city$cidade %in% c('rec', 'goi') & input$ano %in% c(2019)) {
    
    return(input$modo_todos)
    
  } else {
    
    return(input$modo_ativo)
  }
  
  
  
  
})


output$tp <- reactive({
  a() %in% c("public_transport", "car")
})
outputOptions(output, 'tp', suspendWhenHidden = FALSE)



# Reactive para a modo
modo_filtrado <- reactive({
  
  
  
  # print(sprintf("ano filtrado nrow: %s", nrow(ano_filtrado())))
  # print(sprintf("Mode selected: %s", a()))
  
  ano_filtrado()[mode == a()]
  
})


# 3) REACTIVE TO FILTER THE INDICATOR --------------------------------------------------------------


indicador_filtrado <- reactive({
  
  # print(sprintf("go: %s", input$indicador))
  # print(sprintf("modo filtrado nrow: %s", nrow(modo_filtrado())))
  
  cols <- c('id_hex', 'P001', grep(input$indicador, colnames(modo_filtrado()), ignore.case = TRUE, value = TRUE))
  
  modo_filtrado()[, ..cols]
  
  # print(head(modo_filtrado()[, ..cols])) # ok
  
})



# 4) REACTIVE TO FILTER THE ACTIVITY ---------------------------------------------------------------

indicador_ok <- reactive({
  
  # print(input$indicador)
  
  if (input$indicador %in% c("CMA")) {
    
    input$atividade_cma  
    
  } else if (input$indicador == "CMP"){ 
    
    input$atividade_cmp
    
    
  } else if (input$indicador == "TMI") {
    
    input$atividade_min
  }
  
  
})

# Reactive para a atividade para indicador cumulativo
atividade_filtrada_cma <- reactive({
  
  req(input$indicador %in% c("CMA", "CMP"))
  
  # print(input$atividade_cma)
  # print(input$atividade_cmp)
  # print(colnames(indicador_filtrado()))
  
  cols <- c('id_hex', 'P001', grep(indicador_ok(), colnames(indicador_filtrado()), ignore.case = TRUE, value = TRUE))
  
  indicador_filtrado()[, ..cols]
  
  # print(head(indicador_filtrado()[, ..cols]))
  
})

ind <- reactiveValues(ind = NULL)


# Reactive para a atividade para indicador tempo minimo
atividade_filtrada_min <- reactive({
  
  if (input$indicador == "TMI") {
    
    # print("Indicador ok")
    # print(indicador_ok())
    
    # req(input$atividade_min)
    
    cols <- c('id_hex', 'P001', grep(input$atividade_min, colnames(indicador_filtrado()), ignore.case = TRUE, value = TRUE))
    
    indicador_filtrado1 <- indicador_filtrado()[, ..cols]
    
    # guardar nome do indicador
    ind$ind <- cols[3] 
    
    colnames(indicador_filtrado1) <- c('id_hex', 'P001', 'valor')
    indicador_filtrado1[, id := 1:nrow(indicador_filtrado1)]
    indicador_filtrado1[, popup := paste0(i18n()$t("<strong>População:</strong> "), P001, i18n()$t("<br><strong>Valor da acessibilidade:</strong> "), round(valor, 0), " ", i18n()$t("minutos"))]
    
    
    return(indicador_filtrado1)
    
  }
  
})


# 5) REACTIVE TO FILTER THE TIME THRESHOLD ---------------------------------------------------------
# This filter is only applied to the cumulative indicator

# Select time threshold
b <- reactive({
  
  req(v_city$cidade)
  
  # switch (v_city$cidade,
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
  
  req(atividade_filtrada_cma())
  
  cols <- c('id_hex', 'P001', grep(b(), colnames(atividade_filtrada_cma()), ignore.case = TRUE, value = TRUE))
  
  
  atividade_filtrada1 <- atividade_filtrada_cma()[, ..cols]
  
  # guardar nome do indicador
  ind$ind <- cols[3] 
  # print(ind$ind)
  
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
  
  
  # print("BORAAAA")
  # print(head(atividade_filtrada_min_sf1))
  
  return(atividade_filtrada_min_sf1)
  
})



tempo_filtrado_sf <- reactive({
  
  # print(hex_filtrado())
  
  
  # merge
  data.table::setkeyv(tempo_filtrado(), c('id_hex'))
  tempo_filtrado_sf1 <- tempo_filtrado()[hex_filtrado(), on = 'id_hex', geom := i.geom]
  
  # to sf
  tempo_filtrado_sf1 <- st_sf(tempo_filtrado_sf1, crs = 4326)
  
  
  
})

# filter the scale limits of each indicator

scale_limits <- reactive({
  
  # print(head(access_limits))
  # filter indicator
  access_extremes1 <- access_limits[abbrev_muni == v_city$cidade & mode == a()]
  cols <- c("abbrev_muni", "mode", grep(ind$ind, colnames(access_extremes1), ignore.case = TRUE, value = TRUE))
  access_extremes1 <- access_extremes1[,..cols]
  colnames(access_extremes1) <- c("abbrev_muni", "mode", "min", "max")
  # print(head(access_extremes1))
  return(access_extremes1)
  
  
})




# 7) RENDER BRAZIL'S BASEMAP -------------------------------------------------------

output$map <- renderMapdeck({
  
  mapdeck(location = c(-43.95988, -19.902739), 
          zoom = 3,
          style = "mapbox://styles/kauebraga/cl3vtf5ay005v14pkzouvp0yk"
  ) %>%
    add_pointcloud(data = centroids,
                   lon = "lon", lat = "lat",
                   update_view = FALSE,
                   layer_id = "brasil",
                   # fill_colour = "blue",
                   fill_opacity = 170,
                   # auto_highlight = TRUE
                   # id = "brasil",
                   tooltip = "name_muni"
    )
  
  
})




# observeEvent(c(input$map_pointcloud_click), {
#   
#   # req(input$map_arc_click)
#   
#   js <- input$map_pointcloud_click
#   lst <- jsonlite::fromJSON( js )
#   row <- (lst$index) + 1
#   
#   print(row)
#   
# })


# Stop the loading page here !
waiter_hide()

# reactive to get city limits
limits_filtrado <- reactive({
  
  # Filter cities limits
  limits_filtrado <- limits[abrev_muni == v_city$cidade] %>% st_sf(crs = 4326)
  
  # print(limits_filtrado)
  
})


centroid_go <- reactive({
  
  centroid_go <- centroids[abrev_muni == v_city$cidade]
  
  # print(centroid_go)
})



zoom1 <- reactive ({
  
  # Choose zoom based on city: some cities are bigger than others
  if(v_city$cidade %in% c("spo", "man", "cgr", "bsb")) {
    
    zoom1 <- 9
    
  } else if(v_city$cidade %in% c("mac", "for", "nat", "rec", "sal", "slz", "bho")) {
    
    zoom1 <- 11
    
  } else {zoom1 <- 10}
  
  # print(sprintf("zoom: %s", zoom1))
  
})


mapdeck_id_clear <- reactiveVal("us_initial")

# 8) OBSERVER TO RENDER THE CITY INDICATOR -------------------------------------------------------
observeEvent({v_city$cidade},{
  
  mapdeck_id <- ifelse(input$indicador_us == "access", "access_initial", "us_initial")
  
  # print(sprintf("Mapdeck id: %s", mapdeck_id))
  # print(sprintf("Mapdeck id clear: %s", mapdeck_id_clear()))
  
  
  waiter_show(html = tagList(spin_loaders(id = 2, color = "black")),
              color = "rgba(233, 235, 240, .4)")
  
  if (input$indicador_us == "access") {
    
    
    
    # select variables
    data <- if(input$indicador_us == "access" & input$indicador %in% c("CMA", "CMP")) {
      tempo_filtrado_sf()
    } else if(input$indicador_us == "access" & input$indicador %in% c("TMI")) {
      atividade_filtrada_min_sf()
    } else if (input$indicador_us == "us") {
      us_filtrado_ano_atividade_sf()
    }
    
    
    # ordenador data
    data <- data %>% dplyr::arrange(valor)
    
    # print(scale_limits()$max)
    
    # legend_converter <- if (input$indicador_us == "access" & input$indicador %in% c("CMA") &
    #                         input$atividade_cma %in% c("TT", "TB", "TM", "TA")) {
    #   
    #   function(x) scales::comma(as.integer(x), big.mark = " ", accuracy = 100)
    # 
    #   } else if (input$indicador_us == "access" & input$indicador %in% c("CMP")) {
    #   
    #   function(x) scales::comma(as.integer(x), big.mark = " ", accuracy = 100)
    #   
    #     } else if (input$indicador_us == "us" & input$indicador %in% c("TMI")) {
    #       
    #   function(x) scales::comma(as.integer(x), big.mark = " ", accuracy = 100)
    #       
    #       
    #     } else if (input$indicador_us == "access" & input$indicador %in% c("TMI")) {
    #   
    #   function(x) as.integer(x) 
    # 
    #       } else function(x) as.integer(x) 
    
    legend_converter <- function (x) as.integer(x)
    
    
    legend <- if(input$indicador_us == "access" & input$indicador %in% c("CMA", "CMP")) {
      i18n()$t("Oportunidades Acessíveis")
    } else if(input$indicador_us == "access" & input$indicador %in% c("TMI")) {
      i18n()$t("Minutos até a oportunidade mais próxima")
    } else if (input$indicador_us == "us") {
      i18n()$t("Quantidade")
    }
    
    
    # print("DATA")
    # print(head(c(data$valor)))
    # print(head(c(scale_limits()$max)))
    
    palette <- fcase(input$indicador == "CMA", "inferno",
                     input$indicador == "CMP", "viridis",
                     input$indicador == "TMI", "viridis")
    
    fill_values <- c(data$valor, scale_limits()$max)
    if (input$indicador == "TMI") fill_values <- -fill_values else fill_values <- fill_values
    
    
    fill_color <- colourvalues::colour_values(
      # x = c(data$valor, 300000),
      x = fill_values,
      alpha = 200,
      palette = palette
    )
    
    # delete the first
    fill_color <- fill_color[-1]
    # delete the last
    # fill_color <- fill_color[-length(fill_color)]
    
    # print(length(fill_color))
    # print(head(fill_color))
    
    # ADD THE COULOURS TO THE DATA
    data$fill <- fill_color
    
    # fill for the legend
    # compose the vector of values
    # print(head(data$fill))
    
    # create legend
    l <- colourvalues::colour_values(
      # x = c(data$valor, 300000)
      x = c(data$valor, scale_limits()$max)
      , n_summaries = 6,
      palette = palette
    )
    
    legend <- mapdeck::legend_element(
      variables = legend_converter(l$summary_values)
      , colours = if (input$indicador == "TMI") rev(l$summary_colours) else l$summary_colours
      , colour_type = "fill"
      , variable_type = "gradient"
      , title = legend
    )
    js_legend <- mapdeck::mapdeck_legend(legend)
    
    
    
    # create list with values for mapdeck options
    mapdeck_options <- list(
      # 'layer_id1'       = ifelse(input$indicador %in% c("CMA", "CMP"), "acess_min_go", "acess_cum_go"),
      # 'data'            = if(input$indicador %in% c("CMA", "CMP")) tempo_filtrado_sf() else if (input$indicador %in% c("TMI")) ,
      # 'layer_id2'       = ifelse(input$indicador %in% c("CMA", "CMP"), "acess_cum_go", "acess_min_go"),
      # 'palette1'        = if (input$indicador %in% c("CMA", "CMP")) "inferno" else if (input$indicador %in% c("TMI")) colorss,
      'legend_options1' = ifelse(input$indicador %in% c("CMA", "CMP"),
                                 i18n()$t("Oportunidades Acessíveis"),
                                 i18n()$t("Minutos até a oportunidade mais próxima"))
    )
    
    
    
    # Zoom in on the city when it's choosen
    mapdeck_update(map_id = "map") %>%
      mapdeck_view(location = c(centroid_go()$lon, centroid_go()$lat), zoom = zoom1(),
                   duration = 4000, transition = "fly") %>%
      clear_polygon(layer_id = mapdeck_id_clear()) %>%
      clear_pointcloud(layer_id = "brasil") %>%
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
        fill_colour = "fill",
        # fill_opacity = 200,
        layer_id = mapdeck_id,
        # layer_id = mapdeck_options$layer_id2,
        # palette = mapdeck_options$palette1,
        update_view = FALSE,
        focus_layer = FALSE,
        # auto_highlight = TRUE,
        tooltip = "popup",
        legend = js_legend,
        # legend = TRUE,
        # legend_options = list(title = i18n()$t(legend)),
        # legend_format = list( fill_colour = legend_converter),
        stroke_width = NULL,
        stroke_colour = NULL,
        stroke_opacity = 0
      )
    
    
  } else if (input$indicador_us == "us") {
    
    # print(sprintf("Mapdeck id clear1: %s", mapdeck_id_clear()))
    
    print("UUUUUUUUUIUIU")
    
    # mapdeck_id <- "us_update"
    # mapdeck_id_clear <- ifelse(input$indicador_us == "access", "us_initial", "access_initial")
    
    # print(sprintf("Mapdeck id: %s", mapdeck_id))
    
    
    legend_converter_us <- function(x) {
      return( scales::comma(as.integer(x), big.mark = " ", accuracy = 1) )
    }
    
    legend_converter <- if (input$indicador_us == "us" & grepl("^(P|T)", indicador_us_ok())) {
      legend_converter_us
    } else as.integer              
    
    legend_fill <- if (input$demo_ou_us == "demo" & input$atividade_demo %in% c("R002", "R003")) {
      "rdylbu"
    } else if (input$demo_ou_us == "activity") "viridis"
    
    legend_title <- if (input$demo_ou_us == "demo"  & input$atividade_demo %in% c("R001")) {
      "Renda per capita (R$)"
    } else if (input$demo_ou_us == "demo" & input$atividade_demo %in% c("R002")) {
      "Quintil de renda"
    } else if (input$demo_ou_us == "demo" & input$atividade_demo %in% c("R003")) {
      "Decil de renda"
    } else if (input$demo_ou_us == "activity") "Quantidade" else "Quantidade"
    
    mapdeck_update(map_id = "map") %>%
      mapdeck_view(location = c(centroid_go()$lon, centroid_go()$lat), zoom = zoom1(),
                   duration = 4000, transition = "fly") %>%
      clear_polygon(layer_id = ifelse(mapdeck_id_clear() == mapdeck_id, "oi", mapdeck_id_clear())) %>%
      clear_legend(layer_id = ifelse(mapdeck_id_clear() == mapdeck_id, "oi", mapdeck_id_clear())) %>%
      add_polygon(
        data = us_filtrado_ano_atividade_sf(),
        fill_colour = "valor",
        fill_opacity = 200,
        layer_id = mapdeck_id,
        palette = legend_fill,
        update_view = FALSE,
        focus_layer = FALSE,
        tooltip = "popup",
        legend = TRUE,
        na_colour = "#80808000",
        legend_options = list(title = i18n()$t(legend_title)),
        legend_format = list( fill_colour = legend_converter),
        stroke_width = NULL,
        stroke_colour = NULL,
        stroke_opacity = 0
      )
    
    # mapdeck_id_clear(mapdeck_id)
    
    
  }
  
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
                  
                  
                  
                  
                  
                  req(input$indicador_us == "access")
                  
                  # legend_converter_cma <- function(x) {
                  #   scales::comma(as.integer(x), big.mark = " ", accuracy = 100)
                  # }
                  # 
                  # legend_converter <- if (input$indicador_us == "access" & input$indicador %in% c("TMI")) {
                  #   as.integer
                  # } else legend_converter_cma
                  
                  legend_converter <- function (x) as.integer(x)
                  
                  
                  mapdeck_id <- "access_update"
                  
                  
                  if (input$indicador_us == "access") {
                    
                    
                    print(sprintf("Mapdeck id clear2: %s", mapdeck_id_clear()))
                    
                    if (input$indicador == "TMI") {
                      
                      
                      data <- atividade_filtrada_min_sf() %>%
                        dplyr::arrange(valor)
                      
                      print("mean")
                      print(mean(data$valor))
                      print(min(data$valor))
                      
                      fill_color <- colourvalues::colour_values(
                        # x = c(data$valor, 300000),
                        x = -c(data$valor, scale_limits()$max),
                        alpha = 200,
                        palette = "viridis"
                      )
                      
                      # adjust vector with colors
                      # delete the first
                      # fill_color <- fill_color[-1]
                      # delete the last
                      fill_color <- fill_color[-length(fill_color)]
                      
                      # ADD THE COULOURS TO THE DATA
                      data <- data %>% 
                        dplyr::mutate(fill = fill_color)
                      
                      # print("UEEEEE")
                      # print(data$fill)
                      # fill for the legend
                      # compose the vector of values
                      # print(head(data$fill))
                      
                      # create legend
                      l <- colourvalues::colour_values(
                        # x = c(data$valor, 300000)
                        x = c(data$valor, scale_limits()$max)
                        , n_summaries = 6,
                        palette = "viridis"
                      )
                      
                      
                      legend <- mapdeck::legend_element(
                        variables = legend_converter(l$summary_values)
                        , colours = rev(l$summary_colours)
                        , colour_type = "fill"
                        , variable_type = "gradient"
                        , title = i18n()$t("Minutos até a oportunidade mais próxima")
                      )
                      js_legend <- mapdeck::mapdeck_legend(legend)
                      # # create viridis scale in the reverse direction
                      # # create matrix
                      # colorss <- colourvalues::color_values_rgb(x = 1:256, "viridis")
                      # # invert matrix
                      # colorss <- apply(colorss, 2, rev)[, 1:3]
                      # # add alpha
                      # colorss <- cbind(colorss, 200)
                      
                      mapdeck_update(map_id = "map") %>%
                        clear_polygon(layer_id = ifelse(mapdeck_id_clear() == mapdeck_id, "oi", mapdeck_id_clear())) %>%
                        clear_legend(layer_id = ifelse(mapdeck_id_clear() == mapdeck_id, "oi", mapdeck_id_clear())) %>%
                        add_polygon(
                          data = data,
                          fill_colour = "fill",
                          # fill_opacity = 200,
                          layer_id = mapdeck_id,
                          # palette = colorss,
                          update_view = FALSE,
                          tooltip = "popup",
                          legend = js_legend,
                          # legend_options = list(title = i18n()$t("Minutos até a oportunidade mais próxima")),
                          # legend_format = list( fill_colour = legend_converter),
                          stroke_width = 0,
                          stroke_colour = NULL,
                          stroke_opacity = 0
                        )
                      
                    } else 
                      
                      if (input$indicador %in% c("CMA", "CMP")) {
                        
                        data <- tempo_filtrado_sf() %>%
                          dplyr::arrange(valor)
                        
                        # print("AAAAAAAAh")
                        
                        fill_color <- colourvalues::colour_values(
                          # x = c(data$valor, 300000),
                          x = c(data$valor, scale_limits()$max),
                          alpha = 200,
                          palette = "inferno"
                        )
                        # print(fill_color[-length(fill_color)])
                        # print(head(tempo_filtrado_sf()))
                        # print(c(tempo_filtrado_sf()$valor, scale_limits()$max))
                        
                        # ADD THE COULOURS TO THE DATA
                        data <- data %>% 
                          dplyr::mutate(fill = fill_color[-length(fill_color)])
                        # fill for the legend
                        # compose the vector of values
                        
                        # create legend
                        l <- colourvalues::colour_values(
                          # x = c(data$valor, 300000)
                          x = c(data$valor, scale_limits()$max)
                          , n_summaries = 6,
                          palette = "inferno"
                        )
                        legend <- mapdeck::legend_element(
                          variables = legend_converter(l$summary_values)
                          , colours = l$summary_colours
                          , colour_type = "fill"
                          , variable_type = "gradient"
                          , title = i18n()$t("Oportunidades Acessíveis")
                        )
                        js_legend <- mapdeck::mapdeck_legend(legend)
                        
                        mapdeck_update(map_id = "map") %>%
                          clear_polygon(layer_id = ifelse(mapdeck_id_clear() == mapdeck_id, "oi", mapdeck_id_clear())) %>%
                          clear_legend(layer_id = ifelse(mapdeck_id_clear() == mapdeck_id, "oi", mapdeck_id_clear())) %>%
                          add_polygon(
                            data = data,
                            fill_colour = "fill",
                            fill_opacity = 200,
                            layer_id = mapdeck_id,
                            # palette = "inferno",
                            update_view = FALSE,
                            focus_layer = FALSE,
                            # auto_highlight = TRUE,
                            tooltip = "popup",
                            legend = js_legend,
                            # legend_options = list(title = i18n()$t("Oportunidades Acessíveis")),
                            # legend_format = list( fill_colour = legend_converter),
                            stroke_width = NULL,
                            stroke_colour = NULL,
                            stroke_opacity = 0
                          )
                      }
                    
                    
                    
                    
                  } 
                  
                  
                  mapdeck_id_clear(mapdeck_id)
                  
                  
                })

# Observe any change on the atrributes on the city and change the map accordingly
# only for land use
observeEvent({c(input$indicador_us, 
                input$ano_us,
                input$ano_demo,
                input$demo_ou_us,
                input$atividade_demo, input$atividade_us)},{
                  
                  # print(nrow(atividade_filtrada_min_sf))
                  
                  # legend_converter_us <- function(x) {
                  #   return( scales::comma(as.integer(x), big.mark = " ", accuracy = 1) )
                  # }
                  # 
                  # legend_converter <- if (input$indicador_us == "us" & grepl("^(P|T)", indicador_us_ok())) {
                  #   legend_converter_us
                  # } else as.integer        
                  
                  legend_converter <- function (x) as.integer(x)
                  
                  legend_fill <- if (input$demo_ou_us == "demo" & input$atividade_demo %in% c("R002", "R003")) {
                    "rdylbu"
                  } else if (input$demo_ou_us == "activity") "viridis"
                  
                  legend_title <- if (input$demo_ou_us == "demo"  & input$atividade_demo %in% c("R001")) {
                    "Renda per capita (R$)"
                  } else if (input$demo_ou_us == "demo" & input$atividade_demo %in% c("R002")) {
                    "Quintil de renda"
                  } else if (input$demo_ou_us == "demo" & input$atividade_demo %in% c("R003")) {
                    "Decil de renda"
                  } else if (input$demo_ou_us == "activity") "Quantidade" else "Quantidade"
                  
                  
                  
                  
                  
                  if (input$indicador_us == "us") {
                    
                    print(sprintf("Mapdeck id clear1: %s", mapdeck_id_clear()))
                    
                    
                    
                    mapdeck_id <- "us_update"
                    # mapdeck_id_clear <- ifelse(input$indicador_us == "access", "us_initial", "access_initial")
                    
                    # print(sprintf("Mapdeck id: %s", mapdeck_id))
                    
                    mapdeck_update(map_id = "map") %>%
                      clear_polygon(layer_id = ifelse(mapdeck_id_clear() == mapdeck_id, "oi", mapdeck_id_clear())) %>%
                      clear_legend(layer_id = ifelse(mapdeck_id_clear() == mapdeck_id, "oi", mapdeck_id_clear())) %>%
                      add_polygon(
                        data = us_filtrado_ano_atividade_sf(),
                        fill_colour = "valor",
                        fill_opacity = 200,
                        layer_id = mapdeck_id,
                        palette = legend_fill,
                        update_view = FALSE,
                        focus_layer = FALSE,
                        tooltip = "popup",
                        legend = TRUE,
                        legend_options = list(title = i18n()$t(legend_title)),
                        legend_format = list( fill_colour = legend_converter),
                        stroke_width = NULL,
                        stroke_colour = NULL,
                        na_colour = "#80808000",
                        stroke_opacity = 0
                      )
                    
                    mapdeck_id_clear(mapdeck_id)
                    
                    
                  }
                  
                  
                })
