
# 1) LOAD DATA ----------------------------------------------------------------------------------------

# MAP DATA

# Accessiblity data
acess <- read_rds("data/acess_wide.rds")

# Hex Spatial Sf Data
hex <- read_rds("data/hex.rds")

# Cities centroids data
centroids <- read_rds("data/cities_centroids.rds")

# Cities limits data
limits <- read_rds(("data/cities_limits.rds"))

#####################################

# GRAPHS DATA
# Palma data
palma_renda <- read_rds("data/acess_palma_renda.rds") %>% setDT()
palma_cor <- read_rds("data/acess_palma_cor.rds") %>% setDT()

# Dumbell data
dumbell_renda <- read_rds("data/acess_dumbell_renda.rds") %>% setDT()
dumbell_cor <- read_rds("data/acess_dumbell_cor.rds") %>% setDT()

# Define a server for the Shiny app
function(input, output, session) {
  
  # 2) MODAL WITH LANGUAGE OPTION AT STARTUP ----------------------------------------------------------
  query_modal <- div(id = "modal_lang", 
                     modalDialog(
                       title = HTML("<h1>Lingua // Language</h1>"),
                       renderUI({
                         div(style = "width: 50%;margin: 0 auto;", 
                             pickerInput(inputId = 'selected_language',
                                         choices = c(a = "pt", b = "en"),
                                         choicesOpt = list(
                                           content = (c("<p><img src='img/pt.png' width=30px>&nbsp;&nbsp;Português</img></p>",
                                                        "<p><img src='img/en_new.png' width=30px>&nbsp;&nbsp; English</img></p>"))),
                                         selected = input$selected_language),
                         )
                       }),
                       easyClose = TRUE,
                       size = "m",
                       footer = div(id = "openDetails", class = "btn btn-default action-button shiny-bound-input",
                                    tagList(
                                      modalButton(icon("check"))
                                    )
                       )
                       
                     )
  )
  
  # Show the model on start up ...
  showModal(query_modal)
  
  
  # 3) RENDER LANDING PAGE -------------------------------------------------------------------------
  
  observeEvent(input$openDetails, {
    
    output$landing_page <- renderUI({
      
      if (input$selected_language == "pt") {
        
        includeMarkdown("www/landing_page/home_pt.md")
        
      } else if (input$selected_language == "en") {
        
        includeMarkdown("www/landing_page/home_en.md")
      }
      
      
    })
    
  })
  
  
  
  # 4) Reactive to select the translator for the active langague -------------
  i18n <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% translator$languages) {
      translator$set_translation_language(selected)
    }
    translator
  })
  
  
  
  # 5) TRANSLATE TAB TITLES ------------------------------------------------------------------------
  
  # Translate map title stab
  output$title_map = renderText({
    switch(input$selected_language, "pt"="Mapa", "en"="Map") 
  })  
  
  # Translate graphs title tab
  output$title_graph = renderText({
    switch(input$selected_language, "pt"="Gráficos", "en"="Charts") 
  })  
  
  
  
  # 6) UI: GRAPHS ----------------------------------------------------------------------------------
  source("app_files/graphs_ui.R", local = TRUE)
  
  # 7) UI: MAP -------------------------------------------------------------------------------------
  source("app_files/map_ui.R", local = TRUE)
  
  # 8) DOWNLOAD BUTTON -----------------------------------------------------------------------------
  source("app_files/download_button.R", local = TRUE)
  
  # 9) SERVER: GRAPHS ------------------------------------------------------------------------------
  source("app_files/graphs_server.R", local = TRUE)
  
  # 10) SERVER: MAP --------------------------------------------------------------------------------
  source("app_files/map_server.R", local = TRUE)
  
  
  
  
}