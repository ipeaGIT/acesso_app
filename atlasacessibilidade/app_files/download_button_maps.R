# UI FOR THE DOWNLOAD BUTTONS -----------------------------------------------------------------

output$ui_download_button_maps <- renderUI({
  
  tagList(
    
    conditionalPanel(
      # condition = "typeof output.city !== 'undefined'",
      condition = "input.cidade != ''",
      absolutePanel(id = "download_panel_maps", class = "panel panel-default", 
                    fixed = TRUE, draggable = FALSE,
                    top = 70, right = 300, width = 200, height = 100,
                    dropdownButton(
                      tagList(
                        downloadButton("downloadData1", i18n()$t("Baixe os indicadores para a cidade (.gpkg)")),
                        downloadButton("downloadData2", i18n()$t("Baixe os indicadores para todas as cidades (.gpkg)"))
                      ),
                      hr(),
                      actionButton("downloadDic", i18n()$t("Visite a página com o dicionário e todos os dados"), 
                                   onclick = if(input$selected_language == "pt") "location.href='https://www.ipea.gov.br/acessooportunidades/dados';" else "location.href='https://www.ipea.gov.br/acessooportunidades/en/dados';"),
                                   circle = FALSE, 
                                   # status = "danger",
                                   label = "Download",
                                   right = TRUE,
                                   up = FALSE,
                                   icon = icon("download"), 
                                   width = "350px",
                                   # tooltip = tooltipOptions(title = "Click to see inputs !"),
                                   inputId = "download_dropdown_maps"
                                   
                      )
                    )
      )
      
    )
    
    })  
  
  
  
  
  # data
  output$downloadData1 <- downloadHandler(
    
    # generate button with data
    filename = function() {
      
      
      sprintf("acess_%s.gpkg", input$cidade)
      
    },
    content = function(file) {
      
      sf::st_write(cidade_filtrada() %>% dplyr::left_join(hex_filtrado(), by = 'id_hex') %>% st_sf(crs = 4326), file)
      
    }
    
  )
  
  output$downloadData2 <- downloadHandler(
    
    # generate button with data
    filename = function() {
      
      
      "acess.gpkg"
      
    },
    content = function(file) {
      
      sf::st_write(cidade_filtrada()  %>% dplyr::left_join(hex_filtrado(), by = 'id_hex') %>% st_sf(crs = 4326), file)
      
    }
    
  )