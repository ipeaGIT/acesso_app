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
                      actionButton("downloadDic", i18n()$t("Baixe o dicionário dos dados (.xlsx)"), 
                                   onclick = "location.href='http://repositorio.ipea.gov.br/bitstream/11058/9586/5/dicionario2019_v1.0_20200116.xlsx';"),
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
    
    sf::st_write(cidade_filtrada(), file)
    
  }
  
)

output$downloadData2 <- downloadHandler(
  
  # generate button with data
  filename = function() {
    
    
    "acess.gpkg"
    
  },
  content = function(file) {
    
    sf::st_write(cidade_filtrada(), file)
    
  }
  
)