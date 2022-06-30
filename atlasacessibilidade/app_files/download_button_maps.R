# UI FOR THE DOWNLOAD BUTTONS -----------------------------------------------------------------

output$ui_download_button_maps <- renderUI({
  
  tagList(
    downloadButton("downloadData1", i18n()$t("Baixe os indicadores para a cidade (.gpkg)")),
    downloadButton("downloadData2", i18n()$t("Baixe os indicadores para todas as cidades (.gpkg)")),
  )
  
})  

# UI FOR THE DOWNLOAD BUTTONS - dictionary -----------------------------------------------------------------

output$ui_download_dic_maps <- renderUI({
  
  actionButton("downloadDic", i18n()$t("Baixe o dicionário dos dados (.xlsx)"), 
               onclick = "location.href='http://repositorio.ipea.gov.br/bitstream/11058/9586/5/dicionario2019_v1.0_20200116.xlsx';")
  
  # downloadButton("downloadDic", i18n()$t("Baixe o dicionário dos dados (.xlsx)"))
  
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