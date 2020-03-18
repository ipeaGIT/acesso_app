

# 2) MODAL WITH LANGUAGE OPTION AT STARTUP ----------------------------------------------------------
query_modal <- div(id = "modal_lang", 
                   modalDialog(
                     # title = HTML("<h1>Instruções para uso do mapa interativo na aba ao lado &nbsp<i class=\"fas fa-arrow-right\"></i></h1>"),
                     title = HTML("<h1>Lingua // Language</h1>"),
                     renderUI({
                       div(style = "width: 50%;margin: 0 auto;", 
                           pickerInput(inputId = 'selected_language',
                                       # label = "Escolha a lingua:",
                                       choices = c(a = "pt", b = "en"),
                                       choicesOpt = list(content = (c("<p><img src='img/pt.png' width=30px>&nbsp;&nbsp;Português</img></p>",
                                                                      "<p><img src='img/en_new.png' width=30px>&nbsp;&nbsp; English</img></p>"))),
                                       selected = input$selected_language),
                           
                           # actionButton(inputId = "openDetails",
                           #              label = "",
                           #              icon = icon("check"))
                       )
                     }),
                     
                     # includeHTML("www/carousel_2.html"),
                     easyClose = TRUE,
                     size = "m",
                     footer = div(id = "openDetails", class = "btn btn-default action-button shiny-bound-input",
                                  tagList(
                                    modalButton(icon("check"))
                                    # actionButton(inputId = "openDetails", label = "", icon = icon("check"))
                                  )
                     )
                     
                   )
)



translator_server <- function(input, output, session) {
  
  # 3.1 Reactive to select the translator for the active langague -------------
  i18n <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% translator$languages) {
      translator$set_translation_language(selected)
    }
    translator
  })
  
}