# label <- "Selecione etc"
# inputId <- "modo_ativo"
# divClass <- "btn-group-justified"

radio_button_custom <- function(label, inputId, divClass = "btn-group-justified") {
  

 htmltools::tags$div(class = "form-group shiny-input-container shiny-input-radiogroup shiny-input-container-inline", 
                                            htmltools::tags$label(class = "control-label", `for` = inputId, 
                                                                  label), htmltools::tags$br(), htmltools::tags$div(id = inputId, 
                                                                                                              class = "radioGroupButtons", style = "width: 100%;", htmltools::tags$div(class = divClass, 
                                                                                                                                                    role = "group", `aria-label` = "...", `data-toggle` = "buttons", 
                                                                                                                                                    class = "btn-group-container-sw", includeHTML("www/radio_modo_para_ativas_partial.html"))))
}