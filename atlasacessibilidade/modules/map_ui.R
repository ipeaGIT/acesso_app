output$page_content <- renderUI({
  
  # Create lists that will give the select options to the respective language
  list_trabalho <- list('Trabalho' = structure(c("TT"), .Names = c(i18n()$t("Trabalho Total"))))
  list_saude <- list('Saúde' = structure(c("ST", "SB", "SM", "SA"), 
                                         .Names = c(i18n()$t("Saúde Total"),
                                                    i18n()$t("Saúde Baixa"),
                                                    i18n()$t("Saúde Média"),
                                                    i18n()$t("Saúde Alta"))))
  list_edu <- list('Educação' = structure(c("ET", "EI", "EF", "EM"), 
                                          .Names = c(i18n()$t("Educação Total"),
                                                     i18n()$t("Educação Infantil"),
                                                     i18n()$t("Educação Fundamental"),
                                                     i18n()$t("Educação Média"))))
  vector_indicadores <- structure(c("CMA", "TMI"), .Names = c(i18n()$t("Cumulativo"), i18n()$t("Tempo Mínimo")))
  
  # Translate the name of lists accordingly
  names(list_trabalho) <-c(i18n()$t("Trabalho"))
  names(list_saude) <-c(i18n()$t("Saúde"))
  names(list_edu) <-c(i18n()$t("Educação"))
  
  
  
  # Start proper UI here 
  tagList(
    

    # 1. CITY SELECTION -------------------------------------------------------

    pickerInput(inputId = "cidade",
                label = h1(i18n()$t("Escolha a cidade:")),
                choices = list(
                  'Norte' = c("Belém" = "bel",
                              "Manaus" = "man"),
                  'Nordeste' = c("Fortaleza" = "for",
                                 "Maceió" = "mac",
                                 "Natal" = "nat",
                                 "Recife" = "rec",
                                 "Salvador" = "sal",
                                 "São Luís" = "slz"),
                  'Sudeste' = c("Belo Horizonte" = "bho",
                                "Campinas" = "cam",
                                "Duque de Caxias" = "duq",
                                "Guarulhos" = "gua",
                                "Rio de Janeiro" = "rio",
                                "São Gonçalo" = "sgo",
                                "São Paulo" = "spo"),
                  'Sul' = c("Curitiba" = "cur",
                            "Porto Alegre" = "poa"),
                  'Centro-Oeste' = c("Brasília" = "bsb",
                                     "Campo Grande" = "cgr",
                                     "Goiânia" = "goi")
                ),
                choicesOpt = list(
                  icon = c("", 
                           "",
                           "fa-bus",
                           "",
                           "",
                           "fa-bus",
                           "",
                           "",
                           "fa-bus",
                           "",
                           "",
                           "",
                           "fa-bus",
                           "",
                           "fa-bus",
                           "fa-bus",
                           "fa-bus",
                           "",
                           "",
                           ""
                  )
                ),
                options = list('size' = 15,
                               'icon-base' = "fa",
                               'tickIcon' = "fa-check",
                               title = i18n()$t("Selecione aqui"))
    ),
    
    # THIS CONDITIONAL PANEL WILL UNFOLD NICELY WITH THE REMAINING SELECTIONS WHEN A CITY IS SELECTED
    
    conditionalPanel(condition = "input.cidade != ''",
                     absolutePanel(id = "controls_animated", class = "w3-container w3-animate-opacity", 
                                   fixed = TRUE, draggable = FALSE,
                                   top = 180, right = 20, width = 350,
                                   

                                   # 2. INDICATOR SELECTION --------------------------------------------------

                                   awesomeRadio(inputId = "indicador",
                                                label = label_with_info(label = i18n()$t("Escolha o indicador de acessibilidade"),
                                                                        tooltip_id = "q1"),
                                                choices = vector_indicadores,
                                                selected = "CMA"),
                                   div(
                                     bsPopover(id = "q1", 
                                               title = sprintf("<strong>%s</strong>", i18n()$t("Indicadores de acessibilidade")),
                                               content = HTML(i18n()$t("<ul><li><strong>Indicador cumulativo</strong> representa a proporção de oportunidades em relação ao total da cidade que podem ser alcançadas dado um tempo máximo de viagem</li><li><strong>Tempo mínimo</strong> é o tempo de viagem até a oportunidade mais próxima</li></ul>")),
                                               placement = "bottom",
                                               trigger = "hover",
                                               options = list(container = "body"))
                                   ),
                                   

                                   # 3. MODE SELECTION -------------------------------------------------------
                                   
                                   # IF A CITY WITH GTFS IS SELECTED, ALL MODES WILL BE AVAILABLE
                                   conditionalPanel(condition = "cities_todos.indexOf(input.cidade) > -1", 
                                                    radioGroupButtons(inputId = "modo_todos",
                                                                      label = h1(i18n()$t("Escolha o modo de transporte:")), 
                                                                      choices = c("<i class=\"fas fa-bus fa-2x\"></i>" = "tp", 
                                                                                  "<i class=\"fas fa-walking fa-2x\"></i>" = "caminhada",
                                                                                  "<i class=\"fas fa-bicycle fa-2x\"></i>" = "bicicleta"),
                                                                      selected = "tp",
                                                                      individual = TRUE,
                                                                      justified = TRUE
                                                    )),
                                   
                                   # IF A CITY WITHOUT GTFS IS SELECTED, ONLY WALKING AND BIKE WILL BE AVAILABLE
                                   # THE FUN 'RADIO_BUTTON_CUSTOM' WILL CREATE 3 RADIO BUTTONS WITH 1 BEING UNAVAILABLE
                                   
                                   conditionalPanel(condition = "cities_ativo.indexOf(input.cidade) > -1", 
                                                    radio_button_custom(label = h1(i18n()$t("Escolha o modo de transporte:")), inputId = "modo_ativo")
                                   ),
                                   
                                   div(
                                     bsTooltip(id = "modo_des", 
                                               title = i18n()$t("Modo não disponível para essa cidade"),
                                               placement = "top",
                                               trigger = "hover",
                                               options = list(container = "body"))
                                   ),
                                   

                                   # 4. ACTIVITY SELECTION -------------------------------------------------------
                                   
                                   # IF THE CMA INDICATOR IS SELECTED, ALL ACTIVITIES WILL BE AVAILABLE
                                   
                                   conditionalPanel(condition = "input.indicador == 'CMA'",
                                                    pickerInput(inputId = "atividade_cum",
                                                                label = label_with_info(label = i18n()$t("Escolha a atividade"),
                                                                                        tooltip_id = "q3"),
                                                                choices = c(list_trabalho, list_saude, list_edu),
                                                                selected = "TT")),
                                   
                                   # IF THE TMI INDICATOR IS SELECTED, ONLY HEALTH AND EDUCATIONAL ACTIVITIES WILL BE AVAILABLE
                                   
                                   conditionalPanel(condition = "input.indicador == 'TMI'",
                                                    pickerInput(inputId = "atividade_min",
                                                                label = label_with_info(label = i18n()$t("Escolha a atividade"),
                                                                                        tooltip_id = "q4"),
                                                                choices = c(list_saude, list_edu),
                                                                selected = "ST")),
                                   div(
                                     bsPopover(id = "q3", 
                                               title = sprintf("<strong>%s</strong>", i18n()$t("Atividades")),
                                               content = HTML(i18n()$t("<ul><li> Atividades com o sufixo <em>Total</em> representam todas as atividades</li><li> Sufixos da atividade de <b>saúde</b> (<em>Baixa, Média</em> e <em>Alta</em>) representam o nível de atenção dos serviços prestados</li></ul>")),
                                               placement = "top",
                                               trigger = "hover",
                                               options = list(container = "body"))
                                   ),
                                   div(
                                     bsPopover(id = "q4", 
                                               title = sprintf("<strong>%s</strong>", i18n()$t("Atividades")),
                                               content = HTML(i18n()$t("<ul><li> Atividades com o sufixo <em>Total</em> representam todas as atividades</li><li> Sufixos da atividade de <b>saúde</b> (<em>Baixa, Média</em> e <em>Alta</em>) representam o nível de atenção dos serviços prestados</li></ul>")),
                                               placement = "top",
                                               trigger = "hover",
                                               options = list(container = "body"))
                                   ),
                                   

                                   # 5. TIME THRESHOLD SELECTION ---------------------------------------------

                                   conditionalPanel(condition = "cities_todos.indexOf(input.cidade) > -1 && input.indicador == 'CMA' && input.modo_todos == 'tp'",
                                                    sliderInput(inputId = "tempo_tp",
                                                                label = h1(i18n()$t("Escolha o tempo de viagem:")),
                                                                min = 30, max = 120,
                                                                step = 30, value = 30,
                                                                animate = animationOptions(interval = 2000),
                                                                post = " min")),
                                   conditionalPanel(condition = "cities_todos.indexOf(input.cidade) > -1 && input.indicador == 'CMA' && modos_ativos.indexOf(input.modo_todos) > -1",
                                                    sliderInput(inputId = "tempo_ativo_tp",
                                                                label = h1(i18n()$t("Escolha o tempo de viagem:")),
                                                                min = 15, max = 60,
                                                                step = 15, value = 15,
                                                                animate = animationOptions(interval = 2000),
                                                                post = " min")),
                                   conditionalPanel(condition = "cities_ativo.indexOf(input.cidade) > -1 && input.indicador == 'CMA' && modos_ativos.indexOf(input.modo_ativo) > -1",
                                                    sliderInput(inputId = "tempo_ativo",
                                                                label = h1(i18n()$t("Escolha o tempo de viagem:")),
                                                                min = 15, max = 60,
                                                                step = 15, value = 15,
                                                                animate = animationOptions(interval = 2000),
                                                                post = " min")),
                                   
                                   # 6. ADITIONAL INFORMATION AT THE BOTTOM OF THE PANEL ------------------------
                                   
                                   conditionalPanel(condition = "input.indicador == 'TMI'",
                                                    strong(h1(i18n()$t("Observação"))), p(i18n()$t("Valores truncados para 30 minutos")))
                                   
                     )
    )
  )
  
  
  
})