# MAP UI

output$page_content <- renderUI({
  
  # Create lists that will give the select options to the respective language
  list_trabalho <- list('Trabalho' = structure(c("TT", "TB", "TM", "TA"), .Names = c(i18n()$t("Trabalho Total"),
                                                                                     i18n()$t("Trabalho Baixa Escolaridade"),
                                                                                     i18n()$t("Trabalho Média Escolaridade"),
                                                                                     i18n()$t("Trabalho Alta Escolaridade"))))
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
  list_cras <- list('CRAS' = structure(c("CT"), 
                                       .Names = c(i18n()$t("Cras Total"))))
  
  list_pop_total <- list('População' = structure(c("PT"), 
                                                 .Names = c(i18n()$t("População Total"))))
  
  list_pop_sexo <- list('População por sexo' = structure(c("PH", "PM"), 
                                                         .Names = c(i18n()$t("População Homens"),
                                                                    i18n()$t("População Mulheres"))))
  
  list_pop_cor <- list('População por cor' = structure(c("PB", "PN", "PA", "PI"), 
                                                       .Names = c(i18n()$t("População Branca"),
                                                                  i18n()$t("População Negra"),
                                                                  i18n()$t("População Asiática"),
                                                                  i18n()$t("População Indígena"))))
  
  list_pop_idade <- list('População por idade' = structure(c("P0005I", "P0614I", "P1518I", "P1924I",
                                                             "P2539I", "P4069I", "P70I"), 
                                                           .Names = c(i18n()$t("População 0 a 5 anos"),
                                                                      i18n()$t("População 6 a 14 anos"),
                                                                      i18n()$t("População 15 a 18 anos"),
                                                                      i18n()$t("População 19 a 24 anos"),
                                                                      i18n()$t("População 25 a 39 anos"),
                                                                      i18n()$t("População 40 a 69 anos"),
                                                                      i18n()$t("População + 70 anos"))))
  
  
  list_idade <- list('Saúde' = structure(c("ST", "SB", "SM", "SA"), 
                                         .Names = c(i18n()$t("Saúde Total"),
                                                    i18n()$t("Saúde Baixa"),
                                                    i18n()$t("Saúde Média"),
                                                    i18n()$t("Saúde Alta"))))
  
  list_renda <- list('Renda' = structure(c("R001", "R002", "R003"), 
                                         .Names = c(i18n()$t("Renda per capita"),
                                                    i18n()$t("Quintil de renda"),
                                                    i18n()$t("Decil de renda"))))
  
  vector_indicadores_us <- structure(c("access", "us"), .Names = c(i18n()$t("Acessibilidade"), i18n()$t("Demográfico &#8226; <br>Uso do Solo")))
  vector_indicadores_us_go <- structure(c("demo", "activity"), .Names = c(i18n()$t("Demográficos"), i18n()$t("Uso do Solo")))
  vector_indicadores <- structure(c("CMA", "CMP", "TMI"), .Names = c(i18n()$t("Cumulativo Ativo"), i18n()$t("Cumulativo Passivo"), i18n()$t("Tempo Mínimo")))
  
  # Translate the name of lists accordingly
  names(list_trabalho) <-c(i18n()$t("Trabalho"))
  names(list_saude) <-c(i18n()$t("Saúde"))
  names(list_edu) <-c(i18n()$t("Educação"))
  names(list_cras) <-c("CRAS")
  
  names(list_pop_total) <- c(i18n()$t("População"))
  names(list_pop_sexo) <-  c(i18n()$t("População por sexo"))
  names(list_pop_cor) <-   c(i18n()$t("População por cor"))
  names(list_pop_idade) <- c(i18n()$t("População por idade"))
  names(list_renda) <-     c(i18n()$t("Renda"))
  
  
  
  
  # Start proper UI here 
  tagList(
    
    
    # 1) CITY SELECTION -------------------------------------------------------
    # https://www.rapidtables.com/code/text/unicode-characters.html
    
    pickerInput(inputId = "cidade",
                label = i18n()$t("Cidade"),
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
                           "fa-bus",
                           "",
                           "",
                           "fa-bus",
                           "",
                           "fa-bus",
                           "fa-bus",
                           "fa-bus",
                           "",
                           "",
                           "fa-bus"
                  )
                ),
                options = list('size' = 15,
                               'icon-base' = "fa",
                               'tickIcon' = "fa-check",
                               title = i18n()$t("Selecione aqui"))
    ),
    
    # THIS CONDITIONAL PANEL WILL UNFOLD NICELY WITH THE REMAINING SELECTIONS WHEN A CITY IS SELECTED
    
    conditionalPanel(
      # condition = "typeof output.city !== 'undefined'",
      condition = "input.cidade != ''",
      absolutePanel(
        id = "controls_animated", class = "w3-container w3-animate-opacity", 
        fixed = TRUE, draggable = FALSE,
        top = 155, right = 10, width = 350,
        
        
        # 2) INDICATOR SELECTION --------------------------------------------------
        
        radioGroupButtons(inputId = "indicador_us",
                          label = i18n()$t("Categoria do indicador"),
                          choices = vector_indicadores_us,
                          individual = TRUE,
                          justified = TRUE,
                          # status = "primary",
                          selected = "access"),
        conditionalPanel(
          condition = "input.indicador_us == 'access'",
          
          div(style="display:inline-block",
              radioButtons(inputId = "indicador",
                           label = label_with_info(
                             label = i18n()$t("Indicador de acessibilidade"),
                             tooltip_id = "q1"
                           ),
                           choices = vector_indicadores,
                           width = "220px",
                           selected = "CMA"),
              div(
                bsPopover(id = "q1",
                          title = sprintf("<strong>%s</strong>", i18n()$t("Indicadores de acessibilidade")),
                          content = HTML(i18n()$t(includeHTML('www/popovers/popover_indicator.html'))),
                          placement = "bottom",
                          trigger = "hover",
                          options = list(container = "body"))
              )
          ),
          div(style="display:inline-block", 
              radioButtons(inputId = "ano", 
                           label = i18n()$t("Ano"), 
                           choiceNames = list(HTML("2017"),
                                              HTML("2018"),
                                              HTML("2019")),
                           choiceValues = list(2017, 2018, 2019),
                           selected = 2017,
                           width = "85px"
              )),
          
          # 3) MODE SELECTION -------------------------------------------------------
          
          # IF A CITY WITH GTFS IS SELECTED, ALL MODES WILL BE AVAILABLE
          conditionalPanel(
            condition = "cities_todos.indexOf(''.concat(output.city, '_', input.ano)) > -1",
            radioGroupButtons(inputId = "modo_todos",
                              label = label_with_info(
                                label = i18n()$t("Modo de transporte"),
                                tooltip_id = "q_modo1"
                              ),
                              choices = c("<i class=\"fas fa-bus fa-2x\"></i>" = "public_transport", 
                                          "<i class=\"fas fa-car fa-2x\"></i>" = "car",
                                          "<i class=\"fas fa-walking fa-2x\"></i>" = "walk",
                                          "<i class=\"fas fa-bicycle fa-2x\"></i>" = "bicycle"),
                              selected = "public_transport",
                              # selected = character(0),
                              # selected = if (all(v_city$cidade == rv$prev_bins)) input$modo_ativo else "public_transport",
                              individual = TRUE,
                              justified = TRUE
            ),
            div(
              bsPopover(id = "q_modo1",
                        title = sprintf("<strong>%s</strong>", i18n()$t("Modo de transporte")),
                        content = HTML(if (input$selected_language == "pt") includeHTML('www/popovers/popover_modo.html') else includeHTML('www/popovers/popover_modo_en.html')),
                        placement = "bottom",
                        trigger = "hover",
                        options = list(container = "body"))
            )
          ),
          
          # IF A CITY WITHOUT GTFS IS SELECTED, ONLY WALKING AND BIKE WILL BE AVAILABLE
          # THE FUN 'RADIO_BUTTON_CUSTOM' WILL CREATE 3 RADIO BUTTONS WITH 1 BEING UNAVAILABLE
          
          conditionalPanel(
            condition = "cities_ativo.indexOf(''.concat(output.city, '_', input.ano)) > -1",
            radioGroupButtons(inputId = "modo_ativo",
                              label = label_with_info(
                                label = i18n()$t("Modo de transporte"),
                                tooltip_id = "q_modo2"
                              ),
                              choices = c("<i id=\"modo_des\" class=\"fas fa-bus fa-2x\" style=\"color: #e6e8eb;\"></i>" = "public_transport",
                                          "<i class=\"fas fa-car fa-2x\"></i>" = "car",
                                          "<i class=\"fas fa-walking fa-2x\"></i>" = "walk",
                                          "<i class=\"fas fa-bicycle fa-2x\"></i>" = "bicycle"),
                              selected = "walk",
                              # selected = ifelse(modo_cidade$teste == "ativo", "walk", input$modo_todos),
                              individual = TRUE,
                              justified = TRUE
            ),
            div(
              bsPopover(id = "q_modo2",
                        title = sprintf("<strong>%s</strong>", i18n()$t("Modo de transporte")),
                        content = HTML(i18n()$t(includeHTML('www/popovers/popover_modo.html'))),
                        placement = "bottom",
                        trigger = "hover",
                        options = list(container = "body"))
            )
            # radio_button_custom(label = h1(i18n()$t("Escolha o modo de transporte:")), inputId = "modo_ativo")
          ),
          
          div(
            bsTooltip(id = "modo_des", 
                      title = i18n()$t("Modo não disponível para essa cidade/ano"),
                      placement = "top",
                      trigger = "hover",
                      options = list(container = "body"))
          ),
          
          
          # 4) ACTIVITY SELECTION -------------------------------------------------------
          
          # IF THE CMA INDICATOR IS SELECTED, ALL ACTIVITIES WILL BE AVAILABLE
          
          conditionalPanel(
            condition = "input.indicador == 'CMA'",
            pickerInput(inputId = "atividade_cma",
                        label = label_with_info(
                          label = i18n()$t("Atividade"),
                          tooltip_id = "q3"
                        ),
                        choices = c(list_trabalho, list_saude, list_edu, list_cras),
                        selected = "TT")
          ),
          
          conditionalPanel(
            condition = "input.indicador == 'CMP'",
            pickerInput(inputId = "atividade_cmp",
                        label = label_with_info(
                          label = i18n()$t("Atividade"),
                          tooltip_id = "q3"
                        ),
                        choices = c(list_pop_total, list_pop_sexo, list_pop_cor, list_pop_idade),
                        selected = "PT")),
          
          # IF THE TMI INDICATOR IS SELECTED, ONLY HEALTH AND EDUCATIONAL ACTIVITIES WILL BE AVAILABLE
          
          conditionalPanel(
            condition = "input.indicador == 'TMI'",
            pickerInput(inputId = "atividade_min",
                        label = label_with_info(
                          label = i18n()$t("Atividade"),
                          tooltip_id = "q4"
                        ),
                        choices = c(list_saude, list_edu, list_cras),
                        selected = "ST")),
          div(
            bsPopover(id = "q3", 
                      title = sprintf("<strong>%s</strong>", i18n()$t("Atividades")),
                      content = HTML(i18n()$t(includeHTML("www/popovers/popover_activity.html"))),
                      placement = "top",
                      trigger = "hover",
                      options = list(container = "body"))
          ),
          div(
            bsPopover(id = "q4", 
                      title = sprintf("<strong>%s</strong>", i18n()$t("Atividades")),
                      content = HTML(i18n()$t(includeHTML("www/popovers/popover_activity.html"))),
                      placement = "top",
                      trigger = "hover",
                      options = list(container = "body"))
          ),
          
          
          # 5) TIME THRESHOLD SELECTION ---------------------------------------------
          
          conditionalPanel(
            condition = "ind_cum.indexOf(input.indicador) > -1 && output.tp",
            # condition = "cities_todos.indexOf(''.concat(input.cidade, '_', input.ano)) > -1 && ind_cum.indexOf(input.indicador) > -1 && (input.modo_todos == 'public_transport')",
            sliderInput(inputId = "tempo_tp",
                        label = i18n()$t("Tempo de viagem"),
                        min = 30, max = 120,
                        step = 30, value = 30,
                        animate = animationOptions(interval = 2000),
                        post = " min")
          ),
          conditionalPanel(
            condition = "ind_cum.indexOf(input.indicador) > -1 && !output.tp",
            # condition = "ind_cum.indexOf(input.indicador) > -1 && (modos_ativos.indexOf(input.modo_ativo)  > -1 || modos_ativos.indexOf(input.modo_todos)  > -1)",
            sliderInput(inputId = "tempo_ativo",
                        label = i18n()$t("Tempo de viagem"),
                        min = 15, max = 60,
                        step = 15, value = 15,
                        animate = animationOptions(interval = 2000),
                        post = " min")),
          
          # 6. ADITIONAL INFORMATION AT THE BOTTOM OF THE PANEL ------------------------
          
          conditionalPanel(condition = "input.indicador == 'TMI'",
                           strong(h1(i18n()$t("Observação"))), p(i18n()$t("Valores truncados para 30 minutos")))
          
        ),
        conditionalPanel(
          condition = "input.indicador_us == 'us'",
          radioGroupButtons(inputId = "demo_ou_us",
                            label = i18n()$t("Tipo de indicador"), 
                            choices = vector_indicadores_us_go,
                            selected = "demo",
                            individual = TRUE,
                            justified = TRUE
                            # status = "danger"
          ),
          conditionalPanel(
            condition = "input.demo_ou_us == 'demo'",
            awesomeRadio(inputId = "ano_demo", 
                         label = i18n()$t("Ano"), 
                         choices = c("2010" = "2019"),
                         selected = "2019",
                         inline = FALSE
            ),
            pickerInput(inputId = "atividade_demo",
                        label = i18n()$t("Indicador demográfico"),
                        choices = c(list_pop_total, list_pop_sexo, list_pop_cor, list_pop_idade, list_renda),
                        selected = "PT"
                        # options = list(maxItems = 5)
            )
          ),
          conditionalPanel(
            condition = "input.demo_ou_us == 'activity'",
            awesomeRadio(inputId = "ano_us", 
                         label = i18n()$t("Ano"), 
                         choices = c(2017, 2018, 2019),
                         selected = 2019,
                         inline = TRUE
            ),
            pickerInput(inputId = "atividade_us",
                        label = i18n()$t("Indicador de uso do solo"),
                        choices = c(list_trabalho, list_saude, list_edu, list_cras),
                        selected = "TT")
          )
          
          
          
          
        )
        
      )
      
    )
  )
  
  
  
})