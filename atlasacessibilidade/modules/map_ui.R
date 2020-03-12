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
  
  names(list_trabalho) <-c(i18n()$t("Trabalho"))
  names(list_saude) <-c(i18n()$t("Saúde"))
  names(list_edu) <-c(i18n()$t("Educação"))
  
  list_us <- list('Uso do Solo' = structure(c("US"),
                                            .Names = c("Uso do Solo")))
  
  list_acess <- list('Acessibilidade' = structure(c("CMA", "TMI"),
                                                  .Names = c(i18n()$t("Cumulativo"), i18n()$t("Tempo Mínimo"))))
  
  names(list_us) <-c(i18n()$t("Uso do Solo"))
  names(list_acess) <-c(i18n()$t("Acessibilidade"))
  
  
  
  
  # Start proper UI here 
  tagList(
    
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
    conditionalPanel(condition = "input.cidade != ''",
                     absolutePanel(id = "controls_animated", class = "w3-container w3-animate-opacity", 
                                   fixed = TRUE, draggable = FALSE,
                                   top = 180, right = 20, width = 350,
                                   pickerInput(inputId = "indicador",
                                                # label = HTML("<h1>Escolha o indicador de acessibilidade: <img src=\"ipea.jpg\" align=\"leftright\" width=\"70\"/></h1>"),
                                                label = HTML(sprintf("<h1>%s <button id=\"q1\" type=\"button\" class=\"btn btn-light btn-xs\"><i class=\"fa fa-info\"></i></button></h1>", 
                                                                     i18n()$t("Escolha o indicador:"))),
                                                choices = c(list_us, list_acess),
                                                selected = "CMA"),
                                   div(
                                     # edit2
                                     bsPopover(id = "q1", 
                                               title = sprintf("<strong>%s</strong>", i18n()$t("Indicadores de acessibilidade")),
                                               content = HTML(i18n()$t("<ul><li><strong>Indicador cumulativo</strong> representa a proporção de oportunidades em relação ao total da cidade que podem ser alcançadas dado um tempo máximo de viagem</li><li><strong>Tempo mínimo</strong> é o tempo de viagem até a oportunidade mais próxima</li></ul>")),
                                               placement = "bottom",
                                               trigger = "hover",
                                               options = list(container = "body"))
                                   ),
                                   conditionalPanel(condition = "input.indicador == 'US'", 
                                                    pickerInput(inputId = "atividade_us",
                                                                label = i18n()$t("Escolha a atividade:"),
                                                                choices = c(list_saude, list_edu),
                                                                selected = "ET")),
                     conditionalPanel(condition = "input.indicador != 'US'", 
                                      conditionalPanel(condition = "cities_todos.indexOf(input.cidade) > -1", 
                                                       radioGroupButtons(inputId = "modo_todos",
                                                                         # label = HTML("<h1>Escolha o indicador de acessibilidade: <img src=\"ipea.jpg\" align=\"leftright\" width=\"70\"/></h1>"),
                                                                         label = h1(i18n()$t("Escolha o modo de transporte:")), 
                                                                         choices = c("<i class=\"fas fa-bus fa-2x\"></i>" = "tp", 
                                                                                     "<i class=\"fas fa-walking fa-2x\"></i>" = "caminhada",
                                                                                     "<i class=\"fas fa-bicycle fa-2x\"></i>" = "bicicleta"),
                                                                         selected = "tp",
                                                                         individual = TRUE,
                                                                         justified = TRUE
                                                       )),
                                      conditionalPanel(condition = "cities_ativo.indexOf(input.cidade) > -1", 
                                                       radio_button_custom(label = h1(i18n()$t("Escolha o modo de transporte:")), inputId = "modo_ativo")
                                      ),
                                      
                                      div(
                                        # edit2
                                        bsTooltip(id = "modo_des", 
                                                  title = i18n()$t("Modo não disponível para essa cidade"),
                                                  placement = "top",
                                                  trigger = "hover",
                                                  options = list(container = "body"))
                                      ),
                                      # img(src='ipea.jpg', align = "right", width = "150"),
                                      conditionalPanel(condition = "input.indicador == 'CMA'",
                                                       pickerInput(inputId = "atividade_cum",
                                                                   label = HTML(sprintf("<h1>%s: <button id=\"q3\" type=\"button\" class=\"btn btn-light btn-xs\"><i class=\"fa fa-info\"></i></button></h1>", 
                                                                                        i18n()$t("Escolha a atividade"))),
                                                                   choices = c(list_trabalho, list_saude, list_edu),
                                                                   selected = "TT")),
                                      conditionalPanel(condition = "input.indicador == 'TMI'",
                                                       pickerInput(inputId = "atividade_min",
                                                                   label = HTML(sprintf("<h1>%s: <button id=\"q4\" type=\"button\" class=\"btn btn-light btn-xs\"><i class=\"fa fa-info\"></i></button></h1>", 
                                                                                        i18n()$t("Escolha a atividade"))),
                                                                   choices = c(list_saude, list_edu),
                                                                   selected = "ST")),
                                      div(
                                        # edit2
                                        bsPopover(id = "q3", 
                                                  title = sprintf("<strong>%s</strong>", i18n()$t("Atividades")),
                                                  content = HTML(i18n()$t("<ul><li> Atividades com o sufixo <em>Total</em> representam todas as atividades</li><li> Sufixos da atividade de <b>saúde</b> (<em>Baixa, Média</em> e <em>Alta</em>) representam o nível de atenção dos serviços prestados</li></ul>")),
                                                  placement = "top",
                                                  trigger = "hover",
                                                  options = list(container = "body"))
                                      ),
                                      div(
                                        # edit2
                                        bsPopover(id = "q4", 
                                                  title = sprintf("<strong>%s</strong>", i18n()$t("Atividades")),
                                                  content = HTML(i18n()$t("<ul><li> Atividades com o sufixo <em>Total</em> representam todas as atividades</li><li> Sufixos da atividade de <b>saúde</b> (<em>Baixa, Média</em> e <em>Alta</em>) representam o nível de atenção dos serviços prestados</li></ul>")),
                                                  placement = "top",
                                                  trigger = "hover",
                                                  options = list(container = "body"))
                                      ),
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
                                      conditionalPanel(condition = "input.indicador == 'TMI'",
                                                       strong(h1(i18n()$t("Observação"))), p(i18n()$t("Valores truncados para 30 minutos")))
                                      
                     )
    )
  )
  
  )
  
  
  
})