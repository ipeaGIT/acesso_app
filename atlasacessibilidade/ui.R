library(shiny)
library(shinyWidgets)
library(mapdeck)
library(shinyBS)
library(waiter)



# UI SO COM UMA ABA -------------------------------------------------------
a <- HTML("<p>Fortaleza <i class=\"fas fa-bus fa-sm\"></i></p>")

div(class = "navbar-default"
    , use_waiter(include_js = FALSE)
    , waiter_show_on_load(html = spin_loader(), color = "##F4F6F6")
    , navbarPage("Acesso a Oportunidades",
               tabPanel("Mapa",
                        tags$head(includeCSS("www/styles.css")),
                        tags$head(tags$script("var cities_todos = ['for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec'];")),
                        tags$head(tags$script("var cities_ativo = ['bsb', 'sal', 'man', 'goi', 'bel', 'gua', 'cam', 'slz', 'sgo', 'mac', 'duq', 'cgr', 'nat'];")),
                        tags$head(tags$script("var modos_ativos = ['caminhada', 'bicicleta'];")),
                        # https://divadnojnarg.github.io/blog/customsliderinput/
                        chooseSliderSkin("HTML5", color = "#112446"),
                        mapdeckOutput("map"),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                                      top = 80, right = 20, width = 350, height = 630,
                                      pickerInput(inputId = "cidade",
                                                  label = h1("Escolha a cidade:"),
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
                                                                 title = "Selecione aqui")
                                                  ),
                                      awesomeRadio(inputId = "indicador",
                                                   # label = HTML("<h1>Escolha o indicador de acessibilidade: <img src=\"ipea.jpg\" align=\"leftright\" width=\"70\"/></h1>"),
                                                   label = HTML("<h1>Escolha o indicador de acessibilidade: 
                                                                  <button id=\"q1\" type=\"button\" class=\"btn btn-light btn-xs\"><i class=\"fa fa-info\"></i></button></h1>"),
                                                   choices = c("Cumulativo" ="CMA", "Tempo Mínimo" = "TMI"),
                                                   selected = "CMA"),
                                      div(
                                        # edit2
                                        bsPopover(id = "q1", 
                                                  title = "<strong>Indicadores de acessibilidade</strong>",
                                                  content = HTML("<ul><li><strong>Indicador cumulativo</strong> representa a proporção de oportunidades em relação ao total da cidade que podem ser alcançadas dado um tempo máximo de viagem</li><li><strong>Tempo mínimo</strong> é o tempo de viagem até a oportunidade mais próxima</li></ul>"),
                                                  placement = "bottom",
                                                  trigger = "hover",
                                                  options = list(container = "body"))
                                      ),
                                      conditionalPanel(condition = "cities_todos.indexOf(input.cidade) > -1", 
                                                       radioGroupButtons(inputId = "modo_todos",
                                                                         # label = HTML("<h1>Escolha o indicador de acessibilidade: <img src=\"ipea.jpg\" align=\"leftright\" width=\"70\"/></h1>"),
                                                                         label = HTML("<h1>Escolha o modo de transporte:"), 
                                                                         choices = c("<i class=\"fas fa-bus fa-2x\"></i>" = "tp", 
                                                                                     "<i class=\"fas fa-walking fa-2x\"></i>" = "caminhada",
                                                                                     "<i class=\"fas fa-bicycle fa-2x\"></i>" = "bicicleta"),
                                                                         selected = "tp",
                                                                         individual = TRUE,
                                                                         justified = TRUE
                                                       )),
                                      conditionalPanel(condition = "cities_ativo.indexOf(input.cidade) > -1", 
                                                       # radioGroupButtons(inputId = "modo_ativo",
                                                       #                   # label = HTML("<h1>Escolha o indicador de acessibilidade: <img src=\"ipea.jpg\" align=\"leftright\" width=\"70\"/></h1>"),
                                                       #                   label = HTML("<h1>Escolha o modo de transporte:</h1>"),
                                                       #                   choices = c("<i id =\"modo_des\" class=\"fas fa-bus fa-2x\" style=\"color: #F2F3F4;\"></i>" = "error",
                                                       #                               "<i class=\"fas fa-walking fa-2x\"></i>" = "caminhada",
                                                       #                               "<i class=\"fas fa-bicycle fa-2x\"></i>" = "bicicleta"),
                                                       #                   # choiceNames = list(HTML("<i class=\"fas fa-walking fa-2x\"></i>"),
                                                       #                   # HTML("<i class=\"fas fa-bicycle fa-2x\"></i>")),
                                                       #                   # choiceValues = list("caminhada",
                                                       #                   # "bicicleta"),
                                                       #                   selected = "caminhada",
                                                       #                   individual = TRUE,
                                                       #                   justified = TRUE
                                                       #                   # checkIcon = list(
                                                       #                   #   yes = tags$i(class = "fa fa-circle", 
                                                       #                   #                style = "color: steelblue"),
                                                       #                   #   no = tags$i(class = "fa fa-circle-o", 
                                                       #                   #               style = "color: steelblue"))
                                                       # )
                                                       # Tive que fazer esse ui manualmente pq nao consegui mudar a cor do botao de 'tp' quando o transporte era ativo
                                                       includeHTML("www/radio_modo_para_ativas.html")
                                                       ),
                                      
                                      div(
                                        # edit2
                                        bsTooltip(id = "modo_des", 
                                                  title = "Modo não disponível para essa cidade",
                                                  placement = "top",
                                                  trigger = "hover",
                                                  options = list(container = "body"))
                                      ),
                                      # img(src='ipea.jpg', align = "right", width = "150"),
                                      conditionalPanel(condition = "input.indicador == 'CMA'",
                                                       pickerInput(inputId = "atividade_cum",
                                                                   label = HTML("<h1>Escolha a atividade: <button id=\"q3\" type=\"button\" class=\"btn btn-light btn-xs\"><i class=\"fa fa-info\"></i></button></h1>"),
                                                                   choices = list(
                                                                     'Trabalho' = c("Trabalho Total" = "TT"
                                                                                    # , "Trabalho Quintil" = "TQ"
                                                                                    # , "Trabalho Decil" = "TD"
                                                                                    ),
                                                                     'Saúde' = c("Saúde Total" = "ST",
                                                                                 "Saúde Baixa" = "SB",
                                                                                 "Saúde Média" = "SM",
                                                                                 "Saúde Alta" = "SA"),
                                                                     'Educação' = c("Educação Total" = "ET",
                                                                                    "Educação Infantil" = "EI",
                                                                                    "Educação Fundamental" = "EF",
                                                                                    "Educação Média" = "EM")),
                                                                   selected = "Trabalho Total")),
                                      conditionalPanel(condition = "input.indicador == 'TMI'",
                                                       selectInput(inputId = "atividade_min",
                                                                   label = HTML("<h1>Escolha a atividade: <button id=\"q4\" type=\"button\" class=\"btn btn-light btn-xs\"><i class=\"fa fa-info\"></i></button></h1>"),
                                                                       choices = list(
                                                                     'Saúde' = c("Saúde Total" = "ST",
                                                                                 "Saúde Baixa" = "SB",
                                                                                 "Saúde Média" = "SM",
                                                                                 "Saúde Alta" = "SA"),
                                                                     'Educação' = c("Educação Total" = "ET",
                                                                                    "Educação Infantil" = "EI",
                                                                                    "Educação Fundamental" = "EF",
                                                                                    "Educação Média" = "EM")),
                                                                   selected = "Saúde")),
                                      div(
                                        # edit2
                                        bsPopover(id = "q3", 
                                                  title = HTML("<strong>Atividades</strong>"),
                                                  content = HTML("<ul><li> Atividades com o sufixo <em>Total</em> representam todas as atividades</li><li> Sufixos da atividade de <b>saúde</b> (<em>Baixa, Média</em> e <em>Alta</em>) representam o nível de atenção dos serviços prestados</li></ul>"),
                                                  placement = "top",
                                                  trigger = "hover",
                                                  options = list(container = "body"))
                                      ),
                                      div(
                                        # edit2
                                        bsPopover(id = "q4", 
                                                  title = HTML("<strong>Atividades</strong>"),
                                                  content = HTML("<ul><li> Atividades com o sufixo <em>Total</em> representam a soma das subdivisões da atividade</li><li> Sufixos da atividade de <b>saúde</b> (<em>Baixa, Média</em> e <em>Alta</em>) representam a complexidade daqueles estabelecimentos</li></ul>"),
                                                  placement = "top",
                                                  trigger = "hover",
                                                  options = list(container = "body"))
                                      ),
                                      conditionalPanel(condition = "cities_todos.indexOf(input.cidade) > -1 && input.indicador == 'CMA' && input.modo_todos == 'tp'",
                                                       sliderInput(inputId = "tempo_tp",
                                                                   label = h1("Escolha o tempo de viagem:"),
                                                                   min = 30, max = 120,
                                                                   step = 30, value = 30,
                                                                   animate = animationOptions(interval = 2000),
                                                                   post = " min")),
                                      conditionalPanel(condition = "cities_todos.indexOf(input.cidade) > -1 && input.indicador == 'CMA' && modos_ativos.indexOf(input.modo_todos) > -1",
                                                       sliderInput(inputId = "tempo_ativo_tp",
                                                                   label = h1("Escolha o tempo de viagem:"),
                                                                   min = 15, max = 60,
                                                                   step = 15, value = 15,
                                                                   animate = animationOptions(interval = 2000),
                                                                   post = " min")),
                                      conditionalPanel(condition = "cities_ativo.indexOf(input.cidade) > -1 && input.indicador == 'CMA' && modos_ativos.indexOf(input.modo_ativo) > -1",
                                                       sliderInput(inputId = "tempo_ativo",
                                                                   label = h1("Escolha o tempo de viagem:"),
                                                                   min = 15, max = 60,
                                                                   step = 15, value = 15,
                                                                   animate = animationOptions(interval = 2000),
                                                                   post = " min")),
                                      conditionalPanel(condition = "input.indicador == 'TMI'",
                                                       strong("Observação"), p("Valores truncados para 30 minutos"))
                                      # waiter_show_on_load(html = spin_fading_circles())
                                      
                        ),
                         ),
               tabPanel("Sobre o projeto",
                        sidebarPanel(includeMarkdown("about.md")),
               ) 
               
               
               
    )
    
    
)




# div(class = "navbar-default",
#     navbarPage("Acesso a Oportunidades",
#                tabPanel("Mapa",
#                         tags$head(includeCSS("www/styles.css")),
#                         # https://divadnojnarg.github.io/blog/customsliderinput/
#                         chooseSliderSkin("Modern"),
#                         # titlePanel(HTML("<h1>&emsp;Acesso a Oportunidades</h1>")),
#                         mapdeckOutput("map"),
#                         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
#                                       top = 80, right = 20, width = 300, height = 600,
#                                       selectInput(inputId = "cidade",
#                                                   label = h1("Escolha a cidade:"), 
#                                                   choices = c("Belo Horizonte", "Fortaleza", "Rio de Janeiro",
#                                                               "São Paulo", "Curitiba", "Porto Alegre", "Recife"),
#                                                   selected = "Fortaleza"),
#                                       div(
#                                         style="display: inline-block;vertical-align:top;width: 230px;",
#                                         awesomeRadio(inputId = "indicador",
#                                                      label = h1("Escolha o indicador de acessibilidade: "),
#                                                      choices = c("Cumulativo", "Oportunidade mais próxima"),
#                                                      selected = "Cumulativo")),
#                                       div(
#                                         # edit2
#                                         style="display: inline-block;vertical-align:top;float:right;",
#                                         bsButton("q1", label = "", icon = icon("question"),
#                                                  style = "info", size = "extra-small"),
#                                         bsPopover(id = "q1", title = "Tidy data",
#                                                   content = paste0("You should read the ", 
#                                                                    a("tidy data paper", 
#                                                                      href = "http://vita.had.co.nz/papers/tidy-data.pdf",
#                                                                      target="_blank")),
#                                                   placement = "left", 
#                                                   trigger = "click"
#                                         )),
#                                       # img(src='ipea.jpg', align = "right", width = "150"),
#                                       conditionalPanel(condition = "input.indicador == 'Cumulativo'",
#                                                        selectInput(inputId = "atividade_cum",
#                                                                    label = h1("Escolha a atividade:"),
#                                                                    choices = list(
#                                                                      'Trabalho' = c("Trabalho Total" = "TT",
#                                                                                     "Trabalho Quintil" = "TQ",
#                                                                                     "Trabalho Decil" = "TD"),
#                                                                      'Saúde' = c("Saúde Total" = "ST",
#                                                                                  "Saúde Baixa" = "SB",
#                                                                                  "Saúde Média" = "SM",
#                                                                                  "Saúde Alta" = "SA"),
#                                                                      'Educação' = c("Educação Total" = "ET",
#                                                                                     "Educação Infantil" = "EI",
#                                                                                     "Educação Fundamental" = "EF",
#                                                                                     "Educação Média" = "EM")),
#                                                                    selected = "Trabalho Total")),
#                                       conditionalPanel(condition = "input.indicador == 'Oportunidade mais próxima'",
#                                                        selectInput(inputId = "atividade_min",
#                                                                    label = h1("Escolha a atividade:"),
#                                                                    choices = list(
#                                                                      'Saúde' = c("Saúde Total" = "ST",
#                                                                                  "Saúde Baixa" = "SB",
#                                                                                  "Saúde Média" = "SM",
#                                                                                  "Saúde Alta" = "SA"),
#                                                                      'Educação' = c("Educação Total" = "ET",
#                                                                                     "Educação Infantil" = "EI",
#                                                                                     "Educação Fundamental" = "EF",
#                                                                                     "Educação Média" = "EM")),
#                                                                    selected = "Saúde")),
#                                       conditionalPanel(condition = "input.indicador == 'Cumulativo'",
#                                                        sliderInput(inputId = "tempo",
#                                                                    label = h1("Escolha o tempo de viagem:"),
#                                                                    min = 30, max = 120,
#                                                                    step = 30, value = 30,
#                                                                    animate = TRUE)),
#                                       conditionalPanel(condition = "input.indicador == 'Oportunidade mais próxima'",
#                                                        strong("Observação"), p("Valores truncados para 30 minutos"))
#                         )
#                ),
#                tabPanel("Sobre o projeto",
#                         sidebarPanel(includeMarkdown("about.md"))
#                )
#                
#     )
# )