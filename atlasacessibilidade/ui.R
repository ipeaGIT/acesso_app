

# BASE UI -------------------------------------------------------

shinyUI(
    
    div(class = "navbar-default",
        # Load Css
        tags$head(includeCSS("www/styles.css")),
        # Create some variables in JS
        tags$head(tags$script("var cities_todos = ['for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec'];")),
        tags$head(tags$script("var cities_ativo = ['bsb', 'sal', 'man', 'goi', 'bel', 'gua', 'cam', 'slz', 'sgo', 'mac', 'duq', 'cgr', 'nat'];")),
        tags$head(tags$script("var modos_ativos = ['caminhada', 'bicicleta'];")),
        tags$head(tags$script("var graphs_cma = ['palma_renda', 'palma_cor'];")),
        tags$head(tags$script("var graphs_tmi = ['dumbell_renda', 'dumbell_cor'];")),
        # Select custom slider
        # https://divadnojnarg.github.io/blog/customsliderinput/
        chooseSliderSkin("HTML5", color = "#112446")
        , use_waiter(include_js = FALSE)
        , waiter_show_on_load(html = spin_loader(), color = "##F4F6F6")
        , navbarPage("", id = "tabs",
                     # tabsetPanel(id = "tabs",
                                 tabPanel(title = "", value = "tab_home",
                                          uiOutput('landing_page')),
                                 tabPanel(title = uiOutput('title_map'), value = "tab_mapa", 
                                          # Output map
                                          mapdeckOutput("map"),
                                          # Create the main absolute panel  
                                          absolutePanel(id = "controls", class = "panel panel-default", 
                                                        fixed = TRUE, draggable = FALSE,
                                                        top = 80, right = 20, width = 350, height = 615,
                                                        # Output the 'UI' that was generated in the server
                                                        uiOutput('page_content')
                                          )
                                 ),
                                 
                                 tabPanel(id = "tab_graphs", title = uiOutput('title_graph'),
                                          absolutePanel(id = "controls_graphs", class = "panel panel-default", 
                                                        fixed = TRUE, draggable = FALSE,
                                                        top = 80, right = 20, width = 420, height = 615,
                                                        uiOutput('graphs')
                                          ),
                                          # panel to but download button
                                          absolutePanel(id = "download_panel", class = "panel panel-default", 
                                                        fixed = TRUE, draggable = FALSE,
                                                        top = 80, right = 380, width = 200, height = 100,
                                                        dropdownButton(
                                                            uiOutput('ui_download_button'),
                                                            hr(),
                                                            uiOutput('ui_download_dic'),
                                                            circle = FALSE, 
                                                            # status = "danger",
                                                            label = "Download",
                                                            right = TRUE,
                                                            up = FALSE,
                                                            icon = icon("download"), width = "500px",
                                                            # tooltip = tooltipOptions(title = "Click to see inputs !"),
                                                            inputId = "download_dropdown"
                                                            
                                                        )),
                                          absolutePanel(id = "controls_graphs1", class = "panel panel-default", 
                                                        fixed = TRUE, draggable = FALSE,
                                                        top = 80, left = 80, height = "100%",
                                                        highchartOutput('output_graph', height = "90%")
                                          )
                                    )
                                 
                                 
        )
        
    )
)