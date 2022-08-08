

# BASE UI -------------------------------------------------------

shinyUI(
  
  div(class = "navbar-default",
      # Load Css
      tags$head(includeCSS("www/styles.css")),
      # Create some variables in JS
      tags$head(includeScript("www/cities_modes.js")),
      tags$head(tags$script("var ind_cum = ['CMA', 'CMP'];")),
      tags$head(tags$script("var modos_ativos = ['walk', 'bicycle'];")),
      tags$head(tags$script("var modos_todos = ['public_transport', 'walk', 'bicycle'];")),
      tags$head(tags$script("var graphs_cma = ['palma_renda', 'palma_cor'];")),
      tags$head(tags$script("var graphs_tmi = ['dumbell_renda', 'dumbell_cor'];")),
      shinyjs::useShinyjs()
      # Select custom slider
      # https://divadnojnarg.github.io/blog/customsliderinput/
      # chooseSliderSkin("Flat", color = "#112446")
      # Use loading page
      # Use loading page
      , use_waiter()
      , waiter_show_on_load(html = spin_loader(), color = "##F4F6F6")
      # Start navbar page
      , navbarPage("", id = "tabs",
                   # Landing page
                   tabPanel(title = "", value = "tab_home",
                            uiOutput('landing_page')),
                   # Map page
                   tabPanel(title = uiOutput('title_map'), value = "tab_mapa", 
                            # Output map
                            mapdeckOutput("map"),
                            # Create the side panel  
                            absolutePanel(id = "controls", class = "panel panel-default", 
                                          fixed = TRUE, draggable = FALSE,
                                          top = 55, right = 10, width = 350, height = 595,
                                          # Output the 'UI' that was generated in the server
                                          uiOutput('page_content')
                            ),
                            # Panel to put download button
                            uiOutput('ui_download_button_maps'),
                   ),
                   # Graphs page
                   tabPanel(value = "tab_graphs", title = uiOutput('title_graph'),
                            # Create the side panel
                            absolutePanel(id = "controls_graphs", class = "panel panel-default", 
                                          fixed = TRUE, draggable = FALSE,
                                          top = 60, right = 10, width = 350,
                                          uiOutput('graphs')
                            ),
                            # Panel to put download button
                            absolutePanel(id = "download_panel", class = "panel panel-default", 
                                          fixed = TRUE, draggable = FALSE,
                                          top = 70, right = 300, width = 200, height = 100,
                                          dropdownButton(
                                            uiOutput('ui_download_button'),
                                            hr(),
                                            uiOutput('ui_download_dic'),
                                            circle = FALSE, 
                                            # status = "danger",
                                            label = "Download",
                                            right = TRUE,
                                            up = FALSE,
                                            icon = icon("download"), 
                                            width = "300px",
                                            # tooltip = tooltipOptions(title = "Click to see inputs !"),
                                            inputId = "download_dropdown"
                                            
                                          )),
                            # Panel to the graphs output
                            absolutePanel(id = "controls_graphs1", class = "panel panel-default", 
                                          fixed = TRUE, draggable = FALSE,
                                          top = 80, left = 80, height = "100%",
                                          highchartOutput('output_graph', height = "90%", width = "100%")
                            )
                   )
                   
                   
      )
      
  )
)