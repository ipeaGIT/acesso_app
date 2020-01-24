

# BASE UI -------------------------------------------------------

shinyUI(
    
    div(class = "navbar-default"
        , use_waiter(include_js = FALSE)
        , waiter_show_on_load(html = spin_loader(), color = "##F4F6F6")
        , navbarPage("Acesso a Oportunidades",
                     tabPanel("Mapa",
                              # Load Css
                              tags$head(includeCSS("www/styles.css")),
                              # Create some variables in JS
                              tags$head(tags$script("var cities_todos = ['for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec'];")),
                              tags$head(tags$script("var cities_ativo = ['bsb', 'sal', 'man', 'goi', 'bel', 'gua', 'cam', 'slz', 'sgo', 'mac', 'duq', 'cgr', 'nat'];")),
                              tags$head(tags$script("var modos_ativos = ['caminhada', 'bicicleta'];")),
                              tags$script(inactivity),
                              # Select custom slider
                              # https://divadnojnarg.github.io/blog/customsliderinput/
                              chooseSliderSkin("HTML5", color = "#112446"),
                              # Output map
                              mapdeckOutput("map"),
                              # Create the main absolute panel
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                                            top = 80, right = 20, width = 350, height = 630,
                                            # Output the 'UI' that was generated in the server
                                            uiOutput('page_content')
                              )
                     ),
                     
                     tabPanel("Sobre o projeto",
                              sidebarPanel(includeMarkdown("about.md"))
                     ) 
                     
        )
        
    )
)