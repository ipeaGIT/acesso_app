

# UI SO COM UMA ABA -------------------------------------------------------

shinyUI(
    
    div(class = "navbar-default"
        , use_waiter(include_js = FALSE)
        , waiter_show_on_load(html = spin_loader(), color = "##F4F6F6")
        , navbarPage("Acesso a Oportunidades",
                     tabPanel("Mapa",
                              tags$head(includeCSS("www/styles.css")),
                              tags$head(tags$script("var cities_todos = ['for', 'spo', 'rio', 'cur', 'poa', 'bho', 'rec'];")),
                              tags$head(tags$script("var cities_ativo = ['bsb', 'sal', 'man', 'goi', 'bel', 'gua', 'cam', 'slz', 'sgo', 'mac', 'duq', 'cgr', 'nat'];")),
                              tags$head(tags$script("var modos_ativos = ['caminhada', 'bicicleta'];")),
                              tags$script(inactivity),
                              # https://divadnojnarg.github.io/blog/customsliderinput/
                              chooseSliderSkin("HTML5", color = "#112446"),
                              mapdeckOutput("map"),
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                                            top = 80, right = 20, width = 350, height = 630,
                                            uiOutput('page_content')
                              )
                     ),
                     
                     tabPanel("Sobre o projeto",
                              sidebarPanel(includeMarkdown("about.md"))
                     ) 
                     
        )
        
    )
)