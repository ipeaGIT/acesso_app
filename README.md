# Shiny App from Project Acess to Opportunities

## Instructions

After cloning the repository, start by locating the `atlasacessibilidade/acesso_app.Rproj` file and opening it in RStudio. 

### Packages setup

First, you need to install the required packages. While most of them are available at CRAN (and can be installed via `install.packagees`), some functionalities required in the app are only avaliable on pacagkes at development versions. 

Run the following commands to install these packages:

```r
devtools::install_github("SymbolixAU/jsonify", force = TRUE)
devtools::install_github("dcooley/sfheaders", force = TRUE)
devtools::install_github("SymbolixAU/geojsonsf", force = TRUE)
devtools::install_github("SymbolixAU/colourvalues", force = TRUE)
devtools::install_github("SymbolixAU/spatialwidget", force = TRUE)
devtools::install_github("SymbolixAU/mapdeck")

devtools::install_github("JohnCoene/waiter")
devtools::install_github("hrbrmstr/hrbrthemes")
devtools::install_github("Appsilon/shiny.i18n")
```

### Data setup

Data is zipped, so you're gonna need to unzip it before proceding. You can do this using the ``zip`` package:

```r
zip::unzip("data/acess_app_data_2019.zip")
```