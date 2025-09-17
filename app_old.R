# Certifique-se de ter os pacotes instalados: install.packages(c("shiny", "sf", "leaflet"))
library(shiny)
library(sf)
library(leaflet)

# Define a UI com o painel de abas
ui <- fluidPage(
  titlePanel("GeochemMapper"),
  
  # Painel de abas principal
  tabsetPanel(
    id = "main_tabs",
    
    # Aba 1: Visualização do Mapa
    tabPanel(
      "Visualização do Mapa",
      sidebarLayout(
        sidebarPanel(
          h4("Carregar Camadas"),
          fileInput("file_area", "Limite da área (.shp, etc.)", multiple = TRUE),
          fileInput("file_srtm", "Limite srtm (.shp, etc.)", multiple = TRUE),
          fileInput("file_stations", "Estações de Coleta (.shp, etc.)", multiple = TRUE),
          fileInput("file_rivers", "Rios e Drenagens (.shp, etc.)", multiple = TRUE),
          fileInput("file_waterbodies", "Massas d'Água (.shp, etc.)", multiple = TRUE)
        ),
        mainPanel(
          leafletOutput("map_layers", height = "80vh")
        )
      )
    ),
    
    # Aba 2: Processamento
    tabPanel(
      "Processamento",
      fluidRow(
        column(12,
               h3("Escolha o Tipo de Processamento"),
               radioButtons("processing_type", "",
                            choices = c("Gera DEM" = "gera_dem",
                                        "Modelar Bacias" = "model_basins",
                                        "Planejar Estações" = "plan_stations"),
                            selected = "model_basins"),
               hr(),
               uiOutput("processing_ui")
        )
      )
    )
  )
)

# Define a lógica do servidor
server <- function(input, output, session) {
  
  # Função reativa para ler qualquer shapefile de forma segura
  read_shapefile <- reactive({
    function(file_input) {
      req(file_input)
      temp_dir <- tempdir()
      file_list <- file_input
      for (i in seq_along(file_list$datapath)) {
        file.copy(file_list$datapath[i], file.path(temp_dir, file_list$name[i]))
      }
      shp_file <- file_list$name[grep(".shp$", file_list$name, ignore.case = TRUE)]
      if (length(shp_file) == 0) return(NULL)
      sf_data <- st_read(file.path(temp_dir, shp_file), quiet = TRUE)
      if (st_crs(sf_data)$epsg != 4326) {
        sf_data <- st_transform(sf_data, crs = 4326)
      }
      return(sf_data)
    }
  })
  
  # Funções reativas para carregar as camadas
  stations_sf <- reactive({ read_shapefile()(input$file_stations) })
  rivers_sf <- reactive({ read_shapefile()(input$file_rivers) })
  waterbodies_sf <- reactive({ read_shapefile()(input$file_waterbodies) })
  area_sf <- reactive({ read_shapefile()(input$file_area) })
  srtm_sf <- reactive({ read_shapefile()(input$file_srtm) })
  
  # Renderiza o mapa com todas as camadas carregadas
  output$map_layers <- renderLeaflet({
    m <- leaflet() %>% addTiles()
    
    # Adiciona a camada de limite da área
    if (!is.null(area_sf()) && nrow(area_sf()) > 0) {
      m <- m %>% 
        addPolygons(data = area_sf(), color = "black", weight = 2, fillOpacity = 0, group = "Limite da Área")
    }
    if (!is.null(srtm_sf()) && nrow(srtm_sf()) > 0) {
      m <- m %>% 
        addPolygons(data = srtm_sf(), color = "black", weight = 2, fillOpacity = 0, group = "Limite srtm")
    }
    if (!is.null(rivers_sf()) && nrow(rivers_sf()) > 0) {
      m <- m %>% 
        addPolylines(data = rivers_sf(), color = "blue", weight = 2, group = "Rios")
    }
    
    if (!is.null(waterbodies_sf()) && nrow(waterbodies_sf()) > 0) {
      m <- m %>% 
        addPolygons(data = waterbodies_sf(), color = "cyan", fillOpacity = 0.5, group = "Massas d'Água")
    }
    
    if (!is.null(stations_sf()) && nrow(stations_sf()) > 0) {
      stations_points <- st_centroid(stations_sf())
      m <- m %>% 
        addCircleMarkers(data = stations_points, 
                         popup = ~as.character(stations_points[[1]]), 
                         radius = 5, color = "red", fillOpacity = 0.8, group = "Estações")
    }
    
    m
  })
  
  # Lógica para renderizar a UI de processamento dinamicamente
  output$processing_ui <- renderUI({
    if (input$processing_type == "gera_dem") {
      tagList(
        h4("Ferramenta: Gerar DEM"),
        p("Esta função irá carregar e processar o DEM para a área de interesse."),
        actionButton("run_gera_dem", "Preparar DEM")
      )
    } else if (input$processing_type == "model_basins") {
      tagList(
        h4("Ferramenta: Modelar Bacias"),
        p("Esta função irá modelar as bacias de captação para as estações carregadas."),
        actionButton("run_basins_model", "Rodar Análise de Bacias"),
        hr(),
        downloadButton("download_basins", "Baixar Bacias Modeladas")
      )
    } else if (input$processing_type == "plan_stations") {
      tagList(
        h4("Ferramenta: Planejar Estações"),
        p("Use os controles abaixo para planejar novas localizações de estações."),
        sliderInput("grid_spacing", "Espaçamento da Grade (km)", min = 0.5, max = 10, value = 2, step = 0.5),
        actionButton("generate_plan", "Gerar Pontos de Planejamento")
      )
    }
  })
}

# Roda a aplicação
shinyApp(ui = ui, server = server)