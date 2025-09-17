# Certifique-se de ter os pacotes instalados:
# install.packages(c("shiny", "sf", "leaflet", "terra", "digeoqR", "zip"))

library(shiny)
library(sf)
library(leaflet)
library(digeoqR)
library(terra)
library(zip)

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
          fileInput("file_area", "Limite da área (.shp, etc.)", multiple = TRUE, accept = ".shp"),
          fileInput("file_stations", "Estações de Coleta (.shp, etc.)", multiple = TRUE, accept = ".shp"),
          fileInput("file_rivers", "Rios e Drenagens (.shp, etc.)", multiple = TRUE, accept = ".shp"),
          fileInput("file_waterbodies", "Massas d'Água (.shp, etc.)", multiple = TRUE, accept = ".shp"),
          fileInput("file_limite_srtm", "Limite SRTM (.shp, etc.)", multiple = TRUE, accept = ".shp"),
          # Novo botão para download do DEM
          hr(),
          h4("Baixar Resultados"),
          downloadButton("download_dem_map", "Baixar DEM (tif)") 
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
                            selected = "gera_dem"),
               hr(),
               uiOutput("processing_ui"),
               hr(),
               uiOutput("output_selection_ui")
        )
      )
    )
  )
)

# Define a lógica do servidor
server <- function(input, output, session) {
  
  # Variáveis reativas para armazenar os dados e o DEM
  dem_raster <- reactiveVal(NULL)
  basins_model <- reactiveVal(NULL)
  dem_running <- reactiveVal(FALSE)
  
  # Função reativa para ler qualquer shapefile de forma segura
  read_shapefile <- reactive({
    function(file_input) {
      req(file_input)
      temp_dir <- file.path(tempdir(), "data_in")
      dir.create(temp_dir, showWarnings = FALSE)
      
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
  limite_srtm_sf <- reactive({ read_shapefile()(input$file_limite_srtm) })
  
  # Renderiza o mapa com todas as camadas carregadas
  output$map_layers <- renderLeaflet({
    m <- leaflet() %>% addTiles()
    if (!is.null(area_sf()) && nrow(area_sf()) > 0) {
      m <- m %>% addPolygons(data = area_sf(), color = "black", weight = 2, fillOpacity = 0, group = "Limite da Área")
    }
    if (!is.null(limite_srtm_sf()) && nrow(limite_srtm_sf()) > 0) {
      m <- m %>% addPolygons(data = limite_srtm_sf(), color = "black", weight = 2, fillOpacity = 0, group = "Limite SRTM")
    }
    
    if (!is.null(rivers_sf()) && nrow(rivers_sf()) > 0) {
      m <- m %>% addPolylines(data = rivers_sf(), color = "blue", weight = 2, group = "Rios")
    }
    if (!is.null(waterbodies_sf()) && nrow(waterbodies_sf()) > 0) {
      m <- m %>% addPolygons(data = waterbodies_sf(), color = "cyan", fillOpacity = 0.5, group = "Massas d'Água")
    }
    if (!is.null(stations_sf()) && nrow(stations_sf()) > 0) {
      stations_points <- st_centroid(stations_sf())
      m <- m %>% addCircleMarkers(data = stations_points, popup = ~as.character(stations_points[[1]]), 
                                  radius = 5, color = "red", fillOpacity = 0.8, group = "Estações")
    }
    m
  })
  output$output_selection_ui <- renderUI({
    # Exibe a caixa de seleção apenas se o resultado da modelagem estiver disponível
    if (!is.null(basins_model())) {
      tagList(
        selectInput("basin_output_select", "Escolha a camada para download:",
                    choices = names(basins_model()),
                    selected = "bacias"),
        downloadButton("download_basins", "Baixar Camada Selecionada")
      )
    }
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
        numericInput("threshold", "Threshold", value = 250),
        numericInput("min_length", "Min Length", value = 0.02),
        numericInput("dist_buffer", "Dist Buffer", value = 0.01/10),
        numericInput("classe_am", "Class Sample", value = 2),
        numericInput("tipo", "Process type", value = 1),
        numericInput("funcao_snap", "Snap function", value = 1),
        numericInput("snap_dist", "Snap Distance", value = 0.02),
        numericInput("max_ordem", "Maximum Order", value = 4),

        hr(),
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
  # Lógica para rodar o processamento do DEM
  observeEvent(input$run_gera_dem, {
    
    if (dem_running()) {
      return(NULL)
    }
    dem_running(TRUE)
    
    showNotification("Iniciando a preparação do DEM. Por favor, aguarde...", duration = NULL, type = "message", id = "dem_prep_notification")
    
    # Garante que o shapefile de limite foi carregado e pega o nome do arquivo
    req(input$file_limite_srtm)
    
    file_name <- input$file_limite_srtm$name[grep(".shp$", input$file_limite_srtm$name, ignore.case = TRUE)]
   
    # --------------------------------
    
    # 3. Define o diretório de saída para o DEM
    dir_out_dem <- file.path(tempdir(), "dem_out")
    dir.create(dir_out_dem, showWarnings = FALSE)
    
    dem_result <- tryCatch({
      # 4. Chama a função prepara_dem
      digeoqR::prepara_dem(
        dir_in = paste0(file.path(tempdir(), "data_in"), "/"), 
        limite_srtm = file_name, # Passa apenas o NOME do arquivo
        dir_out = dir_out_dem, 
        EPSG = 4674, 
        z = 11
      )
    }, error = function(e) {
      showNotification(paste("Erro ao preparar o DEM:", e$message), type = "error")
      return(NULL)
    }, finally = {
      removeNotification(id = "dem_prep_notification")
      dem_running(FALSE)
    })
    
    req(dem_result)
    dem_raster(dem_result)
    showNotification("DEM preparado com sucesso!", type = "message")
  })
  
  observeEvent(input$run_basins_model, {
    showNotification("Iniciando a modelagem das bacias. Por favor, aguarde...", duration = NULL, type = "message", id = "basin_prep_notification")
    
    req(dem_raster())
    req(area_sf(), limite_srtm_sf(), rivers_sf(), waterbodies_sf(), stations_sf())
    
    dir_in <- file.path(tempdir(), "data_in")

    # Garante que o shapefile de limite foi carregado e pega o nome do arquivo
    req(input$file_area)
    
    file_name1 <- input$file_area$name[grep(".shp$", input$file_area$name, ignore.case = TRUE)]
    file_name2 <- input$file_limite_srtm$name[grep(".shp$", input$file_limite_srtm$name, ignore.case = TRUE)]
    file_name3 <- input$file_rivers$name[grep(".shp$", input$file_rivers$name, ignore.case = TRUE)]
    file_name4 <- input$file_waterbodies$name[grep(".shp$", input$file_waterbodies$name, ignore.case = TRUE)]
    file_name5 <- input$file_stations$name[grep(".shp$", input$file_stations$name, ignore.case = TRUE)]
    
    bases_model <- tryCatch({
      digeoqR::gera_bases_model(
        dir_in = paste0(dir_in, "/"), 
        limite = file_name1,
        limite_srtm = file_name2,
        rios = file_name3,
        massa_dagua = file_name4,
        modo_excluir = FALSE
      )
    }, error = function(e) {
      showNotification(paste("Erro em gera_bases_model:", e$message), type = "error")
      return(NULL)
    })
    
    req(bases_model)
    
    terreno_model <- tryCatch({
      digeoqR::modela_terreno(
        dem = dem_raster(), 
        dir_out = paste0(dir_in, "/"), 
        bases_model = bases_model, 
        modo_excluir = FALSE, 
        EPSG = 4674, 
        threshold = input$threshold, 
        min_length = input$min_length, 
        dist_buffer = input$dist_buffer, 
        wbt_wd = paste0(dir_in, "/"), 
        gera_estacoes = FALSE
      )
    }, error = function(e) {
      showNotification(paste("Erro em modela_terreno:", e$message), type = "error")
      return(NULL)
    })
    
    req(terreno_model)
    
    bacias_model <- tryCatch({digeoqR::modela_bacias(fase = 2, EPSG = 4674, dem = dem_raster(), 
                           bases_model = bases_model, 
                           terreno = terreno_model, 
                           classe_am = input$classe_am,fonte_shp = TRUE, 
                           arquivo_shp = paste0(dir_in, "/", file_name5), 
                           tipo = input$tipo, 
                           funcao_snap = input$funcao_snap, 
                           snap_dist = input$snap_dist, 
                           max_ordem = input$max_ordem, 
                           dir_out = paste0(dir_in, "/"), 
                           wbt_wd = paste0(dir_in, "/"))}, error = function(e) {
                             showNotification(paste("Erro durante a modelagem das bacias:", e$message), type = "error")
                             return(NULL)
                           }, finally = {
                             # 4. Remove a notificação de "iniciando"
                             removeNotification(id = "basin_prep_notification")
                           })
           showNotification("Análise de bacias concluída com sucesso!", type = "message")
           req(bacias_model)
           basins_model(bacias_model)
           
           })
  
  # Lógica para o botão de download do DEM
  output$download_dem_map <- downloadHandler(
    filename = function() {
      # Define o nome do arquivo que o usuário irá baixar
      paste0("dem_outsrtm-", Sys.Date(), ".tif")
    },
    content = function(file) {
      # Requer que o DEM raster esteja disponível
      req(dem_raster())
      
      # Converte o SpatRaster para um formato que o usuário possa salvar
      terra::writeRaster(dem_raster(), file, overwrite = TRUE)
    }
  ) 
  # Lógica para o botão de download das bacias
  output$download_basins <- downloadHandler(
    filename = function() {
      # Garante que os dados estejam prontos antes de definir o nome do arquivo
      req(basins_model())
      
      # Usa a seleção do usuário para nomear o arquivo zip
      paste0(input$basin_output_select, "-", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Garante que os dados estejam prontos antes de tentar o download
      req(basins_model())
      
      # Pega o objeto sf da lista com base na seleção do usuário
      selected_sf <- basins_model()[[input$basin_output_select]]
      
      # Garante que a camada selecionada existe
      req(selected_sf)
      
      temp_dir_download <- file.path(tempdir(), "bacias_modeladas_temp")
      dir.create(temp_dir_download, showWarnings = FALSE)
      
      # Define o nome do arquivo SHP dentro do zip
      file_name <- paste0(input$basin_output_select, ".shp")
      
      # Salva o shapefile
      st_write(selected_sf, 
               file.path(temp_dir_download, file_name), 
               driver = "ESRI Shapefile",
               delete_dsn = TRUE)
      
      # Compacta todos os arquivos do shapefile
      zip_files <- list.files(temp_dir_download, pattern = paste0(input$basin_output_select, "\\."), full.names = TRUE)
      zip::zip(file, files = zip_files)
    }
  )
}

# Roda a aplicação
shinyApp(ui = ui, server = server)