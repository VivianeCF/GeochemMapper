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
      
      # Verifica se a camada está no CRS 4326, se não, transforma
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
      m <- m %>% addCircleMarkers(data = stations_sf(), popup = ~as.character(stations_sf()[[1]]),
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
        actionButton("run_gera_dem", "Preparar DEM"),
        hr(),
        downloadButton("download_dem", "Baixar DEM")
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
        actionButton("run_basins_model", "Rodar Análise de Bacias")
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
  
  # Lógica para rodar o processamento do DEM
  observeEvent(input$run_gera_dem, {
    
    if (dem_running()) {
      return(NULL)
    }
    dem_running(TRUE)
    
    showNotification("Iniciando a preparação do DEM. Por favor, aguarde...", duration = NULL, type = "message", id = "dem_prep_notification")
    
    # Garante que o shapefile de limite foi carregado
    req(input$file_limite_srtm)
    
    # Define o diretório de entrada temporário
    dir_in <- file.path(tempdir(), "data_in")
    
    # Pega o nome do arquivo .shp do upload
    file_name <- input$file_limite_srtm$name[grep(".shp$", input$file_limite_srtm$name, ignore.case = TRUE)]
    
    # Verifica se o arquivo existe antes de prosseguir
    if (!file.exists(file.path(dir_in, file_name))) {
      showNotification("Erro: Arquivo de limite SRTM não encontrado no diretório temporário.", type = "error")
      dem_running(FALSE)
      return(NULL)
    }
    
    # Define o diretório de saída para o DEM
    dir_out_dem <- file.path(tempdir(), "dem_out")
    dir.create(dir_out_dem, showWarnings = FALSE)
    
    dem_result <- tryCatch({
      # CORREÇÃO: Passe o diretório de entrada e o nome do arquivo separadamente
      digeoqR::prepara_dem(
        dir_in = paste0(dir_in, "/"),
        limite_srtm = file_name,
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
  
  # Lógica para rodar o processamento de Bacias
  observeEvent(input$run_basins_model, {
    showNotification("Iniciando a modelagem das bacias. Por favor, aguarde...",
                     duration = NULL, type = "message", id = "basin_prep_notification")
    
    # Garante que todos os dados de entrada necessários foram carregados
    req(dem_raster(), area_sf(), limite_srtm_sf(), rivers_sf(), waterbodies_sf(), stations_sf())
    
    # Define o diretório de entrada temporário
    dir_in <- file.path(tempdir(), "data_in")
    dir_out_basins <- file.path(tempdir(), "bacias_modeladas")
    dir.create(dir_out_basins, showWarnings = FALSE)
    
    # Constrói os caminhos completos
    file_path1 <- file.path(dir_in, input$file_area$name[grep(".shp$", input$file_area$name, ignore.case = TRUE)])
    file_path2 <- file.path(dir_in, input$file_limite_srtm$name[grep(".shp$", input$file_limite_srtm$name, ignore.case = TRUE)])
    file_path3 <- file.path(dir_in, input$file_rivers$name[grep(".shp$", input$file_rivers$name, ignore.case = TRUE)])
    file_path4 <- file.path(dir_in, input$file_waterbodies$name[grep(".shp$", input$file_waterbodies$name, ignore.case = TRUE)])
    file_path5 <- file.path(dir_in, input$file_stations$name[grep(".shp$", input$file_stations$name, ignore.case = TRUE)])
    
    # Executa o processamento
    result <- tryCatch({
      # Usa os caminhos completos para os argumentos
      bases_model <- digeoqR::gera_bases_model(
        limite = file_path1,
        limite_srtm = file_path2,
        rios = file_path3,
        massa_dagua = file_path4,
        modo_excluir = FALSE
      )
      req(bases_model)
      
      terreno_model <- digeoqR::modela_terreno(
        dem = dem_raster(),
        dir_out = dir_out_basins,
        bases_model = bases_model,
        modo_excluir = FALSE,
        EPSG = 4674,
        threshold = input$threshold,
        min_length = input$min_length,
        dist_buffer = input$dist_buffer,
        wbt_wd = dir_in,
        gera_estacoes = FALSE
      )
      req(terreno_model)
      
      bacias_finais <- digeoqR::modela_bacias(
        fase = 2,
        EPSG = 4674,
        dem = dem_raster(),
        bases_model = bases_model,
        terreno = terreno_model,
        classe_am = input$classe_am,
        fonte_shp = TRUE,
        arquivo_shp = file_path5,
        tipo = input$tipo,
        funcao_snap = input$funcao_snap,
        snap_dist = input$snap_dist,
        max_ordem = input$max_ordem,
        dir_out = dir_out_basins,
        wbt_wd = dir_in
      )
      return(bacias_finais)
      
    }, error = function(e) {
      showNotification(paste("Erro durante a modelagem das bacias:", e$message), type = "error", duration = NULL)
      return(NULL)
    }, finally = {
      removeNotification(id = "basin_prep_notification")
    })
    
    req(result)
    basins_model(result)
    showNotification("Análise de bacias concluída com sucesso!", type = "message")
  })  
  # Lógica para o botão de download do DEM
  output$download_dem <- downloadHandler(
    filename = function() {
      # Garante que o raster do DEM esteja disponível antes de criar o nome do arquivo
      req(dem_raster())
      paste0("dem_preparado-", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Garante que o raster do DEM esteja disponível antes de tentar o download
      req(dem_raster())
      
      # Cria um diretório temporário para o arquivo
      temp_dir_dem <- file.path(tempdir(), "dem_out")
      dir.create(temp_dir_dem, showWarnings = FALSE)
      
      # Define o caminho completo do arquivo
      dem_path <- file.path(temp_dir_dem, "dem.tif")
      
      # Salva o raster como um arquivo temporário
      # Adicione a checagem de erros para garantir que o arquivo foi salvo
      tryCatch({
        terra::writeRaster(dem_raster(), dem_path, overwrite = TRUE)
      }, error = function(e) {
        stop(paste("Erro ao salvar o arquivo raster:", e$message))
      })
      
      # Verifica se o arquivo foi realmente criado antes de tentar compactar
      if (!file.exists(dem_path)) {
        stop("O arquivo do DEM não foi criado corretamente.")
      }
      
      # Cria o arquivo zip com o arquivo .tif
      zip::zip(file, files = dem_path)
    }
  )
}

# Roda a aplicação
shinyApp(ui = ui, server = server)