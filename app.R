# Certifique-se de ter os pacotes instalados:
# install.packages(c("shiny", "sf", "leaflet", "terra", "digeoqR", "zip"))

library(shiny)
library(sf)
library(leaflet)
library(digeoqR)
library(terra)
library(zip)

# Define a UI com o painel de abas
ui <- shiny::fluidPage(
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
          h4("Baixar Arquivos"),
          downloadButton("download_uploaded_data", "Baixar Arquivos Processados (zip)")
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
  dem_raster <- shiny::reactiveVal(NULL)
  basins_model <- shiny::reactiveVal(NULL)
  basins_plan <- shiny::reactiveVal(NULL)
  dem_running <- shiny::reactiveVal(FALSE)
  
  # Função reativa para ler qualquer shapefile de forma segura
  read_shapefile <- shiny::reactive({
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
  stations_sf <- shiny::reactive({ read_shapefile()(input$file_stations) })
  rivers_sf <- shiny::reactive({ read_shapefile()(input$file_rivers) })
  waterbodies_sf <- shiny::reactive({ read_shapefile()(input$file_waterbodies) })
  area_sf <- shiny::reactive({ read_shapefile()(input$file_area) })
  limite_srtm_sf <- shiny::reactive({ read_shapefile()(input$file_limite_srtm) })
  
  # Renderiza o mapa com todas as camadas carregadas
  output$map_layers <- renderLeaflet({
      # Cria um vetor de grupos de camadas
  base_groups <- c()
  overlay_groups <- c()
  if (!is.null(area_sf()) && nrow(area_sf()) > 0) {

    m <- leaflet() %>% addTiles()
    if (!is.null(area_sf()) && nrow(area_sf()) > 0) {
      m <- m %>% addPolygons(data = area_sf(), color = "black", weight = 2, fillOpacity = 0, group = "Limite da Área")
    overlay_groups <- c(overlay_groups, "Limite da Área")
    }
    if (!is.null(limite_srtm_sf()) && nrow(limite_srtm_sf()) > 0) {
      m <- m %>% addPolygons(data = limite_srtm_sf(), color = "black", weight = 2, fillOpacity = 0, group = "Limite SRTM")
    overlay_groups <- c(overlay_groups, "Limite SRTM")
    }
    
    if (!is.null(rivers_sf()) && nrow(rivers_sf()) > 0) {
      m <- m %>% addPolylines(data = rivers_sf(), color = "blue", weight = 2, group = "Rios")
    overlay_groups <- c(overlay_groups, "Rios")  
    }
    if (!is.null(waterbodies_sf()) && nrow(waterbodies_sf()) > 0) {
      m <- m %>% addPolygons(data = waterbodies_sf(), color = "cyan", fillOpacity = 0.5, group = "Massas d'Água")
    overlay_groups <- c(overlay_groups, "Massas d'Água")
    }
    if (!is.null(stations_sf()) && nrow(stations_sf()) > 0) {
      stations_points <- st_centroid(stations_sf())
      m <- m %>% addCircleMarkers(data = stations_points, popup = ~as.character(stations_points[[1]]), 
                                  radius = 5, color = "red", fillOpacity = 0.8, group = "Estações")
    
    overlay_groups <- c(overlay_groups, "Estações")
    }
    # Adiciona a camada de bacias se ela existir
  if (!is.null(basins_model())) {
    bacias_sf <- basins_model()$bacias
    if (!is.null(bacias_sf) && nrow(bacias_sf) > 0) {
      m <- m %>% addPolygons(data = bacias_sf, color = "black", weight = 2, fillOpacity = 0, group = "Bacias Modeladas")
    overlay_groups <- c(overlay_groups, "Bacias Modeladas")
    }
  }
    # Adiciona a camada de bacias se ela existir
  if (!is.null(basins_plan())) {
    bacias_sf <- basins_plan()$'bacias plan'
    if (!is.null(bacias_sf) && nrow(bacias_sf) > 0) {
      m <- m %>% addPolygons(data = bacias_sf, color = "black", weight = 2, fillOpacity = 0, group = "Bacias Planejadas")
    overlay_groups <- c(overlay_groups, "Bacias Planejadas")
    }
  }
    # Adiciona a camada de rios modelados se ela existir
      if (!is.null(basins_model())) {
       stream_order_sf <- basins_model()$'stream strahler' 
      m <- m %>% addPolylines(data = stream_order_sf, color = "blue", weight = 2, group = "Rios modelados")
    overlay_groups <- c(overlay_groups, "Rios modelados")
      }
        # Adiciona a camada de rios modelados se ela existir
      if (!is.null(basins_plan())) {
       stream_order_sf <- basins_plan()$'stream strahler' 
      m <- m %>% addPolylines(data = stream_order_sf, color = "blue", weight = 2, group = "Rios modelados planejados")
    overlay_groups <- c(overlay_groups, "Rios modelados planejados")
      }

    # Adiciona a camada de estações deslocadas se ela existir
      if (!is.null(basins_model())) {
       estacoes_deslocadas_sf <- st_centroid(basins_model()$'estacoes deslocadas')
      m <- m %>% addCircleMarkers(data = estacoes_deslocadas_sf, popup = ~as.character(estacoes_deslocadas_sf[[1]]), 
                                  radius = 2, color = "black", fillOpacity = 0.8, group = "Estações Deslocadas")
    overlay_groups <- c(overlay_groups, "Estações Deslocadas")
      }
    # Adiciona a camada de estações deslocadas se ela existir
      if (!is.null(basins_plan())) {
       estacoes_deslocadas_sf <- st_centroid(basins_plan()$'estacoes plan')
      m <- m %>% addCircleMarkers(data = estacoes_deslocadas_sf, popup = ~as.character(estacoes_deslocadas_sf[[1]]), 
                                  radius = 2, color = "black", fillOpacity = 0.8, group = "Estações Planejadas")
    overlay_groups <- c(overlay_groups, "Estações Planejadas")
      }

    # Adiciona o controle de camadas no final
  m %>% addLayersControl(
    overlayGroups = overlay_groups,
    options = layersControlOptions(collapsed = TRUE)
  )
  } 
  })
  
  # Lógica para renderizar a UI de processamento dinamicamente
  output$processing_ui <- shiny::renderUI({
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
      )
    } else if (input$processing_type == "plan_stations") {
      tagList(
      h4("Ferramenta: Planejar Estações e Bacias"),
        p("Esta função irá planejar as estações e suas bacias de captação."),
        numericInput("threshold", "Threshold", value = 250),
        numericInput("min_length", "Min Length", value = 0.02),
        numericInput("dist_buffer", "Dist Buffer", value = 0.01/10),
        numericInput("classe_am", "Class Sample", value = 2),
        numericInput("tipo", "Process type", value = 1),
        numericInput("funcao_snap", "Snap function", value = 1),
        numericInput("snap_dist", "Snap Distance", value = 0.02),
        numericInput("max_ordem", "Maximum Order", value = 4),
        hr(),
        actionButton("generate_plan", "Gerar Pontos de Planejamento")
      )
    }
  })
  # Lógica para rodar o processamento do DEM
  shiny::observeEvent(input$run_gera_dem, {
    
    if (dem_running()) {
      return(NULL)
    }
    dem_running(TRUE)
    
    showNotification("Iniciando a preparação do DEM. Por favor, aguarde...", duration = NULL, type = "message", id = "dem_prep_notification")
    
    # Garante que o shapefile de limite foi carregado e pega o nome do arquivo
    req(input$file_limite_srtm)
    
    file_name <- input$file_limite_srtm$name[grep(".shp$", input$file_limite_srtm$name, ignore.case = TRUE)]
   
    # --------------------------------
    
    # Define o diretório de saída para o DEM
    dir_out_dem <- file.path(tempdir(), "dem_out")
    dir.create(dir_out_dem, showWarnings = FALSE)
    
    dem_result <- tryCatch({
      # Chama a função prepara_dem
      digeoqR::prepara_dem(
        dir_in = paste0(file.path(tempdir(), "data_in"), "/"), 
        limite_srtm = file_name, # Passa apenas o NOME do arquivo
        dir_out = paste0(dir_out_dem,"/"), 
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
  
  shiny::observeEvent(input$run_basins_model, {
    showNotification("Iniciando a modelagem das bacias. Por favor, aguarde...", duration = NULL, type = "message", id = "basin_prep_notification")
    
    req(dem_raster())
    req(area_sf(), limite_srtm_sf(), rivers_sf(), waterbodies_sf(), stations_sf())
    
    # Define o diretório de dados de entrada
    dir_in <- file.path(tempdir(), "data_in")
    dir.create(dir_in, showWarnings = FALSE)
    
    # Define o diretório de saída da modelagem das bacias
    dir_out_bacias <- file.path(tempdir(), "bacias_out")
    dir.create(dir_out_bacias, showWarnings = FALSE)
    
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
    
    # Modela o terreno
    terreno_model <- tryCatch({
      digeoqR::modela_terreno(
        dem = dem_raster(), 
        dir_out = paste0(dir_out_bacias, "/"), 
        bases_model = bases_model, 
        modo_excluir = FALSE, 
        EPSG = 4674, 
        threshold = input$threshold, 
        min_length = input$min_length, 
        dist_buffer = input$dist_buffer, 
        wbt_wd = paste0(dir_out_bacias, "/"), 
        gera_estacoes = FALSE
      )
    }, error = function(e) {
      showNotification(paste("Erro em modela_terreno:", e$message), type = "error")
      return(NULL)
    })
    
    req(terreno_model)
    # Modela as bacias
    bacias_model <- tryCatch({digeoqR::modela_bacias(fase = 2, EPSG = 4674, 
                                                     dem = dem_raster(), 
                           bases_model = bases_model, 
                           terreno = terreno_model, 
                           classe_am = input$classe_am,fonte_shp = TRUE, 
                           arquivo_shp = paste0(dir_in, "/", file_name5), 
                           tipo = input$tipo, 
                           funcao_snap = input$funcao_snap, 
                           snap_dist = input$snap_dist, 
                           max_ordem = input$max_ordem, 
                           dir_out = paste0(dir_out_bacias, "/"), 
                           wbt_wd = paste0(dir_out_bacias, "/"))}, error = function(e) {
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
  
    
  shiny::observeEvent(input$generate_plan, {
    showNotification("Iniciando a modelagem das bacias. Por favor, aguarde...", duration = NULL, type = "message", id = "basin_prep_notification")
    
    req(dem_raster())
    req(area_sf(), limite_srtm_sf(), rivers_sf(), waterbodies_sf(), stations_sf())
    
    # Define o diretório de dados de entrada
    dir_in <- file.path(tempdir(), "data_in")
    dir.create(dir_in, showWarnings = FALSE)
    
    # Define o diretório de saída da modelagem das bacias
    dir_out_bacias <- file.path(tempdir(), "bacias_out")
    dir.create(dir_out_bacias, showWarnings = FALSE)
    
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
    
    # Modela o terreno
    terreno_model <- tryCatch({
      digeoqR::modela_terreno(
        dem = dem_raster(), 
        dir_out = paste0(dir_out_bacias, "/"), 
        bases_model = bases_model, 
        modo_excluir = FALSE, 
        EPSG = 4674, 
        threshold = input$threshold, 
        min_length = input$min_length, 
        dist_buffer = input$dist_buffer, 
        wbt_wd = paste0(dir_out_bacias, "/"), 
        gera_estacoes = TRUE
      )
    }, error = function(e) {
      showNotification(paste("Erro em modela_terreno:", e$message), type = "error")
      return(NULL)
    })
    
    req(terreno_model)
    # Modela as bacias
    bacias_plan <- tryCatch({digeoqR::modela_bacias(fase = 1, EPSG = 4674, 
                                                     dem = dem_raster(), 
                           bases_model = bases_model, 
                           terreno = terreno_model, 
                           classe_am = input$classe_am,fonte_shp = TRUE, 
                           arquivo_shp = paste0(dir_in, "/", file_name5), 
                           tipo = input$tipo, 
                           funcao_snap = input$funcao_snap, 
                           snap_dist = input$snap_dist, 
                           max_ordem = input$max_ordem, 
                           dir_out = paste0(dir_out_bacias, "/"), 
                           wbt_wd = paste0(dir_out_bacias, "/"))}, error = function(e) {
                             showNotification(paste("Erro durante a modelagem das bacias:", e$message), type = "error")
                             return(NULL)
                           }, finally = {
                             # 4. Remove a notificação de "iniciando"
                             removeNotification(id = "basin_prep_notification")
                           })
           showNotification("Análise de bacias concluída com sucesso!", type = "message")
           req(bacias_plan)
           basins_plan(bacias_plan)
           
           })
  
         
  # Ajuste o seu downloadHandler para usar o argumento 'root'
  output$download_uploaded_data <- shiny::downloadHandler(
    filename = function() {
      paste0("dados_processados-", Sys.Date(), ".zip")
    },
    content = function(file) {
      # req(input$file_area, input$file_stations, input$file_rivers, input$file_waterbodies, input$file_limite_srtm)
      
      dir_out_bacias <- file.path(tempdir(), "bacias_out")
      
      if (!dir.exists(dir_out_bacias) || length(list.files(dir_out_bacias)) == 0) {
        showNotification("Não há arquivos para baixar. Por favor, carregue as camadas primeiro.", type = "warning")
        return(NULL)
      }
      
      # -----------------------------
      # Mudança aqui:
      # Use o argumento 'root' para remover o caminho completo com C:
      
      # Obtém apenas os nomes dos arquivos, sem o caminho completo
      files_to_zip <- list.files(dir_out_bacias, full.names = FALSE)
      
      # Adicione a checagem de arquivos
      if (length(files_to_zip) == 0) {
        showNotification("Não há arquivos para baixar no diretório.", type = "warning")
        return(NULL)
      }
      
      tryCatch({
        # Compacta os arquivos, mas a partir do diretório raiz
        zip::zip(file, files = files_to_zip, root = dir_out_bacias)
      }, error = function(e) {
        showNotification(paste("Erro ao criar o arquivo ZIP:", e$message), type = "error")
      })
    }
  )

}

# Roda a aplicação
shiny::shinyApp(ui = ui, server = server)