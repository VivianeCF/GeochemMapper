
#'Gera estações nas confluências dos rios
#'
#'Desenvolvido para automatizar o processo de escolha do locais de amostragem
#'que devem ser a 100 m acima dos pontos de confluência dos rios. Também são
#'excluídos locais nas áreas urbanas e de massa d'água. Todos os arquivos devem
#'ser no formato shapefile.
#'
#'@param dem Modeolo digital do terreno (SRTM)
#'@param EPSG Sistema de coordenadas
#'@param threshold Valor de corte da área das bacias modeladas
#'@param min_length Comprimento mínimo do curso do rio
#'@param bases_model Bases definidas na função gera_bases_model
#'@param dist_buffer
#'@param dir_out Diretório de saída
#'@param wbt_wd
#'@param gera_estacoes TRUE ou FALSE
#'
#'@return Imagem (.png) do mapa de planejamento preliminar.
#'Arquivo da shape (shp) das estações.
#'@export
#'
#' @examples
#' #modela_terreno()
#'
modela_terreno <-
  function(dem, dir_out = "outputs/",
           bases_model,
           EPSG = 4326,
           threshold = 250,
           min_length = "0.02",
           dist_buffer = 0.01/10,
           wbt_wd = "outputs/modelo/",
           gera_estacoes=TRUE)
  {
### GERA DRENAGENS--------------------------------------------------------------
    # Cria objeto lista para a saída
    out <- list()
    # wbt_wd <- tempfile()
    # dir.create(wbt_wd)
    # Salva srtm no diretório escolhido
    terra::writeRaster(dem, paste0(wbt_wd, "srtm.tif"), overwrite=TRUE)

    sf::write_sf(bases_model[["rios"]], paste0(wbt_wd,"\\","rios.shp"),
                 delete_layer = TRUE )
    options("rgdal_show_exportToProj4_warnings" = "none")

    whitebox::wbt_rasterize_streams(
      "rios.shp",
      base = "srtm.tif",
      output = "network_topage.tif",
      nodata = 0,
      wd = wbt_wd
    )

    # Queime a rede fluvial no DEM
    # Vamos negligenciar o efeito dos aterros rodoviários no DEM de 30m
    # criando um shapefile vazio para as estradas
    sf::write_sf(
      sf::st_sfc(sf::st_multilinestring(), crs = EPSG),
      file.path(wbt_wd, "roads.shp"),
      delete_layer = TRUE,
      quiet = TRUE
    )

    # whitebox::wbt_burn_streams_at_roads(
    #   dem = "srtm.tif",
    #   streams = "rios.shp",
    #   roads = "roads.shp",
    #   output = "dem_100m_burn.tif",
    #   wd = wbt_wd
    # )
    whitebox::wbt_fill_burn(dem = "srtm.tif", streams = "rios.shp",
                            output = "dem_100m_burn.tif", wd = wbt_wd)
    # Remove as depressões no DEM
    whitebox::wbt_fill_depressions(dem = "dem_100m_burn.tif",
                                   output = "dem_fill.tif",
                                   wd = wbt_wd)

    # Atribui nome ao arquivo ponteiro d8
    output_d8_pntr <- "d8.tif"

    # Raster de direção do fluxo
    whitebox::wbt_d8_pointer(dem = "dem_fill.tif",
                             output =  output_d8_pntr,
                             wd = wbt_wd)

    # Computa o acúmulo de fluxo
    whitebox::wbt_d8_flow_accumulation(
      input =  output_d8_pntr,
      pntr = TRUE,
      output = "facc.tif",
      wd = wbt_wd
    )


    # Extrai uma rede de fluxo (limite = número de células) consistente
    # com a direção do fluxo
    whitebox::wbt_extract_streams(
      flow_accum =  "facc.tif",
      threshold = threshold,
      # 100 células = 1 km2
      output = "network_1km2.tif",
      wd = wbt_wd)
    # Remove pequenos trechos da rede fluvial
    whitebox::wbt_remove_short_streams(d8_pntr = output_d8_pntr,
                                       streams = "network_1km2.tif",
                                       output = "network_d8.tif",
                                       min_length = min_length, wd = wbt_wd,)
    # Converte a rede fluvial de raster para vetor
    whitebox::wbt_raster_streams_to_vector("network_d8.tif",
                                           output_d8_pntr,
                                           output = "network_d8.shp",
                                           wd = wbt_wd)
    stream_model <- sf::read_sf(file.path(wbt_wd, "network_d8.shp"))


    # Estabelece o sistema de coordenadas
    sf::st_crs(stream_model) <- EPSG

    # Gera drenagem classificada strahler
    output_order <- "strahler_order.tif"
    whitebox::wbt_strahler_stream_order(output_d8_pntr,
                                        "network_d8.tif",
                                        output_order,
                                        wd = wbt_wd)

    # Atribui nome do arquivo da drenagem ordenada
    output_drenagem_ord <- "stream_strahler.shp"

    # Converte a drenagem odenada de raster para vetor
    whitebox::wbt_raster_streams_to_vector(output_order,
                                           output_d8_pntr,
                                           output_drenagem_ord,
                                           wd = wbt_wd)

     # Lê arquivo da drenagem gerado
    stream_strahler <-
      sf::read_sf(file.path(wbt_wd, "stream_strahler.shp"))

    # Projeta para o sistema de coordenadas escolhido
    sf::st_crs(stream_strahler) <- EPSG

    # Salva arquivo da drenagem ordenada na pasta escolhida
    sf::write_sf(stream_strahler,
                 paste0(dir_out, "stream_strahler.shp"),
                 delete_layer = TRUE)
    out[[1]] <- stream_strahler
    # Salva arquivo da drenagem modelada na pasta escolhida
    sf::write_sf(stream_model, paste0(dir_out, "stream_model.shp"),
                 delete_layer = TRUE)
    out[[2]] <- stream_model
 if(gera_estacoes == TRUE){
    ## Gera ESTAÇÕES -----------------------------------------------------------
    ## Lê limite da area do projeto
    area <- bases_model[["limite da área folha"]]
    ## Lê massa d'água
    massa_dagua <- bases_model[["massa de água"]]

    ## Lê área urbana
    area_urbana <- bases_model[["área urbana"]]

    ## Lê área urbana
    pantanal <- bases_model[["pantanal"]]

    ## Lê área urbana
    terra_indigena <- bases_model[["terra indígena"]]

    # ## Lê área urbana
    # unidade_protecao_ambiental <- bases_model[["unidade de protecao ambiental"]]

    ## Cria area impeditiva
    area_impeditiva <- sf::st_union(massa_dagua, area_urbana, pantanal,terra_indigena)

    ## Encontra vértices das junções

    # Simplify river shapefile
    shape_river_simple <- stream_model %>%
      sf::st_as_sf() %>%
      suppressMessages({
        sf::st_union()
      })

    # Converte shapefile em objeto ponto - suprime mensagens de alerta
    river_to_points <- suppressWarnings({
      shape_river_simple %>%
        sf::st_as_sf() %>%
        sf::st_cast("POINT") %>%
        dplyr::mutate(id = 1:nrow(.))
    })

    # Procura pontos que estão sobrepostos
    joins_selection <- river_to_points %>%
      sf::st_equals() %>%
      Filter(function(x) {
        length(x) > 2
      }, .) %>%
      lapply(., FUN = min) %>%
      unlist() %>%
      unique()

    # Filtra shapefile do ponto original para obter só as confluências
    river_joins <- river_to_points %>%
      dplyr::filter(id %in% joins_selection)

    ## Criar buffer circular das junções (100 metros de raio)
    joins_area <-
      suppressMessages({
        suppressWarnings({
          sf::st_buffer(river_joins, dist = dist_buffer)
        })
      })

    ## Remove área de sobreposição do buffer dos rios - suprime alertas
    linhas <- stream_model  %>% sf::st_transform(EPSG)
    outlet <- joins_area  %>% sf::st_transform(EPSG)
    suppressMessages({sf::sf_use_s2(FALSE)})
    res_ls <-
      suppressWarnings({
        suppressMessages({
          sf::st_difference(linhas, sf::st_union(outlet))
        })
      })
    # Acha os pontos finais dos trechos de drenagem resultantes
    ep <- lwgeom::st_endpoint(res_ls) # lista todos os pontos
    p <- as.data.frame(do.call("rbind", ep)) # convrte lista em data frame

    # Converte tabela de coordenadas em dados espaciais
    pontos_originais <-
      sf::st_as_sf(p, coords = c("V1", "V2"), crs = EPSG)

    ## Pontos selecionados pelo filtro de localização --------------------------
    ## Extrair pontos dentro da carta 100 mil
    area <- sf::st_transform(area, EPSG)
    pontos_area <-
      suppressMessages({
        suppressWarnings({
          sf::st_intersection(pontos_originais, area)
        })
      })
    pontos_area$id <- seq(1, nrow(pontos_area))

    ## Remover pontos dentro das áreas alagadas
    massa_dagua <- sf::st_transform(massa_dagua, EPSG)
    pontos_area_remover1 <-
      suppressMessages({
        suppressWarnings({
          sf::st_intersection(pontos_area, massa_dagua)
        })
      })
    ## Remover pontos dentro de áreas urbanas
    area_impeditiva <- sf::st_transform(area_impeditiva, EPSG)
    pontos_area_remover2 <-
      suppressMessages({
        suppressWarnings({
          sf::st_intersection(pontos_area, area_impeditiva)
        })
      })

    if ((nrow(pontos_area_remover2) + nrow(pontos_area_remover1)) > 0) {
      pontos_remover <- dplyr::bind_rows(pontos_area_remover1, pontos_area_remover2)
      pontos_area <-
        pontos_area[!(pontos_area$id %in% pontos_remover$id),]
    }

    # Mapa
    m <- ggplot2::ggplot() +
      # ggplot2::coord_fixed() +
      ggspatial::layer_spatial(res_ls) +
      ggspatial::layer_spatial(area, fill = NA, lwd = 0.6) +
      ggspatial::layer_spatial(pontos_area, color = "red") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom") +
      ggspatial::annotation_scale(location = "bl", style = "ticks") +
      ggspatial::annotation_north_arrow(location = "br") +
      ggplot2::labs(caption = paste0(
        "* Pontos vermelhos = pontos de coleta planejados.",
        " N= ",
        nrow(pontos_area)
      ))
    out[[3]] <- m
    png(
      paste0(dir_out, "mapa_plan_ini.png"),
      units = "cm",
      width = 16,
      height = 16,
      res = 300
    )
    print(m)
    dev.off()
    # ## Salvar dados espaciais gerados ----------------------------------------
    pontos_area$id <- 1:nrow(pontos_area)
    pontos_area <- pontos_area[, "id"]
    out[[4]] <- pontos_area
    sf::write_sf(pontos_area,
                 paste0(dir_out, "estacoes_geradas.shp"),
                 delete_layer = TRUE)
    out[[5]] <- wbt_wd
    names(out) <- c("stream sthraler", "stream model",
                    "mapa", "estacoes geradas")
 } else {

   names(out) <- c("stream sthraler", "stream model")
}

    return(out)
  }
