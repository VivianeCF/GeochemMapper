#'Prepara a base de dados para o processamento dos dados geoquímicos
#'
#'Recupera e une os dados dos boletins analíticos e os dados de campo da base de
#'dados fcampo. Os dados precisam estar nos diretórios gerados pela função
#'estrutura_diretorio. Os boletins analíticos devem estar no formato .csv com
#'layout modelo nos diretórios inputs/mineral ou inputs/quimica, conforme o
#'exemplo.
#'
#' @param analise Selecione o tipo de análise: 1 = mineral ou 2 = química
#' @param dir_bol Diretório dos boletins analíticos ex: "inputs/qumica/R/"
#' @param classe_am Classe da amostra: 1 = concentrado de bateia, 2 = sedimento de
#'   corrente, 3 = rocha, 4 = solo, 5 = água#'
#' @param quimica
#' @param mineral
#' @param ex_campo

#'@return Retorna a lista com as bases de dados em diferentes formatos
#'  transformados ou brutos, pivotados ou não.
#'@export
#' @examples
#'
prepara_bases <- function(dir_bol, classe_am, analise, quimica, mineral,
                          ex_campo) {
  out <- list()
  a <- c("mineral", "química")
  t <-
    c("CONCENTRADO DE BATEIA",
      "SEDIMENTO CORRENTE",
      "SOLO",
      "ROCHA",
      "ÁGUA")
  abrev <- c("cb", "sc", "solo", "rocha", "agua")
  nm_classe <- c("Concentrado de bateia",
                 "Sedimento de corrente",
                 "Solo",
                 "Rocha",
                 "Água")
  r <-  EPSG
  if (analise == 2) {
    ex_campo
    dados_campo <- ex_campo[[1]]
    dados_campo <- dados_campo[dados_campo$CLASSE ==  nm_classe[classe_am],]
    VALUE <- 1:nrow(dados_campo)
    dados_campo <- data.frame(VALUE, dados_campo)

    data_dup <-  dados_campo[duplicated(c( dados_campo$LONG_DEC,  dados_campo$LAT_DEC)), ]
    data_dup <- data_dup[!is.na(data_dup$N_LAB),]
    data_dup <- data_dup[!is.na(data_dup$N_LAB),]
    dup <- rep("DUP", nrow(data_dup))
    data_dup <- data.frame(COD_SMP = dup, data_dup)
    data_smp <- unique(dados_campo[!duplicated(c(dados_campo$LONG_DEC, dados_campo$LAT_DEC)), ])
    data_smp <- data_smp[!is.na(data_smp$N_LAB),]
    smp <- rep("SMP", nrow(data_smp))
    data_smp <- data.frame(COD_SMP = smp, data_smp)
    data_campo <- rbind(data_smp, data_dup)
    data_campo <- data_campo[order(data_campo$VALUE),]
    data_campo <- data_campo %>% dplyr::relocate(COD_SMP, .after = NUM_CAMPO)

    # Base não pivotada
    out[[1]] <- dplyr::right_join(dados_campo,
                                  quimica$`dados brutos`, by = "N_LAB")
    out[[2]] <- dplyr::right_join(dados_campo,
                                  quimica$`dados transformados`, by = "N_LAB")
    # Base pivotada
    out[[3]] <- dplyr::right_join(dados_campo,
                                  quimica$`dados brutos pivotados`, by = "N_LAB")
    out[[4]] <- dplyr::right_join(dados_campo,
                                  quimica$`dados transformados pivotados`,
                                  by = "N_LAB")
    out[[5]] <- quimica[[7]]

  } else{
    dados_brutos <- mineral$`dados brutos`
    dados_transformados <- mineral$`dados transformados`
     dados_brutos_pivotados <- mineral$`dados brutos pivotados`
    dados_transformados_pivotados <- mineral$`dados transformados pivotados`
    dados_campo <- ex_campo[[1]]
    dados_campo <- dados_campo[dados_campo$CLASSE ==  nm_classe[classe_am],]
    VALUE <- 1:nrow(dados_campo)
    dados_campo <- data.frame(VALUE, dados_campo)

    # Base não pivotada
    out[[1]] <- dplyr::right_join(dados_campo,
                                  dados_brutos, by = "N_LAB")
    out[[2]] <- dplyr::right_join(dados_campo,
                                  dados_transformados, by = "N_LAB")
    # Base pivotada
    out[[3]] <- dplyr::right_join(dados_campo,
                                  dados_brutos_pivotados, by = "N_LAB")
    out[[4]] <- dplyr::right_join(dados_campo,
                                  dados_transformados_pivotados,
                                  by = "N_LAB")
    out[[5]] <- mineral[[7]]

  }




 dados_campo <- dados_campo[dados_campo$CLASSE == nm_classe[classe_am],]
 dados_campo$LONG_DEC <- round(as.numeric(gsub(",", ".", dados_campo$LONG_DEC, fixed = TRUE)), 5)
 dados_campo$LAT_DEC <- round(as.numeric(gsub(",", ".", dados_campo$LAT_DEC, fixed = TRUE)), 5)
 out[[6]] <- dados_campo


 write.csv2(dados_campo, paste0(dir_out, "dados_campo","_", abrev[classe_am], ".csv"), row.names = FALSE)

# Cria dados espaciais
dados_campo_st <-
  sf::st_as_sf(dados_campo,
               coords = c("LONG_DEC", "LAT_DEC"),
               crs = r, remove = FALSE )
sf::st_write(
  dados_campo_st,
  paste0(dir_out, "estacoes_",abrev[classe_am], ".shp"),
  driver = "ESRI Shapefile",
  delete_layer = TRUE
)
 out [[7]] <- dados_campo_st
 out [[8]] <- quimica[[6]]
 names(out) <-  c(
    "dados brutos",
    "dados transformados",
    "dados brutos pivotados",
    "dados transformados pivotados",
    "condições analíticas",
    "dados de campo",
    "dados campo sf",
    "dados qaqc transformados"
  )
  return(out)
}
