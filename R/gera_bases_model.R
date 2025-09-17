#' Gera bases para a modelagem das bacias e estações
#'
#' Converte as bases de entrada em um alista de dados para serem carregadas nas
#' funções modela_bacias e gera estações.
#'
#' @param dir_in
#' @param limite
#' @param area_urbana
#' @param rios
#' @param massa_dagua
#' @param limite_srtm
#'
#' @return
#' @export
#'
#' @examples
gera_bases_model <- function(dir_in = "inputs/campo/",
                             limite = "carta_100M.shp",
                             limite_srtm = "area_srtm.shp",
                             area_urbana = "area_urbana.shp",
                             rios = "rios_ibge.shp",
                             massa_dagua = "massa_dagua.shp",
                             pantanal = "pantanal.shp",
                             terra_indigena = "terra_indigena.shp"

                             )
{
  out <- list()

  out[[1]] <- sf::read_sf(paste0(dir_in, limite_srtm))
  out[[2]] <- sf::read_sf(paste0(dir_in, limite))
  out[[3]] <- sf::read_sf(paste0(dir_in, area_urbana))
  out[[4]] <- sf::read_sf(paste0(dir_in, rios))
  out[[5]] <- sf::read_sf(paste0(dir_in, massa_dagua))
  out[[6]] <- sf::read_sf(paste0(dir_in, pantanal))
  out[[7]] <- sf::read_sf(paste0(dir_in, terra_indigena))
   names(out) <-
    c("limite da área srtm", "limite da área folha", "área urbana",
      "rios", "massa de água", "pantanal", "terra indígena")
  return(out)
}
