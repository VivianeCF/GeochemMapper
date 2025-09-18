GeochemMapper é um aplicativo shiny que possibilita a análise espacial das estações de coleta em rios brasileiros, com foco em dados geoquímicos. É possivel escolher o tipo de processamento das bacias, modelando as bacias a partir das estações coletadas ou gerando as estações para o planejamento da coleta de amostras. O aplicativo também permite a visualização dos dados em mapas interativos, facilitando a interpretação e análise dos resultados.

## Instalação
Para instalar o GeochemMapper, você pode usar o seguinte comando no R:

```R
install.packages("devtools") # Caso ainda não tenha o devtools instalado
devtools::install_github("vivianeCF/GeochemMapper")   
```
## Uso
Para iniciar o aplicativo, basta carregar a biblioteca e executar a função `runApp()`:

```R
library(GeochemMapper)
runApp()
```

## Dependências
O GeochemMapper depende dos seguintes pacotes R:
- shiny
- leaflet
- sf    
- dplyr 
- ggplot2
- terra
- stars
- whitebox
- ggspatial
- digeoqR
- raster
- elevatr

## Contribuição
Contribuições são bem-vindas! Sinta-se à vontade para abrir issues ou pull requests no repositório do GitHub.
## Licença
Este projeto está licenciado sob a licença MIT. Veja o arquivo LICENSE para mais detalhes.
## Autor
Viviane C. Ferrari - [GitHub](#autor) and [LinkedIn](https://www.linkedin.com/in/vivianecferrari/)
## Agradecimentos
Agradecemos a todos que contribuíram para o desenvolvimento deste aplicativo.
