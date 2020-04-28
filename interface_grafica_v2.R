library(leaflet)
library(mapview)
library(leaflet.extras)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(rmarkdown)


####################### MAPA DO SUBSISTEMA FERROVIÁRIO FEDERAL #######################

####################### AJUSTES AO SHAPEFILE ####################### 


# Leitura das bases georreferenciadas
## myshp é um shapefile do tipo linestring das linhas do SFF. 
## myshp2 é um shapefile do tipo point das estações cadastradas na Declaração de Rede
myshp <- readOGR(dsn=path.expand("shp_atualizado"),
                 layer="dbo_tblLinhaEstacao_spatial_linestring", stringsAsFactors = FALSE)
myshp2 <- readOGR(dsn=path.expand("shp_atualizado"),
                  layer="dbo_tblEstacao_spatial_point", stringsAsFactors = FALSE, encoding = "UTF-8")

# Substituição do CodigoFerr, antes numérico, agora string, com o nome das estações cadastradas na DR
tblFerrovia <- readxl::read_excel("Dados/tblFerrovia.xlsx")
tblFerrovia$CodigoFerr <- tblFerrovia$CodigoFerrovia
myshp@data <- plyr::join(myshp@data,
                         tblFerrovia,
                         by='CodigoFerr')
myshp2@data <- plyr::join(myshp2@data,
                          tblFerrovia,
                          by='CodigoFerr')

# Substituição do CodigoLi00, antes numérico, agora string, com o nome das linhas cadastradas na DR
tblLinha <- readxl::read_excel("Dados/tblLinha.xlsx")
tblLinha$CodigoLi00 <- tblLinha$CodigoLinha
myshp@data <- plyr::join(myshp@data,
                         tblLinha,
                         by='CodigoLi00')

# Substituição do CodigoEsta, antes numérico, agora string, com o código de tres letras
tblEstacao <- readxl::read_excel("Dados/tblEstacao.xlsx")
tblEstacao$CodigoEsta <- tblEstacao$CodigoEstacao
myshp@data <- plyr::join(myshp@data,
                         tblEstacao,
                         by='CodigoEsta')

# Merge da tabela de atributos do shapefile com a Declaração de Rede 2020
DR_2020 <- readxl::read_excel("Dados/dr2020_original.xlsx")
DR_2020$linesta <- paste(DR_2020$Linha, DR_2020$B, sep='!')
myshp@data$linesta <- paste(myshp@data$NomeLinha, myshp@data$CodigoTresLetrasEstacao, sep='!')
myshp@data <- plyr::join(myshp@data,
                         DR_2020,
                         by='linesta')

# Ajustes à tabela de atributos do shapefile: drop de colunas desnecessárias e 
# alteração dos nomes das variáveis
myshp2@data <- select(myshp2@data, -c(CodigoEsta, CodigoFerr, CodigoMuni, DataExclus,
                                      IndicadorP, CodigoPort, CodigoEsca, CodigoEsca,
                                      CodigoArqu, IndicadorT, IndicadorF,LogotipoFerrovia,
                                      DataExclusao, IndicadorObrigatorioDesempenhoProducao))

myshp2@data <- rename(myshp2@data, "Nome Estação" = NomeEstaca)
myshp2@data <- rename(myshp2@data, "Código Estação" = CodigoTres)
myshp2@data <- rename(myshp2@data, "Ferrovia" = NomeFerrovia)

## Composição do nome completo das estações: Nome da Estação (Código de Três Letras)
df_estac <- tibble(myshp2@data$`Código Estação`, myshp2@data$`Nome Estação`)
colnames(df_estac) <- c('A','NomeA')
myshp@data <- plyr::join(myshp@data,
                         df_estac,
                         by='A')
colnames(df_estac) <- c('B','NomeB')
myshp@data <- plyr::join(myshp@data,
                         df_estac,
                         by='B')

myshp@data$`Nome Estação A` <- paste(myshp@data$NomeA, " (", myshp@data$A,")", sep="")
myshp@data$`Nome Estação B` <- paste(myshp@data$NomeB, " (", myshp@data$B,")", sep="")
myshp2@data$`Código Estação` <- paste(myshp2@data$`Nome Estação`, " (", myshp2@data$`Código Estação`,")", sep="")

myshp@data <- select(myshp@data, -c(B, A, linesta, CodigoEsta, CodigoLi00, CodigoFerr,
                                    CodigoLinh, NumeroSequ, IndicadorC, IndicadorE,
                                    NomeReduzidoFerrovia, LogotipoFerrovia, DataExclusao,
                                    IndicadorObrigatorioDesempenhoProducao, CodigoTresLetrasEstacao,
                                    Ferrovia, CodigoBito, NomeLinha, NomeA, NomeB,
                                    CodigoFerrovia, CodigoLinha, CodigoEstacao))

myshp@data <- rename(myshp@data, "Marco Quilométrico" = NumeroQuil)
myshp@data <- rename(myshp@data, "Extensão do Entre Pátio (km)" = NumeroExte)
myshp@data <- rename(myshp@data, "Ferrovia" = NomeFerrovia)
myshp@data <- rename(myshp@data, "Sigla - Ferrovia" = SiglaFerrovia)
myshp@data <- rename(myshp@data, "Instalada Crescente (Trens/dia)" = "Instalada Crescente")
myshp@data <- rename(myshp@data, "Instalada Decrescente (Trens/dia)" = "Instalada Decrescente")
myshp@data <- rename(myshp@data, "Vinculada Crescente (Trens/dia)" = "Vinculada Crescente")
myshp@data <- rename(myshp@data, "Vinculada Decrescente (Trens/dia)" = "Vinculada Decrescente")

# Cálculo da Ocupação da Capacidade Instalada
myshp@data$OcupaçãoCrescente <- myshp@data$`Vinculada Crescente`/myshp@data$`Instalada Crescente`
myshp@data$OcupaçãoCrescente[is.na(myshp@data$OcupaçãoCrescente)] <- 0
myshp@data$OcupaçãoDecrescente <- myshp@data$`Vinculada Decrescente`/myshp@data$`Instalada Decrescente`
myshp@data$OcupaçãoDecrescente[is.na(myshp@data$OcupaçãoDecrescente)] <- 0

# Criação da Coluna Ocupação, com o maior valor percentual dentre as colunas OcupaçãoCrescente e OcupaçãoDecrescente
l <- vector("list", length(myshp@data$OcupaçãoDecrescente))
for (i in c(1:length(myshp@data$OcupaçãoDecrescente))){
  l[[i]] = max(myshp@data[i,c("OcupaçãoCrescente", "OcupaçãoDecrescente")], na.rm=TRUE)
}
l <- lapply(l, FUN = function(x) ifelse(is.nan(x),NA,x))
l <- lapply(l, FUN = function(x) ifelse(is.infinite(x),NA,x))
myshp@data$Ocupação <- unlist(l)
myshp@data$Ocupação[myshp@data$Ocupação == 0] <- NA
myshp@data$Ocupação <- round(myshp@data$Ocupação, digits=2)
myshp@data$`Instalada Crescente (Trens/dia)` <- round(myshp@data$`Instalada Crescente (Trens/dia)`, digits=2)
myshp@data$`Instalada Decrescente (Trens/dia)` <- round(myshp@data$`Instalada Decrescente (Trens/dia)`, digits=2)
myshp@data$`Vinculada Crescente (Trens/dia)` <- round(myshp@data$`Vinculada Crescente (Trens/dia)`, digits=2)
myshp@data$`Vinculada Decrescente (Trens/dia)` <- round(myshp@data$`Vinculada Decrescente (Trens/dia)`, digits=2)

myshp@data <- myshp@data %>%
  select("Ferrovia", "Sigla - Ferrovia", "Nome Estação A", "Nome Estação B", everything())

myshp@data <- select(myshp@data, -c(OcupaçãoCrescente,
                                    OcupaçãoDecrescente))

# Construção da paleta de cores a ser usada no mapa para representação da Ocupação
red = colorRampPalette(c('green', 'yellow','red'))

# Logo ANTT
img <- "https://upload.wikimedia.org/wikipedia/commons/thumb/2/29/Logo_ANTT.svg/1200px-Logo_ANTT.svg.png"
urlh <- "https://github.com/tovictorino/declaracao_de_rede"

# Criação do mapa
map <- mapview(myshp, zcol="Ocupação",
               legend = TRUE,
               layer.name = '',
               color = red) %>% 
  
  leafem::addLogo(img, width = 120, height = 60, url = "http://www.antt.gov.br/", position="topleft") %>%
  
  addControl("Pesquisa de Estações",
             position = "topleft") %>%
  
  addCircleMarkers(data = myshp2, lng = myshp2@coords[,1],
                   lat=myshp2@coords[,2],
                   popup = ~`Código Estação`,
                   label=~`Código Estação`,
                   group='Estações') %>%
  
  addSearchFeatures(targetGroups = 'Estações',
                    options = searchFeaturesOptions(
                      zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
                      autoCollapse = FALSE, hideMarkerOnCollapse = FALSE)) %>% 
  
  addLayersControl(overlayGroups = c("Estações"),
                   options = layersControlOptions(collapsed = FALSE),
                   position='topleft') %>%
  
  groupOptions("Estações", zoomLevels = 10:30)

mapshot(map, url = "DR_Interface_Grafica.html", selfcontained = FALSE)
