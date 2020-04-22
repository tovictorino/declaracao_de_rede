library(leaflet)
library(mapview)
library(leaflet.extras)
library(shiny)
library(rgdal)
library(dplyr)
library(png)
library(RColorBrewer)

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

# Substituição do CodigoEsta, antes numérico, agora string, com o código de tres
# letras das estações cadastradas na DR
tblEstacao <- readxl::read_excel("Dados/tblEstacao.xlsx")
tblEstacao$CodigoEsta <- tblEstacao$CodigoEstacao
myshp@data <- plyr::join(myshp@data,
                         tblEstacao,
                         by='CodigoEsta')
DR_2020 <- readxl::read_excel("Dados/dr2020_original.xlsx")
DR_2020$linesta <- paste(DR_2020$Linha, DR_2020$B, sep='!')
myshp@data$linesta <- paste(myshp@data$NomeLinha, myshp@data$CodigoTresLetrasEstacao, sep='!')
myshp@data <- plyr::join(myshp@data,
                         DR_2020,
                         by='linesta')

# Substituição dos códigos de bitola, antes numéricos, agora string, pela sua classificação real
{myshp@data[myshp@data[["CodigoBito"]] == '1', 'CodigoBito'] <- 'Métrica'
  myshp@data[myshp@data[["CodigoBito"]] == '3', 'CodigoBito'] <- 'Larga'
  myshp@data[myshp@data[["CodigoBito"]] == '5', 'CodigoBito'] <- 'Mista'}

# Ajustes à tabela de atributos do shapefile; drop de colunas desnecessárias e 
# alteração dos nomes das variáveis

myshp2@data <- myshp2@data[ , !names(myshp2@data) %in% c("CodigoEsta","CodigoFerr", 
                                                         "CodigoMuni", "DataExclus",
                                                         "IndicadorP", "CodigoPort",
                                                         "CodigoEsca", "CodigoArqu",
                                                         "IndicadorT", "IndicadorF",
                                                         "CodigoFerrovia", "LogotipoFerrovia",
                                                         "DataExclusao", 
                                                         "IndicadorObrigatorioDesempenhoProducao")]

colnames(myshp2@data)[which(names(myshp2@data) == "NomeEstaca")] <- "Nome Estação"
colnames(myshp2@data)[which(names(myshp2@data) == "CodigoTres")] <- "Código Estação"
colnames(myshp2@data)[which(names(myshp2@data) == "NomeFerrovia")] <- "Ferrovia"
colnames(myshp2@data)[which(names(myshp2@data) == "NomeReduzidoFerrovia")] <- "Sigla-Ferrovia"

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

myshp@data <- myshp@data[ , !names(myshp@data) %in% c("CodigoLinh", "CodigoLi00",
                                                      "CodigoEsta", "CodigoBito",
                                                      "NumeroSequ", "IndicadorC",
                                                      "IndicadorE", "CodigoLinha",
                                                      "CodigoTresC", "A", "B",
                                                      "NomeA", "NomeB",
                                                      "CodigoLinh", "CodigoLi00",
                                                      "CodigoEsta", "CodigoFerr",
                                                      "CodigoFerrovia", "SiglaFerrovia",
                                                      "LogotipoFerrovia", "DataExclusao",
                                                      "IndicadorObrigatorioDesempenhoProducao",
                                                      "CodigoLinha", "CodigoEstacao",
                                                      "linesta", "Ferrovia", "Linha",
                                                      "X", "Ferrovia", "Linha",
                                                      "x", "CodigoTresLetrasEstacao",
                                                      "ExtensÃ.o..km.", "Instalada",
                                                      "Utilizada", "Trens")]


colnames(myshp@data)[which(names(myshp@data) == "NumeroQuil")] <- "Marco Quilométrico"
colnames(myshp@data)[which(names(myshp@data) == "NumeroExte")] <- "Extensão do Entre Pátio (km)"
colnames(myshp@data)[which(names(myshp@data) == "NomeFerrovia")] <- "Ferrovia"
colnames(myshp@data)[which(names(myshp@data) == "NomeReduzidoFerrovia")] <- "Sigla-Ferrovia"
colnames(myshp@data)[which(names(myshp@data) == "NomeLinha")] <- "Linha"

myshp@data$OcupaçãoCrescente <- myshp@data$`Vinculada Crescente`/myshp@data$`Instalada Crescente`
myshp@data$OcupaçãoDecrescente <- myshp@data$`Vinculada Decrescente`/myshp@data$`Instalada Decrescente`

l <- vector("list", length(myshp@data$OcupaçãoDecrescente))
for (i in c(1:length(myshp@data$OcupaçãoDecrescente))){
  l[[i]] = max(myshp@data[i,c("OcupaçãoCrescente", "OcupaçãoCrescente")])
  
}
l <- lapply(l, FUN = function(x) ifelse(is.nan(x),NA,x))
l <- lapply(l, FUN = function(x) ifelse(is.infinite(x),NA,x))
myshp@data$Ocupação <- unlist(l)
myshp@data$Ocupação[myshp@data$Ocupação == 0] <- NA
myshp@data$Ocupação <- round(myshp@data$Ocupação, digits=2)


# construção da paleta de cores a ser usada no mapa para representação das concessionárias
red = colorRampPalette(c('green', 'yellow','red'))

# Logo ANTT
img <- "https://upload.wikimedia.org/wikipedia/commons/thumb/2/29/Logo_ANTT.svg/1200px-Logo_ANTT.svg.png"

m3 <- mapview(myshp, zcol="Ocupação",
              legend = TRUE,
              layer.name = 'Utilização da Capacidade Instalada',
              color = red) %>% 
  
  leafem::addLogo(img, width = 120, height = 60, url = "http://www.antt.gov.br/", position="topleft")%>%
  
  addControl("Pesquisa de Estações",
             position = "topleft") %>%
  
  addCircleMarkers(data = myshp2, lng = myshp2@coords[,1],
                   lat=myshp2@coords[,2],
                   popup = ~`Código Estação`,
                   label=~`Código Estação`,
                   group='Código Estação') %>%
  
  addSearchFeatures(targetGroups = 'Código Estação',
                    options = searchFeaturesOptions(
                      zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
                      autoCollapse = FALSE, hideMarkerOnCollapse = FALSE)) %>%
  
  hideGroup('Código Estação')



mapshot(m3, url = "DR_Interface_Grafica.html", selfcontained = FALSE)




