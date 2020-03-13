library(rgdal)
library(mapview)
library(leaflet)
library(RColorBrewer)
library(dplyr)


####################### AJUSTE DAS CAMADAS DO SHAPEFILE #######################

# Leitura das bases georreferenciadas
## myshp = linhas. myshp3 = estações
myshp <- readOGR(dsn=path.expand("shp_atualizado"),
                 layer="dbo_tblLinhaEstacao_spatial_linestring", stringsAsFactors = FALSE)


# Substituição do CodigoFerr, antes numérico, agora string, com o nome das estações
tblFerrovia <- readxl::read_excel("Dados/tblFerrovia.xlsx")
tblFerrovia$CodigoFerr <- tblFerrovia$CodigoFerrovia
myshp@data <- plyr::join(myshp@data,
                         tblFerrovia,
                         by='CodigoFerr')

# Substituição dos códigos de linha pelo nome completo
tblLinha <- readxl::read_excel("Dados/tblLinha.xlsx")
tblLinha$CodigoLi00 <- tblLinha$CodigoLinha
myshp@data <- plyr::join(myshp@data,
                  tblLinha,
                  by='CodigoLi00')

# Substituição dos códigos de estação pelo código de três letras
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


# Substituição dos códigos de bitola pela sua classificação real
{myshp@data[myshp@data[["CodigoBito"]] == '1', 'CodigoBito'] <- 'Métrica'
  myshp@data[myshp@data[["CodigoBito"]] == '3', 'CodigoBito'] <- 'Larga'
  myshp@data[myshp@data[["CodigoBito"]] == '5', 'CodigoBito'] <- 'Mista'}


# Colocando a TU
DR_TU <- read.table("Dados/Capacidade_Utilizada_2019.csv",
                    sep=';', dec=',', header=TRUE)
DR_TU$AB <- NA
DR_TU$AB <- paste(DR_TU$A, DR_TU$B, sep=" - ")


myshp@data$AB <- NA
myshp@data$AB <- paste(myshp@data$A,
                       myshp@data$B, sep=" - ")

myshp@data <- plyr::join(myshp@data,
                         DR_TU,
                         by='AB',
                         match="first")

# writeOGR(myshp, ".", "RFF_Shape", driver="ESRI Shapefile")

red = colorRampPalette(c('green', 'yellow','red'))

myshp@data$dummy <- myshp@data$Utilizada
myshp@data$dummy <- ifelse(myshp@data$Utilizada > 1, 1, myshp@data$dummy)
myshp@data$dummy <- ifelse(myshp@data$dummy == 0, NA, myshp@data$dummy)

# Drop das colunas desnecessarias
myshp@data <- myshp@data[ , !names(myshp@data) %in% c("CodigoLinh", "CodigoLi00",
                                                      "CodigoEsta", "CodigoBito",
                                                      "NumeroSequ", "IndicadorC",
                                                      "IndicadorE", "CodigoLinha",
                                                      "CodigoEstacao", "CodigoTresC",
                                                      "CodigoLinh", "CodigoLi00",
                                                      "CodigoEsta", "CodigoFerr",
                                                      "CodigoFerrovia", "SiglaFerrovia",
                                                      "LogotipoFerrovia", "DataExclusao",
                                                      "IndicadorObrigatorioDesempenhoProducao",
                                                      "CodigoLinha", "CodigoEstacao",
                                                      "linesta", "Ferrovia", "Linha",
                                                      "X", "Ferrovia", "Linha", "A", "B",
                                                      "x", "CodigoTresLetrasEstacao",
                                                      "ExtensÃ.o..km.")]

colnames(myshp@data)[which(names(myshp@data) == "NumeroQuil")] <- "Marco Quilométrico"
colnames(myshp@data)[which(names(myshp@data) == "NumeroExte")] <- "Extensão do Entre Pátio (km)"
colnames(myshp@data)[which(names(myshp@data) == "NomeFerrovia")] <- "Ferrovia"
colnames(myshp@data)[which(names(myshp@data) == "NomeReduzidoFerrovia")] <- "Sigla-Ferrovia"
colnames(myshp@data)[which(names(myshp@data) == "NomeLinha")] <- "Linha"
colnames(myshp@data)[which(names(myshp@data) == "Instalada")] <- "Capacidade Instalada"
colnames(myshp@data)[which(names(myshp@data) == "Trens")] <- "Capacidade Vinculada"
colnames(myshp@data)[which(names(myshp@data) == "Utilizada")] <- "Saturação"

m3 <- mapview(myshp, zcol="dummy",
              legend = TRUE,
              layer.name = 'Utilização da Capacidade Instalada',
              color = red)

mapshot(m3, url = "Dados/DR_Interface_Grafica.html", selfcontained = FALSE)







