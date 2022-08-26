####### SELECIONANDO A PASTA DE AREA DE TRABALHO #########
setwd("D:/04 - ANÁLISE POR POSTOS")
getwd()

##################### ANÁLISE DE 'RANKING' ###########################################

########## CARREGANDO OS DADOS DE IMPORTÂNCIA  #############################
dados <- read.csv("dados_fatorial_IMP_final.csv", header = TRUE, sep = ";", encoding = "Windows-1252")




############## CARREGANDO PACOTES NECESS?RIOS ############
require(readxl)
require(corrplot)
library(psych)
library(factoextra)
library(FactoMineR)
library(qgraph)
library(irr)
library(ggplot2)

########################### IMPORTÂNCIA ###########################
OG_CL <- subset(dados, dados$REGISTRO == "OG_CL")
CL_TC <- subset(dados, dados$REGISTRO == "CL_TC")
TC_MJ <- subset(dados, dados$REGISTRO == "TC_MJ")

G1 <- as.data.frame(rbind(OG_CL, CL_TC, TC_MJ))


MJ_CP <- subset(dados, dados$REGISTRO == "MJ_CP")
CP_T <- subset(dados, dados$REGISTRO == "CP_T")

G2 <- as.data.frame(rbind(MJ_CP, CP_T))

T_SO <- subset(dados, dados$REGISTRO == "T_SO")
SO_1S <- subset(dados, dados$REGISTRO == "SO_1S")

G3 <- as.data.frame(rbind(T_SO, SO_1S))

S_2S <- subset(dados, dados$REGISTRO == "1S_2S")
S_3S <- subset(dados, dados$REGISTRO == "2S_3S")
S_CB <- subset(dados, dados$REGISTRO == "3S_CB")

G4 <- as.data.frame(rbind(S_2S, S_3S, S_CB))

G1['GRUPO'] = 'G1'
G2['GRUPO'] = 'G2'
G3['GRUPO'] = 'G3'
G4['GRUPO'] = 'G4'

dados <- rbind(G1, G2, G3, G4)
dados <- subset(dados, select = -c(REGISTRO, ID))

aggregate(x = dados, by = list(unique.values = dados$GRUPO), FUN = "mean")

################################# EXIGÊNCIA ########################################
#################### CARREGANDO OS DADOS DE EXIGÊNCIA  #############################
dados <- read.csv("dados_fatorial_COM_final.csv", header = TRUE, sep = ";", encoding = "Windows-1252")


OG_CL <- subset(dados, dados$REGISTRO == "OG_CL")
CL_TC <- subset(dados, dados$REGISTRO == "CL_TC")
TC_MJ <- subset(dados, dados$REGISTRO == "TC_MJ")

G1 <- as.data.frame(rbind(OG_CL, CL_TC, TC_MJ))


MJ_CP <- subset(dados, dados$REGISTRO == "MJ_CP")
CP_T <- subset(dados, dados$REGISTRO == "CP_T")

G2 <- as.data.frame(rbind(MJ_CP, CP_T))

T_SO <- subset(dados, dados$REGISTRO == "T_SO")
SO_1S <- subset(dados, dados$REGISTRO == "SO_1S")

G3 <- as.data.frame(rbind(T_SO, SO_1S))

S_2S <- subset(dados, dados$REGISTRO == "1S_2S")
S_3S <- subset(dados, dados$REGISTRO == "2S_3S")
S_CB <- subset(dados, dados$REGISTRO == "3S_CB")

G4 <- as.data.frame(rbind(S_2S, S_3S, S_CB))

G1['GRUPO'] = 'G1'
G2['GRUPO'] = 'G2'
G3['GRUPO'] = 'G3'
G4['GRUPO'] = 'G4'

dados <- rbind(G1, G2, G3, G4)
dados <- subset(dados, select = -c(REGISTRO, ID))

aggregate(x = dados, by = list(unique.values = dados$GRUPO), FUN = "mean")
