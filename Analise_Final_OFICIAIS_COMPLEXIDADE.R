####### SELECIONANDO A PASTA DE ?REA DE TRABALHO #########
setwd("C:/Users/DPL/Desktop/TRABALHO_PESQUISA_CPO/03 - PESQUISA FINAL")
getwd()

##################### AN?LISE FATORIAL ###########################################

########## CARREGANDO OS DADOS PARA AN?LISE FATORIAL #############################
dados <- read.csv("dados_fatorial_COM_final.csv", header = TRUE, sep = ";", encoding = "Windows-1258")

############## CARREGANDO PACOTES NECESS?RIOS ############
require(readxl)
require(corrplot)
require(psych)
library(factoextra)
library(FactoMineR)
library(qgraph)
## SUMMARY DA BASE

attach(dados)
summary(dados)

## tratando os dados missing

  ## TRAnsformando todos os NA em 0

dados[is.na(dados)] <- 0

  ## Criando o Loop ##
  # o loop funcionar? da seguinte maneira: calcula a m?dia de uma coluna e arredonda
  # este valor para cima, o valor obtido ser? inserido no lugar dos valores '0' que
  # representam os valores NA. Este processo ? repetido para todas as colunas

for (j in names(dados)) {
  for (i in seq_along(dados[,j])) {
    dados[,j][[i]][dados[,j][[i]] %in% 0] <- ceiling(mean(dados[,j]))
  }
}


####SEGMENTANDO A BASE EM SUBSETS
OG_CL <- subset(dados, dados$REGISTRO == "OG_CL")
CL_TC <- subset(dados, dados$REGISTRO == "CL_TC")
TC_MJ <- subset(dados, dados$REGISTRO == "TC_MJ")
MJ_CP <- subset(dados, dados$REGISTRO == "MJ_CP")
CP_T  <- subset(dados, dados$REGISTRO == "CP_T")
T_SO  <- subset(dados, dados$REGISTRO == "T_SO")
SO_1S <- subset(dados, dados$REGISTRO == "SO_1S")
S_2S  <- subset(dados, dados$REGISTRO == "1S_2S")
S_3S  <- subset(dados, dados$REGISTRO == "2S_3S")
S_CB  <- subset(dados, dados$REGISTRO == "3S_CB")

OFICIAIS <- as.data.frame(rbind(OG_CL, CL_TC, TC_MJ, MJ_CP, CP_T))

## CONSTRU??O DA MATRIZ DE CORRELA??O
dados <- subset(OFICIAIS, select = -c(REGISTRO))

dados_matrix <- data.matrix(dados[,2:20])
rownames(dados_matrix) <- dados[,1] # add city names as row labels
matcor <- cor(dados_matrix)
print(matcor, digits = 2)

corrplot(matcor, method="circle")

qgraph(cor(matcor))

qgraph(cor(matcor), title="Correla??es Grau de Exig?ncia - Oficiais",
       # layout = "spring", 
       posCol = "darkgreen", negCol = "darkred", arrows = FALSE,
       node.height=0.8, node.width=0.8, vTrans=255, edge.width=0.75, label.cex=1.0,
       width=7, height=5, normalize=TRUE, edge.width=0.75 ) 

qgraph(cor(matcor), layout="spring", shape="rectangle", 
       posCol="darkgreen", negCol="darkmagenta" , 
       title = "Correla??es Grau de Exig?ncia - Oficiais")

## Teste de Esfericidade de Bartlett

  #Hip?teses:
  
  #Ho: A matriz de correla??o da popula??o ? uma matriz identidade, ou seja 
  #as vari?veis n?o s?o correlacionadas na popula??o.

  #H1: A matriz de correla??o da popula??o n?o ? uma matriz identidade, ou seja 
  #as vari?veis s?o correlacionadas na popula??o.

cortest.bartlett(dados)

  #Veja que a hip?tese nula de que a matriz de correla??o da popula??o seja uma 
  #matriz identidade ? rejeitada pelo teste de esfericidade de Bartlett. 
  #A estat?stica qui-quadrado aproximada ? 173071.3 com 2485 graus de liberdade, 
  #significativa ao n?vel de 0,05.

  #A medida de adequacidade da amostra de Kaiser-Meyer-Olkin (KMO) compara as 
  #magnitudes dos coeficientes de correla??o observados com as magnitudes dos 
  #coeficientes de correla??o parcial. Pequenos valores de KMO indicam que as 
  #correla??es entre os pares de vari?veis n?o podem ser explicadas por outras 
  #vari?veis, indicando que a an?lise fatorial n?o ? adequada.

KMO(dados)
  #A estat?stica KMO maior que 0,5 tamb?m concorda quanto ao fato 
  #de que a an?lise fatorial pode ser considerada uma t?cnica apropriada 
  #para analisar a matriz de correla??o.


### AN?LISE DE COMPONENTES PRINCIPAIS
pca=PCA(dados_matrix, graph=TRUE)
pca

#A vari?ncia retida em cada um dos compomentes ? medida pelos
#"autovalores" (eigenvalues), que podem ser extra?dos utilizando a 
#fun??o get_eigenvalue():

autovalores=get_eigenvalue(pca)
autovalores

#Autovaloes superiores a 1 indicam que a vari?ncia do componente ? superior
#ao que representaria a vari?ncia dos dados originais, sendo poss?vel utilizar 
#inclusive como ponto de corte para decidir quantos componentes utilizar. Observa-se
#que no exemplo acima, foram criados 22 componentes principais, dos quais os 4
#primeiros explicam 55.03% da varia??o.


gpca <- fviz_eig(pca, addlabels=FALSE, ylim = c(0,50), barfill = "deeppink4")

gpca <- gpca +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    plot.title = element_text(hjust = 0.5, size = 22, colour = "white"),
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    axis.text.x=element_text(colour="white"),
    axis.text.y=element_text(colour="white"),
    axis.title.x = element_text(colour = "white"),
    axis.title.y = element_text(colour = "white"),
    legend.box.background = element_rect(fill = "transparent")) + # get rid of legend panel bg
  geom_text(aes(label=paste0(round(eig, digits = 1), "%") ), 
            position=position_dodge(width=0.9), vjust=-0.25, col = c("white")) +
  ggtitle("Scree Plot - Exig?ncia - Oficiais") +
  ylab("Porcentagem de Vari?ncia Explicada") +
  xlab("Dimens?es")
gpca
ggsave(gpca, filename = "Scree_Exi_OF - Final.png",  bg = "transparent")

variaveis=get_pca_var(pca)
head(variaveis$coord)

fit<-princomp(dados_matrix,cor=TRUE)
fit
#cor = TRUE: as componentes principais ser?o geradas a partir da matriz de correla??o.
#cor = FALSE: as componentes principais ser?o geradas a partir da matriz de covari?ncia.

summary(fit)

#A fun??o summary(fit) mostra a aplica??o da an?lise de componentes principais. 
#O fator 1 responde por 35.68% da vari?ncia total. Da mesma forma, o segundo fator 
#responde por 8.65% da vari?ncia total. V?rias considera??es devem integrar a an?lise do n?mero
#de fatores que devem ser usados na an?lise.


screeplot(fit)

biplot(fit, col = c("black", "red"), cex = c(0.7, 0.8))

PCAcompetencias <-principal(dados_matrix, nfactors=3, n.obs = 440,
                            rotate="none", scores=TRUE)
PCAcompetencias

print(loadings(PCAcompetencias), cutoff=0.4)

plot(PCAcompetencias$scores[,1:2], type="n")
text(PCAcompetencias$scores[,1:2], labels=seq(1:length(dados[,1])))


qgraph_loadings_plot <- function(loadings_in, title) {
  ld <- loadings(loadings_in)
  qg_pca <- qgraph(ld, title=title, 
                   posCol = "darkgreen", negCol = "darkmagenta", arrows = FALSE, 
                   labels=attr(ld, "dimnames")[[1]])
  qgraph(qg_pca, title=title,
         layout = "spring", 
         posCol = "darkgreen", negCol = "darkmagenta", arrows = FALSE,
         node.height=1, node.width=1, vTrans=255, edge.width=0.75, label.cex=1.2,
         width=7, height=5, normalize=TRUE, edge.width=0.75)
}

qgraph(loadings(PCAcompetencias), title="Cargas dos Componentes N?o Rotacionadas - Vis?o Elipse", 
       posCol = "darkgreen", negCol = "darkmagenta", arrows = FALSE, 
       labels=attr(loadings(PCAcompetencias), "dimnames")[[1]])

qgraph_loadings_plot(PCAcompetencias, "Cargas dos Componentes N?o Rotacionadas - Vis?o spring")


#Os gr?ficos acima s?o resultados de duas fun??es que buscam gerar vizualiza??es 
#de como as vari?veis originais se relacionam com os componentes. Os componentes
#principais s?o representados pelos n?meros dentro de c?rculos e as vari?veis por
#suas iniciais dentro de quadrados. A rela??o entre os componentes e as vari?veis ? 
#verificada pelas linhas que os unem, as linhas de colora??o verde representam rela??es positivas
#e as de cor vermelha rela??es antag?nicas, a expessura desas liga??es revela a magni
#tude desta rela??o. O primeiro gr?fico simplesmente organiza as vari?veis e componentes 
#ao redor de uma elipse, j? o segundo utiliza uma configura??o onde a representa??o das cargas ? 
#escalada de forma inversamente proporcional ? maginitude da rela??o vari?vel - componente,
#ou seja, segmentos mais curtos representam carga maior entre vari?vel e componente.

### MATRIX ROTADA DO FATOR

#Com o objetivo de possibilitar uma melhor interpreta??o dos fatores, ? pr?tica comum 
#fazer uma rota??o ou uma transforma??o dos fatores.

#O conjunto de cargas fatoriais, obtidas por qualquer m?todo de solu??o fatorial, 
#quando o n?mero de fatores comuns ? maior do que um, n?o ? ?nico, pois outros conjuntos
#equivalentes podem ser encontrados, por transforma??es ortogonais de cargas.

#Na rota??o ortogonal, os eixos s?o mantidos em ?ngulo reto, sendo o m?todo mais 
#utilizado o processo varimax. Esse m?todo ortogonal de rota??o minimiza o n?mero 
#de vari?veis com altas cargas sobre um fator afim de permitir a interpreta?? dos fatores. 
#A rota??o ortogonal resulta em fatores n?ocorrelacionados ao passo que a rota??o obl?qua 
#n?o mant?m os eixos em ?ngulo reto e os fatores s?o correlacionados (MALHOTRA, 2001).

PCACompetenciasVarimax<-principal(dados_matrix, nfactors=3,
                                  n.obs=440,rotate="varimax",scores=TRUE)
PCACompetenciasVarimax

print(loadings(PCACompetenciasVarimax), cutoff=0.6)

plot(PCACompetenciasVarimax$scores[,1:2], type="n")
text(PCACompetenciasVarimax$scores[,1:2], labels=seq(1:length(dados[,1])))

biplot.psych(PCACompetenciasVarimax, labels=rownames(PCACompetenciasVarimax), col=c("black","red"), cex=c(0.7,0.8),
             xlim.s=c(-3,3), ylim.s=c(-2,4))

qgraph(loadings(PCACompetenciasVarimax), title="Carga dos Componentes Rotacionados - Vis?o Elipse",
       posCol = "darkgreen", negCol = "darkmagenta", arrows = FALSE, 
       labels=attr(loadings(PCAcompetencias), "dimnames")[[1]])

qgraph_loadings_plot(PCACompetenciasVarimax, 
                     title="Carga dos Componentes Rotacionados - Vis?o Spring")

### AUTOVALORES 

PCACompetenciasVarimax$values

#verificar quantos autovalores acima de 1 temos

print(loadings(PCACompetenciasVarimax), cutoff=0.5)

#Recurso importante na interpreta??o dos fatores, o gr?fico das vari?veis, 
#apresenta ao final do eixo, as vari?veis que com cargas mais altas 
#sobre aquele fator. Quanto mais pr?ximas da origem menores as cargas 
#destas vari?veis sobre aquele fator. Vari?veis distantes dos dois eixos, 
#est?o relacionadas a ambos os fatores.

factor.scores(dados_matrix,PCACompetenciasVarimax, 
              Phi = NULL, 
              method = c("Thurstone", "tenBerge", "Anderson",
                         "Bartlett", "Harman","components"),
              rho=NULL)

### M?TODO DE AN?LISE FATORIAL

Comp_FA <- factanal(dados_matrix, factors=2, rotation="varimax",
                    scores="regression")
Comp_FA # lista de resultados
print(loadings(Comp_FA),cutoff=0.4)
biplot(Comp_FA$scores[,1:2], loadings(Comp_FA)[,1:2],
       main="PCA/An?lise Fatorial Biplot - 2 Componentes", cex=0.5)

qgraph_loadings_plot(Comp_FA, "An?lise Fatorial, Componnentes Rotacionados")

