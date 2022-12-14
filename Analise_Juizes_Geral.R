####### SELECIONANDO A PASTA DE ?REA DE TRABALHO #########
setwd("C:/Users/brunno_bksr/Desktop/TRABALHO_PESQUISA_CPO/02 - AN?LISE DE JU?ZES")
getwd()

##################### AN?LISE FATORIAL ###########################################

########## CARREGANDO OS DADOS PARA AN?LISE FATORIAL #############################
dados <- read.csv("dados_fatorial_COM.csv", header = TRUE, sep = ";", encoding = "Windows-1258")

############## CARREGANDO PACOTES NECESS?RIOS ############
require(readxl)
require(corrplot)
require(psych)
library(factoextra)
library(FactoMineR)

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

## CONSTRU??O DA MATRIZ DE CORRELA??O
dados <- subset(dados, select = -c(ID, REGISTRO))
matcor <- cor(dados)
print(matcor, digits = 2)

corrplot(matcor, method="circle")

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


### M?TODO DE AN?LISE FATORIAL

fit<-princomp(dados,cor=TRUE)
fit

  #cor = TRUE: as componentes principais ser?o geradas a partir da matriz de correla??o.
  #cor = FALSE: as componentes principais ser?o geradas a partir da matriz de covari?ncia.

summary(fit)

  #A fun??o summary(fit) mostra a aplica??o da an?lise de componentes principais. 
  #O fator 1 responde por 31.59% da vari?ncia total. Da mesma forma, o segundo fator 
  #responde por 6.84% da vari?ncia total. V?rias considera??es devem integrar a an?lise do n?mero
  #de fatores que devem ser usados na an?lise.

screeplot(fit)

plot(fit,type="lines")

### AN?LISE DE COMPONENTES PRINCIPAIS

PCAcompetencias <-principal(dados, nfactors=2,
                    n.obs=723,rotate="none", scores=TRUE)
PCAcompetencias

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

PCACompetenciasVarimax<-principal(dados, nfactors=2,
                           n.obs=723,rotate="varimax",scores=TRUE)
PCACompetenciasVarimax

### AUTOVALORES 

PCACompetenciasVarimax$values
  
  #verificar quantos autovalores acima de 1 temos

PCACompetenciasVarimax$loadings

  #Recurso importante na interpreta??o dos fatores, o gr?fico das vari?veis, 
  #apresenta ao final do eixo, as vari?veis que com cargas mais altas 
  #sobre aquele fator. Quanto mais pr?ximas da origem menores as cargas 
  #destas vari?veis sobre aquele fator. Vari?veis distantes dos dois eixos, 
  #est?o relacionadas a ambos os fatores.

biplot(PCACompetenciasVarimax)

factor.scores(dados,PCACompetenciasVarimax, 
              Phi = NULL, 
              method = c("Thurstone", "tenBerge", "Anderson",
                         "Bartlett", "Harman","components"),
              rho=NULL)



### An?lise de Componentes Principais
pca=PCA(dados, graph=TRUE)
pca

  #A vari?ncia retida em cada um dos compomentes ? medida pelos
  #"autovalores" (eigenvalues), que podem ser extra?dos utilizando a 
  #fun??o get_eigenvalue():

autovalores=get_eigenvalue(pca)
autovalores

  #Autovaloes superiores a 1 indicam que a vari?ncia do componente ? superior
  #ao que representaria a vari?ncia dos dados originais, sendo poss?vel utilizar 
  #inclusive como ponto de corte para decidir quantos componentes utilizar. Observa-se
  #que no exemplo acima, foram criados 71 componentes principais, dos quais os 11
  #primeiros explicam 60.15% da varia??o.

fviz_eig(pca, addlabels=TRUE, ylim = c(0,50))

variaveis=get_pca_var(pca)
head(variaveis$coord)