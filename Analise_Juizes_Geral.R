####### SELECIONANDO A PASTA DE ÁREA DE TRABALHO #########
setwd("C:/Users/brunno_bksr/Desktop/TRABALHO_PESQUISA_CPO/02 - ANÁLISE DE JUÍZES")
getwd()

##################### ANÁLISE FATORIAL ###########################################

########## CARREGANDO OS DADOS PARA ANÁLISE FATORIAL #############################
dados <- read.csv("dados_fatorial_COM.csv", header = TRUE, sep = ";", encoding = "Windows-1258")

############## CARREGANDO PACOTES NECESSÁRIOS ############
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
  # o loop funcionará da seguinte maneira: calcula a média de uma coluna e arredonda
  # este valor para cima, o valor obtido será inserido no lugar dos valores '0' que
  # representam os valores NA. Este processo é repetido para todas as colunas

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

## CONSTRUÇÃO DA MATRIZ DE CORRELAÇÃO
dados <- subset(dados, select = -c(ID, REGISTRO))
matcor <- cor(dados)
print(matcor, digits = 2)

corrplot(matcor, method="circle")

## Teste de Esfericidade de Bartlett

  #Hipóteses:
  
  #Ho: A matriz de correlação da população é uma matriz identidade, ou seja 
  #as variáveis não são correlacionadas na população.

  #H1: A matriz de correlação da população não é uma matriz identidade, ou seja 
  #as variáveis são correlacionadas na população.

cortest.bartlett(dados)

  #Veja que a hipótese nula de que a matriz de correlação da população seja uma 
  #matriz identidade é rejeitada pelo teste de esfericidade de Bartlett. 
  #A estatística qui-quadrado aproximada é 173071.3 com 2485 graus de liberdade, 
  #significativa ao nível de 0,05.

  #A medida de adequacidade da amostra de Kaiser-Meyer-Olkin (KMO) compara as 
  #magnitudes dos coeficientes de correlação observados com as magnitudes dos 
  #coeficientes de correlação parcial. Pequenos valores de KMO indicam que as 
  #correlações entre os pares de variáveis não podem ser explicadas por outras 
  #variáveis, indicando que a análise fatorial não é adequada.

KMO(dados)

  #A estatística KMO maior que 0,5 também concorda quanto ao fato 
  #de que a análise fatorial pode ser considerada uma técnica apropriada 
  #para analisar a matriz de correlação.


### MÉTODO DE ANÁLISE FATORIAL

fit<-princomp(dados,cor=TRUE)
fit

  #cor = TRUE: as componentes principais serão geradas a partir da matriz de correlação.
  #cor = FALSE: as componentes principais serão geradas a partir da matriz de covariância.

summary(fit)

  #A função summary(fit) mostra a aplicação da análise de componentes principais. 
  #O fator 1 responde por 31.59% da variância total. Da mesma forma, o segundo fator 
  #responde por 6.84% da variância total. Várias considerações devem integrar a análise do número
  #de fatores que devem ser usados na análise.

screeplot(fit)

plot(fit,type="lines")

### ANÁLISE DE COMPONENTES PRINCIPAIS

PCAcompetencias <-principal(dados, nfactors=2,
                    n.obs=723,rotate="none", scores=TRUE)
PCAcompetencias

### MATRIX ROTADA DO FATOR

  #Com o objetivo de possibilitar uma melhor interpretação dos fatores, é prática comum 
  #fazer uma rotação ou uma transformação dos fatores.

  #O conjunto de cargas fatoriais, obtidas por qualquer método de solução fatorial, 
  #quando o número de fatores comuns é maior do que um, não é único, pois outros conjuntos
  #equivalentes podem ser encontrados, por transformações ortogonais de cargas.

  #Na rotação ortogonal, os eixos são mantidos em ângulo reto, sendo o método mais 
  #utilizado o processo varimax. Esse método ortogonal de rotação minimiza o número 
  #de variáveis com altas cargas sobre um fator afim de permitir a interpretaçã dos fatores. 
  #A rotação ortogonal resulta em fatores nãocorrelacionados ao passo que a rotação oblíqua 
  #não mantém os eixos em ângulo reto e os fatores são correlacionados (MALHOTRA, 2001).

PCACompetenciasVarimax<-principal(dados, nfactors=2,
                           n.obs=723,rotate="varimax",scores=TRUE)
PCACompetenciasVarimax

### AUTOVALORES 

PCACompetenciasVarimax$values
  
  #verificar quantos autovalores acima de 1 temos

PCACompetenciasVarimax$loadings

  #Recurso importante na interpretação dos fatores, o gráfico das variáveis, 
  #apresenta ao final do eixo, as variáveis que com cargas mais altas 
  #sobre aquele fator. Quanto mais próximas da origem menores as cargas 
  #destas variáveis sobre aquele fator. Variáveis distantes dos dois eixos, 
  #estão relacionadas a ambos os fatores.

biplot(PCACompetenciasVarimax)

factor.scores(dados,PCACompetenciasVarimax, 
              Phi = NULL, 
              method = c("Thurstone", "tenBerge", "Anderson",
                         "Bartlett", "Harman","components"),
              rho=NULL)



### Análise de Componentes Principais
pca=PCA(dados, graph=TRUE)
pca

  #A variância retida em cada um dos compomentes é medida pelos
  #"autovalores" (eigenvalues), que podem ser extraídos utilizando a 
  #função get_eigenvalue():

autovalores=get_eigenvalue(pca)
autovalores

  #Autovaloes superiores a 1 indicam que a variância do componente é superior
  #ao que representaria a variância dos dados originais, sendo possível utilizar 
  #inclusive como ponto de corte para decidir quantos componentes utilizar. Observa-se
  #que no exemplo acima, foram criados 71 componentes principais, dos quais os 11
  #primeiros explicam 60.15% da variação.

fviz_eig(pca, addlabels=TRUE, ylim = c(0,50))

variaveis=get_pca_var(pca)
head(variaveis$coord)