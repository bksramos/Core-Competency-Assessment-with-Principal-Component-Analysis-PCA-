####### SELECIONANDO A PASTA DE ÁREA DE TRABALHO #########
setwd("C:/Users/DPL/Desktop/TRABALHO_PESQUISA_CPO/03 - PESQUISA FINAL")
getwd()

##################### ANÁLISE FATORIAL ###########################################

########## CARREGANDO OS DADOS PARA ANÁLISE FATORIAL #############################
dados <- read.csv("dados_fatorial_IMP_final.csv", header = TRUE, sep = ";", encoding = "Windows-1252")

############## CARREGANDO PACOTES NECESSÁRIOS ############
require(readxl)
require(corrplot)
require(psych)
library(factoextra)
library(FactoMineR)
library(qgraph)
library(irr)
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

OFICIAIS <- as.data.frame(rbind(OG_CL, CL_TC, TC_MJ, MJ_CP, CP_T))

## CONSTRUÇÃO DA MATRIZ DE CORRELAÇÃO
dados <- subset(OFICIAIS, select = -c(REGISTRO))

dados_matrix <- data.matrix(dados[,2:23])
rownames(dados_matrix) <- dados[,1] # add city names as row labels
matcor <- cor(dados_matrix)
print(matcor, digits = 2)

corrplot(matcor, method="circle")

qgraph(cor(matcor))

qgraph(cor(matcor), title="Correlações Importância - Oficiais",
       # layout = "spring", 
       posCol = "darkgreen", negCol = "darkred", arrows = FALSE,
       node.height=0.8, node.width=0.8, vTrans=255, edge.width=0.75, label.cex=1.0,
       width=7, height=5, normalize=TRUE, edge.width=0.75 ) 

qgraph(cor(matcor), layout="spring", shape="rectangle", 
       posCol="darkgreen", negCol="darkmagenta" , 
       title = "Correlações Importância - Oficiais")

## Teste de Esfericidade de Bartlett

  #Hipóteses:
  
  #Ho: A matriz de correlação da população é uma matriz identidade, ou seja 
  #as variáveis não são correlacionadas na população.

  #H1: A matriz de correlação da população não é uma matriz identidade, ou seja 
  #as variáveis são correlacionadas na população.

cortest.bartlett(matcor, n=440)

  #Veja que a hipótese nula de que a matriz de correlação da população seja uma 
  #matriz identidade é rejeitada pelo teste de esfericidade de Bartlett. 
  #A estatística qui-quadrado aproximada é 4141,041 com 231 graus de liberdade, 
  #significativa ao nível de 0,05.

  #A medida de adequacidade da amostra de Kaiser-Meyer-Olkin (KMO) compara as 
  #magnitudes dos coeficientes de correlação observados com as magnitudes dos 
  #coeficientes de correlação parcial. Pequenos valores de KMO indicam que as 
  #correlações entre os pares de variáveis não podem ser explicadas por outras 
  #variáveis, indicando que a análise fatorial não é adequada.

KMO(matcor)

  #A estatística KMO maior que 0,5 também concorda quanto ao fato 
  #de que a análise fatorial pode ser considerada uma técnica apropriada 
  #para analisar a matriz de correlação.

icc(matcor, unit = "average")
### ANÁLISE DE COMPONENTES PRINCIPAIS
pca=PCA(dados_matrix, graph=TRUE)
pca

#A variância retida em cada um dos compomentes é medida pelos
#"autovalores" (eigenvalues), que podem ser extraídos utilizando a 
#função get_eigenvalue():

autovalores=get_eigenvalue(pca)
autovalores

#Autovaloes superiores a 1 indicam que a variância do componente é superior
#ao que representaria a variância dos dados originais, sendo possível utilizar 
#inclusive como ponto de corte para decidir quantos componentes utilizar. Observa-se
#que no exemplo acima, foram criados 22 componentes principais, dos quais os 4
#primeiros explicam 55.03% da variação.


gpca <- fviz_eig(pca, addlabels=FALSE, ylim = c(0,40), title = "")

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
  ggtitle("Scree Plot - Importância - Oficiais") +
  ylab("Porcentagem de Variância Explicada") +
  xlab("Dimensões")
gpca
ggsave(gpca, filename = "Scree_IMP_OF - Final.png",  bg = "transparent")

variaveis=get_pca_var(pca)
head(variaveis$coord)

fit<-princomp(dados_matrix,cor=TRUE)
fit
  #cor = TRUE: as componentes principais serão geradas a partir da matriz de correlação.
  #cor = FALSE: as componentes principais serão geradas a partir da matriz de covariância.

summary(fit)

  #A função summary(fit) mostra a aplicação da análise de componentes principais. 
  #O fator 1 responde por 35.68% da variância total. Da mesma forma, o segundo fator 
  #responde por 8.65% da variância total. Várias considerações devem integrar a análise do número
  #de fatores que devem ser usados na análise.


screeplot(fit)

biplot(fit, col = c("black", "red"), cex = c(0.7, 0.8))

PCAcompetencias <-principal(dados_matrix, nfactors=4, n.obs = 471,
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

qgraph(loadings(PCAcompetencias), title="Cargas dos Componentes Não Rotacionadas - Visão Elipse", 
       posCol = "darkgreen", negCol = "darkmagenta", arrows = FALSE, 
       labels=attr(loadings(PCAcompetencias), "dimnames")[[1]])

qgraph_loadings_plot(PCAcompetencias, "Cargas dos Componentes Não Rotacionadas - Visão spring")


#Os gráficos acima são resultados de duas funções que buscam gerar vizualizações 
#de como as variáveis originais se relacionam com os componentes. Os componentes
#principais são representados pelos números dentro de círculos e as variáveis por
#suas iniciais dentro de quadrados. A relação entre os componentes e as variáveis é 
#verificada pelas linhas que os unem, as linhas de coloração verde representam relações positivas
#e as de cor vermelha relações antagônicas, a expessura desas ligações revela a magni
#tude desta relação. O primeiro gráfico simplesmente organiza as variáveis e componentes 
#ao redor de uma elipse, já o segundo utiliza uma configuração onde a representação das cargas é 
#escalada de forma inversamente proporcional à maginitude da relação variável - componente,
#ou seja, segmentos mais curtos representam carga maior entre variável e componente.

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

PCACompetenciasVarimax<-principal(dados_matrix, nfactors=4,
                           n.obs=440,rotate="varimax",scores=TRUE)
PCACompetenciasVarimax

print(loadings(PCACompetenciasVarimax), cutoff=0.4)

plot(PCACompetenciasVarimax$scores[,1:2], type="n")
text(PCACompetenciasVarimax$scores[,1:2], labels=seq(1:length(dados[,1])))

biplot.psych(PCACompetenciasVarimax, labels=rownames(PCACompetenciasVarimax), col=c("black","red"), cex=c(0.7,0.8),
             xlim.s=c(-3,3), ylim.s=c(-2,4))

qgraph(loadings(PCACompetenciasVarimax), title="Carga dos Componentes Rotacionados - Visão Elipse",
       posCol = "darkgreen", negCol = "darkmagenta", arrows = FALSE, 
       labels=attr(loadings(PCAcompetencias), "dimnames")[[1]])

qgraph_loadings_plot(PCACompetenciasVarimax, 
                     title="Carga dos Componentes Rotacionados - Visão Spring")

### AUTOVALORES 

PCACompetenciasVarimax$values
  
  #verificar quantos autovalores acima de 1 temos

print(loadings(PCACompetenciasVarimax), cutoff=0.6)

  #Recurso importante na interpretação dos fatores, o gráfico das variáveis, 
  #apresenta ao final do eixo, as variáveis que com cargas mais altas 
  #sobre aquele fator. Quanto mais próximas da origem menores as cargas 
  #destas variáveis sobre aquele fator. Variáveis distantes dos dois eixos, 
  #estão relacionadas a ambos os fatores.

factor.scores(dados_matrix,PCACompetenciasVarimax, 
              Phi = NULL, 
              method = c("Thurstone", "tenBerge", "Anderson",
                         "Bartlett", "Harman","components"),
              rho=NULL)

### MÉTODO DE ANÁLISE FATORIAL

Comp_FA <- factanal(dados_matrix, factors=2, rotation="varimax",
                         scores="regression")
Comp_FA # lista de resultados
print(loadings(Comp_FA),cutoff=0.4)
biplot(Comp_FA$scores[,1:2], loadings(Comp_FA)[,1:2],
       main="PCA/Análise Fatorial Biplot - 2 Componentes", cex=0.5)

qgraph_loadings_plot(Comp_FA, "Análise Fatorial, Componnentes Rotacionados")

