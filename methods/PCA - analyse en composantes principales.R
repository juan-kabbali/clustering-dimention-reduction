### Packages
install.packages(c("FactoMineR", "factoextra","corrplot", "missMDA"))
library("FactoMineR")
library("factoextra")
library("missMDA")
library("corrplot")


### Define our gradient colors
grt.clrs = c("green","blue","red")


### Importation de donnees.
#   - Chaque pays est une ligne
d = readxl::read_xlsx('assets/world_indicator.xlsx')
row.names(d) = d$Country  # mettre la colonne Country comme row name
d$Country = NULL          # suprimmer la colonne "Country"
d = as.data.frame(d)
str(d)


# Estimations des valeurs manquantes a partir du voisinage et des autres individus qui se ressemblent
nb = estim_ncpPCA(d, ncp.max = 5)
res.comp = imputePCA(d, ncp = nb$ncp)


### ACP. Analyse en Composantes Principales
#   * d.actives   = jeu de données
#   * scale.unit  = si TRUE les données sont standardisées 
#   * ncp         = nombre de dimensions affichés
#   * quanti.sup  = on utilise l'indicateur distance-frontier-score
res.pca = PCA(res.comp$completeObs, scale.unit = TRUE, ncp = 5, graph = TRUE, quanti.sup = c(4))
res.pca


### Obtenir les valeures propres
eig.val = get_eigenvalue(res.pca)
eig.val


### Visualisation des valeurs propres. Le percentage c'est l'innertie des dimentions.
#   - On retien les premiers deux axes
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0,45))


### Obtenir les vars des dimentions du PCA
#   * coord
#   * cor
#   * cos2
#   * contrib 
var = get_pca_var(res.pca)


### Analyse par rapport aux variables
### Dimentions 1
#   * cote +  met en evidence des pays avec infant-mortality-rate m-f-u5-neonatal risk-maternal-rate elevé. 
#   * cote -  met en evidence des pays avec water-source et sanitation-facilities elevés
coord.dim_1   = var$coord[,1]
cor.dim_1     = var$cor[,1]
cos2.dim_1    = var$cos2[,1]
contrib.dim_1 = var$contrib[,1]
display.dim_1 = cbind(coord.dim_1, contrib.dim_1, cos2.dim_1, cor.dim_1)


### Visualisation du Cos2 et Contribution des variables sur l'axe 1
fviz_cos2(res.pca, choice = "var", axes = 1)    # Mieux representés
fviz_contrib(res.pca, choice = "var", axes = 1) # Plus contributions pour la constrution de l'axe


### Analyse par rapport aux variables
### Dimension 2
#   * cote + met en evidence des pays avec un nombre de infant-mortality m-f-u5-neonatal risk-maternal elevé (en nombre et pas en rate)
#   * cote - rien
coord.dim_2   = var$coord[,2]
cor.dim_2     = var$cor[,2]
cos2.dim_2    = var$cos2[,2]
contrib.dim_2 = var$contrib[,2]
display.dim_2 = cbind(coord.dim_2, contrib.dim_2, cos2.dim_2, cor.dim_2)


### Visualisation du Cos2 et Contribution des variables sur l'axe 2
fviz_cos2(res.pca, choice = "var", axes = 2)    # Mieux representés
fviz_contrib(res.pca, choice = "var", axes = 2) # Plus contributions pour la constrution de l'axe


### Visualisation du Cos2 et Contribution des variables sur les axes 1 et 2
fviz_cos2(res.pca, choice = "var", axes=1:2)
fviz_contrib(res.pca, choice = "var", axes=1:2)


### Visualisation du Cos2 et Contribution des variables sur toutes les dimensions
corrplot(var$cos2, is.corr=FALSE)
corrplot(var$contrib, is.corr=FALSE)


### Visualisation des variables selon leurs cos2
fviz_pca_var(res.pca, col.var = 'cos2', gradient.cols = grt.clrs, repel = TRUE)


### Obtenir les individuos du PCA
#   * coord
#   * cor
#   * cos2
#   * contrib 
ind = get_pca_ind(res.pca)


### Analyse par rapport aux individuos
### Dimension 1
#   * cote + met en evidence des pays comme Nigeria - Chad - Angola - Sierra Leone - Congo - Mali - Ethiopia
#   * cote - met en evidence des pays comme Finland - Alemagne - Denmark - Canada - Suisse - Iceland - Netherlands
coord.dim_1   = ind$coord[,1]
cor.dim_1     = ind$cor[,1]
cos2.dim_1    = ind$cos2[,1]
contrib.dim_1 = ind$contrib[,1]
display.dim_1 = cbind(coord.dim_1, contrib.dim_1, cos2.dim_1, cor.dim_1)


### Visualisation du Cos2 et Contribution des individuos sur l'axe 1
fviz_cos2(res.pca, choice = "ind", axes = 1, top = 30)    # Top 30 mieux representés
fviz_contrib(res.pca, choice = "ind", axes = 1, top = 30) # Top 30 qui ont plus contribue pour la constrution de l'axe


### Analyse par rapport aux individuos - Dimension 2
#   - cote + met en evidence des pays comme India - Arab World et Nigeria - Chine
#   - cote - rien
coord.dim_2   = ind$coord[,2]
cor.dim_2     = ind$cor[,2]
cos2.dim_2    = ind$cos2[,2]
contrib.dim_2 = ind$contrib[,2]
display.dim_2 = cbind(coord.dim_2, contrib.dim_2, cos2.dim_2, cor.dim_2)


### Visualisation du Cos2 et Contribution des individuos sur l'axe 2
fviz_cos2(res.pca, choice = "ind", axes = 2, top = 20)     # Top 20 mieux representés
fviz_contrib(res.pca, choice = "ind", axes = 2, top = 20)  # Top 20 qui ont plus contribue pour la constrution de l'axe


### Visualisation du Cos2 et Contribution des individuos sur les axes 1 et 2
fviz_cos2(res.pca, choice = "ind", axes=1:2, top = 50)
fviz_contrib(res.pca, choice = "ind", axes=1:2, top = 20)


### Visualisation des pays selon leurs cos2 sur les deux axes
fviz_pca_ind(res.pca, select.ind = list(cos2 = 120), pointsize = "cos2", col.ind = 'cos2', gradient.cols = grt.clrs, repel = TRUE)


### Visualisation des variables et individuos
fviz_pca(res.pca)



### Conclusions

# Dans tous les indicateur de ce jeux de donnees, ceux qui ont pris une relevance ont ete ceux qui sont relationes
# aux numbre de morts et ses respectives taux, plutot maternale et infantil

# Les pays qui ont une bonne economie "distance-frontier-score" ont au meme temps des bonnes conditions sanitaires
# "sanitation-facilities" autant aux areas ruraux comme urbainnes et aussi de bonne ressource d'eau et pour consequence
# sont les pays qui ont les taux de mortalité plus bass

# Avec la reprensetation des individuos "pays" sur les deux dimmentions, on peut distinguer les pays developes,
# les pays en developpement et les sousdevelopes





