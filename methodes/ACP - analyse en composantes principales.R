### Packages
install.packages(c("FactoMineR", "factoextra","corrplot"))
library("FactoMineR")
library("factoextra")
library("corrplot")


### Define our gradient colors
grt.clrs = c("green","blue","red")


### Importation de donnees.
#   - Chaque etudiant est une ligne
d = read.csv2('assets/student-mat.csv', sep = ";", header = TRUE)
head(d)
str(d)


### TODO pearson correlation test for G1 G2 -> G3
### TODO add plots titles


### Selectioner les vars actives.
#   - G1 G2 et G3 sont des vars suplementaires car ils dependent des autres vars
d.active.names = c("age", "Medu", "Fedu", "traveltime", "studytime", "failures", "famrel",
                   "freetime", "goout", "Dalc", "Walc", "health", "absences", "G3");
d.active = d[, d.active.names]
str(d.active)


### ACP. Analyse en Composantes Principales
#   * d.actives   = jeu de données
#   * scale.unit  = si TRUE les données sont standardisées 
#   * ncp         = nombre de dimensions affichés
#   * quanti.sup  = on utilise G1 G2 et G3
res.pca = PCA(d.active, quanti.sup = c(14), scale.unit = TRUE, ncp = 5, graph = TRUE)
res.pca


### Obtenir les valeures propres
eig.val = get_eigenvalue(res.pca)
eig.val


### Visualisation des valeurs propres. Le percentage c'est l'innertie des dimentions.
#   - On retien deux axes
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0,20))



### Obtenir les vars des dimentions du PCA
#   * coord
#   * cor
#   * cos2
#   * contrib 
var = get_pca_var(res.pca)



### Analyse par rapport aux variables - Dimension 1
#   - cote + etudiants avec walc dalc eleves
#   - cote - les autres
coord.dim_1   = var$coord[,1]
cor.dim_1     = var$cor[,1]
cos2.dim_1    = var$cos2[,1]
contrib.dim_1 = var$contrib[,1]
display.dim_1 = cbind(coord.dim_1, contrib.dim_1, cos2.dim_1, cor.dim_1)


### Visualisation du Cos2 et Contribution des variables sur l'axe 1
fviz_cos2(res.pca, choice = "var", axes = 1)
fviz_contrib(res.pca, choice = "var", axes = 1)


### Analyse par rapport aux variables - Dimension 2
#   - cote + etudiants avec Medu Fedu eleves
#   - cote - les autres
coord.dim_2   = var$coord[,2]
cor.dim_2     = var$cor[,2]
cos2.dim_2    = var$cos2[,2]
contrib.dim_2 = var$contrib[,2]
display.dim_2 = cbind(coord.dim_2, contrib.dim_2, cos2.dim_2, cor.dim_2)


### Visualisation du Cos2 et Contribution des variables sur l'axe 2
fviz_cos2(res.pca, choice = "var", axes = 2)
fviz_contrib(res.pca, choice = "var", axes = 2)


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


### Analyse par rapport aux individuos - Dimension 1
#   - cote + eleves 350, 391, 351, 101, 354
#   - cote - eleves 143, 136, 102
coord.dim_1   = ind$coord[,1]
cor.dim_1     = ind$cor[,1]
cos2.dim_1    = ind$cos2[,1]
contrib.dim_1 = ind$contrib[,1]
display.dim_1 = cbind(coord.dim_1, contrib.dim_1, cos2.dim_1, cor.dim_1)


### Visualisation du Cos2 et Contribution des individuos sur l'axe 1
fviz_cos2(res.pca, choice = "ind", axes = 1, top = 30)
fviz_contrib(res.pca, choice = "ind", axes = 1, top = 30)


### Analyse par rapport aux individuos - Dimension 2
#   - cote + eleves 101, 30, 130, 350
#   - cote - eleves 354, 362, 172
coord.dim_2   = ind$coord[,2]
cor.dim_2     = ind$cor[,2]
cos2.dim_2    = ind$cos2[,2]
contrib.dim_2 = ind$contrib[,2]
display.dim_2 = cbind(coord.dim_2, contrib.dim_2, cos2.dim_2, cor.dim_2)


### Visualisation du Cos2 et Contribution des individuos sur l'axe 2
fviz_cos2(res.pca, choice = "ind", axes = 2, top = 30)
fviz_contrib(res.pca, choice = "ind", axes = 2, top = 30)


### Visualisation du Cos2 et Contribution des individuos sur les axes 1 et 2
fviz_cos2(res.pca, choice = "ind", axes=1:2, top = 30)
fviz_contrib(res.pca, choice = "ind", axes=1:2, top = 30)


### Visualisation des variables selon leurs cos2
fviz_pca_ind(res.pca, select.ind = list(cos2 = 30), pointsize = "cos2", col.ind = 'cos2', gradient.cols = grt.clrs, repel = TRUE)
fviz_pca_ind(res.pca, select.ind = list(contrib = 40), pointsize = "contrib", col.ind = 'contrib', gradient.cols = grt.clrs, repel = TRUE)


### Visualisation des variables et individuos
fviz_pca(res.pca)







