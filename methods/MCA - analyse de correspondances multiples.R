# Packages
install.packages(c("FactoMineR", "factoextra"))
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


### Selectioner les vars actives.
#   - G3 sont des vars suplementaires car ils dependent des autres vars
d.active.names = c("school", "sex", "address", "famsize", "Pstatus", "Mjob", "Fjob", "reason", "guardian",
                   "schoolsup", "famsup", "paid", "activities", "nursery", "higher", "internet", "romantic", "G3");
d.active = d[, d.active.names]
str(d.active)


###########################################################################
### ACM - Analyse des correspondances multiples
#   * d.actives   = jeu de données
#   * scale.unit  = si TRUE les données sont standardisées 
#   * ncp         = nombre de dimensions affichés
#   * quanti.sup  = on utilise G1 G2 et G3
res.mca = MCA(d.active, graph = TRUE, ncp = 5, quanti.sup = 18)
res.mca


### Obtenir les valeures propres
eig.val = get_eigenvalue(res.mca) 
eig.val


### Graphique des valeurs propres avec la fonction
fviz_eig(res.mca, addlabels = TRUE, ylim = c(0, 10))


### Visualisation des
#    * individus -- points
#    * modalites -- triangles
fviz_mca_biplot (res.mca, repel = TRUE)


### Visualisation de la correlation des variables avec les axes
fviz_mca_var(res.mca, choice = "mca.cor", repel = TRUE)


###########################################################################
### Obtenir les resultats pour les profiles lignes des dimentions du MCA
#   * coord
#   * cor
#   * cos2
#   * contrib 
ind = get_mca_ind(res.mca)


###########################################################################
### Analyse par rapport aux profiles lignes - Dimension 1
#   - cote + 
#   - cote - les autres
coord.dim_1   = ind$coord[,1]
cor.dim_1     = ind$cor[,1]
cos2.dim_1    = ind$cos2[,1]
contrib.dim_1 = ind$contrib[,1]
display.dim_1 = cbind(coord.dim_1, contrib.dim_1, cos2.dim_1, cor.dim_1)


### Visualisation des profiles lignes selon leurs cos2
fviz_mca_ind(res.mca, col.var = 'cos2', gradient.cols = grt.clrs, repel = TRUE)


### Visualisation du Cos2 et Contribution des variables sur l'axe 1
fviz_cos2(res.mca, choice = "ind", axes = 1, top = 30)
fviz_contrib(res.mca, choice = "ind", axes = 1, top = 30)


###########################################################################
### Analyse par rapport aux profiles lignes - Dimension 2
#   - cote + 
#   - cote - les autres
coord.dim_2   = ind$coord[,2]
cor.dim_2     = ind$cor[,2]
cos2.dim_2    = ind$cos2[,2]
contrib.dim_2 = ind$contrib[,2]
display.dim_2 = cbind(coord.dim_2, contrib.dim_2, cos2.dim_2, cor.dim_2)


### Visualisation du Cos2 et Contribution des profiles lignes sur l'axe 2
fviz_cos2(res.mca, choice = "ind", axes = 2, top = 30)
fviz_contrib(res.mca, choice = "ind", axes = 2, top = 30)


### Visualisation du Cos2 et Contribution des profiles lignes sur les axes 1 et 2
fviz_cos2(res.mca, choice = "ind", axes=1:2, top = 30)
fviz_contrib(res.mca, choice = "ind", axes=1:2, top = 30)


###########################################################################
### Obtenir les resultats pour les profiles colonnes des dimentions du MCA
#   * coord
#   * cor
#   * cos2
#   * contrib 
var = get_mca_var(res.mca)
var


###########################################################################
### Analyse par rapport aux profiles colonnes - Dimension 1
#   - cote + 
#   - cote - les autres
coord.dim_1   = var$coord[,1]
cor.dim_1     = var$cor[,1]
cos2.dim_1    = var$cos2[,1]
contrib.dim_1 = var$contrib[,1]
display.dim_1 = cbind(coord.dim_1, contrib.dim_1, cos2.dim_1, cor.dim_1)


### Visualisation du Cos2 et Contribution des profiles colonnes sur l'axe 1
fviz_cos2(res.mca, choice = "var", axes = 1)
fviz_contrib(res.mca, choice = "var", axes = 1)


###########################################################################
### Analyse par rapport aux profiles colonnes - Dimension 2
#   - cote + 
#   - cote - les autres
coord.dim_2   = var$coord[,2]
cor.dim_2     = var$cor[,2]
cos2.dim_2    = var$cos2[,2]
contrib.dim_2 = var$contrib[,2]
display.dim_2 = cbind(coord.dim_2, contrib.dim_2, cos2.dim_2, cor.dim_2)


### Visualisation du Cos2 et Contribution des profiles colonnes sur l'axe 2
fviz_cos2(res.mca, choice = "var", axes = 2)
fviz_contrib(res.mca, choice = "var", axes = 2)


### Visualisation du Cos2 et Contribution des profiles colonnes sur les axes 1 et 2
fviz_cos2(res.mca, choice = "var", axes=1:2, top = 30)
fviz_contrib(res.mca, choice = "var", axes=1:2, top = 30)


### Visualisation du Cos2 et Contribution des profiles colonnes sur toutes les dimensions
corrplot(var$cos2, is.corr=FALSE)
corrplot(var$contrib, is.corr=FALSE)


### Visualisation des variables selon leurs cos2
fviz_pca_ind(res.mca, select.ind = list(cos2 = 30), pointsize = "cos2", col.ind = 'cos2', gradient.cols = grt.clrs, repel = TRUE)
fviz_pca_ind(res.mca, select.ind = list(contrib = 40), pointsize = "contrib", col.ind = 'contrib', gradient.cols = grt.clrs, repel = TRUE)


### Visualisation des variables et individuos
fviz_mca(res.mca)


### Description des dimensions et mise en evidence des points qui contribuent le plus aux axes 
res.desc <- dimdesc(res.mca, axes = c(1, 2))
res.desc[[1]]
res.desc[[2]]





