# Packages
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
library("corrplot")


### Definir les couleurs gradient 
grt.clrs = c("green","blue","red")


### Importation de donnees.
#   - Chaque ligne est un patient
d = readxl::read_xlsx('assets/diagnosis.xlsx')
row.names(d) = d$id_patient
head(d)
str(d)

### Selectioner les vars actives.
#   - Temperature c'est suplementaire
d.active.names = c("temperature", "nausea", "lumbar-pain", "urine-pushing", "micturition-pains",
                   "burning-urethra", "inflammation-urinary-bladder", "nephritis-renal-pelvis-origin")
d.active = d[, d.active.names]


#d.active$Cruise[d.active$Cruise == 0] = "no"
#d.active$Cruise[d.active$Cruise == 1] = "yes"
#d.active$Sound[d.active$Sound == 0] = "no"
#d.active$Sound[d.active$Sound == 1] = "yes"
#d.active$Leather[d.active$Leather == 0] = "no"
#d.active$Leather[d.active$Leather == 1] = "yes"
#d.active[, c("Cruise", "Sound", "Leather")] = as.factor(d.active[, c("Cruise", "Sound", "Leather")])
#d.active$wife.age[d.active$wife.age >= 15 & d.active$wife.age < 20] = "15-19"
#d.active$wife.age[d.active$wife.age >= 20 & d.active$wife.age < 25] = "20-24"
#d.active$wife.age[d.active$wife.age >= 25 & d.active$wife.age < 30] = "25-29"
#d.active$wife.age[d.active$wife.age >= 30 & d.active$wife.age < 35] = "30-34"
#d.active$wife.age[d.active$wife.age >= 35 & d.active$wife.age < 40] = "35-39"
#d.active$wife.age[d.active$wife.age >= 40 & d.active$wife.age < 45] = "40-44"
#d.active$wife.age[d.active$wife.age >= 45 & d.active$wife.age <= 50] = "45-50"
#for (col in colnames(d.active)){
#  d.active[[col]] = as.factor(d.active[[col]])
#}
#d.active$contraceptive.method.used = as.integer(d.active$contraceptive.method.used)


###########################################################################
### ACM - Analyse des correspondances multiples
#   * d.actives   = jeu de données
#   * scale.unit  = si TRUE les données sont standardisées 
#   * ncp         = nombre de dimensions affichés
#   * quanti.sup  = on utilise temperature
res.mca = MCA(d.active, graph = TRUE, ncp = 5, quanti.sup = 1)
res.mca


### Obtenir les valeures propres
eig.val = get_eigenvalue(res.mca) 
eig.val


### Graphique des valeurs propres
fviz_eig(res.mca, addlabels = TRUE, ylim = c(0, 40))


### Visualisation des
#    * individus -- points
#    * modalites -- triangles
fviz_mca_biplot (res.mca, repel = TRUE, select.ind = list(cos2 = 80))


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
#   - cote - met en evidence les profile lignes 3, 6, 8, 12, 13, 15, 16, 20, 23, 29, 33, 35, 38, 41, 42, 51, 53, 58, 1
#   - cote + les autres

coord         = ind$coord[,1]
cor           = ind$cor[,1]
cos2          = ind$cos2[,1]
contrib       = ind$contrib[,1]
display       = cbind(coord, contrib, cos2, cor)


### Visualisation du Cos2 et Contribution des variables sur l'axe 1
fviz_cos2(res.mca, choice = "ind", axes = 1, top = 50)
fviz_contrib(res.mca, choice = "ind", axes = 1, top = 50)


###########################################################################
### Analyse par rapport aux profiles lignes - Dimension 2
#   - cote + 76, 77, 82, 88, 92, 96, 104, 109, 113, 118
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


### representation bidimentionnelles des profiles lignes selon leurs cos2
fviz_mca_ind(res.mca, col.var = 'cos2', gradient.cols = grt.clrs, repel = TRUE)


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
fviz_pca_ind(res.mca, select.ind = list(cos2 = 100), pointsize = "cos2", col.ind = 'cos2', gradient.cols = grt.clrs, repel = TRUE)
fviz_pca_ind(res.mca, select.ind = list(contrib = 40), pointsize = "contrib", col.ind = 'contrib', gradient.cols = grt.clrs, repel = TRUE)


### Visualisation des variables et individuos
fviz_mca(res.mca)


### Description des dimensions et mise en evidence des points qui contribuent le plus aux axes 
res.desc <- dimdesc(res.mca, axes = c(1, 2))
res.desc[[1]]
res.desc[[2]]





