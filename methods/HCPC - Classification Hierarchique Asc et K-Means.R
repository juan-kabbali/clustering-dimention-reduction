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


### Hierarchical Clustering on Principal Components 
#   * CAH mixte CAH et k-Means
#   * nb.clust egale -1 pour laiser decider automatiquement a la method le nombre de classes
res.hcpc<-HCPC(res.pca, graph = TRUE, consol = TRUE, nb.clust = -1)


### Visualiser le dendrogramme
fviz_dend(res.hcpc,                      
          cex = 0.7,                     # Taille du text
          palette = "jco",               # Palette de couleur ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
          rect_border = "jco",           # Couleur du rectangle
          labels_track_height = 0.8)     # Augment l'espace pour le texte


### le dendrogramme suggere 4 clusters
#   * on utilise nb.clust pour definir le nombre de clusters
res.hcpc<-HCPC(res.pca, graph = TRUE, consol = TRUE, nb.clust = 4)  


# visualiser la carte factorielle avec les individus colores selon les classes
fviz_cluster(res.hcpc,                
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE,  # Montre le centre des clusters
             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Countries clusters from PCA")


# Explorer les resultats
res.hcpc                # connaitre les resultats disponibles
res.hcpc$data.clust     # jeu de donnees initial + typologie
res.hcpc$desc.var       # description des classes par les variables qualis et quantis
res.hcpc$desc.axes      # description par les axes fatoriels
res.hcpc$desc.ind       # description par les individus
res.hcpc$desc.ind$para  # description par les individus - parangons 


# Conclusions

# Nous avons constate qu'avec le nombre de classes automatique de la methode HCPC, n'est pas toujours le plus
# adecat pour certaines cases et qu'a travers du dendograme, nous pouvons reperer facilement le nombre de classes
# ou clusters qu'on voudrais comme resultat. 

# L'interpretation de chaque cluster depend des resultat obtenus dans le point precedant "ACP"

# On voit clairement que les pays ont ete regroupes a 4 clases où chaque classe represente un ensemble de pays 
# proches dans l'espace factorielle du resulta de l'ACP qui resume 39 dimmentions. La premiere classe "blue" 
# regroupe tous les pays qui ont des bonnes systemes d'eau, d'hygiene et d'assainissement et qui ont un system 
# economique plus forte. La seconde classe "jeune" regroupe tous les pays intermediaeres par rappors a ses 
# systemes d'eau, d'hygiene et d'assainissement  et de ses niveaux economiques. La troisieme classe regrupe
# les pays plus defavorises ou il n'y a pas de bonnes systemes d'eau, d'hygiene et d'assainissement et 
# qui n'ont pas non plus de fortes systemes economiques, en plus, cette classe nous montre l'ensemble de pays
# qui ont de taux de mortalite infantiles et maternnailles eleves. Finalement la quatrieme classe "rouge" 
# represent les outliers mais grace a l'interpretation de l'ACP, on peut dire que ces pays sont ceux qui ont 
# les plus gros nombre de morts d'enfatns - neonatal - et de mortalite maternelle (en chiffres net)

# Dire quelque chose par rappor aux pays developes en developement et sousdevelopes
