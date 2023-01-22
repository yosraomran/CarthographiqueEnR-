# =============================================
# 
# OBJET : INTRODUCTION A L'ANALYSE SPATIALE AVEC R
# 
# Objectif: Petite étude stat-SIG-carto
# auteur : Nicolas Lambert, Timothée Giraud, Claude Grasland
# version : 1.0 (oct 2014)
#
# ===============================================

# ----------------------------------------------------
# R, UN LANGAGE POUR LES STATS
# ----------------------------------------------------

setwd("- votre repertoire de travail - ")
donnees<-read.csv( "data/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",encoding="latin1",)
head(donnees)

mean(donnees$POPTO2010)
sd(donnees$POPTO2010)
summary(donnees$POPTO2010)
plot.new()
boxplot(donnees$POPTO2010,main="Population totale en 2010", horizontal=TRUE)

# ----------------------------------------------------
# MAIS DANS R, ON PEUT AUSSI GERRE LES OBJETS SPATIAUX
# ----------------------------------------------------

# On charge deux packages

library(maptools) # package pour lire et gerer les objets spatiaux
library(rgeos) # http://trac.osgeo.org/geos/ (Geometry Engine Open Source)

# Ouverture de deux couches

fdcOri<-"geom/Tunisie_snuts4.shp"
fdcOri2<-"geom/sebkhas.shp"
delegations<-readShapeSpatial(fdcOri)
sebkhas<-readShapeSpatial(fdcOri2)

# Affichage

plot(delegations, col="#CCCCCC")
plot(sebkhas, add=T,col="blue")

# Tester le shp
getinfo.shape(fdcOri)
class(delegations)
head(gIsValid(delegations, byid = TRUE, reason=TRUE))

# Acceder à la table attributaire
head(delegations@data)

# ---------------------------------------
# GRACE A GEOS, DANS R, ON PEUT AUSSI FAIRE DU SIG
# ---------------------------------------

#extraction d'un polygone par simple requete
poly1<-delegations[delegations@data$id=="TS3234",]
plot(poly1,col="black", add=T)

#Extraction des contours (gBoundary)
b = gBoundary(poly1)
plot(b, col="red",lwd=3,add=T)

#buffer (gBuffer)
buff<-gBuffer(poly1, byid=TRUE, id=NULL, width=20000, quadsegs=5, capStyle="ROUND",joinStyle="ROUND", mitreLimit=1.0)
plot(buff,add=TRUE,col="yellow")
plot(poly1,col="black", add=T)

#centroide (gCentroid)
centres<-gCentroid(poly1, byid=TRUE, id = NULL)
plot(centres,col="red",add=T,lwd=4)
head(centres@coords)

#Aggregation des géométries avec gUnaryUnion
head(delegations@data)
buff<-gBuffer(delegations, byid=TRUE, id=NULL, width=1, quadsegs=5, capStyle="ROUND",joinStyle="ROUND", mitreLimit=1.0)
gouvernorats<-gUnaryUnion(buff,id = delegations@data$id_snuts3)
regions<-gUnaryUnion(buff, id = delegations@data$id_snuts2)
macro<-gUnaryUnion(buff, id = delegations@data$id_snuts1)
country<-gUnaryUnion(buff, id = delegations@data$id_snuts0)
par(mfrow=c(1,5))
plot(delegations)
title(main="Délégations")
plot(gouvernorats)
title(main="Gouvernorats")
plot(regions)
title(main="Regions")
plot(macro)
title(main="Zones")
plot(country)
title(main="Pays")
par(mfrow=c(1,1))


# ---------------------------------------
# EXEMPLE D'UTILISATION RAPIDE
# Existe t il une relation entre l'indice de développement régional
# Et la distance à la côte en Tunise
# ---------------------------------------

# ----------------------------------------------------------
# Etape 1 : Ouverture du tableau de données

donnees<-read.csv( "data/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",encoding="latin1",)
head(donnees)
# simplification du tableau (extraction des variables pertinentes)
donnees<-donnees[,c("del","del_nom","IDRVA2011")]
head(donnees)

# ----------------------------------------------------------
# etape 2 : créer une nouvelle variable : distance à la côte

# Affichage de la cote et des centroides
centres<-readShapeSpatial("geom/Tunisie_snuts4_centres.shp")
coast<-readShapeSpatial("geom/coast.shp")
plot(coast,col="red", lwd=1.5)
plot(centres,add=TRUE)

# calcul distance points -> ligne (gDistance)
dist<-gDistance(coast,centres,byid=TRUE)
dist<-data.frame(centres@data$id,dist)
colnames(dist)<-c("id","dist")
head(dist)

# Jointure entre les deux tableaux et selections des colonnes (match)
mydata = data.frame(dist, donnees[match(dist[,"id"], donnees[,"del"]),])
mydata<-mydata[,c("id","del_nom","IDRVA2011","dist")]
colnames(mydata)<-c("id","nom","idr","dist")
head(mydata)

# ----------------------------------------------------------
# etape 3 : Analyse statistique

# Variable quantitative à expliquer (Y) : idr
# Variable quantitative explicative (X) : dist (en log)
mydata$logDist<-log(mydata$dist)

# résumé et visualisation stat des variables
summary(mydata$idr)
summary(mydata$logDist)

plot.new()
par(mfrow=c(2,2))
hist(mydata$idr,main="idr",breaks=10)
hist(mydata$logDist,main="dist à la côte (log)",breaks=10)
boxplot(mydata$idr,main="idr", horizontal=TRUE)
boxplot(mydata$logDist,main="dist à la côte (log)", horizontal=TRUE)

# Etude de la relation entre X et Y
X <- mydata$logDist
Y <- mydata$idr
par(mfrow=c(1,1))
plot(X,Y, main="Relation entre X et Y", xlab="distance à la côte (log)",  ylab="Indice de développement régional", type="p",  pch=20,   cex=0.7)   

cor(X,Y)
cor.test(X,Y) # une relatio est signification si p-value < 0.05 (5% d'erreur)
MonModele <- lm(Y~X)
summary(MonModele)
names(MonModele)
abline(MonModele,  col="red")

# Calcul des résuidus
mydata$Yres<-MonModele$residuals
mydata$Yres_std<-mydata$Yres/(sd(mydata$Yres))
head(mydata)

# ----------------------------------------------------------
# etape 4 : Cartographie

library(RColorBrewer) # jeu de couleurs
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE)
library(classInt) # methodes de discretisation

# chargement du fond de carte
fdc <- readShapeSpatial("geom/Tunisie_snuts4.shp")
# id = 1ere col
codecarto<-names(fdc@data)[1]
# jointure
fdc@data = data.frame(fdc@data, mydata[match(fdc@data[,codecarto], mydata[,"id"]),])
# discretisation
distr<-c(-1000,-2,-1,-0.5,0,0.5,1,2,1000)
#distr <- classIntervals(fdc@data$Yres_std,5,style="quantile")$brks
# Choix des couleurs
colours <- brewer.pal(8,"RdBu")
# Affectation des couleurs aux classes
colMap <- colours[(findInterval(fdc@data$Yres_std,distr,all.inside=TRUE))]
# Plot
plot(fdc, col=colMap,border="#000000",lwd=0.2)
legend(x="bottomleft", legend=leglabs(round(distr,2),over="sup. ",under="inf. "), fill=colours, bty="n",pt.cex=1,cex=0.7,title="residus standardisés")
title(main="Residus standardisés",sub="Auteur: Nicolas Lambert, CNRS, 2014",cex.sub=0.7)

