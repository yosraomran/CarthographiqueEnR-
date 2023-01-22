library(maptools)
library(sp)
library(shapefiles)
fdc <- readShapePoly("./geom/Tunisie_snuts4")
summary(fdc)
donnees <- read.csv("./data/tunisie_data_del_2011.csv", header = TRUE, sep = ";", dec = ",", encoding = "latin1")
dim(donnees)

i=match(fdc@data[, "id"], donnees[, "del"])
fdc@data <- data.frame(fdc@data, donnees[i,])
fdc@data$var <- fdc@data$POP692014

var <- as.vector(na.omit(fdc@data$var))
nbclass <- 4
library(classInt)
distr <- classIntervals(var, nbclass, style = "quantile")$brks
distr
library(RColorBrewer)
colours <- brewer.pal(nbclass, "YlOrRd")
colMap <- colours[(findInterval(fdc$var, distr, all.inside = TRUE))]
plot(fdc, col = colMap, border = "black", lwd = 1)
 # legende
 legend(x = "topright", legend = leglabs(round(distr, 2), over = "plus de", under = "moins de"), fill = colours, bty = "n", pt.cex = 1, cex = 0.7, title = "indice de population")
 # Titre
 title(main = "Population des enfants agés entre 6-9ans", cex.sub = 0.7)

#deuxiemeMap1014
fdc@data$var <- fdc@data$POP10142014
fdc@data$var
var <- as.vector(na.omit(fdc@data$var))
nbclass <- 6
library(classInt)
distr <- classIntervals(var, nbclass, style = "quantile")$brks
library(RColorBrewer)
colours <- brewer.pal(nbclass, "YlOrRd")
colMap <- colours[(findInterval(fdc$var, distr, all.inside = TRUE))]
plot(fdc, col = colMap, border = "black", lwd = 1)
 # legende
 legend(x = "topright", legend = leglabs(round(distr, 2), over = "plus de", under = "moins de"), fill = colours, bty = "n", pt.cex = 1, cex = 0.7, title = "indice de population")
 # Titre
 title(main = "Population des enfants agés entre 10-14ans", cex.sub = 0.7)
 
 #les enfants entre 6-9 scolariés
 i=match(fdc@data[, "id"], donnees[, "del"])
fdc@data <- data.frame(fdc@data, donnees[i,])
fdc@data$var <- fdc@data$SCL692014

var <- as.vector(na.omit(fdc@data$var))
nbclass <- 4
library(classInt)
distr <- classIntervals(var, nbclass, style = "quantile")$brks
distr
library(RColorBrewer)
colours <- brewer.pal(nbclass, "YlOrRd")
colMap <- colours[(findInterval(fdc$var, distr, all.inside = TRUE))]
plot(fdc, col = colMap, border = "black", lwd = 1)
 # legende
 legend(x = "topright", legend = leglabs(round(distr, 2), over = "plus de", under = "moins de"), fill = colours, bty = "n", pt.cex = 1, cex = 0.7, title = "indice de population")
 # Titre
 title(main = "Population des enfants agés entre 6-9ans scolariés", cex.sub = 0.7)
  #les enfants entre 6-9 scolariés
 i=match(fdc@data[, "id"], donnees[, "del"])
fdc@data <- data.frame(fdc@data, donnees[i,])
fdc@data$var <- fdc@data$NSCL692014

var <- as.vector(na.omit(fdc@data$var))
nbclass <- 4
library(classInt)
distr <- classIntervals(var, nbclass, style = "quantile")$brks
distr
library(RColorBrewer)
colours <- brewer.pal(nbclass, "YlOrRd")
colMap <- colours[(findInterval(fdc$var, distr, all.inside = TRUE))]
plot(fdc, col = colMap, border = "black", lwd = 1)
 # legende
 legend(x = "topright", legend = leglabs(round(distr, 2), over = "plus de", under = "moins de"), fill = colours, bty = "n", pt.cex = 1, cex = 0.7, title = "indice de population")
 # Titre
 title(main = "Population des enfants agés entre 6-9ans  Non scolariés", cex.sub = 0.7)

#les enfants entre 10-14 scolariés
 i=match(fdc@data[, "id"], donnees[, "del"])
fdc@data <- data.frame(fdc@data, donnees[i,])
fdc@data$var <- fdc@data$SCL10142014

var <- as.vector(na.omit(fdc@data$var))
nbclass <- 4
library(classInt)
distr <- classIntervals(var, nbclass, style = "quantile")$brks
distr
library(RColorBrewer)
colours <- brewer.pal(nbclass, "YlOrRd")
colMap <- colours[(findInterval(fdc$var, distr, all.inside = TRUE))]
plot(fdc, col = colMap, border = "black", lwd = 1)
 # legende
 legend(x = "topright", legend = leglabs(round(distr, 2), over = "plus de", under = "moins de"), fill = colours, bty = "n", pt.cex = 1, cex = 0.7, title = "indice de population")
 # Titre
 title(main = "Population des enfants agés entre 10-14ans  scolariés", cex.sub = 0.7)
 #les enfants entre 10-14 scolariés
 i=match(fdc@data[, "id"], donnees[, "del"])
fdc@data <- data.frame(fdc@data, donnees[i,])
fdc@data$var <- fdc@data$NSCL10142014

var <- as.vector(na.omit(fdc@data$var))
nbclass <- 4
library(classInt)
distr <- classIntervals(var, nbclass, style = "quantile")$brks
distr
library(RColorBrewer)
colours <- brewer.pal(nbclass, "YlOrRd")
colMap <- colours[(findInterval(fdc$var, distr, all.inside = TRUE))]
plot(fdc, col = colMap, border = "black", lwd = 1)
 # legende
 legend(x = "topright", legend = leglabs(round(distr, 2), over = "plus de", under = "moins de"), fill = colours, bty = "n", pt.cex = 1, cex = 0.7, title = "indice de population")
 # Titre
 title(main = "Population des enfants agés entre 10-14 ans  non scolariés", cex.sub = 0.7)

#visualisation 
 
x <- fdc$NSCL10142014
y <- fdc$SCL10142014
# Plot with main and axis titles
# Change point shape (pch = 19) and remove frame.
plot(x, y, main = "Main title",
     xlab = "Population des enfants agés entre 10-14ans ", ylab = "les enfants scolarisés entre 10-14 ans en 2014",
     pch = 15, frame = FALSE)
