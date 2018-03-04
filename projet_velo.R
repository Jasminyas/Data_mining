#Packages nécessaires pour faire de beaux graphiques

options(scipen=999) #Option pour l'affichage des valeurs en numérique et non en scientifique

#install.packages("ggplot")
#install.packages("ggthemes")
#install.packages("plotly")

#Charger les packages

require(ggplot2)
require(ggthemes)
require(plotly)

#Package pour la manipulation de tables
#install.packages("data.table")
library(data.table)

#Charger les données
path = "C:/Users/Astry/Documents/imsd/Data Mining"

caracteristiques = read.csv(paste(path, "/caracteristiques_2015.csv", sep=""), sep=",", 
                            stringsAsFactors = FALSE)

lieux = read.csv(paste(path, "/lieux_2015.csv", sep=""), sep=",", 
                 stringsAsFactors = FALSE)

usagers =read.csv(paste(path, "/usagers_2015.csv", sep=""), sep=",", 
                  stringsAsFactors = FALSE)

vehicules = read.csv(paste(path, "/vehicules_2015.csv", sep=""), sep=",", 
                     stringsAsFactors = FALSE)

#Merge de lieux et caracteristiques
cara_lieu = merge(caracteristiques, lieux, by ="Num_Acc")

###########################################################

#Creation d'une nouvelle table à partir de vehicules_2015 afin d'avoir un 
#Num_Acc unique

##########################################################

#Recodage des variables
#catv : segmentation en 4 groupes : velo(1),vehicule ordinaire (2), 2 roues(3),
#vehicule lourd (4), autres(5)
vehicules_orginal = vehicules 
vehicules= vehicules_orginal
vehicules$catv[vehicules$catv >= 7 & vehicules$catv<=13] = 2 # Vehicules ordinaires
vehicules$catv[vehicules$catv>=30 & vehicules$catv<= 34] = 3 # 2 roues
vehicules$catv[(vehicules$catv>=37 & vehicules$catv<= 40) | 
                 (vehicules$catv>13 & vehicules$catv<= 18) ] = 4 # Vehicules lourds
vehicules$catv[vehicules$catv > 4] = 5 # Autres

#Ajout des colonnes "Nombre de vehicules ordinaires", "nombre de 2 roues", etc
vehicules_colCat = vehicules
vehicules_colCat$nbrVelo[vehicules_colCat$catv==1] = 1 #Velo
vehicules_colCat$nbrVehiO[vehicules_colCat$catv==2] = 1 #Vehicule ordinaire
vehicules_colCat$nbr2R[vehicules_colCat$catv==3] = 1 #2 roues
vehicules_colCat$nbrVehiL[vehicules_colCat$catv==4] = 1 #vehicules lourds
vehicules_colCat$nbrAutres[vehicules_colCat$catv==5] = 1 #Autres

#Recodage de obsm (Obstacle mobile heurté) en 3 catégories : Piéton(1), Véhicule(2), Autres(3)
# Que faire des 0 ?
vehicules_colCat$obsm [vehicules_colCat$obsm>2] = 3 
# Lorsque que deux vehicules sont présents dans un même accident obsm = 2
vehicules_colCat$obsm [duplicated(vehicules_colCat$Num_Acc, fromLast = FALSE)] = 2
vehicules_colCat$obsm [duplicated(vehicules_colCat$Num_Acc,fromLast = TRUE )] = 2

#Recodage de manv en 4 variables :
vehicules_colCat$manv[vehicules_colCat$manv < 11] = 4 #Autre
vehicules_colCat$manv[vehicules_colCat$manv > 10 &  vehicules_colCat$manv <= 14] = 1 # Déportation
vehicules_colCat$manv[vehicules_colCat$manv > 14 &  vehicules_colCat$manv <= 16] = 2 # Tournant
vehicules_colCat$manv[vehicules_colCat$manv > 16 &  vehicules_colCat$manv <= 18] = 3 # Dépassement
vehicules_colCat$manv[vehicules_colCat$manv  > 4 ] = 4 #Autre

#Ajout des colonnes "Deportation", "Tournant", etc
vehicules_colCat$manDep[vehicules_colCat$manv==1] = 1 #Deportation
vehicules_colCat$manTour[vehicules_colCat$manv==2] = 1 #Tournant
vehicules_colCat$manDepa[vehicules_colCat$manv==3] = 1 #Dépassement
vehicules_colCat$manAutre[vehicules_colCat$manv==4] = 1 #Autre


#Suppression de la variable "choc" ? A-t-elle vraiment un interêt aux vues de l'emcombrement
#que son recodage génére ?
#Suppression dans anciennes variables recodées
vehicules_new = vehicules_colCat
vehicules_new$catv = NULL
vehicules_new$num_veh = NULL
vehicules_new$choc = NULL
vehicules_new$manv = NULL

#Suppression de la variable senc  (Sens de la circulation  selon numero adresse croissant
#décroissant) car incohénrences dans le relevé et n'apporte pas d'information pertinente 
#idem pour occutc

#Creation de la table à Num_Acc unique
vehicules_final = vehicules_new[,c ("Num_Acc",  "obsm")]
vehicules_final = unique(vehicules_final)
vehicules_final$Num_Acc[duplicated(vehicules_final$Num_Acc)]

#Problème avec le Obs, Lorsque que deux vehicules sont présents dans un même accident prendre le max de obs
#la deuxième valeur étant 0
#vehicules_final$obs [duplicated(vehicules_final$Num_Acc)] = aggregate(obs ~ Num_Acc, vehicules_final, max)

#Fusion des lignes de même Num_Acc
#DT <- data.table(vehicules_new)
#vehicules_final$nbrVehiO = DT[, sum(nbrVehiO), by = Num_Acc]

#Somme des vehicules ordinaires :
somme = aggregate(nbrVehiO ~ Num_Acc,
                  vehicules_new, sum, na.action=na.pass)
vehicules_final=  merge(somme, vehicules_final, by ="Num_Acc")
#Somme des vehicules lourds :
somme = aggregate(nbrVehiL ~ Num_Acc,
                  vehicules_new, sum, na.action=na.pass)
vehicules_final=  merge(somme, vehicules_final, by ="Num_Acc")
#Somme des vehicules velos :
somme = aggregate(nbrVelo ~ Num_Acc,
                  vehicules_new, sum, na.action=na.pass)
vehicules_final=  merge(somme, vehicules_final, by ="Num_Acc")
#Somme des vehicules autres :
somme = aggregate(nbrAutres ~ Num_Acc,
                  vehicules_new, sum, na.action=na.pass)
vehicules_final=  merge(somme, vehicules_final, by ="Num_Acc")
#Somme des vehicules autres :
somme = aggregate(nbr2R ~ Num_Acc,
                  vehicules_new, sum, na.action=na.pass)
vehicules_final=  merge(somme, vehicules_final, by ="Num_Acc")

#######################################################???

#Creation d'une nouvelle table à partir de usagers afin d'avoir un 
#Num_Acc unique

########################################################

#Nettoyage de l'environnement pour libérer de la mémoire et y voir plus clair
remove(vehicules_new)
remove(vehicules_orginal)
remove(vehicules_colCat)

#Création de la base de données finales
database_final =  merge(cara_lieu, vehicules_final, by ="Num_Acc")
database_final =  database_final[database_final$dep==750,] #¯Filtre sur Paris

#Nettoyage de l'environnement pour libérer de la mémoire et y voir plus clair
rm(caracteristiques)
rm(cara_lieu)
rm(lieux)
remove(somme)
remove(usagers)
remove(vehicules_final)