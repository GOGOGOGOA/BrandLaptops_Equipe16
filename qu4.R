# question 4 : Étudiez les liens entre les variables qua-
# litatives et réaliser une AFC dans le cas de dépendance

Dataset=read.csv("Laptop2024.csv")
summary(Dataset)

# quels sont les variables qualitatives ?

# brand, (Model, processor_brand), processor_tier,
# primary_storage_type, (secondary_storage_type,
# gpu_brand, gpu_type, is_touch_screen,) OS

# on fait que pour les 4 qu'on considère importantes dans la qu1

# connaitre le nbr de modalité d'une variable
# length(unique(Dataset$la_variable))

# premièrement on calcul le chi-deux :
# X2 = for (i in 1:k){for (j in 1:l){(
#  (nij - tij)**2 / tij
#)}}
# où ti,j = n×fi,.×f.,j est l'effectif
# théorique de la case (i,j).
# puis on compare avec la table des fractiles de la loi
# du chi-deux avec alpha = 5% (la table donne le seuil)
# si X2 > seuil : dépendance
# si X2 < seuil indépendance

# si dépendance on réalise une AFC

######################################################
########### pour brand et processor_tier #############
######################################################

# tableau des contingences
tab=table(Dataset$brand, Dataset$processor_tier)
# tableau des contingences avec effectifs marginaux
tab0=addmargins(tab)
# affichage
tab0

# tableau de contingences avec fréquences marginales
# fréquences observées
tabfreq=tab0/tab0[27,16]
tabfreq

# pour bien voir le tableau
write.csv(tabfreq, "tabfreq.csv")
freqcsv=read.csv("tabfreq.csv")

# Calcul des fréquences theoriques 
freq_theo = tabfreq
for (i in 1:26){
  for (j in 1:15){
    freq_theo[i,j]=tabfreq[i,16]*tabfreq[27,j]
  }
}

# Calcul des effectifs théoriques
eff_theo = freq_theo*tab0[27,16]
eff_theo

# 4b Calcul du khi-deux
diff=((eff_theo-tab0)**2)/eff_theo
khi_2=sum(diff[1:26,1:15])
khi_2

# khi_2 = 1678.627
# Degres de liberte : (26-1)*(15-1)=350
# Seuil : 394.626
# les variables sont liées

####################### AFC ##########################

# Réalisation de l'AFC
res.afc=CA(tab, graph = TRUE)
res.afc$eig

# Affichage des résultats
print(res.afc)

# contribution
res.afc$row$contrib
res.afc$col$contrib

# Visualisation des résultats
# Graphique des individus
fviz_ca_row(res.afc, repel = TRUE)

# Graphique des variables (colonnes)
fviz_ca_col(res.afc, repel = TRUE)

# Biplot des lignes et colonnes
fviz_ca_biplot(res.afc, repel = TRUE)
fviz_ca_biplot(res.afc, select.row = list(cos2 = 0.5), select.col = list(cos2 = 0.5), repel = TRUE, labelsize = 5)

print(biplot)
######################################################
######## pour brand et primary_storage_type ##########
######################################################

tab1=table(Dataset$brand, Dataset$primary_storage_type)
tab01=addmargins(tab1)
tab01

tabfreq1=tab01/tab01[27,3]
tabfreq1

# pour bien voir le tableau
write.csv(tabfreq1, "tabfreq1.csv")
freqcsv1=read.csv("tabfreq1.csv")

# Calcul des frequences theoriques 

freq_theo1 = tabfreq1
for (i in 1:26){
  for (j in 1:2){
    freq_theo1[i,j]=tabfreq1[i,3]*tabfreq1[27,j]
  }
}

# Calcul des effectifs theoriques
eff_theo1 = freq_theo1*tab01[27,3]

# 4b Calcul du khi-deux
diff1=((eff_theo1-tab01)**2)/eff_theo1
khi_21=sum(diff1[1:26,1:2])
khi_21

# khi_2 = 175.5126
# Degre de liberte : (26-1)*(3-1)=25
# Seuil : 37.652
# les variables sont liées mais il n'y a qu'un seul axe

######################################################
############### pour brand et OS #####################
######################################################

tab2=table(Dataset$brand, Dataset$OS)
tab02=addmargins(tab2)
tab02

tabfreq2=tab02/tab02[27,8]
tabfreq2

# pour bien voir le tableau
write.csv(tabfreq2, "tabfreq3.csv")
freqcsv2=read.csv("tabfreq2.csv")

# Calcul des frequences theoriques 

freq_theo2 = tabfreq2
for (i in 1:26){
  for (j in 1:7){
    freq_theo2[i,j]=tabfreq2[i,8]*tabfreq2[27,j]
  }
}

# Calcul des effectifs theoriques
eff_theo2 = freq_theo2*tab02[27,8]

# 4b Calcul du khi-deux
diff2=((eff_theo2-tab02)**2)/eff_theo2
khi_22=sum(diff2[1:26,1:7])
khi_22

# khi_2 = 2318.835
# Degré de liberté : (26-1)*(7-1)=150
# Seuil : 179.581
# les variables sont liées

# Réalisation de l'AFC
res.afc2=CA(tab2, graph = TRUE)
res.afc2$eig

# Affichage des résultats
print(res.afc2)

# contribution
res.afc2$row$contrib
res.afc2$col$contrib

# Visualisation des résultats
# Graphique des individus
fviz_ca_row(res.afc2, repel = TRUE)

# Graphique des variables (colonnes)
fviz_ca_col(res.afc2, repel = TRUE)

# Biplot des lignes et colonnes
fviz_ca_biplot(res.afc2, repel = TRUE)
fviz_ca_biplot(res.afc2, select.row = list(cos2 = 0.5), select.col = list(cos2 = 0.5), repel = TRUE, labelsize = 5)

print(biplot)

######################################################
#### pour processor_tier et primary_storage_type #####
######################################################

tab3=table(Dataset$processor_tier, Dataset$primary_storage_type)
tab03=addmargins(tab3)
tab03

tabfreq3=tab03/tab03[16,3]
tabfreq3

# pour bien voir le tableau
write.csv(tabfreq3, "tabfreq3.csv")
freqcsv3=read.csv("tabfreq3.csv")

# Calcul des frequences theoriques 

freq_theo3 = tabfreq3
for (i in 1:15){
  for (j in 1:2){
    freq_theo3[i,j]=tabfreq3[i,3]*tabfreq3[16,j]
  }
}

# Calcul des effectifs theoriques
eff_theo3 = freq_theo3*tab03[16,3]

# 4b Calcul du khi-deux
diff3=((eff_theo3-tab03)**2)/eff_theo3
khi_23=sum(diff3[1:15,1:2])
khi_23

# khi_2 = 97.86439
# Degre de liberte : (15-1)*(2-1)=14
# Seuil : 23.685
# les variables sont liées : mais il n'y a qu'un seul axe

######################################################
############# pour processor_tier et OS ##############
######################################################

tab4=table(Dataset$processor_tier, Dataset$OS)
tab04=addmargins(tab4)
tab04

tabfreq4=tab04/tab04[16,8]
tabfreq4

# pour bien voir le tableau
write.csv(tabfreq4, "tabfreq4.csv")
freqcsv4=read.csv("tabfreq4.csv")

# Calcul des frequences theoriques 

freq_theo4 = tabfreq4
for (i in 1:15){
  for (j in 1:7){
    freq_theo4[i,j]=tabfreq4[i,8]*tabfreq4[16,j]
  }
}

# Calcul des effectifs theoriques
eff_theo4 = freq_theo4*tab04[16,8]

# 4b Calcul du khi-deux
diff4=((eff_theo4-tab04)**2)/eff_theo4
khi_24=sum(diff4[1:15,1:7])
khi_24

# khi_2 = 1543.959
# Degre de liberte : (15-1)*(7-1)=84
# Seuil : 106.395
# les variables sont liées

# Réalisation de l'AFC
res.afc4=CA(tab4, graph = TRUE)
res.afc4$eig

# Affichage des résultats
print(res.afc4)

# Visualisation des résultats
# Graphique des individus
fviz_ca_row(res.afc4, repel = TRUE)

# Graphique des variables (colonnes)
fviz_ca_col(res.afc4, repel = TRUE)

# Biplot des lignes et colonnes
fviz_ca_biplot(res.afc4, repel = TRUE)

print(biplot)


######################################################
########## pour primary_storage_type et OS ###########
######################################################

tab5=table(Dataset$primary_storage_type, Dataset$OS)
tab05=addmargins(tab5)
tab05

tabfreq5=tab05/tab05[3,8]
tabfreq5

# pour bien voir le tableau
write.csv(tabfreq5, "tabfreq5.csv")
freqcsv5=read.csv("tabfreq5.csv")

# Calcul des frequences theoriques 

freq_theo5 = tabfreq5
for (i in 1:2){
  for (j in 1:7){
    freq_theo5[i,j]=tabfreq5[i,8]*tabfreq5[3,j]
  }
}

# Calcul des effectifs theoriques
eff_theo5 = freq_theo5*tab05[3,8]

# 4b Calcul du khi-deux
diff5=((eff_theo5-tab05)**2)/eff_theo5
khi_25=sum(diff5[1:2,1:7])
khi_25

# khi_2 = 183.3157
# Degre de liberte : (2-1)*(7-1)=6
# Seuil : 12.592
# les variables sont liées mais il n'y a qu'un seul axe

