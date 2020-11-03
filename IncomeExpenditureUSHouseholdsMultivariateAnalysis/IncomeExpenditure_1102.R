remove(list=ls())
# setwd("D:/Manuscript/Submit R codes")
#----------------------------------------------------------------------------------------------------------------------------------
#Cleaning data
#----------------------------------------------------------------------------------------------------------------------------------
fmli161 <- read.csv("fmli161.csv", header = TRUE, na.string = c(".", "NA"), row.names = 1)
name <- c("HH_CU_Q", "AS_COMP1", "AS_COMP2",
          "BLS_URBN", "CUTENURE",
          "FAM_SIZE",  "NO_EARNR",  "PERSLT18",
          "PERSOT64", "VEHQ", "AGE_REF",
          "EDUC_REF",  "REF_RACE",  "SEX_REF",
          "INC_HRS1", "OCCUCOD1", "FINCBTXM",
          "FSALARYM",  "FRRETIRM","FSMPFRXM",
          "WELFAREM", "INTRDVXM",
          "NETRENTM","RETSURVM",
          "TOTEXPCQ","FOODCQ",
          "FDHOMECQ",  "FDAWAYCQ","HOUSCQ",
          "APPARCQ","TRANSCQ", "HEALTHCQ",
          "ENTERTCQ", "EDUCACQ","READCQ")

name2 <- c("HH_CU_Q_", "AS_C_MP1", "AS_C_MP2",
           "BLS_URBN", "CUTE_URE",
           "FAM__IZE", "NO_E_RNR", "PERS_T18",
           "PERS_T64", "VEHQ_", "AGE_REF_",
           "EDUC0REF", "REF__ACE", "SEX_REF_",
           "INC__RS1", "OCCU_OD1", "FINCB_XM",
           "FSAL_RYM", "FRRE_IRM","FSMP_RXM",
           "WELF_REM", "INTR_VXM",
           "NETR_NTM","RETS_RVM",
           "TOTEXPCQ","FOODCQ",
           "FDHOMECQ", "FDAWAYCQ","HOUSCQ",
           "APPARCQ","TRANSCQ", "HEALTHCQ",
           "ENTERTCQ", "EDUCACQ","READCQ")
#Total of 34 variables selected
fmli <- subset(fmli161, select = name)
flag <- subset(fmli161,select = c(name2 ))

library("mice")

#Try to split the dataset according to if TOTEXPCQ is empty
delete = which(fmli$TOTEXPCQ == 0)
str(which(fmli$TOTEXPCQ == 0))#total of 2171 missing tot expenditure
fmlifull = fmli[-delete,]
#Table 3
str(which(fmlifull$FOODCQ == 0)) #28/4524 = 0.61%
str(which(fmlifull$FDHOMECQ == 0)) #46/4524 = 1.01%
str(which(fmlifull$FDAWAYCQ == 0)) #879/4524 = 19.42%
str(which(fmlifull$HOUSCQ == 0)) #23/4524 = 0.50%
str(which(fmlifull$APPARCQ == 0)) #2168/4524 = 47.92%
str(which(fmlifull$TRANSCQ == 0)) #268/4524 = 5.92%
str(which(fmlifull$HEALTHCQ == 0)) #941/4524 = 20.81%
str(which(fmlifull$ENTERTCQ == 0)) #649/4524 = 14.34%
str(which(fmlifull$EDUCACQ == 0)) #3803/4524 = 84.06%
str(which(fmlifull$READCQ == 0)) #3629/4524 = 80.22%
#Table 4
str(which(is.na(fmlifull[,"INC_HRS1"])))  #1453/4254 = %34
str(fmlifull$INC_HRS1)
str(which(is.na(fmlifull[,"OCCUCOD1"])))  #1453/4254 = %34
str(which(is.na(fmlifull[,"INTRDVXM"])))  #3519/4254 = 82%
str(which(is.na(fmlifull[,"RETSURVM"])))  #3645/4254= 85%
str(which(is.na(fmlifull[,"NETRENTM"])))  #4048/4254 = 95%
str(which(is.na(fmlifull[,"WELFAREM"])))  #4209/4254 = 98%

fmlifull = fmlifull[,-c(21:24)]
fmlifull = fmlifull[,-c(15:16)]
name3 <- c("HH_CU_Q", "AS_COMP1", "AS_COMP2", "BLS_URBN", "CUTENURE", 
           "FAM_SIZE", "NO_EARNR", "PERSLT18", "PERSOT64",  "VEHQ", "AGE_REF",
           "EDUC_REF", "REF_RACE", "SEX_REF", "FINCBTXM", 
           "FSALARYM", "FRRETIRM","FSMPFRXM", "TOTEXPCQ","FOODCQ",
           "FDHOMECQ", "FDAWAYCQ","HOUSCQ", "APPARCQ","TRANSCQ", "HEALTHCQ",
           "ENTERTCQ")

name4 <- c("HH_CU_Q_", "AS_C_MP1", "AS_C_MP2", "BLS_URBN", "CUTE_URE", 
           "FAM__IZE", "NO_E_RNR", "PERS_T18", "PERS_T64",  "VEHQ_", "AGE_REF_",
           "EDUC0REF", "REF__ACE", "SEX_REF_", "FINCB_XM",
           "FSAL_RYM", "FRRE_IRM","FSMP_RXM", "TOTEXPCQ","FOODCQ",
           "FDHOMECQ", "FDAWAYCQ","HOUSCQ", "APPARCQ","TRANSCQ", "HEALTHCQ", "ENTERTCQ")
flagfull <- subset(fmli161,select = c(name4))[-delete,]
str(fmlifull)

blank = rep(c(0),4254)
urb_ind <- which(fmlifull$BLS_URBN == 1)
for(i in (0:(length(urb_ind)-1))){
  blank[urb_ind[i]] = 1
}
BLS_URBN_URBAN = blank

blank = rep(c(0),4254)
rural_ind <- which(fmlifull$BLS_URBN == 2)
for(i in (0:(length(rural_ind)-1))){
  blank[rural_ind[i]] = 1
}
BLS_URBN_RURAL = blank

blank = rep(c(0),4254)
never_ind <- which(fmlifull$EDUC_REF == 0)
for(i in (0:(length(never_ind)-1))){
  blank[never_ind[i]] = 1
}
EDUC_REF_NEVER = blank

blank = rep(c(0),4254)
first_ind <- which(fmlifull$EDUC_REF == 10)
for(i in (0:(length(first_ind)-1))){
  blank[first_ind[i]] = 1
}
EDUC_REF_FIRST = blank

blank = rep(c(0),4254)
second_ind <- which(fmlifull$EDUC_REF == 11)
for(i in (0:(length(second_ind)-1))){
  blank[second_ind[i]] = 1
}
EDUC_REF_SECOND = blank

blank = rep(c(0),4254)
high_ind <- which(fmlifull$EDUC_REF == 12)
for(i in (0:(length(high_ind)-1))){
  blank[high_ind[i]] = 1
}
EDUC_REF_HIGH = blank

blank = rep(c(0),4254)
col_ind <- which(fmlifull$EDUC_REF == 13)
for(i in (0:(length(col_ind)-1))){
  blank[col_ind[i]] = 1
}
EDUC_REF_COL = blank

blank = rep(c(0),4254)
assc_ind <- which(fmlifull$EDUC_REF == 14)
for(i in (0:(length(assc_ind)-1))){
  blank[assc_ind[i]] = 1
}
EDUC_REF_ASSC = blank

blank = rep(c(0),4254)
udgrd_ind <- which(fmlifull$EDUC_REF == 15)
for(i in (0:(length(udgrd_ind)-1))){
  blank[udgrd_ind[i]] = 1
}
EDUC_REF_UDGRD = blank

blank = rep(c(0),4254)
grd_ind <- which(fmlifull$EDUC_REF == 16)
for(i in (0:(length(grd_ind)-1))){
  blank[grd_ind[i]] = 1
}
EDUC_REF_GRD = blank
#
#REF_RACE
blank = rep(c(0),4254)
wh_ind <- which(fmlifull$REF_RACE == 1)
for(i in (0:(length(wh_ind)-1))){
  blank[wh_ind[i]] = 1
}
REF_RACE_WH = blank

blank = rep(c(0),4254)
blk_ind <- which(fmlifull$REF_RACE == 2)
for(i in (0:(length(blk_ind)-1))){
  blank[blk_ind[i]] = 1
}
REF_RACE_BLK = blank

blank = rep(c(0),4254)
na_ind <- which(fmlifull$REF_RACE == 3)
for(i in (0:(length(na_ind)-1))){
  blank[na_ind[i]] = 1
}
REF_RACE_NA = blank

blank = rep(c(0),4254)
as_ind <- which(fmlifull$REF_RACE == 4)
for(i in (0:(length(as_ind)-1))){
  blank[as_ind[i]] = 1
}
REF_RACE_AS = blank

blank = rep(c(0),4254)
pi_ind <- which(fmlifull$REF_RACE == 5)
for(i in (0:(length(pi_ind)-1))){
  blank[pi_ind[i]] = 1
}
REF_RACE_PI = blank

blank = rep(c(0),4254)
multi_ind <- which(fmlifull$REF_RACE == 6)
for(i in (0:(length(multi_ind)-1))){
  blank[multi_ind[i]] = 1
}
REF_RACE_MULTI = blank
#
#SEX_REF
blank = rep(c(0),4254)
male_ind <- which(fmlifull$SEX_REF == 1)
for(i in (0:(length(male_ind)-1))){
  blank[male_ind[i]] = 1
}
SEX_REF_MALE = blank

blank = rep(c(0),4254)
female_ind <- which(fmlifull$SEX_REF == 2)
for(i in (0:(length(female_ind)-1))){
  blank[female_ind[i]] = 1
}
SEX_REF_FEMALE = blank

fmlifull = fmlifull[,-(12:14)]
fmlifull = fmlifull[,-4]
fmlifull = cbind(fmlifull, BLS_URBN_URBAN, BLS_URBN_RURAL, EDUC_REF_NEVER, EDUC_REF_FIRST, EDUC_REF_SECOND, 
                 EDUC_REF_HIGH, EDUC_REF_ASSC, EDUC_REF_UDGRD, EDUC_REF_GRD, REF_RACE_WH, REF_RACE_BLK, 
                 REF_RACE_NA, REF_RACE_AS, REF_RACE_PI, REF_RACE_MULTI, SEX_REF_MALE, SEX_REF_FEMALE)
write.csv(fmlifull,"fmlifull.csv")

#------------------------------------------------------------------------------------------------------------------------------------------------
#Correlation
#------------------------------------------------------------------------------------------------------------------------------------------------
round(cor(fmlifull),digits = 2)  #Table 5
large = function(m, value){
  if(m >= value){
    return(TRUE)
  }
}

#------------------------------------------------------------------------------------------------------------------------------------------------
#PCA
#------------------------------------------------------------------------------------------------------------------------------------------------
library(factoextra)
fmlifull.pca = prcomp(x=fmlifull, scale = TRUE)
pca_1_rotation = round(fmlifull.pca$rotation , digits = 2)
pca_1_importance  = round(summary(fmlifull.pca)$importance, digits = 2)
pca_1_rotation   #Table 6
pca_1_importance  #Table 7
# write.csv(pca_1_rotation, "pca_1_rotation.txt")
# write.csv(pca_1_importance, "pca_1_importance.txt")

#Figure 1
fviz_eig(fmlifull.pca, xlab = "Pricipal Components", ylab = "Proportion of Total Variance Explained, 1 for 10%")

#Figure 2: plot the variables in PC1-PC2 
par(cex = 0.5)
plot(fmlifull.pca$rotation[,1],fmlifull.pca$rotation[,2],
     xlab="PC1",ylab="PC2",type="n", cex.lab = 1.5)
text(fmlifull.pca$rotation[,1],fmlifull.pca$rotation[,2],labels=name3,cex = 1.5)

#Figure 3: plot the data points in PC1-PC2 grouping by race
m = matrix(data=c(1:6), nrow=3, ncol=2, byrow=TRUE)
layout(m)

par(cex=0.5)
z1 = fmlifull.pca$x[,1]
z2 = fmlifull.pca$x[,2]
colors <- c("blue","black","red","orange", "brown", "green")
leg.txt <- c("White","Black","Native American","Asian", "Pacific Islander", "Multiple Races")

plot(z1,z2,xlab="PC1",ylab="PC2",type="n")#, xlim = c(-15, 5), ylim = c(-6,9))
gp <- fmlifull$REF_RACE_WH == 1 #1 is white
points(z1[gp],z2[gp],col="blue")
legend("bottomleft",legend=leg.txt[1],fill=colors[1])

plot(z1,z2,xlab="PC1",ylab="PC2",type="n")
gp <- fmlifull$REF_RACE_BLK == 1#2 is black
points(z1[gp],z2[gp],col="black")
legend("bottomleft",legend=leg.txt[2],fill=colors[2])

plot(z1,z2,xlab="PC1",ylab="PC2",type="n")
gp <- fmlifull$REF_RACE_NA  == 1#3 is native american
points(z1[gp],z2[gp],col="red")
legend("bottomleft",legend=leg.txt[3],fill=colors[3])

plot(z1,z2,xlab="PC1",ylab="PC2",type="n")
gp <- fmlifull$REF_RACE_AS  == 1#4 is asian
points(z1[gp],z2[gp],col="orange")
legend("bottomleft",legend=leg.txt[4],fill=colors[4])

plot(z1,z2,xlab="PC1",ylab="PC2",type="n")
gp <- fmlifull$REF_RACE_PI  == 1#5 is pacific islander
points(z1[gp],z2[gp],col="brown")
legend("bottomleft",legend=leg.txt[5],fill=colors[5])

plot(z1,z2,xlab="PC1",ylab="PC2",type="n")
gp <- fmlifull$REF_RACE_MULTI  == 1#6 is multiple
points(z1[gp],z2[gp],col="darkgreen")
legend("bottomleft",legend=leg.txt[6],fill=colors[6])

layout(matrix(data=1, nrow=1, ncol=1))

#Figure 4: biplot of the variables in PC1-PC2
fviz_pca_var(fmlifull.pca,
             col.var = "contrib", # Color by contributions to the PC
             alpha.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             xlab = "PC1", ylab = "PC2", title = "Biplot", cex = 0.7,
             repel = TRUE     # Avoid text overlapping
)

#male/female plotted against pca, pc2, pc3
only_male = intersect(which(fmlifull$AS_COMP1 > 0), which(fmlifull$AS_COMP2 == 0)) 
only_female = intersect(which(fmlifull$AS_COMP1 == 0), which(fmlifull$AS_COMP2 > 0)) 
together = sort(c(only_male, only_female)) 
fmlitemp = fmlifull[together,] #1696 observations of 42 variables
fmlitemp.pca = prcomp(x=fmlitemp, scale = TRUE)
pca_2_rotation = round(fmlitemp.pca$rotation[,1:5], digits = 2)
pca_2_importance = round(summary(fmlitemp.pca)$importance, digits = 2)
pca_2_rotation  #Table 8
pca_2_importance  #Table 9
# write.csv(pca_2_rotation, "pca_2_rotation.csv")
# write.csv(pca_2_importance, "pca_2_importance.csv")

library(rgl)
plot3d(fmlitemp.pca$x[,1:3], col=fmlitemp$SEX_REF_FEMALE+8, size = 5)  #Figure 5

#------------------------------------------------------------------------------------------------------------------------------------------------
#Sparse PCA
#------------------------------------------------------------------------------------------------------------------------------------------------
library(nsprcomp)
set.seed(0)
fmlifull.pca.sparse <- nsprcomp(fmlifull, ncomp = 3, center=T, scale.=T, k=c(5,5,5), nneg = FALSE)
summary(fmlifull.pca.sparse)  #Table 11
print(fmlifull.pca.sparse)  #Table 10

#Figure 6
fviz_eig(fmlifull.pca.sparse, xlab = "Pricipal Components", ylab = "Proportion of Total Variance Explained, 1 for 10%")

#plot the variables in PC1-PC2
par(cex = 0.5)
plot(fmlifull.pca.sparse$x[,1],fmlifull.pca.sparse$x[,2],
     xlab="sparsePC1",ylab="sparsePC2",type="n")
text(fmlifull.pca.sparse$x[,1],fmlifull.pca.sparse$x[,2],labels="o")

library(rgl)
fmlifull.sex.pca.sparse <- nsprcomp(fmlifull[together,], ncomp = 3, center=T, scale.=T, k=c(5,5,5), nneg = FALSE)
summary(fmlifull.sex.pca.sparse)
plot3d(fmlifull.sex.pca.sparse$x[,1:3], col=fmlitemp$SEX_REF_FEMALE+1, size = 5)  #Figure 8

#------------------------------------------------------------------------------------------------------------------------------------------------
#Cluster analysis
#------------------------------------------------------------------------------------------------------------------------------------------------
#try cluster of variables
library(ClustOfVar)
clustofvar <- hclustvar(fmlifull)
par(cex = 0.7)
dev.new(width = 10,height = 5)
plot(clustofvar)  #Figure 9

#try Hmisc to cluster variables and compare to ClusofVar
library(Hmisc)
#using Spearman's rho
Hclust_rho <- varclus(as.matrix(fmlifull), similarity = "spearman")
Hclust_rho
par(cex = 0.7)
dev.new(width = 10,height = 5)
plot(Hclust_rho) #Figure 10

Hclust_D <- varclus(as.matrix(fmlifull), similarity = "hoeffding")#very slow, results not so good
Hclust_D
par(cex = 0.7)
dev.new(width = 10,height = 5)
plot(Hclust_D) #Figure 11


Hclust_Pearson <- varclus(as.matrix(fmlifull), similarity = "pearson")
Hclust_Pearson
par(cex = 0.7)
dev.new(width = 10,height = 5)
plot(Hclust_Pearson) #Figure 12

#------------------------------------------------------------------------------------------------------------------------------------------------
#Canonical correlation analysis
#------------------------------------------------------------------------------------------------------------------------------------------------
#We are interested in comparing the 4 income variables:FINCBTXM, FSALARYM, FRRETIRM, FSMPFRXM
#to the 11 expenditure variables
#Pick subgroups of expenditure of size four and then do cca with
#4 income variables
#Selection criterion:based on cluster of variables analysis
#FOODCQ,TOTEXPCQ,HOUSCQ,HEALTHCQ, each from as far away a subcluster as possible
fincbtm <- scale(fmlifull$FINCBTXM)
fsalarym <- scale(fmlifull$FSALARYM)
frretirm <- scale(fmlifull$FRRETIRM)
fsmpfrxm <- scale(fmlifull$FSMPFRXM)
totexpcq <- scale(fmlifull$TOTEXPCQ)
foodcq <- scale(fmlifull$FOODCQ)
houscq <- scale(fmlifull$HOUSCQ)
healthcq <- scale(fmlifull$HEALTHCQ)

cca.cor <- cor(cbind(fincbtm, fsalarym, frretirm, fsmpfrxm, totexpcq, foodcq, houscq, healthcq))
r11 <- cca.cor[1:4,1:4]
r22 <- cca.cor[5:8,5:8]
r12 <- cca.cor[1:4,5:8]
r21 <- t(r12)
e1 <- solve(r11) %*% r12 %*% solve(r22) %*% r21
e2 <- solve(r22) %*% r21 %*% solve(r11) %*% r12
eigen(e1)
round(sqrt(eigen(e1)$values), digits = 2)  #Table 14
round((eigen(e1)$vectors), digits = 2) #Table 15
eigen(e2)
round(sqrt(eigen(e2)$values), digits = 2) #Table 14
round((eigen(e2)$vectors), digits = 2) #Table 15
x <- cbind(fincbtm, fsalarym, frretirm, fsmpfrxm)
y <- cbind(totexpcq, foodcq, houscq, healthcq)
u <- x %*% eigen(e1)$vectors
v <- y %*% eigen(e2)$vectors
