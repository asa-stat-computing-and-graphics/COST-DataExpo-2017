remove(list=ls())

fmli161 <- read.csv("fmli161.csv", header = TRUE,na.string = c(".", "NA"), row.names = 1)

name <- c("HH_CU_Q", "AS_COMP1", "AS_COMP2", "BLS_URBN", "CUTENURE", 
         "FAM_SIZE", "NO_EARNR", "PERSLT18", "PERSOT64", "VEHQ", "AGE_REF",
         "EDUC_REF", "REF_RACE", "SEX_REF", "INC_HRS1", "OCCUCOD1", "FINCBTXM", 
         "FSALARYM", "FRRETIRM","FSMPFRXM", "WELFAREM", "INTRDVXM", 
         "NETRENTM","RETSURVM",  "TOTEXPCQ","FOODCQ",
         "FDHOMECQ", "FDAWAYCQ","HOUSCQ", "APPARCQ","TRANSCQ", "HEALTHCQ",
         "ENTERTCQ", "EDUCACQ","READCQ")

name2 <- c("HH_CU_Q_", "AS_C_MP1", "AS_C_MP2", "BLS_URBN", "CUTE_URE", 
           "FAM__IZE", "NO_E_RNR", "PERS_T18", "PERS_T64", "VEHQ_", "AGE_REF_",
           "EDUC0REF", "REF__ACE", "SEX_REF_", "INC__RS1", "OCCU_OD1", "FINCB_XM",
           "FSAL_RYM", "FRRE_IRM","FSMP_RXM", "WELF_REM", "INTR_VXM", 
           "NETR_NTM","RETS_RVM",  "TOTEXPCQ","FOODCQ",
           "FDHOMECQ", "FDAWAYCQ","HOUSCQ", "APPARCQ","TRANSCQ", "HEALTHCQ",
           "ENTERTCQ", "EDUCACQ","READCQ")
#Total of 34 variables selected
fmli <- subset(fmli161, select = name)
flag <- subset(fmli161,select = c(name2 ))

library("mice")

#Try to split the dataset according to if TOTEXPCQ is empty
delete = which(fmli$TOTEXPCQ == 0)
str(which(fmli$TOTEXPCQ == 0))#total of 2171 missing tot expenditure
fmlifull = fmli[-delete,]
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

# md.pattern(fmlifull)
#drop X,Y,Z,AA,S:5000 - 6000+ NAs, not usable
fmlifull = fmlifull[,-c(21:24)]
#for INC-HRS1 and OCCUCOD1 missing 2191, 2191/4524 = 48.43%, dropped

identical(which(is.na(fmli[,"OCCUCOD1"])), which(is.na(fmli[,"INC_HRS1"])))

fmlifull = fmlifull[,-c(15:16)]
# md.pattern(fmlifull)
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

#Correlation
round(cor(fmlifull),digits = 2)
large = function(m, value){
  if(m >= value){
    return(TRUE)
  }
}

#PCA

# prcomp
library(factoextra)
fmlifull.pca = prcomp(x=fmlifull, scale  = TRUE)
pca_1_rotation = round(fmlifull.pca$rotation[,1:5], digits = 2)
pca_1_importance = round(summary(fmlifull.pca)$importance, digits = 2)
# pca_1_rotation
# pca_1_importance
write.csv(pca_1_rotation, "pca_1_rotation.txt")
write.csv(pca_1_importance, "pca_1_importance.txt")

pdf(file = "Fig 1 Scree Plot for 10 PC.pdf", width = 15, height = 12, family = "Helvetica") 
fviz_eig(fmlifull.pca, labelsize = 30,
         ylim=c(0,14),bar_width=0.3, xlab = "Pricipal Components", ylab = "Proportion of Total Variance Explained, 1 for 10%") +
      theme(legend.text = element_text(size = 30), axis.text = element_text(size = 30), text = element_text(size = 30), plot.title = element_blank())
dev.off()

#plot the variables in PC1-PC2
pdf(file = "Fig 2 Variables in First and Second PC.pdf", width = 15, height = 12, family = "Helvetica")
plot(fmlifull.pca$rotation[,1],fmlifull.pca$rotation[,2],
     xlab="",ylab="",type="n", cex.axis = 2)
text(fmlifull.pca$rotation[,1],fmlifull.pca$rotation[,2],labels=name3,cex = 1.5)
dev.off()


#The First six principle components
# kable(data.frame(spca$loadings[1:6,]),format = "latex", digit =2)

#Plot the data points in PC1-PC2 grouping by race
pca_race1 = as.data.frame(fmlifull.pca$x[fmlifull$REF_RACE_WH == 1 ,1:2])
colnames(pca_race1) = c("PC1", "PC2")
race1 = ggplot(pca_race1, aes(x = PC1, y = PC2)) +
  geom_point(color = "cyan2") + 
  ggtitle("White") +
  theme_bw()+
  theme(plot.title = element_text(size = 27),
        axis.text=element_text(size=28),
        axis.title=element_text(size=28)) + #,face="bold")) +
  ylab("PC2") +
  xlab("PC1") +
  theme(legend.position="none") +
  theme(text = element_text(size=20))

pca_race2 = as.data.frame(fmlifull.pca$x[fmlifull$REF_RACE_BLK == 1 ,1:2])
colnames(pca_race2) = c("PC1", "PC2")
race2 = ggplot(pca_race2, aes(x = PC1, y = PC2)) +
  geom_point(color = "gold2") + 
  ggtitle("Black") +
  theme_bw()+
  theme(plot.title = element_text(size = 27),
        axis.text=element_text(size=28),
        axis.title=element_text(size=28)) + #,face="bold")) +
  ylab("PC2") +
  xlab("PC1") +
  theme(legend.position="none") +
  theme(text = element_text(size=20))

pca_race3 = as.data.frame(fmlifull.pca$x[fmlifull$REF_RACE_NA == 1 ,1:2])
colnames(pca_race3) = c("PC1", "PC2")
race3 = ggplot(pca_race3, aes(x = PC1, y = PC2)) +
  geom_point(color = "coral") + 
  ggtitle("Native American") +
  theme_bw()+
  theme(plot.title = element_text(size = 27),
        axis.text=element_text(size=28),
        axis.title=element_text(size=28)) + #,face="bold")) +
  ylab("PC2") +
  xlab("PC1") +
  theme(legend.position="none") +
  theme(text = element_text(size=20))

pca_race4 = as.data.frame(fmlifull.pca$x[fmlifull$REF_RACE_AS == 1 ,1:2])
colnames(pca_race4) = c("PC1", "PC2")
race4 = ggplot(pca_race4, aes(x = PC1, y = PC2)) +
  geom_point(color = "darkorchid2") + 
  ggtitle("Asian") +
  theme_bw()+
  theme(plot.title = element_text(size = 27),
        axis.text=element_text(size=28),
        axis.title=element_text(size=28)) + #,face="bold")) +
  ylab("PC2") +
  xlab("PC1") +
  theme(legend.position="none") +
  theme(text = element_text(size=20))

pca_race5  = as.data.frame(fmlifull.pca$x[fmlifull$REF_RACE_PI == 1 ,1:2])
colnames(pca_race5) = c("PC1", "PC2")
race5 = ggplot(pca_race5, aes(x = PC1, y = PC2)) +
  geom_point(color = "aquamarine3") + 
  ggtitle("Pacific Islander") +
  theme_bw()+
  theme(plot.title = element_text(size = 27),
        axis.text=element_text(size=28),
        axis.title=element_text(size=28)) + #,face="bold")) +
  ylab("PC2") +
  xlab("PC1") +
  theme(legend.position="none") +
  theme(text = element_text(size=20))

pca_race6 = as.data.frame(fmlifull.pca$x[fmlifull$REF_RACE_MULTI == 1 ,1:2])
colnames(pca_race6) = c("PC1", "PC2")
race6 = ggplot(pca_race6, aes(x = PC1, y = PC2)) +
  geom_point(color = "chocolate2") + 
  ggtitle("Multiple Races") +
  theme_bw()+
  theme(plot.title = element_text(size = 27),
        axis.text=element_text(size=28),
        axis.title=element_text(size=28)) + #,face="bold")) +
  ylab("PC2") +
  xlab("PC1") +
  theme(legend.position="none") +
  theme(text = element_text(size=20))

library(ggpubr)
pdf(file = "Fig 3 Grouping in PC1 vs PC2 separated by Ethnicity.pdf", width = 15, height = 12, family = "Helvetica")
ggarrange(race1, race2, race3, race4, race5, race6, nrow = 3, ncol = 2)
dev.off()


#Two ways to make biplots
require(graphics)
par( pch = 20)
par(cex = 0.8)
biplot(princomp(fmlifull, cor = TRUE), xlabs=rep("??", nrow(fmlifull))) 

#Biplot of the variables in PC1-PC2
pdf(file = "Fig 4 Biplot of First PC vs Second PC.pdf", width = 15, height = 12, family = "Helvetica")
fviz_pca_var(fmlifull.pca, labelsize = 9,
             col.var = "contrib", # Color by contributions to the PC
             alpha.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             xlab = "PC1", ylab = "PC2", title = "Biplot", cex = 1.5,
             repel = TRUE     # Avoid text overlapping
             ) +
      theme(legend.text = element_text(size = 25), axis.text = element_text(size = 25), text = element_text(size = 25), plot.title = element_blank())
dev.off()

#male/female plotted against pca, pc2, pc3
only_male = intersect(which(fmlifull$AS_COMP1 > 0), which(fmlifull$AS_COMP2 == 0)) 
only_female = intersect(which(fmlifull$AS_COMP1 == 0), which(fmlifull$AS_COMP2 > 0)) 
together = sort(c(only_male, only_female)) 
fmlitemp = fmlifull[together,] #1696 observations of 42 variables
fmlitemp.pca = prcomp(x=fmlitemp, scale = TRUE)
pca_2_rotation = round(fmlitemp.pca$rotation[,1:5], digits = 2)
pca_2_importance = round(summary(fmlitemp.pca)$importance, digits = 2)
write.csv(pca_2_rotation, "pca_2_rotation.csv")
write.csv(pca_2_importance, "pca_2_importance.csv")

library(rgl)
plot3d(fmlitemp.pca$x[,1:3], col=fmlitemp$SEX_REF_FEMALE+8, size = 5)

#Sparse PCA
library(nsprcomp)
set.seed(0)
fmlifull.pca.sparse <- nsprcomp(fmlifull, ncomp = 3, center=T, scale.=T, k=c(5,5,5), nneg = FALSE)
summary(fmlifull.pca.sparse)
print(fmlifull.pca.sparse)
plot(fmlifull.pca.sparse,type="lines")

pdf(file = "Fig 6 Scree Plot for Sparse PCA.pdf", width = 15, height = 12, family = "Helvetica") 
fviz_eig(fmlifull.pca.sparse, xlab = "Pricipal Components", ylab = "Proportion of Total Variance Explained, 1 for 10%", labelsize = 30,
         ylim=c(0,40),bar_width=0.3) +
      theme(legend.text = element_text(size = 30), axis.text = element_text(size = 30), text = element_text(size = 30), plot.title = element_blank())
dev.off()

#plot the variables in PC1-PC2

pdf(file = "Fig 7.pdf", width = 15, height = 12, family = "Helvetica") 
pcas = as.data.frame(fmlifull.pca.sparse$x[,1:2])
colnames(pcas) = c("PC1", "PC2")
ggplot(pcas, aes(x = PC1, y = PC2)) +
  geom_point(color = "turquoise3") + 
  theme_bw()+
  theme(plot.title = element_text(size = 27),
        axis.text=element_text(size=28),
        axis.title=element_text(size=28)) + #,face="bold")) +
  ylab("Sparse PC2") +
  xlab("Sparse PC1") +
  theme(legend.position="none") +
  theme(text = element_text(size=20))
dev.off()


library(rgl)
fmlifull.sex.pca.sparse <- nsprcomp(fmlifull[together,], ncomp = 3, center=T, scale.=T, k=c(5,5,5), nneg = FALSE)
summary(fmlifull.sex.pca.sparse)
plot3d(fmlifull.sex.pca.sparse$x[,1:3], col=fmlitemp$SEX_REF_FEMALE+1, size = 5)

#Cluster analysis
# library(mclust)
# mclus <- Mclust(fmlifull) # fits up to 9 clusters by default
# mclass <- mclus$classification
# k <- mclus$G # number of clusters
# for(i in 1:k){ print(paste("Cluster",i))
#   print(c.names[mclass == i])
# } 
#Not useful, since we already know that the datset is not normal so should not use this, and returns only one cluster

n <- dim(fmlifull)[1] 
k <- 9
wss <- rep(0,k)
fmlifull.m <- apply(fmlifull,2,mean)
for(i in 1:n){
  wss[1] <- wss[1]+sum((fmlifull[i,]-fmlifull.m)^2)
}
for(i in 2:k){
  model <- kmeans(fmlifull,i)
  wss[i] <- sum(model$withinss)
}
par(cex=0.8)
plot(1:k,wss,type="b",xlab="Number of clusters",
     ylab="Within cluster sum of squares", main="Scree plot")

#try cluster of variables
library(ClustOfVar)
clustofvar <- hclustvar(fmlifull)



pdf(file = "Fig 9 Cluster Dendrogram.pdf", width = 15, height = 12, family = "Helvetica")
par(mar=c(5,6,4,1)+.1, cex = 1.7)
plot(clustofvar, main = "")
dev.off()

fviz_eig(fmlifull.pca.sparse, xlab = "Pricipal Components", ylab = "Proportion of Total Variance Explained, 1 for 10%", labelsize = 30,
         ylim=c(0,40),bar_width=0.3) +
      theme(legend.text = element_text(size = 30), axis.text = element_text(size = 30), text = element_text(size = 30), plot.title = element_blank())

#try Hmisc to cluster variables and compare to ClusofVar
library(Hmisc)
#using Spearman's rho
Hclust_rho <- varclus(as.matrix(fmlifull), similarity = "spearman")
Hclust_rho
pdf(file = "Fig 10 Cluster Dendrogram using Spearman rho.pdf", width = 15, height = 12, family = "Helvetica")
par(mar=c(5,6,4,1)+.1, cex = 1.7)
plot(Hclust_rho, main = "")
dev.off()

Hclust_D <- varclus(as.matrix(fmlifull), similarity = "hoeffding")#very slow, results not so good
Hclust_D
pdf(file = "Fig 11 Cluster Dendrogram using Hoeffdings D.pdf", width = 15, height = 12, family = "Helvetica")
par(mar=c(5,6,4,1)+.1, cex = 1.7)
plot(Hclust_D, main = "")
dev.off()

Hclust_Pearson <- varclus(as.matrix(fmlifull), similarity = "pearson")
Hclust_Pearson
pdf(file = "Fig 12 Cluster Dendrogram using Pearsons.pdf", width = 15, height = 12, family = "Helvetica")
par(mar=c(5,6,4,1)+.1, cex = 1.7)
plot(Hclust_Pearson, main = "")
dev.off()

#Canonical correlation analysis, compare with 
#we are interested in comparing the 4 income variables:FINCBTXM, FSALARYM, FRRETIRM, FSMPFRXM
#to the 11 expenditure variables: 
str(fmlifull)
round(cor(fmlifull),digits = 2)
fincbtm <- scale(fmlifull$FINCBTXM)
fsalarym <- scale(fmlifull$FSALARYM)
frretirm <- scale(fmlifull$FRRETIRM)
fsmpfrxm <- scale(fmlifull$FSMPFRXM)
totexpcq <- scale(fmlifull$TOTEXPCQ)
foodcq <- scale(fmlifull$FOODCQ)
fdhomecq <- scale(fmlifull$FDHOMECQ)
fdawaycq <- scale(fmlifull$FDAWAYCQ)
houscq <- scale(fmlifull$HOUSCQ)
apparcq <- scale(fmlifull$APPARCQ)
transcq <- scale(fmlifull$TRANSCQ)
healthcq <- scale(fmlifull$HEALTHCQ)
entertcq <- scale(fmlifull$ENTERTCQ)
educacq <- scale(fmlifull$EDUCACQ)
readcq <- scale(fmlifull$READCQ)
cca.cor <- cor(cbind(fincbtm, fsalarym, frretirm, fsmpfrxm, totexpcq, foodcq, fdhomecq, fdawaycq, houscq,
                     apparcq, transcq, healthcq, entertcq, educacq, readcq))
r11 <- cca.cor[1:4,1:4]
r22 <- cca.cor[5:15,5:15]
r12 <- cca.cor[1:4,5:15]
r21 <- t(r12)
e1 <- solve(r11) %*% r12 %*% solve(r22) %*% r21
e2 <- solve(r22) %*% r21 %*% solve(r11) %*% r12
eigen(e1)
round(sqrt(eigen(e1)$values), digits = 2)
round((eigen(e1)$vectors), digits = 2)
eigen(e2)
round(sqrt(eigen(e2)$values), digits = 2)
round((eigen(e2)$vectors), digits = 2)
x <- cbind(fincbtm, fsalarym, frretirm, fsmpfrxm)
y <- cbind(totexpcq, foodcq, fdhomecq, fdawaycq, houscq, apparcq, transcq, healthcq, entertcq, educacq, readcq)
u <- x %*% eigen(e1)$vectors
v <- y %*% eigen(e2)$vectors
round(cor(u,x), digits = 2)
round(cor(v,y),digits = 2)

#
#Second approach: pick subgroups of expenditure of size four and then do cca with
#4 income variables
#Selection criterion:based on sparse pca, cluster and intuition


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
round(sqrt(eigen(e1)$values), digits = 2)
round((eigen(e1)$vectors), digits = 2)
eigen(e2)
round(sqrt(eigen(e2)$values), digits = 2)
round((eigen(e2)$vectors), digits = 2)
x <- cbind(fincbtm, fsalarym, frretirm, fsmpfrxm)
y <- cbind(totexpcq, foodcq, houscq, healthcq)
u <- x %*% eigen(e1)$vectors
v <- y %*% eigen(e2)$vectors
round(cor(u,x),digits = 2)
round(cor(v,y),digits = 2)
