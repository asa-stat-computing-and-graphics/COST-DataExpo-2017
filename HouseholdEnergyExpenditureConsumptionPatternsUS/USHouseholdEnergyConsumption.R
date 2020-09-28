# title: "Household Energy Expenditure and Consumption Patterns in the United States"
# author: "J Meechai (R code), M Wijesinha (SAS code)"
# For JSM Data Challenge 2017

# Load R Libraries
library(nlme)
library(scales)
library(lsmeans)
library(emmeans)
library(multcompView)
library(cluster)
library(tidyverse)
library(factoextra)

# Load BLS Consumer Expenditure Survey Data
setwd("C:/Users/jmeec/OneDrive/Computational Statistics")
fm151<-read.csv("fmli152.csv",header=TRUE,comm="#")
fm152<-read.csv("fmli153.csv",header=TRUE,comm="#")
fm153<-read.csv("fmli154.csv",header=TRUE,comm="#")
fm154<-read.csv("fmli161.csv",header=TRUE,comm="#")

# Create data set "fm15r" which covers all four quarters and is made up of only the relevant BLS variables. 25,797 survey responses.
fm151r<-subset(fm151,select=c(CUID,QINTRVMO,STATE,BLS_URBN,REGION,BUILDING,CUTENURE,FAM_TYPE,FAM_SIZE,NUM_AUTO,ROOMSQ,HIGH_EDU,NTLGASPQ,ELCTRCPQ,FULOILPQ,OTHFLSPQ,GASMOPQ,FINCBTXM,FINLWT21,WTREP01,WTREP02,WTREP03,WTREP04,WTREP05,WTREP06,WTREP07,WTREP08,WTREP09,WTREP10,WTREP11,WTREP12,WTREP13,WTREP14,WTREP15,WTREP16,WTREP17,WTREP18,WTREP19,WTREP20,WTREP21,WTREP22,WTREP23,WTREP24,WTREP25,WTREP26,WTREP27,WTREP28,WTREP29,WTREP30,WTREP31,WTREP32,WTREP33,WTREP34,WTREP35,WTREP36,WTREP37,WTREP38,WTREP39,WTREP40,WTREP41,WTREP42,WTREP43,WTREP44))
fm152r<-subset(fm152,select=c(CUID,QINTRVMO,STATE,BLS_URBN,REGION,BUILDING,CUTENURE,FAM_TYPE,FAM_SIZE,NUM_AUTO,ROOMSQ,HIGH_EDU,NTLGASPQ,ELCTRCPQ,FULOILPQ,OTHFLSPQ,GASMOPQ,FINCBTXM,FINLWT21,WTREP01,WTREP02,WTREP03,WTREP04,WTREP05,WTREP06,WTREP07,WTREP08,WTREP09,WTREP10,WTREP11,WTREP12,WTREP13,WTREP14,WTREP15,WTREP16,WTREP17,WTREP18,WTREP19,WTREP20,WTREP21,WTREP22,WTREP23,WTREP24,WTREP25,WTREP26,WTREP27,WTREP28,WTREP29,WTREP30,WTREP31,WTREP32,WTREP33,WTREP34,WTREP35,WTREP36,WTREP37,WTREP38,WTREP39,WTREP40,WTREP41,WTREP42,WTREP43,WTREP44))
fm153r<-subset(fm153,select=c(CUID,QINTRVMO,STATE,BLS_URBN,REGION,BUILDING,CUTENURE,FAM_TYPE,FAM_SIZE,NUM_AUTO,ROOMSQ,HIGH_EDU,NTLGASPQ,ELCTRCPQ,FULOILPQ,OTHFLSPQ,GASMOPQ,FINCBTXM,FINLWT21,WTREP01,WTREP02,WTREP03,WTREP04,WTREP05,WTREP06,WTREP07,WTREP08,WTREP09,WTREP10,WTREP11,WTREP12,WTREP13,WTREP14,WTREP15,WTREP16,WTREP17,WTREP18,WTREP19,WTREP20,WTREP21,WTREP22,WTREP23,WTREP24,WTREP25,WTREP26,WTREP27,WTREP28,WTREP29,WTREP30,WTREP31,WTREP32,WTREP33,WTREP34,WTREP35,WTREP36,WTREP37,WTREP38,WTREP39,WTREP40,WTREP41,WTREP42,WTREP43,WTREP44))
fm154r<-subset(fm154,select=c(CUID,QINTRVMO,STATE,BLS_URBN,REGION,BUILDING,CUTENURE,FAM_TYPE,FAM_SIZE,NUM_AUTO,ROOMSQ,HIGH_EDU,NTLGASPQ,ELCTRCPQ,FULOILPQ,OTHFLSPQ,GASMOPQ,FINCBTXM,FINLWT21,WTREP01,WTREP02,WTREP03,WTREP04,WTREP05,WTREP06,WTREP07,WTREP08,WTREP09,WTREP10,WTREP11,WTREP12,WTREP13,WTREP14,WTREP15,WTREP16,WTREP17,WTREP18,WTREP19,WTREP20,WTREP21,WTREP22,WTREP23,WTREP24,WTREP25,WTREP26,WTREP27,WTREP28,WTREP29,WTREP30,WTREP31,WTREP32,WTREP33,WTREP34,WTREP35,WTREP36,WTREP37,WTREP38,WTREP39,WTREP40,WTREP41,WTREP42,WTREP43,WTREP44))

fm15r<-rbind(fm151r,fm152r,fm153r,fm154r)
dim(fm15r)

# Add code for ST (based on 2-letter state abbreviation) and PQTR
fm15r$ST<-as.factor(fm15r$STATE)
levels(fm15r$ST)<-c("AL","AK","AZ","CA","CO","CT","DE","DC","FL","GA","HI","IL","IN","IA","KS","KY","LA","MD","MA","MI","MN","MS","MO","NE","NV","NH","NJ","NY","NC","OH","OK","OR","PA","SC","TN","TX","UT","VA","WA","WI")

fm15r$PQTR<-as.factor(fm15r$QINTRVMO)
levels(fm15r$PQTR)<-c("4","4","4","1","1","1","2","2","2","3","3","3")
dim(fm15r)

# Redefine factor levels. See Appendix A
fm15r$REGION<-as.factor(fm15r$REGION)
levels(fm15r$REGION)<-c("Northeast","Midwest","South","West")
fm15r$BUILDING<-as.factor(ifelse(fm15r$BUILDING==1,"Detached",(ifelse(fm15r$BUILDING==2|fm15r$BUILDING==3|fm15r$BUILDING==4|fm15r$BUILDING==5,"NonDetached",(ifelse(fm15r$BUILDING==6|fm15r$BUILDING==7|fm15r$BUILDING==8,"Apartment","Other"))))))
fm15r$CUTENURE<-as.factor(ifelse(fm15r$CUTENURE==4,"Renter",(ifelse(fm15r$CUTENURE==1|fm15r$CUTENURE==2|fm15r$CUTENURE==3,"Homeowner","Other"))))
fm15r$FAM_TYPE<-as.factor(ifelse(fm15r$FAM_TYPE==1,"Married Couple Only",(ifelse(fm15r$FAM_TYPE==2|fm15r$FAM_TYPE==3|fm15r$FAM_TYPE==4|fm15r$FAM_TYPE==5,"Married Couple And Others",(ifelse(fm15r$FAM_TYPE==6|fm15r$FAM_TYPE==7,"Single Parent",(ifelse(fm15r$FAM_TYPE==8,"Single Person","Other"))))))))
fm15r$HIGH_EDU<-as.numeric(ifelse(fm15r$HIGH_EDU==0,0,(ifelse(fm15r$HIGH_EDU==10,6,(ifelse(fm15r$HIGH_EDU==11,10,(ifelse(fm15r$HIGH_EDU==12,12,(ifelse(fm15r$HIGH_EDU==13|fm15r$HIGH_EDU==14,14,(ifelse(fm15r$HIGH_EDU==15,16,(ifelse(fm15r$HIGH_EDU==16,19,50))))))))))))))

# SECTION 3.1.2 EXPLORATORY DATA ANALYSIS
# Table 1: Summary Statistics for Household Energy Expenditures
fm15r$MO_SCOPE<-as.numeric(ifelse(fm15r$QINTRVMO==2,2,(ifelse(fm15r$QINTRVMO==3,1,3))))
PopWeight<-sum(fm15r$FINLWT21*fm15r$MO_SCOPE)
NTLGASPQ_Mean<-sum(fm15r$NTLGASPQ*fm15r$FINLWT21*fm15r$MO_SCOPE)/PopWeight
ELCTRCPQ_Mean<-sum(fm15r$ELCTRCPQ*fm15r$FINLWT21*fm15r$MO_SCOPE)/PopWeight
FULOILPQ_Mean<-sum(fm15r$FULOILPQ*fm15r$FINLWT21*fm15r$MO_SCOPE)/PopWeight
OTHFLSPQ_Mean<-sum(fm15r$OTHFLSPQ*fm15r$FINLWT21*fm15r$MO_SCOPE)/PopWeight
GASMOPQ_Mean<-sum(fm15r$GASMOPQ*fm15r$FINLWT21*fm15r$MO_SCOPE)/PopWeight
summary(fm15r[,13:17])
NTLGASPQ_Mean
ELCTRCPQ_Mean
FULOILPQ_Mean
OTHFLSPQ_Mean
GASMOPQ_Mean

# SECTION 3.1.2 EXPLORATORY DATA ANALYSIS
# Figure 1: Bar Chart for for Household Energy Expenditures by Region
fm15rRegionKnown<-subset(fm15r,REGION!= "NA")
R1Responses<-fm15rRegionKnown[fm15rRegionKnown$REGION=="Northeast",]
R2Responses<-fm15rRegionKnown[fm15rRegionKnown$REGION=="Midwest",]
R3Responses<-fm15rRegionKnown[fm15rRegionKnown$REGION=="South",]
R4Responses<-fm15rRegionKnown[fm15rRegionKnown$REGION=="West",]

PopWgt_R1<-sum(R1Responses$FINLWT21*R1Responses$MO_SCOPE)
PopWgt_R2<-sum(R2Responses$FINLWT21*R2Responses$MO_SCOPE)
PopWgt_R3<-sum(R3Responses$FINLWT21*R3Responses$MO_SCOPE)
PopWgt_R4<-sum(R4Responses$FINLWT21*R4Responses$MO_SCOPE)

fm15rRegionKnown$PopWgtByRegion<-ifelse(fm15rRegionKnown$REGION=="Northeast",PopWgt_R1,ifelse(fm15rRegionKnown$REGION=="Midwest",PopWgt_R2,ifelse(fm15rRegionKnown$REGION=="South",PopWgt_R3,PopWgt_R4)))

fm15rRegionKnown$RegionMeanComponent_NG<-(fm15rRegionKnown$NTLGASPQ*fm15rRegionKnown$FINLWT21*fm15rRegionKnown$MO_SCOPE)/fm15rRegionKnown$PopWgtByRegion
fm15rRegionKnown$RegionMeanComponent_ELEC<-(fm15rRegionKnown$ELCTRCPQ*fm15rRegionKnown$FINLWT21*fm15rRegionKnown$MO_SCOPE)/fm15rRegionKnown$PopWgtByRegion
fm15rRegionKnown$RegionMeanComponent_FUEL<-(fm15rRegionKnown$FULOILPQ*fm15rRegionKnown$FINLWT21*fm15rRegionKnown$MO_SCOPE)/fm15rRegionKnown$PopWgtByRegion
fm15rRegionKnown$RegionMeanComponent_OTHFUL<-(fm15rRegionKnown$OTHFLSPQ*fm15rRegionKnown$FINLWT21*fm15rRegionKnown$MO_SCOPE)/fm15rRegionKnown$PopWgtByRegion
fm15rRegionKnown$RegionMeanComponent_GAS<-(fm15rRegionKnown$GASMOPQ*fm15rRegionKnown$FINLWT21*fm15rRegionKnown$MO_SCOPE)/fm15rRegionKnown$PopWgtByRegion

Fig1Data<-aggregate(fm15rRegionKnown[,68:72],by=fm15rRegionKnown[5],FUN=sum)
Fig1Data

# Fig1Data2 combines fuel oil and other fuels.
Fig1Data2<-data.frame(Fig1Data$REGION,Fig1Data$RegionMeanComponent_NG,Fig1Data$RegionMeanComponent_ELEC,Fig1Data$RegionMeanComponent_FUEL+Fig1Data$RegionMeanComponent_OTHFUL,Fig1Data$RegionMeanComponent_GAS)
names(Fig1Data2)<-c("Region","Natural Gas","Electricity","Fuels","Gasoline")
Fig1Data2

EnergyColors<-c("blue","red","gold","green")
EnergyFillDensity<-c(20,40,20,20)
EnergyFillAngle<-c(0,45,90,135)
BarNames<-c("Natural Gas","Electricity","Fuels","Gasoline")
RegionNames<-c("Northeast","Midwest","South","West")

Fig1Matrix2<-as.matrix(Fig1Data2)
Fig1Northeast<-as.numeric(Fig1Matrix2[1,2:5])/3
Fig1Midwest<-as.numeric(Fig1Matrix2[2,2:5])/3
Fig1South<-as.numeric(Fig1Matrix2[3,2:5])/3
Fig1West<-as.numeric(Fig1Matrix2[4,2:5])/3
AvgExpByRegion<-cbind(Fig1Northeast,Fig1Midwest,Fig1South,Fig1West)

barplot(AvgExpByRegion,col=EnergyColors,angle=EnergyFillAngle,density=EnergyFillDensity,names.arg=RegionNames,ylim=c(0,300),main="Average Monthly Expenditure (USD)",cex.lab=2,cex.main=2.5,cex.sub=2,cex.names=2,cex.axis=1.7)
legend("topright",legend=c("NG","Elec","Fuels","Gas"),angle=EnergyFillAngle,density=EnergyFillDensity,fill=EnergyColors,bty="n",cex=2,ncol=2)

# Create variable INCOME_T which is Arcsinh transformed income
# Create Archsinh transformed energy expenditure variables for trimming purposes
# Log-Transformed ConsumptionData
fm15r$NTLGASPQ_T<-log(fm15r$NTLGASPQ+((fm15r$NTLGASPQ)^2+1)^0.5)
fm15r$ELCTRCPQ_T<-log(fm15r$ELCTRCPQ+((fm15r$ELCTRCPQ)^2+1)^0.5)
fm15r$GASMOPQ_T<-log(fm15r$GASMOPQ+((fm15r$GASMOPQ)^2+1)^0.5)
fm15r$INCOME_T<-log(fm15r$FINCBTXM+((fm15r$FINCBTXM)^2+1)^0.5)
dim(fm15r)

# Create data set "fm15NoBlankROOMSQ" which has CUID's with unidentified/non-numeric number of rooms removed. fm15NoBlankROOMSQ contains 25,538 responses. There were 259 responses with unidentified ROOMSQ.
fm15r$ROOMSQ<-as.numeric(paste(fm15r$ROOMSQ))
fm15NoBlankROOMSQ<-fm15r
fm15NoBlankROOMSQ<-subset(fm15NoBlankROOMSQ,ROOMSQ!= ".")
dim(fm15r)
dim(fm15NoBlankROOMSQ)

# SECTION 3.2.2 DATA TRIMMING
# Trim households with survey values more than 3IQR beyond 3rd IQ.
# fm15Trimmed contains 25,454 responses.
fm15Trimmed<-fm15NoBlankROOMSQ[-1*(which((fm15NoBlankROOMSQ$FAM_SIZE>quantile(fm15NoBlankROOMSQ$FAM_SIZE)[4]+3*IQR(fm15NoBlankROOMSQ$FAM_SIZE))|(fm15NoBlankROOMSQ$NUM_AUTO>quantile(fm15NoBlankROOMSQ$NUM_AUTO)[4]+3*IQR(fm15NoBlankROOMSQ$NUM_AUTO))|(fm15NoBlankROOMSQ$ROOMSQ>quantile(fm15NoBlankROOMSQ$ROOMSQ)[4]+3*IQR(fm15NoBlankROOMSQ$ROOMSQ))|(fm15NoBlankROOMSQ$NTLGASPQ_T>quantile(fm15NoBlankROOMSQ$NTLGASPQ_T)[4]+3*IQR(fm15NoBlankROOMSQ$NTLGASPQ_T))|(fm15NoBlankROOMSQ$ELCTRCPQ_T>quantile(fm15NoBlankROOMSQ$ELCTRCPQ_T)[4]+3*IQR(fm15NoBlankROOMSQ$ELCTRCPQ_T))|(fm15NoBlankROOMSQ$GASMOPQ_T>quantile(fm15NoBlankROOMSQ$GASMOPQ_T)[4]+3*IQR(fm15NoBlankROOMSQ$GASMOPQ_T))|(fm15NoBlankROOMSQ$INCOME_T>quantile(fm15NoBlankROOMSQ$INCOME_T)[4]+3*IQR(fm15NoBlankROOMSQ$INCOME_T)))),]
dim(fm15r)
dim(fm15Trimmed)

# CLUSTER ANALYSIS ON TRIMMED CUs REGARDLESS OF ZERO OR NONZERO ENERGY EXPENDITURE
# SECTION 4.2.1 Cluster Analysis: Dendrogram
# Cluster Analysis
# Hierarchical Clustering on FAM_SIZE, NUM_AUTO, ROOMSQ, HIGH_EDU, BLS_URBN, and INCOME_T
# Complete linkage method finds similar clusters.
memory.limit(30000)
fm15SocioDemoClusComp<-hclust(dist(scale(fm15Trimmed[,c(4,9,10,11,12,70)])),method="complete")
fm15SocioDemoDendComp<-as.dendrogram(fm15SocioDemoClusComp)
plot(cut(fm15SocioDemoDendComp,h=2)$upper,ylim=c(2,22),leaflab="none",ylab="Distance",xlab="Household",main="Complete Linkage Cluster Dendrogram: Sociodemographics")

# Figure 5
# Ward's minimum variance method aims at finding complact, spherical clusters.
fm15SocioDemoClusWard<-hclust(dist(scale(fm15Trimmed[,c(4,9,10,11,12,70)])),method="ward.D")
fm15SocioDemoDendWard<-as.dendrogram(fm15SocioDemoClusWard)
plot(cut(fm15SocioDemoDendWard,h=2)$upper,ylim=c(100,9000),leaflab="none",ylab="Distance",xlab="Household",main="Ward's Cluster Dendrogram: Sociodemographics")

# Cluster Analysis: Trying Out Different Numbers of Clusters from Wards
fm15SocioDemoWardGroup2<-cutree(fm15SocioDemoClusWard,k=2)
table(fm15SocioDemoWardGroup2,dnn="Number of Households per Cluster (Wards)")

fm15SocioDemoWardGroup3<-cutree(fm15SocioDemoClusWard,k=3)
table(fm15SocioDemoWardGroup3,dnn="Number of Households per Cluster (Wards)")

fm15SocioDemoWardGroup4<-cutree(fm15SocioDemoClusWard,k=4)
table(fm15SocioDemoWardGroup4,dnn="Number of Households per Cluster (Wards)")

fm15SocioDemoWardGroup5<-cutree(fm15SocioDemoClusWard,k=5)
table(fm15SocioDemoWardGroup5,dnn="Number of Households per Cluster (Wards)")

fm15SocioDemoWardGroup6<-cutree(fm15SocioDemoClusWard,k=6)
table(fm15SocioDemoWardGroup6,dnn="Number of Households per Cluster (Wards)")

fm15SocioDemoWardGroup7<-cutree(fm15SocioDemoClusWard,k=7)
table(fm15SocioDemoWardGroup7,dnn="Number of Households per Cluster (Wards)")

fm15SocioDemoWardGroup8<-cutree(fm15SocioDemoClusWard,k=8)
table(fm15SocioDemoWardGroup8,dnn="Number of Households per Cluster (Wards)")

fm15SocioDemoWardGroup9<-cutree(fm15SocioDemoClusWard,k=9)
table(fm15SocioDemoWardGroup9,dnn="Number of Households per Cluster (Wards)")

fm15SocioDemoWardGroup10<-cutree(fm15SocioDemoClusWard,k=10)
table(fm15SocioDemoWardGroup10,dnn="Number of Households per Cluster (Wards)")

# Testing Different Ward Sizes
TwoClusterW<-as.factor(fm15SocioDemoWardGroup2)
ThreeClusterW<-as.factor(fm15SocioDemoWardGroup3)
FourClusterW<-as.factor(fm15SocioDemoWardGroup4)
FiveClusterW<-as.factor(fm15SocioDemoWardGroup5)
SixClusterW<-as.factor(fm15SocioDemoWardGroup6)
SevenClusterW<-as.factor(fm15SocioDemoWardGroup7)
EightClusterW<-as.factor(fm15SocioDemoWardGroup8)
NineClusterW<-as.factor(fm15SocioDemoWardGroup9)
TenClusterW<-as.factor(fm15SocioDemoWardGroup10)
fmClustered<-cbind(fm15Trimmed,TwoClusterW,ThreeClusterW,FourClusterW,FiveClusterW,SixClusterW,SevenClusterW,EightClusterW,NineClusterW,TenClusterW)

# Cluster Analysis: Determining Number of Clusters Based on All Trimmed CUs
# SECTION 4.2.2
# Test Different Cluster Sizes continued
FStat2ClusterW<-c(summary(lm(NTLGASPQ_T~TwoClusterW,data=fmClustered))$fstatistic[1],summary(lm(ELCTRCPQ_T~TwoClusterW,data=fmClustered))$fstatistic[1],summary(lm(GASMOPQ_T~TwoClusterW,data=fmClustered))$fstatistic[1])
names(FStat2ClusterW)<-c("Natural Gas","Electricity","Gasoline")

FStat3ClusterW<-c(summary(lm(NTLGASPQ_T~ThreeClusterW,data=fmClustered))$fstatistic[1],summary(lm(ELCTRCPQ_T~ThreeClusterW,data=fmClustered))$fstatistic[1],summary(lm(GASMOPQ_T~ThreeClusterW,data=fmClustered))$fstatistic[1])
names(FStat3ClusterW)<-c("Natural Gas","Electricity","Gasoline")

FStat4ClusterW<-c(summary(lm(NTLGASPQ_T~FourClusterW,data=fmClustered))$fstatistic[1],summary(lm(ELCTRCPQ_T~FourClusterW,data=fmClustered))$fstatistic[1],summary(lm(GASMOPQ_T~FourClusterW,data=fmClustered))$fstatistic[1])
names(FStat4ClusterW)<-c("Natural Gas","Electricity","Gasoline")

FStat5ClusterW<-c(summary(lm(NTLGASPQ_T~FiveClusterW,data=fmClustered))$fstatistic[1],summary(lm(ELCTRCPQ_T~FiveClusterW,data=fmClustered))$fstatistic[1],summary(lm(GASMOPQ_T~FiveClusterW,data=fmClustered))$fstatistic[1])
names(FStat5ClusterW)<-c("Natural Gas","Electricity","Gasoline")

FStat6ClusterW<-c(summary(lm(NTLGASPQ_T~SixClusterW,data=fmClustered))$fstatistic[1],summary(lm(ELCTRCPQ_T~SixClusterW,data=fmClustered))$fstatistic[1],summary(lm(GASMOPQ_T~SixClusterW,data=fmClustered))$fstatistic[1])
names(FStat6ClusterW)<-c("Natural Gas","Electricity","Gasoline")

FStat7ClusterW<-c(summary(lm(NTLGASPQ_T~SevenClusterW,data=fmClustered))$fstatistic[1],summary(lm(ELCTRCPQ_T~SevenClusterW,data=fmClustered))$fstatistic[1],summary(lm(GASMOPQ_T~SevenClusterW,data=fmClustered))$fstatistic[1])
names(FStat7ClusterW)<-c("Natural Gas","Electricity","Gasoline")

FStat8ClusterW<-c(summary(lm(NTLGASPQ_T~EightClusterW,data=fmClustered))$fstatistic[1],summary(lm(ELCTRCPQ_T~EightClusterW,data=fmClustered))$fstatistic[1],summary(lm(GASMOPQ_T~EightClusterW,data=fmClustered))$fstatistic[1])
names(FStat8ClusterW)<-c("Natural Gas","Electricity","Gasoline")

FStat9ClusterW<-c(summary(lm(NTLGASPQ_T~NineClusterW,data=fmClustered))$fstatistic[1],summary(lm(ELCTRCPQ_T~NineClusterW,data=fmClustered))$fstatistic[1],summary(lm(GASMOPQ_T~NineClusterW,data=fmClustered))$fstatistic[1])
names(FStat9ClusterW)<-c("Natural Gas","Electricity","Gasoline")

FStat10ClusterW<-c(summary(lm(NTLGASPQ_T~TenClusterW,data=fmClustered))$fstatistic[1],summary(lm(ELCTRCPQ_T~TenClusterW,data=fmClustered))$fstatistic[1],summary(lm(GASMOPQ_T~TenClusterW,data=fmClustered))$fstatistic[1])
names(FStat10ClusterW)<-c("Natural Gas","Electricity","Gasoline")

FStatClusterSummaryW<-data.frame(rbind(FStat3ClusterW,FStat4ClusterW,FStat5ClusterW,FStat6ClusterW,FStat7ClusterW,FStat8ClusterW,FStat9ClusterW,FStat10ClusterW))
FStatClusterSummaryW

# It is not as obvious which is the best number of clusters. In terms of maximizing energy expenditure F-Stat values, it is 3, with 5 also looking good. In terms of evening out the cluster sizes, it is 6 or 7.

# Cluster Analysis Using K-Means
# K-Means Clustering on FAM_SIZE, NUM_AUTO, ROOMSQ, HIGH_EDU, BLS_URBN, and INCOME_LT
# KMeans aims to partition the points into k groups such that the sum of squares from points to the assigned cluster centres is minimized. 
# https://www.r-bloggers.com/k-means-clustering-in-r/

set.seed(20)
#Reference columns from fm15Trimmed: BLS_URBN, FAM_SIZE, NUM_AUTO, ROOMSQ, HIGH_EDU, INCOME_T
fm15SocioDemoClusKMeans3<-kmeans(scale(fm15Trimmed[,c(4,9,10,11,12,70)]),centers=3,nstart=20)
fm15SocioDemoClusKMeans3$size
fm15SocioDemoClusKMeans3$centers
KMeansCluster3<-as.factor(fm15SocioDemoClusKMeans3$cluster)

fm15SocioDemoClusKMeans4<-kmeans(scale(fm15Trimmed[,c(4,9,10,11,12,70)]),centers=4,nstart=20)
fm15SocioDemoClusKMeans4$size
fm15SocioDemoClusKMeans4$centers
KMeansCluster4<-as.factor(fm15SocioDemoClusKMeans4$cluster)

fm15SocioDemoClusKMeans5<-kmeans(scale(fm15Trimmed[,c(4,9,10,11,12,70)]),centers=5,nstart=20)
fm15SocioDemoClusKMeans5$size
fm15SocioDemoClusKMeans5$centers
KMeansCluster5<-as.factor(fm15SocioDemoClusKMeans5$cluster)

fm15SocioDemoClusKMeans6<-kmeans(scale(fm15Trimmed[,c(4,9,10,11,12,70)]),centers=6,nstart=20)
fm15SocioDemoClusKMeans6$size
fm15SocioDemoClusKMeans6$centers
KMeansCluster6<-as.factor(fm15SocioDemoClusKMeans6$cluster)

fm15SocioDemoClusKMeans7<-kmeans(scale(fm15Trimmed[,c(4,9,10,11,12,70)]),centers=7,nstart=20)
fm15SocioDemoClusKMeans7$size
fm15SocioDemoClusKMeans7$centers
KMeansCluster7<-as.factor(fm15SocioDemoClusKMeans7$cluster)

fm15SocioDemoClusKMeans8<-kmeans(scale(fm15Trimmed[,c(4,9,10,11,12,70)]),centers=8,nstart=20)
fm15SocioDemoClusKMeans8$size
fm15SocioDemoClusKMeans8$centers
KMeansCluster8<-as.factor(fm15SocioDemoClusKMeans8$cluster)

fmClustered<-cbind(fmClustered,KMeansCluster3,KMeansCluster4,KMeansCluster5,KMeansCluster6,KMeansCluster7,KMeansCluster8)

summary(fmClustered$KMeansCluster3)
summary(fmClustered$KMeansCluster4)
summary(fmClustered$KMeansCluster5)
summary(fmClustered$KMeansCluster6)
summary(fmClustered$KMeansCluster7)
summary(fmClustered$KMeansCluster8)

write.csv(fmClustered,file="fmClusteredwMissingST.csv")

# APPENDIX B
# Tests to determine optimal number of clusters k. 
# Elbow and Gap Statistic method not conclusive. Silhouette suggests either k = 3 or 6.
set.seed(20)

#Elbow Method
wss<-function(k) {
  kmeans(scale(fm15Trimmed[,c(4,9,10,11,12,70)]),k,nstart=20)$tot.withinss
}
k.values<-2:15
wss_values<-map_dbl(k.values,wss)
plot(k.values,wss_values,type="b",pch=19,frame=FALSE,main="Elbow Study",xlab="Number of Clusters",ylab="Total Within-Clusters Sum of Squares",cex.main=2.5,cex.lab=1.3)

#Average Silhouette Method
avg_sil<-function(k){
  km.res<-kmeans(scale(fm15Trimmed[,c(4,9,10,11,12,70)]),centers=k,nstart=20)
  ss<-silhouette(km.res$cluster,dist(scale(fm15Trimmed[,c(4,9,10,11,12,70)])))
  mean(ss[,3])
}
avg_sil_values<-map_dbl(k.values,avg_sil)
plot(k.values,avg_sil_values,type="b",pch=19,frame=FALSE,main="Silhouette Study",xlab="Number of Clusters",ylab="Average Silhouette",cex.main=2.5,cex.lab=1.3)

#Gap Statistic Method
gap_stat<-clusGap(scale(fm15Trimmed[,c(4,9,10,11,12,70)]),FUN=kmeans,nstart=20,K.max=10,B=10)
print(gap_stat,method="firstmax")

# Add Energy Prices and Estimate Energy Consumption
# Create data set "fm15" which includes energy prices from EIA.
# Define variable for consumption PER CAPITA
EIAPrice<-read.csv("EIAPriceByState.csv",comm="#",header=TRUE)
EIAPrice$EIA_NG<-EIAPrice$EIA_NG
EIAPrice$EIA_ELEC<-EIAPrice$EIA_ELEC
EIAPrice$EIA_GAS<-EIAPrice$EIA_GAS
fmClusteredwEIA<-merge(fmClustered,EIAPrice[,1:4],by.x="ST",by.y="EIA_STATE")

fmClusteredwEIA$C_NG<-fmClusteredwEIA$NTLGASPQ/(fmClusteredwEIA$EIA_NG*fmClusteredwEIA$FAM_SIZE)
fmClusteredwEIA$C_ELEC<-(100/293.071070172)*(fmClusteredwEIA$ELCTRCPQ/(fmClusteredwEIA$EIA_ELEC*fmClusteredwEIA$FAM_SIZE))
fmClusteredwEIA$C_GAS<-fmClusteredwEIA$GASMOPQ/(fmClusteredwEIA$EIA_GAS*fmClusteredwEIA$FAM_SIZE)

fmClusteredwEIA$C_NG_T<-log(fmClusteredwEIA$C_NG+((fmClusteredwEIA$C_NG)^2+1)^0.5)
fmClusteredwEIA$C_ELEC_T<-log(fmClusteredwEIA$C_ELEC+((fmClusteredwEIA$C_ELEC)^2+1)^0.5)
fmClusteredwEIA$C_GAS_T<-log(fmClusteredwEIA$C_GAS+((fmClusteredwEIA$C_GAS)^2+1)^0.5)

summary(fmClusteredwEIA)
dim(fmClusteredwEIA)

#write.csv(fmClusteredwEIA,file="fmClusteredwEIA.csv")

# Figure 3
# Histograms of Transformation
layout(matrix(c(1,2,3,4),nrow=2))
hist(fmClusteredwEIA$C_GAS,col="green",xlim=c(0,1000),breaks=c(25*0:212),main="Gasoline Consumption (gal)",xlab="",ylab="",cex.main=1.75,cex.axis=1.2)
hist(fmClusteredwEIA$FINCBTXM,col="gray",xlim=c(0,900000),breaks=c(20000*-2:50),main="Income(USD)",xlab="",ylab="",cex.main=1.75,cex.axis=1.2)
hist(fmClusteredwEIA$C_GAS_T,col="green",xlim=c(0,8),breaks=c(0.2*0:50),main="Arcsinh Transformation",xlab="",ylab="",cex.main=1.75,cex.axis=1.2)
hist(fmClusteredwEIA$INCOME_T,col="gray",xlim=c(0,15),breaks=c(0.5*-22:30),main="Arcsinh Transformation",xlab="",ylab="",cex.main=1.75,cex.axis=1.2)

# Load fmClusteredwEIA Survey Data
fmClusteredwEIA$MO_SCOPE<-as.numeric(ifelse(fmClusteredwEIA$QINTRVMO==2,3,(ifelse(fmClusteredwEIA$QINTRVMO==3,1,3))))
fmCompleteOnly<-subset(fmClusteredwEIA,ST!= "NA")
dim(fmCompleteOnly)

# SECTION 3.1.2: FIGURE 2
# Barplots comparing energy consumption patterns in HI and MN.
HIMNData<-subset(fmClusteredwEIA,ST=="HI"|ST=="MN")[,c(1,20,65,66,89:91)]

HIQ1Responses<-HIMNData[HIMNData$ST=="HI"&HIMNData$PQTR==1,]
HIQ2Responses<-HIMNData[HIMNData$ST=="HI"&HIMNData$PQTR==2,]
HIQ3Responses<-HIMNData[HIMNData$ST=="HI"&HIMNData$PQTR==3,]
HIQ4Responses<-HIMNData[HIMNData$ST=="HI"&HIMNData$PQTR==4,]
MNQ1Responses<-HIMNData[HIMNData$ST=="MN"&HIMNData$PQTR==1,]
MNQ2Responses<-HIMNData[HIMNData$ST=="MN"&HIMNData$PQTR==2,]
MNQ3Responses<-HIMNData[HIMNData$ST=="MN"&HIMNData$PQTR==3,]
MNQ4Responses<-HIMNData[HIMNData$ST=="MN"&HIMNData$PQTR==4,]

PopWgt_HIQ1<-sum(HIQ1Responses$FINLWT21*HIQ1Responses$MO_SCOPE)
PopWgt_HIQ2<-sum(HIQ2Responses$FINLWT21*HIQ2Responses$MO_SCOPE)
PopWgt_HIQ3<-sum(HIQ3Responses$FINLWT21*HIQ3Responses$MO_SCOPE)
PopWgt_HIQ4<-sum(HIQ4Responses$FINLWT21*HIQ4Responses$MO_SCOPE)
PopWgt_MNQ1<-sum(MNQ1Responses$FINLWT21*MNQ1Responses$MO_SCOPE)
PopWgt_MNQ2<-sum(MNQ1Responses$FINLWT21*MNQ2Responses$MO_SCOPE)
PopWgt_MNQ3<-sum(MNQ1Responses$FINLWT21*MNQ3Responses$MO_SCOPE)
PopWgt_MNQ4<-sum(MNQ1Responses$FINLWT21*MNQ4Responses$MO_SCOPE)

HIMNData$PopWgtByState<-ifelse(HIMNData$ST=="HI"&HIMNData$PQTR==1,PopWgt_HIQ1,ifelse(HIMNData$ST=="HI"&HIMNData$PQTR==2,PopWgt_HIQ2,ifelse(HIMNData$ST=="HI"&HIMNData$PQTR==3,PopWgt_HIQ3,ifelse(HIMNData$ST=="HI"&HIMNData$PQTR==4,PopWgt_HIQ4,ifelse(HIMNData$ST=="MN"&HIMNData$PQTR==1,PopWgt_MNQ1,ifelse(HIMNData$ST=="MN"&HIMNData$PQTR==2,PopWgt_MNQ2,ifelse(HIMNData$ST=="MN"&HIMNData$PQTR==3,PopWgt_MNQ3,PopWgt_MNQ4)))))))

HIMNData$StateMeanComponent_NG<-(HIMNData$C_NG*HIMNData$FINLWT21*HIMNData$MO_SCOPE)/HIMNData$PopWgtByState
HIMNData$StateMeanComponent_ELEC<-(HIMNData$C_ELEC*HIMNData$FINLWT21*HIMNData$MO_SCOPE)/HIMNData$PopWgtByState
HIMNData$StateMeanComponent_GAS<-(HIMNData$C_GAS*HIMNData$FINLWT21*HIMNData$MO_SCOPE)/HIMNData$PopWgtByState

HIMNCompare_NG<-aggregate(HIMNData[,9],by=HIMNData[c(1,3)],FUN=sum)
HIMNCompare_NG<-rbind(HIMNCompare_NG[3:8,],HIMNCompare_NG[1:2,])
HIMNCompare_ELEC<-aggregate(HIMNData[,10],by=HIMNData[c(1,3)],FUN=sum)
HIMNCompare_ELEC<-rbind(HIMNCompare_ELEC[3:8,],HIMNCompare_ELEC[1:2,])
HIMNCompare_GAS<-aggregate(HIMNData[,11],by=HIMNData[c(1,3)],FUN=sum)
HIMNCompare_GAS<-rbind(HIMNCompare_GAS[3:8,],HIMNCompare_GAS[1:2,])

HIMNMatrix_NG<-matrix(HIMNCompare_NG$x,ncol=4,byrow=FALSE)
colnames(HIMNMatrix_NG)<-c("Q1","Q2","Q3","Q4")
rownames(HIMNMatrix_NG)<-c("HI","MN")
HIMNMatrix_NG<-as.table(HIMNMatrix_NG)
HIMNMatrix_NG

HIMNMatrix_ELEC<-matrix(HIMNCompare_ELEC$x,ncol=4,byrow=FALSE)
colnames(HIMNMatrix_ELEC)<-c("Q1","Q2","Q3","Q4")
rownames(HIMNMatrix_ELEC)<-c("HI","MN")
HIMNMatrix_ELEC<-as.table(HIMNMatrix_ELEC)
HIMNMatrix_ELEC

HIMNMatrix_GAS<-matrix(HIMNCompare_GAS$x,ncol=4,byrow=FALSE)
colnames(HIMNMatrix_GAS)<-c("Q1","Q2","Q3","Q4")
rownames(HIMNMatrix_GAS)<-c("HI","MN")
HIMNMatrix_GAS<-as.table(HIMNMatrix_GAS)
HIMNMatrix_GAS

layout(matrix(c(1,2,3),nrow=1))
par(mar=c(5,7,8,2))
HIMNFillDensity<-c(100,25)
HIMNFillAngle<-c(0,45)

barplot(HIMNMatrix_NG,beside=TRUE, col=c("turquoise","violet"),density=HIMNFillDensity,angle=HIMNFillAngle,ylab="Consumption",main="Natural Gas (Mcf) \n HI vs MN",space=c(0,0.5),cex.lab=3,cex.main=3,cex.sub=2.5,cex.names=3,cex.axis=1.7)
barplot(HIMNMatrix_ELEC,beside=TRUE, col=c("turquoise","violet"),density=HIMNFillDensity,angle=HIMNFillAngle,ylab="Consumption",main="Electricity (MMBtu) \n HI vs MN",space=c(0,0.5),cex.lab=3,cex.main=3,cex.sub=2.5,cex.names=3,cex.axis=1.7)
barplot(HIMNMatrix_GAS,beside=TRUE, col=c("turquoise","violet"),density=HIMNFillDensity,angle=HIMNFillAngle,ylab="Consumption",main="Gasoline (gal) \n HI vs MN",space=c(0,0.5),cex.lab=3,cex.main=3,cex.sub=2.5,cex.names=3,cex.axis=1.7)
legend("topright",legend=c("HI","MN"),fill=c("turquoise","violet"),density=HIMNFillDensity,angle=HIMNFillAngle,bty="n",cex=2)

# Section 4.2.3 Sociodemographic Cluster Characteristics for Table 4 and Appendix
fmClusteredwEIA$MO_SCOPE<-as.numeric(ifelse(fmClusteredwEIA$QINTRVMO==2,2,(ifelse(fmClusteredwEIA$QINTRVMO==3,1,3))))

Pop<-sum(fmClusteredwEIA$FINLWT21*fmClusteredwEIA$MO_SCOPE)

C1Responses<-fmClusteredwEIA[fmClusteredwEIA$KMeansCluster6==1,]
C2Responses<-fmClusteredwEIA[fmClusteredwEIA$KMeansCluster6==2,]
C3Responses<-fmClusteredwEIA[fmClusteredwEIA$KMeansCluster6==3,]
C4Responses<-fmClusteredwEIA[fmClusteredwEIA$KMeansCluster6==4,]
C5Responses<-fmClusteredwEIA[fmClusteredwEIA$KMeansCluster6==5,]
C6Responses<-fmClusteredwEIA[fmClusteredwEIA$KMeansCluster6==6,]

PopWgt_C1<-sum(C1Responses$FINLWT21*C1Responses$MO_SCOPE)
PopWgt_C2<-sum(C2Responses$FINLWT21*C2Responses$MO_SCOPE)
PopWgt_C3<-sum(C3Responses$FINLWT21*C3Responses$MO_SCOPE)
PopWgt_C4<-sum(C4Responses$FINLWT21*C4Responses$MO_SCOPE)
PopWgt_C5<-sum(C5Responses$FINLWT21*C5Responses$MO_SCOPE)
PopWgt_C6<-sum(C6Responses$FINLWT21*C6Responses$MO_SCOPE)

fmClusteredWeightedStudy<-fmClusteredwEIA[,c(1:13,19:20,66,70,83,89:94)]

fmClusteredWeightedStudy$PopWgtByCluster<-ifelse(fmClusteredWeightedStudy$KMeansCluster6==1,PopWgt_C1,ifelse(fmClusteredWeightedStudy$KMeansCluster6==2,PopWgt_C2,ifelse(fmClusteredWeightedStudy$KMeansCluster6==3,PopWgt_C3,ifelse(fmClusteredWeightedStudy$KMeansCluster6==4,PopWgt_C4,ifelse(fmClusteredWeightedStudy$KMeansCluster6==5,PopWgt_C5,PopWgt_C6)))))

#Average Quantiative Sociodemogrphic Characteristics of Six Clusters
fmClusteredWeightedStudy$Comp_FAM_SIZE<-(fmClusteredWeightedStudy$FAM_SIZE*fmClusteredWeightedStudy$FINLWT21*fmClusteredWeightedStudy$MO_SCOPE)/fmClusteredWeightedStudy$PopWgtByCluster
fmClusteredWeightedStudy$Comp_NUM_AUTO<-(fmClusteredWeightedStudy$NUM_AUTO*fmClusteredWeightedStudy$FINLWT21*fmClusteredWeightedStudy$MO_SCOPE)/fmClusteredWeightedStudy$PopWgtByCluster
fmClusteredWeightedStudy$Comp_ROOMSQ<-(fmClusteredWeightedStudy$ROOMSQ*fmClusteredWeightedStudy$FINLWT21*fmClusteredWeightedStudy$MO_SCOPE)/fmClusteredWeightedStudy$PopWgtByCluster
fmClusteredWeightedStudy$Comp_HIGH_EDU<-(fmClusteredWeightedStudy$HIGH_EDU*fmClusteredWeightedStudy$FINLWT21*fmClusteredWeightedStudy$MO_SCOPE)/fmClusteredWeightedStudy$PopWgtByCluster
fmClusteredWeightedStudy$Comp_FINCBTXM<-(fmClusteredWeightedStudy$FINCBTXM*fmClusteredWeightedStudy$FINLWT21*fmClusteredWeightedStudy$MO_SCOPE)/fmClusteredWeightedStudy$PopWgtByCluster

ClustAggData1<-aggregate(fmClusteredWeightedStudy[,26:30],by=fmClusteredWeightedStudy[18],FUN=sum)
names(ClustAggData1)<-c("Cluster","Family Size","Cars","Rooms","Years Education","Income")
ClustAggData1

#Average Per Capita Energy Consumption Levels NOT Arcsinh Transformed
fmClusteredWeightedStudy$Comp_C_NG<-(fmClusteredWeightedStudy$C_NG*fmClusteredWeightedStudy$FINLWT21*fmClusteredWeightedStudy$MO_SCOPE)/fmClusteredWeightedStudy$PopWgtByCluster
fmClusteredWeightedStudy$Comp_C_ELEC<-(fmClusteredWeightedStudy$C_ELEC*fmClusteredWeightedStudy$FINLWT21*fmClusteredWeightedStudy$MO_SCOPE)/fmClusteredWeightedStudy$PopWgtByCluster
fmClusteredWeightedStudy$Comp_C_GAS<-(fmClusteredWeightedStudy$C_GAS*fmClusteredWeightedStudy$FINLWT21*fmClusteredWeightedStudy$MO_SCOPE)/fmClusteredWeightedStudy$PopWgtByCluster

ClustAggData2<-aggregate(fmClusteredWeightedStudy[,31:33],by=fmClusteredWeightedStudy[18],FUN=sum)
names(ClustAggData1)<-c("Cluster","Natural Gas","Electricity","Gasoline")
ClustAggData2

# NOTE: there are a handful of CUIDs that changed sociodemographic cluster from one survey to another.

PopWeight<-sum(fmClusteredwEIA$FINLWT21*fmClusteredwEIA$MO_SCOPE)
C_NG_Mean<-sum(fmClusteredwEIA$C_NG*fmClusteredwEIA$FINLWT21*fmClusteredwEIA$MO_SCOPE)/PopWeight
C_ELEC_Mean<-sum(fmClusteredwEIA$C_ELEC*fmClusteredwEIA$FINLWT21*fmClusteredwEIA$MO_SCOPE)/PopWeight
C_GAS_Mean<-sum(fmClusteredwEIA$C_GAS*fmClusteredwEIA$FINLWT21*fmClusteredwEIA$MO_SCOPE)/PopWeight

C_NG_Mean
C_ELEC_Mean
C_GAS_Mean

FAM_SIZE_Mean<-sum(fmClusteredwEIA$FAM_SIZE*fmClusteredwEIA$FINLWT21*fmClusteredwEIA$MO_SCOPE)/PopWeight
NUM_AUTO_Mean<-sum(fmClusteredwEIA$NUM_AUTO*fmClusteredwEIA$FINLWT21*fmClusteredwEIA$MO_SCOPE)/PopWeight
ROOMSQ_Mean<-sum(fmClusteredwEIA$ROOMSQ*fmClusteredwEIA$FINLWT21*fmClusteredwEIA$MO_SCOPE)/PopWeight
HIGH_EDU_Mean<-sum(fmClusteredwEIA$HIGH_EDU*fmClusteredwEIA$FINLWT21*fmClusteredwEIA$MO_SCOPE)/PopWeight
FINCBTXM_Mean<-sum(fmClusteredwEIA$FINCBTXM*fmClusteredwEIA$FINLWT21*fmClusteredwEIA$MO_SCOPE)/PopWeight
FAM_SIZE_Mean
NUM_AUTO_Mean
ROOMSQ_Mean
HIGH_EDU_Mean
FINCBTXM_Mean

# INELASTICITY OF ENERGY DEMAND
# SECTION 4.1: TABLE 2, FIGURE 4**
# Consumption vs Price
fmNZ_NG<-subset(fmClusteredwEIA,C_NG!=0)
fmNZ_NG<-subset(fmNZ_NG,EIA_NG!=1)
fmNZ_ELEC<-subset(fmClusteredwEIA,C_ELEC!=0)
fmNZ_ELEC<-subset(fmNZ_ELEC,EIA_ELEC!=1)
fmNZ_GAS<-subset(fmClusteredwEIA,C_GAS!=0)
fmNZ_GAS<-subset(fmNZ_GAS,EIA_GAS!=1)
summary(lm(C_NG_T~EIA_NG,weights=FINLWT21,data=fmNZ_NG))
summary(lm(C_ELEC_T~EIA_ELEC,weights=FINLWT21,data=fmNZ_ELEC))
summary(lm(C_GAS_T~EIA_GAS,weights=FINLWT21,data=fmNZ_GAS))

# SECTION 4.1: FIGURE 4
# Scatterplots of Consumption vs Price
library(scales)
layout(matrix(c(1,2,3),nrow=1))
par(mar=c(5,8,8,2))
plot(C_NG_T~EIA_NG,data=fmNZ_NG,col=alpha("blue",0.01),pch=16,xlab="Retail Price (USD/Mcf)",ylab="Arcsinh \n (Consumption)",main="Natural Gas: \n Consumption vs Price",cex.lab=2.7,cex.main=3,cex.sub=2.5,cex.axis=1.7,cex=2)
plot(C_ELEC_T~EIA_ELEC,data=fmNZ_ELEC,col=alpha("red",0.01),pch=16,xlab="Retail Price (USc/MMBtu)",ylab="Arcsinh \n (Consumption)",main="Electricity: \n Consumption vs Price",cex.lab=2.7,cex.main=3,cex.sub=2.5,cex.axis=1.7,cex=2)
plot(C_GAS_T~EIA_GAS,data=fmNZ_GAS,col=alpha("green",0.01),pch=16,xlab="Retail Price (USD/gal)",ylab="Arcsinh \n (Consumption)",main="Gasoline: \n Consumption vs Price",cex.lab=2.7,cex.main=3,cex.sub=2.5,cex.axis=1.7,cex=2)

# SECTION 4.4 CHI-SQUARE ANALYSIS AND MOSAIC PLOTS
# Chi-Square Analysis on Other Households: Refer to fmCompleteOnly dataset
# SECTION 4.4.1: Table 6
#  Households in which fuel oil and other fuel expenditures are claimed
fmCompleteOnly$AbbreviatedREGION<-fmCompleteOnly$REGION
levels(fmCompleteOnly$AbbreviatedREGION)<-c("NE","MW","S","W")
fmCompleteOnly$AbbreviatedBUILDING<-fmCompleteOnly$BUILDING
levels(fmCompleteOnly$AbbreviatedBUILDING)<-c("Apt","Det","NonDet","Oth")
fmCompleteOnly$AbbreviatedCUTENURE<-fmCompleteOnly$CUTENURE
levels(fmCompleteOnly$AbbreviatedCUTENURE)<-c("Own","Oth","Rent")
fmCompleteOnly$AbbreviatedFAM_TYPE<-fmCompleteOnly$FAM_TYPE
levels(fmCompleteOnly$AbbreviatedFAM_TYPE)<-c("MwO","MC","Oth","SPar","SPer")
fmCompleteOnly$AbbreviatedCluster<-fmCompleteOnly$KMeansCluster6
levels(fmCompleteOnly$AbbreviatedCluster)<-c("uUC","uLF","r","uMC","uBC","uW")

fmCompleteOnly$UseFuel<-as.factor(ifelse(fmCompleteOnly$FULOILPQ==0&fmCompleteOnly$OTHFLSPQ==0,"No","Yes"))
dim(fmCompleteOnly)
FuelUseRegion<-table(fmCompleteOnly$UseFuel,fmCompleteOnly$AbbreviatedREGION)
FuelUseBuildingType<-table(fmCompleteOnly$UseFuel,fmCompleteOnly$AbbreviatedBUILDING)
FuelUseCutenure<-table(fmCompleteOnly$UseFuel,fmCompleteOnly$AbbreviatedCUTENURE)
FuelUseFamType<-table(fmCompleteOnly$UseFuel,fmCompleteOnly$AbbreviatedFAM_TYPE)
FuelUseCluster<-table(fmCompleteOnly$UseFuel,fmCompleteOnly$AbbreviatedCluster)

chisq.test(FuelUseRegion)
mosaicplot(FuelUseRegion,color=TRUE,main="Fuel Use by Region",xlab="Fuel Use",ylab="Region")

chisq.test(FuelUseBuildingType)
mosaicplot(FuelUseBuildingType,color=TRUE,main="Fuel Use by Building Type",xlab="Fuel Use",ylab="Building Type")

chisq.test(FuelUseCutenure)
mosaicplot(FuelUseCutenure,color=TRUE,main="Fuel Use by Occupancy Tenure",xlab="Fuel Use",ylab="Occupancy Tenure")

chisq.test(FuelUseFamType)
mosaicplot(FuelUseFamType,color=TRUE,main="Fuel Use by Family Type",xlab="Fuel Use",ylab="Family Type")

chisq.test(FuelUseCluster)
mosaicplot(FuelUseCluster,color=TRUE,main="Fuel Use by Cluster",xlab="Fuel Use",ylab="Cluster")

# Figure 10
layout(matrix(c(1,2),nrow=1))
par(mar=c(3,3,3,1))
mosaicplot(FuelUseBuildingType,color=TRUE,main="Fuel Use by Building Type",xlab="Fuel Use",ylab="Building Type",cex.axis=0.85,las=1)
mosaicplot(FuelUseCutenure,color=TRUE,main="Fuel Use by Occupancy Tenure",xlab="Fuel Use",ylab="Occupancy Tenure",cex.axis=0.85,las=1)

# SECTION 4.4.2: Mosaic Plots
# Natural Gas
fmCompleteOnly$UseNG<-as.factor(ifelse(fmCompleteOnly$NTLGASPQ==0,"No","Yes"))
dim(fmCompleteOnly)
NGUseRegion<-table(fmCompleteOnly$UseNG,fmCompleteOnly$AbbreviatedREGION)
NGUseBuildingType<-table(fmCompleteOnly$UseNG,fmCompleteOnly$AbbreviatedBUILDING)
NGUseCutenure<-table(fmCompleteOnly$UseNG,fmCompleteOnly$AbbreviatedCUTENURE)
NGUseFamType<-table(fmCompleteOnly$UseNG,fmCompleteOnly$AbbreviatedFAM_TYPE)
NGUseCluster<-table(fmCompleteOnly$UseNG,fmCompleteOnly$AbbreviatedCluster)

chisq.test(NGUseRegion)
mosaicplot(NGUseRegion,color=TRUE,main="Natural Gas Use by Region",xlab="Natural Gas Use",ylab="Region")

chisq.test(NGUseBuildingType)
mosaicplot(NGUseBuildingType,color=TRUE,main="Natural Gas Use by Building Type",xlab="Natural Gas Use",ylab="Building Type")

chisq.test(NGUseCutenure)
mosaicplot(NGUseCutenure,color=TRUE,main="Natural Gas Use by Occupancy Tenure",xlab="Natural Gas Use",ylab="Occupancy Tenure")

chisq.test(NGUseFamType)
mosaicplot(NGUseFamType,color=TRUE,main="Natural Gas Use by Family Type",xlab="Natural Gas Use",ylab="Family Type")

chisq.test(NGUseCluster)
mosaicplot(NGUseCluster,color=TRUE,main="Natural Gas Use by Cluster",xlab="Natural Gas Use",ylab="Cluster")

fmCompleteOnlyOrderedCluster<-fmCompleteOnly[order(fmCompleteOnly$AbbreviatedCluster),]
fmCompleteOnlyOrderedCluster$AbbreviatedCluster<-factor(fmCompleteOnlyOrderedCluster$AbbreviatedCluster,levels=c("r","uBC","uLC","uMC","uUC","uW"))
NGUseOrderedCluster<-table(fmCompleteOnlyOrderedCluster$UseNG,fmCompleteOnlyOrderedCluster$AbbreviatedCluster)
mosaicplot(NGUseOrderedCluster,color=TRUE,main="Natural Gas Use by Cluster",xlab="Natural Gas Use",ylab="Cluster")

# Electricity
fmCompleteOnly$UseElec<-as.factor(ifelse(fmCompleteOnly$ELCTRCPQ==0,"No","Yes"))
dim(fmCompleteOnly)
ElecUseRegion<-table(fmCompleteOnly$UseElec,fmCompleteOnly$AbbreviatedREGION)
ElecUseBuildingType<-table(fmCompleteOnly$UseElec,fmCompleteOnly$AbbreviatedBUILDING)
ElecUseCutenure<-table(fmCompleteOnly$UseElec,fmCompleteOnly$AbbreviatedCUTENURE)
ElecUseFamType<-table(fmCompleteOnly$UseElec,fmCompleteOnly$AbbreviatedFAM_TYPE)
ElecUseCluster<-table(fmCompleteOnly$UseElec,fmCompleteOnly$AbbreviatedCluster)

chisq.test(ElecUseRegion)
mosaicplot(ElecUseRegion,color=TRUE,main="Electricity Use by Region",xlab="Electricity Use",ylab="Region")

chisq.test(ElecUseBuildingType)
mosaicplot(ElecUseBuildingType,color=TRUE,main="Electricity Use by Building Type",xlab="Electricity Use",ylab="Building Type")

chisq.test(ElecUseCutenure)
mosaicplot(ElecUseCutenure,color=TRUE,main="Electricity Use by Occupancy Tenure",xlab="Electricity Use",ylab="Occupancy Tenure")

chisq.test(ElecUseFamType)
mosaicplot(ElecUseFamType,color=TRUE,main="Electricity Use by Family Type",xlab="Electricity Use",ylab="Family Type")

chisq.test(ElecUseCluster)
mosaicplot(ElecUseCluster,color=TRUE,main="Electricity Use by Cluster",xlab="Electricity Use",ylab="Cluster")

fmCompleteOnlyOrderedCluster<-fmCompleteOnly[order(fmCompleteOnly$AbbreviatedCluster),]
fmCompleteOnlyOrderedCluster$AbbreviatedCluster<-factor(fmCompleteOnlyOrderedCluster$AbbreviatedCluster,levels=c("r","uBC","uLC","uMC","uUC","uW"))
ElecUseOrderedCluster<-table(fmCompleteOnlyOrderedCluster$UseElec,fmCompleteOnlyOrderedCluster$AbbreviatedCluster)
mosaicplot(ElecUseOrderedCluster,color=TRUE,main="Electricity Use by Cluster",xlab="Electricity Use",ylab="Cluster")

# Gasoline and Motor Oil
fmCompleteOnly$UseGas<-as.factor(ifelse(fmCompleteOnly$GASMOPQ==0,"No","Yes"))
dim(fmCompleteOnly)
GasUseRegion<-table(fmCompleteOnly$UseGas,fmCompleteOnly$AbbreviatedREGION)
GasUseBuildingType<-table(fmCompleteOnly$UseGas,fmCompleteOnly$AbbreviatedBUILDING)
GasUseCutenure<-table(fmCompleteOnly$UseGas,fmCompleteOnly$AbbreviatedCUTENURE)
GasUseFamType<-table(fmCompleteOnly$UseGas,fmCompleteOnly$AbbreviatedFAM_TYPE)
GasUseCluster<-table(fmCompleteOnly$UseGas,order(fmCompleteOnly$AbbreviatedCluster))

chisq.test(GasUseRegion)
mosaicplot(GasUseRegion,color=TRUE,main="Natural Gas Use by Region",xlab="Gasoline and Motor Oil Use",ylab="Region")

chisq.test(GasUseBuildingType)
mosaicplot(GasUseBuildingType,color=TRUE,main="Gasoline and Motor Oil Use \n by Building Type",xlab="Gasoline and Motor Oil Use",ylab="Building Type")

chisq.test(GasUseCutenure)
mosaicplot(GasUseCutenure,color=TRUE,main="Gasoline and Motor Oil Use by Occupancy Tenure",xlab="Gasoline and Motor Oil Use",ylab="Occupancy Tenure")

chisq.test(GasUseFamType)
mosaicplot(GasUseFamType,color=TRUE,main="Gasoline and Motor Oil Use by Family Type",xlab="Gasoline and Motor Oil Use",ylab="Family Type")

chisq.test(GasUseCluster)
mosaicplot(GasUseCluster,color=TRUE,main="Gasoline and Motor Oil Use by Cluster",xlab="Gasoline and Motor Oil Use",ylab="Cluster")

# Figure 11
layout(matrix(c(1,2,3,4),nrow=2))
par(mar=c(3,3,4,0))
mosaicplot(NGUseOrderedCluster,color=TRUE,main="Natural Gas Use by Cluster",xlab="Natural Gas Use",ylab="Cluster",cex.axis = 0.85,las=1)
mosaicplot(ElecUseOrderedCluster,color=TRUE,main="Electricity Use by Cluster",xlab="Electricity Use",ylab="Cluster",cex.axis = 0.85,las=1)
mosaicplot(NGUseRegion,color=TRUE,main="Natural Gas Use by Region",xlab="Natural Gas Use",ylab="Region",cex.axis = 0.85,las=1)
mosaicplot(GasUseBuildingType,color=TRUE,main="Gasoline and Motor Oil Use \n by Building Type",xlab="Gasoline and Motor Oil Use",ylab="Building Type",cex.axis = 0.85,las=1)

# SECTION 4.3: Table 5, Figures 6-9
# Plots using SAS Results from Dr. M Wijesinha: FigxData, FigxDiffData

# Figure 6
Fig6Data<-read.csv("Figure6DataNG.csv",comm="#",header=TRUE)
ClustDiffLabels<-subset(Fig6Data,Domain=="Northeast")$DiffMean

layout(matrix(c(1,2,3,4),nrow=2))
op<-par(mar=c(5,6,3,1))

#Northeast
plot(subset(Fig6Data,Domain=="Northeast")$Estimate,1:6,xlim=c(-6,6),type="p",yaxt="n",xlab="Mean Consumption",ylab="",main="Northeast",cex.main=1.75,cex.lab=1.5,cex.axis=1.25)
axis(side=2,at=1:6,labels=ClustDiffLabels,las=1)
with(subset(Fig6Data,Domain=="Northeast"),arrows(LCLMean,1:6,UCLMean,1:6,code=3,angle=90,length=0.04))
#with(subset(Fig6Data,Domain=="Northeast"),text(Estimate,1:6,Tukey,pos=3))

#South
plot(subset(Fig6Data,Domain=="South")$Estimate,1:6,xlim=c(-6,6),type="p",yaxt="n",xlab="Mean Consumption",ylab="",main="South",cex.main=1.75,cex.lab=1.5,cex.axis=1.25)
axis(side=2,at=1:6,ClustDiffLabels,las=1)
with(subset(Fig6Data,Domain=="South"),arrows(LCLMean,1:6,UCLMean,1:6,code=3,angle=90,length=0.04))
#with(subset(Fig6Data,Domain=="South"),text(Estimate,1:6,Tukey,pos=3))

#Midwest
plot(subset(Fig6Data,Domain=="Midwest")$Estimate,1:6,xlim=c(-6,6),type="p",yaxt="n",xlab="Mean Consumption",ylab="",main="Midwest",cex.main=1.75,cex.lab=1.5,cex.axis=1.25)
axis(side=2,at=1:6,labels=ClustDiffLabels,las=1)
with(subset(Fig6Data,Domain=="Midwest"),arrows(LCLMean,1:6,UCLMean,1:6,code=3,angle=90,length=0.04))
#with(subset(Fig6Data,Domain=="Midwest"),text(Estimate,1:6,Tukey,pos=3))

#West
plot(subset(Fig6Data,Domain=="West")$Estimate,1:6,xlim=c(-6,6),type="p",yaxt="n",xlab="Mean Consumption",ylab="",main="West",cex.main=1.75,cex.lab=1.5,cex.axis=1.25)
axis(side=2,at=1:6,labels=ClustDiffLabels,las=1)
with(subset(Fig6Data,Domain=="West"),arrows(LCLMean,1:6,UCLMean,1:6,code=3,angle=90,length=0.04))
#with(subset(Fig6Data,Domain=="West"),text(Estimate,1:6,Tukey,pos=3))

# Figure 6 Differences
Fig6DiffData<-read.csv("Figure6DataNGDiff.csv",comm="#",header=TRUE)
ClustDiffLabels<-subset(Fig6DiffData,Domain=="Northeast")$DiffMean

layout(matrix(c(1,2,3,4),nrow=2))
op<-par(mar=c(5,6,3,1))

#Northeast
plot(subset(Fig6DiffData,Domain=="Northeast")$Estimate,1:15,xlim=c(-7,7),type="p",yaxt="n",xlab="Difference in Means",ylab="",main="Northeast",cex.main=1.75,cex.lab=1.5,cex.axis=1.25)
lines(c(0,0),c(0,16),lty=3)
axis(side=2,at=1:15,labels=ClustDiffLabels,las=1)
with(subset(Fig6DiffData,Domain=="Northeast"),arrows(LCLMean,1:15,UCLMean,1:15,code=3,angle=90,length=0.04))

#South
plot(subset(Fig6DiffData,Domain=="South")$Estimate,1:15,xlim=c(-7,7),type="p",yaxt="n",xlab="Difference in Means",ylab="",main="South",cex.main=1.75,cex.lab=1.5,cex.axis=1.25)
lines(c(0,0),c(0,16),lty=3)
axis(side=2,at=1:15,ClustDiffLabels,las=1)
with(subset(Fig6DiffData,Domain=="South"),arrows(LCLMean,1:15,UCLMean,1:15,code=3,angle=90,length=0.04))

#Midwest
plot(subset(Fig6DiffData,Domain=="Midwest")$Estimate,1:15,xlim=c(-7,7),type="p",yaxt="n",xlab="Difference in Means",ylab="",main="Midwest",cex.main=1.75,cex.lab=1.5,cex.axis=1.25)
lines(c(0,0),c(0,16),lty=3)
axis(side=2,at=1:15,labels=ClustDiffLabels,las=1)
with(subset(Fig6DiffData,Domain=="Midwest"),arrows(LCLMean,1:15,UCLMean,1:15,code=3,angle=90,length=0.04))

#West
plot(subset(Fig6DiffData,Domain=="West")$Estimate,1:15,xlim=c(-7,7),type="p",yaxt="n",xlab="Difference in Means",ylab="",main="West",cex.main=1.75,cex.lab=1.5,cex.axis=1.25)
lines(c(0,0),c(0,16),lty=3)
axis(side=2,at=1:15,labels=ClustDiffLabels,las=1)
with(subset(Fig6DiffData,Domain=="West"),arrows(LCLMean,1:15,UCLMean,1:15,code=3,angle=90,length=0.04))

# Figure 7
setwd("C:/Users/joyance.meechai/OneDrive - United Nations/Computational Statistics")
Fig7Data<-read.csv("Figure7DataELEC.csv",comm="#",header=TRUE)
ClustDiffLabels<-subset(Fig7Data,Domain=="Northeast")$Cluster

layout(matrix(c(1,2,3,4),nrow=2))
op<-par(mar=c(5,6,3,1))

#Northeast
plot(subset(Fig7Data,Domain=="Northeast")$Estimate,1:6,xlim=c(0,4),type="p",yaxt="n",xlab="Mean Consumption",ylab="",main="Northeast",cex.main=1.75,cex.lab=1.5,cex.axis=1.25)
axis(side=2,at=1:6,labels=ClustDiffLabels,las=1)
with(subset(Fig7Data,Domain=="Northeast"),arrows(LCLMean,1:6,UCLMean,1:6,code=3,angle=90,length=0.04))

#South
plot(subset(Fig7Data,Domain=="South")$Estimate,1:6,xlim=c(0,4),type="p",yaxt="n",xlab="Mean Consumption",ylab="",main="South",cex.main=1.75,cex.lab=1.5,cex.axis=1.25)
axis(side=2,at=1:6,ClustDiffLabels,las=1)
with(subset(Fig7Data,Domain=="South"),arrows(LCLMean,1:6,UCLMean,1:6,code=3,angle=90,length=0.04))

#Midwest
plot(subset(Fig7Data,Domain=="Midwest")$Estimate,1:6,xlim=c(0,4),type="p",yaxt="n",xlab="Mean Consumption",ylab="",main="Midwest",cex.main=1.75,cex.lab=1.5,cex.axis=1.25)
axis(side=2,at=1:6,labels=ClustDiffLabels,las=1)
with(subset(Fig7Data,Domain=="Midwest"),arrows(LCLMean,1:6,UCLMean,1:6,code=3,angle=90,length=0.04))

#West
plot(subset(Fig7Data,Domain=="West")$Estimate,1:6,xlim=c(0,4),type="p",yaxt="n",xlab="Mean Consumption",ylab="",main="West",cex.main=1.75,cex.lab=1.5,cex.axis=1.25)
axis(side=2,at=1:6,labels=ClustDiffLabels,las=1)
with(subset(Fig7Data,Domain=="West"),arrows(LCLMean,1:6,UCLMean,1:6,code=3,angle=90,length=0.04))

# Figure 7 Difference
Fig7DiffData<-read.csv("Figure7DataELECDiff.csv",comm="#",header=TRUE)
ClustDiffLabels<-subset(Fig7DiffData,Domain=="Northeast")$DiffMean

layout(matrix(c(1,2,3,4),nrow=2))
op<-par(mar=c(5,6,3,1))

#Northeast
plot(subset(Fig7DiffData,Domain=="Northeast")$Estimate,1:15,xlim=c(-4,4),type="p",yaxt="n",xlab="Difference in Means",ylab="",main="Northeast",cex.main=1.75,cex.lab=1.5,cex.axis=1.25)
lines(c(0,0),c(0,16),lty=3)
axis(side=2,at=1:15,labels=ClustDiffLabels,las=1)
with(subset(Fig7DiffData,Domain=="Northeast"),arrows(LCLMean,1:15,UCLMean,1:15,code=3,angle=90,length=0.04))

#South
plot(subset(Fig7DiffData,Domain=="South")$Estimate,1:15,xlim=c(-4,4),type="p",yaxt="n",xlab="Difference in Means",ylab="",main="South",cex.main=1.75,cex.lab=1.5,cex.axis=1.25)
lines(c(0,0),c(0,16),lty=3)
axis(side=2,at=1:15,ClustDiffLabels,las=1)
with(subset(Fig7DiffData,Domain=="South"),arrows(LCLMean,1:15,UCLMean,1:15,code=3,angle=90,length=0.04))

#Midwest
plot(subset(Fig7DiffData,Domain=="Midwest")$Estimate,1:15,xlim=c(-4,4),type="p",yaxt="n",xlab="Difference in Means",ylab="",main="Midwest",cex.main=1.75,cex.lab=1.5,cex.axis=1.25)
lines(c(0,0),c(0,16),lty=3)
axis(side=2,at=1:15,labels=ClustDiffLabels,las=1)
with(subset(Fig7DiffData,Domain=="Midwest"),arrows(LCLMean,1:15,UCLMean,1:15,code=3,angle=90,length=0.04))

#West
plot(subset(Fig7DiffData,Domain=="West")$Estimate,1:15,xlim=c(-4,4),type="p",yaxt="n",xlab="Difference in Means",ylab="",main="West",cex.main=1.75,cex.lab=1.5,cex.axis=1.25)
lines(c(0,0),c(0,16),lty=3)
axis(side=2,at=1:15,labels=ClustDiffLabels,las=1)
with(subset(Fig7DiffData,Domain=="West"),arrows(LCLMean,1:15,UCLMean,1:15,code=3,angle=90,length=0.04))

# Figures 8 and 9
Fig8Data<-read.csv("Figure8DataGAS.csv",comm="#",header=TRUE)
Fig8DiffData<-read.csv("Figure8DataGASDiff.csv",comm="#",header=TRUE)
Fig9Data<-read.csv("Figure9DataGAS.csv",comm="#",header=TRUE)
Fig9DiffData<-read.csv("Figure9DataGASDiff.csv",comm="#",header=TRUE)

layout(matrix(c(1),nrow=1))

#GasByRegion
par(mar=c(5,8,3,1))
plot(Fig8Data$Estimate,1:4,xlim=c(3.25,4.25),ylim=c(0.75,4.25),type="p",yaxt="n",xlab="Mean",ylab="",main="Consumption by Region",cex.main=2.5,cex.lab=2,cex.axis=1.75)
axis(side=2,at=1:4,labels=Fig8Data$Region,las=1,cex.axis=1.75)
with(Fig8Data,arrows(LCLMean,1:4,UCLMean,1:4,code=3,angle=90,length=0.04))

#GasByRegionDiff
par(mar=c(5,8,3,1))
plot(Fig8DiffData$Estimate,1:6,xlim=c(-1,1),ylim=c(0.75,6.25),type="p",yaxt="n",xlab="Difference in Means",ylab="",main="Consumption by Region",cex.main=2.5,cex.lab=2,cex.axis=1.75)
lines(c(0,0),c(0,7),lty=3)
axis(side=2,at=1:6,labels=Fig8DiffData$DiffMean,las=1,cex.axis=1.75)
with(Fig8DiffData,arrows(LCLMean,1:6,UCLMean,1:6,code=3,angle=90,length=0.04))


#GasByCluster
par(mar=c(5,8,3,1))
plot(Fig9Data$Estimate,1:6,xlim=c(3,4.5),ylim=c(0.75,6.25),type="p",yaxt="n",xlab="Mean",ylab="",main="Consumption by Cluster",cex.main=2.5,cex.lab=2,cex.axis=1.75)
axis(side=2,at=1:6,labels=Fig9Data$Cluster,las=1,cex.axis=1.75)
with(Fig9Data,arrows(LCLMean,1:6,UCLMean,1:6,code=3,angle=90,length=0.04))

#GasByClusterDiff
par(mar=c(5,9,3,1))
plot(Fig9DiffData$Estimate,1:15,xlim=c(-1,1),type="p",yaxt="n",xlab="Difference in Means",ylab="",main="Consumption by Cluster",cex.main=2.5,cex.lab=2,cex.axis=1.75)
lines(c(0,0),c(0,16),lty=3)
axis(side=2,at=1:15,labels=Fig9DiffData$DiffMean,las=1,cex.axis=1.75)
with(Fig9DiffData,arrows(LCLMean,1:15,UCLMean,1:15,code=3,angle=90,length=0.04))