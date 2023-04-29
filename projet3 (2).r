###importation des données###
data=read.table(file=file.choose(),header=T,sep=",",dec=".")
data
#type des variables
str(data)
dim(data)
summary(data)
#données abérantes
boxplot(data$A)#pas de données abérantes
boxplot(data$P)#pas de données abérantes
boxplot(data$C)#pas de données abérantes
boxplot(data$Lk)#pas de données abérantes
boxplot(data$Wk)#pas de données abérantes
boxplot(data$Ac)#deux valeurs abérantes
boxplot(data$Lkg)#pas de données abérantes

library("ggplot2")
ggplot(data,aes(x=varietie,y=A,fill=varietie))+geom_boxplot()
+xlab(label="Varietie")+ ylab(label="A")
+ggtitle("Boites à moustache")
#pour le champs A on a des données aberante pour le type kama wheat 
s=data$A[data$varietie=="Kama wheat"]
#View(s)
summary(s)
Q1=17.99
Q3=19.14
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
#imputation pour A 
s[which(s>Vmax)]=Vmax
s[which(s<Vmin)]=Vmin
data$A[data$varietie=="Kama wheat"]=s
library("ggplot2")
ggplot(data,aes(x=varietie,y=A,fill=varietie))+geom_boxplot()
+xlab(label="Varietie")+ ylab(label="A")
+ggtitle("Boites à moustache")

#champs P
library("ggplot2")
ggplot(data,aes(x=varietie,y=P,fill=varietie))+geom_boxplot()
+xlab(label="Varietie")+ ylab(label="P")
+ggtitle("Boites à moustache")
#pour le champs P on a des données aberante pour le type kama wheat 
s=data$P[data$varietie=="Kama wheat"]
View(s)
summary(s)
Q1=15.86
Q3=16.57
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
#imputation pour P
s[which(s>Vmax)]=Vmax
s[which(s<Vmin)]=Vmin
data$P[data$varietie=="Kama wheat"]=s
library("ggplot2")
ggplot(data,aes(x=varietie,y=P,fill=varietie))+geom_boxplot()
+xlab(label="Varietie")+ ylab(label="P")
+ggtitle("Boites à moustache")


#champs C
library("ggplot2")
ggplot(data,aes(x=varietie,y=C,fill=varietie))+geom_boxplot()
+xlab(label="Varietie")+ ylab(label="C")
+ggtitle("Boites à moustache")
#pour le champs C on a des données aberante pour le type kama wheat 
s=data$C[data$varietie==""]
View(s)
summary(s)
Q1=0.8644
Q3=0.8779
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
#imputation pour C
s[which(s>Vmax)]=Vmax
s[which(s<Vmin)]=Vmin
data$C[data$varietie==""]=s
library("ggplot2")
ggplot(data,aes(x=varietie,y=C,fill=varietie))+geom_boxplot()
+xlab(label="Varietie")+ ylab(label="C")
+ggtitle("Boites à moustache")


#champs Lk
library("ggplot2")
ggplot(data,aes(x=varietie,y=Lk,fill=varietie))+geom_boxplot()
+xlab(label="Varietie")+ ylab(label="Lk")
+ggtitle("Boites à moustache")
#pour le champs Lk on a des données aberante pour le type kama wheat et canadian 
s=data$Lk[data$varietie=="Canadian wheat"]
View(s)
summary(s)
Q1=5.357
Q3=5.658
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
#imputation pour Lk canadian
s[which(s>Vmax)]=Vmax
s[which(s<Vmin)]=Vmin
data$Lk[data$varietie=="Canadian wheat"]=s

#imputation pour Lk kama
z=data$Lk[data$varietie=="Kama wheat"]
View(z)
summary(z)
Q1=5.980
Q3=6.315
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
z[which(z>Vmax)]=Vmax
z[which(z<Vmin)]=Vmin
data$Lk[data$varietie=="Kama wheat"]=z

library("ggplot2")
ggplot(data,aes(x=varietie,y=Lk,fill=varietie))+geom_boxplot()
+xlab(label="Varietie")+ ylab(label="Lk")
+ggtitle("Boites à moustache")

summary(data)
#champs Wk
library("ggplot2")
ggplot(data,aes(x=varietie,y=Wk,fill=varietie))+geom_boxplot()
+xlab(label="Varietie")+ ylab(label="Wk")
+ggtitle("Boites à moustache")
#pour le champs C on a pas de données aberantes 


#champ Ac
ggplot(data,aes(x=varietie,y=Ac,fill=varietie))+geom_boxplot()
+xlab(label="Varietie")+ ylab(label="Ac")
+ggtitle("Boites à moustache")
#pour le champs Ac on a des données aberantes pour les type Rosa et Canadian

#Ac/canadian wheat
s1=data$Ac[data$varietie=="Canadian wheat"]
View(s1)
summary(s1)
Q1=1.8355
Q3=3.1990
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
#imputation pour Ac pour canadian wheat 
s1[which(s1>Vmax)]=Vmax
s1[which(s1<Vmin)]=Vmin
data$Ac[data$varietie=="Canadian wheat"]=s1

#mainten
#maintenant Ac/Rosa wheat
s2=data$Ac[data$varietie=="Rosa wheat"]
View(s2)
summary(s2)
Q1=4.032
Q3=5.470
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
#imputation pour Ac pour Rosa wheat 
s2[which(s2>Vmax)]=Vmax
s2[which(s2<Vmin)]=Vmin
data$Ac[data$varietie=="Rosa wheat"]=s2

ggplot(data,aes(x=varietie,y=Ac,fill=varietie))+geom_boxplot()
+xlab(label="Varietie")+ ylab(label="Ac")
+ggtitle("Boites à moustache")
########################valeurs manquantes##############

#õn a remarquee qu'il existe des données manquantes pour la variable varietie
#dont le nombre est 6 donc 3% de la totalité des données donc on peut les supprimer
data=data[(data$varietie!=""),]
summary(data)
dim(data)
#donc on a 4 données manquantes pour la variable C et une pour la variable Lk
sum(is.na(data))/prod(dim(data))
#on a 0.34% donc on peut les effacer mais on peut faire l'imputation par moyenne puisque
#les valeurs de chaque variable sont trés rapprocher mais par rapport a chaque 
#type de varieté
install.packages("VIM")
canadian=data[(data$varietie=="Canadian wheat"),]
summary(canadian)
library(VIM)
canadian=kNN(canadian)
summary(canadian)
na.fail(canadian)
data[(data$varietie=="Canadian wheat"),]=canadian

kama=data[(data$varietie=="Kama wheat"),]
summary(kama)
library(VIM)
kama=kNN(kama)
summary(kama)
na.fail(kama)
data[(data$varietie=="Kama wheat"),]=kama

rosa=data[(data$varietie=="Rosa wheat"),]
summary(rosa)
library(VIM)
rosa=kNN(rosa)
summary(rosa)
na.fail(rosa)
data[(data$varietie=="Rosa wheat"),]=rosa
summary(data)

#######"test de normalite de la variable A########################""
shapiro.test(data$A)
#p_value<0.05 donc on a pas la normalité
#######"test de normalite de la variable P########################""
shapiro.test(data$P)
#p_value<0.05 donc on a pas la normalité
#######"test de normalite de la variable C########################""
shapiro.test(data$C)
#p_value<0.05 donc on a pas la normalité
#######"test de normalite de la variable Lk########################""
shapiro.test(data$Lk)
#p_value<0.05 donc on a pas la normalité
#######"test de normalite de la variable Wk########################""
shapiro.test(data$Wk)
#p_value<0.05 donc on a pas la normalité
#######"test de normalite de la variable Ac########################""
shapiro.test(data$Ac)
#p_value<0.05 donc on a pas la normalité
#######"test de normalite de la variable Lkg########################""
shapiro.test(data$Lkg)
#p_value<0.05 donc on a pas la normalité 
str(data)
#############################analyse bivariée###################
#plot de correlation avec A
par(mfrow=c(2,3))
plot(data$A,data$P)
plot(data$A,data$C)
plot(data$A ,data$Lk)
plot(data$A,data$Wk)
plot(data$A,data$Ac)
plot(data$A,data$Lkg)

#plot de correlation avec P
par(mfrow=c(2,3))
plot(data$P,data$A)
plot(data$P,data$C)
plot(data$P,data$Lk)
plot(data$P,data$Wk)
plot(data$P,data$Ac)
plot(data$P,data$Lkg)

#plot de correlation avec C
par(mfrow=c(2,3))
plot(data$C,data$P)
plot(data$C,data$A)
plot(data$C,data$Lk)
plot(data$C,data$Wk)
plot(data$C,data$Ac)
plot(data$C,data$Lkg)

#plot de correlation avec LK
par(mfrow=c(2,3))
plot(data$Lk,data$P)
plot(data$Lk,data$C)
plot(data$Lk,data$A)
plot(data$Lk,data$Wk)
plot(data$Lk,data$Ac)
plot(data$Lk,data$Lkg)

#plot de correlation avec Wk
par(mfrow=c(2,3))
plot(data$Wk,data$P)
plot(data$Wk,data$C)
plot(data$Wk,data$A)
plot(data$Wk,data$Lk)
plot(data$Wk,data$Ac)
plot(data$Wk,data$Lkg)

#plot de correlation avec AC
par(mfrow=c(2,3))
plot(data$Ac,data$P)
plot(data$Ac,data$C)
plot(data$Ac,data$Lk)
plot(data$Ac,data$Wk)
plot(data$Ac,data$A)
plot(data$Ac,data$Lkg)

#plot de correlation avec Lkg
par(mfrow=c(2,3))
plot(data$Lkg,data$P)
plot(data$Lkg,data$C)
plot(data$Lkg,data$Lk)
plot(data$Lkg,data$Wk)
plot(data$Lkg,data$Ac)
plot(data$Lkg,data$A)

#puisque on a pas la normalité des variables quantitatives donc
#pour faire le test de corrélation on doit utiliser la methode de "spearman"
cor(data[,c('A','P','C','Lk','Wk','Ac','Lkg')],method="spearman")
#la variable Ac est presque non corrélée avec toutes les autres variables
#la variable C est faiblement corrélée avec Lk et Lkg(target)
#les autres sont bien corrélées  

#test de correlation
cor.test(data$A,data$P,method="spearman")
#pvalue < 2.2e − 16
#on accepte alors H1:les deux variables sont bien correlees
cor.test(data$A,data$C,method="spearman")
cor.test(data$A,data$Lk,method="spearman")
cor.test(data$A,data$Wk,method="spearman")
cor.test(data$A,data$Ac,method="spearman") #######
cor.test(data$A,data$Lkg,method="spearman")
cor.test(data$P,data$C,method="spearman")
cor.test(data$P,data$Lk,method="spearman")
cor.test(data$P,data$Wk,method="spearman")
cor.test(data$P,data$Ac,method="spearman")####
cor.test(data$P,data$Lkg,method="spearman")
cor.test(data$C,data$Lk,method="spearman")####
cor.test(data$C,data$Wk,method="spearman")
cor.test(data$C,data$Ac,method="spearman")####
cor.test(data$C,data$Lkg,method="spearman")#####
cor.test(data$Lk,data$Wk,method="spearman")
cor.test(data$Lk,data$Ac,method="spearman")######
cor.test(data$Lk,data$Lkg,method="spearman")
cor.test(data$Wk,data$Ac,method="spearman")######
cor.test(data$Wk,data$Lkg,method="spearman")
cor.test(data$Ac,data$Lkg,method="spearman")######
#pour la correlation avec la variable qualitative
#on a pas la normalité des autres variable quantitatives  
#de plus on a 3 modalités pour la variables qualitative
#donc on utilise Test de Kruskall-Wallis pour trouver sa corrélation
kruskal.test(data$A ~ data$varietie)
#p-value < 2.2e-16 donc on accepte H1 alors on a une bonne corrélation
kruskal.test(data$P ~ data$varietie)
#p-value < 2.2e-16 donc on accepte H1 alors on a une bonne corrélation
kruskal.test(data$C ~ data$varietie)
#p-value < 2.2e-16 donc on accepte H1 alors on a une bonne corrélation
kruskal.test(data$Lk ~ data$varietie)
#p-value < 2.2e-16 donc on accepte H1 alors on a une bonne corrélation
kruskal.test(data$Wk ~ data$varietie)
#p-value < 2.2e-16 donc on accepte H1 alors on a une bonne corrélation
kruskal.test(data$Ac ~ data$varietie)
#p-value = 4.366e-15 < 0.05 donc on accepte H1 alors on a une bonne corrélation
kruskal.test(data$Lkg ~ data$varietie)




Model1=lm(Lkg~A+P+C+Lk+Wk+Ac ,data=data)
summary(Model1)
#R^2=0.9242 donc 92% de la variabilité de Lkg est expliquée par les autres variables

Model2=lm(Lkg~A+P+C+Lk+Ac ,data=data)
summary(Model2)
#R^2=0.9232 donc on a toujours 92% de la variabilité de Lkg est expliquée
#donc le modèle ne perd pas de qualité

AIC(Model1)
AIC(Model2)
#On peut remarquer que l’AIC du Model2 est plus faible alors ce modele est le meilleur entre les deux

Model3=lm(Lkg~A+P+C+Lk ,data=data)
summary(Model3)
#R^2=0.9174 donc on a 91% de la variabilité de Lkg est expliquée qui est 
#inferieure par rapport au model2 donc on reste avec le model 2



############# ACP ##########
data1=data[,1:6]
pcData=princomp(data1, cor= "True")
summary(pcData)
###scree plot
screeplot(pcData)
#donc on a besoin de 3 PC

library(psych)
pc=principal(data1,nfactors=3,rotate="none")
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
#######PCA(data1, scale.unit = TRUE, ncp = 3, graph = TRUE)
trg = predict(pcData, data1)
datapca = data.frame(trg, data1)
datapca=datapca[,0:3]
datapca$Lkg =data$Lkg
datapca$varietie =data$varietie
datapca



####regression datapca##############
Modelx=lm(Lkg~Comp.1+Comp.2+Comp.3,data=datapca)
summary(Modelx)
#R^2=0.9 donc 90% de la variabilité de Lkg est expliquée par les autres variables
#de PCA
#les variable on une P_value tres proche de 0 donc si on modifie le model 
#on va perdre de qualité 
#En effet:

Modely=lm(Lkg~Comp.1+Comp.2,data=datapca)
summary(Modely)
#R^2=0.8274, donc le model perd de qualité 
#donc on reste avec Modelx


########"tache 6##################
datapca2=datapca
datapca2=datapca2[,-4]
#normalisation entre 0 et 1###########""
min_max=function(x)
	{
	res=(x-min(x))/(max(x)-min(x))
	return(res)
	}
newdata=as.data.frame(sapply(datapca2,min_max))

regLOGIT=glm(Lkg ~ Comp.1+Comp.2+Comp.3,data=newdata,family=binomial(link=logit))
summary(regLOGIT)
##AIC=122.38

install.packages('tidyverse')
install.packages('sjPlot')
library(tidyverse)
library(sjPlot)
library(lme4)
tab_model(regLOGIT)

regPoisson=glm(Lkg ~ Comp.1+Comp.2+Comp.3,data=newdata,family=poisson)
summary(regPoisson)
##AIC=NA

regQuasiPoisson=glm(Lkg ~ Comp.1+Comp.2+Comp.3,data=newdata,family=quasipoisson)
summary(regQuasiPoisson)
##AIC=NA

regPROBIT=glm(Lkg ~ Comp.1+Comp.2+Comp.3,data=newdata, family=binomial(link=probit))
summary(regPROBIT)
##AIC=123.46

#apres avoir faire tous les modele du GLM on constate que le AIC pour le 
#GLM avec family =binomial(link=logit) est le meilleur car il a L'AIC le plus 
#faible





