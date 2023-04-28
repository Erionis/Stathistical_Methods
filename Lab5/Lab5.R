############################################################
# Lab 5: Esplicative Qualitative
############################################################

####Esplicativa dicotomica

mem=read.table(file="memoria1.txt",header=TRUE)
boxplot(mem)

#Testiamo l'uguaglianza delle varianze
bartlett.test(list(mem$Younger,mem$Older))

#Verifichiamo l'assunzione di normalità delle popolazioni
qqnorm(mem$Younger)
qqline(mem$Younger)
qqnorm(mem$Older)
qqline(mem$Older)


#t-test per confronto medie 

ttest=t.test(mem$Younger,mem$Older,alternative="greater", var.equal = TRUE)
ttest


##data frame risposta-esplicative
mem.df=data.frame(parole=c(mem$Younger,mem$Older), eta=c(rep("Younger",10), rep("Older",10)))
mem.df
mem.df$eta


plot(mem.df$parole ~ mem.df$eta)

#modello lineare: 'eta' viene tradotta in una variabile indicatrice

fit=lm(parole ~ eta, data = mem.df)
#summary(fit)

#matrice delle esplicative
model.matrix(fit)



### Diverse parametrizzazioni #####################################

#fit2 esclude l'intercetta dal modello
fit2=lm(parole ~ -1 + eta, data = mem.df)
summary(fit2)
model.matrix(fit2)

#cambiamo l'ordine dei livelli del fattore
mem.df$eta1=factor(as.character(mem.df$eta),
                   levels = c("Younger", "Older"))
mem.df$eta1

#stimiamo un muovo modello
fit3=lm(parole ~ eta1, data=mem.df)
summary(fit)
summary(fit3)


#####Esplicativa qualitativa con più livelli 

#Carichiamo e visualizziamo i dati
mA=read.table(file="memoriaAll.txt",stringsAsFactors=FALSE)

names(mA)[1:2]=c("Age","Process")



# m conterra' il nuovo data.frame
# inizializzo m a null, per poterlo poi aggiornare
m=NULL
for (i in 1:10){ #ciclo sulle righe
  for (j in 1:10){ #ciclo sulle colonne
    # si costruisce la riga r-esima della matrice risultato
    agg=data.frame(mA[i,c(1,2,2+j)])
    # si definiscono i nomi di colonna della riga r-esima
    colnames(agg)=c("Age","Process","Words")
    # si aggiunge la riga al data.frame m
    m=rbind(m,agg)
  }}

#Seconda opzione  
vettoreWords=stack(mA[,-(1:2)])[,1]
vettoreWords
m2=data.frame(Age=mA[,1],Process=mA[,2],Words=vettoreWords)
m2
#Terza opzione
m3=reshape(mA, direction="long", varying=names(mA)[-(1:2)],
           v.names="Words")



#Trasformiamo 'Age' e 'Process' in fattori
m$Age=factor(m$Age)
m$Process=factor(as.character(m$Process),
                 levels=c("Counting","Rhyming", "Adjective", "Imagery", "Intentional"))

#boxplot
par(mfrow=c(1,2))
plot(m$Words~m$Age)
plot(m$Words~m$Process)


##################################################
##ANOVA a un fattore
##################################################

#dati sui giovani
m.g=subset(m,Age=="Younger")
layout(1:1)
plot(m.g$Words~m.g$Process)

#modello lineare
fit.g=lm(Words ~ Process,data=m.g)
summary(fit.g)
#ANOVA
anova(fit.g)

#Verifica del sistema d'ip H0:beta2=0,
#beta3=beta4=beta5 

#Ricodifichiamo il fattore process

m.g$Proc2=m.g$Process
levels(m.g$Proc2)
levels(m.g$Proc2)=c("I","I","II","II","II")
levels(m.g$Proc2)

fit.g2=lm(Words ~ Proc2, data=m.g)
summary(fit.g2)

#Confronto modello ricodificato-modello originale
anova(fit.g2,fit.g)

########ANOVA a due fattori


##Consideriamo effetto congiunto di processo ed età

fit.2=lm(Words~ Process + Age,data = m)
summary(fit.2)

anova(fit.2)

#Confrontiamo le medie dei gruppi per livelli

par(mfrow=c(1,2),mar=c(5,4,0.2,4))
interaction.plot(m$Process,m$Age,m$Words)
interaction.plot(m$Age,m$Process,m$Words)

fit.int=lm(Words~Process*Age,data=m)
summary(fit.int)
anova(fit.int)
anova(fit.2,fit.int)

###Variabili esplicative `miste'#########################################################

###Record Olimpici

r=read.table(file="risultati100m.dat")
head(r)

layout(1:1)
plot(r$Risultato~r$Anno, xlab="Anno", ylab="Tempo migliore", 
     pch=as.character(r$Sesso))

#Modello lineare che non tiene conto del sesso
fit0=lm(Risultato~Anno,data=r)
summary(fit0)

#Aggiungiamo l'effetto Sesso
fit1=lm(Risultato~Anno+Sesso,data=r)
summary(fit1)

#Modello con interazione
fit2=lm(Risultato~Anno*Sesso,data=r)
summary(fit2)

#anova(fit1,fit2)

#Otteniamo la Matrice di varianza di hat.beta
fit2.s=summary(fit2,correlation=TRUE)
fit2.s$correlation

#Centriamo la variabile Anno
r$Anno1=r$Anno-mean(r$Anno)

#senza interazione
fit1s=lm(Risultato~Anno1+Sesso,data=r)
summary(fit1s)
#con interazione
fit2s=lm(Risultato~Anno1*Sesso,data=r)
fit2s.s=summary(fit2s,correlation=TRUE)
fit2s.s$correlation
summary(fit2s)

#Diagramma di dispersione
plot(r$Risultato~r$Anno,xlab="Anno",
     ylab="Tempo migliore",pch=as.character(r$Sesso))

abline(coef(fit0))  #modello base
#somma effetto
abline(coef(fit1)[1:2],col="red")
abline(coef(fit1)[1:2]+c(coef(fit1)[3],0),col="red")
#interazione
abline(coef(fit2)[1:2],col="green")
abline(coef(fit2)[1:2]+coef(fit2)[3:4],col="green")



####Diverse distanze#############################################################################
#carichiamo i dati

rr=read.table("risultatiCorsa.dat",header=TRUE)
head(rr)

#definiamo var. 'colori' e 'simboli' per M/F e diverse distanze

colori=c("blue","red")[1+(rr$Sesso=="W")]
rr$DistanzaF=factor(rr$Distanza)
simboli=as.double(rr$DistanzaF)
#livelli del fattore rr$DistanzaF 
levels(rr$DistanzaF)

par(mar=c(5,4,0.2,5),xpd=NA)
plot(rr$Risultato~rr$Anno,pch=simboli,col=colori)
legend(2014,1500,pch=1:7,legend=levels(rr$DistanzaF))

#Creiamo la var risposta
rr$Velocita=rr$Distanza/rr$Risultato
plot(rr$Anno,rr$Velocita,pch=simboli,col=colori)

#Riportiamo i tempi al tempo equivalente sui 100m
rr$Tempoeq=100*rr$Risultato/rr$Distanza
plot(rr$Anno,rr$Tempoeq,pch=simboli,col=colori)

#centriamo la variabile anno
rr$Annoc=rr$Anno-1964

#Stimiamo diversi modelli
fitnoint=lm(Tempoeq~Annoc+Sesso+DistanzaF,data=rr)

fitintAD=lm(Tempoeq~Annoc*DistanzaF+Sesso,data=rr)
fitintAS=lm(Tempoeq~Annoc*Sesso+DistanzaF,data=rr)
fitintSD=lm(Tempoeq~Annoc+DistanzaF*Sesso,data=rr)
fitintASD=lm(Tempoeq~Annoc*Sesso*DistanzaF,data=rr)

##Contronto mediante BIC
BIC(fitnoint)
BIC(fitintASD)
BIC(fitintAD)
BIC(fitintAS)
BIC(fitintSD)


#modello senza interazioni
summary(fitnoint)

################################################################################################



###Consumi delle automobili

auto=read.table(file="auto2.dat")
head(auto)
levels(auto$alimentazione2)

colori=c("red","blue","green")
plot(auto$cons.misto~auto$potenzacv,col=colori[as.double(auto$alimentazione2)],pch=20)

fit1=lm(cons.misto~potenzacv,data=auto)
summary(fit1)

fit2=lm(cons.misto~potenzacv+alimentazione2,data=auto)
fit3=lm(cons.misto~potenzacv*alimentazione2,data=auto)
plot(fit2)
plot(fit3)

anova(fit3)

fit1=lm(log(cons.misto)~log(potenzacv),data=auto)
fit2=lm(log(cons.misto)~log(potenzacv)+alimentazione2,data=auto)
fit3=lm(log(cons.misto)~log(potenzacv)*alimentazione2,data=auto)

anova(fit3)
summary(fit2)



##Gatti######################################################################################

gatti=read.table(file="Cats.dat",col.names=c("pcorpo","pcuore","sesso"))
head(gatti)

gatti$sesso
gatti$sesso=factor(gatti$sesso)
gatti$sesso
levels(gatti$sesso)=c("F","M")
gatti$sesso

plot(gatti$pcuore~gatti$sesso)

fit=lm(pcuore~sesso,data=gatti)
summary(fit)

plot(gatti$pcorpo~gatti$sesso)

plot(gatti$pcuore~gatti$pcorpo,pch=as.character(gatti$sesso))

fit=lm(pcuore~pcorpo+sesso,data=gatti)
summary(fit)

plot(gatti$pcuore~gatti$pcorpo,pch=as.character(gatti$sesso))
abline(coef(fit)[1:2])
abline(coef(fit)[1:2] + c(coef(fit)[3],0))