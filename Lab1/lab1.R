
##################################################################################

#1. La regressione semplice

##################################################################################

# Il file 'olimpiadi100m' contiene i tempi di percorrenza dei 100m dei vincitori
# di medaglie d'oro nelle olimpiadi dal 1896 al 2008

#Acquisizione dei dati
d=read.table(file = "olimpiadi100m.csv", sep = ",", header = TRUE)
is(d)
head(d)

#Studiamo la relazione esistente tra il tempo di percorrenza e l'anno
x = d$Anno
y = d$Risultato
x
y
n = length(x)   
n
#diagramma di dispersione
plot(x, y)      

#### stime dei minimi quadrati dei coefficienti 
#stima del coefficiente angolare 
beta2.stima = (sum(x * y)/n - mean(x) * mean(y))/(mean(x^2) - mean(x)^2)
beta2.stima = cov(x, y)/var(x)
beta2.stima

#sima dell'intercetta
beta1.stima = mean(y) - beta2.stima * mean(x)
beta1.stima

#retta stimata con abline()
#visualizziamo l'help 
?abline

abline(beta1.stima, beta2.stima)


#Otteniamo i valori teorici
y.teorici = beta1.stima + beta2.stima * x

#aggiungiamo i valori teorici sul grafico
points(x, y.teorici, pch = 4)

#otteniamo i residui del modello
residui = y - y.teorici

#proprietà algebriche
sum(residui)
sum(residui * x)
sum(residui * y.teorici)

#Scomposizione della devianza

#devianza totale
devtot = sum((y - mean(y))^2)
#devianza spiegata 
devsp = sum((y.teorici - mean(y))^2)
#devianza residua
devres = sum((y - y.teorici)^2)

devsp + devres

#coefficiente di determinazione R2

R2 = 1 - devres/devtot
R2

#oppure
devsp/devtot


#Stima della varianza della distribuzione degli errori

sigma2.stima = sum(residui^2)/n

#stima non distorta

s2 = sigma2.stima * n/(n - 2)

#Stime delle varianze degli stimatori 

var.beta2.stima = s2/sum((x - mean(x))^2)

var.beta1.stima = s2 * (1/n + mean(x)^2/sum((x - mean(x))^2))


###IC per beta2 al 95% 

beta2.inf = beta2.stima - qt(0.975, df = n - 2) * sqrt(var.beta2.stima)
beta2.sup = beta2.stima + qt(0.975, df = n - 2) * sqrt(var.beta2.stima)

c(beta2.inf,beta2.sup)

# lo stesso intervallo si ottiene anche con:
beta2.ic2 = beta2.stima + c(-1, 1) * qt(0.975, df = n - 2) * sqrt(var.beta2.stima)
beta2.ic3 = beta2.stima + qt(c(0.025, 0.975), df = n - 2) * sqrt(var.beta2.stima)
beta2.ic2
beta2.ic3

##Verifica dell'ipotesi di nullità di beta2

t2 = beta2.stima/sqrt(var.beta2.stima)

# valore p 

alfaoss = 2 * (1 - pt(abs(t2), n - 2))

#Calcoliamo la previsione per la successiva olimpiade (2012)

t.2012 = beta1.stima + beta2.stima * 2012
t.2012

#intervallo di confidenza al 95% per la stima di E(Y|X=2012)

t.2012.ic = t.2012 + c(-1, 1) * qt(0.975, n - 2) * 
  sqrt(s2 * (1/n + (2012-mean(x))^2/sum((x - mean(x))^2)))
t.2012.ic

#intervallo di previsione al 95% per Y0=Y|X=2012

t.2012.iprev = t.2012 + c(-1, 1) * qt(0.975, n - 2) * 
  sqrt(s2 * (1 + 1/n + (2012 - mean(x))^2/sum((x - mean(x))^2)))
t.2012.iprev


###########################################################################

##2. La regressione semplice con la funzione lm()

###########################################################################

#Utilizziamo la funzione lm()
?lm
fit = lm(y ~ x) 
fit

fit$coefficients
fit$residuals
fit$fitted.values

#oppure
coef(fit)
resid(fit)
fitted(fit)

summary(fit)

#memorizziamo il risultato di summary()
fit.sum = summary(fit)
names(fit.sum)

fit.sum$coefficients
#l'errore standard di beta2.stima è
fit.sum$coefficients[2, 2]

#R^2
fit.sum$r.squared


#Intervalli di confidenza per i parametri

help(confint)

#default: 95%
confint(fit)

#IC di livello 0.99
confint(fit, "x", level = 0.99)
confint(fit, "(Intercept)", level = 0.99)

#Previsioni in corrispondenza a arbitrari valori di x
help(predict.lm)

#default: valori teorici
predict(fit)   
predict(fit, se.fit = TRUE)
predict(fit, se.fit = FALSE, interval = "confidence", level = 0.95)

#Definiamo un nuovo data frame
dati.nuovi = data.frame(x = seq(1896, 2016, by = 4))
dati.nuovi

#IC per E(Y|X=x)
#default level is .95
prev.ic = predict(fit, newdata = dati.nuovi, interval = "confidence")
prev.ic

#Int. previsione per Y0=(Y|X=x)
prev.iprev = predict(fit, newdata = dati.nuovi, interval = "prediction")
prev.iprev

## diagramma di dispersione
plot(x, y, xlim = range(dati.nuovi$x), ylim = range(prev.iprev))
matlines(dati.nuovi$x, prev.ic, lty = c(1, 2, 2), col = "black")
#prev.iprev[, -1] esclude la colonna 'fit'
matlines(dati.nuovi$x, prev.iprev[, -1], lty = c(2, 2), col = "red")



#########################################################################

## 3.Analisi dei residui

##########################################################################

#visualizziamo l'oggetto 'residui'
residui

#residui standardizzati
res.std=residui/sqrt((1-1/n-(x-mean(x))^2/sum((x-mean(x))^2)))
res.std

##residui studentizzati
res.st=residui/sqrt(s2*(1-1/n-(x-mean(x))^2/sum((x-mean(x))^2)))
res.st
#o utilizzando la funzione rstandard()
res.st=rstandard(fit)
res.st


#diagramma di dispersione dei residui
par(mfrow=c(1,3))

plot(x,residui)
abline(h=0)
plot(x,res.std)
abline(h=0)
plot(x,res.st)
abline(h=0)

#boxplot dei residui

par(mfrow=c(1,3))

boxplot(residui)
boxplot(res.std)
boxplot(res.st)


#dENSITà-ISTOGRAMMA
par(mfrow=c(1,2))
hist(res.std,freq=FALSE)
curve(dnorm(x,0,sd=sqrt(s2)),add=TRUE,col="red")
hist(res.st,freq=FALSE)
curve(dnorm(x,0,sd=1),add=TRUE,col="red")


#diagramma dei quantili osservati contro i quantili teorici
plot(qnorm((1:27-.5)/n),sort(res.std))
xcord<-qnorm(c(0.25,0.75))
ycord<-quantile(res.std,c(0.25,0.75))
slope <- diff(ycord)/diff(xcord)
int <- ycord[1] - slope * xcord[1]
abline(int,slope)

#usiamo le funzioni qqnorm() e qqline()

par(mfrow=c(1,2))
qqnorm(res.std)
qqline(res.std)

qqnorm(res.st)
qqline(res.st)



####################################
# Dati sulle automobili
#####################################

d=read.table(file="auto.csv",sep="\t",dec=",")
dim(d)
head(d)

plot(d$potenzacv,d$accel)
identify(d$potenzacv,d$accel,row.names(d))


#modello lineare

fit0=lm(accel~potenzacv,data=d)
summary(fit0)


#Grafici residui

#residui studentizzati 
residui=rstandard(fit0)
par(mfrow=c(1,2))

#Grafico dei residui contro i valori teorici
plot(fitted(fit0),residui)
abline(h=0)

#Nornal Q-Q plot
qqnorm(residui)
qqline(residui)


#oppure chiamando plot() con argomento l'0ggetto lm

par(mfrow=c(1,2))
plot(fit0, which=c(1,2))


#Applichiamo diverse trasformazioni alla variabile esplicativa

plot(log(d$potenzacv),d$accel)
cor(log(d$potenzacv),d$accel)

plot(sqrt(d$potenzacv),d$accel)
cor(sqrt(d$potenzacv),d$accel)

plot(1/(d$potenzacv),d$accel)
cor(1/(d$potenzacv),d$accel)

plot(1/sqrt(d$potenzacv),d$accel)
cor(1/sqrt(d$potenzacv),d$accel)


#Analizziamo i quattro modelli

fitlog=lm(accel~log(potenzacv),data=d)

fitsqrt=lm(accel~sqrt(potenzacv),data=d)

fitrec=lm(accel~I(1/(potenzacv)),data=d)

fitrecsqrt=lm(accel~I(1/sqrt(potenzacv)),data=d)


#Analisi dei residui 
par(mfrow=c(1,2))
#modello 1
plot(fitlog,which=c(1,2))
#modello 2
plot(fitsqrt,which=c(1,2))
#modello 3
plot(fitrec,which=c(1,2))
#modello 4
plot(fitrecsqrt,which=c(1,2))

summary(fitrecsqrt)

#definiamo un nuovo dataframe
dim(d)
nuovidati=data.frame(potenzacv=seq(min(d$potenzacv),max(d$potenzacv),length=200))


#IC per E(Y|X=x0)
A=predict(fitrecsqrt,newdata=nuovidati,interval="confidence")

#intervallo di previsione per Y0=Y|X=x0
B=predict(fitrecsqrt,newdata=nuovidati,interval="prediction")

#plottiamo dati e intervalli
matplot(nuovidati,cbind(A,B[ ,-1]), lty=c(1,3,3,2,2), type="l", 
        col=c("black","red","red","blue","blue"))
points(d$potenzacv,d$accel)


########################################
# Dati sulla turbina eolica
########################################

turbina=read.table("windmill.dat",sep="",header=TRUE)

#diagramma di dispersione
plot(turbina$wind,turbina$dc)

#stimiamo il modello di regressione lineare

fit=lm(dc ~ wind, data = turbina)
summary(fit)

#estraiamo singole informazioni
summary(fit)$r.squared    
summary(fit)$coefficients  

plot(turbina$wind, turbina$dc)
abline(summary(fit)$coefficients[1,1],
       summary(fit)$coefficients[2,1])

####Procediamo con l'analisi dei residui del modello

res.st=rstandard(fit)
plot(fitted(fit),res.st)
abline(h=0)

fit.rec=lm(dc~I(1/wind),data=turbina)

fit.rad=lm(dc~sqrt(wind),data=turbina)

fit.log=lm(dc~log(wind),data=turbina)

fit.rr=lm(dc~I(1/sqrt(wind)),data=turbina)


# Analisi dei residui dei modelli alternativi 

par(mfrow=c(1,2))

#modello1
plot(fitted(fit.rec),rstandard(fit.rec))
abline(h=0)

qqnorm(r.st)
qqline(r.st)

#modello2
plot(fitted(fit.rad),rstandard(fit.rad))
abline(h=0)

qqnorm(r.st)
qqline(r.st)

#modello3
plot(fitted(fit.log),rstandard(fit.log))
abline(h=0)

qqnorm(r.st)
qqline(r.st)

#modello4
plot(fitted(fit.rr),rstandard(fit.rr))
abline(h=0)

qqnorm(r.st)
qqline(r.st)

summary(fit.rr)

#grafico del modello stimato

par(mfrow=c(1,1))
plot(turbina$wind,turbina$dc)
curve(predict(fit.rr,newdata=data.frame(wind=x)),
      add=TRUE,col=2)