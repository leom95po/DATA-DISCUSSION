####------ TALLER ENTREGABLE MODULO 4: MODELOS PREDICTIVOS 2--#####

##-- CARGANDO LIBRERIAS----#####
library(lmtest)
library(forecast)
library(foreign)
library(gmodels)
library(ResourceSelection)
library(ROCR)
library(Epi)
library(ggplot2)
library(readxl)
library(dplyr)
library(readxl)
library(memisc)
library(haven)
library(QuantPsyc)
library(gridExtra)
library(reshape2)
library(plotly)

###-----  CARGAMOS LA DATA germancredit---#####

germancredit <- read.csv("CERTIFICACION_DATA_SCIENTIST/MODULO 4/TALLER_FINAL_ENTREGABLE/germancredit.csv")
attach(germancredit)
names(germancredit)

# delimito la data

credit<-germancredit[c("Default","duration","installment","age","cards")]


###----ajuste de los modelos modelos---####

##  MODELO LOGIT--#

logitModel<-glm(Default~.,family = binomial(link = "logit"),data = credit)
summary(logitModel)


## MODELO PROBIT--##

probitModel<-glm(Default~.,family = binomial(link = "probit"),data = credit)
summary(probitModel)


####------ CONTRASTES HOLSMER LEMESHOW----####

hlogit<-hoslem.test(credit$Default,fitted(logitModel),g=10)
hlogit

# por tanto con modelo logit tiene buena bondad de ajuste

hprobit<-hoslem.test(credit$Default,fitted(probitModel),g=10)
hprobit

# para probit tambien tiene buena bondad de ajuste


####---- MATRIZ DE CONFUSIÓN PARA LOS DOS MODELOS------######

### hallamos la media de los valores ajustados del modelo: ese es el umbral hasta el
# momento 

## primero LOGIT---##

umb<-mean(logitModel$fitted.values)
umb

# para la matriz de clasificacion usamos la parte rapida

ClassLog(logitModel,credit$Default,cut = umb)


##------ evaluando la capacidad predictiva a traves de otros criterios (ROC)---####

predi<- prediction(logitModel$fitted.values,credit$Default)

perf<-performance(predi,"tpr","fpr")
plot(perf)
abline(0,1,col="blue")

## garficando ROC con la libreria EPI

ROC(form = Default~.,plot = "ROC",data = credit)

## hallamos el punto de corte optimo o umbral--####

ROC(form = Default~.,plot = "sp",data = credit)

### ahora para PROBIT--##

umbPro<-mean(probitModel$fitted.values)
umbPro

ClassLog(probitModel,credit$Default,cut = umbPro)

##--- evaluamos la capacidad predictiva curva ROC #####

prediccion<-prediction(probitModel$fitted.values,credit$Default)

perfor<-performance(prediccion,"tpr","fpr")
plot(perfor)
abline(0,1,col="blue")


####----- curvas de precisión----#####

perfo1<-performance(prediccion,measure = "prec",x.measure = "rec")
plot(perfo1) ## la curva debe caer lentamente

###--- hallando el punto de corte optimo---####

perfo2<-performance(prediccion,"sens","spec")
sensi<-slot(perfo2,"y.values"[[1]])
espe<-slot(perfo2,"x.values"[[1]])
alf<-slot(perfo2,"alpha.values"[[1]])


matri<-data.frame(alf,sensi,espe)

names(matri)[1]<-"alf"
names(matri)[2]<-"sensi"
names(matri)[3]<-"espe"


me<-melt(matri,id=c("alf"))

### punto de corte optimo para probit--###

punto_probit<-ggplot(me,aes(alf,value,group=variable,colour=variable))+
  geom_line(size=1.2)+
  labs(title = "Punto de corte óptimo para PROBIT")
punto_probit


ggplotly(punto_probit)

# matriz de clasificacion con el umbral corrrecto 
#solo para probit porque para el logit es casi lo mismo

ClassLog(probitModel,credit$Default,cut = 0.29115213)

#####----comparamos los dos modelos---######

memisc::mtable(logitModel,probitModel,digits = 6,sdigits = 3,
               summary.stats = c("Nagelkerke R-sq.","Deviance","AIC","BIC","N"))

### cambiando el umbral arbitrariamente---##

umbr<-0.45

ClassLog(logitModel,credit$Default,cut=umbr)

ClassLog(probitModel,credit$Default,cut = umbr)

# con un umbral de 0.45 se mejora el porcentaje de clasificacion, pero siguen siendo 
# modelos malos


###--- evaluando la capacidad predictiva--####

pred_logi<-prediction(as.numeric(logitModel$fitted.values),as.numeric(credit$Default))
pred_probit<-prediction(as.numeric(probitModel$fitted.values),as.numeric(credit$Default))

perflo<-performance(pred_logi,"tpr","fpr")
perfpro<-performance(pred_probit,"tpr","fpr")

### curvas ROC para logit-----####

plot(perflo)
abline(0,1,col="blue")


plot(perfpro)
abline(0,1,col="blue")


###----Area bajo a curva logit y probit
auclo<-performance(pred_logi,measure = "auc")
auclo<-auclo@y.values[[1]]
auclo

aucpr<-performance(pred_probit,measure = "auc")
aucpr<-aucpr@y.values[[1]]
aucpr

##--- PRONOSTICANDO--#####

# uso el logit porque tiene mayor area bajo la curva pero no significa que este sea el mejor por 
# lo dicho anteriormente

predic<-data.frame(duration=10,installment=2,age=24,cards=3)
predict(logitModel,newdata = predic,type = "response")
predict(probitModel,newdata = predic,type = "response")


predic2<-data.frame(duration=6,installment=24,age=43,cards=2)
predict(logitModel,newdata = predic2,type = "response")

## sin embargo ninguno de los dos modelos es bueno


####----- CONSTRUIMOS OTROS MODELOS SIN LA VAR SIGNIFICATIVA: CARDS-----######


##  MODELO LOGIT--#

logitModelo<-glm(Default~duration+installment+age,family = binomial(link = "logit"),data = credit)
summary(logitModelo)


## MODELO PROBIT--##

probitModelo<-glm(Default~duration+installment+age,family = binomial(link = "probit"),data = credit)
summary(probitModelo)


####------ CONTRASTES HOLSMER LEMESHOW PARA MODELOS NUEVOS----####

h.logit<-hoslem.test(credit$Default,fitted(logitModelo),g=10)
h.logit

# por tanto con modelo logit tiene buena bondad de ajuste

h.probit<-hoslem.test(credit$Default,fitted(probitModelo),g=10)
h.probit

# para probit tambien tiene buena bondad de ajuste



####---- MATRIZ DE CONFUSIÓN PARA LOS DOS MODELOS NUEVOS------######

### hallamos la media de los valores ajustados del modelo: ese es el umbral hasta el
# momento 

## primero LOGIT---##

umb.l<-mean(logitModelo$fitted.values)
umb.l

# para la matriz de clasificacion usamos la parte rapida

ClassLog(logitModelo,credit$Default,cut = umb.l)


##------ evaluando la capacidad predictiva a traves de otros criterios (ROC)---####

predi.l<- prediction(logitModelo$fitted.values,credit$Default)

perf.l<-performance(predi.l,"tpr","fpr")
plot(perf)
abline(0,1,col="blue")

## garficando ROC con la libreria EPI

ROC(form = Default~duration+amount+installment+age,plot = "ROC",data = credit)

## hallamos le punto de corte optimo o umbral--####

ROC(form = Default~duration+amount+installment+age,plot = "sp",data = credit)



### ahora para PROBIT DEL NUEVO MODELO--##

umbProb<-mean(probitModelo$fitted.values)
umbProb

ClassLog(probitModelo,credit$Default,cut = umbProb)

##--- evaluamos la capacidad predictiva curva ROC #####

prediccion_pro<-prediction(probitModelo$fitted.values,credit$Default)

perfor.p<-performance(prediccion_pro,"tpr","fpr")
plot(perfor.p)
abline(0,1,col="blue")


####----- curvas de precisión----#####

perfo.p<-performance(prediccion_pro,measure = "prec",x.measure = "rec")
plot(perfo.p) ## la curva debe caer lentamente

###--- hallando el punto de corte optimo---####

perfo2.p<-performance(prediccion_pro,"sens","spec")
sensi.p<-slot(perfo2.p,"y.values"[[1]])
espe.p<-slot(perfo2.p,"x.values"[[1]])
alf.p<-slot(perfo2.p,"alpha.values"[[1]])


matri.p<-data.frame(alf.p,sensi.p,espe.p)

names(matri.p)[1]<-"alf"
names(matri.p)[2]<-"sensi"
names(matri.p)[3]<-"espe"


me.p<-melt(matri.p,id=c("alf"))

### punto de corte optimo para probit--###

punto_probi<-ggplot(me.p,aes(alf,value,group=variable,colour=variable))+
  geom_line(size=1.2)+
  labs(title = "Punto de corte óptimo para PROBIT")
punto_probi


ggplotly(punto_probi)




#####----comparamos los dos modelos---######

memisc::mtable(logitModelo,probitModelo,digits = 6,sdigits = 3,
               summary.stats = c("Nagelkerke R-sq.","Deviance","AIC","BIC","N"))


### cambiando el umbral arbitrariamente---##

umbra<-0.45

ClassLog(logitModelo,credit$Default,cut=umbra)

ClassLog(probitModelo,credit$Default,cut = umbra)

# con un umbral de 0.45 se mejora el porcentaje de clasificacion, pero siguen siendo 
# modelos malos


## FIN































