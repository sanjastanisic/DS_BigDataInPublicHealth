library(tidyr)
library(dplyr)
library(data.table)

library(ggplot2)

library(epitools)
library(Epi)
library(epiR)

library(survival)
library(survminer)


## 1.  Statistiche descrittive e preprocessamento dati

### 1.1 Dataset "GermanH.csv"
  
ghr <- read.csv2 ("GermanH.csv")

ghrF <- ghr

ghrF$smoke <- as.factor(ghrF$smoke)
ghrF$sex <- as.factor(ghrF$sex)
ghrF$married <- as.factor(ghrF$married)
ghrF$kids <- as.factor(ghrF$kids)
ghrF$work <- factor(ghr$work, levels = c("yes", "no"))
ghrF$education <- factor(ghr$education, levels = c("medium/high", "low"))


#tibble(ghrF)
summary(ghrF)


ghr <- na.omit(ghr) 
#tibble(ghr)
#summary(ghr)

ghrF <-na.omit(ghrF)
#tibble(ghrF)
summary(ghrF)

#Verifica duplicati
dups<- ghrF$idnum[which(duplicated(ghrF$idnum))]
dups
length(dups)

### 1.2 Data set "Cancerregister.csv"

cr <- read.csv2 ("Cancerregister.csv")
cr$incidenza <- as.Date(cr$incidenza, format = "%d/%m/%Y")

crF <- cr

crF$Stadio <- as.factor(crF$Stadio)
crF$tipotumore <- as.factor(crF$tipotumore)
crF$geneticm <- as.factor(crF$geneticm)


#tibble(crF)
summary(crF)

cr <- na.omit(cr)
cr <- cr[!(trimws(cr$Stadio)==""| trimws(cr$tipotumore)==""), ]

crF <- na.omit(crF)
crF <- crF[!(trimws(crF$Stadio)==""| trimws(crF$tipotumore)==""), ]
summary(crF)
#tibble(crF)

dups<- crF$idnum[which(duplicated(crF$idnum))]
dups
length(dups)

cr<- distinct(cr)

crF<- distinct(crF)
#tibble(crF)
summary(crF)

### 1.3 Dataset "SDO.csv"

sdo <- read.csv2 ("SDO.csv")
sdo$dataprestazione <- as.Date(sdo$dataprestazione, format = "%d/%m/%Y")
sdo$dimissione <- as.Date(sdo$dimissione, format = "%d/%m/%Y")

sdoF <- sdo

sdoF$Prestazione <- as.factor(sdoF$Prestazione)

#tibble(sdoF)
summary(sdoF)

sdo <- na.omit(sdo)
sdo <- distinct(sdo) 

sdoF <- na.omit(sdoF)
sdoF <- distinct(sdoF)

summary(sdoF)

sdof2 <- sdoF%>% group_by(ospedale) %>% summarise(n())
sdof2

#Record con date incongruenti
toRemove <- sdoF[(sdoF$dataprestazione > sdoF$dimissione ), ]
toRemove
#tibble(toRemove)

sdoF <- sdoF[!(sdoF$dataprestazione > sdoF$dimissione ), ]
#tibble(sdoF)

### 1.4 Dataset "Deathregister.csv"

dreg <- read.csv2 ("Deathregister.csv")
dreg$enddate <- as.Date(dreg$enddate, format = "%Y-%m-%d")

dregF <- dreg
dregF$dead <- as.factor(dregF$dead)

#tibble(dregF)
summary(dregF)

dups<- dregF$idnum[which(duplicated(dregF$idnum))]
dups
length(dups)


## 2. Indicatore ‘Intervento chirurgico di asportazione del tumore al 
## seno entro 60 giorni dalla data di diagnosi’


join2 <- inner_join(crF, sdoF, by = "idnum")
nrow(join2)

join2 <- join2 [!(join2$dimissione < join2$incidenza | join2$dataprestazione < join2$incidenza),]
nrow(join2)

join2 <- inner_join(join2, ghrF, by = "idnum") %>%
filter(incidenza >=  as.Date("01/01/1984", "%d/%m/%Y") & incidenza <=  as.Date("31/01/1984", "%d/%m/%Y")) %>%     
filter(sex == "Female") %>%
filter (tipotumore == "seno") %>%
filter(Stadio == "Stadio I" | Stadio == "Stadio II" )%>%
filter (Prestazione == "chirurgica") 

summary(join2)
#tibble(join2)

denominatore <- nrow(join2)
denominatore

join2$tempoIntervento <- join2$dataprestazione - join2$incidenza 

numeratore <- nrow( join2 %>% filter(tempoIntervento <= 60)) 
numeratore

indicatoreTot = numeratore/denominatore
indicatoreTot

## 3. Indicatore per ospedale 

join3 <- inner_join(crF, sdoF, by = "idnum")

join3$tempoIntervento <- join3$dataprestazione - join3$incidenza 

join3 <- inner_join(join3, ghrF, by = "idnum") %>%
  filter(incidenza >=  as.Date("01/01/1984", "%d/%m/%Y") & incidenza <=  as.Date("31/01/1984", "%d/%m/%Y")) %>%     
  filter(sex == "Female") %>%
  filter (tipotumore == "seno") %>%
  filter(Stadio == "Stadio I" | Stadio == "Stadio II" )%>%
  filter (Prestazione == "chirurgica") 

join3$status <- ifelse(join3$tempoIntervento <= 60, 1, 0) # 1 intervento, 0 non intervento

summary(join3)
#tibble(join3)

TTS <- table(join3$status,join3$ospedale)
TTS

prop.table(TTS,2)
TTS

prop.test(TTS[2,1],TTS[1,1]+TTS[2,1])

lower <- c()
upper <- c()
for (i  in (1:(length(TTS)/2))) {
  ci <- prop.test(TTS[2, i], TTS[1, i]+TTS[2,i])$conf.int
  lower <- append(lower, ci[1])
  upper <- append(upper, ci[2])
}

interventi_perc <- prop.table(table(join3$status,join3$ospedale), 2)[2,]
ospedale <- names(interventi_perc)
num<-as.numeric(table(join3$ospedale))

df <- data.frame(ospedale, num,interventi_perc, lower, upper)

dt <- as.data.table(df)
dt %>% ggplot +
  geom_point( aes(x=ospedale, y=interventi_perc,size=num), stat="identity", alpha=0.7) +  
  ylab("intervento a meno di 60 gg")+
  geom_errorbar( aes(x=ospedale, ymin=lower, ymax=upper), width=0.4, colour="black", 
                 alpha=0.9, size=0.9) + 
  geom_hline(yintercept=indicatoreTot)


## 4. Associazione tra il livello di educazione e indicatore

join4 <- join3
join4$statusF <- factor(join4$status, levels = c(1,0))

contTable <- xtabs( ~ join4$education + join4$statusF  )
contTable

epi.2by2(dat=contTable, method="case.control")


## 5. OR aggiustato per la variabile 'work'

join5 <- join3

join5$statusF <- factor(join5$status, levels = c(1,0))

join5G <- join5 %>%
  group_by(statusF, education, work) %>%
  summarise(count=n())

tibble(join5G)

tab_join5G <- xtabs(join5G$count ~ join5G$education + join5G$work + join5G$statusF)
tab_join5G

join5G_yes <- join5G[join5G$work== "yes",]
join5G_yes
join5G_no <- join5G[join5G$work=="no",]
join5G_no

epi.2by2(xtabs(join5G_yes$count ~ join5G_yes$education + join5G_yes$statusF), method="case.control")

epi.2by2(xtabs(join5G_no$count ~ join5G_no$education + join5G_no$statusF), method="case.control")

epi.2by2(tab_join5G, method = "case.control")

## 6. Stima dell'associazione tra il livello di educazione e 
## il valore dell'indicatore, aggiustata per confondenti tramite regressione logistica


# Associazione a livello individuale tra il livello di educazione 
# e il valore dell'indicatore
join6 <- join3 
nrow(join6)

join6$statusF <- factor(join6$status, levels = c(0,1))
join6$education <- relevel(join6$education, ref = "low") 
join6$work <- relevel(join6$work, ref = "no")


model6_overall <- glm((statusF ~ education),data = join6, family = "binomial")
model6_overall

summary(model6_overall)

exp(cbind("OR" = coef(model6_overall), confint.default(model6_overall, level = 0.95)))

## Variabili associate all'indicatore 
                                               
# Si riporta soltanto il codice di verifica dell'associazione con lo stadio
# dell'unica variabile risultata associata all'indicatore.
# Tutte le altre variabili disponibili:'ospedale', 'geneticm', 'smoke', 
#  'married', 'kids', 'work', 'age' non risultano associate
# al valore dell'indicatore.
                                               
join6$Stadio <- relevel (join6$Stadio, ref = "Stadio I")
                                               
model_st <- glm((statusF ~ Stadio),data = join6, family = "binomial")
summary(model_st)
exp(cbind("OR" = coef(model_st), confint.default(model_st, level = 0.95)))
                                               
                                               
## 7. Tumore al colon 
                                               
#Selezione dati
join7 <- inner_join(crF, dregF, by = "idnum")
nrow(join7)
                                               
join7 <- inner_join(join7, ghrF, by = "idnum") %>%
filter(incidenza >=  as.Date("01/01/1984", "%d/%m/%Y") & incidenza <=  as.Date("31/01/1984", "%d/%m/%Y")) %>%     
filter(enddate <=  as.Date("31/12/1988", "%d/%m/%Y")) %>%     
filter (tipotumore == "colon")
                                               
# Stima della sopravvivenza con lo stimatore di Kapan Meier
join7$timeFUP <- as.numeric(( join7$enddate - join7$incidenza)/365.25)
join7$dead <- as.numeric(join7$dead)
                                               
fit7<-survfit(Surv(timeFUP, dead) ~1,data=join7)
summary(fit7)
                                               
names(fit7)

#Curva di sopravvivenza con numero di soggetti a rischio
ggsurvplot(fit7, data = join7, risk.table = TRUE, xlab = c("Time in years"), 
surv.median.line = "hv", ggtheme=theme_minimal(), 
title = 'Stima Kaplan Meier della sopravvivenza')
                                               
#Curva di sopravvivenza con numero di soggetti a rischio e numero eventi cumulato
ggsurvplot(fit7, data = join7, risk.table = TRUE, fontsize = 2.5,cumevents = TRUE, 
xlab = c("Time in years"), surv.median.line = "hv", 
ggtheme=theme_minimal(), title = 'Stima Kaplan Meier della sopravvivenza') 
                                             

## 8. Stima della sopravvivenza per stadio
### 8.1 Stima della funzione di sopravvivenza per stadio di malattia
join8 <- join7
# KM stima della sopravvivenza
fit8<- survfit(Surv(timeFUP, dead) ~ Stadio ,data=join8)
summary(fit8)
                                               
#Curva di sopravvivenza stratificata per stadio di malattia
ggsurvplot(fit8, data = join8, conf.int = TRUE, xlab = c("Time in years"), 
surv.median.line = "hv", ggtheme=theme_minimal(),
title = 'Stima della sopravvivenza per stadio di malattia')
                                               
#Curva di sopravvivenza con numero di soggetti a rischio 
ggsurvplot(fit8, data = join8, risk.table = TRUE,  risk.table.height = 0.3,
fontsize =2.5, xlab = c("Time in years"), 
surv.median.line = "hv", ggtheme=theme_minimal(), 
title = 'Stima della sopravvivenza per stadio di malattia') 
                                              
 # Verifica la differenza di azzardo per stadio di malattia

# Stadio I vs. Stadio II, Stadio III, Stadio IV
                                               
join8$stadio1 <- ifelse(join8$Stadio == "Stadio I", 1, 0)
join8$stadio2 <- ifelse(join8$Stadio == "Stadio II", 1, 0)
join8$stadio3 <- ifelse(join8$Stadio == "Stadio III", 1, 0)
join8$stadio4 <- ifelse(join8$Stadio == "Stadio IV", 1, 0)
                                               
#Modello di Cox
model8 <- coxph(Surv(timeFUP, dead) ~ stadio2 + stadio3 + stadio4, data = join8)
summary(model8)
                                               
# Verifica dell'assunzione della proporzionalità di azzardi tramite Schoenfeld test
#Stadio II
par(mfrow=c(1,2),mar=c(4,4,2,2))
checkPH.stadio2 <-cox.zph(model8)[1]
plot(checkPH.stadio2,main="Check PH assumption of Stadio II")
points(checkPH.stadio2$x,checkPH.stadio2$y,pch=16,col="lightgray")
abline(h=0,lty=2,col=2)
                                               
#Stadio III
par(mfrow=c(1,2),mar=c(4,4,2,2))
checkPH.stadio3 <-cox.zph(model8)[2]
plot(checkPH.stadio3,main="Check PH assumption of Stadio III")
points(checkPH.stadio3$x,checkPH.stadio3$y,pch=16,col="lightgray")
abline(h=0,lty=2,col=2)
                                               
#Stadio IV
par(mfrow=c(1,2),mar=c(4,4,2,2))
checkPH.stadio4 <-cox.zph(model8)[3]
plot(checkPH.stadio3,main="Check PH assumption of Stadio IV")
points(checkPH.stadio3$x,checkPH.stadio3$y,pch=16,col="lightgray")
abline(h=0,lty=2,col=2)

## 9. Stima dell'associazione tra sesso e mortalità
                                               

join9 <- inner_join(cr, dreg, by = "idnum")
join9 <- inner_join(join9, ghr, by = "idnum") %>%
filter(incidenza >=  as.Date("01/01/1984", "%d/%m/%Y") & incidenza <=  as.Date("31/01/1984", "%d/%m/%Y")) %>%     
filter(enddate <=  as.Date("31/12/1988", "%d/%m/%Y")) %>%     
filter (tipotumore == "colon")
                                               
nrow(join9)
#summary (join9)
                                               
join9$dead <- as.factor(join9$dead)
join9$sex <- factor(join9$sex, levels = c("Female","Male"))
                                               
join9 %>% group_by(dead) %>%
summarise(count = n())
                                               
#tibble(join9)
                                               
model9_overall <- glm((dead ~ sex),data = join9, family = "binomial")
model9_overall
                                               
summary(model9_overall)
                                               
exp(cbind("OR" = coef(model9_overall), confint.default(model9_overall, level = 0.95)))
                                               
#str(join9)
 
## 10. Associazione alla mortalità
join10 <- join9
                                               
# Associazione mortalità e stadio della malattia
join10$Stadio <- as.factor(join10$Stadio)
                                               
model_10_st <- glm((dead ~ Stadio),data = join10, family = "binomial")
model_10_st
                                               
summary(model_10_st)
                                               
exp(cbind("OR" = coef(model_10_st), confint.default(model_10_st, level = 0.95)))
                                               
# Associazione mortalità e fattore genetico
join10$geneticm <- as.factor(join10$geneticm)
                                               
model_10_g <- glm((dead ~ geneticm),data = join10, family = "binomial")
model_10_g
                                               
summary(model_10_g)
                                               
exp(cbind("OR" = coef(model_10_g), confint.default(model_10_g, level = 0.95)))
                                               
                                               
#Associazione mortalità e fumo
join10$smoke <- as.factor(join10$smoke)
                                               
model_10_sm <- glm((dead ~ smoke),data = join10, family = "binomial")
model_10_sm
                                               
summary(model_10_sm)
                                               
exp(cbind("OR" = coef(model_10_sm), confint.default(model_10_sm, level = 0.95)))
                                               
#Associazione mortalità ed educazione
join10$education <- as.factor(join10$education)
                                               
model_10_ed <- glm((dead ~ education),data = join10, family = "binomial")
model_10_ed
                                               
summary(model_10_ed)
                                               
exp(cbind("OR" = coef(model_10_ed), confint.default(model_10_ed, level = 0.95)))
                                               
                                               
# Associazione mortalità e anni compiuti
                                               
model_10_age <- glm((dead ~ age),data = join10, family = "binomial")
model_10_age
                                               
summary(model_10_age)
                                               
exp(cbind("OR" = coef(model_10_age), confint.default(model_10_age, level = 0.95)))

## 11. Valutazione di presenza di confondenti e/o modificatori d'effetto
                                               
### 11.1 Valutazione confondenti
join11 <- join9
                                               
join11$dead <- as.factor(join11$dead)
join11$Stadio <- as.factor(join11$Stadio)
join11$geneticm <- as.factor(join11$geneticm)
join11$smoke <- as.factor(join11$smoke)
join11$education <- as.factor(join11$education)
                                               
#tibble(join11)
                                               
# Aggiustamento per la variabile Stadio
model11_adj_st <- glm((dead ~ sex + Stadio),data = join11, family = "binomial")
model11_adj_st
                                               
summary(model11_adj_st)
                                               
exp(cbind("OR" = coef(model11_adj_st), confint.default(model11_adj_st, level = 0.95)))
                                               
model11_int_st <- glm((dead ~ sex * Stadio),data = join11, family = "binomial")
model11_int_st
                                               
summary(model11_int_st)
                                               
# Aggiustamento per la variabile geneticm
model11_adj_g <- glm((dead ~ sex + geneticm),data = join11, family = "binomial")
model11_adj_g
                                               
summary(model11_adj_g)
                                               
exp(cbind("OR" = coef(model11_adj_g), confint.default(model11_adj_g, level = 0.95)))
                                               
model11_adj_g <- glm((dead ~ sex * geneticm),data = join11, family = "binomial")
model11_adj_g
                                               
summary(model11_adj_g)
                                               
# Aggiustamento per la variabile smoke
model11_adj_sm <- glm((dead ~ sex + smoke),data = join11, family = "binomial")
model11_adj_sm
                                               
summary(model11_adj_sm)
                                               
exp(cbind("OR" = coef(model11_adj_sm), confint.default(model11_adj_sm, level = 0.95)))
                                               
model11_adj_sm <- glm((dead ~ sex * smoke),data = join11, family = "binomial")
model11_adj_sm
                                               
summary(model11_adj_sm)
                                               
# Aggiustamento per la variabile education
model11_adj_ed <- glm((dead ~ sex + education),data = join11, family = "binomial")
model11_adj_ed
                                               
summary(model11_adj_ed)
                                               
exp(cbind("OR" = coef(model11_adj_ed), confint.default(model11_adj_ed, level = 0.95)))
                                               
model11_adj_ed <- glm((dead ~ sex * education),data = join11, family = "binomial")
model11_adj_ed
                                               
summary(model11_adj_ed)
                                               
# Aggiustamento per la variabile age
model11_adj_age <- glm((dead ~ sex + age),data = join11, family = "binomial")
model11_adj_age
                                               
summary(model11_adj_age)
                                               
exp(cbind("OR" = coef(model11_adj_age), confint.default(model11_adj_age, level = 0.95)))
                                               
model11_adj_age <- glm((dead ~ sex * age),data = join11, family = "binomial")
model11_adj_age
                                               
summary(model11_adj_age)
                                               
### 11.2 Valutazione interazione tra le variabili sesso ed educazione 

#tibble(join11)
#summary(join11)
                                                                                                                                                    
join11$dead <- relevel(join11$dead, ref = "1")
join11$sex <- relevel(join11$sex, ref = "Male")
join11$education <- relevel(join11$education, "medium/high")
                                                                                                                                                    
                                                                                                                                                    
join11G <- join11 %>%
group_by(dead, sex, education) %>%
summarise(count=n())
                                                                                                                                                    
tab_join11G <- xtabs(join11G$count ~ join11G$sex + join11G$education + join11G$dead)
tab_join11G
                                                                                                                                                    
join11G_Male <- join11G[join11G$sex == "Male",]
join11G_Male
join11G_Female <- join11G[join11G$sex == "Female",]
join11G_Female
                                                                                                                                                    
epi.2by2(xtabs(join11G_Male$count ~ join11G_Male$education + join11G_Male$dead), method="case.control")
                                                                                                                                                    
epi.2by2(xtabs(join11G_Female$count ~ join11G_Female$education + join11G_Female$dead), method="case.control")
                                                                                                                                                    

## 12. Modello finale
                                               

#tibble(join11)
#summary(join11)
                                               
join12 <- join9
                                               
xjoin12$dead <- as.factor(join12$dead)
join12$sex <- factor(join12$sex, levels = c("Female","Male"))
xjoin12Stadio <- as.factor(join12$Stadio)
join12$geneticm <- as.factor(join11$geneticm)
join12$smoke <- as.factor(join11$smoke)
join12$education <- as.factor(join11$education)
                                               
                                               
model_final <- glm(dead ~ sex + Stadio + geneticm + age + 
smoke + education, data = join12, family = "binomial")
#model_final
                                               
summary(model_final)
                                               
anova(model_final, test="Chisq")
                                               
                                               