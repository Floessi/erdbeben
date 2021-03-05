######################################################################################################
# Dennis - Logit ####################################################
######################################################################################################


library(ggplot2)
library(mgcv)
library(MASS)
library(dplyr)
library(cowplot)

# install.packages("caTools")
library(caTools)

install.packages("devtools")
devtools::install_github("dustinfife/flexplot")
library(flexplot)

install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

load("data_full_00001.Rda")



######################################################################################################
# Welche Variablen und wie sind diese fuer ein Modell relevant? ######################################
######################################################################################################

# Relevant zum jetztigen Stand fuer ein triggerCountTh-Modell:

# mag:          einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen, a*exp(b*mag) waere denkbar
#               a ist im Intercept enthalten und b ist dann die Koeffizient fuer mag
# depth:        kommt drauf an ob das im vollen Modell signifikant wird
# heatFlow:     einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen
# crustalThick: einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen
# mantleThick:  kommt drauf an ob das im vollen Modell signifikant wird
# dip:          einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen
# rake_mod:     mit Spline oder Polynom einfliessen lassen, siehe stacked-Barplot
# strainRate:   einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen


##### Visualisierung ######

######Magnitude

m1 <- ggplot(data = full_data, aes(x = mag, color = willTrigger)) +
  geom_histogram(bins = 50) + xlab("Magnitude") + ylab("Count")

m2 <- ggplot(data = full_data, aes(x = mag, color = willTrigger)) +
  geom_boxplot() + xlab("Magnitude")

m3 <- ggplot(data = full_data, aes(x = mag, y = willTrigger, color = willTrigger)) +
  geom_point() + xlab("Magnitude")
####depth
d1 <- ggplot(data = full_data, aes(x = depth, color = willTrigger)) +
  geom_histogram(bins = 50) + xlab("Tiefe") + ylab("Count")

d2 <- ggplot(data = full_data, aes(x = depth, color = willTrigger)) +
  geom_boxplot() + xlab("Tiefe")

d3 <- ggplot(data = full_data, aes(x = depth, y = willTrigger, color = willTrigger)) +
  geom_point() + xlab("Tiefe") + ylab("Count")
####heatFlow
h1 <- ggplot(data = full_data, aes(x = heatFlow, color = willTrigger)) +
  geom_histogram(bins = 50) + xlab("Heat Flow")

h2 <- ggplot(data = full_data, aes(x = heatFlow, color = willTrigger)) +
  geom_boxplot() + xlab("Heat Flow")

h3 <- ggplot(data = full_data, aes(x = heatFlow, y = willTrigger, color = willTrigger)) +
  geom_point() + xlab("Heat Flow")
###crustal
c1 <- ggplot(data = full_data, aes(x = crustalThick, color = willTrigger)) +
  geom_histogram(bins = 50) + xlab("Krustendicke") + ylab("Count")

c2 <- ggplot(data = full_data, aes(x = crustalThick, color = willTrigger)) +
  geom_boxplot() + xlab("Krustendicke")

c3 <- ggplot(data = full_data, aes(x = crustalThick, y = willTrigger, color = willTrigger)) +
  geom_point() + xlab("Krustendicke")
####mantle
mt1 <- ggplot(data = full_data, aes(x = mantleThick, color = willTrigger)) +
  geom_histogram(bins = 50) + xlab("Manteldicke") + ylab("Count")

mt2 <- ggplot(data = full_data, aes(x = mantleThick, color = willTrigger)) +
  geom_boxplot() + xlab("Manteldicke")

mt3 <- ggplot(data = full_data, aes(x = mantleThick, y = willTrigger, color = willTrigger)) +
  geom_point() + xlab("Manteldicke")
###dip
di1 <- ggplot(data = full_data, aes(x = dip, color = willTrigger)) +
  geom_histogram(bins = 50) + xlab("Dip") + ylab("Count")

di2 <- ggplot(data = full_data, aes(x = dip, color = willTrigger)) +
  geom_boxplot() + xlab("Dip")

di3 <- ggplot(data = full_data, aes(x = dip, y = willTrigger, color = willTrigger)) +
  geom_point() + xlab("Dip")
####rake_mod
r1 <- ggplot(data = full_data, aes(x = rake_mod, color = willTrigger)) +
  geom_histogram(bins = 50) + xlab("Rake (modifiziert)") + ylab("Count")

r2 <- ggplot(data = full_data, aes(x = rake_mod, color = willTrigger)) +
  geom_boxplot() + xlab("Rake (modifiziert)")

r3 <- ggplot(data = full_data, aes(x = rake_mod, y = willTrigger, color = willTrigger)) +
  geom_point() + xlab("Rake (modifiziert)") + ylab("Count")
###strainRate
s1 <- ggplot(data = full_data, aes(x = strainRate, color = willTrigger)) +
  geom_histogram(bins = 50) + xlab("Strain Rate")

s2 <- ggplot(data = full_data, aes(x = strainRate, color = willTrigger)) +
  geom_boxplot() + xlab("Strain Rate")

s3 <- ggplot(data = full_data, aes(x = strainRate, y = willTrigger, color = willTrigger)) +
  geom_point() + xlab("Strain Rate")
############################

plot_grid(m1, m2, m3, d1, d2, d3, h1, h2, h3, ncol = 3)
plot_grid(c1, c2, c3, mt1, mt2, mt3, di1, di2, di3, ncol = 3)
plot_grid(r1, r2, r3, s1, s2, s3, ncol = 3)


a <- flexplot(data = full_data, mag ~ 1)
b <- flexplot(data = full_data, depth ~ 1)
c <- flexplot(data = full_data, heatFlow ~ 1)
d <- flexplot(data = full_data, crustalThick ~ 1)
e <- flexplot(data = full_data, mantleThick ~ 1)
f <- flexplot(data = full_data, dip ~ 1)
g <- flexplot(data = full_data, rake_mod ~ 1)
h <- flexplot(data = full_data, strainRate ~ 1)

plot_grid(a,b,c,d,e,f,g,h, ncol = 3)

#######################
###### Modelle #####
#######################


###GLM
#GLM ohne Interaktion
glmmod1 <- glm(data = full_data, willTrigger ~ mag + depth +
              heatFlow + crustalThick + mantleThick + dip + rake_mod +
              strainRate, family = binomial(link = "probit"))
summary(glmmod1)

#GLM mit allen 2er Interaktionen
glmmod2 <- glm(data = full_data, willTrigger ~ (mag + depth +
              heatFlow + crustalThick + mantleThick + dip + rake_mod +
              strainRate)^2, family = "binomial")
summary(glmmod2)

stepAIC(glmmod2, direction = "both")

#GLM mit Variablen vom StepAIC
glmmod3 <- glm(data = full_data, willTrigger ~ mag + depth +
               heatFlow + crustalThick + mantleThick + dip + rake_mod +
               strainRate + mag*depth + mag*heatFlow + mag*strainRate +
               depth*heatFlow + depth*crustalThick + depth*mantleThick +
               depth*dip + heatFlow*crustalThick + heatFlow*mantleThick +
               heatFlow*rake_mod + heatFlow*strainRate + crustalThick*mantleThick +
               crustalThick*rake_mod + mantleThick*dip + mantleThick*rake_mod +
               mantleThick*strainRate + dip*strainRate + rake_mod*strainRate, family = "binomial")
summary(glmmod3)



AIC(glmmod1, glmmod2, glmmod3)
BIC(glmmod1, glmmod2, glmmod3)
anova(glmmod1, glmmod2, glmmod3)


###GAM
gammod1 <- gam(data = full_data, willTrigger ~ mag + depth +
              heatFlow + crustalThick + mantleThick + dip + s(rake_mod, bs = "ps") +
              strainRate, family = "binomial")
summary(gammod1)

gammod2 <- gam(data = full_data, willTrigger ~ s(mag, bs = "ps") + depth +
              heatFlow + crustalThick + mantleThick + dip + s(rake_mod, bs = "ps") +
              strainRate, family = "binomial")
summary(gammod2)

gammod3 <- gam(data = full_data, willTrigger ~ s(mag, bs = "ps") + depth +
              heatFlow + crustalThick + mantleThick + dip + s(rake_mod, bs = "ps") +
              s(strainRate, bs = "ps"), family = "binomial")
summary(gammod3)

gammod4 <- gam(data = full_data, willTrigger ~ s(mag, bs = "ps") + depth +
                 s(heatFlow, bs = "ps") + crustalThick + mantleThick + dip + s(rake_mod, bs = "ps") +
                 s(strainRate, bs = "ps"), family = "binomial")
summary(gammod4)

gammod5 <- gam(data = full_data, willTrigger ~ s(mag, bs = "ps") + s(depth, bs = "ps") +
                 s(heatFlow, bs = "ps") + s(crustalThick, bs = "ps") + s(mantleThick, bs = "ps") +
                 s(dip, bs = "ps") + s(rake_mod, bs = "ps") +
                 s(strainRate, bs = "ps"), family = "binomial")
summary(gammod5)

AIC(gammod1, gammod2, gammod3, gammod4, gammod5)
BIC(gammod1, gammod2, gammod3, gammod4, gammod5)
anova(gammod1, gammod2, gammod3, gammod4, gammod5)

############################
##### Kreuzvalidierung #####
############################
set.seed(2020)

index <- createDataPartition(full_data$willTrigger, p=0.8, list = FALSE, times = 1)
train <- full_data[index,]
test <- full_data[-index,]

#Umwandlung von willTrigger zu Charakter und dann Faktor weil "FALSE/TRUE" nicht erlaubt sind
full_data$willTrigger <- as.character(full_data$willTrigger)
train$willTrigger <- as.character(train$willTrigger)
test$willTrigger <- as.character(test$willTrigger)

full_data$willTrigger[full_data$willTrigger == "FALSE"] <- "nein"
full_data$willTrigger[full_data$willTrigger == "TRUE"] <- "ja"
test$willTrigger[test$willTrigger == "FALSE"] <- "nein"
test$willTrigger[test$willTrigger == "TRUE"] <- "ja"
train$willTrigger[train$willTrigger == "FALSE"] <- "nein"
train$willTrigger[train$willTrigger == "TRUE"] <- "ja"

full_data$willTrigger <- as.factor(full_data$willTrigger)
train$willTrigger <- as.factor(train$willTrigger)
test$willTrigger <- as.factor(test$willTrigger)

#Kontrollieren von k, etc
ctrlspecs <- trainControl(method = "cv", number = 10, savePredictions = "all",
                           classProbs = TRUE)


#2. mal Seed
set.seed(2020)

####Cohens Kappa
## 1- 0.81 fast perfekt
## 0.80 - 0.61 gut
## 0.60 - 0.41 mittel
## 0.40 - 0.21 okay
## <20 schwach

######glm1
cv_glm1 <- train(willTrigger ~ mag + depth +
               heatFlow + crustalThick + mantleThick + dip + rake_mod +
               strainRate, data = train, method = "glm", family = binomial,
             trControl=ctrlspecs)
cv_glm1
varImp(cv_glm1)

###glm2
cv_glm2 <- train(willTrigger ~ (mag + depth +
                 heatFlow + crustalThick + mantleThick + dip + rake_mod +
                 strainRate)^2, data = train, method = "glm", family = binomial,
                 trControl=ctrlspecs)
cv_glm2
varImp(cv_glm2)

###glm3
cv_glm3 <- train(willTrigger ~ mag + depth +
                 heatFlow + crustalThick + mantleThick + dip + rake_mod +
                 strainRate + mag*depth + mag*heatFlow + mag*strainRate +
                 depth*heatFlow + depth*crustalThick + depth*mantleThick +
                 depth*dip + heatFlow*crustalThick + heatFlow*mantleThick +
                 heatFlow*rake_mod + heatFlow*strainRate + crustalThick*mantleThick +
                 crustalThick*rake_mod + mantleThick*dip + mantleThick*rake_mod +
                 mantleThick*strainRate + dip*strainRate + rake_mod*strainRate, data = train, method = "glm", family = binomial,
                 trControl=ctrlspecs)
cv_glm3
varImp(cv_glm3)



####GAM1
cv_gam1 <- train(willTrigger ~ mag + depth +
                 heatFlow + crustalThick + mantleThick + dip +
                 rake_mod + strainRate,
                 data = train, method = "gam", family = "binomial",
                 trControl=ctrlspecs)

cv_gam1
summary(cv_gam1)

#Anwenden auf Test
res_glm1 <- predict(cv_glm1, newdata = test)
res_glm2 <- predict(cv_glm2, newdata = test)
res_glm3 <- predict(cv_glm3, newdata = test)
res_gam1 <- predict(cv_gam1, newdata = test)

confusionMatrix(data = res_glm1, test$willTrigger)
confusionMatrix(data = res_glm2, test$willTrigger)
confusionMatrix(data = res_glm3, test$willTrigger)
confusionMatrix(data = res_gam1, test$willTrigger)







