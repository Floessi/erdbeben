###################################################################################################
# MODELLSUCHE #####################################################################################
###################################################################################################

# Ausgangszustand:  Daten aufbereitet, aber keine Zusammenhänge mit der Zielgrosse (Anzahl direkter
#                   Nachbeben) bekannt ausser ein linearer mit der Magnitude
# Ziel:             Modellfindung, dass die Zielvariable halbwegs gut erklaert


# Daten (Threshold fuer das Distanmass = 0.00001 (Standardwert in der Literatur)):
load("Daten/data_full_no_lists_00001.Rda")
# Nowendige Packages
library(mboost)
library(ggplot2)


###################################################################################################
# ZIELVARIABLE UNTERSUCHEN ########################################################################
###################################################################################################
# Haeufigkeitstabelle
table(full_data$triggerCountTh)
full_data[max(full_data$triggerCountTh) == full_data$triggerCountTh,]
# Hauptbeben vom Tōhoku-Erdbeben (2011, Fukushima) hat mit Abstand am meisten Nachbeben
# Teile die Daten auf
biggest_quake <- full_data[6247, ]
data_rest <- full_data[-6247, ]
data_no_willnottrigger <- full_data[full_data$triggerCountTh > 0, ]

# Nur mit den Restdaten ohne das Tohoku-Beben

hist(x = data_rest$triggerCountTh, main = "Histogramm der Variable triggerCountTh",
     breaks = seq(-0.5, 600, 1), xlim = c(0, 20))
hist(x = data_no_willnottrigger$triggerCountTh, main = "Histogramm der Variable triggerCountTh",
     breaks = seq(-0.5, 600, 1), xlim = c(0, 20))
plot(x = density(data_rest$triggerCountTh), main = "Dichte der Variable triggerCountTh")
boxplot(x = data_rest$triggerCountTh, main = "Boxplot der Variable triggerCountTh")



###################################################################################################
# Ist diese Ueberdurchschnittlichkeit des Ausreissers in den Einflssgroessen erkennbar ?

# Schaue dir die einzelnen Einfluessgroessen an
# Koordinaten
ggplot(data = data_rest, aes(x = lon, y = lat)) + geom_point() +
  geom_point(data = biggest_quake, col = "red")

#Magnitude
ggplot(data = data_rest, aes(x = mag)) + geom_histogram(bins = 100) +
  geom_vline(xintercept = biggest_quake$mag, col = "red")

# Tiefe
ggplot(data = data_rest, aes(x = depth)) + geom_histogram(bins = 100) +
  geom_vline(xintercept = biggest_quake$depth, col = "red")
# (Beben an der Erdoberflaeche?)

# Waermetransport an die Oberflaeche
ggplot(data = data_rest, aes(x = heatFlow)) + geom_histogram(bins = 100) +
  geom_vline(xintercept = biggest_quake$heatFlow, col = "red")

# Krustendicke
ggplot(data = data_rest, aes(x = crustalThick)) + geom_histogram(bins = 100) +
  geom_vline(xintercept = biggest_quake$crustalThick, col = "red")

# Manteldicke
ggplot(data = data_rest, aes(x = mantleThick)) + geom_histogram(bins = 100) +
  geom_vline(xintercept = biggest_quake$mantleThick, col = "red")

# Orthometrische Höhe
ggplot(data = data_rest, aes(x = elevation)) + geom_histogram(bins = 100) +
  geom_vline(xintercept = biggest_quake$elevation, col = "red")

# Einzige aber dafuer ein deutlicher High-Leverage-Point ist das Beben bezueglich der Magnitude
# 0.8 mag vom naechst-niedrigeren Magnitudenwert entfernt
###################################################################################################


###################################################################################################
# BOOSTING ########################################################################################
###################################################################################################
# Ohne Boosting
model_lm_1 <- lm(triggerCountTh ~ mag + depth + heatFlow + crustalThick + mantleThick + elevation +
                   strike*dip*rake, data = full_data)
summary(model_lm_1)
anova(model_lm_1)
# Signifikante Einflussgroessen: Magnitude (***), heatFlow(.), mantleThick(*), elevation (.)

# Boosting (Standardparameter)
model_glm_1 <- glmboost(triggerCountTh ~ mag + depth + heatFlow + crustalThick + mantleThick +
                      elevation + strike*dip*rake , data = full_data,
                    control = boost_control(nu =0.1, mstop = 100))
summary(model_glm_1)
AIC(model_glm_1)
cvrisk(model_glm_1)


# Boosting mit optimalen Iteration (nach AIC)
model_glm_2 <- glmboost(triggerCountTh ~ mag + depth + heatFlow + crustalThick + mantleThick +
                          elevation +
                          strike*dip*rake , data = full_data, control = boost_control(nu =0.1, mstop = 131))
summary(model_glm_2)
par(mar = c(5, 5, 4, 6))
plot(x = model_glm_2, main = "Koeffizientenpfade")

# Boosting mit optimalen Iteration (nach CV)
model_glm_3 <- glmboost(triggerCountTh ~ mag + depth + heatFlow + crustalThick + mantleThick +
                          elevation +
                      strike*dip*rake , data = full_data, control = boost_control(nu =0.1, mstop = 9))
par(mar = c(5, 5, 4, 6))
plot(x = model_glm_3, main = "Koeffizientenpfade")
# Im Boosting mit mstop = 10 scheint nur die Magnitude relevant zu sein
# Konvergiert noch nicht




# GLMs ohne Boosting
# Nochmal visualisieren
ggplot(data_rest, aes(x = mag, y = triggerCountTh)) + geom_point()
ggplot(data_rest, aes(x = depth, y = triggerCountTh)) + geom_point()
ggplot(data_rest, aes(x = heatFlow, y = triggerCountTh)) + geom_point()
ggplot(data_rest, aes(x = crustalThick, y = triggerCountTh)) + geom_point()
ggplot(data_rest, aes(x = mantleThick, y = triggerCountTh)) + geom_point()
ggplot(data_rest, aes(x = elevation, y = triggerCountTh)) + geom_point()

# Alle Daten
finaltry1 <- glm(triggerCountTh ~ mag + depth + heatFlow + crustalThick + mantleThick +
                  elevation +
                  strike*dip*rake, family = "quasipoisson", full_data)

finaltry2 <- glm(triggerCountTh ~ mag + I(mag^3) + depth + heatFlow + crustalThick + mantleThick +
                   elevation +
                   strike*dip*rake, family = "quasipoisson", full_data)

# Daten ohne Tohoku-Hauptbeben
finaltry3 <- glm(triggerCountTh ~ mag + depth + heatFlow + crustalThick + mantleThick +
                   elevation +
                   strike*dip*rake, family = "quasipoisson", data_rest)

finaltry4 <- glm(triggerCountTh ~ mag + I(mag^3) + depth + heatFlow + crustalThick + mantleThick +
                   elevation +
                   strike*dip*rake, family = "quasipoisson", data_rest)

summary(finaltry1)
summary(finaltry2)
anova(finaltry1)
anova(finaltry2)

plot(finaltry2)

summary(finaltry3)
summary(finaltry4)
anova(finaltry3)
anova(finaltry4)

plot(finaltry4)

data.frame(finaltry1$coefficients,finaltry3$coefficients)
data.frame(finaltry2$coefficients,finaltry4$coefficients)




a<- full_data
#Poisson Model
model1<-glm(triggerCountTh~ mag,family= poisson, data= full_data)
summary(model1)

#Residuen Analyse des Poisson Models
qchisq(.95, df=13707) #deviance= 8967<13980=x^2(13707)
residp<-resid(model1, type="pearson")
residd<-resid(model1, type="deviance")
library(ggplot2)
p1<-ggplot(model1, aes(sample = residp)) + geom_point(stat = "qq", color = "#7fc97f") +ylab("Pearson residuals")
p2<- ggplot(model1, aes(sample = residd)) + geom_point(stat = "qq", color = "#7fc97f") +ylab("Deviance residuals")
p3<- ggplot(model1, aes(x = predict(model1, type="link"), y =residd))+geom_point(col = "#7fc97f") +ylab("Deviance residuals") + xlab("Linear predictor")
library(gridExtra)
grid.arrange(p1, p2, p3, nrow = 1)

ggplot(model1, aes(x=log(fitted(model1)), y=log((a$triggerCountTh-fitted(model1))^2)))+geom_point(col="#f46d43") +geom_abline(slope=1, intercept=0, col="#a6d96a", size=1) +ylab(expression((y-hat(mu))^2)) + xlab(expression(hat(mu)))

#Negatives Binomialmodel wegen Overdisposition(Varianz ist größer als Mittelwert)
mean(a$triggerCountTh)
var(a$triggerCountTh)

library(MASS)
model2<- glm.nb(triggerCountTh ~ mag ,data = a)
summary(model2)

#Residualanalyse des Negativ Binomialmodels
residp2<-resid(model2, type="pearson")
residd2<-resid(model2, type="deviance")
p4<-ggplot(model2, aes(sample = residp2)) + geom_point(stat = "qq", color = "#7fc97f") +ylab("Pearson residuals")
p5<- ggplot(model2, aes(sample = residd2)) + geom_point(stat = "qq", color = "#7fc97f") +ylab("Deviance residuals")
p6<- ggplot(model2, aes(x = predict(model2, type="link"), y =residd2))+geom_point(col = "#7fc97f") +ylab("Deviance residuals") + xlab("Linear predictor")
grid.arrange(p4, p5, p6, nrow = 1)

#Poisson Model logarithmiert
ggplot(a, aes(y=log(triggerCountTh), x=log(mag))) +geom_point(col="#f46d43")
model3<-glm(formula = triggerCountTh ~ log(mag), family = poisson,data =a)
summary(model3)

#Negative Binomialverteilung logarithmiert
model4<- glm.nb(triggerCountTh ~ log(mag) ,data = a)
summary(model4)

#Wenn man exp(mag) einsetzt kommt ein Model raus, dass besser zu sein scheint, allerdings weiß ich nicht was die
#E Funktion inhaltlich bedeutet

#Hurdle und Zero-Inflated Modelle gehen besser mit viellen Nullen um
library(pscl)
model5<-hurdle(triggerCountTh ~ mag ,data = full_data)
summary(model5)

#Binomiales Zero-Inflated Modell
library(pscl)
library(MASS)
library(boot)
library(ggplot2)

model6 <- zeroinfl(triggerCountTh ~ mag ,data = full_data, dist = "negbin") # mag | cluster
summary(model6)

#Residualanalyse
op <- par(mfrow = c(2,1 ))
plot(fitted(model6), residuals(model6, type="pearson"), main= "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(model6), residuals(model6, type="pearson")))
plot(full_data$mag, residuals(model6, type="pearson"), main= "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
par(op)

summary(model)
#Poisson Zero-Inflated Modell
model7 <- zeroinfl(triggerCountTh ~ mag ,data = full_data)
summary(model7)

#Neg Bin Zero-Inflated Modell logarithmiert
model8 <- zeroinfl(triggerCountTh ~ log(mag) ,data = full_data, dist = "negbin") # mag | cluster
summary(model8)

#Poisson Zero-Inflated Modell logarithmiert
model9<-zeroinfl(triggerCountTh ~ log(mag),data = full_data)
summary(model9)
#Vergleich der  Modelle, neg Binomialverteilung ist besser
c(model1$deviance, model1$aic)
c(model2$deviance, model2$aic)
c(model3$deviance, model3$aic)
c(model4$deviance, model4$aic)

vuong(model6, model7)
vuong(model6, model2)
vuong(model6, model4)
vuong(model6, model8)
vuong(model6, model9)

#Negativ Binomialverteiltes Zeroinflated Model ist am Besten


#Plot wird nicht angezeigt
ggplot(a, aes(x= mag, y= triggerCountTh)) +geom_abline(slope = coef(model6)[[2]], intercept = coef(model6)[[1]])
ggplot(a, aes(x= mag, y= triggerCountTh)) +geom_abline(slope = coef(model6)[[2]], intercept = coef(model6)[[1]])

















#Weitere Modelle, die nicht gehen oder nicht geeignet sind
Zero-truncated negative binomial regression
library(VGAM)
library(foreign)


model10<- vglm(triggerCountTh ~ mag , family = posnegbinomial(), data = a)
model11 <- vglm(triggerCountTh ~ mag,family= pospoisson(), data = a)
summary(model10)


library(truncreg)
model10 <- truncreg(triggerCountTh ~ mag, data = full_data)
summary(model10)




# Vergleich was besser passt, Quasi-Poisson oder Neg.Binomial
model_poi <- glm(triggerCountTh ~ mag + I(mag^3) + depth + heatFlow + crustalThick + mantleThick +
                   elevation + strike*dip*rake, family = "poisson", full_data)
model_poi_rest <- glm(triggerCountTh ~ mag + I(mag^3) + depth + heatFlow + crustalThick + mantleThick +
                   elevation + strike*dip*rake, family = "poisson", data_rest)
model_qua <- glm(triggerCountTh ~ mag + I(mag^3) + depth + heatFlow + crustalThick + mantleThick +
                       elevation + strike*dip*rake, family = "quasipoisson", full_data)
model_negb <- glm.nb(triggerCountTh ~ mag + I(mag^3) + depth + heatFlow + crustalThick + mantleThick +
                       elevation + strike*dip*rake, data = full_data)

res.sq <- residuals(model_poi, type = "response")^2
set1 <- data.frame(res.sq, mu.hat = model_poi$fitted.values)
fit.lin <- lm(formula = res.sq ~ mu.hat, data = set1)
fit.quad <- lm(formula = res.sq ~ mu.hat + I(mu.hat^2), data = set1)
summary(fit.quad)
plot(set1$mu.hat, y = set1$res.sq, xlab = "Predicted count",
     ylab = "Squared Residual")
curve(expr = predict(fit.lin, newdata = data.frame(mu.hat = x), type = "response"),
      col = "blue", add = TRUE, lty = "solid")
curve(expr = predict(fit.quad, newdata = data.frame(mu.hat = x), type = "response"),
      col = "red", add = TRUE, lty = "dashed")
legend("topleft", legend = c("Quadratic","Linear"), col = c("red", "blue"),
       lty = c("solid", "dashed"), bty = "n")

res.sq2 <- residuals(model_poi_rest, type = "response")^2
set2 <- data.frame(res.sq2, mu.hat2 = model_poi_rest$fitted.values)
fit.lin <- lm(formula = res.sq2 ~ mu.hat2, data = set2)
fit.quad <- lm(formula = res.sq2 ~ mu.hat2 + I(mu.hat2^2), data = set2)
summary(fit.quad)
plot(set2$mu.hat2, y = set2$res.sq2, xlab = "Predicted count",
     ylab = "Squared Residual")
curve(expr = predict(fit.lin, newdata = data.frame(mu.hat2 = x), type = "response"),
      col = "blue", add = TRUE, lty = "solid")
curve(expr = predict(fit.quad, newdata = data.frame(mu.hat2 = x), type = "response"),
      col = "red", add = TRUE, lty = "dashed")
legend("topleft", legend = c("Quadratic","Linear"), col = c("red", "blue"),
       lty = c("solid", "dashed"), bty = "n")

# Zielvariable prinzipiell log-transformieren
