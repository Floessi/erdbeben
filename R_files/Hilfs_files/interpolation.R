# Curve-fitting-Toolbox von Matlab (8k) + matlab(5+k)
int_fun_matlab <- function(x) {
  8.971e-08 * exp(2.581 * x)
}

test_model <- lm(triggerCountTh ~ I(8.971e-08 * exp(2.581 * mag)) - 1, full_data)
summary(test_model)
test_model2 <- lm(avg_afters ~ I(8.971e-08 * exp(2.581 * mag)) - 1, interpolation_data)
summary(test_model2)



library(ggplot2)
library(MASS)
library(mgcv)
library(dplyr)
library(gridExtra)
load("Daten/data_full_00001.Rda")



AnteileTriggerBeben <- data.frame("keinNachbeben" = c(table(full_data$magType, full_data$willTrigger)[, 1],
                                                      sum(table(full_data$magType, full_data$willTrigger)[, 1])),
                                  "TriggerBeben" = c(table(full_data$magType, full_data$willTrigger)[, 2],
                                                     sum(table(full_data$magType, full_data$willTrigger)[, 2])),
                                  "Sum" = c(table(full_data$magType), nrow(full_data)),
                                  row.names = c(levels(full_data$magType), "Sum"))
AnteileTriggerBeben$relativerAnteil_Triggerbeben <- round(AnteileTriggerBeben$TriggerBeben/AnteileTriggerBeben$Sum,
                                                          digits = 2)
AnteileTriggerBeben



ggplot(full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  xlab("Magnitude") + ylab("Anzahl Nachbeben")
ggplot(full_data, aes(x = mag, y = triggerCountTh)) + geom_point() + xlim(c(4, 8)) +
  ylim(c(0, 80)) + xlab("Magnitude") + ylab("Anzahl Nachbeben")


# Aggregiere die Daten mit durchschnittlichen Nachbeben bezueglich der Magnitude
interpolation_data <- aggregate(triggerCountTh ~ mag, data = full_data, FUN = mean)
colnames(interpolation_data) <- c("mag", "avg_afters")
interpolation_data

interp_data <- interpolation_data
# Geplottet
ggplot(interp_data, aes(x = mag, y = avg_afters, color = weigth)) + geom_point() +
  scale_color_gradient(low = "yellow", high = "red") + xlab("Magnitude") + ylab("Durchschnittliche Nachbeben")
ggplot(interp_data, aes(x = mag, y = avg_afters)) + geom_point() +
  xlab("Magnitude") + ylab("Durchschnittliche Nachbeben")



test<- seq(4, 8.7, 0.01)
a <- data.frame("mag"=test, "avg"=int_fun_matlab(test))
b<-merge(a, interp_data, all.x = TRUE)

ggplot(b, aes(x = mag, y = avg)) + geom_line() + geom_point(aes(mag, avg_afters))





















interp_data$int_avg_afers <- int_fun(interp_data$mag)
ggplot(interp_data, aes(x = mag, y = avg_afters)) + geom_point() +
  geom_line(aes(mag, int_avg_afers), col = "red")


plot(interp_data$avg_afters~interp_data$mag)
lines(predict(temp), col='red', lwd=2)
temp<-loess(interp_data$avg_afters~interp_data$mag)

interp_data[,c(1,2)]
write.csv(interp_data[,c(1,2)], file = "Daten/jonas.csv")

temp<-lm(avg_afters~mag + I(mag^2)+ I(mag^3)+ I(mag^4)+ I(mag^5)+ I(mag^6)+ I(mag^7), interp_data)
plot(seq(4,9,00.1),predict(temp, newdata = data.frame("mag"=seq(4,9,00.1))))

testdata <- data.frame("mag" = seq(4,9,00.1), "pred_after" = predict(temp, newdata = data.frame("mag"=seq(4,9,00.1))))

ggplot(testdata, aes(mag, pred_after))+ geom_point()+ xlim(c(4,7)) + ylim(-1,1)


# Curve-fitting-Toolbox von Matlab (8k) + matlab(5+k)
int_fun_matlab <- function(x) {
  8.971e-08 * exp(2.581 * x)
}


plot(int_fun_matlab(test)~test)
interp_data$int_avg_afers <- int_fun_matlab(interp_data$mag)
ggplot(interp_data, aes(x = mag, y = avg_afters)) + geom_point() +
  geom_line(aes(mag, int_avg_afers), col = "red")

a<-data.frame("mag"=test, "avg"=int_fun_matlab(test))
ggplot(a, aes(x = mag, y = avg)) + geom_point() + geom_hline(yintercept = 512, col = "red")+ geom_vline(xintercept = 8.7, col = "red")
