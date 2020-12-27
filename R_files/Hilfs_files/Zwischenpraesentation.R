load("Daten/data_full_no_lists_00001.Rda")

library(ggplot2)
library(gridExtra)
library(MASS)
library(pscl)




Daten_haeufig_1 <- as.data.frame(table(full_data[full_data$triggerCountTh <= 10 &
                                                   full_data$triggerCountTh > 0, ]$triggerCountTh))
p1 <- ggplot(data = Daten_haeufig_1, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") + xlab("Anzahl direkter Nachbeben") +
  ylab("Absolute Häufigkeit") + labs(title = "Häufigkeit von Beben mit\nzwischen 1-10 Nachbeben") +
  theme(plot.title = element_text(size = 10))

Daten_haeufig_2 <- as.data.frame(table(full_data[full_data$triggerCountTh <= 10, ]$triggerCountTh))
p2 <- ggplot(data = Daten_haeufig_2, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") + xlab("Anzahl direkter Nachbeben") +
  ylab("Absolute Häufigkeit") + labs(title = "Häufigkeit von Beben mit\nzwischen 0-10 Nachbeben") +
  theme(plot.title = element_text(size = 10))

Daten_haeufig_3 <- as.data.frame(table(full_data$triggerCountTh))
p3 <- ggplot(data = Daten_haeufig_3, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") + xlab("Anzahl direkter Nachbeben") +
  ylab("Absolute Häufigkeit") + labs(title = "Häufigkeit von Beben nach\nder Anzahl ihrer Nachbeben") +
  theme(plot.title = element_text(size = 10))

grid.arrange(p1, p2, p3, ncol = 3)

mean(full_data$triggerCountTh)
var(full_data$triggerCountTh)

mod_quasi_1 <- glm(triggerCountTh ~ mag, family = "quasipoisson", full_data)

mod_quasi_2 <- glm(triggerCountTh ~ mag + depth + heatFlow + crustalThick + mantleThick +
                   elevation + rake*strike, family = "quasipoisson", full_data)

mod_quasi_3 <- glm(triggerCountTh ~ mag + I(mag^3) + depth + heatFlow + crustalThick + mantleThick +
                   elevation + rake*strike, family = "quasipoisson", full_data)

mod_quasi_4 <- glm(triggerCountTh ~ mag + I(mag^3) + I(mag^5) + depth + heatFlow + crustalThick + mantleThick +
                   elevation + rake*strike, family = "quasipoisson", full_data)

anova(mod_quasi_1, mod_quasi_2, mod_quasi_3, mod_quasi_4)


mod_negb_1 <- glm.nb(triggerCountTh ~ mag, full_data)

mod_negb_2 <- glm.nb(triggerCountTh ~ mag + depth + heatFlow + crustalThick + mantleThick +
                     elevation + rake*strike, full_data)

mod_negb_3 <- glm.nb(triggerCountTh ~ mag + I(mag^3) + depth + heatFlow + crustalThick + mantleThick +
                     elevation + rake*strike, full_data)

mod_negb_4 <- glm.nb(triggerCountTh ~ mag + I(mag^3) + I(mag^5) + depth + heatFlow + crustalThick + mantleThick +
                     elevation + rake*strike, full_data)

anova(mod_negb_1, mod_negb_2, mod_negb_3, mod_negb_4)

coef_data <- data.frame("Quasipoisson" = c(coefficients(mod_quasi_3)[1:3], 0, coefficients(mod_quasi_3)[4:11]),
           "NegBin_1" = c(coefficients(mod_negb_3)[1:3], 0, coefficients(mod_negb_3)[4:11]),
           "NegBin_2" = c(coefficients(mod_negb_4)),
           row.names = names(coefficients(mod_negb_4)))
coef_data

ggplot(full_data, aes(x = heatFlow, colour = willTrigger)) + geom_histogram(bins = 50)
ggplot(full_data, aes(x = elevation, colour = willTrigger)) + geom_histogram(bins = 50)
ggplot(full_data, aes(x = mantleThick, colour = willTrigger)) + geom_histogram(bins = 50)
ggplot(full_data, aes(x = crustalThick, colour = willTrigger)) + geom_histogram(bins = 50)


x <-
ggplot(full_data, aes(x = elevation, colour = willTrigger)) + geom_histogram()
ggplot(full_data[full_data$willTrigger,],aes(x=elevation, y=relativTrigger))+geom_bar(stat="identity")

ggplot(full_data, aes(x=elevation, colour=willTrigger))+ geom_boxplot()
ggplot(full_data, aes(x=mantleThick, colour=willTrigger))+ geom_boxplot()

PlotRelativeFrequency(x)
full_data$relativTrigger<- ifelse(full_data$willTrigger,1/nrow(full_data[full_data$willTrigger,]),1/nrow(full_data[!full_data$willTrigger,]))
ggplot(full_data, aes(x=elevation, y=relativTrigger, group=willTrigger)) +geom_bar(stat = "identity")
ggplot(full_data, aes(x=elevation))+geom_freq()

plot(elevation~relativTrigger, data=full_data)




df <- rbind(
  data.frame(gender=c(rep('M',5)), outcome=c(rep('1',4),'0')),
  data.frame(gender=c(rep('F',10)), outcome=c(rep('1',7),rep('0',3)))
)

df

ggplot(df, aes(outcome)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  facet_wrap(~gender, nrow=2, ncol=1)


df1 <- melt(ddply(df,.(gender),function(x){prop.table(table(x$outcome))}),id.vars = 1)

ggplot(df1, aes(x = variable,y = value)) +
  facet_wrap(~gender, nrow=2, ncol=1) +
  geom_freqpoly(stat = "identity")
