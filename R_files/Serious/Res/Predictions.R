mod_best <- mod_zeroinfl_best3
notrig_data <- full_data[full_data$triggerCountTh==0,]
new_notrig <-apply(notrig_data[,colnames(notrig_data) %in%
                    c("mag","depth","crustalThick","mantleThick","rake_mod","strainRate")],
      2, mean)
new_data_1 <-as.data.frame(t(new_notrig))
predict(mod_best,newdata = new_data_1)

prednb<-mod_best$coefficients[[1]]
exp(sum(prednb*c(1,new_notrig[c(1,2,4,5,6)])))

predlog<-mod_best$coefficients[[2]]

1/(1+exp(-sum(predlog*c(1,new_notrig))))


trig_data <- full_data[full_data$triggerCountTh>0,]
new_trig <-apply(trig_data[,colnames(trig_data) %in%
                                 c("mag","depth","crustalThick","mantleThick","rake_mod","strainRate")],
                   2, mean)
new_data_2 <-as.data.frame(t(new_trig))
predict(mod_best,newdata = new_data_2)
exp(sum(prednb*c(1,new_trig[c(1,2,4,5,6)])))


new_tohoku <- full_data[6247,colnames(trig_data) %in%
                          c("mag","depth","crustalThick","mantleThick","rake_mod","strainRate")]
predict(mod_best,new_tohoku)
