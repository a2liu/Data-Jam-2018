observed<-c(4114,5886)
expected<-c(.1,.9)

chisq.test(observed,p=expected,correct = FALSE)

observeCorrect<-rep(1,4114)
observeIncorrect<-rep(0,5886)

expectCorrect<-rep(1,1000)
expectIncorrect<-rep(0,9000)

observeBinary<-c(observeCorrect,observeIncorrect)
expectBinary<-c(expectCorrect,expectIncorrect)

t.test(observeBinary,expectBinary)

random<-10
mlModel<-41.14
radiologist<-85
categories<-c(random,mlModel,radiologist)
barplot(categories,col  = brewer.pal(3,"Set1"))
barplot(categories, main="Accuracy", xlab=" ", ylab="Accuracy (%)", names.arg=c("Random Chance","Model","Radiologist"),border="red")
