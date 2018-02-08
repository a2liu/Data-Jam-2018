setwd("Directory of file folder")
library(Hmisc)

#Read in Data
generalData<-read.csv("General Datasheet for analysis.csv")
conditionsList<-read.csv("List of Diseases.csv")[,1]

#Total Rows
matrix(c(dim(generalData)),ncol=2,nrow=1,byrow = TRUE,dimnames = list(c(""),c("Rows:","Columns:")))

#Column Names
matrix(c(colnames(generalData)),ncol=1,dimnames = list(c(1:25),c("")))

#Age
paste("Average Age:", sum(generalData[,19])/dim(generalData)[1])
describe(generalData[,19])
hist(generalData[,19],xlim = c(0,100),breaks = 200)

#Amounts of occurences of each condition
conditionSummary <-matrix(c(as.character(conditionsList)),ncol=14,nrow=2,byrow = TRUE)
for (x in 1:length(conditionsList)) {
  conditionSummary[2,x]<-sum(generalData[,x+2])
}
conditionSummary

#Amounts of occurences of each condition, alone
conditionSummary2 <-matrix(c(as.character(conditionsList)),ncol=14,nrow=2,byrow = TRUE)
for (x in 1:length(conditionsList)) {
  conditionSummary2[2,x]<-length(which(generalData[,2]==conditionSummary2[1,x]))
}
conditionSummary2

#Gender
paste(c("Men:",sum(grepl("M",generalData[,20])),"Women:",sum(grepl("F",generalData[,20]))))

#View
paste(c("PA:",sum(grepl("PA",generalData[,21])),"AP:",sum(grepl("AP",generalData[,21]))))


