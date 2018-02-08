setwd("C:/Users/Alyer/Desktop/R/Data Jam 2018/CSV Files")

#Read in Data
generalData<-read.csv("General Datasheet for analysis.csv")
gBoolFinal<-read.csv("Datasheet of Patients.csv")
conditionsList<-read.csv("List of Diseases.csv")[,1]
for (x in 3:16) {
generalData[,x]<-as.numeric(generalData[,x])
}

#Generating list of patients
gFilteredData<-matrix(c(0),ncol=14,nrow=range(generalData[,18])[2])
for (y in 1:dim(generalData)[1]) {
  z<-generalData[y,18]
  gFilteredData[z,1:14]<-as.logical(gFilteredData[z,1:14]+generalData[y,3:16])
}
gBoolFinal<-cbind(gFilteredData,generalData[which(generalData[,17]==0),19:20])

write.csv(gBoolFinal,"Datasheet of Patients.csv",row.names = FALSE)

#Disease Occurence Rates
gTestSample<-matrix(c(colSums(gBoolData),dim(gBoolData)[1]-colSums(gBoolData)), ncol=14,nrow=2,dimnames = list(c("Positive", "Negative"),conditionsList),byrow = TRUE)
gTestSample

#Sort by Age
ageSortedIndex<-order(gBoolFinal[,15])
ageSorted<-gBoolFinal[ageSortedIndex,]

#Sort by gender
menSortIndex<-grep("M",gBoolFinal[,16])
womenSortIndex<-setdiff(1:dim(gBoolFinal)[1],menSortIndex)
#genderSorted<-gBoolFinal[c(menSortIndex,womenSortIndex),]


#Edema: 1:2.1 male to female
#Effusion: 1.3:1 Male to female
#Emphysema: 2.1:2.5 Male to female
#Pleural Thickening: 724:53 Male to female
#Pneumothorax: 3.3:1 male to female

#Pneumonia: 1:1 children to adults

gTestResultsMatrix<-matrix(c(0),nrow = 14,ncol = 2, dimnames = list(conditionsList,c("G","p-Value")))
gTestTables<-list()
numMales<-length(menSortIndex)
numFemales<-length(womenSortIndex)
totalPeople<-numMales+numFemales
expectedRatios<-list(c(.5,.5),c(.5,.5),c(.5,.5),c(1/3.1,2.1/3.1),c(1.3/2.3,1/2.3),c(2.1/4.6,2.5/4.6),c(.5,.5),c(.5,.5),c(.5,.5),c(.5,.5),c(.5,.5),c(724/779,53/779),c(.5,.5),c(3.3/4.3,1/4.3))
degreesFreed<-1

for (x in 1:length(conditionsList)) {
  
  #List of positives
  posList<-which(gBoolFinal[,x]==1)
  
  #List of positive men and women
  posMenIndex<-intersect(posList,menSortIndex)
  posWomenIndex<-intersect(posList,womenSortIndex)
  
  #Corrected amounts of positive men and women
  posMen<-length(posMenIndex)/numMales*100
  posWomen<-length(posWomenIndex)/numFemales*100
  observedValues<-c(posMen,posWomen)
  
  #create a two way table
  gTestTables[[x]]<-matrix(c(observedValues,expectedRatios[[x]]),byrow = TRUE,nrow = 2)
  
  #perform chi-square/g-test
  gValue<-gTestFunction(observedValues,expectedRatios[[x]])
  gTestResultsMatrix[x,1]<-gValue
  gTestResultsMatrix[x,2]<-1-pchisq(gValue, df=degreesFreed,lower.tail=FALSE)
  
}

write.csv(gTestResultsMatrix,"Dataset Accuracy.csv",row.names = FALSE)


