# train and test a random forest model based on the Dengue HTS dataset

# load the required libraries
library(randomForest)
library(Kendall)

#calculate molecular descriptors using RDKit
system("python R/cheminformaticsbook-master/listing_2.py c:\\Users\\Noreena\\Documents\\R\\DengueKUPLibrary\\KUPLibraryDengueInhsmi.txt c:\\Users\\Noreena\\Documents\\R\\DengueKUPLibrary\\KUPLibraryDenguePercentInhmd.out")

# read the Dengue HTS data
test = read.table("c:\\Users\\Noreena\\Documents\\R\\DengueKUPLibrary\\KUPLibraryDenguePercentInhmd.out",header=T,row.names=1)

# calculate the root mean squared error
rmsError<-function(a,b){
	sqrt(sum((a-b)**2)/length(a))
}

# split a dataset into training and test sets
splitTrainTest<-function(dataSet,trainingFraction=0.7){
	idxList = sample(1:nrow(dataSet))	
	numTrain = floor(trainingFraction * nrow(dataSet))
	trainIdx = idxList[1:numTrain]
	testIdx = idxList[(numTrain+1):length(idxList)]
	list("train"=trainIdx,"test"=testIdx)
}

# merge descriptors and experimental data into a single dataframe
mergeData<-function(descriptors,logS){
	mergedData = merge(logS,descriptors,by=0)
	mergedData = mergedData[,-c(1,3)]
	mergedData
}

# use descriptors and experimental data to train and test a random forest model
predictRf<-function(test,trainingFraction=0.7){
	ttSplit = splitTrainTest(test,trainingFraction)
	rf = randomForest(LOGS~.,test[ttSplit$train,])
	pred = predict(rf,test[ttSplit$test,])
	list("train"=ttSplit$train,"test"=ttSplit$test,"pred"=pred,"exper"=test[ttSplit$test,]$LOGS,"model"=rf)
}

# build and test the random forest model
res = predictRf(test)

# plot the test set results
colors = c("blue")
plot(res$exper,res$pred,xlab="Experimental LogS",ylab="Predicted LogS",col=colors)

# output results
cat(sprintf("      Train = %d\n",length(res$train)))
cat(sprintf("       Test = %d\n",length(res$test)))
cat(sprintf("  Pearson r = %.2f\n",cor(res$pred,res$exper)))
cat(sprintf("Kendall tau = %.2f\n",Kendall(res$pred,res$exper)$tau))
cat(sprintf("  RMS error = %.2f\n",rmsError(res$pred,res$exper)))

