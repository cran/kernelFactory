
	
kernelFactory <- 
function(x=NULL,
	   y=NULL, 
	   cp=1, 
	   rp=round(log(ncol(x)+1,4)), 
	   method="burn" ){
	
	
	if (is.null(x) || is.null(y)) {
		stop("x or y cannot be NULL.")
	}else if (any(is.na(x)) || any(is.na(y))){
		stop("NAs not permitted.")
	}

	if (!is.factor(y)) stop("y must be a factor. Support for regression will be added later.")
	
	
	if (any(table(y) == 0)) stop("Cannot have empty classes in y.")
	
	if (length(unique(y)) != 2) stop("Must have 2 classes. Support for more classes will be added later.")

	if (length(y) != nrow(x)) stop("x and dependent variable have to be of equal length.")


	x <- data.frame(x,Y=y)
	

	x <- data.frame(x[order(runif(nrow(x))),])
	trainingset <- data.frame(x)[1:1:floor(0.75*nrow(x)),]

	validationset <- data.frame(x)[(floor(0.75*nrow(x))+1 ):nrow(x),]
	datasets <- list(trainingset,validationset)

	


	 	
	numIDtrain <- sapply(datasets[[1]],is.numeric)
	numericcolumnsTRAIN <- datasets[[1]][,numIDtrain ]
	numericcolumnsVAL <- datasets[[2]][,numIDtrain ]


	ranges <- data.frame()
	ranges <- data.frame(sapply(numericcolumnsTRAIN ,range))
	ranges[3,] <- ranges[2,]- ranges[1,]
	ranges[3,][ranges[3,]==0] <- 1

	numericcolumnsTRAIN <- data.frame(t(t(numericcolumnsTRAIN)/as.numeric(ranges[3,])))
	numericcolumnsVAL <- data.frame(t(t(numericcolumnsVAL)/as.numeric(ranges[3,])))




	datasets[[1]] <- data.frame(numericcolumnsTRAIN,datasets[[1]][,sapply(datasets[[1]],is.factor)])
	datasets[[2]] <- data.frame(numericcolumnsVAL,datasets[[2]][,sapply(datasets[[2]],is.factor)])


	
	rm(numericcolumnsTRAIN)
	rm(numericcolumnsVAL)
	

	train <- datasets[[1]]
	test <- datasets[[2]]



	train <- data.frame(train[order(runif(nrow(train))),order(runif(ncol(train)))])
	train <- data.frame(train[,names(train)!= "Y"], Y=train$Y)
	
	test <- test[,colnames(train)]



	
	trainlist <- list()
	colparts <- cp
	rowparts <- rp

	startcol <- 1
	counter <- 0
	
	
	if (ncol(train[,names(train)!= "Y"])%%colparts > 0) {
	numbercols <- ncol(train[,names(train)!= "Y"])-(ncol(train[,names(train)!= "Y"])%%colparts )
	} else {
	numbercols <- ncol(train[,names(train)!= "Y"]) }

	
	if (nrow(train)%%rowparts > 0) {
	numberrows <- nrow(train)-(nrow(train)%%rowparts )
	} else {
	numberrows <- nrow(train)}


	for (i in 1:colparts ){

		 
		endcol <-(i/colparts )*numbercols 
		if (i==colparts ) endcol <- ncol(train[,names(train)!= "Y"])
		
		startrow <- 1
		for (j in 1:rowparts) {
			counter = counter + 1
			endrow <-(j/rowparts )*numberrows
			if (j==rowparts) endrow <- nrow(train)


			trainlist[[counter]] <- train[startrow:endrow ,c(startcol:endcol,which(colnames(train)=="Y"))]
	
			
			if (any(table(trainlist[[counter]]$Y) == 0)) stop("Cannot have empty classes in y. Make sure number of rp is not too high.")
			
			if (length(unique(trainlist[[counter]]$Y)) != 2) stop("Must have 2 classes. Make sure number of rp is not too high.")

			startrow <- endrow + 1
		}

		startcol <- endcol + 1
	}



	
	testlist <- list()
	colparts <- colparts
	rowparts <- rowparts 

	startcol <- 1
	counter <- 0
	
	if (ncol(test[,names(test)!= "Y"])%%colparts > 0) {
	numbercols <- ncol(test[,names(test)!= "Y"])-(ncol(test[,names(test)!= "Y"])%%colparts )
	} else {
	numbercols <- ncol(test[,names(test)!= "Y"]) }



	for (i in 1:colparts ){

		 
			endcol <-(i/colparts )*numbercols 
			if (i==colparts ) endcol <- ncol(test[,names(test)!= "Y"])
		
			startrow <- 1
			for (j in 1:rowparts) {
				counter = counter + 1
			
				testlist[[counter]] <- test[,c(startcol:endcol,which(colnames(test)=="Y"))]
				
				if (any(table(testlist[[counter]]$Y) == 0)) stop("Cannot have empty classes in y. Make sure number of rp is not too high.")
				
				if (length(unique(testlist[[counter]]$Y)) != 2) stop("Must have 2 classes. Make sure number of rp is not too high.")

	
			
			}

			startcol <- endcol + 1
	}







	
	rbfstore <- list()
	rbfmatrX <- data.frame()
	resultsKF <- data.frame()


	
	
	
	if (as.character(substitute(method))=="rbf") {
		for (ii in 1:counter) {
			rbfstore[[ii ]]<- rbfdot(sigma = 1)
		}
	} else if (as.character(substitute(method))=="pol") {
		for (ii in 1:counter) {
			rbfstore[[ii ]]<- polydot(degree=2,scale=1)
		}
	} else if (as.character(substitute(method))=="lin") {
		for (ii in 1:counter) {
			rbfstore[[ii ]]<- vanilladot()
		}
	} else if (as.character(substitute(method))=="random") {
		for (ii in 1:counter) {
			randomnumber <- runif(1,min=0, max=1)
			if(randomnumber <= 0.33) {
				rbfstore[[ii ]]<- rbfdot(sigma = 1)
			} else if (randomnumber > 0.33 & randomnumber <= 0.66 ) {
				rbfstore[[ii ]]<- polydot(degree=2,scale=1)
			} else if (randomnumber > 0.66 ) {
				rbfstore[[ii ]]<- vanilladot()
			}
		}
	} else if (as.character(substitute(method))=="burn") {
		rbfstore <- list(rbfdot(sigma = 1),polydot(degree=2,scale=1),vanilladot())
		burnperf <- list()
		for (ii in 1:3) {
			
			numID <- sapply(trainlist[[1]],is.numeric)
			numericcolumns <- trainlist[[1]][,numID]
			
			if (is.null(trainlist[[1]][,sapply(trainlist[[1]],is.numeric)]) == FALSE) {
								
											
				rbfdt<-as.matrix(numericcolumns)
				rbfmatr<-kernelMatrix(rbfstore[[ii]], rbfdt)
				rbfmatr <-  data.frame(rbfmatr)


				rbfmatrX <- data.frame(rbfmatr, trainlist[[1]][,names(trainlist[[1]])!= "Y"])
				rbfmatrY <- trainlist[[1]][,which(colnames(trainlist[[1]])=="Y")]
                   
				rm(numID)
				rm(numericcolumns)
        								
            
		
				
				resultsKF  <-  	 randomForest(x=rbfmatrX,y=as.factor(rbfmatrY),  ntree=1000, importance=FALSE, na.action=na.omit)
				
				
				
				trainobs <- trainlist[[1]][,sapply(trainlist[[1]],is.numeric)]
								
																
				
           
								
				resultsKFScored <- data.frame(data.frame(kernelMatrix(rbfstore[[ii]], as.matrix(testlist[[1]][,sapply(testlist[[1]],is.numeric)]),as.matrix(trainobs))),
													data.frame(testlist[[1]][,names(testlist[[1]])!= "Y"]))
                 
          			
				colnames(resultsKFScored) <- colnames(rbfmatrX)
					
				predicted <- predict(resultsKF,resultsKFScored,type="prob")[,2]
			
				burnperf[[ii]] <- performance(prediction(predicted,testlist[[1]]$Y),"auc")@y.values[[1]]

			} else {
				
		 		resultsKF <-  randomForest(x=trainlist[[1]][,names(trainlist[[1]])!= "Y"],y=as.factor(trainlist[[1]]$Y),  ntree=1000, importance=FALSE, na.action=na.omit )
				
				
				predicted <- predict(resultsKF,testlist[[1]],type="prob")[,2]

				burnperf[[ii]] <- performance(prediction(predicted,testlist[[1]]$Y),"auc")@y.values[[1]]
  			}

		}
		
		bestkernelfunction <- rbfstore[[which.max(burnperf)]]
		rbfstore <- list()
		for (ii in 1:counter) {
			rbfstore[[ii]] <- bestkernelfunction
		}
				
	}


	rbfmatrX <- list()
	resultsKF <- list()


	for (i in 1:counter) {

			
								
			numID <- sapply(trainlist[[i]],is.numeric)
			numericcolumns <- trainlist[[i]][,numID]
				
			
			if (is.null(trainlist[[i]][,sapply(trainlist[[i]],is.numeric)]) == FALSE) {
								
				
				
				rbfdt<-as.matrix(numericcolumns)
				rbfmatr<-kernelMatrix(rbfstore[[i]], rbfdt)
				rbfmatr <-  data.frame(rbfmatr)


				rbfmatrX[[i]] <- data.frame(rbfmatr, trainlist[[i]][,names(trainlist[[i]])!= "Y"])
				rbfmatrY <- trainlist[[i]][,which(colnames(trainlist[[i]])=="Y")]
                   
				rm(numID)
				rm(numericcolumns)
        								


				
            
		
				
				resultsKF[[i]] <-   randomForest(x=rbfmatrX[[i]],y=as.factor(rbfmatrY),  ntree=1000, importance=FALSE, na.action=na.omit)
				

			} else {
				
		 		resultsKF[[i]]  <-   randomForest(x=trainlist[[i]][,names(trainlist[[i]])!= "Y"],y=as.factor(trainlist[[i]]$Y),  ntree=1000, importance=FALSE, na.action=na.omit )
				

			}

			
	}




	
	predicted <- list()
	resultsKFScored <- list()




	for (i in 1:counter) {                
	
			if (is.null(trainlist[[i]][,sapply(trainlist[[i]],is.numeric)]) == FALSE) {
	
				
				trainobs <- trainlist[[i]][,sapply(trainlist[[i]],is.numeric)]
								
																
				
           
								
				resultsKFScored[[i]] <- data.frame(data.frame(kernelMatrix(rbfstore[[i]], as.matrix(testlist[[i]][,sapply(testlist[[i]],is.numeric)]),as.matrix(trainobs))),
													data.frame(testlist[[i]][,names(testlist[[i]])!= "Y"]))
                 
          			
				colnames(resultsKFScored[[i]]) <- colnames(rbfmatrX[[i]])
	
				
				predicted[[i]] <- predict(resultsKF[[i]],resultsKFScored[[i]],type="prob")[,2]
				


	
			} else {
				predicted[[i]] <- predict(resultsKF[[i]],testlist[[i]],type="prob")[,2]
				

			
			}
	}
	



	
	final <- data.frame(matrix(nrow=nrow(test),ncol=(counter)))
	for (i in 1:counter) {
	final[,i] <- as.numeric(predicted[[i]])
	}


	
	

	evaluate <- function(string=c()) {
	    returnVal = NA
	    stringRepaired <- as.numeric(string)/sum(as.numeric(string))
		returnVal = -performance(prediction(rowSums(t(as.numeric(stringRepaired ) * t(final))),testlist[[i]]$Y),"auc")@y.values[[1]]
   
	   returnVal
	}


	rbga.results = rbga(rep(0,counter), rep(1,counter), 
	suggestions=t( as.data.frame( c(rep((1/counter),counter)))), popSize=40, iters=80,  mutationChance=0.01, evalFunc=evaluate)

	


	


	weights <- rbga.results$population[which.min(rbga.results$evaluations),]

	weights <- as.numeric(weights)/sum(as.numeric(weights))


result <- list(trn=train, 						
		   trnlst=trainlist,  					
		   rbfstre=rbfstore, 					
		   rbfmtrX=rbfmatrX, 					
		   rsltsKF=resultsKF, 					
		   cpr=cp,  						
		   rpr=rp, 	 						
		   cntr=counter,  					
		   wghts=weights,  					
		   nmDtrn=numIDtrain ,  					
		   rngs=ranges[3,]  ) 

class(result) <- "kernelFactory"					

return(result)
}




