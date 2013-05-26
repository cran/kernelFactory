predict.kernelFactory <-
function(object, 
   newdata=NULL, ...  )
{
if (!inherits(object, "kernelFactory")) stop("object not of class kernelFactory")

if (is.null(object)) stop("You must provide the name of the model through object.")
if (is.null(newdata)) {
stop("Data cannot be NULL.")
}else if (any(is.na(newdata))){
stop("NAs not permitted.")
}






if(all(colnames(object$trn[,names(object$trn)!= "Y"]) %in% colnames(newdata ))==FALSE ) stop("Column names have to be equal in data and new data.")




 



numericcolumns <- newdata[,object$nmDtrn]


numericcolumns <- data.frame(t(t(numericcolumns )/as.numeric(object$rngs)))


if ( length(data.frame(newdata[,which(sapply(newdata[,names(newdata) != "Y"],is.factor))]  )) > 1 ) {
		
		newdata <- data.frame(numericcolumns,newdata[,which(sapply(newdata[,names(newdata)!= "Y"],is.factor))])
	
} else if ( length(data.frame(newdata[,which(sapply(newdata[,names(newdata) != "Y"],is.factor))]  )) == 1 ) {
		
		tempdf <- data.frame(newdata[,which(sapply(newdata[,names(newdata)!= "Y"],is.factor))])
		colnames(tempdf) <- names(	which(	sapply(newdata,is.factor)	)	)
		newdata <- data.frame(numericcolumns,tempdf)
			
		rm(tempdf)

	
} else {
		
		newdata <- data.frame(numericcolumns)

}

rm(numericcolumns)



newdata <- newdata[,colnames(object$trn[,names(object$trn)!= "Y"])]





testlist <- list()
colparts <- object$cpr
rowparts <- object$rpr

startcol <- 1
counter <- 0


if (ncol(newdata)%%colparts > 0) {
numbercols <- ncol(newdata)-(ncol(newdata)%%colparts )
} else {
numbercols <-  ncol(newdata)  }



for (i in 1:colparts ){

  
endcol <-(i/colparts )*numbercols 
if (i==colparts ) endcol <- ncol(newdata)

startrow <- 1
for (j in 1:rowparts) {
counter = counter + 1

testlist[[counter]] <- newdata[,c(startcol:endcol)]



}

startcol <- endcol + 1
}





predicted <- list()
resultsKIRFScored <- list()
for (i in 1:object$cntr) {                


if (is.null(testlist[[i]][,sapply(testlist[[i]],is.numeric)]) == FALSE) {



trainobs <- object$trnlst[[i]][,sapply(object$trnlst[[i]],is.numeric)]



           

resultsKIRFScored[[i]] <- data.frame(data.frame(kernelMatrix(object$rbfstre[[i]], as.matrix(testlist[[i]][,sapply(testlist[[i]],is.numeric)]),as.matrix(trainobs))),
data.frame(testlist[[i]]))
                 
          
colnames(resultsKIRFScored[[i]]) <- colnames(object$rbfmtrX[[i]])


predicted[[i]] <- predict(object$rsltsKF[[i]],resultsKIRFScored[[i]],type="prob")[,2]



} else {
predicted[[i]] <- predict(object$rsltsKF[[i]],testlist[[i]],type="prob")[,2]


}
}



final <- data.frame(matrix(nrow=nrow(newdata),ncol=(object$cntr)))
for (i in 1:object$cntr) {
final[,i] <- predicted[[i]]
}





result <- rowSums(t(as.numeric(object$wghts) * t(final)))


return(result)
}
