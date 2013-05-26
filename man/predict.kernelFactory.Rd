\name{predict.kernelFactory}
\alias{predict.kernelFactory}

\title{Predict method for kernelFactory objects}
\description{
Prediction of new data using kernelFactory.
}
\usage{
\method{predict}{kernelFactory}(object , newdata = NULL,...)
}

\arguments{
  \item{object}{An object of class \code{kernelFactory}, as created by the function \code{kernelFactory}}
  \item{newdata}{A data frame with the same predictors as in the training data.}
  \item{...}{Not used currently.}
}

\value{
A vector containing the response probabilities.
}
\references{
Ballings, M. and Van den Poel, D. (2013), Kernel Factory: An Ensemble of Kernel Machines. Expert Systems With Applications. Forthcoming.
}
\author{
Michel Ballings and Dirk Van den Poel
}

\seealso{
\code{\link{kernelFactory}}
}
\examples{
#Credit Approval data available at UCI Machine Learning Repository
data(Credit)

#Create training set (take a small subset for demonstration purposes)
Credit <- data.frame(Credit[order(runif(nrow(Credit ))),])[1:100,]
trainingset <- Credit[1:1:floor(0.50*nrow(Credit)),]
#Create test set
testset <- Credit[(floor(0.50*nrow(Credit))+1 ):nrow(Credit),]

#Train Kernel Factory on training data
kFmodel <- kernelFactory(x=trainingset[,names(trainingset)!= "Response"], 
                         y=trainingset$Response, method=random)

#Deploy Kernel Factory to predict response for test data
predictedresponse <- predict(kFmodel, newdata=testset[,names(testset)!= "Response"])
}
\keyword{classification}

