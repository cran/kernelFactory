\name{kernelFactory}
\alias{kernelFactory}
\title{Binary classification with Kernel Factory}
\description{
\code{kernelFactory} implements an ensemble method for kernel machines (Ballings and Van den Poel, 2013).
}
\usage{
kernelFactory(x = NULL, 
              y = NULL, 
              cp = 1, 
              rp = round(log(ncol(x)+1,4)), 
              method = burn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A data frame of predictors. Categorical variables need to be factors.
}
  \item{y}{
A factor containing the response vector.
}
  \item{cp}{
The number of column partitions.
}
  \item{rp}{
The number of row partitions. Setting this number too high may create partitions with only one class.
}
  \item{method}{
Can be one of the following: POLynomial kernel function (\code{pol}), LINear kernel function (\code{lin}), Radial Basis kernel Function \code{rbf}),
random choice (random={pol, lin, rbf}) (\code{random}), burn- in choice of best function (burn={pol, lin, rbf }) (\code{burn}).
Use \code{random} or \code{burn} if you don't know in advance which kernel function is best.
}
}
\value{
An object of class \code{kernelFactory}, which is a list with the following elements:
  \item{trn}{Training data set.}
  \item{trnlst}{List of training partitions.}
  \item{rbfstre}{List of used kernel functions.}
  \item{rbfmtrX}{List of augmented kernel matrices.}
  \item{rsltsKF}{List of models.}
  \item{cpr}{Number of column partitions.}
  \item{rpr}{Number of row partitions.}
  \item{cntr}{Number of partitions.}
  \item{wghts}{Weights of the ensemble members.}
  \item{nmDtrn}{Vector indicating the numeric features.}
  \item{rngs}{Ranges of numeric predictors.}
}
\references{
Ballings, M. and Van den Poel, D. (2013), Kernel Factory: An Ensemble of Kernel Machines. Expert Systems With Applications. Forthcoming.
}
\author{
Michel Ballings and Dirk Van den Poel
}

\seealso{
\code{\link{predict.kernelFactory}}
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
#predictedresponse <- predict(kFmodel, newdata=testset[,names(testset)!= "Response"])
}
\keyword{classification}