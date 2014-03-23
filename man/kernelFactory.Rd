\name{kernelFactory}
\alias{kernelFactory}
\title{Binary classification with Kernel Factory}
\usage{
kernelFactory(x = NULL, y = NULL, cp = 1, rp = round(log(nrow(x), 10)),
  method = "burn", ntree = 500, popSize = rp * cp * 7, iters = 80,
  mutationChance = 1/(rp * cp), elitism = max(1, round((rp * cp) * 0.05)))
}
\arguments{
  \item{x}{A data frame of predictors (numeric, integer or
  factor). Categorical variables need to be factors.}

  \item{y}{A factor containing the response vector. Only
  \{0,1\} is allowed.}

  \item{cp}{The number of column partitions.}

  \item{rp}{The number of row partitions.}

  \item{method}{Can be one of the following: POLynomial
  kernel function (\code{pol}), LINear kernel function
  (\code{lin}), Radial Basis kernel Function \code{rbf}),
  random choice (random={pol, lin, rbf}) (\code{random}),
  burn- in choice of best function (burn={pol, lin, rbf })
  (\code{burn}). Use \code{random} or \code{burn} if you
  don't know in advance which kernel function is best.}

  \item{ntree}{Number of trees in the Random Forest base
  classifiers.}

  \item{popSize}{Population size of the genetic algorithm.}

  \item{iters}{Number of generations of the genetic
  algorithm.}

  \item{mutationChance}{Mutationchance of the genetic
  algorithm.}

  \item{elitism}{Elitism parameter of the genetic
  algorithm.}
}
\value{
An object of class \code{kernelFactory}, which is a list
with the following elements: \item{trn}{Training data set.}
\item{trnlst}{List of training partitions.}
\item{rbfstre}{List of used kernel functions.}
\item{rbfmtrX}{List of augmented kernel matrices.}
\item{rsltsKF}{List of models.} \item{cpr}{Number of column
partitions.} \item{rpr}{Number of row partitions.}
\item{cntr}{Number of partitions.} \item{wghts}{Weights of
the ensemble members.} \item{nmDtrn}{Vector indicating the
numeric (and integer) features.} \item{rngs}{Ranges of
numeric predictors.}
}
\description{
\code{kernelFactory} implements an ensemble method for
kernel machines (Ballings and Van den Poel, 2013).
}
\examples{
#Credit Approval data available at UCI Machine Learning Repository
data(Credit)
#take subset (for the purpose of a quick example) and train and test
Credit <- Credit[1:100,]
train.ind <- sample(nrow(Credit),round(0.5*nrow(Credit)))

#Train Kernel Factory on training data
kFmodel <- kernelFactory(x=Credit[train.ind,names(Credit)!= "Response"],
          y=Credit[train.ind,"Response"], method=random)

#Deploy Kernel Factory to predict response for test data
#predictedresponse <- predict(kFmodel, newdata=Credit[-train.ind,names(Credit)!= "Response"])
}
\author{
Authors: Michel Ballings and Dirk Van den Poel, Maintainer:
\email{Michel.Ballings@GMail.com}
}
\references{
Ballings, M. and Van den Poel, D. (2013), Kernel Factory:
An Ensemble of Kernel Machines. Expert Systems With
Applications, 40(8), 2904-2913.
}
\seealso{
\code{\link{predict.kernelFactory}}
}
\keyword{classification}

