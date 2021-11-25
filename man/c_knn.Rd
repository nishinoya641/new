\name{c_knn}
\alias{c_knn}
\title{knn!}
\usage{
c_knn()
}
\description{
give the classification error rate of a binary problem using knn
}
\examples{
data(iris)
irisnew <- iris
X2 <- as.matrix(irisnew[,1:4])
irisnew$Species <- ifelse(irisnew$Species=="setosa",1,0)
c_knn(irisnew)
}
