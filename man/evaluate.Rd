\name{Evaluate}
\alias{evaluate}
\title{
Evaluate
}
\description{
Function to compute prediction metric on PSet
}
\usage{evaluate(template)}
\details{
Computes the average within-environment correlation between predictions and validation set.
}
\examples{
data(data2024)
PSet$Yield_Mg_ha = rnorm(nrow(PSet))
evaluate(PSet)
}
\keyword{evaluate}
