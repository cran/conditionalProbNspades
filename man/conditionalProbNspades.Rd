\name{conditionalProbNspades}
\alias{conditionalProbNspades}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
conditionalProbNspades
}
\description{
computes an array of probabilities of number of spades in an opponent's (unknown) hand \cr
conditioned on the number of spades in my (known) hand
}
\usage{
conditionalProbNspades(N)
}
\arguments{
  \item{N}{
N is the number of spades in my hand
}
}
\value{
returns an array of probabilities of number of spades in an opponent's (unknown) hand \cr
conditioned on the number of spades in my (known) hand
}
\author{
Barry Zeeberg
}
\examples{
x<-conditionalProbNspades(4)
}
