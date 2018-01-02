\name{bigen}
\alias{bigen.visual}
\alias{bigen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Binomal random variable
}
\description{
  \code{bigen}, takes two parameters p and n and repeats an experience with success probability p and repeat it n times. Then returns number of success.

  \code{bigen.visual}, takes two parameters p and n like \code{bigen} and plots a Binomial distribution with taking 1000 data from \code{bigen}.
}
\usage{
bigen(p, n)
bigen.visual(p, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
  \code{Success probability of a single experiment.}
%%     ~~Describe \code{x} here~~
}
  \item{n}{
  \code{Number of repeating the experience}
  }
}
\details{

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
bigen(0.5, 10) ##{1...10}
bigen.visual(0.5, 10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ bigen }% use one of  RShowDoc("KEYWORDS")
\keyword{ binomial }% __ONLY ONE__ keyword per line
