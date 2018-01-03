\name{bigen}
\alias{bigen.visual}
\alias{bigen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Binomal RV
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
In probability theory and statistics, the binomial distribution with parameters n and p is the discrete probability distribution of the number of successes in a sequence of n independent experiments, each asking a yes–no question, and each with its own boolean-valued outcome: a random variable containing single bit of information: success/yes/true/one (with probability p) or failure/no/false/zero (with probability q = 1 − p). A single success/failure experiment is also called a Bernoulli trial or Bernoulli experiment and a sequence of outcomes is called a Bernoulli process; for a single trial, i.e., n = 1, the binomial distribution is a Bernoulli distribution. The binomial distribution is the basis for the popular binomial test of statistical significance.

The binomial distribution is frequently used to model the number of successes in a sample of size n drawn with replacement from a population of size N. If the sampling is carried out without replacement, the draws are not independent and so the resulting distribution is a hypergeometric distribution, not a binomial one. However, for N much larger than n, the binomial distribution remains a good approximation, and is widely used.
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
