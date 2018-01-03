\name{gegen}
\alias{gegen.visual}
\alias{gegen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Geometric RV
}
\description{
\code{gegen}, takes a parameter p as the probability of a single experience and returns a random variable witch follows Geometric distribution.

  \code{expgen.visual}, takes a parameter p like \code{expgen} and plots a Geometric distribution with taking 1000 data from \code{gegen}.
}
\usage{
gegen(p)
gegen.visual(p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
    \code{
    success Probability of a single experiment.
    }
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
%Geometric random variable is a random variable that describes the number failures before the first success, supported on the set { 0, 1, 2, 3, ... }.
In probability theory and statistics, the geometric distribution is either of two discrete probability distributions:

    The probability distribution of the number X of Bernoulli trials needed to get one success, supported on the set { 1, 2, 3, ...}
    The probability distribution of the number Y = X − 1 of failures before the first success, supported on the set { 0, 1, 2, 3, ... }

Which of these one calls "the" geometric distribution is a matter of convention and convenience.

These two different geometric distributions should not be confused with each other. Often, the name shifted geometric distribution is adopted for the former one (distribution of the number X); however, to avoid ambiguity, it is considered wise to indicate which is intended, by mentioning the support explicitly.

The geometric distribution gives the probability that the first occurrence of success requires k independent trials, each with success probability p. If the probability of success on each trial is p, then the probability that the kth trial (out of k trials) is the first success is

    Pr ( X = k ) = ( 1 − p ) k − 1 p {\displaystyle \Pr(X=k)=(1-p)^{k-1}\,p\,} \Pr(X=k)=(1-p)^{k-1}\,p\,

for k = 1, 2, 3, ....

The above form of the geometric distribution is used for modeling the number of trials up to and including the first success. By contrast, the following form of the geometric distribution is used for modeling the number of failures until the first success:

    Pr ( Y = k ) = ( 1 − p ) k p {\displaystyle \Pr(Y=k)=(1-p)^{k}\,p\,} \Pr(Y=k)=(1-p)^{k}\,p\,

for k = 0, 1, 2, 3, ....

In either case, the sequence of probabilities is a geometric sequence.

For example, suppose an ordinary die is thrown repeatedly until the first time a "1" appears. The probability distribution of the number of times it is thrown is supported on the infinite set { 1, 2, 3, ... } and is a geometric distribution with p = 1/6.
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
gegen(0.5)
gegen.visual(0.5)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
