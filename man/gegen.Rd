\name{gegen}
\alias{gegen.visual}
\alias{gegen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Geometric random variable
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
Geometric random variable is a random variable that describes the number failures before the first success, supported on the set { 0, 1, 2, 3, ... }.
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
