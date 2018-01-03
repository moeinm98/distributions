\name{expgen}
\alias{expgen.visual}
\alias{expgen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Exponential RV
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
\code{expgen}, takes a parameter lambda as the parameter of exponential distribution and returns a random variable witch follows Exponential distribution.

  \code{expgen.visual}, takes a parameter lambda like \code{expgen} and plots an Exponantial distribution with taking 1000 data from \code{expgen}.
}
\usage{
expgen(lambda)
expgen.visual(lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lambda}{
    \code{
    Exponential distribution parameter.
    }
}
}
\details{
In probability theory and statistics, the exponential distribution (also known as negative exponential distribution) is the probability distribution that describes the time between events in a Poisson point process, i.e. a process in which events occur continuously and independently at a constant average rate. It is a particular case of the gamma distribution. It is the continuous analogue of the geometric distribution, and it has the key property of being memoryless. In addition to being used for the analysis of Poisson point processes it is found in various other contexts.

The exponential distribution is not the same as the class of exponential families of distributions, which is a large class of probability distributions that includes the exponential distribution as one of its members, but also includes the normal distribution, binomial distribution, gamma distribution, Poisson, and many others.
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
\code{expgen}(2)
\code{expgen.visual}(2)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
