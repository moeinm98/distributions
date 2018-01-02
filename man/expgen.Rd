\name{expgen}
\alias{expgen.visual}
\alias{expgen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Exponential random variable
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
\code{expgen}, takes a parameter lambda as the parameter of exponential distribution and returns a random variable witch follows exponential distribution.

  \code{expgen.visual}, takes a parameter lambda like \code{expgen} and plots a expgen with taking 1000 data from \code{expgen}.
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
%%  ~~ If necessary, more details than the description above ~~
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
