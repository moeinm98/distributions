\name{nogen}
\alias{nogen}
\alias{nogen.visual}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
  Takes in "u" as the mean and "s" as the variance and by using poisson distribution returns a value following normal distribution with the given arguments.
}
\usage{
nogen(u,s)
nogen.visual(u,s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{
    \code{
      mean of the Normal distribution.
    }
  }
  \item{s}{
    \code{
       variance of the Normal distribution.
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
\code{nogen}(0,2)
\code{nogen.visual}(0,2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
