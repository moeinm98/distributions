\name{rgenerator}
\alias{rgenerator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pseudo-Random Generator
}
\description{
This function use linear congruential generator to generate random number between min and max.
The seed of this method is set automatically with time of system.
This method are using in C,C++ and java random generator.
The constants which are used in this function are same as java random generator.
This function has 3 parametes which are set as specific number but can be set with inputs.
}
\usage{
rgenerator(min=0 , max=1 , number=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{min}{
      \code{
      minimum
      }
    }
      \item{max}{
      \code{
      maximum
      }
    }
    \item{number}{
    \code{
    number of random which must be generate
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
rgenerator()
or
rgenerator(3,8)
or
rgenerator(3,8,10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
