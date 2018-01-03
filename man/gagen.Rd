\name{gagen}
\alias{gagen}
\alias{gagen.visualS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gamma RV
}
\description{
Takes argument k and lambda ,by using Exponential distribution with argument "lambda" , returns the time that the kth Gamma event occurs.
}
\usage{
gagen(k,lambda)
gagen.visual(k,lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{
    \code{
      number of events.
    }
  }
  \item{lambda}{
    \code{
      Gamma distribution parameter.
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
\examples{
\code{gagen}(5,2)
\code{gagen.visual}(5,2)

}