\name{pogen}
\alias{pogen}
\alias{pogen.visual}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Poisson RV
}
\description{
uses Exponential distribution with parameter "lambda" to create Poisson distribution with parameter "lambda" within time "t".
}
\usage{
pogen(t,lambda)
pogen.visual(t,lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{
  \code{
    time.
  }
  }
  \item{lambda}{
  \code{
    Poisson distribution parameter.
  }

}
}
\details{
In probability theory and statistics, the Poisson distribution (French pronunciation [pwaˈsɔ̃]; in English often rendered /ˈpwɑːsɒn/), named after French mathematician Siméon Denis Poisson, is a discrete probability distribution that expresses the probability of a given number of events occurring in a fixed interval of time or space if these events occur with a known constant rate and independently of the time since the last event. The Poisson distribution can also be used for the number of events in other specified intervals such as distance, area or volume.

For instance, an individual keeping track of the amount of mail they receive each day may notice that they receive an average number of 4 letters per day. If receiving any particular piece of mail does not affect the arrival times of future pieces of mail, i.e., if pieces of mail from a wide range of sources arrive independently of one another, then a reasonable assumption is that the number of pieces of mail received in a day obeys a Poisson distribution. Other examples that may follow a Poisson include the number of phone calls received by a call center per hour and the number of decay events per second from a radioactive source.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\examples{
\code{pogen}(5,2)
\code{pogen.visual}(5,2)
}
