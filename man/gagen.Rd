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
In probability theory and statistics, the gamma distribution is a two-parameter family of continuous probability distributions. The exponential distribution, Erlang distribution, and chi-squared distribution are special cases of the gamma distribution. There are three different parametrizations in common use:

    With a shape parameter k and a scale parameter θ.
    With a shape parameter α = k and an inverse scale parameter β = 1/θ, called a rate parameter.
    With a shape parameter k and a mean parameter μ = k/β.

In each of these three forms, both parameters are positive real numbers.

The gamma distribution is the maximum entropy probability distribution (both with respect to a uniform base measure and with respect to a 1/x base measure) for a random variable X for which E[X] = kθ = α/β is fixed and greater than zero, and E[ln(X)] = ψ(k) + ln(θ) = ψ(α) − ln(β) is fixed (ψ is the digamma function).
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
