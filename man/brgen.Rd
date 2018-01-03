\name{brgen}
\alias{brgen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bernoulli Numbers
}
\description{
Brgen function using cugen function to create bernoulli number with parameter p
}
\usage{
brgen(p)
}
\arguments{
  \item{p}{
    \code{
    Probability of success
    }
}
}

\details{
In probability theory and statistics, the Bernoulli distribution, named after Swiss mathematician Jacob Bernoulli,[1] is the probability distribution of a random variable which takes the value 1 with probability p {\displaystyle p} p and the value 0 with probability q = 1 − p {\displaystyle q=1-p} q=1-p — i.e., the probability distribution of any single experiment that asks a yes–no question; the question results in a boolean-valued outcome, a single bit of information whose value is success/yes/true/one with probability p and failure/no/false/zero with probability q. It can be used to represent a coin toss where 1 and 0 would represent "head" and "tail" (or vice versa), respectively. In particular, unfair coins would have p ≠ 0.5 {\displaystyle p\neq 0.5} p\neq 0.5.

The Bernoulli distribution is a special case of the Binomial distribution where a single experiment/trial is conducted (n=1). It is also a special case of the two-point distribution, for which the outcome need not be a bit, i.e., the two possible outcomes need not be 0 and 1.
}

\examples{
brgen(0.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
