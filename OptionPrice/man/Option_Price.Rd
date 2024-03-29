\name{Option_Price}
\alias{Option_Price}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{


Option_price(s_now, strike_price, sigma, r, q, T, n, Call_or_Put). 
s_now is the stock price now;
strike_price is the strike price of the option;
sigma is stock price volatility;
r is risk-free interest rate;
q is the dividend rate;
T is the validity period of the option;
n is the number of option cycles;
Call_or_Put is whether the option is call option or put option, if it is call option, then Call_or_Put=1, if it is put option, then Call_or_Put=2.
}
\usage{
Option_Price(s_now, strike_price, sigma, r, q, T, n, Call_or_Put)
}

\arguments{
  \item{s_now}{
stock price now
}
  \item{strike_price}{
strike price of the option
}
  \item{sigma}{
stock price volatility
}
  \item{r}{
risk-free interest rate
}
  \item{q}{
dividend rate
}
  \item{T}{
validity period of the option
}
  \item{n}{
number of option cycles
}
  \item{Call_or_Put}{
whether the option is call option or put option, if it is call option, then Call_or_Put=1, if it is put option, then Call_or_Put=2
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (s_now, strike_price, sigma, r, q, T, n, Call_or_Put) 
{
    dt <- T/n
    p1 = 0.5
    p2 = 0.5
    u <- exp(sigma * sqrt(dt) + r * dt - q * dt)/(0.5 * (exp(sigma * 
        sqrt(dt)) + exp(-sigma * sqrt(dt))))
    d <- exp(-sigma * sqrt(dt) + r * dt - q * dt)/(0.5 * (exp(sigma * 
        sqrt(dt)) + exp(-sigma * sqrt(dt))))
    m <- matrix(nrow = n, ncol = n)
    m[1, 1] <- s_now
    for (j in 2:n) {
        m[1, j] <- m[1, j - 1] * u
    }
    for (i in 2:n) {
        for (j in i:n) {
            m[i, j] <- m[i - 1, j - 1] * d
        }
    }
    if (Call_or_Put == 1) {
        for (i in 1:n) {
            for (j in i:n) {
                m[i, j] <- max(0, (m[i, j] - strike_price))
            }
        }
        p <- matrix(nrow = n, ncol = n)
        p[, n] <- m[, n]
        for (jj in 1:(n - 1)) {
            j <- n - jj
            for (i in 1:j) {
                p[i, j] <- max((exp(-r * dt) * (p1 * p[i, j + 
                  1] + p2 * p[i + 1, j + 1])), m[i, j])
            }
        }
        EU <- matrix(nrow = n, ncol = n)
        EU[, n] <- m[, n]
        for (jj in 1:(n - 1)) {
            j <- n - jj
            for (i in 1:j) {
                EU[i, j] <- exp(-r * dt) * (p1 * EU[i, j + 1] + 
                  p2 * EU[i + 1, j + 1])
            }
        }
        cat("American call option price:", p[1, 1])
        cat(sep = "\n")
        cat("European call option price:", EU[1, 1])
    }
    for (i in 1:n) {
        for (j in i:n) {
            m[i, j] <- max(0, (strike_price - m[i, j]))
        }
    }
    p <- matrix(nrow = n, ncol = n)
    p[, n] <- m[, n]
    for (jj in 1:(n - 1)) {
        j <- n - jj
        for (i in 1:j) {
            p[i, j] <- max((exp(-r * dt) * (p1 * p[i, j + 1] + 
                p2 * p[i + 1, j + 1])), m[i, j])
        }
    }
    EU <- matrix(nrow = n, ncol = n)
    EU[, n] <- m[, n]
    for (jj in 1:(n - 1)) {
        j <- n - jj
        for (i in 1:j) {
            EU[i, j] <- exp(-r * dt) * (p1 * EU[i, j + 1] + p2 * 
                EU[i + 1, j + 1])
        }
    }
    cat("American put option price:", p[1, 1])
    cat(sep = "\n")
    cat("European put option price:", EU[1, 1])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
