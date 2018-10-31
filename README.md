# Bayesian Methods for Ecology

### Auckland, 6-8 november 2018

This GitHub site contains materials for a workshop on Bayesian Methods for Ecology.

## Schedule

TBD.

We’ll cover Bayesian theory from the basics, Markov chain Monte Carlo methods, linear models (linear regression, logistic regression, Poisson regression, including random effects and mixed models).

The rest of the course will respond to what participants are interested in, such as imperfect detection or survival analysis, and analysis of participants’ own data.

## Software to install

The course will be taught in R, with most Bayesian models being fit using JAGS.

Participants will need to install JAGS version 4.3.0, e.g. [from the JAGS sourceforge page](https://sourceforge.net/projects/mcmc-jags/files/)

There are various R packages to interface with JAGS. One of the most straightforward is `jagsUI`, which can be installed from within R with the following command:

```r
install.packages("jagsUI")
```

