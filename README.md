# Bayesian Ecological Modelling workshop

### Auckland University of Technology, 6-8 november 2018

This GitHub site contains materials for a workshop on Bayesian Ecological Modelling, taught by [Mick McCarthy](https://qaeco.com/principal-researchers/#mick) and [Nick Golding](https://qaeco.com/researchfellows/#ngolding)

Scroll down for the schedule and [software installation instructions](#software-to-install).

You can view all these files on GitHub, or to [download them directly here](https://www.dropbox.com/sh/mn7thtbxvkwzppo/AABc1tiNrnObVQTv2d2ngpTfa?dl=0)

[Here's a link](https://chi-feng.github.io/mcmc-demo/app.html) to a cool interactive illustration of different MCMC algorithms fare on different types of posterior distribution.

## Schedule

This schedule is deliberately light on details - we'll introduce Bayesian theory and practical issues, and cover lots of example models in the *Examples and Exercises* sections. We have lots of examples to choose from and plan to adapt (and add to) these depending on the interests and experience of the participants.

### Tuesday 6th

##### 08.30-09.00 **Introductions and setup**

##### 09.00-10.30 **Introduction to Bayes**

##### 10.30-11.00 **Break**

##### 11.00-12.30 **Fitting a model in JAGS**

##### 12.30-13.30 **Lunch**

##### 13.30-15.00 **Examples and Exercises**

##### 15.00-15.30 **Break**

##### 15.30-17.00 **Examples and Exercises**


### Wednesday 7th

##### 08.30-09.00 **Feedback on content**

##### 09.00-10.30 **How does MCMC work?**

##### 10.30-11.00 **Break**

##### 11.00-12.30 **Examples and Exercises**

##### 12.30-13.30 **Lunch**

##### 13.30-15.00 **Examples and Exercises**

##### 15.00-15.30 **Break**

##### 15.30-17.00 **Examples and Exercises**


### Thursday 8th

##### 08.30-09.00 **Feedback on content**

##### 09.00-10.30 **BYO modelling problems**

##### 10.30-11.00 **Break**

##### 11.00-12.30ish **BYO modelling problems**

On the latter two mornings, there's alotted time for participants to feed back on which issues aren't clear, and what topics we should cover. Some possible topics are: imperfect detection, survival analysis, hierarchical modelling, and other Bayesian modelling software (Stan, INLA, greta). Please let us know if those, or anything else, are of interest!

## Software to install

The course will be taught in R, using R notebooks, and with Bayesian models in JAGS.

Requirements
 - `JAGS` >= 4.3.0
 - `R` >= 3.4.0
 - `jagsUI` R package >= 1.5.0

Suggestions:
 - `RStudio` >= 1.10
 - `rmarkdown` >= 1.10.0

### installing JAGS & jagsUI

Participants will need to install JAGS version 4.3.0, e.g. [from the JAGS sourceforge page](https://sourceforge.net/projects/mcmc-jags/files/)

We'll be calling JAGS form R using the `jagsUI` package, which can be installed from within R with the following command:

```r
install.packages("jagsUI")
```

### using R notebooks

We have provided a number of R notebooks to run models. R notebooks let you mix comments, code, and outputs (including figures) in a single document. [Here's a guide](https://bookdown.org/yihui/rmarkdown/notebook.html) in case you haven't used an R notebook before.

To use R notebooks, you'll need to have the `rmarkdown` package installed, and use RStudio as your interface to R.

You can install `rmarkdown` from within R with the following command:

```r
install.packages("rmarkdown")
```

You can install the latest RStudio for your system from [here](https://www.rstudio.com/products/rstudio/download/#download) 

If you can't or don't want to use RStudio, that's fine too. You can just edit the R markdown files to comment out the text and treat them like an R script. But it's less fun.

