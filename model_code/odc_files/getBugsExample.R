# downloaded from: http://www.stat.cmu.edu/~hseltman/rube/rube0.2-16/R/getBugsExample.R
# following this SO comment: https://stackoverflow.com/questions/10169575/how-to-open-winbugs-odc-file-in-r#comment13048777_10169575


# Read models, data, and intialization out of WinBUGS .odc example files
#
# (This is a horrible hack, but it works.)
#
# If ename' is not found exactly, a partial match is returned with a warning.
# 'repair' is a matrix with column triplets: model#, line#, character# to split the line
#
getBugsExample = function(ename, repair=NULL, 
  bugs.directory = ifelse(Sys.getenv("BUGSDIR")=="", 
                     "c:/Program Files/WinBUGS14/Examples",
                     paste(Sys.getenv("BUGSDIR"),"/Examples",sep=""))) {
  if (!require(stringr)) stop("please install package stringr")

  if (!is.null(repair)) {
    if (length(repair)%%3!=0) stop(gettextf("bad repair: length must be a multiple of 3"))
    if (is.null(dim(repair))) repair = matrix(repair, ncol=3, byrow=T)
    if (ncol(repair)!=3) stop(gettextf("bad repair: need three columns"))
  }
  
  # Get mixed ascii/binary data from odc file
  fname <- file.path(bugs.directory, ifelse(length(grep("[.]odc$",ename)),ename,paste(ename,".odc",sep="")))
  if (!file.exists(fname)) {
    tmp <- file.info(bugs.directory)$isdir
    if (is.na(tmp)) stop("directory ", bugs.directory, " not found")
    if (tmp==FALSE) stop(bugs.directory, " is not a directory")
    tmp <- list.files(bugs.directory)
    loc <- grep(ename, tmp, ignore.case=TRUE)
    if (length(loc)==0) stop(gettextf("can't find '%s'", fname))
    fname <- file.path(bugs.directory, tmp[loc[1]])
    warning(gettextf("could not find '%s'; used '%s' instead",
            ename, substring(tmp[loc[1]], 1, nchar(tmp[loc][1])-4)))
  }
  fsize <- file.info(fname)$size
  text <- suppressWarnings(readBin(fname, "character", fsize))
  text <- strsplit(paste(text,collapse="\r"),"\r")[[1]]
  text <- text[nchar(text)>0]
  text <- gsub("^ +","",text)
  text <- text[nchar(text)>0]

  # Get model(s) from Bugs Example odc file
  modelLoc <- grep("model",text)
  if (length(modelLoc)<1) stop("model statement not found")
  model <- text[modelLoc[1]:length(text)]
  together <- grep("model[[:space:]]*\\{",model)[1]
  if (!is.na(together)) {
    m1 <- grep("model[[:space:]]*\\{",model)
  } else {
    leftBrace <- as.numeric(sapply(model,function(x)length(grep("\\{",x)>0)))
    # Note: "model" sometimes appears in comments
    m1 <- intersect(grep("model$",gsub("[[:space:]]","",model)), which(leftBrace==1)-1)
  }
  if (length(m1)==0) stop("no model found")
  m1 <- m1[1]
  rtn <- NULL
  numModel <- 1 
  while (TRUE) {
    model <- model[m1:length(model)]
    leftBrace <- as.numeric(sapply(model,function(x)length(grep("\\{",x)>0)))
    rightBrace <- as.numeric(sapply(model,function(x)length(grep("\\}",x)>0)))
    braces <- as.numeric(cumsum(leftBrace-rightBrace))
    m2 <- which(braces==0)[2-braces[1]]
    this <- model[1:m2]
    loc <- str_locate(this[1], "model")
    this[1] <- substring(this[1], loc[1,1])
    this <- gsub("\\t","  ",this)
    eb <- length(this) # kill everything after the "end brace"
    this[eb] <- substring(this[eb], 1, rev(str_locate(this[eb],"}")[,1])[1])
    this <- gsub("[[:cntrl:]]", "", this)

    lenThis <- length(this)
    # Specific errors from Inhalers Ordered Logit
    if (lenThis>=11 && this[11]=="      group[i] <- 2 for (t in 1 : T) { response[i, t] <- pattern[1, t] }")
      this[11] <- "      group[i] <- 2; for (t in 1 : T) { response[i, t] <- pattern[1, t] }"
    if (lenThis>=15 && this[15]=="        group[i] <- 1 for (t in 1 : T) { response[i, t] <- pattern[k, t] }")
      this[15] <- "        group[i] <- 1; for (t in 1 : T) { response[i, t] <- pattern[k, t] }"
    if (lenThis>=18 && this[18]=="        group[i] <- 2 for (t in 1 : T) { response[i, t] <- pattern[k, t] }")
      this[18] <- "        group[i] <- 2; for (t in 1 : T) { response[i, t] <- pattern[k, t] }"
    # Specific errors from Endometrial Cancer Binomial vs. Multinomial vs. Poisson
    if (lenThis>=4 && this[4]=="    for (i in 1:I){ Y[i,1] <- 1  Y[i,2] <- 0}")
      this[4] <- "    for (i in 1:I){ Y[i,1] <- 1;  Y[i,2] <- 0}"
    if (lenThis>=6 && this[6]=="    for (i in 1:n10){ est[i,1] <- 1   est[i,2] <- 0}")
      this[6] <- "    for (i in 1:n10){ est[i,1] <- 1;   est[i,2] <- 0}"
    if (lenThis>=8 && this[8]=="    for (i in (n10+1):(n10+n01)){  est[i,1] <- 0     est[i,2] <- 1}")
      this[8] <- "for (i in (n10+1):(n10+n01)){  est[i,1] <- 0;     est[i,2] <- 1}"
    if (lenThis>=10 && this[10]=="    for (i in (n10+n01+1):(n10+n01+n11)){ est[i,1] <- 1  est[i,2] <- 1}")
      this[10] <- "    for (i in (n10+n01+1):(n10+n01+n11)){ est[i,1] <- 1;  est[i,2] <- 1}"
    if (lenThis>=12 && this[12]=="    for (i in (n10+n01+n11+1):I){ est[i,1] <- 0  est[i,2] <- 0}")
      this[12] <- "    for (i in (n10+n01+n11+1):I){ est[i,1] <- 0;  est[i,2] <- 0}"
    # Specific error from Hearts Zero Inflation Poisson Mixture
    if (lenThis>=15 && this[15]=="    logit(theta) <- delta delta ~ dnorm(0, 1.0E-4)")
      this[15] <- "    logit(theta) <- delta; delta ~ dnorm(0, 1.0E-4)"
    # Specific errors from Epilepsy Poisson Random Intercept models 1 and 2
    if (lenThis>=16 && this[16]=="      log.Base4[j] <- log(Base[j] / 4) log.Age[j] <- log(Age[j])")
      this[16] <- "      log.Base4[j] <- log(Base[j] / 4); log.Age[j] <- log(Age[j])"
    if (lenThis>=17 && this[17]=="      log.Base4[j] <- log(Base[j] / 4) log.Age[j] <- log(Age[j])")
      this[17] <- "      log.Base4[j] <- log(Base[j] / 4); log.Age[j] <- log(Age[j])"
    # Specific error from Dogs Log-linear Model
    if (lenThis>=4 && this[4]=="        xa[i, 1] <- 0; xs[i, 1] <- 0 p[i, 1] <- 0 ")
      this[4] <- "        xa[i, 1] <- 0; xs[i, 1] <- 0; p[i, 1] <- 0 "
    # Specific error from Equivalence Gaussian Crossover with Random Intercept
    if (lenThis>=13 && this[13]=="      tau1 ~ dgamma(0.001, 0.001) sigma1 <- 1 / sqrt(tau1)")
      this[13] <- "      tau1 ~ dgamma(0.001, 0.001); sigma1 <- 1 / sqrt(tau1)"
    # Specific error from Eyes Gaussian Mixture
    if (lenThis>=12 && this[12]=="    tau ~ dgamma(0.001, 0.001) sigma <- 1 / sqrt(tau)")
      this[12] <- "    tau ~ dgamma(0.001, 0.001); sigma <- 1 / sqrt(tau)"
    # Specific error from Line Simple Regression
    if (lenThis>=7 && this[7]=="    tau ~ dgamma(0.001,0.001) sigma <- 1 / sqrt(tau)")
      this[7] <- "    tau ~ dgamma(0.001,0.001); sigma <- 1 / sqrt(tau)"
    # Specific error from Leukemia Frailty Pairs
    if (lenThis>=37 && this[37]=="    c <- 0.001   r <- 0.1 ")
      this[37] <- "    c <- 0.001; r <- 0.1"
    # Specific errors from Schools Gaussian Hierarchy
    if (lenThis>=9 && this[6]=="                   + alpha[school[p], 3] * VR[p, 1] + beta[1] * LRT2[p] ")
      this[5:9] <- paste(this[5:9],"\\")
    # Specific errors from Salmonella Extra-Poisson Variation
    if (lenThis>=7 && this[6]=="        log(mu[i , j]) <- alpha + beta * log(x[i] + 10) + ")
      this[6] <- paste(this[6],"\\")
    # Specific error from Kidney Weibull Regression Random Effects
    if (lenThis>=9 && this[9]=="            + beta.dis[disease[i]] + b[i];")
      this[7:8] <- paste(this[7:8],"\\")
    # Specific errors from Epilepsy Poisson Random Intercept
    if (lenThis>=9 && this[9]=="                    + alpha.V4  * (V4[k] - V4.bar) ")
      this[5:9] <- paste(this[5:9],"\\")
    if (lenThis>=38 && this[38]=="    - alpha.BT * BT.bar - alpha.Age * log.Age.bar - alpha.V4 * V4.bar")
      this[37] <- paste(this[37],"\\")
    # Specific errors from Epilepsy Poisson Random Intercept -- model 2
    if (lenThis>=11 && this[9]=="                    + alpha.Age * (log.Age[j] - log.Age.bar)  ")
      this[5:10] <- paste(this[5:10],"\\")
    if (lenThis>=40 && this[38]=="    alpha0 <- a0 - a.Base * log.Base4.bar - a.Trt * Trt.bar ")
      this[38] <- paste(this[38],"\\")

    rtn <- c(rtn, list(this))
    names(rtn)[length(rtn)] <- paste("model",numModel,sep="")
    Len <- length(model)
    if (m2==Len) break
    model <- model[(m2+1): Len]
    leftBrace <- as.numeric(sapply(model,function(x)length(grep("\\{",x)>0)))
    m1 <- intersect(grep("model$",gsub("[[:space:]]","",model)), which(leftBrace==1)-1)
    if (length(m1)==0) break
    m1 <- m1[1]
    numModel <- numModel + 1
  }

  # Get list(s) from Bugs Example odc file
  listsLocs <- grep("list\\(",text)
  if (length(listsLocs)<1) stop("list statement(s) not found")
  lists <- text[listsLocs[1]:length(text)]
  # remove crap before "list"
  loc <- str_locate(lists, "list")
  toChange <- which(!is.na(loc[,1]))
  lists[toChange] <- substring(lists[toChange], loc[toChange,1])
  # remove random terminating crap
  toChange <- grep("[[:alpha:]]{1}[[:cntrl:]]{1}@[[:alpha:]]{1}$",lists)
  if (length(toChange)>0)
    lists[toChange] <- substring(lists[toChange],1,nchar(lists[toChange])-4)
  numList <- 1 
  while (TRUE) {
    leftParen <- as.numeric(sapply(lists,function(x)nrow(str_locate_all(x,"\\(")[[1]])))
    rightParen <- as.numeric(sapply(lists,function(x)nrow(str_locate_all(x,"\\)")[[1]])))
    parens <- as.numeric(cumsum(leftParen-rightParen))
    m2 <- which(parens<=0)[1]
    this <- paste(lists[1:m2], collapse=" ")
    # remove crap at the end of the line
    extra <- -parens[m2]
    pos <- str_locate_all(this, "\\)")[[1]]
    if (nrow(pos)<=extra) stop("can't parse list data")
    this <- gsub("[[:space:]]","",substring(this, 1, pos[nrow(pos)-extra,1]))
    # error from Pigs Pedigree Analysis
    if (substring(this,69)=="Ian=2Jane=3)")
      this <- paste(substring(this,1,73), ",Jane=3)")
    parsed <- try(eval(parse(text=this)), silent=TRUE)
    if (is(parsed,"try-error")) {
      # Remove dross from end of Cervix Errors-in-Vars and Schools Gaussian Hierarchy
      good <- c(LETTERS,letters,strsplit("+-0123456789.(),= \t","")[[1]])
      tmp <- sapply(lists[1:m2], function(x) {
                      ch <- strsplit(x, "")[[1]]
                      bad <- which(!ch%in%good)
                      if (length(bad)==0) return(0)
                      return(bad[1])
                    }, USE.NAMES=FALSE)
      if (any(tmp>0)) {
        lists[which(tmp>0)] <- substring(lists[which(tmp>0)], 1, tmp[tmp>0]-1)
        this <- paste(lists[1:m2], collapse=" ")
        this <- gsub("[[:space:]]","",substring(this, 1, pos[nrow(pos)-extra,1]))
        parsed <- try(eval(parse(text=this)), silent=TRUE)
      }
    }
    if (is(parsed,"try-error")) stop("cannot parse ", this, " -- may work if tried again!")
    # fix C vs. Fortran array coding
    arrDim <- sapply(parsed, function(x)ifelse(is.array(x),length(dim(x)),1))
    for (n in seq(along=arrDim)) {
      if (arrDim[n]>3) {
        warning("can't fix dim>3")
      } else if (arrDim[n]>2) {
        Dim <- dim(parsed[[n]])
        arr <- array(NA, Dim)
        dat <- as.numeric(parsed[[n]])
        index <- 1 
        for (i in 1:Dim[1]) {
          for (j in 1:Dim[2]) {
            for(k in 1:Dim[3]) {
              arr[i,j,k] <- dat[index]
              index <- index + 1
            }
          }
        }
        parsed[[n]] <- arr
      } else if (arrDim[n]==2) {
        parsed[[n]] <- matrix(as.numeric(parsed[[n]]),nrow(parsed[[n]]),byrow=TRUE)
      }
    }
    rtn <- c(rtn, list(parsed))
    # Guess data vs. inits
    if (numModel==1) {
      if (numList==1) {
        nam = "data1"
      } else {
        nam = paste("inits", numList-1, sep="")
      }
    } else {
      nam <- paste(ifelse(numList%%2,"data","inits"),(numList+1)%/%2,sep="")
    }
    names(rtn)[length(rtn)] <- nam
    Len <- length(lists)
    if (m2==Len) break
    lists <- lists[(m2+1): Len]
    listsLocs <- grep("list\\(",lists)
    if (length(listsLocs)<1) break
    lists <- lists[listsLocs[1]:length(lists)]
    numList <- numList + 1
  }

  # Allow user repair of common run-on error in WinBUGS example files
  if (!is.null(repair)) {
    modelPos <- which(substring(names(rtn),1,5)=="model")
    Nmod <- max(as.numeric(substring(names(rtn)[modelPos],6)))
    for (N in 1:Nmod) {
      repMat <- repair[repair[,1]==N, 2:3, drop=FALSE]
      if (nrow(repMat)>0) {
        modName <- paste("model",N,sep="")
        this <- rtn[[modName]]
        for (i in nrow(repMat):1) {
          line <- repMat[i,1]
          tmpL <- length(this)
          if (line<1 || line>tmpL) stop(gettextf("Bad repair line number(%d) in model %d", line, N))
          if (repMat[i,2]<2 || repMat[i,2]>nchar(this[line])-1) 
            stop(gettextf("Bad repair split position(%d) in model %d", repMat[i,2], N))
          newtxt <- c(substring(this[line], 1, repMat[i,2]),
                      substring(this[line], repMat[i,2]+1))
          tmp <- if (line>1) c(this[1:(line-1)], newtxt) else newtxt
          this <- if (line<tmpL) c(tmp, this[(line+1):tmpL]) else tmp
        }
        rtn[[modName]] <- this
      }
    }
  }


  
  return(rtn)
}


## Testing: (Note: I change the example file names on my system to be more informative.)
if (exists("testingODC")) {
  library(rube)
  jnk1 = getBugsExample("LsatRasch")
  print(lapply(jnk1,names))
  with(jnk1, rube(model1, data1, inits1))
  with(jnk1, rube(model1, data1, inits1, c("alpha","beta"), n.iter=100, n.chains=1))

  jnk2 = getBugsExample("AirLogisticRegErrsInVars")
  print(lapply(jnk2,names))
  with(jnk2, summary(rube(model1, data1, inits1)))
  with(jnk2, rube(model1, data1, inits1, c("X","theta"), n.iter=100, n.chains=1))
  with(jnk2, summary(rube(model2, data2, inits2)))
  with(jnk2, rube(model2, data2, inits2, c("X","theta","theta0"), n.iter=100, n.chains=1))

  jnk3 = getBugsExample("LeukCoxReg")
  with(jnk3, summary(rube(model1, data1, inits1)))
  with(jnk3, rube(model1, data1, inits1, c("dL0","beta"), n.iter=100))


  with(getBugsExample("LineSimpReg", repair=c(1,7,30)),summary(rube(model1,data1,inits1)))

  jnk4 = getBugsExample("AlligatorsMultinomLogit")
  with(jnk4, summary(rube(model1, data1, inits1)))
  jnk4B=with(jnk4, rube(model1, data1, inits1, "b", n.iter=5000, n.chains=1))
  print(jnk4B,digits=4)

  with(getBugsExample("Eyes", repair=c(1,12,30)),summary(rube(model1)))
}
