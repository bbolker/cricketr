
lastruns <- function(x) tail(x,1)[["totruns"]]
zero.na <- function(x) { x[is.na(x)] <- 0; return(x) }

## old, plyr-based versions (obsolete, I hope)
get.res.old <- function(sub,full,fun=mean) {
    totruns <- daply(sub,"ID",lastruns)
    finalruns <- daply(subset(full,ID %in% sub$ID),"ID",lastruns)
    100*fun(1-totruns/finalruns)
}
get.rtab.old <- function(dd,fun=mean,corner=100,edge=NA) {
    rtab <- daply(dd,c("over","wickets"),
                  get.res,full=dd,fun=fun)
    rtab <- rtab[50:1,]  ## flip in over direction
    rtab[1,] <- 0        ## no overs remaining -> no resources
    rtab <- rtab[,-11]   ## drop 10-wicket cases
    rtab <- rbind(rtab,c(corner,rep(edge,9)))
    dimnames(rtab) <- list(over=0:50,wickets=0:9)
    return(rtab)
}

##' compute resources for a specific set: new hotness (dplyr)
##'
##' FIXME: can we compute finalruns in advance?
##' 
##' @param sub  subset of data: e.g. single over/wicket combination
##' @param full full data
##' @param fun summarizing function
##' @param rmult scaling factor for table
##' @importFrom dplyr group_by summarise filter full_join
get.res <- function(sub,full,fun=mean,rmult=100) {
    ## FIXME: do we really need last() here? this should already be
    ## subsetted down to wicket/over/ID combo, so there should only
    ## be one value of totruns ???
    totruns <- sub %>% group_by(ID) %>% summarise(totruns=last(totruns))
    ## I wonder if filtering by key is faster if we do it after grouping?
    finalruns <- full %>% filter(ID %in% unique(sub$ID)) %>%
        group_by(ID) %>% summarise(finalruns=last(totruns))
    ## FIXME: confusingly, totruns = {ID,totruns}, finalruns = {ID,finalruns}
    r <- full_join(finalruns,totruns,by="ID") %>%
        summarise(res=rmult*fun(1-totruns/finalruns))
    return(r)
}

##' calculate average of final runs
##' @param dd data frame
##' @export
get.tot <- function(dd) {
    res <- dd %>% group_by(ID) %>% summarise(finalruns=last(totruns)) %>%
        ungroup() %>%  summarise(tot=mean(finalruns))
    return(res[[1]])  ## pull out of 1x1 data frame
}

##' calculate resource table
##' @param dd data frame
##' @param fun summarizing function
##' @param corner value for max wicket/over combination
##' @param edge value for impossible combinations
##' @importFrom dplyr do select group_by count
##' @importFrom tidyr spread
##' @export
get.rtab <- function(dd,fun=mean,corner=100,edge=NA) {
    if (identical(fun,count)) {
        rtab <- dd %>% count(over,wickets) %>%
            rename(res=n)
    } else {
        rtab <- dd %>% group_by(over,wickets) %>%
            do(get.res(.,full=dd,fun=fun))
    }
    ## how can we turn off the progress bar?
    rtab2 <- as.matrix(spread(rtab,wickets,res) %>% select(-over))  ## reshape
    rtab3 <- clean.rtab(rtab2,corner=corner,edge=edge)              ## clean up
    class(rtab3) <- c("restab","matrix")
    return(rtab3)
}

##' clean/reformat resource table
##' FIXME: don't hard-code max overs (wickets)
##' @param rtab resource table
##' @param corner
##' @param edge
##' @param flip
clean.rtab <- function(rtab,corner=100,edge=edge,flip=TRUE) {
    if (flip) rtab <- rtab[50:1,]  ## flip in over direction
    rtab[1,] <- 0        ## no overs remaining -> no resources
    rtab <- rtab[,-11]   ## drop 10-wicket cases
    rtab <- rbind(rtab,c(corner,rep(edge,9)))
    dimnames(rtab) <- list(over=0:50,wickets=0:9)
    return(rtab)
}

##' count number of games by overs/wickets
##' @param dd data
##' @export
get.ntab <- function(dd) {
    get.rtab(dd,fun=count,corner=length(unique(dd$ID)),edge=0)
}
    ## BMB: FIXME: should be merged with get.rtab
## get.ntab <- function(dd,corner=length(unique(dd$ID)),edge=0) {
##     count(dd,over,wickets)
##     ntab <- daply(dd,c("over","wickets"),nrow)
##     ntab <- zero.na(ntab)
##     ntab <- ntab[50:1,]  ## flip in over direction
##     ntab[1,] <- 0  ## AB: should we zero this row or not?
##     ntab <- ntab[,-11]
##     ntab <- rbind(ntab,c(corner,rep(edge,9)))
##     dimnames(ntab) <- list(over=0:50,wickets=0:9)
##     return(ntab)
## }

##' normal density with profiled sd
##' @param x data
##' @param mean
##' @param log return log-density?
##' @return probability density or log-density of a Normal distribution with the SD set to the (biased) sample SD
##' @export
dnorm2 <- function(x,mean,log=TRUE) {
  sd0 <- sqrt(sum((x-mean)^2,na.rm=TRUE)/length(x))
  dnorm(x,mean,sd=sd0,log=log)
} 

##' root-mean-square error, weighted by n
RMSE <- function(expected,actual,n) {
    MSE <- sum((expected-actual)^2,na.rm=TRUE)/n
    return(sqrt(MSE))
}

rmse.CI <- function(a,b,c) {
  est <- RMSE(b,c,length(c))
  RMSE2 <- function(r,w=1) sqrt((sum(w*r^2,na.rm=TRUE)/sum(w))/length(a))
  bootfun <- function(x) x[sample(length(x),replace=TRUE)]
  bdist <- replicate(500,RMSE2(bootfun(a)))
  quan <- quantile(bdist,c(0.025,0.975),na.rm=TRUE)
  quan <- as.data.frame(quan)
  c(est=est,lwr=quan[1,1],upr=quan[2,1]) # Dont know how else to do it
}

##' Duckworth-Lewis resource table
##' 
##' @param tot.runs avg number of runs across all games
##' @param max.over max number of overs
##' @export
DLtab <- function(p,tot.runs,max.over=50) {
    b <- p[1]
    Z0 <- p[2]
    Fvec <- c(1,p[-(1:2)])
    res <- outer(0:max.over, 
                 Fvec,
                 function(u,F) Z0*F*(1-exp(-b*u/F))/tot.runs*100)
    dimnames(res) <- list(over=0:max.over,wickets=0:9)
    class(res) <- c("restab","matrix")
    res
}

##' weighted least-squares
##' @param weights
##' @param restab resource table
##' @param p Duckworth-Lewis parameters
##' @export
objfun <- function(p,restab,weights=1,...) {
    weights <- rep(weights,length.out=length(restab))
    predtab <- DLtab(p,...)
    s <- !is.na(restab) & !is.na(weights) & weights>0
    -sum(weights[s]*dnorm2(restab[s],predtab[s],log=TRUE))/
        sum(weights[s])
    ## RMSE(DLtab2(p,...),restab,n=length(restab))
}


##' Find optimal D-L parameters
##' @param restab resource table
##' @param weights weights (typically counts)
##' @param tot.runs total runs for scaling
##' @param start starting parameters
##' @param control control list for \code{\link{optim}}
##' @param method optimization method
##' @export
optfun <- function(restab,weights,tot.runs,start=DL.par,
                   control=list(maxit=1e5,parscale=start),
                   method="BFGS") {
    ## FIXME: use mle2 ??
    return(optim(fn=objfun,
                 par=start,
                 restab=restab,
                 tot.runs=tot.runs,
                 weights=weights,
                 control=control,
                 method=method))
}

check.eq <- function(x,y) {
    max(abs(na.omit(c(x))-na.omit(c(y))))
}


##' run a single cross-validation 
##' @param dat data set: should include both innings for subset of games
##' @param dat.sub
##' @param samp sampling fraction
cross.valid <- function(dat,samp=0.8) {
    train.set <- sample(unique(dat$ID),
           size=round(samp*length(unique(dat$ID))),
           replace=FALSE)
    training <- subset(dat,ID %in% train.set)
    validate <- subset(dat,!ID %in% train.set)
    ## want to do most operations on innings-2 only
    inn2 <- filter(training,innings==2)
    restab2 <- get.rtab(inn2)
    ## FIXME: here we should further process resource tables
    ##    according to specified rule (D-L fit, isotonic, etc)
    ## * we may need to calculate weights etc. as well
    ## get final runs for inning1
    final.inn1 <- filter(training,innings==1) %>% group_by(ID) %>%
        summarise(totruns=last(totruns))
    ## combine relevant inning2 data with (1) resource table,
    ## (2) final-inning-1 runs
    ## FIXME: allow filter rule for whether we are evaluating
    ##  the rule for a specific subset?
    inn2_comb <- inn2 %>% rename(wicket=wickets) %>%
        select(ID,over,wicket,totruns) %>%
            inner_join(mutate(tab.to.df(restab2),
                              over=50-over),
                       by=c("over","wicket")) %>%
                           inner_join(rename(final.inn1,inn1.runs=totruns),
                                      by="ID")
    inn2_comb <- mutate(inn2_comb,
                        exp_runs=(totruns/(1-runs/100)))
    ## ID, over, wicket, totruns=current total in innings2
    ## ru1ns=resources
    ## inn1.runs=final innings-1 runs
    
    
    ## FIXME: need to
    ##  add prediction fun to arguments
    ##     -- i.e. how do we get from observed vs expected?
    ##  add options for RMSE vs kappa
    ##  add options for different resource

}
