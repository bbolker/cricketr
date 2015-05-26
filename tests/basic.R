library("cricketr")
library("dplyr")
library("ggplot2"); theme_set(theme_bw())

dd <- filter(over50.data,innings==1)
dd2 <- filter(over50.data,innings==2)

rr0 <- get.rtab(dd)

plot(rr0,type="rgl")


plot(rr0,type="gg_lines",factor_wicket=TRUE)
## calculate premature stopping times (unfinished)
dd.last <- over50.data %>% group_by(ID,innings) %>%
        filter(totruns==last(totruns)) %>%
            select(ID,wickets,innings,over,totruns) %>%
                arrange(ID)
## regular ending if
##    same numbers of overs in both innings OR
##    all wickets in inning 2 lost (wickets>=9) OR

rr <- get.rtab(dd)   ## raw resource table
nn <- get.ntab(dd)   ## counts per (wicket,over)
plot(rr,type="levelplot")  ## Figure 2.1 from thesis
plot(rr,type="heatmap")
plot(rr,type="gg_lines")
dl <- DLtab(DL.par,get.tot(dd))  ## compute DL from parameters
plot(dl,type="levelplot")

## compute weighted RMSE for DL parameters from resource tab
objfun(DL.par,rr,weights=nn,tot.runs=get.tot(dd))

optim(fn=objfun,par=DL.par,
      restab=rr,
      tot.runs=get.tot(dd),
      weights=nn,
      control=list(parscale=DL.par),
      method="L-BFGS-B")

library("bbmle")
objfun <- function (p, restab, weights = 1, tot.runs) { 
    weights <- rep(weights, length.out = length(restab))
    predtab <- DLtab(p, tot.runs)
    s <- !is.na(restab) & !is.na(weights) & weights > 0
    -sum(weights[s] * cricketr:::dnorm2(restab[s], predtab[s], log = TRUE))/sum(weights[s])
}

parnames(objfun) <- names(DL.par)
m1 <- mle2(minuslogl=objfun,start=DL.par,
     vecpar=TRUE,
     data=list(weights=nn,
               tot.runs=get.tot(dd),
               restab=rr),
     method="L-BFGS-B",
     control=list(parscale=DL.par,maxit=1e5))

if (FALSE) {
pp <- profile(m1,debug=TRUE)
pp.b <- profile(m1,which="b")
plot(pp.b,show.points=TRUE)
confint(pp.b)
## ???
}

cc <- confint(m1,method="quad")
mtab <- rbind(
    data.frame(type="newest",
               par=names(coef(m1)),
               est=coef(m1),
               lwr=cc[,1],
               upr=cc[,2]),
    data.frame(type="DL",
               par=names(DL.par),
               est=DL.par,
               lwr=NA,
               upr=NA))
mtab <- transform(mtab,
                  ptype=ifelse(par=="b","b",
                               ifelse(par=="Z0","Z0","F")))
ggplot(mtab,aes(x=par,y=est,ymin=lwr,ymax=upr,colour=type))+
    geom_pointrange()+facet_wrap(~ptype,scale="free")                             

confint(m1)

DL.opt1 <- optfun(rr,nn,get.tot(dd))
plot(DLtab(DL.opt1$par,get.tot(dd)),type="gg_lines")

library("numDeriv")
hh <- hessian(objfun,DL.opt1$par,restab=rr)
sqrt(diag(solve(hh)))
