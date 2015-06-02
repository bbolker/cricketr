##' plot resource tables
##' @method plot restab
##' @importFrom ggplot2 ggplot aes geom_line labs
##' @importFrom lattice levelplot
##' @importFrom rgl persp3d
##' @param x resource table
##' @param type plot type
##' @param xlab
##' @param ylab
##' @param line_mapping mapping aesthetics for lines
##' @param dots (unused at present; for method consistency)
##' @export

plot.restab <- function(x,
                        type=c("heatmap","levelplot","gg_lines","gg_tiles",
                               "rgl"),
                        xlab="Overs remaining",
                        ylab="Wickets lost",
                        line_mapping=NULL,
                        factor_wicket=FALSE, ...
                        ) {
    type <- match.arg(type)
    tt <- tab.to.df(x)
    if (factor_wicket) tt <- mutate(tt,wicket=factor(wicket)) 
    switch(type,
           heatmap=heatmap(x,Rowv=NA,Colv=NA),
           gg_lines=ggplot(tt,
                        aes(over,rsc_avail_pct,
                            colour=wicket,group=wicket))+
                            geom_line(line_mapping)+
                            labs(x=xlab,y=ylab),
           gg_tiles=ggplot(tt,
                        aes(over,wicket,fill=runs))+
                            geom_tile(line_mapping)+
                            labs(x=xlab,y=ylab),
           levelplot=levelplot(unclass(x),xlab=xlab,ylab=ylab,
                        scales=list(x=list(rot=90)),asp="fill"),
           rgl=persp3d(x,xlab=xlab,ylab=ylab))
}

##' rearrange resource table to data frame
##' @param x resource table \emph{or} named list of resource tables
##' @importFrom dplyr %>% mutate add_rownames
##' @importFrom tidyr gather
##' @export
tab.to.df <- function(x) {
    if (is.list(x)) {
        xx <- lapply(x,tab.to.df)
        xx <- mapply(function(x,n) transform(x,m=n),xx,names(x),
                     SIMPLIFY=FALSE)
        return(do.call(rbind,xx))
    }
    x2 <- x %>% data.frame(check.names=FALSE) %>%
        add_rownames("over") %>%
            mutate(over=as.numeric(as.character(over))) %>%
                gather(wicket,rsc_avail_pct,-over,convert=TRUE)
    return(x2)
}

