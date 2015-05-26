##' Calculate summary statistics (bias, variance, RMSE, kappa) based on a resource table, at a specified stopping point (number of overs).  Uses first inning
##' @param stop_overs number of overs to check
##' @param rtab_full resource table
##' @param max_overs max number of overs per game
##' @param fulldata full data frame (typically over50.data)
##' @importFrom dplyr ungroup
##' @export
gof <- function(stop_overs,rtab_full=res_tab_R_50,
                max_overs=50,
                max_wickets=9,
                fulldata) {
    T20I <- team <- wicket.over <- innings <- bias <- totruns.x <-
        totruns.y <- runs.y <- rsc_avail_pct <- final_rsc_avail <-
            runs_over_stop <- exp_runs <- ID <- NULL
    dd <- filter(fulldata,innings==1) %>%
        select(-c(T20I,team,wicket.over,innings))  ## don't really need this
    ## Data from stoppage over (one per game)
    over.stop <- filter(dd,over==stop_overs)
    ## Final runs (one per game, for games that are relevant)
    final.inn <- dd %>% filter(ID %in% over.stop$ID) %>%
        group_by(ID) %>%
            filter(over==max(over))
    ## Merge resource table with resource table.  
    ## Keep only rows corresponding to (max_overs-stop_overs) REMAINING overs
    rtab_df <- rename(tab.to.df(rtab_full),wickets=wicket)
    rtab_df_stop <- rtab_df %>%
        filter(over==max_overs-stop_overs) %>% select(-over)
    ## merge wickets rem at over x with resource table
    over.stop.2 <- over.stop %>%
        ## all wickets lost, no entry in rtab
        mutate(wickets=pmin(wickets,max_wickets))  %>% 
            left_join(rtab_df_stop,by=c("wickets")) %>%
                select(ID,totruns,rsc_avail_pct=runs.y)
    ## at this point we have resources available for each game, merged with
    ##  actual runs, but we're still assuming full resources were used in every game ...
    ## In case if less than 50 overs are played
    ## Compute fraction of resources actually used
    rtab_df_final <- rtab_df %>%
        filter(wickets==max_wickets) %>% select(-wickets)
    over_tab <- final.inn %>% group_by(ID) %>%
        ## wickets<9 means game stopped early, so use obs rather than max_overs
        mutate(over=ifelse(wickets<max_wickets,max_overs-over,0)) %>%
            left_join(rtab_df_final,by="over") %>%
                select(ID,rsc_avail_pct=runs.y) %>%
                    rename(final_rsc_avail=rsc_avail_pct)
    comb_tab <- full_join(over.stop.2,over_tab,by="ID") %>%
        left_join(final.inn,by="ID") %>%
            select(ID,runs_over_stop=totruns.x,rsc_avail_pct,final_rsc_avail,
                   totruns=totruns.y) %>%
                mutate(exp_runs=runs_over_stop/(100-rsc_avail_pct)*(100-final_rsc_avail),
                       resid=exp_runs-totruns)
    comb_sum <- comb_tab %>% ungroup %>%
        summarise(bias=mean(resid,na.rm=TRUE),
                  var=var(resid,na.rm=TRUE),
                  rmse=sqrt(bias^2+var))
    ## TODO: add kappa?
    return(comb_sum)
}

