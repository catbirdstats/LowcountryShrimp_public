# size distribution, colored by month, faceted by species
beeswarm_sizeDistn <- function(df, x, y, month, survey){
    # all options except df need to be fed in as character strings
    
    ylab <- y
    
    toplo <- df
    toplo$x <- df[[which(names(df) == x)]]
    toplo$y <- df[[which(names(df) == y)]]
    toplo$month <- df[[which(names(df) == month)]]
    
    
    ggplot(toplo) +
        geom_beeswarm(aes(x = x,
                          y = y,
                          col = month),
                      alpha = 0.5) +
        facet_wrap(~x, scales = "free_x") +
        khroma::scale_color_YlOrBr() +
        labs(title = paste("Size distribution by species in", survey),
             x = "Species",
             y = ylab,
             col = "Month")
    
}


beeswarm_abundByMonth <- function(df, y, month, facet, survey){
    # all options except df need to be fed in as character strings
    
    ylab <- y
    
    toplo <- df
    toplo$y <- df[[which(names(df) == y)]]
    toplo$month <- df[[which(names(df) == month)]]
    toplo$facet <- df[[which(names(df) == facet)]]
    
    ggplot(toplo) +
        geom_beeswarm(aes(x = factor(month),
                          y = y,
                          fill = month)) +
        facet_wrap(~facet, ncol = 1) +
        scale_y_log10() +
        khroma::scale_fill_YlOrBr() +
        theme(legend.position = "none") +
        labs(title = paste("Abundance by time of year in", survey),
             x = "Month",
             y = ylab)
}



