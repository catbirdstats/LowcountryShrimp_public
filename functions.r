# definitions ----
# column selection order ----
# not really functions; objects that I want in the environment
sel_order_monthly <- c("survey", "species", "life_stage", 
                       "year", "month", "shrimp_year",
                       "abundance_measure", "abundance")
sel_order_annual <- c("survey", "species", "life_stage", 
                      "shrimp_year",
                      "abundance_measure", "abundance",
                      "sqrt_abundance")
sel_order_annual_extra <- c("survey", "species", "life_stage", 
                            "shrimp_year",
                            "abundance_measure", 
                            "abundance", "sqrt_abundance",
                            "abundance_sum", "sqrt_abundance_sum")

# common time period definition ----
# for trimming files
common_shrimpYears <- seq(1989, 2017)


# life stage ordering ----
ls_order_brn <- c("postlarval",
                  "juvenile",
                  "subadult",
                  "adult",
                  "adult.landings")

ls_order_wht <- c("postlarval",
                  "juvenile",
                  "subadult",
                  "adult.nonspawning",
                  "adult.spawning",
                  "adult.nonspawning.landings",
                  "adult.spawning.landings")


# size distribution, colored by month, faceted by species ----
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


# function to add cohort and adult.year  ----
add_cohorts <- function(df, lifestage){
    # df must be a data frame containing columns for species, year, and month
    # lifestage must be a character, "postlarvae", "juvenile", "subadult"
    
    #    for brown shrimp, shrimp_year is the year they're expected to show up in the fishery
    #    for white shrimp, it's the year they live most of their lives, but
    #         adults can bleed over into the next year
    
    if(lifestage == "postlarvae"){
        df <- df |> 
            dplyr::mutate(shrimp_year = case_when(species == "brown" & month <= 5 ~ year,
                                                  species == "brown" ~ 9999,
                                                  species == "white" ~ year,
                                                  .default = NA_integer_))
        return(df)
    } else if(lifestage == "juvenile"){
        df <- df |> 
            dplyr::mutate(shrimp_year = case_when(species == "brown" & month <= 8 ~ year,
                                                  species == "brown" & month >= 9 ~ year + 1,
                                                  species == "white" ~ year,
                                                  .default = NA_integer_))
        return(df)
    } else if(lifestage == "subadult"){
        df <- df |> 
            dplyr::mutate(shrimp_year = case_when(species == "brown" & month <= 8 ~ year,
                                                  species == "brown" ~ year + 1,
                                                  species == "white" & month <= 6 ~ year - 1,
                                                  species == "white" & month >= 7 ~ year,
                                                  .default = NA_integer_))
        return(df)
    } else if(lifestage == "adult"){
        df <- df |> 
            dplyr::mutate(shrimp_year = case_when(species == "brown" ~ year,
                                                  species == "white" & month <= 6 ~ year - 1,
                                                  species == "white" & month >= 7 ~ year,
                                                  .default = NA_integer_))
        return(df)
    } else {
        message("lifestage must be one of postlarvae, juvenile, subadult, adult")
    }
}

# if desired, to check the function:
# source(here::here("R", "tests.R"))

#
# function to calculate consec days ----
consec_stats <- function(data, threshold){
    # data is a vector of temperature values
    # threshold is a numeric threshold
    
    # make the vector of true/falses - 
    # is each temperature below the threshold
    x <- data < threshold
    
    # use rle to count runs of trues and falses
    rle_out <- rle(x)
    rle_df <- data.frame(value = rle_out$values,
                         n_consec = rle_out$lengths)
    
    # if there weren't any days below the threshold, return 0s
    if(!(TRUE %in% rle_df$value)){
        summ_df <- data.frame(threshold = threshold,
                              totalDays = 0,
                              totalSpells = 0,
                              longestSpell = 0)
        # otherwise calculate the summary stats
    } else {
        summ_df <- rle_df |> 
            filter(value == TRUE) |> 
            summarize(threshold = threshold,
                      totalDays = sum(n_consec),
                      totalSpells = sum(n_consec >= 7),
                      longestSpell = max(n_consec))
    }
    
    summ_df
}

# plotly ----
# deal with plotly: interactive in html; static in pdf

plotly_out <- function(p, legend_pos = "bottom") {

    if (interactive() || knitr::opts_knit$get("rmarkdown.pandoc.to") == "html"){
        # Map positions to Plotly layout settings
        legend_settings <- switch(
            legend_pos,
            bottom = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2),
            top    = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1),
            left   = list(orientation = "v", x = -0.1, xanchor = "right", y = 0.5, yanchor = "middle"),
            right  = list(orientation = "v", x = 1.05, xanchor = "left", y = 0.5, yanchor = "middle"),
            default = NULL,   # use Plotly’s default
            stop("legend_pos must be one of: 'bottom', 'top', 'left', 'right', 'default'")
        )
        
        if (is.null(legend_settings)) {
            ggplotly(p)
        } else {
            ggplotly(p) %>% layout(legend = legend_settings)
        }
        
    } else {
        # PDF/LaTeX just uses the ggplot legend settings
        p
    }
}

# correlation plot function ----
lowerFn <- function(data, mapping, method = "lm", ...) {
    p <- ggplot(data = data, mapping = mapping) +
        geom_point(colour = "navy", alpha = 0.8) +
        geom_smooth(method = method, color = "darkorange", 
                    se = FALSE, linewidth = 0.8, ...)
    p
}
