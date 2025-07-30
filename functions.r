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
            dplyr::mutate(born_year = case_when(species == "brown" & month <= 5 ~ paste0(year, ", early"),
                                                species == "brown" & month >= 6 ~ paste0(year, ", late"),
                                                species == "white" & month <= 4 ~ paste0(year, ", early"),
                                                species == "white" & month <= 10 ~ paste0(year, ", mid"),
                                                species == "white" & month > 10 ~ paste0(year, ", late"),
                                                .default = NA_character_),
                          shrimp_year = case_when(species == "brown" & month <= 5 ~ year,
                                                  species == "brown" ~ 9999,
                                                  species == "white" ~ year,
                                                  .default = NA_integer_))
        return(df)
    } else if(lifestage == "juvenile"){
        df <- df |> 
            dplyr::mutate(born_year = case_when(species == "brown" & month <= 3 ~ paste0(year-1, ", late"),
                                                species == "brown" & month <= 8 ~ paste0(year, ", early"),
                                                species == "brown" & month >= 9 ~ paste0(year, ", late"),
                                                species == "white" & month <= 5 ~ paste0(year-1, ", late"),
                                                species == "white" & month >= 6 ~ paste0(year, ", mid"),
                                                .default = NA_character_),
                          shrimp_year = case_when(species == "brown" & month <= 8 ~ year,
                                                  species == "brown" & month >= 9 ~ year + 1,
                                                  species == "white" ~ year,
                                                  .default = NA_integer_))
        return(df)
    } else if(lifestage == "subadult"){
        df <- df |> 
            dplyr::mutate(born_year = case_when(species == "brown" & month <= 5 ~ paste0(year-1, ", late"),
                                                species == "brown" & month <= 8 ~ paste0(year, ", early"),
                                                species == "brown" & month >= 9 ~ paste0(year, ", late"),
                                                species == "white" & month <= 7 ~ paste0(year-1, ", late"),
                                                species == "white" & month >= 8 ~ paste0(year, ", mid"),
                                                .default = NA_character_),
                          shrimp_year = case_when(species == "brown" & month <= 8 ~ year,
                                                  species == "brown" ~ year + 1,
                                                  species == "white" & month <= 6 ~ year - 1,
                                                  species == "white" & month >= 7 ~ year,
                                                  .default = NA_integer_))
        return(df)
    } else if(lifestage == "adult"){
        df <- df |> 
            dplyr::mutate(born_year = case_when(species == "brown" ~ as.character(year),
                                                species == "white" & month <= 7 ~ paste0(year-1, ", late"),
                                                species == "white" & month >= 8 ~ paste0(year, ", mid"),
                                                .default = NA_character_),
                          shrimp_year = case_when(species == "brown" ~ year,
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
