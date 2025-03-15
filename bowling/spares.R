library(tidyverse)
library(openxlsx)

# spare randomizer

# define the vectors
pins <- c("10", "6-10", "6",
            "3/9", "1/5", "2/8",
            "4", "4-7", "7")

pins <- glue::glue("'{pins}")

# CJs number line
feet <- c(-14, -13, -11, -8, -5, -2, 1, 3, 4)

# spare system V4 - actual boards
feet_real <- c(34, 32, 31, 29, 27, 22, 19, 17, 16)

# spare system V4
slide <- c(30, 30, 28, 26, 27, 20, 17, 17, 16)

weights <- c(.15, rep((.7/7), 7), .15)

spare_chart <- data.frame(pins, feet_real, slide, weights, actual = NA_character_, `make/miss` = NA_character_)

# make a gauntlet of random spares

make_spare_gauntlet <- function(spare_chart, reps = 10){
        df <- slice_sample(spare_chart, n = 10, replace = T, weight_by = weights) %>% 
        add_row(.after = 1) %>% 
        add_row(.after = 3) %>% 
        add_row(.after = 5) %>% 
        add_row(.after = 7) %>%
        add_row(.after = 9) %>% 
        add_row(.after = 11) %>% 
        add_row(.after = 13) %>% 
        add_row(.after = 15) %>% 
        add_row(.after = 17) %>% 
        add_row(.after = 19) %>% 
        select(-weights)
        
        return(df)
}

spare_1 <- make_spare_gauntlet(spare_chart)
spare_2 <- make_spare_gauntlet(spare_chart)
spare_3 <- make_spare_gauntlet(spare_chart)
spare_4 <- make_spare_gauntlet(spare_chart)

list_of_datasets <- list(spare_1, spare_2, spare_3, spare_4)

write.xlsx(list_of_datasets, "spare_gauntlet.xlsx")
