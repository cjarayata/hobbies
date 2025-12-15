library(tidyverse)
library(openxlsx)

# spare randomizer

# define the vectors
pins <- c("10 pin", "6 pin",
            "3/9 pin", "1/5 pin", "2/8 pin",
            "4 pin", "7 pin")

# CJs number line
# feet <- c(-14, -13, -11, -8, -5, -2, 1, 3, 4)

# # spare system V4 - actual boards
# feet_real <- c(34, 32, 31, 29, 27, 22, 19, 17, 16)
# 
# # spare system V4
# slide <- c(30, 30, 28, 26, 27, 20, 17, 17, 16)

# spare system v5 - april 19, 2025
# feet_real <- c(37, 34, 32, 28, 22, 21, 19)

weights <- c(.25, rep((.5/5), 5), .25)

spare_chart <- data.frame(pins,
                          # feet_real,
                          # slide,
                          weights, actual = NA_character_, `make/miss` = NA_character_)

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

write.xlsx(list_of_datasets, "spare_gauntlet_v6.xlsx")

# skills randomizer

# axis
axis_rotation <- c("max rotation", "regular rotation", "low rotation")

# speed
speed <- c("regular speed", "slower speed")

# loft
loft <- c("regular laydown", "lofted")

library("tidyverse")
all_permutations <- crossing(axis_rotation, speed, loft)

slice_sample(all_permutations, n = 6)
