# set up

# writing a script to read in pinpal data and use python 'lane murmur'
library(tidyverse)
library(janitor)
library(DBI)
library(reticulate)

# pacman::p_load(tidyverse, janitor, DBI)

# to do
## SPARES - calculating single, makeables, splits percentages
# figure out what leaves are
# figure out how spares are differentiated
## general scoring
# remove the "fake" 12th frame, unless it's a strike


# mydb <- dbConnect(RSQLite::SQLite(), "bowling/Backup_2024_1214.pinpal")
# 
# as.data.frame(dbListTables(mydb))
# 
# dbGetQuery(mydb, 'SELECT * from league WHERE "_id" = 10')
# 
# dbDisconnect(mydb)
# 

# source_python("bowling/pinpal.py")

getDatabaseTables <- function(dbname="bowling/Backup_2024_1214.pinpal", tableName=NULL){
  library("RSQLite")
  con <- dbConnect(drv=RSQLite::SQLite(), dbname=dbname) # connect to db
  tables <- dbListTables(con) # list all table names

  if (is.null(tableName)){
    # get all tables
    lDataFrames <- map(tables, ~{ dbGetQuery(conn=con, statement=paste("SELECT * FROM '", .x, "'", sep="")) })
    # name tables
    names(lDataFrames) <- tables
    return (lDataFrames)
  }
  else{
    # get specific table
    return(dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tableName, "'", sep="")))
  }
  dbDisconnect(con)
}

# get all tables
lDataFrames <- getDatabaseTables(dbname="bowling/Backup_2024_1214.pinpal")

purrr::iwalk(
  .x = lDataFrames,
  .f = function(x, y) {
    x <- as.data.frame(x)
    y <- paste0('dataframe', y)
    assign(y, x, envir = globalenv())
  }
)

# get specific table
# df <- getDatabaseTables(dbname="YOURSQLITEFILE", tableName="YOURTABLE")


# look at normandy mens to uncover the cold hard truth
dataframeweek <- dataframeweek %>% 
        mutate(date_corrected = as_date(as_datetime(date/1000))) # correct the milliseconds

normandy_mens <- dataframeframe %>% 
        filter(leagueFk == 10) %>% 
        mutate(deliveries = case_when(flags == 0 ~ 0,
                                      flags == 193 ~ 1,
                                      flags == 195 ~ 2),
               frameNum = frameNum + 1,
               was_strike = case_when(flags == 193 ~ TRUE,
                                      TRUE ~ FALSE)) %>% 
        left_join(dataframeweek, by = c('weekFk' ='_id'))

# look at only the last week to try to make sense of it
last_week <- normandy_mens %>% 
        filter(date_corrected == max(date_corrected),
               gameFk %in% c(237, 238, 239))

last_week_simple <- last_week %>% 
        select('_id', date_corrected, gameFk, frameNum, pins, scores, flags, deliveries, was_strike) %>% 
        mutate(was_open = case_when(scores < 165 ~ TRUE,
                                    TRUE ~ FALSE))

# start sketching out a codebook for leaves ("pins" column)
leaves <- c(512 = "10", # made 10
            64 = "7", # made 7
            544 = "6-10", # made 6-10
            16 = "5", # made 5
            66144, "6-7-10", # whiffed but got 6-10
            262400, "9", # whiffed 9
            623200, "6-7-10", # whiffed
            515 = "1-2-10", # made
            65600 = "7" # whiffed 7
            )

# note that in gameFk 237, 12th frame is actually marked as 11, because 10 encapsulates the 10th and 11th

# these seem to be "scores" codes for open frames
open_frames <- c(119, 153, 134, 144)

## play around with some analyses

# what's my average?
blah <- dataframegame %>% 
        filter(leagueFk == 10) %>% pull(score) %>% mean()

# average over time (monthly)

# strike # - do the math and replicate/validate with what pinpal spits out

# single spare % over time
## needs full codebook of all single pin leaves (10 x 2 possibilities = 20)

# multi %
## needs full codebook of all mutli makes (a ton, so only code makes)

# split % - just needs a flag, may already exist

# open % - may just be a flag

## try to run an anova on scores per game
check <- dataframegame %>% 
        filter(leagueFk == 10) %>% 
        group_by(weekFk) %>% 
        mutate(game_number = 1:n()) %>% 
        ungroup()

# not significant; will need to run with the final league numbers
game_number_anova <- aov(score ~ game_number, check)
summary(game_number_anova)

means <- check %>% 
        group_by(game_number) %>% 
        summarise(mean = mean(score))

library(ggplot2)

check %>% ggplot(aes(x = factor(game_number), y = score, fill = game_number)) +
        geom_boxplot() 


## QC section - this will require hand-entered data

# missed headpins per pattern
## will require a 'sport' 'challenge' etc. flag, probably from dataframepattern

# number of frames to double

# max non-strike frames in a row
