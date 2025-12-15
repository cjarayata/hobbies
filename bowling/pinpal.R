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
        mutate(date_corrected = as_date(as_datetime(date/1000)),
               bowling_month = floor_date(date_corrected, "month"))

normandy_mens <- dataframeframe %>% 
                filter(leagueFk == 10) %>% 
        mutate(deliveries = case_when(flags == 0 ~ 0,
                                      flags == 193 ~ 1,
                                      flags == 195 ~ 2),
               frameNum = frameNum + 1,
               was_strike = case_when((flags == 193 &
                                               scores == 170) ~ TRUE,
                                      TRUE ~ FALSE),
               was_open = case_when(scores < 165 ~ TRUE,
                                    TRUE ~ FALSE)) %>% 
        left_join(dataframeweek, by = c('weekFk' ='_id')) %>% # correct the milliseconds
        filter(deliveries != 0) %>% 
        select('_id', date_corrected, bowling_month, gameFk, frameNum, pins, scores, flags, deliveries, was_strike, was_open) %>% 
        # not sure why this needs a double mutate...
         mutate(was_open = case_when(frameNum == 12 ~ FALSE, # if 12th frame but not strike, don't count as open
                                    (frameNum == 11 & deliveries == 1) ~ FALSE, # if 10th frame spare, don't count 11th as open
                                    TRUE ~ as.logical(was_open)),
                count_towards_open = case_when(frameNum == 12 ~ FALSE,
                                               (frameNum == 11 & deliveries == 1) ~ FALSE,
                                               TRUE ~ TRUE))
        

# look at only the last week to try to make sense of it
last_week <- normandy_mens %>% 
        filter(date_corrected == max(date_corrected),
               gameFk %in% c(237, 238, 239))

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

# what's my overall average?
blah <- dataframegame %>% 
        filter(leagueFk == 10) %>% pull(score) %>% mean()

# average over time (monthly)
blah <- dataframegame %>% 
        filter(leagueFk == 10) %>% 
        left_join(dataframeweek, by = c('weekFk' ='_id')) %>% 
        group_by(bowling_month) %>% 
        summarise(mean(score)) %>% 
        ungroup()
        
# strike # - do the math and replicate/validate with what pinpal spits out
# strike percentage is accurate!
strike_check <- normandy_mens %>% 
        group_by(date_corrected) %>% 
        summarise(strike_pct = round(sum(was_strike) / n() * 100, 2)) %>% 
        ungroup()

ggplot(strike_check, aes(x = date_corrected, y = strike_pct)) +
        geom_line()

# weee
strike_check <- normandy_mens %>% 
        group_by(bowling_month) %>% 
        summarise(strike_pct = round(sum(was_strike) / n() * 100, 2)) %>% 
        ungroup()

# single spare % over time
## needs full codebook of all single pin leaves (10 x 2 possibilities = 20)

# multi %
## needs full codebook of all mutli makes (a ton, so only code makes)

# split % - just needs a flag, may already exist

# open % - may just be a flag
# isn't totally correct, my flag is wrong
# check 10/10
check <- normandy_mens %>% filter(date_corrected == "2024-10-10")

normandy_mens %>% 
        group_by(date_corrected) %>% 
        summarise(total_opens = sum(was_open),
                  total_frames = n(),
                  total_frames2 = sum(count_towards_open),
                  open_pct1 = round(total_opens / total_frames * 100, 2),
                  open_pct2 = round(total_opens / total_frames2 * 100, 2)) %>% 
        ungroup()

# check opens - the math is kind of weird but as long as i'm consisitent...
# 12/12 game 1: 2 / 11 (or is it 11?)
# game 2: 2 / 11
# game 3: 2 / 11
blah <- normandy_mens %>% 
        filter(gameFk %in% c(193:195))

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
