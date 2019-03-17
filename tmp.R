reg_season_stats <- read.csv("data/datafiles/RegularSeasonDetailedResults.csv",
                      stringsAsFactors = FALSE) %>% 
  as_tibble()
tourney_stats <- read.csv("data/datafiles/NCAATourneyDetailedResults.csv",
                          stringsAsFactors = FALSE) %>% 
  as_tibble()
teams <- read.csv("data/DataFiles/Teams.csv", 
                  stringsAsFactors = FALSE) %>% 
  as_tibble()
results <- read.csv("data/DataFiles/NCAATourneyCompactResults.csv",
                    stringsAsFactors = FALSE) %>% 
  as_tibble()




tourney_seeds <- read.csv("data/DataFiles/NCAATourneySeeds.csv", 
                          stringsAsFactors = FALSE) %>% 
  as_tibble()
reg_stats_compact <- read.csv("data/DataFiles/RegularSeasonCompactResults.csv",
                                     stringsAsFactors = FALSE) %>% 
  as_tibble()
sample <- read.csv("data/SampleSubmissionStage1.csv", 
                   stringsAsFactors = FALSE) %>% 
  as_tibble()
tourney_stats_compact <- read.csv("data/DataFiles/NCAATourneyCompactResults.csv", 
                                  stringsAsFactors = FALSE) %>% 
  as_tibble()

massey <- read.csv("data/MasseyOrdinals.csv", stringsAsFactors = FALSE)

source("src/function/validate.R")

library(tidyverse)
library(gridExtra)
library(ggridges)
library(magrittr)


## calculate possessions per game statistic: field goal attempts + 0.475 x free throw attempts - offensive rebounds + turnovers

# regular season
reg_season_stats <- reg_season_stats %>%
  mutate(WPoss = WFGA + (WFTA * 0.47) + WTO - WOR,
         LPoss = LFGA + (LFTA * 0.47) + LTO - LOR)


# Tourney
tourney_stats <- tourney_stats %>%
  mutate(WPoss = WFGA + (WFTA * 0.47) + WTO - WOR,
         LPoss = LFGA + (LFTA * 0.47) + LTO - LOR)



# cite Max Philipp's kernal for the below: https://www.kaggle.com/maxphilipp/creating-a-tidy-dataset-for-starters-using-dplyr/code

#---------- tidy data for regular season totals ----------#

# First I will define a function that will take the detailed results data and reshape it so that it results in a dataframe where 
# each observation is a team for that season and their totals statistics for that season. It will also calculate the statistics allowed
# for the season. This function only requires one parameter to be passed to it: either the regular season or Tourney detailed data.

# IMPORTANT: #
# The data required for this function to work is either the regular season or tourney detailed stats, and the "teams" dataset

reshape_detailed_results <- function(detailed_dataset) {
  
  season_team_stats_tot <- rbind(
    detailed_dataset %>%
      select(Season, DayNum, TeamID=WTeamID, Score=WScore, OScore=LScore, WLoc, NumOT, Poss=WPoss, FGM=WFGM, FGA=WFGA, FGM3=WFGM3,  FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR,
             DR=WDR, Ast=WAst, TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF, OPoss=LPoss, OFGM=LFGM, OFGA=LFGA, OFGM3=LFGM3, OFGA3=LFGA3, OFTM=LFTM, OFTA=LFTA, O_OR=LOR, ODR=LDR,
             OAst=LAst, OTO=LTO, OStl=LStl, OBlk=LBlk, OPF=LPF) %>%
      mutate(Winner=1),
    
    detailed_dataset %>%
      select(Season, DayNum, TeamID=LTeamID, Score=LScore, OScore=WScore, WLoc, NumOT, Poss=LPoss, FGM=LFGM, FGA=LFGA, FGM3=LFGM3,  FGA3=LFGA3, FTM=LFTM, FTA=LFTA, OR=LOR,
             DR=LDR, Ast=LAst, TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF, OPoss=WPoss, OFGM=WFGM, OFGA=WFGA, OFGM3=WFGM3, OFGA3=WFGA3, OFTM=WFTM, OFTA=WFTA, O_OR=WOR, ODR=WDR,
             OAst=WAst, OTO=WTO, OStl=WStl, OBlk=WBlk, OPF=WPF) %>%
      mutate(Winner=0)) %>%
    left_join(teams, by= "TeamID") %>%
    group_by(Season, TeamID, TeamName) %>%
    summarise(GP = n(),
              wins = sum(Winner),
              TotPoints = sum(Score),
              TotPointsAllow = sum(OScore),
              NumOT = sum(NumOT),
              TotPos = sum(Poss),
              TotFGM = sum(FGM),
              TotFGA = sum(FGA),
              TotFGM3 = sum(FGM3),
              TotFGA3 = sum(FGA3),
              TotFTM = sum(FTM),
              TotFTA = sum(FTA),
              TotOR = sum(OR),
              TotDR = sum(DR),
              TotAst = sum(Ast),
              TotTO = sum(TO),
              TotStl = sum(Stl),
              TotBlk = sum(Blk),
              TotPF = sum(PF),
              TotPossAllow = sum(OPoss),
              TotFGMAllow = sum(OFGM),
              TotFGAAllow = sum(OFGA),
              TotFGM3Allow = sum(OFGM3),
              TotFGA3Allow = sum(OFGA3),
              TotFTMAllow = sum(OFTM),
              TotFTAAllow = sum(OFTA),
              TotORAllow = sum(O_OR),
              TotDRAllow = sum(ODR),
              TotAstAllow = sum(OAst),
              TotTOAllow = sum(OTO),
              TotStlAllow = sum(OStl),
              TotBlkAllow = sum(OBlk),
              TotPFAllow = sum(OPF)) %>% ungroup()
  
}


season_team_stats_tot <- reshape_detailed_results(reg_season_stats)


# calculate win percentage
season_team_stats_tot$WinPerc <- season_team_stats_tot$wins / season_team_stats_tot$GP


#---------- Create a dataframe of season averages for each team ----------#

# Next I will define a function that takes in the totals dataframe from the above function and calculate a season averages dataset.
# this function also only requires one parameter to be passed to it, the totals dataframe.


calculate_detailed_averages <- function(totals_dataframe) {
  
  averages <- totals_dataframe
  
  cols <- names(averages[,c(6:36)])
  
  for (eachcol in cols) {
    averages[,eachcol] <- round(averages[,eachcol] / averages$GP,2)
    
  }
  
  averages <- averages %>%
    rename(AvgPoints = TotPoints, AvgPointsAllow=TotPointsAllow, AvgOT=NumOT, AvgPoss=TotPos, AvgFGM=TotFGM,  AvgFGA=TotFGA, AvgFGM3=TotFGM3, AvgFGA3=TotFGA3, AvgFTM=TotFTM,
           AvgFTA=TotFTA, AvgOR=TotOR, Avg_DR=TotDR, AvgAst=TotAst, AvgTO=TotTO, AvgStl=TotStl, Avg_Blk=TotBlk, AvgPF=TotPF, AvgPossAllow=TotPossAllow, AvgFGMAllow=TotFGMAllow, 
           AvgFGAAllow=TotFGAAllow,  AvgFGM3Allow=TotFGM3Allow, AvgFGA3Allow=TotFGA3Allow,  AvgFTMAllow=TotFTMAllow, AvgFTAAllow=TotFTAAllow, 
           AvgORAllow=TotORAllow, AvgDRAllow=TotDRAllow,  AvgAstAllow=TotAstAllow, AvgTOAllow=TotTOAllow, AvgStlAllow=TotStlAllow,  AvgBlkAllow=TotBlkAllow,
           AvgPFAllow=TotPFAllow) %>%
    mutate(PointsPerPoss = AvgPoints / AvgPoss,
           PointsPerPossAllow = AvgPointsAllow / AvgPossAllow)
  
  return(averages)
  
}


# create an averages dataframe
season_team_stats_averages <- calculate_detailed_averages(season_team_stats_tot)


#---------- Join some stats back to the results df ----------#
key_features <- season_team_stats_averages %>%
  select(Season, TeamID, AvgPoints, AvgPointsAllow, AvgPoss, AvgPossAllow, WinPerc, PointsPerPoss, PointsPerPossAllow) %>%
  mutate(AvgPointsDiff = AvgPoints - AvgPointsAllow) %>%
  select(-AvgPoints, -AvgPointsAllow) %>%
  mutate(SeasID = paste(Season, TeamID, sep = "_"))






## Cite RyanWilson for the bit below:
#https://www.kaggle.com/rjwdata/basic-logistic-regression-r

results <- read.csv("data/datafiles/NCAATourneyCompactResults.csv", stringsAsFactors = FALSE)



# Keep only the needed variables from the results; Season, WTeamID, and LTeamID
results %<>% select(Season, WTeamID, LTeamID)
# Rearrange data so it matches submission file
results %<>% mutate(team_id_diff = WTeamID - LTeamID,
                    Team1 = case_when(team_id_diff < 0 ~ WTeamID,
                                      team_id_diff > 0 ~ LTeamID),
                    Team2 = case_when(team_id_diff > 0 ~ WTeamID,
                                      team_id_diff < 0 ~ LTeamID),
                    result = if_else(WTeamID == Team1, 1, 0))
results %>% filter((Team1 - Team2) > 0)
# Remove WTeamID, LTeamID, and team_id_diff
results %<>% select(1,5:7)
head(results)


# Split results into training and test sets
train <- results 
test <- read.csv("data/SampleSubmissionStage1.csv", stringsAsFactors = FALSE)

# Create Training Set
# Remove characters from seed so integer remains
tourney_seeds$Seed <- as.integer(str_extract_all(tourney_seeds$Seed, "[0-9]+"))

# Create Training Set
# Join seeds onto the results for team1 and team2
team1_seeds <- tourney_seeds %>% set_colnames(c("Season", "T1Seed", "Team1ID"))
team2_seeds <- tourney_seeds %>% set_colnames(c("Season", "T2Seed", "Team2ID"))

# Join seeds to training set
train %<>% left_join(., team1_seeds, by = c("Season", "Team1"="Team1ID"))
train %<>% left_join(., team2_seeds, by = c("Season", "Team2"="Team2ID"))

# Create relative round indicator 
train %<>% mutate(team1_seed_str = if_else(T1Seed < 9, 1,0),
                  team2_seed_str = if_else(T2Seed < 9, 1,0))
head(train)

# Create variable seed_diff
train %<>% mutate(seed_diff = T1Seed - T2Seed)
head(train)

# Join points diff to training set
train <- train %>% mutate(SeasID1 = paste(Season, Team1, sep = "_"),
                          SeasID2 = paste(Season, Team2, sep = "_")) %>%
  left_join(key_features, by = c("SeasID1" = "SeasID")) %>%
  left_join(key_features, by = c("SeasID2" = "SeasID"))

# remove na's
train <- na.omit(train)

names((train))

# clean up training df
train <- train %>%
  select(-SeasID1, -SeasID2, -Season.y, -TeamID.x, -Season, -TeamID.y) %>%
  rename(Season = Season.x,
         Team1AvgPoss = AvgPoss.x,
         Team2AvgPoss = AvgPoss.y,
         Team1AvgPossAllow = AvgPossAllow.x,
         Team2AvgPossAllow = AvgPossAllow.y,
         Team1WinPerc = WinPerc.x,
         Team2WinPerc = WinPerc.y,
         Team1PPP = PointsPerPoss.x,
         Team1PPPAllow = PointsPerPossAllow.x,
         Team2PPP = PointsPerPoss.y,
         Team2PPPAllow = PointsPerPossAllow.y,
         Team1APD = AvgPointsDiff.x,
         Team2APD = AvgPointsDiff.y) %>%
  mutate(PossDiff = Team1AvgPoss - Team2AvgPoss,
         PossAllowDiff = Team1AvgPossAllow - Team2AvgPossAllow,
         WinPercDiff = Team1WinPerc - Team2WinPerc,
         PointsPerPossDiff = Team1PPP - Team2PPP,
         PointsPerPossAllowDiff = Team1PPPAllow - Team2PPPAllow,
         PointsDiff = Team1APD - Team2APD)

# subset train so that the model doesn't overfit by seeing the data

train <- subset(train, Season < 2014)


#####################################
#---------- Train models -----------#
#####################################

# random forest
library(randomForest)



# 3rd RF MOdel
fitRF <- randomForest(result ~ seed_diff + team1_seed_str + team2_seed_str + Team1APD + Team2APD + Team1AvgPoss + Team1AvgPossAllow + Team1WinPerc + Team1PPP +
                        Team1PPPAllow + Team1APD + Team2AvgPoss + Team2AvgPossAllow + Team2WinPerc + Team2PPP + Team2PPPAllow + Team2APD, 
                      data = train,
                      ntree = 500) ##### This model has the best submission score so far, with a public score of 0.208922 #####

train$PredRF <- predict(fitRF, train)
train$PredRF1_0 <- ifelse(train$PredRF >0.5,1,0)

mean(train$result == train$PredRF1_0)

plot(fitRF)
varImpPlot(fitRF)



#---------- Create predictions for submission file: ----------#

# Prepare submission file
test %<>% select(ID) %>% separate(ID, sep = "_", into = c("Season", "Team1", "Team2"), convert = TRUE)

test %<>% left_join(., team1_seeds, by = c("Season", "Team1"="Team1ID"))
test %<>% left_join(., team2_seeds, by = c("Season", "Team2"="Team2ID"))
head(test)

# Create relative round indicator 
test %<>% mutate(team1_seed_str = if_else(T1Seed < 9, 1,0),
                 team2_seed_str = if_else(T2Seed < 9, 1,0),
                 seed_diff = T1Seed - T2Seed)

head(test)

# Create variable seed_diff
test %<>% mutate(seed_diff = T1Seed - T2Seed)
head(test)

# Join points diff to training set
test <- test %>% mutate(SeasID1 = paste(Season, Team1, sep = "_"),
                        SeasID2 = paste(Season, Team2, sep = "_")) %>%
  left_join(key_features, by = c("SeasID1" = "SeasID")) %>%
  left_join(key_features, by = c("SeasID2" = "SeasID"))


# clean up training df
test <- test %>%
  select(-SeasID1, -SeasID2, -Season.y, -TeamID.x, -Season, -TeamID.y) %>%
  rename(Season = Season.x,
         Team1AvgPoss = AvgPoss.x,
         Team2AvgPoss = AvgPoss.y,
         Team1AvgPossAllow = AvgPossAllow.x,
         Team2AvgPossAllow = AvgPossAllow.y,
         Team1WinPerc = WinPerc.x,
         Team2WinPerc = WinPerc.y,
         Team1PPP = PointsPerPoss.x,
         Team1PPPAllow = PointsPerPossAllow.x,
         Team2PPP = PointsPerPoss.y,
         Team2PPPAllow = PointsPerPossAllow.y,
         Team1APD = AvgPointsDiff.x,
         Team2APD = AvgPointsDiff.y) %>%
  mutate(PossDiff = Team1AvgPoss - Team2AvgPoss,
         PossAllowDiff = Team1AvgPossAllow - Team2AvgPossAllow,
         WinPercDiff = Team1WinPerc - Team2WinPerc,
         PointsPerPossDiff = Team1PPP - Team2PPP,
         PointsPerPossAllowDiff = Team1PPPAllow - Team2PPPAllow,
         PointsDiff = Team1APD - Team2APD)

test$Pred <- predict(fitRF, test, type = "response")

test$PredRF <- predict(fitRF, test)


#---------- create submission file: ----------# 

## Random Forest
R1SubmissionRF <- test %>% 
  select(Season, Team1, Team2, PredRF) %>%
  rename(Pred = PredRF)

R1SubmissionRF <- unite(R1SubmissionRF, "ID", Season, Team1, Team2, sep = "_")
write.csv(R1SubmissionRF, "R1SubmissionRF.csv", row.names = FALSE)



# for kuroki

train %>% 
  mutate(ID = str_c(Season, "_", Team1, "_", Team2)) %>% 
  select(ID, 
         # target = result, 
         seed_diff, team1_seed_str, team2_seed_str, Team1APD,
         Team2APD, Team1AvgPoss, Team1AvgPossAllow, Team1WinPerc, Team1PPP,
         Team1PPPAllow, Team1APD, Team2AvgPoss,
         Team2AvgPossAllow, Team2WinPerc, Team2PPP, 
         Team2PPPAllow, Team2APD) %>% 
  write_csv("data/tmp.csv")
  
