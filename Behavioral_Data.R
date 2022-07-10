# Libraries ---------------------------------------------------------------
library("tidyverse")
library("tidyr")
library("dplyr")
library("pROC")
library("ROSE")
library("psycho")

# Creating List of Participant Data Files ---------------------------------
participants <- list("MURA_3_edited.csv", "MURA_4_edited.csv", "MURA_5_edited.csv", "MURA_6_edited.csv", 
                     "MURA_7_edited.csv", "MURA_8_edited.csv", "MURA_9_edited.csv", "MURA_11_edited.csv", 
                     "MURA_12_edited.csv", "MURA_14_edited.csv", "MURA_15_edited.csv", "MURA_20_edited.csv",
                     "MURA_21_edited.csv", "MURA_22_edited.csv", "MURA_23_edited.csv", "MURA_24_edited.csv",
                     "MURA_25_edited.csv", "MURA_26_edited.csv", "MURA_27_edited.csv", "MURA_28_edited.csv",
                     "MURA_29_edited.csv", "MURA_30_edited.csv", "MURA_31_edited.csv", "MURA_32_edited.csv",
                     "MURA_33_edited.csv", "MURA_34_edited.csv", "MURA_35_edited.csv", "MURA_36_edited.csv",
                     "MURA_37_edited.csv", "MURA_38_edited.csv", "MURA_39_edited.csv", "MURA_40_edited.csv",
                     "MURA_41_edited.csv", "MURA_42_edited.csv", "MURA_43_edited.csv",
                     "MURA_45_edited.csv", "MURA_46_edited.csv", "MURA_47_edited.csv", "MURA_48_edited.csv",
                     "MURA_49_edited.csv", "MURA_50_edited.csv", "MURA_51_edited.csv", "MURA_52_edited.csv",
                     "MURA_53_edited.csv", "MURA_54_edited.csv", "MURA_55_edited.csv", "MURA_56_edited.csv",
                     "MURA_57_edited.csv", "MURA_58_edited.csv", "MURA_59_edited.csv", "MURA_60_edited.csv",
                     "MURA_61_edited.csv", "MURA_62_edited.csv", "MURA_63_edited.csv", "MURA_64_edited.csv")

# Hit Rates ---------------------------------------------------------------

# Creating empty final data set
HitRates1 <- data.frame(matrix(ncol = 7, nrow = 0))

colnames(HitRates1)<-c("Subject ID",
                       "CC Black Hispanic", "CC White", 
                       "SC Black Hispanic", "SC White",
                       "WhiteWhite Black Hispanic", "WhiteWhite White")

for (i in 1:length(participants)) {
  
  # load dataset
  data_set <- read_csv(file = participants[[i]])
  colnames(data_set)<-c("ExperimentName", "Subject", "Age", "FamHand", "Group", "Handedness", "Sex", "Block", "Counterbalance", "Counterbalance.Cycle", "Counterbalance.Sample", "Hand",	"Procedure[Block]",	"Running[Block]",	"Ss",	"Trial",	"BlockList",	"BlockList.Cycle",	"BlockList.Sample",	"FixDuration[Trial]",	"MajorClips",	"MajorSound",	"MinorClips",	"MinorSound", "MusicCondition", "Procedure[Trial]",	"RightText[Trial]",	"Running[Trial]",	"StimDuration[Trial]",	"WhiteNoise",	"WhiteSound",	"SubTrial",	"BlackFaceList[SubTrial]",	"BufferFaceList",	"Condition[SubTrial]",	"CorrAns1[SubTrial]",	"CorrAns2[SubTrial]",	"CorrAns3[SubTrial]",	"ExpBlock",	"ExpBlock.Cycle",	"ExpBlock.Sample",	"FeedDur[SubTrial]",	"FixDuration[SubTrial]",	"HisFaceList[SubTrial]",	"ITIDuration[SubTrial]",	"PracRecallList",	"PracRecallList.Cycle",	"PracRecallList.Sample",	"PracStudyList",	"PracStudyList.Cycle",	"PracStudyList.Sample",	"Procedure[SubTrial]",	"Race[SubTrial]",	"RandNum[SubTrial]",	"Running[SubTrial]",	"StimDuration[SubTrial]",	"Stimulus[SubTrial]",	"StudySound[SubTrial]",	"TestSound[SubTrial]",	"TestStimulus.ACC[SubTrial]",	"TestStimulus.CRESP[SubTrial]",	"TestStimulus.DurationError[SubTrial]",	"TestStimulus.OnsetDelay[SubTrial]",	"TestStimulus.OnsetTime[SubTrial]",	"TestStimulus.RESP[SubTrial]",	"TestStimulus.RT[SubTrial]",	"TestStimulus.RTTime[SubTrial]",	"LogLevel5",	"BirdSong",	"BirdSongRest",	"BirdSongRest.Cycle",	"BirdSongRest.Sample",	"BlackFaceList[LogLevel5]",	"BlockNum",	"CellNumber",	"Condition[LogLevel5]",	"Confidence",	"CorrAns1[LogLevel5]",	"CorrAns2[LogLevel5]",	"CorrAns3[LogLevel5]", "CR", "FA", "FeedDur[LogLevel5]",	"FixDuration[LogLevel5]",	"HisFaceList[LogLevel5]", "hit",	"ITIDuration[LogLevel5]",	"Judgement", "LeftText[LogLevel5]",	"miss", "Procedure[LogLevel5]", "Race[LogLevel5]", "RandNum[LogLevel5]",	"RecallList",	"RecallList.Cycle",	"RecallList.Sample",	"RightText[LogLevel5]",	"Running[LogLevel5]",	"Stimulus[LogLevel5]",	"StudyFaceList",	"StudyList",	"StudyList.Cycle",	"StudyList.Sample",	"StudyOnset",	"StudySound[LogLevel5]",	"TestSound[LogLevel5]",	"TestStimulus.ACC[LogLevel5]",	"TestStimulus.CRESP[LogLevel5]",	"TestStimulus.RESP[LogLevel5]",	"TestStimulus.RT[LogLevel5]", "TestStimulus.OnsetTime[LogLevel5]",	"TestStimulus.RTTime[LogLevel5]",	"WhiteFaceList")
  mura <- data_set
  SubjectID <- mura[1,2]
  
  # dropping unnecessary columns
  col.keep3 <- c("Subject", "MusicCondition", "Race[LogLevel5]", "hit", "FA", "miss", "CR", "Procedure[SubTrial]")
  mura_short <- mura %>% select(one_of(col.keep3))
  
  # uniting independent variables to make one condition variable
  mura_short_2 <- unite(mura_short, "Condition", c("MusicCondition","Race[LogLevel5]"), sep = "_")
  
  # selecting the rows that measure test blocks
  mura_3 <- subset(mura_short_2, `Procedure[SubTrial]` == "TestBlock", select = , drop = FALSE)
  drop.cols2 <-c("Procedure[SubTrial]")
  mura_4 <- mura_3 %>% select(-one_of(drop.cols2))
  
  # Creating the Hit rate for WhiteWhite_black_hispanic
  WhiteWhite_black_hispanic_misses <- length(which(mura_4$Condition == "WhiteWhite_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "WhiteWhite_hispanic" & mura_4$miss == 1))
  WhiteWhite_black_hispanic_Hits <- length(which(mura_4$Condition == "WhiteWhite_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "WhiteWhite_hispanic" & mura_4$hit == 1))
  WhiteWhite_black_hispanic_hit_rate <- WhiteWhite_black_hispanic_Hits / (WhiteWhite_black_hispanic_Hits + WhiteWhite_black_hispanic_misses)
  WhiteWhite_black_hispanic_hit_rate
  
  
  # Creating the Hit rate for WhiteWhite_white
  WhiteWhite_white_misses <- length(which(mura_4$Condition == "WhiteWhite_white" & mura_4$miss == 1))
  WhiteWhite_white_Hits <- length(which(mura_4$Condition == "WhiteWhite_white" & mura_4$hit == 1))
  WhiteWhite_white_hit_rate <- WhiteWhite_white_Hits / (WhiteWhite_white_Hits + WhiteWhite_white_misses)
  WhiteWhite_white_hit_rate
  
  # Creating the Hit rate for CC_black_hispanic
  CC_black_hispanic_misses <- length(which(mura_4$Condition == "MinorMajor_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMinor_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MinorMajor_hispanic" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMinor_hispanic" & mura_4$miss == 1))
  CC_black_hispanic_Hits <- length(which(mura_4$Condition == "MinorMajor_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMinor_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MinorMajor_hispanic" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMinor_hispanic" & mura_4$hit == 1))
  CC_black_hispanic_hit_rate <- (CC_black_hispanic_Hits / (CC_black_hispanic_Hits + CC_black_hispanic_misses))
  CC_black_hispanic_hit_rate
  
  
  # Creating the Hit rate for CC_white
  CC_white_misses <- length(which(mura_4$Condition == "MinorMajor_white" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMinor_white" & mura_4$miss == 1))
  CC_white_Hits <- length(which(mura_4$Condition == "MinorMajor_white" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMinor_white" & mura_4$hit == 1))
  CC_white_hit_rate <- CC_white_Hits / (CC_white_Hits + CC_white_misses)
  CC_white_hit_rate
  
  # Creating the Hit rate for SC_black_hispanic
  SC_black_hispanic_misses <- length(which(mura_4$Condition == "MinorMinor_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMajor_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MinorMinor_hispanic" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMajor_hispanic" & mura_4$miss == 1))
  SC_black_hispanic_Hits <- length(which(mura_4$Condition == "MinorMinor_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMajor_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MinorMinor_hispanic" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMajor_hispanic" & mura_4$hit == 1))
  SC_black_hispanic_hit_rate <- (SC_black_hispanic_Hits / (SC_black_hispanic_Hits + SC_black_hispanic_misses))
  SC_black_hispanic_hit_rate
  
  # Creating the Hit rate for SC_white
  SC_white_misses <- length(which(mura_4$Condition == "MinorMinor_white" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMajor_white" & mura_4$miss == 1))
  SC_white_Hits <- length(which(mura_4$Condition == "MinorMinor_white" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMajor_white" & mura_4$hit == 1))
  SC_white_hit_rate <- SC_white_Hits / (SC_white_Hits + SC_white_misses)
  SC_white_hit_rate
  
  # adding a new row to the data frame
  HitRates1[nrow(HitRates1) + 1,] = c(SubjectID,
                                      CC_black_hispanic_hit_rate, CC_white_hit_rate, 
                                      SC_black_hispanic_hit_rate, SC_white_hit_rate,
                                      WhiteWhite_black_hispanic_hit_rate, WhiteWhite_white_hit_rate)
  
  colnames(HitRates1)<-c("Subject ID",
                         "CC Black Hispanic", "CC White", 
                         "SC Black Hispanic", "SC White",
                         "WhiteWhite Black Hispanic", "WhiteWhite White")
}
HitRates_2 <- HitRates1[!duplicated(HitRates1),]

# Converting to Long Format
HitRates_3 <- HitRates_2 %>% gather(Condition, Value, -`Subject ID`)

# Creating Context and Race Variables
HitRates_3$Context[HitRates_3$Condition=='CC Black Hispanic'] <- 'CC'
HitRates_3$Race[HitRates_3$Condition=='CC Black Hispanic'] <- 'BH'

HitRates_3$Context[HitRates_3$Condition=='CC White'] <- 'CC'
HitRates_3$Race[HitRates_3$Condition=='CC White'] <- 'W'

HitRates_3$Context[HitRates_3$Condition=='SC Black Hispanic'] <- 'SC'
HitRates_3$Race[HitRates_3$Condition=='SC Black Hispanic'] <- 'BH'

HitRates_3$Context[HitRates_3$Condition=='SC White'] <- 'SC'
HitRates_3$Race[HitRates_3$Condition=='SC White'] <- 'W'

HitRates_3$Context[HitRates_3$Condition=='WhiteWhite Black Hispanic'] <- 'WW'
HitRates_3$Race[HitRates_3$Condition=='WhiteWhite Black Hispanic'] <- 'BH'

HitRates_3$Context[HitRates_3$Condition=='WhiteWhite White'] <- 'WW'
HitRates_3$Race[HitRates_3$Condition=='WhiteWhite White'] <- 'W'

# Creating Final Data set
HitRates_Final <- HitRates_3


# Miss Rates --------------------------------------------------------------

# Setting up the final data frame
MissRates1 <- data.frame(matrix(ncol = 7, nrow = 0))


colnames(MissRates1)<-c("Subject ID",
                        "CC Black Hispanic", "CC White", 
                        "SC Black Hispanic", "SC White",
                        "WhiteWhite Black Hispanic", "WhiteWhite White")

for (i in 1:length(participants)) {
  
  # load dataset
  data_set <- read_csv(file = participants[[i]])
  colnames(data_set)<-c("ExperimentName", "Subject", "Age", "FamHand", "Group", "Handedness", "Sex", "Block", "Counterbalance", "Counterbalance.Cycle", "Counterbalance.Sample", "Hand",	"Procedure[Block]",	"Running[Block]",	"Ss",	"Trial",	"BlockList",	"BlockList.Cycle",	"BlockList.Sample",	"FixDuration[Trial]",	"MajorClips",	"MajorSound",	"MinorClips",	"MinorSound", "MusicCondition", "Procedure[Trial]",	"RightText[Trial]",	"Running[Trial]",	"StimDuration[Trial]",	"WhiteNoise",	"WhiteSound",	"SubTrial",	"BlackFaceList[SubTrial]",	"BufferFaceList",	"Condition[SubTrial]",	"CorrAns1[SubTrial]",	"CorrAns2[SubTrial]",	"CorrAns3[SubTrial]",	"ExpBlock",	"ExpBlock.Cycle",	"ExpBlock.Sample",	"FeedDur[SubTrial]",	"FixDuration[SubTrial]",	"HisFaceList[SubTrial]",	"ITIDuration[SubTrial]",	"PracRecallList",	"PracRecallList.Cycle",	"PracRecallList.Sample",	"PracStudyList",	"PracStudyList.Cycle",	"PracStudyList.Sample",	"Procedure[SubTrial]",	"Race[SubTrial]",	"RandNum[SubTrial]",	"Running[SubTrial]",	"StimDuration[SubTrial]",	"Stimulus[SubTrial]",	"StudySound[SubTrial]",	"TestSound[SubTrial]",	"TestStimulus.ACC[SubTrial]",	"TestStimulus.CRESP[SubTrial]",	"TestStimulus.DurationError[SubTrial]",	"TestStimulus.OnsetDelay[SubTrial]",	"TestStimulus.OnsetTime[SubTrial]",	"TestStimulus.RESP[SubTrial]",	"TestStimulus.RT[SubTrial]",	"TestStimulus.RTTime[SubTrial]",	"LogLevel5",	"BirdSong",	"BirdSongRest",	"BirdSongRest.Cycle",	"BirdSongRest.Sample",	"BlackFaceList[LogLevel5]",	"BlockNum",	"CellNumber",	"Condition[LogLevel5]",	"Confidence",	"CorrAns1[LogLevel5]",	"CorrAns2[LogLevel5]",	"CorrAns3[LogLevel5]", "CR", "FA", "FeedDur[LogLevel5]",	"FixDuration[LogLevel5]",	"HisFaceList[LogLevel5]", "hit",	"ITIDuration[LogLevel5]",	"Judgement", "LeftText[LogLevel5]",	"miss", "Procedure[LogLevel5]", "Race[LogLevel5]", "RandNum[LogLevel5]",	"RecallList",	"RecallList.Cycle",	"RecallList.Sample",	"RightText[LogLevel5]",	"Running[LogLevel5]",	"Stimulus[LogLevel5]",	"StudyFaceList",	"StudyList",	"StudyList.Cycle",	"StudyList.Sample",	"StudyOnset",	"StudySound[LogLevel5]",	"TestSound[LogLevel5]",	"TestStimulus.ACC[LogLevel5]",	"TestStimulus.CRESP[LogLevel5]",	"TestStimulus.RESP[LogLevel5]",	"TestStimulus.RT[LogLevel5]", "TestStimulus.OnsetTime[LogLevel5]",	"TestStimulus.RTTime[LogLevel5]",	"WhiteFaceList")
  mura <- data_set
  SubjectID <- mura[1,2]
  
  # dropping unnecessary columns
  col.keep3 <- c("Subject", "MusicCondition", "Race[LogLevel5]", "hit", "FA", "miss", "CR", "Procedure[SubTrial]")
  mura_short <- mura %>% select(one_of(col.keep3))
  
  # uniting independent variables to make one condition variable
  mura_short_2 <- unite(mura_short, "Condition", c("MusicCondition","Race[LogLevel5]"), sep = "_")
  
  # selecting the rows that measure test blocks
  mura_3 <- subset(mura_short_2, `Procedure[SubTrial]` == "TestBlock", select = , drop = FALSE)
  drop.cols2 <-c("Procedure[SubTrial]")
  mura_4 <- mura_3 %>% select(-one_of(drop.cols2))
  
  # Creating the Miss rate for WhiteWhite_black_hispanic
  WhiteWhite_black_hispanic_misses <- length(which(mura_4$Condition == "WhiteWhite_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "WhiteWhite_hispanic" & mura_4$miss == 1))
  WhiteWhite_black_hispanic_Hits <- length(which(mura_4$Condition == "WhiteWhite_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "WhiteWhite_hispanic" & mura_4$hit == 1))
  WhiteWhite_black_hispanic_miss_rate <- WhiteWhite_black_hispanic_misses / (WhiteWhite_black_hispanic_Hits + WhiteWhite_black_hispanic_misses)
  WhiteWhite_black_hispanic_miss_rate
  
  # Creating the miss rate for WhiteWhite_white
  WhiteWhite_white_misses <- length(which(mura_4$Condition == "WhiteWhite_white" & mura_4$miss == 1))
  WhiteWhite_white_Hits <- length(which(mura_4$Condition == "WhiteWhite_white" & mura_4$hit == 1))
  WhiteWhite_white_miss_rate <- WhiteWhite_white_misses / (WhiteWhite_white_Hits + WhiteWhite_white_misses)
  WhiteWhite_white_miss_rate
  
  # Creating the miss rate for CC_black_hispanic
  CC_black_hispanic_misses <- length(which(mura_4$Condition == "MinorMajor_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMinor_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MinorMajor_hispanic" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMinor_hispanic" & mura_4$miss == 1))
  CC_black_hispanic_Hits <- length(which(mura_4$Condition == "MinorMajor_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMinor_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MinorMajor_hispanic" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMinor_hispanic" & mura_4$hit == 1))
  CC_black_hispanic_miss_rate <- (CC_black_hispanic_misses / (CC_black_hispanic_Hits + CC_black_hispanic_misses))
  CC_black_hispanic_miss_rate
  
  
  # Creating the miss rate for CC_white
  CC_white_misses <- length(which(mura_4$Condition == "MinorMajor_white" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMinor_white" & mura_4$miss == 1))
  CC_white_Hits <- length(which(mura_4$Condition == "MinorMajor_white" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMinor_white" & mura_4$hit == 1))
  CC_white_miss_rate <- CC_white_misses / (CC_white_Hits + CC_white_misses)
  CC_white_miss_rate
  
  # Creating the miss rate for SC_black_hispanic
  SC_black_hispanic_misses <- length(which(mura_4$Condition == "MinorMinor_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMajor_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MinorMinor_hispanic" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMajor_hispanic" & mura_4$miss == 1))
  SC_black_hispanic_Hits <- length(which(mura_4$Condition == "MinorMinor_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMajor_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MinorMinor_hispanic" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMajor_hispanic" & mura_4$hit == 1))
  SC_black_hispanic_miss_rate <- (SC_black_hispanic_misses / (SC_black_hispanic_Hits + SC_black_hispanic_misses))
  SC_black_hispanic_miss_rate
  
  # Creating the Hit rate for SC_white
  SC_white_misses <- length(which(mura_4$Condition == "MinorMinor_white" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMajor_white" & mura_4$miss == 1))
  SC_white_Hits <- length(which(mura_4$Condition == "MinorMinor_white" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMajor_white" & mura_4$hit == 1))
  SC_white_miss_rate <- SC_white_misses / (SC_white_Hits + SC_white_misses)
  SC_white_miss_rate
  
  # adding a new row to the data frame
  MissRates1[nrow(MissRates1) + 1,] = c(SubjectID,
                                        CC_black_hispanic_miss_rate, CC_white_miss_rate, 
                                        SC_black_hispanic_miss_rate, SC_white_miss_rate,
                                        WhiteWhite_black_hispanic_miss_rate, WhiteWhite_white_miss_rate)
  
  colnames(MissRates1)<-c("Subject ID",
                          "CC Black Hispanic", "CC White", 
                          "SC Black Hispanic", "SC White",
                          "WhiteWhite Black Hispanic", "WhiteWhite White")
}
MissRates_2 <- MissRates1[!duplicated(MissRates1),]

# Converting to Long Format
MissRates_3 <- MissRates_2 %>% gather(Condition, Value, -`Subject ID`)

# Creating Context and Race Variables
MissRates_3$Context[MissRates_3$Condition=='CC Black Hispanic'] <- 'CC'
MissRates_3$Race[MissRates_3$Condition=='CC Black Hispanic'] <- 'BH'

MissRates_3$Context[MissRates_3$Condition=='CC White'] <- 'CC'
MissRates_3$Race[MissRates_3$Condition=='CC White'] <- 'W'

MissRates_3$Context[MissRates_3$Condition=='SC Black Hispanic'] <- 'SC'
MissRates_3$Race[MissRates_3$Condition=='SC Black Hispanic'] <- 'BH'

MissRates_3$Context[MissRates_3$Condition=='SC White'] <- 'SC'
MissRates_3$Race[MissRates_3$Condition=='SC White'] <- 'W'

MissRates_3$Context[MissRates_3$Condition=='WhiteWhite Black Hispanic'] <- 'WW'
MissRates_3$Race[MissRates_3$Condition=='WhiteWhite Black Hispanic'] <- 'BH'

MissRates_3$Context[MissRates_3$Condition=='WhiteWhite White'] <- 'WW'
MissRates_3$Race[MissRates_3$Condition=='WhiteWhite White'] <- 'W'

# Creating Final Data set
MissRates_Final <- MissRates_3

# False Alarm Rates -------------------------------------------------------

# Setting up the final data frame
FARates <- data.frame(matrix(ncol = 7, nrow = 0))

colnames(FARates)<-c("Subject ID",
                     "CC Black Hispanic", "CC White", 
                     "SC Black Hispanic", "SC White",
                     "WhiteWhite Black Hispanic", "WhiteWhite White")

for (i in 1:length(participants)) {
  
  # load dataset
  data_set <- read_csv(file = participants[[i]])
  colnames(data_set)<-c("ExperimentName", "Subject", "Age", "FamHand", "Group", "Handedness", "Sex", "Block", "Counterbalance", "Counterbalance.Cycle", "Counterbalance.Sample", "Hand",	"Procedure[Block]",	"Running[Block]",	"Ss",	"Trial",	"BlockList",	"BlockList.Cycle",	"BlockList.Sample",	"FixDuration[Trial]",	"MajorClips",	"MajorSound",	"MinorClips",	"MinorSound", "MusicCondition", "Procedure[Trial]",	"RightText[Trial]",	"Running[Trial]",	"StimDuration[Trial]",	"WhiteNoise",	"WhiteSound",	"SubTrial",	"BlackFaceList[SubTrial]",	"BufferFaceList",	"Condition[SubTrial]",	"CorrAns1[SubTrial]",	"CorrAns2[SubTrial]",	"CorrAns3[SubTrial]",	"ExpBlock",	"ExpBlock.Cycle",	"ExpBlock.Sample",	"FeedDur[SubTrial]",	"FixDuration[SubTrial]",	"HisFaceList[SubTrial]",	"ITIDuration[SubTrial]",	"PracRecallList",	"PracRecallList.Cycle",	"PracRecallList.Sample",	"PracStudyList",	"PracStudyList.Cycle",	"PracStudyList.Sample",	"Procedure[SubTrial]",	"Race[SubTrial]",	"RandNum[SubTrial]",	"Running[SubTrial]",	"StimDuration[SubTrial]",	"Stimulus[SubTrial]",	"StudySound[SubTrial]",	"TestSound[SubTrial]",	"TestStimulus.ACC[SubTrial]",	"TestStimulus.CRESP[SubTrial]",	"TestStimulus.DurationError[SubTrial]",	"TestStimulus.OnsetDelay[SubTrial]",	"TestStimulus.OnsetTime[SubTrial]",	"TestStimulus.RESP[SubTrial]",	"TestStimulus.RT[SubTrial]",	"TestStimulus.RTTime[SubTrial]",	"LogLevel5",	"BirdSong",	"BirdSongRest",	"BirdSongRest.Cycle",	"BirdSongRest.Sample",	"BlackFaceList[LogLevel5]",	"BlockNum",	"CellNumber",	"Condition[LogLevel5]",	"Confidence",	"CorrAns1[LogLevel5]",	"CorrAns2[LogLevel5]",	"CorrAns3[LogLevel5]", "CR", "FA", "FeedDur[LogLevel5]",	"FixDuration[LogLevel5]",	"HisFaceList[LogLevel5]", "hit",	"ITIDuration[LogLevel5]",	"Judgement", "LeftText[LogLevel5]",	"miss", "Procedure[LogLevel5]", "Race[LogLevel5]", "RandNum[LogLevel5]",	"RecallList",	"RecallList.Cycle",	"RecallList.Sample",	"RightText[LogLevel5]",	"Running[LogLevel5]",	"Stimulus[LogLevel5]",	"StudyFaceList",	"StudyList",	"StudyList.Cycle",	"StudyList.Sample",	"StudyOnset",	"StudySound[LogLevel5]",	"TestSound[LogLevel5]",	"TestStimulus.ACC[LogLevel5]",	"TestStimulus.CRESP[LogLevel5]",	"TestStimulus.RESP[LogLevel5]",	"TestStimulus.RT[LogLevel5]", "TestStimulus.OnsetTime[LogLevel5]",	"TestStimulus.RTTime[LogLevel5]",	"WhiteFaceList")
  mura <- data_set
  SubjectID <- mura[1,2]
  
  # dropping unnecessary columns
  drop.cols <- c("ExperimentName", "Age", "FamHand", "Group", "Handedness", "Sex", "Block", "Counterbalance", "Counterbalance.Cycle", "Counterbalance.Sample", "Hand",	"Procedure[Block]",	"Running[Block]",	"Ss",	"Trial",	"BlockList",	"BlockList.Cycle",	"BlockList.Sample",	"FixDuration[Trial]",	"MajorClips",	"MajorSound",	"MinorClips",	"MinorSound", "Procedure[Trial]",	"RightText[Trial]",	"Running[Trial]",	"StimDuration[Trial]",	"WhiteNoise",	"WhiteSound",	"SubTrial",	"BlackFaceList[SubTrial]",	"BufferFaceList",	"Condition[SubTrial]",	"CorrAns1[SubTrial]",	"CorrAns2[SubTrial]",	"CorrAns3[SubTrial]",	"ExpBlock",	"ExpBlock.Cycle",	"ExpBlock.Sample",	"FeedDur[SubTrial]",	"FixDuration[SubTrial]",	"HisFaceList[SubTrial]",	"ITIDuration[SubTrial]",	"PracRecallList",	"PracRecallList.Cycle",	"PracRecallList.Sample",	"PracStudyList",	"PracStudyList.Cycle",	"PracStudyList.Sample",	"Race[SubTrial]",	"RandNum[SubTrial]",	"Running[SubTrial]",	"StimDuration[SubTrial]",	"Stimulus[SubTrial]",	"StudySound[SubTrial]",	"TestSound[SubTrial]",	"TestStimulus.ACC[SubTrial]",	"TestStimulus.CRESP[SubTrial]",	"TestStimulus.DurationError[SubTrial]",	"TestStimulus.OnsetDelay[SubTrial]",	"TestStimulus.OnsetTime[SubTrial]",	"TestStimulus.RESP[SubTrial]",	"TestStimulus.RT[SubTrial]",	"TestStimulus.RTTime[SubTrial]",	"LogLevel5",	"BirdSong",	"BirdSongRest",	"BirdSongRest.Cycle",	"BirdSongRest.Sample",	"BlackFaceList[LogLevel5]",	"BlockNum",	"CellNumber",	"Condition[LogLevel5]",	"Confidence",	"CorrAns1[LogLevel5]",	"CorrAns2[LogLevel5]",	"CorrAns3[LogLevel5]", "FeedDur[LogLevel5]",	"FixDuration[LogLevel5]",	"HisFaceList[LogLevel5]",	"ITIDuration[LogLevel5]", "Judgement", "LeftText[LogLevel5]", "miss", "Procedure[LogLevel5]", "RandNum[LogLevel5]",	"RecallList",	"RecallList.Cycle",	"RecallList.Sample",	"RightText[LogLevel5]",	"Running[LogLevel5]",	"Stimulus[LogLevel5]",	"StudyFaceList",	"StudyList",	"StudyList.Cycle",	"StudyList.Sample",	"StudyOnset",	"StudySound[LogLevel5]",	"TestSound[LogLevel5]",	"TestStimulus.ACC[LogLevel5]",	"TestStimulus.CRESP[LogLevel5]",	"TestStimulus.RESP[LogLevel5]", "TestStimulus.RT[LogLevel5]", "TestStimulus.OnsetTime[LogLevel5]",	"TestStimulus.RTTime[LogLevel5]",	"WhiteFaceList")
  mura_short <- mura %>% select(-one_of(drop.cols))
  
  # uniting independent variables to make one condition variable
  mura_short_2 <- unite(mura_short, "Condition", c("MusicCondition","Race[LogLevel5]"), sep = "_")
  
  # selecting the rows that measure test blocks
  mura_3 <- subset(mura_short_2, `Procedure[SubTrial]` == "TestBlock", select = , drop = FALSE)
  drop.cols2 <-c("Procedure[SubTrial]")
  mura_4 <- mura_3 %>% select(-one_of(drop.cols2))
  
  # Creating the FA rate for WhiteWhite_black_hispanic
  WhiteWhite_black_hispanic_FAs <- length(which(mura_4$Condition == "WhiteWhite_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "WhiteWhite_hispanic" & mura_4$FA == 1))
  WhiteWhite_black_hispanic_CRs <- length(which(mura_4$Condition == "WhiteWhite_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "WhiteWhite_hispanic" & mura_4$CR == 1))
  WhiteWhite_black_hispanic_FA_rate <- WhiteWhite_black_hispanic_FAs / (WhiteWhite_black_hispanic_CRs + WhiteWhite_black_hispanic_FAs)
  WhiteWhite_black_hispanic_FA_rate
  
  # Creating the FA rate for WhiteWhite_white
  WhiteWhite_white_FAs <- length(which(mura_4$Condition == "WhiteWhite_white" & mura_4$FA == 1))
  WhiteWhite_white_CRs <- length(which(mura_4$Condition == "WhiteWhite_white" & mura_4$CR == 1))
  WhiteWhite_white_FA_rate <- WhiteWhite_white_FAs / (WhiteWhite_white_CRs + WhiteWhite_white_FAs)
  WhiteWhite_white_FA_rate
  
  # Creating the FA rate for CC_black_hispanic
  CC_black_hispanic_FAs <- length(which(mura_4$Condition == "MinorMajor_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMinor_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MinorMajor_hispanic" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMinor_hispanic" & mura_4$FA == 1))
  CC_black_hispanic_CRs <- length(which(mura_4$Condition == "MinorMajor_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMinor_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MinorMajor_hispanic" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMinor_hispanic" & mura_4$CR == 1))
  CC_black_hispanic_FA_rate <- CC_black_hispanic_FAs / (CC_black_hispanic_CRs + CC_black_hispanic_FAs)
  CC_black_hispanic_FA_rate
  
  # Creating the FA rate for CC_white
  CC_white_FAs <- length(which(mura_4$Condition == "MinorMajor_white" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMinor_white" & mura_4$FA == 1))
  CC_white_CRs <- length(which(mura_4$Condition == "MinorMajor_white" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMinor_white" & mura_4$CR == 1))
  CC_white_FA_rate <- CC_white_FAs / (CC_white_CRs + CC_white_FAs)
  CC_white_FA_rate
  
  # Creating the FA rate for SC_black_hispanic
  SC_black_hispanic_FAs <- length(which(mura_4$Condition == "MinorMinor_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMajor_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MinorMinor_hispanic" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMajor_hispanic" & mura_4$FA == 1))
  SC_black_hispanic_CRs <- length(which(mura_4$Condition == "MinorMinor_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMajor_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MinorMinor_hispanic" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMajor_hispanic" & mura_4$CR == 1))
  SC_black_hispanic_FA_rate <- SC_black_hispanic_FAs / (SC_black_hispanic_CRs + SC_black_hispanic_FAs)
  SC_black_hispanic_FA_rate
  
  # Creating the FA rate for SC_white
  SC_white_FAs <- length(which(mura_4$Condition == "MinorMinor_white" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMajor_white" & mura_4$FA == 1))
  SC_white_CRs <- length(which(mura_4$Condition == "MinorMinor_white" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMajor_white" & mura_4$CR == 1))
  SC_white_FA_rate <- SC_white_FAs / (SC_white_CRs + SC_white_FAs)
  SC_white_FA_rate
  
  # adding a new row to the data frame
  FARates[nrow(FARates) + 1,] = c(SubjectID,
                                  CC_black_hispanic_FA_rate, CC_white_FA_rate, 
                                  SC_black_hispanic_FA_rate, SC_white_FA_rate,
                                  WhiteWhite_black_hispanic_FA_rate, WhiteWhite_white_FA_rate)
  
  colnames(FARates)<-c("Subject ID",
                       "CC Black Hispanic", "CC White", 
                       "SC Black Hispanic", "SC White",
                       "WhiteWhite Black Hispanic", "WhiteWhite White")
}

FARates_2 <- FARates[!duplicated(FARates),]

# Converting to Long Format
FARates_3 <- FARates_2 %>% gather(Condition, Value, -`Subject ID`)

# Creating Context and Race Variables
FARates_3$Context[FARates_3$Condition=='CC Black Hispanic'] <- 'CC'
FARates_3$Race[FARates_3$Condition=='CC Black Hispanic'] <- 'BH'

FARates_3$Context[FARates_3$Condition=='CC White'] <- 'CC'
FARates_3$Race[FARates_3$Condition=='CC White'] <- 'W'

FARates_3$Context[FARates_3$Condition=='SC Black Hispanic'] <- 'SC'
FARates_3$Race[FARates_3$Condition=='SC Black Hispanic'] <- 'BH'

FARates_3$Context[FARates_3$Condition=='SC White'] <- 'SC'
FARates_3$Race[FARates_3$Condition=='SC White'] <- 'W'

FARates_3$Context[FARates_3$Condition=='WhiteWhite Black Hispanic'] <- 'WW'
FARates_Long$Race[FARates_Long$Condition=='WhiteWhite Black Hispanic'] <- 'BH'

FARates_3$Context[FARates_3$Condition=='WhiteWhite White'] <- 'WW'
FARates_3$Race[FARates_3$Condition=='WhiteWhite White'] <- 'W'

# Creating Final Data set
FARates_Final <- FARates_3


# Correct Rejection Rates -------------------------------------------------

# Setting up the final data frame
CRRates <- data.frame(matrix(ncol = 7, nrow = 0))

colnames(CRRates)<-c("Subject ID",
                     "CC Black Hispanic", "CC White", 
                     "SC Black Hispanic", "SC White",
                     "WhiteWhite Black Hispanic", "WhiteWhite White")

for (i in 1:length(participants)) {
  
  # load dataset
  data_set <- read_csv(file = participants[[i]])
  colnames(data_set)<-c("ExperimentName", "Subject", "Age", "FamHand", "Group", "Handedness", "Sex", "Block", "Counterbalance", "Counterbalance.Cycle", "Counterbalance.Sample", "Hand",	"Procedure[Block]",	"Running[Block]",	"Ss",	"Trial",	"BlockList",	"BlockList.Cycle",	"BlockList.Sample",	"FixDuration[Trial]",	"MajorClips",	"MajorSound",	"MinorClips",	"MinorSound", "MusicCondition", "Procedure[Trial]",	"RightText[Trial]",	"Running[Trial]",	"StimDuration[Trial]",	"WhiteNoise",	"WhiteSound",	"SubTrial",	"BlackFaceList[SubTrial]",	"BufferFaceList",	"Condition[SubTrial]",	"CorrAns1[SubTrial]",	"CorrAns2[SubTrial]",	"CorrAns3[SubTrial]",	"ExpBlock",	"ExpBlock.Cycle",	"ExpBlock.Sample",	"FeedDur[SubTrial]",	"FixDuration[SubTrial]",	"HisFaceList[SubTrial]",	"ITIDuration[SubTrial]",	"PracRecallList",	"PracRecallList.Cycle",	"PracRecallList.Sample",	"PracStudyList",	"PracStudyList.Cycle",	"PracStudyList.Sample",	"Procedure[SubTrial]",	"Race[SubTrial]",	"RandNum[SubTrial]",	"Running[SubTrial]",	"StimDuration[SubTrial]",	"Stimulus[SubTrial]",	"StudySound[SubTrial]",	"TestSound[SubTrial]",	"TestStimulus.ACC[SubTrial]",	"TestStimulus.CRESP[SubTrial]",	"TestStimulus.DurationError[SubTrial]",	"TestStimulus.OnsetDelay[SubTrial]",	"TestStimulus.OnsetTime[SubTrial]",	"TestStimulus.RESP[SubTrial]",	"TestStimulus.RT[SubTrial]",	"TestStimulus.RTTime[SubTrial]",	"LogLevel5",	"BirdSong",	"BirdSongRest",	"BirdSongRest.Cycle",	"BirdSongRest.Sample",	"BlackFaceList[LogLevel5]",	"BlockNum",	"CellNumber",	"Condition[LogLevel5]",	"Confidence",	"CorrAns1[LogLevel5]",	"CorrAns2[LogLevel5]",	"CorrAns3[LogLevel5]", "CR", "FA", "FeedDur[LogLevel5]",	"FixDuration[LogLevel5]",	"HisFaceList[LogLevel5]", "hit",	"ITIDuration[LogLevel5]",	"Judgement", "LeftText[LogLevel5]",	"miss", "Procedure[LogLevel5]", "Race[LogLevel5]", "RandNum[LogLevel5]",	"RecallList",	"RecallList.Cycle",	"RecallList.Sample",	"RightText[LogLevel5]",	"Running[LogLevel5]",	"Stimulus[LogLevel5]",	"StudyFaceList",	"StudyList",	"StudyList.Cycle",	"StudyList.Sample",	"StudyOnset",	"StudySound[LogLevel5]",	"TestSound[LogLevel5]",	"TestStimulus.ACC[LogLevel5]",	"TestStimulus.CRESP[LogLevel5]",	"TestStimulus.RESP[LogLevel5]",	"TestStimulus.RT[LogLevel5]", "TestStimulus.OnsetTime[LogLevel5]",	"TestStimulus.RTTime[LogLevel5]",	"WhiteFaceList")
  mura <- data_set
  SubjectID <- mura[1,2]
  
  # dropping unnecessary columns
  drop.cols <- c("ExperimentName", "Age", "FamHand", "Group", "Handedness", "Sex", "Block", "Counterbalance", "Counterbalance.Cycle", "Counterbalance.Sample", "Hand",	"Procedure[Block]",	"Running[Block]",	"Ss",	"Trial",	"BlockList",	"BlockList.Cycle",	"BlockList.Sample",	"FixDuration[Trial]",	"MajorClips",	"MajorSound",	"MinorClips",	"MinorSound", "Procedure[Trial]",	"RightText[Trial]",	"Running[Trial]",	"StimDuration[Trial]",	"WhiteNoise",	"WhiteSound",	"SubTrial",	"BlackFaceList[SubTrial]",	"BufferFaceList",	"Condition[SubTrial]",	"CorrAns1[SubTrial]",	"CorrAns2[SubTrial]",	"CorrAns3[SubTrial]",	"ExpBlock",	"ExpBlock.Cycle",	"ExpBlock.Sample",	"FeedDur[SubTrial]",	"FixDuration[SubTrial]",	"HisFaceList[SubTrial]",	"ITIDuration[SubTrial]",	"PracRecallList",	"PracRecallList.Cycle",	"PracRecallList.Sample",	"PracStudyList",	"PracStudyList.Cycle",	"PracStudyList.Sample",	"Race[SubTrial]",	"RandNum[SubTrial]",	"Running[SubTrial]",	"StimDuration[SubTrial]",	"Stimulus[SubTrial]",	"StudySound[SubTrial]",	"TestSound[SubTrial]",	"TestStimulus.ACC[SubTrial]",	"TestStimulus.CRESP[SubTrial]",	"TestStimulus.DurationError[SubTrial]",	"TestStimulus.OnsetDelay[SubTrial]",	"TestStimulus.OnsetTime[SubTrial]",	"TestStimulus.RESP[SubTrial]",	"TestStimulus.RT[SubTrial]",	"TestStimulus.RTTime[SubTrial]",	"LogLevel5",	"BirdSong",	"BirdSongRest",	"BirdSongRest.Cycle",	"BirdSongRest.Sample",	"BlackFaceList[LogLevel5]",	"BlockNum",	"CellNumber",	"Condition[LogLevel5]",	"Confidence",	"CorrAns1[LogLevel5]",	"CorrAns2[LogLevel5]",	"CorrAns3[LogLevel5]", "FeedDur[LogLevel5]",	"FixDuration[LogLevel5]",	"HisFaceList[LogLevel5]",	"ITIDuration[LogLevel5]", "Judgement", "LeftText[LogLevel5]", "miss", "Procedure[LogLevel5]", "RandNum[LogLevel5]",	"RecallList",	"RecallList.Cycle",	"RecallList.Sample",	"RightText[LogLevel5]",	"Running[LogLevel5]",	"Stimulus[LogLevel5]",	"StudyFaceList",	"StudyList",	"StudyList.Cycle",	"StudyList.Sample",	"StudyOnset",	"StudySound[LogLevel5]",	"TestSound[LogLevel5]",	"TestStimulus.ACC[LogLevel5]",	"TestStimulus.CRESP[LogLevel5]",	"TestStimulus.RESP[LogLevel5]", "TestStimulus.RT[LogLevel5]", "TestStimulus.OnsetTime[LogLevel5]",	"TestStimulus.RTTime[LogLevel5]",	"WhiteFaceList")
  mura_short <- mura %>% select(-one_of(drop.cols))
  
  # uniting independent variables to make one condition variable
  mura_short_2 <- unite(mura_short, "Condition", c("MusicCondition","Race[LogLevel5]"), sep = "_")
  
  # selecting the rows that measure test blocks
  mura_3 <- subset(mura_short_2, `Procedure[SubTrial]` == "TestBlock", select = , drop = FALSE)
  drop.cols2 <-c("Procedure[SubTrial]")
  mura_4 <- mura_3 %>% select(-one_of(drop.cols2))
  
  # Creating the CR rate for WhiteWhite_black_hispanic
  WhiteWhite_black_hispanic_FAs <- length(which(mura_4$Condition == "WhiteWhite_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "WhiteWhite_hispanic" & mura_4$FA == 1))
  WhiteWhite_black_hispanic_CRs <- length(which(mura_4$Condition == "WhiteWhite_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "WhiteWhite_hispanic" & mura_4$CR == 1))
  WhiteWhite_black_hispanic_CR_rate <- WhiteWhite_black_hispanic_CRs / (WhiteWhite_black_hispanic_CRs + WhiteWhite_black_hispanic_FAs)
  WhiteWhite_black_hispanic_CR_rate
  
  # Creating the CR rate for WhiteWhite_white
  WhiteWhite_white_FAs <- length(which(mura_4$Condition == "WhiteWhite_white" & mura_4$FA == 1))
  WhiteWhite_white_CRs <- length(which(mura_4$Condition == "WhiteWhite_white" & mura_4$CR == 1))
  WhiteWhite_white_CR_rate <- WhiteWhite_white_CRs / (WhiteWhite_white_CRs + WhiteWhite_white_FAs)
  WhiteWhite_white_CR_rate
  
  # Creating the CR rate for CC_black_hispanic
  CC_black_hispanic_FAs <- length(which(mura_4$Condition == "MinorMajor_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMinor_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MinorMajor_hispanic" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMinor_hispanic" & mura_4$FA == 1))
  CC_black_hispanic_CRs <- length(which(mura_4$Condition == "MinorMajor_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMinor_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MinorMajor_hispanic" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMinor_hispanic" & mura_4$CR == 1))
  CC_black_hispanic_CR_rate <- CC_black_hispanic_CRs / (CC_black_hispanic_CRs + CC_black_hispanic_FAs)
  CC_black_hispanic_CR_rate
  
  # Creating the CR rate for CC_white
  CC_white_FAs <- length(which(mura_4$Condition == "MinorMajor_white" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMinor_white" & mura_4$FA == 1))
  CC_white_CRs <- length(which(mura_4$Condition == "MinorMajor_white" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMinor_white" & mura_4$CR == 1))
  CC_white_CR_rate <- CC_white_CRs / (CC_white_CRs + CC_white_FAs)
  CC_white_CR_rate
  
  # Creating the CR rate for SC_black_hispanic
  SC_black_hispanic_FAs <- length(which(mura_4$Condition == "MinorMinor_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMajor_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MinorMinor_hispanic" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMajor_hispanic" & mura_4$FA == 1))
  SC_black_hispanic_CRs <- length(which(mura_4$Condition == "MinorMinor_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMajor_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MinorMinor_hispanic" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMajor_hispanic" & mura_4$CR == 1))
  SC_black_hispanic_CR_rate <- SC_black_hispanic_CRs / (SC_black_hispanic_CRs + SC_black_hispanic_FAs)
  SC_black_hispanic_CR_rate
  
  # Creating the CR rate for SC_white
  SC_white_FAs <- length(which(mura_4$Condition == "MinorMinor_white" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMajor_white" & mura_4$FA == 1))
  SC_white_CRs <- length(which(mura_4$Condition == "MinorMinor_white" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMajor_white" & mura_4$CR == 1))
  SC_white_CR_rate <- SC_white_CRs / (SC_white_CRs + SC_white_FAs)
  SC_white_CR_rate
  
  # adding a new row to the data frame
  CRRates[nrow(CRRates) + 1,] = c(SubjectID,
                                  CC_black_hispanic_CR_rate, CC_white_CR_rate, 
                                  SC_black_hispanic_CR_rate, SC_white_CR_rate,
                                  WhiteWhite_black_hispanic_CR_rate, WhiteWhite_white_CR_rate)
  
  colnames(CRRates)<-c("Subject ID",
                       "CC Black Hispanic", "CC White", 
                       "SC Black Hispanic", "SC White",
                       "WhiteWhite Black Hispanic", "WhiteWhite White")
}

CRRates_2 <- CRRates[!duplicated(CRRates),]

# Converting to Long Format
CRRates_3 <- CRRates_2 %>% gather(Condition, Value, -`Subject ID`)

# Creating Context and Race Variables
CRRates_3$Context[CRRates_3$Condition=='CC Black Hispanic'] <- 'CC'
CRRates_3$Race[CRRates_3$Condition=='CC Black Hispanic'] <- 'BH'

CRRates_3$Context[CRRates_3$Condition=='CC White'] <- 'CC'
CRRates_3$Race[CRRates_3$Condition=='CC White'] <- 'W'

CRRates_3$Context[CRRates_3$Condition=='SC Black Hispanic'] <- 'SC'
CRRates_3$Race[CRRates_3$Condition=='SC Black Hispanic'] <- 'BH'

CRRates_3$Context[CRRates_3$Condition=='SC White'] <- 'SC'
CRRates_3$Race[CRRates_3$Condition=='SC White'] <- 'W'

CRRates_3$Context[CRRates_3$Condition=='WhiteWhite Black Hispanic'] <- 'WW'
CRRates_3$Race[CRRates_3$Condition=='WhiteWhite Black Hispanic'] <- 'BH'

CRRates_3$Context[CRRates_3$Condition=='WhiteWhite White'] <- 'WW'
CRRates_3$Race[CRRates_3$Condition=='WhiteWhite White'] <- 'W'

# Creating Final Data set
CRRates_Final <- CRRates_3



# AUC ROC -----------------------------------------------------------------

# Setting up the final data frame
AUC_dataframe <- data.frame(matrix(ncol = 7, nrow = 0))

colnames(AUC_dataframe)<-c("Subject ID",
                           "CC Black Hispanic", "CC White", 
                           "SC Black Hispanic", "SC White",
                           "WhiteWhite Black Hispanic", "WhiteWhite White")

for (i in 1:length(participants)) {
  
  # load dataset
  data_set <- read_csv(file = participants[[i]])
  colnames(data_set)<-c("ExperimentName", "Subject", "Age", "FamHand", "Group", "Handedness", "Sex", "Block", "Counterbalance", "Counterbalance.Cycle", "Counterbalance.Sample", "Hand",	"Procedure[Block]",	"Running[Block]",	"Ss",	"Trial",	"BlockList",	"BlockList.Cycle",	"BlockList.Sample",	"FixDuration[Trial]",	"MajorClips",	"MajorSound",	"MinorClips",	"MinorSound", "MusicCondition", "Procedure[Trial]",	"RightText[Trial]",	"Running[Trial]",	"StimDuration[Trial]",	"WhiteNoise",	"WhiteSound",	"SubTrial",	"BlackFaceList[SubTrial]",	"BufferFaceList",	"Condition[SubTrial]",	"CorrAns1[SubTrial]",	"CorrAns2[SubTrial]",	"CorrAns3[SubTrial]",	"ExpBlock",	"ExpBlock.Cycle",	"ExpBlock.Sample",	"FeedDur[SubTrial]",	"FixDuration[SubTrial]",	"HisFaceList[SubTrial]",	"ITIDuration[SubTrial]",	"PracRecallList",	"PracRecallList.Cycle",	"PracRecallList.Sample",	"PracStudyList",	"PracStudyList.Cycle",	"PracStudyList.Sample",	"Procedure[SubTrial]",	"Race[SubTrial]",	"RandNum[SubTrial]",	"Running[SubTrial]",	"StimDuration[SubTrial]",	"Stimulus[SubTrial]",	"StudySound[SubTrial]",	"TestSound[SubTrial]",	"TestStimulus.ACC[SubTrial]",	"TestStimulus.CRESP[SubTrial]",	"TestStimulus.DurationError[SubTrial]",	"TestStimulus.OnsetDelay[SubTrial]",	"TestStimulus.OnsetTime[SubTrial]",	"TestStimulus.RESP[SubTrial]",	"TestStimulus.RT[SubTrial]",	"TestStimulus.RTTime[SubTrial]",	"LogLevel5",	"BirdSong",	"BirdSongRest",	"BirdSongRest.Cycle",	"BirdSongRest.Sample",	"BlackFaceList[LogLevel5]",	"BlockNum",	"CellNumber",	"Condition[LogLevel5]",	"Confidence",	"CorrAns1[LogLevel5]",	"CorrAns2[LogLevel5]",	"CorrAns3[LogLevel5]", "CR", "FA", "FeedDur[LogLevel5]",	"FixDuration[LogLevel5]",	"HisFaceList[LogLevel5]", "hit",	"ITIDuration[LogLevel5]",	"Judgement", "LeftText[LogLevel5]",	"miss", "Procedure[LogLevel5]", "Race[LogLevel5]", "RandNum[LogLevel5]",	"RecallList",	"RecallList.Cycle",	"RecallList.Sample",	"RightText[LogLevel5]",	"Running[LogLevel5]",	"Stimulus[LogLevel5]",	"StudyFaceList",	"StudyList",	"StudyList.Cycle",	"StudyList.Sample",	"StudyOnset",	"StudySound[LogLevel5]",	"TestSound[LogLevel5]",	"TestStimulus.ACC[LogLevel5]",	"TestStimulus.CRESP[LogLevel5]",	"TestStimulus.RESP[LogLevel5]",	"TestStimulus.RT[LogLevel5]", "TestStimulus.OnsetTime[LogLevel5]",	"TestStimulus.RTTime[LogLevel5]",	"WhiteFaceList")
  mura <- data_set
  SubjectID <- mura[1,2]
  
  # dropping unnecessary columns
  col.keep3 <- c("Subject", "MusicCondition", "Race[LogLevel5]", "hit", "FA", "miss", "CR", "Procedure[SubTrial]")
  mura_short <- mura %>% select(one_of(col.keep3))
  
  # creating the participant response column
  mura_short$ParticipantResponse <- (mura_short$hit + mura_short$FA)
  
  # creating the "Presence of Old Face" column
  mura_short$PresenceOfOldFace <- (mura_short$hit + mura_short$miss)
  
  # uniting independent variables to make one condition variable
  mura_short_2 <- unite(mura_short, "Condition", c("MusicCondition","Race[LogLevel5]"), sep = "_")
  
  # selecting the rows that measure test blocks
  mura_3 <- subset(mura_short_2, `Procedure[SubTrial]` == "TestBlock", select = , drop = FALSE)
  drop.cols2 <-c("Procedure[SubTrial]")
  mura_4 <- mura_3 %>% select(-one_of(drop.cols2))
  
  # Dropping hit, FA, miss, CR columns
  drop.cols3 <- c("hit", "FA", "miss", "CR")
  mura_5 <- mura_4 %>% select(-one_of(drop.cols3))
  
  # Creating Data Subsets and their AUC
  
  # CC_BH subset
  mura_5_CC_BH_1 <- subset(mura_5, `Condition` == "MinorMajor_black", select = , drop = FALSE)
  mura_5_CC_BH_2 <- subset(mura_5, `Condition` == "MajorMinor_black", select = , drop = FALSE) 
  mura_5_CC_BH_3 <- subset(mura_5, `Condition` == "MinorMajor_hispanic", select = , drop = FALSE) 
  mura_5_CC_BH_4 <- subset(mura_5, `Condition` == "MajorMinor_hispanic", select = , drop = FALSE)
  mura_5_CC_BH_5 <- rbind(mura_5_CC_BH_1, mura_5_CC_BH_2, mura_5_CC_BH_3, mura_5_CC_BH_4)
  
  mura_5_CC_BH_AUC_1 <-roc.curve(mura_5_CC_BH_5$PresenceOfOldFace, mura_5_CC_BH_5$ParticipantResponse)
  mura_5_CC_BH_AUC <- mura_5_CC_BH_AUC_1[[2]]
  mura_5_CC_BH_AUC
  
  # CC_Cau subset
  mura_5_CC_Cau_1 <- subset(mura_5, `Condition` == "MinorMajor_white", select = , drop = FALSE)
  mura_5_CC_Cau_2 <- subset(mura_5, `Condition` == "MajorMinor_white", select = , drop = FALSE) 
  mura_5_CC_Cau_3 <- rbind(mura_5_CC_Cau_1, mura_5_CC_Cau_2)
  
  mura_5_CC_Cau_AUC_1 <-roc.curve(mura_5_CC_Cau_3$PresenceOfOldFace, mura_5_CC_Cau_3$ParticipantResponse)
  mura_5_CC_Cau_AUC <- mura_5_CC_Cau_AUC_1[[2]]
  
  # SC_BH subset
  mura_5_SC_BH_1 <- subset(mura_5, `Condition` == "MinorMinor_black", select = , drop = FALSE)
  mura_5_SC_BH_2 <- subset(mura_5, `Condition` == "MajorMajor_black", select = , drop = FALSE) 
  mura_5_SC_BH_3 <- subset(mura_5, `Condition` == "MinorMinor_hispanic", select = , drop = FALSE) 
  mura_5_SC_BH_4 <- subset(mura_5, `Condition` == "MajorMajor_hispanic", select = , drop = FALSE)
  mura_5_SC_BH_5 <- rbind(mura_5_CC_BH_1, mura_5_CC_BH_2, mura_5_CC_BH_3, mura_5_CC_BH_4)
  
  mura_5_SC_BH_AUC_1 <-roc.curve(mura_5_SC_BH_5$PresenceOfOldFace, mura_5_SC_BH_5$ParticipantResponse)
  mura_5_SC_BH_AUC <- mura_5_SC_BH_AUC_1[[2]]
  
  # SC_Cau subset
  mura_5_SC_Cau_1 <- subset(mura_5, `Condition` == "MinorMinor_white", select = , drop = FALSE)
  mura_5_SC_Cau_2 <- subset(mura_5, `Condition` == "MajorMajor_white", select = , drop = FALSE) 
  mura_5_SC_Cau_3 <- rbind(mura_5_SC_Cau_1, mura_5_SC_Cau_2)
  
  mura_5_SC_Cau_AUC_1 <-roc.curve(mura_5_SC_Cau_3$PresenceOfOldFace, mura_5_SC_Cau_3$ParticipantResponse)
  mura_5_SC_Cau_AUC <- mura_5_SC_Cau_AUC_1[[2]]
  
  # C_BH subset
  mura_5_C_BH_1 <- subset(mura_5, `Condition` == "WhiteWhite_black", select = , drop = FALSE)
  mura_5_C_BH_3 <- subset(mura_5, `Condition` == "WhiteWhite_hispanic", select = , drop = FALSE) 
  mura_5_C_BH_4 <- rbind(mura_5_C_BH_1, mura_5_C_BH_3)
  
  mura_5_C_BH_AUC_1 <-roc.curve(mura_5_C_BH_4$PresenceOfOldFace, mura_5_C_BH_4$ParticipantResponse)
  mura_5_C_BH_AUC <- mura_5_C_BH_AUC_1[[2]]
  
  # C_Cau subset
  mura_5_C_Cau <- subset(mura_5, `Condition` == "WhiteWhite_white", select = , drop = FALSE)
  
  mura_5_C_Cau_AUC_1 <-roc.curve(mura_5_C_Cau$PresenceOfOldFace, mura_5_C_Cau$ParticipantResponse)
  mura_5_C_Cau_AUC <- mura_5_C_Cau_AUC_1[[2]]
  
  # adding a new row to the data frame
  AUC_dataframe[nrow(AUC_dataframe) + 1,] = c(SubjectID,
                                              mura_5_CC_BH_AUC, mura_5_CC_Cau_AUC, 
                                              mura_5_SC_BH_AUC, mura_5_SC_Cau_AUC,
                                              mura_5_C_BH_AUC, mura_5_C_Cau_AUC)
  
  colnames(AUC_dataframe)<-c("Subject ID",
                             "CC Black Hispanic", "CC White", 
                             "SC Black Hispanic", "SC White",
                             "WhiteWhite Black Hispanic", "WhiteWhite White")
}

AUC_2 <- AUC_dataframe[!duplicated(AUC_dataframe),]

# Converting to Long Format
AUC_3 <- AUC_2 %>% gather(Condition, Value, -`Subject ID`)

# Creating Context and Race Variables
AUC_3$Context[AUC_3$Condition=='CC Black Hispanic'] <- 'CC'
AUC_3$Race[AUC_3$Condition=='CC Black Hispanic'] <- 'BH'

AUC_3$Context[AUC_3$Condition=='CC White'] <- 'CC'
AUC_3$Race[AUC_3$Condition=='CC White'] <- 'W'

AUC_3$Context[AUC_3$Condition=='SC Black Hispanic'] <- 'SC'
AUC_3$Race[AUC_3$Condition=='SC Black Hispanic'] <- 'BH'

AUC_3$Context[AUC_3$Condition=='SC White'] <- 'SC'
AUC_3$Race[AUC_3$Condition=='SC White'] <- 'W'

AUC_3$Context[AUC_3$Condition=='WhiteWhite Black Hispanic'] <- 'WW'
AUC_3$Race[AUC_3$Condition=='WhiteWhite Black Hispanic'] <- 'BH'

AUC_3$Context[AUC_3$Condition=='WhiteWhite White'] <- 'WW'
AUC_3$Race[AUC_3$Condition=='WhiteWhite White'] <- 'W'

# Creating Final Data set
AUC_Final <- AUC_3


# D Prime -----------------------------------------------------------------

# Create data set with rows as participant and columns as conditions
Dprime <- data.frame(matrix(ncol = 7, nrow = 0))

colnames(Dprime)<-c("Subject ID",
                    "CC Black Hispanic", "CC White", 
                    "SC Black Hispanic", "SC White",
                    "WhiteWhite Black Hispanic", "WhiteWhite White")

for (i in 1:length(participants)) {
  
  # load dataset
  data_set <- read_csv(file = participants[[i]])
  colnames(data_set)<-c("ExperimentName", "Subject", "Age", "FamHand", "Group", "Handedness", "Sex", "Block", "Counterbalance", "Counterbalance.Cycle", "Counterbalance.Sample", "Hand",	"Procedure[Block]",	"Running[Block]",	"Ss",	"Trial",	"BlockList",	"BlockList.Cycle",	"BlockList.Sample",	"FixDuration[Trial]",	"MajorClips",	"MajorSound",	"MinorClips",	"MinorSound", "MusicCondition", "Procedure[Trial]",	"RightText[Trial]",	"Running[Trial]",	"StimDuration[Trial]",	"WhiteNoise",	"WhiteSound",	"SubTrial",	"BlackFaceList[SubTrial]",	"BufferFaceList",	"Condition[SubTrial]",	"CorrAns1[SubTrial]",	"CorrAns2[SubTrial]",	"CorrAns3[SubTrial]",	"ExpBlock",	"ExpBlock.Cycle",	"ExpBlock.Sample",	"FeedDur[SubTrial]",	"FixDuration[SubTrial]",	"HisFaceList[SubTrial]",	"ITIDuration[SubTrial]",	"PracRecallList",	"PracRecallList.Cycle",	"PracRecallList.Sample",	"PracStudyList",	"PracStudyList.Cycle",	"PracStudyList.Sample",	"Procedure[SubTrial]",	"Race[SubTrial]",	"RandNum[SubTrial]",	"Running[SubTrial]",	"StimDuration[SubTrial]",	"Stimulus[SubTrial]",	"StudySound[SubTrial]",	"TestSound[SubTrial]",	"TestStimulus.ACC[SubTrial]",	"TestStimulus.CRESP[SubTrial]",	"TestStimulus.DurationError[SubTrial]",	"TestStimulus.OnsetDelay[SubTrial]",	"TestStimulus.OnsetTime[SubTrial]",	"TestStimulus.RESP[SubTrial]",	"TestStimulus.RT[SubTrial]",	"TestStimulus.RTTime[SubTrial]",	"LogLevel5",	"BirdSong",	"BirdSongRest",	"BirdSongRest.Cycle",	"BirdSongRest.Sample",	"BlackFaceList[LogLevel5]",	"BlockNum",	"CellNumber",	"Condition[LogLevel5]",	"Confidence",	"CorrAns1[LogLevel5]",	"CorrAns2[LogLevel5]",	"CorrAns3[LogLevel5]", "CR", "FA", "FeedDur[LogLevel5]",	"FixDuration[LogLevel5]",	"HisFaceList[LogLevel5]", "hit",	"ITIDuration[LogLevel5]",	"Judgement", "LeftText[LogLevel5]",	"miss", "Procedure[LogLevel5]", "Race[LogLevel5]", "RandNum[LogLevel5]",	"RecallList",	"RecallList.Cycle",	"RecallList.Sample",	"RightText[LogLevel5]",	"Running[LogLevel5]",	"Stimulus[LogLevel5]",	"StudyFaceList",	"StudyList",	"StudyList.Cycle",	"StudyList.Sample",	"StudyOnset",	"StudySound[LogLevel5]",	"TestSound[LogLevel5]",	"TestStimulus.ACC[LogLevel5]",	"TestStimulus.CRESP[LogLevel5]",	"TestStimulus.RESP[LogLevel5]",	"TestStimulus.RT[LogLevel5]", "TestStimulus.OnsetTime[LogLevel5]",	"TestStimulus.RTTime[LogLevel5]",	"WhiteFaceList")
  mura <- data_set
  SubjectID <- mura[1,2]
  
  # dropping unnecessary columns
  drop.cols <- c("ExperimentName", "Age", "FamHand", "Group", "Handedness", "Sex", "Block", "Counterbalance", "Counterbalance.Cycle", "Counterbalance.Sample", "Hand",	"Procedure[Block]",	"Running[Block]",	"Ss",	"Trial",	"BlockList",	"BlockList.Cycle",	"BlockList.Sample",	"FixDuration[Trial]",	"MajorClips",	"MajorSound",	"MinorClips",	"MinorSound", "Procedure[Trial]",	"RightText[Trial]",	"Running[Trial]",	"StimDuration[Trial]",	"WhiteNoise",	"WhiteSound",	"SubTrial",	"BlackFaceList[SubTrial]",	"BufferFaceList",	"Condition[SubTrial]",	"CorrAns1[SubTrial]",	"CorrAns2[SubTrial]",	"CorrAns3[SubTrial]",	"ExpBlock",	"ExpBlock.Cycle",	"ExpBlock.Sample",	"FeedDur[SubTrial]",	"FixDuration[SubTrial]",	"HisFaceList[SubTrial]",	"ITIDuration[SubTrial]",	"PracRecallList",	"PracRecallList.Cycle",	"PracRecallList.Sample",	"PracStudyList",	"PracStudyList.Cycle",	"PracStudyList.Sample",	"Race[SubTrial]",	"RandNum[SubTrial]",	"Running[SubTrial]",	"StimDuration[SubTrial]",	"Stimulus[SubTrial]",	"StudySound[SubTrial]",	"TestSound[SubTrial]",	"TestStimulus.ACC[SubTrial]",	"TestStimulus.CRESP[SubTrial]",	"TestStimulus.DurationError[SubTrial]",	"TestStimulus.OnsetDelay[SubTrial]",	"TestStimulus.OnsetTime[SubTrial]",	"TestStimulus.RESP[SubTrial]",	"TestStimulus.RT[SubTrial]",	"TestStimulus.RTTime[SubTrial]",	"LogLevel5",	"BirdSong",	"BirdSongRest",	"BirdSongRest.Cycle",	"BirdSongRest.Sample",	"BlackFaceList[LogLevel5]",	"BlockNum",	"CellNumber",	"Condition[LogLevel5]",	"Confidence",	"CorrAns1[LogLevel5]",	"CorrAns2[LogLevel5]",	"CorrAns3[LogLevel5]", "FeedDur[LogLevel5]",	"FixDuration[LogLevel5]",	"HisFaceList[LogLevel5]",	"ITIDuration[LogLevel5]", "Judgement", "LeftText[LogLevel5]", "Procedure[LogLevel5]", "RandNum[LogLevel5]",	"RecallList",	"RecallList.Cycle",	"RecallList.Sample",	"RightText[LogLevel5]",	"Running[LogLevel5]",	"Stimulus[LogLevel5]",	"StudyFaceList",	"StudyList",	"StudyList.Cycle",	"StudyList.Sample",	"StudyOnset",	"StudySound[LogLevel5]",	"TestSound[LogLevel5]",	"TestStimulus.ACC[LogLevel5]",	"TestStimulus.CRESP[LogLevel5]",	"TestStimulus.RESP[LogLevel5]", "TestStimulus.RT[LogLevel5]", "TestStimulus.OnsetTime[LogLevel5]",	"TestStimulus.RTTime[LogLevel5]",	"WhiteFaceList")
  mura_short <- mura %>% select(-one_of(drop.cols))
  
  # uniting independent variables to make one condition variable
  mura_short_2 <- unite(mura_short, "Condition", c("MusicCondition","Race[LogLevel5]"), sep = "_")
  
  # selecting the rows that measure test blocks
  mura_3 <- subset(mura_short_2, `Procedure[SubTrial]` == "TestBlock", select = , drop = FALSE)
  drop.cols2 <-c("Procedure[SubTrial]")
  mura_4 <- mura_3 %>% select(-one_of(drop.cols2))
  
  
  # Calculating CR, FA, hit, miss and dprime for WhiteWhite_black_hispanic
  WhiteWhite_black_hispanic_hits <- length(which(mura_4$Condition == "WhiteWhite_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "WhiteWhite_hispanic" & mura_4$hit == 1))
  WhiteWhite_black_hispanic_FAs <- length(which(mura_4$Condition == "WhiteWhite_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "WhiteWhite_hispanic" & mura_4$FA == 1))
  WhiteWhite_black_hispanic_CRs <- length(which(mura_4$Condition == "WhiteWhite_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "WhiteWhite_hispanic" & mura_4$CR == 1))
  WhiteWhite_black_hispanic_misses <- length(which(mura_4$Condition == "WhiteWhite_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "WhiteWhite_hispanic" & mura_4$miss == 1))
  
  WhiteWhite_black_hispanic_dprime <- dprime(WhiteWhite_black_hispanic_hits, WhiteWhite_black_hispanic_misses, WhiteWhite_black_hispanic_FAs, WhiteWhite_black_hispanic_CRs)
  WhiteWhite_black_hispanic_dprime
  
  # Calculating CR, FA, hit, miss and dprime for WhiteWhite_white
  WhiteWhite_white_hits <- length(which(mura_4$Condition == "WhiteWhite_white" & mura_4$hit == 1))
  WhiteWhite_white_FAs <- length(which(mura_4$Condition == "WhiteWhite_white" & mura_4$FA == 1))
  WhiteWhite_white_CRs <- length(which(mura_4$Condition == "WhiteWhite_white" & mura_4$CR == 1))
  WhiteWhite_white_misses <- length(which(mura_4$Condition == "WhiteWhite_white" & mura_4$miss == 1))
  
  WhiteWhite_white_dprime <- dprime(WhiteWhite_white_hits, WhiteWhite_white_misses, WhiteWhite_white_FAs, WhiteWhite_white_CRs)
  WhiteWhite_white_dprime
  
  # Calculating CR, FA, hit, miss and dprime for CC_black
  CC_black_hispanic_hits <- length(which(mura_4$Condition == "MinorMajor_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMinor_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MinorMajor_hispanic" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMinor_hispanic" & mura_4$hit == 1))
  CC_black_hispanic_FAs <- length(which(mura_4$Condition == "MinorMajor_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMinor_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MinorMajor_hispanic" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMinor_hispanic" & mura_4$FA == 1))
  CC_black_hispanic_CRs <- length(which(mura_4$Condition == "MinorMajor_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMinor_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MinorMajor_hispanic" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMinor_hispanic" & mura_4$CR == 1))
  CC_black_hispanic_misses <- length(which(mura_4$Condition == "MinorMajor_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMinor_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MinorMajor_hispanic" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMinor_hispanic" & mura_4$miss == 1))
  
  CC_black_hispanic_dprime <- dprime(CC_black_hispanic_hits, CC_black_hispanic_misses, CC_black_hispanic_FAs, CC_black_hispanic_CRs)
  CC_black_hispanic_dprime
  
  
  # Calculating CR, FA, hit, miss and dprime for CC_white
  CC_white_hits <- length(which(mura_4$Condition == "MinorMajor_white" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMinor_white" & mura_4$hit == 1))
  CC_white_FAs <- length(which(mura_4$Condition == "MinorMajor_white" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMinor_white" & mura_4$FA == 1))
  CC_white_CRs <- length(which(mura_4$Condition == "MinorMajor_white" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMinor_white" & mura_4$CR == 1))
  CC_white_misses <- length(which(mura_4$Condition == "MinorMajor_white" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMinor_white" & mura_4$miss == 1))
  
  CC_white_dprime <- dprime(CC_white_hits, CC_white_misses, CC_white_FAs, CC_white_CRs)
  CC_white_dprime
  
  # Calculating CR, FA, hit, miss and dprime for SC_black_hispanic
  SC_black_hispanic_hits <- length(which(mura_4$Condition == "MinorMinor_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMajor_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MinorMinor_hispanic" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMajor_hispanic" & mura_4$hit == 1))
  SC_black_hispanic_FAs <- length(which(mura_4$Condition == "MinorMinor_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMajor_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MinorMinor_hispanic" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMajor_hispanic" & mura_4$FA == 1))
  SC_black_hispanic_CRs <- length(which(mura_4$Condition == "MinorMinor_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMajor_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MinorMinor_hispanic" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMajor_hispanic" & mura_4$CR == 1))
  SC_black_hispanic_misses <- length(which(mura_4$Condition == "MinorMinor_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMajor_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MinorMinor_hispanic" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMajor_hispanic" & mura_4$miss == 1))
  
  SC_black_hispanic_dprime <- dprime(SC_black_hispanic_hits, SC_black_hispanic_misses, SC_black_hispanic_FAs, SC_black_hispanic_CRs)
  SC_black_hispanic_dprime
  
  # Calculating CR, FA, hit, miss and dprime for SC_white
  SC_white_hits <- length(which(mura_4$Condition == "MinorMinor_white" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMajor_white" & mura_4$hit == 1))
  SC_white_FAs <- length(which(mura_4$Condition == "MinorMinor_white" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMajor_white" & mura_4$FA == 1))
  SC_white_CRs <- length(which(mura_4$Condition == "MinorMinor_white" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMajor_white" & mura_4$CR == 1))
  SC_white_misses <- length(which(mura_4$Condition == "MinorMinor_white" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMajor_white" & mura_4$miss == 1))
  
  SC_white_dprime <- dprime(SC_white_hits, SC_white_misses, SC_white_FAs, SC_white_CRs)
  SC_white_dprime
  
  # Create dataset with rows as participant and columns as conditions
  Dprime[nrow(Dprime) + 1,] = c(SubjectID,
                                CC_black_hispanic_dprime$dprime, CC_white_dprime$dprime, 
                                SC_black_hispanic_dprime$dprime, SC_white_dprime$dprime,
                                WhiteWhite_black_hispanic_dprime$dprime, WhiteWhite_white_dprime$dprime)
  colnames(Dprime)<-c("Subject ID",
                      "CC Black Hispanic", "CC White", 
                      "SC Black Hispanic", "SC White",
                      "WhiteWhite Black Hispanic", "WhiteWhite White")
}
Dprime_2 <- Dprime[!duplicated(Dprime),]

# Converting to Long Format
Dprime_3 <- Dprime_2 %>% gather(Condition, Value, -`Subject ID`)

# Creating Context and Race Variables
Dprime_3$Context[Dprime_3$Condition=='CC Black Hispanic'] <- 'CC'
Dprime_3$Race[Dprime_3$Condition=='CC Black Hispanic'] <- 'BH'

Dprime_3$Context[Dprime_3$Condition=='CC White'] <- 'CC'
Dprime_3$Race[Dprime_3$Condition=='CC White'] <- 'W'

Dprime_3$Context[Dprime_3$Condition=='SC Black Hispanic'] <- 'SC'
Dprime_3$Race[Dprime_3$Condition=='SC Black Hispanic'] <- 'BH'

Dprime_3$Context[Dprime_3$Condition=='SC White'] <- 'SC'
Dprime_3$Race[Dprime_3$Condition=='SC White'] <- 'W'

Dprime_3$Context[Dprime_3$Condition=='WhiteWhite Black Hispanic'] <- 'WW'
Dprime_3$Race[Dprime_3$Condition=='WhiteWhite Black Hispanic'] <- 'BH'

Dprime_3$Context[Dprime_3$Condition=='WhiteWhite White'] <- 'WW'
Dprime_3$Race[Dprime_3$Condition=='WhiteWhite White'] <- 'W'

# Creating Final Data set
Dprime_Final <- Dprime_3

# Overall Accuracy --------------------------------------------------------

# Setting up the final data frame
correctRates1 <- data.frame(matrix(ncol = 7, nrow = 0))

colnames(correctRates1)<-c("Subject ID",
                           "CC Black Hispanic", "CC White", 
                           "SC Black Hispanic", "SC White",
                           "WhiteWhite Black Hispanic", "WhiteWhite White")

for (i in 1:length(participants)) {
  
  # load dataset
  data_set <- read_csv(file = participants[[i]])
  colnames(data_set)<-c("ExperimentName", "Subject", "Age", "FamHand", "Group", "Handedness", "Sex", "Block", "Counterbalance", "Counterbalance.Cycle", "Counterbalance.Sample", "Hand",	"Procedure[Block]",	"Running[Block]",	"Ss",	"Trial",	"BlockList",	"BlockList.Cycle",	"BlockList.Sample",	"FixDuration[Trial]",	"MajorClips",	"MajorSound",	"MinorClips",	"MinorSound", "MusicCondition", "Procedure[Trial]",	"RightText[Trial]",	"Running[Trial]",	"StimDuration[Trial]",	"WhiteNoise",	"WhiteSound",	"SubTrial",	"BlackFaceList[SubTrial]",	"BufferFaceList",	"Condition[SubTrial]",	"CorrAns1[SubTrial]",	"CorrAns2[SubTrial]",	"CorrAns3[SubTrial]",	"ExpBlock",	"ExpBlock.Cycle",	"ExpBlock.Sample",	"FeedDur[SubTrial]",	"FixDuration[SubTrial]",	"HisFaceList[SubTrial]",	"ITIDuration[SubTrial]",	"PracRecallList",	"PracRecallList.Cycle",	"PracRecallList.Sample",	"PracStudyList",	"PracStudyList.Cycle",	"PracStudyList.Sample",	"Procedure[SubTrial]",	"Race[SubTrial]",	"RandNum[SubTrial]",	"Running[SubTrial]",	"StimDuration[SubTrial]",	"Stimulus[SubTrial]",	"StudySound[SubTrial]",	"TestSound[SubTrial]",	"TestStimulus.ACC[SubTrial]",	"TestStimulus.CRESP[SubTrial]",	"TestStimulus.DurationError[SubTrial]",	"TestStimulus.OnsetDelay[SubTrial]",	"TestStimulus.OnsetTime[SubTrial]",	"TestStimulus.RESP[SubTrial]",	"TestStimulus.RT[SubTrial]",	"TestStimulus.RTTime[SubTrial]",	"LogLevel5",	"BirdSong",	"BirdSongRest",	"BirdSongRest.Cycle",	"BirdSongRest.Sample",	"BlackFaceList[LogLevel5]",	"BlockNum",	"CellNumber",	"Condition[LogLevel5]",	"Confidence",	"CorrAns1[LogLevel5]",	"CorrAns2[LogLevel5]",	"CorrAns3[LogLevel5]", "CR", "FA", "FeedDur[LogLevel5]",	"FixDuration[LogLevel5]",	"HisFaceList[LogLevel5]", "hit",	"ITIDuration[LogLevel5]",	"Judgement", "LeftText[LogLevel5]",	"miss", "Procedure[LogLevel5]", "Race[LogLevel5]", "RandNum[LogLevel5]",	"RecallList",	"RecallList.Cycle",	"RecallList.Sample",	"RightText[LogLevel5]",	"Running[LogLevel5]",	"Stimulus[LogLevel5]",	"StudyFaceList",	"StudyList",	"StudyList.Cycle",	"StudyList.Sample",	"StudyOnset",	"StudySound[LogLevel5]",	"TestSound[LogLevel5]",	"TestStimulus.ACC[LogLevel5]",	"TestStimulus.CRESP[LogLevel5]",	"TestStimulus.RESP[LogLevel5]",	"TestStimulus.RT[LogLevel5]", "TestStimulus.OnsetTime[LogLevel5]",	"TestStimulus.RTTime[LogLevel5]",	"WhiteFaceList")
  mura <- data_set
  SubjectID <- mura[1,2]
  
  # dropping unnecessary columns
  col.keep3 <- c("Subject", "MusicCondition", "Race[LogLevel5]", "hit", "FA", "miss", "CR", "Procedure[SubTrial]")
  mura_short <- mura %>% select(one_of(col.keep3))
  
  # uniting independent variables to make one condition variable
  mura_short_2 <- unite(mura_short, "Condition", c("MusicCondition","Race[LogLevel5]"), sep = "_")
  
  # selecting the rows that measure test blocks
  mura_3 <- subset(mura_short_2, `Procedure[SubTrial]` == "TestBlock", select = , drop = FALSE)
  drop.cols2 <-c("Procedure[SubTrial]")
  mura_4 <- mura_3 %>% select(-one_of(drop.cols2))
  
  # Creating the correct rate for WhiteWhite_black_hispanic
  WhiteWhite_black_hispanic_misses <- length(which(mura_4$Condition == "WhiteWhite_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "WhiteWhite_hispanic" & mura_4$miss == 1))
  WhiteWhite_black_hispanic_Hits <- length(which(mura_4$Condition == "WhiteWhite_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "WhiteWhite_hispanic" & mura_4$hit == 1))
  WhiteWhite_black_hispanic_FAs <- length(which(mura_4$Condition == "WhiteWhite_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "WhiteWhite_hispanic" & mura_4$FA == 1))
  WhiteWhite_black_hispanic_CRs <- length(which(mura_4$Condition == "WhiteWhite_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "WhiteWhite_hispanic" & mura_4$CR == 1))
  WhiteWhite_black_hispanic_correct_rate <- (WhiteWhite_black_hispanic_Hits + WhiteWhite_black_hispanic_CRs) / (WhiteWhite_black_hispanic_Hits + WhiteWhite_black_hispanic_misses + WhiteWhite_black_hispanic_CRs + WhiteWhite_black_hispanic_FAs)
  WhiteWhite_black_hispanic_correct_rate
  
  
  # Creating the correct rate for WhiteWhite_white
  WhiteWhite_white_misses <- length(which(mura_4$Condition == "WhiteWhite_white" & mura_4$miss == 1))
  WhiteWhite_white_Hits <- length(which(mura_4$Condition == "WhiteWhite_white" & mura_4$hit == 1))
  WhiteWhite_white_FAs <- length(which(mura_4$Condition == "WhiteWhite_white" & mura_4$FA == 1))
  WhiteWhite_white_CRs <- length(which(mura_4$Condition == "WhiteWhite_white" & mura_4$CR == 1))
  WhiteWhite_white_correct_rate <- (WhiteWhite_white_Hits + WhiteWhite_white_CRs) / (WhiteWhite_white_Hits + WhiteWhite_white_misses + WhiteWhite_white_FAs + WhiteWhite_white_CRs)
  WhiteWhite_white_correct_rate
  
  # Creating the Hit rate for CC_black_hispanic
  CC_black_hispanic_misses <- length(which(mura_4$Condition == "MinorMajor_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMinor_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MinorMajor_hispanic" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMinor_hispanic" & mura_4$miss == 1))
  CC_black_hispanic_Hits <- length(which(mura_4$Condition == "MinorMajor_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMinor_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MinorMajor_hispanic" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMinor_hispanic" & mura_4$hit == 1))
  CC_black_hispanic_FAs <- length(which(mura_4$Condition == "MinorMajor_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMinor_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MinorMajor_hispanic" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMinor_hispanic" & mura_4$FA == 1))
  CC_black_hispanic_CRs <- length(which(mura_4$Condition == "MinorMajor_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMinor_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MinorMajor_hispanic" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMinor_hispanic" & mura_4$CR == 1))
  CC_black_hispanic_correct_rate <- (CC_black_hispanic_Hits + CC_black_hispanic_CRs) / (CC_black_hispanic_Hits + CC_black_hispanic_misses + CC_black_hispanic_FAs + CC_black_hispanic_CRs)
  CC_black_hispanic_correct_rate
  
  
  # Creating the correct rate for CC_white
  CC_white_misses <- length(which(mura_4$Condition == "MinorMajor_white" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMinor_white" & mura_4$miss == 1))
  CC_white_Hits <- length(which(mura_4$Condition == "MinorMajor_white" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMinor_white" & mura_4$hit == 1))
  CC_white_FAs <- length(which(mura_4$Condition == "MinorMajor_white" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMinor_white" & mura_4$FA == 1))
  CC_white_CRs <- length(which(mura_4$Condition == "MinorMajor_white" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMinor_white" & mura_4$CR == 1))
  CC_white_correct_rate <- (CC_white_Hits + CC_white_CRs) / (CC_white_Hits + CC_white_misses + CC_white_FAs + CC_white_CRs)
  CC_white_correct_rate
  
  # Creating the correct rate for SC_black_hispanic
  SC_black_hispanic_misses <- length(which(mura_4$Condition == "MinorMinor_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMajor_black" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MinorMinor_hispanic" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMajor_hispanic" & mura_4$miss == 1))
  SC_black_hispanic_Hits <- length(which(mura_4$Condition == "MinorMinor_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMajor_black" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MinorMinor_hispanic" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMajor_hispanic" & mura_4$hit == 1))
  SC_black_hispanic_FAs <- length(which(mura_4$Condition == "MinorMinor_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMajor_black" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MinorMinor_hispanic" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMajor_hispanic" & mura_4$FA == 1))
  SC_black_hispanic_CRs <- length(which(mura_4$Condition == "MinorMinor_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMajor_black" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MinorMinor_hispanic" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMajor_hispanic" & mura_4$CR == 1))
  SC_black_hispanic_correct_rate <- (SC_black_hispanic_Hits + SC_black_hispanic_CRs) / (SC_black_hispanic_Hits + SC_black_hispanic_misses + SC_black_hispanic_CRs + SC_black_hispanic_FAs)
  SC_black_hispanic_correct_rate
  
  # Creating the Hit rate for SC_white
  SC_white_misses <- length(which(mura_4$Condition == "MinorMinor_white" & mura_4$miss == 1)) + length(which(mura_4$Condition == "MajorMajor_white" & mura_4$miss == 1))
  SC_white_Hits <- length(which(mura_4$Condition == "MinorMinor_white" & mura_4$hit == 1)) + length(which(mura_4$Condition == "MajorMajor_white" & mura_4$hit == 1))
  SC_white_FAs <- length(which(mura_4$Condition == "MinorMinor_white" & mura_4$FA == 1)) + length(which(mura_4$Condition == "MajorMajor_white" & mura_4$FA == 1))
  SC_white_CRs <- length(which(mura_4$Condition == "MinorMinor_white" & mura_4$CR == 1)) + length(which(mura_4$Condition == "MajorMajor_white" & mura_4$CR == 1))
  SC_white_correct_rate <- (SC_white_Hits + SC_white_CRs) / (SC_white_Hits + SC_white_misses + SC_white_FAs + SC_white_CRs)
  SC_white_correct_rate
  
  # adding a new row to the data frame
  correctRates1[nrow(correctRates1) + 1,] = c(SubjectID,
                                              CC_black_hispanic_correct_rate, CC_white_correct_rate, 
                                              SC_black_hispanic_correct_rate, SC_white_correct_rate,
                                              WhiteWhite_black_hispanic_correct_rate, WhiteWhite_white_correct_rate)
  
  colnames(correctRates1)<-c("Subject ID",
                             "CC Black Hispanic", "CC White", 
                             "SC Black Hispanic", "SC White",
                             "WhiteWhite Black Hispanic", "WhiteWhite White")
}
CorrectRates_2 <- correctRates1[!duplicated(correctRates1),]

# Converting to Long Format
CorrectRates_3 <- CorrectRates_2 %>% gather(Condition, Value, -`Subject ID`)

# Creating Context and Race Variables
CorrectRates_3$Context[CorrectRates_3$Condition=='CC Black Hispanic'] <- 'CC'
CorrectRates_3$Race[CorrectRates_3$Condition=='CC Black Hispanic'] <- 'BH'

CorrectRates_3$Context[CorrectRates_3$Condition=='CC White'] <- 'CC'
CorrectRates_3$Race[CorrectRates_3$Condition=='CC White'] <- 'W'

CorrectRates_3$Context[CorrectRates_3$Condition=='SC Black Hispanic'] <- 'SC'
CorrectRates_3$Race[CorrectRates_3$Condition=='SC Black Hispanic'] <- 'BH'

CorrectRates_3$Context[CorrectRates_3$Condition=='SC White'] <- 'SC'
CorrectRates_3$Race[CorrectRates_3$Condition=='SC White'] <- 'W'

CorrectRates_3$Context[CorrectRates_3$Condition=='WhiteWhite Black Hispanic'] <- 'WW'
CorrectRates_3$Race[CorrectRates_3$Condition=='WhiteWhite Black Hispanic'] <- 'BH'

CorrectRates_3$Context[CorrectRates_3$Condition=='WhiteWhite White'] <- 'WW'
CorrectRates_3$Race[CorrectRates_3$Condition=='WhiteWhite White'] <- 'W'

# Creating Final Data set
CorrectRates_Final <- CorrectRates_3



# Removing Unwanted Data frames -------------------------------------------------------------

# Removing Unwanted data frames
keep <- c("HitRates_Final", "MissRates_Final", "FARates_Final", "CRRates_Final", "AUC_Final", "Dprime_Final", "CorrectRates_Final")
rm(list=setdiff(ls(), keep))


# Analysis -----------------------------------------------

# Two Way Anova for all data sets
Hit_Aov <- aov(Value ~ Race + Context + Race*Context, data = HitRates_Final)
Miss_Aov <- aov(Value ~ Race + Context + Race*Context, data = MissRates_Final)
FA_Aov <- aov(Value ~ Race + Context + Race*Context, data = FARates_Final)
CR_Aov <- aov(Value ~ Race + Context + Race*Context, data = CRRates_Final)
AUC_Aov <- aov(Value ~ Race + Context + Race*Context, data = AUC_Final)
Dprime_Aov <- aov(Value ~ Race + Context + Race*Context, data = Dprime_Final)
Correct_Aov <- aov(Value ~ Race + Context + Race*Context, data = CorrectRates_Final)

# Displaying Anova results
summary(Hit_Aov)
summary(Miss_Aov)
summary(FA_Aov)
summary(CR_Aov)
summary(AUC_Aov)
summary(Dprime_Aov)
summary(Correct_Aov)
  

