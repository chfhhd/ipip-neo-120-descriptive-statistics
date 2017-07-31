library(foreign)

###############
# Import data #
###############

# Note: In the IPIP120.por file, reverse-scored items were recoded
# at the time the respondent completed the inventory, so values for 
# these items can simply be added without recoding when scale 
# scores are computed.

data.all <- read.spss("data/IPIP120.por", to.data.frame = TRUE, use.value.labels = FALSE)


#################
# Data cleaning #                                                    
#################

# Replace all zeros with NA and...
data.all[data.all == 0] <- NA

# ... keep only rows with no NA
data.all <- na.omit(data.all)

# Keep only entries with AGE between 16 and 75
data.all <- data.all[data.all$AGE>=16 & data.all$AGE<76, ]


###################
# Building Facets #
###################

# Neuroticism facets
data.all$ANXIETY             <- c(data.all$I1  + data.all$I31 + data.all$I61  + data.all$I91)
data.all$ANGER               <- c(data.all$I6  + data.all$I36 + data.all$I66  + data.all$I96)
data.all$DEPRESSION          <- c(data.all$I11 + data.all$I41 + data.all$I71  + data.all$I101)
data.all$SELFCONSCIOUSNESS   <- c(data.all$I16 + data.all$I46 + data.all$I76  + data.all$I106)
data.all$IMMODERATION        <- c(data.all$I21 + data.all$I51 + data.all$I81  + data.all$I111)
data.all$VULNERABILITY       <- c(data.all$I26 + data.all$I56 + data.all$I86  + data.all$I116)

# Extraversion facets
data.all$FRIENDLINESS        <- c(data.all$I2  + data.all$I32 + data.all$I62  + data.all$I92)
data.all$GREGARIOUSNESS      <- c(data.all$I7  + data.all$I37 + data.all$I67  + data.all$I97)
data.all$ASSERTIVENESS       <- c(data.all$I12 + data.all$I42 + data.all$I72  + data.all$I102)
data.all$ACTIVITYLEVEL       <- c(data.all$I17 + data.all$I47 + data.all$I77  + data.all$I107)
data.all$EXCITEMENTSEEKING   <- c(data.all$I22 + data.all$I52 + data.all$I82  + data.all$I112)
data.all$CHEERFULNESS        <- c(data.all$I27 + data.all$I57 + data.all$I87  + data.all$I117)

# Openness to experience facets
data.all$IMAGINATION         <- c(data.all$I3  + data.all$I33 + data.all$I63  + data.all$I93)
data.all$ARTISTICINTERESTS   <- c(data.all$I8  + data.all$I38 + data.all$I68  + data.all$I98)
data.all$EMOTIONALITY        <- c(data.all$I13 + data.all$I43 + data.all$I73  + data.all$I103)
data.all$ADVENTUROUSNESS     <- c(data.all$I18 + data.all$I48 + data.all$I78  + data.all$I108)
data.all$INTELLECT           <- c(data.all$I23 + data.all$I53 + data.all$I83  + data.all$I113)
data.all$LIBERALISM          <- c(data.all$I28 + data.all$I58 + data.all$I88  + data.all$I118)

# Agreeableness facets
data.all$TRUST               <- c(data.all$I4  + data.all$I34 + data.all$I64  + data.all$I94)
data.all$MORALITY            <- c(data.all$I9  + data.all$I39 + data.all$I69  + data.all$I99)
data.all$ALTRUISM            <- c(data.all$I44 + data.all$I14 + data.all$I74  + data.all$I104)
data.all$COOPERATION         <- c(data.all$I19 + data.all$I49 + data.all$I79  + data.all$I109)
data.all$MODESTY             <- c(data.all$I24 + data.all$I54 + data.all$I84  + data.all$I114)
data.all$SYMPATHY            <- c(data.all$I29 + data.all$I59 + data.all$I89  + data.all$I119)

# Conscientiousness facets
data.all$SELFEFFICACY        <- c(data.all$I5  + data.all$I35 + data.all$I65  + data.all$I95)
data.all$ORDERLINESS         <- c(data.all$I10 + data.all$I40 + data.all$I70  + data.all$I100)
data.all$DUTIFULNESS         <- c(data.all$I15 + data.all$I45 + data.all$I75  + data.all$I105)
data.all$ACHIEVEMENTSTRIVING <- c(data.all$I50 + data.all$I20 + data.all$I110 + data.all$I80)
data.all$SELFDISCIPLINE      <- c(data.all$I25 + data.all$I55 + data.all$I85  + data.all$I115)
data.all$CAUTIONESS          <- c(data.all$I30 + data.all$I60 + data.all$I90  + data.all$I120)


#####################
# Building Big-Five #
#####################

data.all$NEUROTICISM       <- c(data.all$ANXIETY +
                                  data.all$ANGER +
                                  data.all$DEPRESSION +
                                  data.all$SELFCONSCIOUSNESS +
                                  data.all$IMMODERATION +
                                  data.all$VULNERABILITY
                                 )

data.all$EXTRAVERSION      <- c(data.all$FRIENDLINESS +
                                  data.all$GREGARIOUSNESS +
                                  data.all$ASSERTIVENESS +
                                  data.all$ACTIVITYLEVEL +
                                  data.all$EXCITEMENTSEEKING +
                                  data.all$CHEERFULNESS
                                 )

data.all$OPENESS           <- c(data.all$IMAGINATION +
                                  data.all$ARTISTICINTERESTS +
                                  data.all$EMOTIONALITY +
                                  data.all$ADVENTUROUSNESS +
                                  data.all$INTELLECT +
                                  data.all$LIBERALISM
                                 )

data.all$AGREEABLENESS     <- c(data.all$TRUST + 
                                  data.all$MORALITY +
                                  data.all$ALTRUISM +
                                  data.all$COOPERATION +
                                  data.all$MODESTY +
                                  data.all$SYMPATHY
                                 )

data.all$CONSCIENTIOUSNESS <- c(data.all$SELFEFFICACY +
                                  data.all$ORDERLINESS +
                                  data.all$DUTIFULNESS +
                                  data.all$ACHIEVEMENTSTRIVING +
                                  data.all$SELFDISCIPLINE +
                                  data.all$CAUTIONESS
                                 )


####################
# Building Subsets #
####################

# Subsets USA
data.usa                   <- data.all[grep("USA", data.all$COUNTRY), ]
data.usa.male              <- data.usa[data.usa$SEX==1, ]
data.usa.female            <- data.usa[data.usa$SEX==2, ]

data.usa.16to20            <- data.usa[data.usa$AGE>=16 & data.usa$AGE<21, ]
data.usa.18to35            <- data.usa[data.usa$AGE>=18 & data.usa$AGE<36, ]
data.usa.18to75            <- data.usa[data.usa$AGE>=18 & data.usa$AGE<76, ]
data.usa.21to24            <- data.usa[data.usa$AGE>=21 & data.usa$AGE<25, ]
data.usa.25to29            <- data.usa[data.usa$AGE>=25 & data.usa$AGE<30, ]
data.usa.30to49            <- data.usa[data.usa$AGE>=30 & data.usa$AGE<50, ]
data.usa.50to75            <- data.usa[data.usa$AGE>=50 & data.usa$AGE<76, ]

data.usa.male.16to20       <- data.usa.male[data.usa.male$AGE>=16 & data.usa.male$AGE<21, ]
data.usa.male.18to35       <- data.usa.male[data.usa.male$AGE>=18 & data.usa.male$AGE<36, ]
data.usa.male.18to75       <- data.usa.male[data.usa.male$AGE>=18 & data.usa.male$AGE<76, ]
data.usa.male.21to24       <- data.usa.male[data.usa.male$AGE>=21 & data.usa.male$AGE<25, ]
data.usa.male.25to29       <- data.usa.male[data.usa.male$AGE>=25 & data.usa.male$AGE<30, ]
data.usa.male.30to49       <- data.usa.male[data.usa.male$AGE>=30 & data.usa.male$AGE<50, ]
data.usa.male.50to75       <- data.usa.male[data.usa.male$AGE>=50 & data.usa.male$AGE<76, ]

data.usa.female.16to20     <- data.usa.female[data.usa.female$AGE>=16 & data.usa.female$AGE<21, ]
data.usa.female.18to35     <- data.usa.female[data.usa.female$AGE>=18 & data.usa.female$AGE<36, ]
data.usa.female.18to75     <- data.usa.female[data.usa.female$AGE>=18 & data.usa.female$AGE<76, ]
data.usa.female.21to24     <- data.usa.female[data.usa.female$AGE>=21 & data.usa.female$AGE<25, ]
data.usa.female.25to29     <- data.usa.female[data.usa.female$AGE>=25 & data.usa.female$AGE<30, ]
data.usa.female.30to49     <- data.usa.female[data.usa.female$AGE>=30 & data.usa.female$AGE<50, ]
data.usa.female.50to75     <- data.usa.female[data.usa.female$AGE>=50 & data.usa.female$AGE<76, ]

# Subsets Germany
data.germany               <- data.all[grep("Germany", data.all$COUNTRY), ]
data.germany.male          <- data.usa[data.germany$SEX==1, ]
data.germany.female        <- data.usa[data.germany$SEX==2, ]

data.germany.16to20       <- data.germany[data.germany$AGE>=16 & data.germany$AGE<21, ]
data.germany.18to35       <- data.germany[data.germany$AGE>=18 & data.germany$AGE<36, ]
data.germany.18to75       <- data.germany[data.germany$AGE>=18 & data.germany$AGE<76, ]
data.germany.21to24       <- data.germany[data.germany$AGE>=21 & data.germany$AGE<25, ]
data.germany.25to29       <- data.germany[data.germany$AGE>=25 & data.germany$AGE<30, ]
data.germany.30to49       <- data.germany[data.germany$AGE>=30 & data.germany$AGE<50, ]
data.germany.50to75       <- data.germany[data.germany$AGE>=50 & data.germany$AGE<76, ]

data.germany.male.16to20   <- data.germany.male[data.germany.male$AGE>=16 & data.germany.male$AGE<21, ]
data.germany.male.18to35   <- data.germany.male[data.germany.male$AGE>=18 & data.germany.male$AGE<36, ]
data.germany.male.18to75   <- data.germany.male[data.germany.male$AGE>=18 & data.germany.male$AGE<76, ]
data.germany.male.21to24   <- data.germany.male[data.germany.male$AGE>=21 & data.germany.male$AGE<25, ]
data.germany.male.25to29   <- data.germany.male[data.germany.male$AGE>=25 & data.germany.male$AGE<30, ]
data.germany.male.30to49   <- data.germany.male[data.germany.male$AGE>=30 & data.germany.male$AGE<50, ]
data.germany.male.50to75   <- data.germany.male[data.germany.male$AGE>=50 & data.germany.male$AGE<76, ]

data.germany.female.16to20 <- data.germany.female[data.germany.female$AGE>=16 & data.germany.female$AGE<21, ]
data.germany.female.18to35 <- data.germany.female[data.germany.female$AGE>=18 & data.germany.female$AGE<36, ]
data.germany.female.18to75 <- data.germany.female[data.germany.female$AGE>=18 & data.germany.female$AGE<76, ]
data.germany.female.21to24 <- data.germany.female[data.germany.female$AGE>=21 & data.germany.female$AGE<25, ]
data.germany.female.25to29 <- data.germany.female[data.germany.female$AGE>=25 & data.germany.female$AGE<30, ]
data.germany.female.30to49 <- data.germany.female[data.germany.female$AGE>=30 & data.germany.female$AGE<50, ]
data.germany.female.50to75 <- data.germany.female[data.germany.female$AGE>=50 & data.germany.female$AGE<76, ]