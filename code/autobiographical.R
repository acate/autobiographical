# color_survey_DRAFT.R
# 2019-04-19 adc
# Analyzes csv files submitted by students for color survey

# This here script lives in .../color_survey/code/
# csv files live in .../color_survey/submissions/

library(ggplot2)
library(grid)
library(gridExtra) # for grid.arrange, etc.
library(RColorBrewer)
library(Hmisc) # for rcorr, which gives p-values
library(ez) # Only for code that might be useful for something else



################
# Directories
################

plotDir <- "../plots/"


##################
# Load data
##################

# Create large list with one data frame per list item
dataFiles <- lapply(Sys.glob("../submissions/*.csv"), read.csv)


# Number of individuals who completed survey
nIDs <- length(dataFiles)


# Loop through dataFiles
# Goals are
#   1) Add columns to data frames that might be missing some
#   2) Add column with subject ID number
#   3) Concatenate all df's into one big one

# Define list of df columns that should be there after reading in csv files
csvCols <- c(
  "Memory",
  "AgeRangeLowEnd",
  "Misery.1..Pleasure.7.",
  "NoArousal.1..Arousal.7.",
  "Contexts",
  "Episodes",
  "Feelings",
  "People",
  "Places",
  "Things",
  "Times",
  "otherDetails",
  "YourGender",
  "YourAge",
  "X..AnswerOnce"
)


# # HAND FIX one person's data frame after initial inspection using for loop below
# # df in list position 40 has "Name" column named "X" instead
# colnames(dataFiles[[40]]) <- csvCols


dfCols <- c(
  "MEMORY",
  "AGE",
  "PLEASURE",
  "AROUSAL",
  "CONTEXTS",
  "EPISODES",
  "FEELINGS",
  "PEOPLE",
  "PLACES",
  "THINGS",
  "TIMES",
  "DETAILS",
  "GENDER",
  "YOURAGE",
  "ID"
)

df <- data.frame(matrix(ncol = length(dfCols), nrow = 0))
colnames(df) <- dfCols

for (i in 1:nIDs) {

  # # Find any missing columsn
  # colDiff <- setdiff(csvCols,colnames(dataFiles[[i]]))
  #
  # # Append missing columns; ASSUMES they are all at end
  # if (length(colDiff) > 0) {
  #   for (j in length(colDiff)) {
  #     dataFiles[[i]] <- cbind(dataFiles[[i]],NA)
  #     colnames(dataFiles[[i]])[length(colnames(dataFiles[[i]]))] <- colDiff[j]
  #   }
  # }

  # Chop off final column (doesn't hold any data)
  dataFiles[[i]][,15] <- NULL

  # Fill in rows with gender (only first row originally has value)
  dataFiles[[i]][,13] <- dataFiles[[i]][1,13]

  # Fill in rows with "YourAge" (only first row originally has value)
  dataFiles[[i]][,14] <- dataFiles[[i]][1,14]

  # Add id numbers; just use loop iteration number
  dataFiles[[i]] <- cbind(dataFiles[[i]],i)

  # Fix column names, include name for the new ID column
  colnames(dataFiles[[i]]) <- dfCols

  df <- rbind(df,dataFiles[[i]])

}

df <- df[is.na(df$AGE)==FALSE,]

df$ID <- factor(df$ID)



######################################
# Limit data to subset of age levels
######################################

# only a few responses for AGE == 22, so drop
#   also relatively few for 2, 4

df <- df[!df$AGE %in% c(2,4,22),]

#######################################
# Remove possible outlier participant
#######################################

df <- df[df$ID != 2,]


df <- droplevels(df)





########################
# Make a new variable
########################

# Total number of elements of all 8 kinds
df$ELEMENTS <- rowSums(df[,5:12],na.rm=TRUE)



#########################
# Attach main data frame
#########################

attach(df)



#####################
#####################
# PLOTS
#####################
#####################

# # Capture local environment (variables) to pass to ggplot2 functions
# .e <- environment()


######################
# Histograms of data
######################



####### NOTE!!!!!!!!!!
# Need to type value of nIDs in geom_histogram below
# Anthony couldn't get it working to pass local var. to geom_hist(aes())

# Histogram of number of memories by age
pAgeHist <- ggplot(data=df,aes(AGE)) +
  geom_histogram(aes(y=..count../46),bins=length(unique(AGE))
    ) +
  labs(
    y = "Average count per individual",
    title = "Unique autobiographical memories"
    )


#########################################
svg(
  paste0(plotDir,'histogram_AGE.svg')
)
print(pAgeHist)
dev.off()
#########################################


# pAgeFreqPoly <- ggplot(data=df,aes(AGE)) + geom_freqpoly(bins=length(unique(AGE)))






##################################
# TYPE means by AGE
#   and interactions with GENDER
##################################


dfLong <- reshape(df,
                  varying = c("PLEASURE","AROUSAL","CONTEXTS","EPISODES","FEELINGS","PEOPLE","PLACES","THINGS","TIMES","DETAILS","ELEMENTS"),
                  v.names = "RATING",
                  timevar = "TYPE",
                  times = c("pleasure","arousal","contexts","episodes","feelings","people","places","things","times","details","elements"),
                  direction = "long")
dfLong$TYPE <- factor(dfLong$TYPE)

dfTypeMeans <- aggregate(RATING ~ AGE*TYPE, data=dfLong, FUN = "mean")
dfTypeSE <- aggregate(RATING ~ AGE*TYPE, data=dfLong, FUN = "sd")/sqrt(nIDs)

dfMemoryTypeMeans <- aggregate(RATING ~ MEMORY*TYPE, data=dfLong, FUN = "mean")
dfMemoryTypeSE <- aggregate(RATING ~ MEMORY*TYPE, data=dfLong, FUN = "sd")/sqrt(nIDs)


dfTypeGenderMeans <- aggregate(RATING ~ AGE*TYPE*GENDER, data=dfLong, FUN = "mean")
dfTypeGenderSE <- aggregate(RATING ~ AGE*TYPE*GENDER, data=dfLong, FUN = "sd")/sqrt(nIDs)

# ggplot(data=dfTypeMeans[!dfTypeMeans$TYPE %in% c("arousal","pleasure"),],aes(AGE,RATING,group=TYPE,color=TYPE)) + geom_line()


dfTypeErrBar <- cbind(dfTypeMeans,dfTypeMeans$RATING + dfTypeSE$RATING,dfTypeMeans$RATING - dfTypeSE$RATING)

colnames(dfTypeErrBar) <- c("AGE","TYPE","RATING","upper","lower")


dfMemoryTypeErrBar <- cbind(dfMemoryTypeMeans,dfMemoryTypeMeans$RATING + dfMemoryTypeSE$RATING,dfMemoryTypeMeans$RATING - dfMemoryTypeSE$RATING)

colnames(dfMemoryTypeErrBar) <- c("MEMORY","TYPE","RATING","upper","lower")



dfTypeGenderErrBar <- cbind(dfTypeGenderMeans,dfTypeGenderMeans$RATING + dfTypeGenderSE$RATING,dfTypeGenderMeans$RATING - dfTypeGenderSE$RATING)

colnames(dfTypeGenderErrBar) <- c("AGE","TYPE","GENDER","RATING","upper","lower")



pTypeErrorBar <- ggplot(data=dfTypeErrBar[!dfTypeErrBar$TYPE %in% c("arousal","pleasure","elements"),],aes(AGE,RATING,group=TYPE,color=TYPE)) + geom_errorbar(data=dfTypeErrBar[!dfTypeErrBar$TYPE %in% c("arousal","pleasure","elements"),],mapping = aes(ymin=lower,ymax=upper),width=.1) + geom_line()


pMemoryTypeErrorBar <- ggplot(data=dfMemoryTypeErrBar[!dfMemoryTypeErrBar$TYPE %in% c("arousal","pleasure","elements"),],aes(MEMORY,RATING,group=TYPE,color=TYPE)) + geom_errorbar(data=dfMemoryTypeErrBar[!dfMemoryTypeErrBar$TYPE %in% c("arousal","pleasure","elements"),],mapping = aes(ymin=lower,ymax=upper),width=.1) + geom_line()




pTypeGenderPleasureErrorBar <- ggplot(data=dfTypeGenderErrBar[dfTypeGenderErrBar$TYPE=="pleasure",],aes(AGE,RATING,group=GENDER,color=GENDER)) + geom_errorbar(data=dfTypeGenderErrBar[dfTypeGenderErrBar$TYPE=="pleasure",],mapping = aes(ymin=lower,ymax=upper),width=.1) + geom_line(size=2) + geom_point(size=3) + labs(title= "Pleasure (1 = misery, 7 = pleasure)")

#########################################
svg(
  paste0(plotDir,'PLEASURE_bY_AGE_and_GENDER.svg')
)
print(pTypeGenderPleasureErrorBar)
dev.off()
#########################################





pTypeGenderArousalErrorBar <- ggplot(data=dfTypeGenderErrBar[dfTypeGenderErrBar$TYPE=="arousal",],aes(AGE,RATING,group=GENDER,color=GENDER)) + geom_errorbar(data=dfTypeGenderErrBar[dfTypeGenderErrBar$TYPE=="arousal",],mapping = aes(ymin=lower,ymax=upper),width=.1) + geom_line(size=2) + geom_point(size=3) + labs(title="Arousal(emotional intensity) ratings (scale = 1 to 7)")

#########################################
svg(
  paste0(plotDir,'AROUSAL_bY_AGE_and_GENDER.svg')
)
print(pTypeGenderArousalErrorBar)
dev.off()
#########################################




pTypeGenderElementsErrorBar <- ggplot(data=dfTypeGenderErrBar[dfTypeGenderErrBar$TYPE=="elements",],aes(AGE,RATING,group=GENDER,color=GENDER)) + geom_errorbar(data=dfTypeGenderErrBar[dfTypeGenderErrBar$TYPE=="elements",],mapping = aes(ymin=lower,ymax=upper),width=.1) + geom_line(size=2) + geom_point(size=3) + labs(title="Number of distinct elements per memory")

#########################################
svg(
  paste0(plotDir,'ELEMENTS_bY_AGE_and_GENDER.svg')
)
print(pTypeGenderElementsErrorBar)
dev.off()
#########################################




pElementsErrorBar <- ggplot(data=dfTypeErrBar[dfTypeErrBar$TYPE=="elements",],aes(AGE,RATING)) +
  geom_errorbar(data=dfTypeErrBar[dfTypeErrBar$TYPE=="elements",],mapping = aes(ymin=lower,ymax=upper),width=.1) +
  geom_line(size=2) +
  geom_point(size=3) +
  labs(
    y="Number of elements per memory"
    )

# pMemoryElementsErrorBar <- ggplot(data=dfMemoryTypeErrBar[dfMemoryTypeErrBar$TYPE=="elements",],aes(MEMORY,RATING)) + geom_errorbar(data=dfMemoryTypeErrBar[dfMemoryTypeErrBar$TYPE=="elements",],mapping = aes(ymin=lower,ymax=upper),width=.1) + geom_line()


#########################################
svg(
  paste0(plotDir,'elements_by_AGE.svg')
)
print(pElementsErrorBar)
dev.off()
#########################################






pPleasureErrorBar <- ggplot(data=dfTypeErrBar[dfTypeErrBar$TYPE=="pleasure",],aes(AGE,RATING)) +
  geom_errorbar(data=dfTypeErrBar[dfTypeErrBar$TYPE=="pleasure",],mapping = aes(ymin=lower,ymax=upper),width=.1) +
  geom_line(size=2) +
  geom_point(size=3) +
  labs(
    title="Pleasure (1 = misery, 7 = pleasure)"
  )

#########################################
svg(
  paste0(plotDir,'pleasure_by_AGE.svg')
)
print(pPleasureErrorBar)
dev.off()
#########################################



pArousalErrorBar <- ggplot(data=dfTypeErrBar[dfTypeErrBar$TYPE=="arousal",],aes(AGE,RATING)) +
  geom_errorbar(data=dfTypeErrBar[dfTypeErrBar$TYPE=="arousal",],mapping = aes(ymin=lower,ymax=upper),width=.1) +
  geom_line(size=2) +
  geom_point(size=3) +
  labs(
    title="Arousal(emotional intensity) ratings (scale = 1 to 7)"
  )

#########################################
svg(
  paste0(plotDir,'arousal_by_AGE.svg')
)
print(pArousalErrorBar)
dev.off()
#########################################






#######################
# Mean ratings by TYPE
#######################

dfRatingByTypeByID <- aggregate(RATING ~ TYPE*ID, data=dfLong[!dfLong$TYPE %in% c("arousal","pleasure","elements"),], FUN="mean")

dfRatingByTypeMean <- aggregate(RATING ~ TYPE, data=dfRatingByTypeByID, FUN="mean")
dfRatingByTypeSE <- aggregate(RATING ~ TYPE, data=dfRatingByTypeByID, FUN="sd")
dfRatingByTypeSE$RATING <- dfRatingByTypeSE$RATING/sqrt(nIDs)

# Order TYPE levels by mean rating
dfRatingByTypeMean$TYPE <- reorder(dfRatingByTypeMean$TYPE,dfRatingByTypeMean$RATING)
dfRatingByTypeSE$TYPE <- reorder(dfRatingByTypeSE$TYPE,dfRatingByTypeMean$RATING)


dfRatingByTypeErrBar <- cbind(dfRatingByTypeMean,dfRatingByTypeMean$RATING + dfRatingByTypeSE$RATING,dfRatingByTypeMean$RATING - dfRatingByTypeSE$RATING)

colnames(dfRatingByTypeErrBar) <- c("TYPE","RATING","upper","lower")

pRatingByTypeErrorBar <- ggplot(data=dfRatingByTypeErrBar[!dfRatingByTypeErrBar$TYPE %in% c("arousal","pleasure","elements"),],aes(TYPE,RATING)) + geom_errorbar(data=dfRatingByTypeErrBar[!dfRatingByTypeErrBar$TYPE %in% c("arousal","pleasure","elements"),],mapping = aes(ymin=lower,ymax=upper),width=.1) + geom_point(size = 3) + labs(title="Number of distinct elements per memory by type")

#########################################
svg(
  paste0(plotDir,'TYPE_mean_ratings.svg')
)
print(pRatingByTypeErrorBar)
dev.off()
#########################################




##############################################
# Correlations of different TYPEs of ratings
#   and their changes across AGE
##############################################


uniqueAges <- sort(unique(AGE))

corrArr <- array(NA, c(11,11,length(uniqueAges)))

for (ii in 1:length(uniqueAges)){
  corrArr[,,ii] <- rcorr(
    as.matrix(df[AGE == uniqueAges[ii],c(3:12,16)])
  )$r
}

dimnames(corrArr)[[1]] <- colnames(df[,c(3:12,16)])
dimnames(corrArr)[[2]] <- colnames(df[,c(3:12,16)])
dimnames(corrArr)[[3]] <- uniqueAges


# dfPleasureElementsCorr <- as.data.frame(as.table(corrArr[1,10,]))
# colnames(dfPleasureElementsCorr) <- c("AGE","PleasureElements_r")
#
# dfArousalElementsCorr <- as.data.frame(as.table(corrArr[2,10,]))
# colnames(dfArousalElementsCorr) <- c("AGE","ArousalElements_r")



dfPleasureCorr <- as.data.frame(as.table(corrArr[1,3:10,]))
colnames(dfPleasureCorr) <- c("TYPE","AGE","r")

dfArousalCorr <- as.data.frame(as.table(corrArr[2,3:10,]))
colnames(dfArousalCorr) <- c("TYPE","AGE","r")

dfPlacesCorr <- as.data.frame(as.table(corrArr[7,c(3,4,5,6,8,9,10),]))
colnames(dfPlacesCorr) <- c("TYPE","AGE","r")

dfThingsCorr <- as.data.frame(as.table(corrArr[8,c(3,4,5,6,7,9,10),]))
colnames(dfThingsCorr) <- c("TYPE","AGE","r")

dfPeopleCorr <- as.data.frame(as.table(corrArr[6,c(3,4,5,7,8,9,10),]))
colnames(dfPeopleCorr) <- c("TYPE","AGE","r")

dfContextsCorr <- as.data.frame(as.table(corrArr[3,c(4,5,6,7,8,9,10),]))
colnames(dfContextsCorr) <- c("TYPE","AGE","r")

dfEpisodesCorr <- as.data.frame(as.table(corrArr[4,c(3,5,6,7,8,9,10),]))
colnames(dfEpisodesCorr) <- c("TYPE","AGE","r")

dfTimesCorr <- as.data.frame(as.table(corrArr[9,c(3,4,5,6,7,8,10),]))
colnames(dfTimesCorr) <- c("TYPE","AGE","r")

dfFeelingsCorr <- as.data.frame(as.table(corrArr[5,c(3,4,6,7,8,9,10),]))
colnames(dfFeelingsCorr) <- c("TYPE","AGE","r")

dfDetailsCorr <- as.data.frame(as.table(corrArr[10,c(3:9),]))
colnames(dfDetailsCorr) <- c("TYPE","AGE","r")


# pPleasureElementsCorr <- ggplot(data=dfPleasureElementsCorr,aes(AGE,PleasureElements_r)) + geom_point()
#
# pArousalElementsCorr <- ggplot(data=dfArousalElementsCorr,aes(AGE,ArousalElements_r)) + geom_point()



pPleasureCorr <-  ggplot(data=dfPleasureCorr,aes(AGE,r,group=TYPE,color=TYPE)) + geom_line()

pArousalCorr <- ggplot(data=dfArousalCorr,aes(AGE,r,group=TYPE,color=TYPE)) + geom_line()

pPlacesCorr <- ggplot(data=dfPlacesCorr,aes(AGE,r,group=TYPE,color=TYPE)) + geom_line()

pThingsCorr <- ggplot(data=dfThingsCorr,aes(AGE,r,group=TYPE,color=TYPE)) + geom_line()

pPeopleCorr <- ggplot(data=dfPeopleCorr,aes(AGE,r,group=TYPE,color=TYPE)) + geom_line()

pContextsCorr <- ggplot(data=dfContextsCorr,aes(AGE,r,group=TYPE,color=TYPE)) + geom_line()

pEpisodesCorr <- ggplot(data=dfEpisodesCorr,aes(AGE,r,group=TYPE,color=TYPE)) + geom_line()

pTimesCorr <- ggplot(data=dfTimesCorr,aes(AGE,r,group=TYPE,color=TYPE)) + geom_line()

pFeelingsCorr <- ggplot(data=dfFeelingsCorr,aes(AGE,r,group=TYPE,color=TYPE)) + geom_line()

pDetailsCorr <- ggplot(data=dfDetailsCorr,aes(AGE,r,group=TYPE,color=TYPE)) + geom_line()


# Set by hand after viewing data
commonYLim <- c(-.1,.4)

pPlacesMeanCorr <- ggplot(aggregate(data=dfPlacesCorr,r ~ AGE, FUN="mean"),aes(AGE,r,group=1)) + geom_line() + geom_point() + ylim(commonYLim) + labs(title="PLACES") + labs(y = "mean r")

pThingsMeanCorr <- ggplot(aggregate(data=dfThingsCorr,r ~ AGE, FUN="mean"),aes(AGE,r,group=1)) + geom_line() + geom_point() + ylim(commonYLim) + labs(title="THINGS")

pPeopleMeanCorr <- ggplot(aggregate(data=dfPeopleCorr,r ~ AGE, FUN="mean"),aes(AGE,r,group=1)) + geom_line() + geom_point() + ylim(commonYLim) + labs(title="PEOPLE")

pContextsMeanCorr <- ggplot(aggregate(data=dfContextsCorr,r ~ AGE, FUN="mean"),aes(AGE,r,group=1)) + geom_line() + geom_point() + ylim(commonYLim) + labs(title="CONTEXTS")

pEpisodesMeanCorr <- ggplot(aggregate(data=dfEpisodesCorr,r ~ AGE, FUN="mean"),aes(AGE,r,group=1)) + geom_line() + geom_point() + ylim(commonYLim) + labs(title="EPISODES")

pTimesMeanCorr <- ggplot(aggregate(data=dfTimesCorr,r ~ AGE, FUN="mean"),aes(AGE,r,group=1)) + geom_line() + geom_point() + ylim(commonYLim) + labs(title="TIMES")

pFeelingsMeanCorr <- ggplot(aggregate(data=dfFeelingsCorr,r ~ AGE, FUN="mean"),aes(AGE,r,group=1)) + geom_line() + geom_point() + ylim(commonYLim) + labs(title="FEELINGS")

pDetailsMeanCorr <- ggplot(aggregate(data=dfDetailsCorr,r ~ AGE, FUN="mean"),aes(AGE,r,group=1)) + geom_line() + geom_point() + ylim(commonYLim) + labs(title="DETAILS")

###############################################
svg(
  paste0(plotDir,'TYPE_mean_correlations.svg'),
  width = 16, # this is in inches
  height = 3
)
grid.arrange(grobs = list(pPlacesMeanCorr,pPeopleMeanCorr,pThingsMeanCorr,pContextsMeanCorr,pEpisodesMeanCorr,pTimesMeanCorr,pFeelingsMeanCorr,pDetailsMeanCorr),nrow=1)
dev.off()
###############################################

###############
###############
# ANOVAs
###############
###############


########################################
# One-way ANOVAs of AGE and TYPE levels
#   and two-way interaction
########################################

# NOTE: don't actually need RM here because dfLong is one mean per cell per sub ("ID")

summary(aov(data=dfLong[!dfLong$TYPE %in% c("arousal","pleasure","elements"),],RATING ~ factor(AGE) + Error(1/ID)))

summary(aov(data=dfLong[!dfLong$TYPE %in% c("arousal","pleasure","elements"),],RATING ~ TYPE))

# Currently shows no interaction.  Hmm.
summary(aov(data=dfLong[!dfLong$TYPE %in% c("arousal","pleasure","elements"),],RATING ~ factor(AGE)*TYPE + Error(1/ID)))


#########################################
# Post-hoc tests for above 2-way ANOVA
#########################################

# Currently shows p<0.05 age 12<10; significantly more elements recalled for 10-yr-old memories than 12.
TukeyHSD(aov(data=dfLong[!dfLong$TYPE %in% c("arousal","pleasure","elements"),],RATING ~ factor(AGE)))


#####################
#####################
# Corr. p-vals
#  Between specific D.V.s
#####################
#####################


corrMat <- rcorr(as.matrix(df[,3:12]))

# For checking specific values
# corrMat$r
# corrMat$n
# corrMat$P



# Not quite significant, weak positive
rcorr(df$PLEASURE,df$ELEMENTS)

# Positive, extremely significant
rcorr(df$AROUSAL,df$ELEMENTS)



################################################
# Heatmap of correlations among levels of TYPE
################################################


#########################################
svg(
  paste0(plotDir,'TYPE_correlation_matrix.svg')
)
# Can't save output to var. and print
heatmap(cor(na.omit(df[,5:12])),symm=TRUE,col=brewer.pal(8,"YlOrRd"))
dev.off()
#########################################






############################################################
############################################################
############################################################

stop()

######################################
# Unused code,
#   maybe useful for something else
######################################

aov_PLACES = ezANOVA(
  data = na.omit(df)
  , dv = .(PLACES)
  , wid = .(ID)
  , within = .(AGE)
  , within_full = .(AGE)
)

print(aov_PLACES)

x_PLACES <- ezMixed(
  data = na.omit(df)
  , dv = .(PLACES)
  , random = .(ID)
  , fixed = .(AGE)
)
