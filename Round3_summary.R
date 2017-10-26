#### Qualtrics Prestige Study #####

#Data file for handling the various Qualtrics output for our prestige study. 
#first is just a summary file, followed by the full "info choice" data for the choices ppts made
#Finally need to dig into when the "most copied" or "highest scorer" was chosen

Round3Summary <- read.delim('Round3summary_stuff.txt')

scoreMeans = tapply(Round3Summary$o_score, list(Round3Summary$Condition),mean)
scoreMeans

copyFreqMeans = tapply(Round3Summary$CopyFreq, list(Round3Summary$Condition),mean)
copyFreqMeans
  
copyCopMeans = tapply(Round3Summary$CopCopied, list(Round3Summary$Condition),mean)
copyCopMeans

copyFreqSums = tapply(Round3Summary$CopyFreq, list(Round3Summary$Condition),sum)
copyFreqSums

copyCopSums = tapply(Round3Summary$CopCopied, list(Round3Summary$Condition),sum)
copyCopSums

copyHobSums = tapply(Round3Summary$CopHobs, list(Round3Summary$Condition),sum)
copyHobSums

copyScoreSums = tapply(Round3Summary$CopScore, list(Round3Summary$Condition),sum)
copyScoreSums

#create simple summary table
Condition <- c("A", "B","C") 
CopyTotal <- c(187,107,198)
CopiedCopy <- c(86,62,36)
CopiedHobby <- c(99,45,"na")
CopiedScore <- c("na","na",161)
SummaryInfo <- (data.frame(Condition, CopyTotal, CopiedCopy, CopiedHobby, CopiedScore))

#create secondn summary table to visualise the summary data
Condition <- c("A","A","A","B","B","B","C","C","C") 
CopyInfo<- c("Chobby","Ccopy","Cscore","Chobby","Ccopy","Cscore","Chobby","Ccopy","Cscore")
CopySum <- c(99,86,0,45,62,0,0,36,161)
SummaryInfo2 <- (data.frame(Condition, CopyInfo, CopySum))

Condition <-SummaryInfo2$Condition
CopySum <- SummaryInfo2$CopySum
CopyInfo <- SummaryInfo2$CopyInfo

#plot the summary info
sumPlot <- ggplot(SummaryInfo2, aes(Condition, CopySum, fill= CopyInfo)) +
  stat_summary(fun.y=sum, geom = "bar", position = "dodge") 
sumPlot




#overall score models in rethinking
library(rethinking)


nullModel <- map2stan(
  alist(o_score ~ dnorm(mu, sigma),
        mu <- a,
        a ~ dnorm(0,10),
        sigma ~ dunif(0,10)),
  data=Round3Summary, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(nullModel)

#create condition A as baseline: 
Round3Summary$CondB <- ifelse(Round3Summary$Condition =="B", 1, 0)
Round3Summary$CondC <- ifelse(Round3Summary$Condition =="C", 1, 0)

condModel <- map2stan(
  alist(o_score ~ dnorm(mu, sigma),
        mu <- a + b_b*CondB + b_c*CondC,
        a ~ dnorm(0,10),
        c(b_b,b_c) ~ dnorm(0,1),
        sigma ~ dunif(0,10)),
  data=Round3Summary, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(condModel)
compare(condModel,nullModel)


#overall copyFrequency models:
#rethinking


nullModel_cf <- map2stan(
  alist(CopyFreq ~ dnorm(mu, sigma),
        mu <- a,
        a ~ dnorm(0,10),
        sigma ~ dunif(0,10)),
  data=Round3Summary, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(nullModel_cf)


condModel_cf <- map2stan(
  alist(CopyFreq ~ dnorm(mu, sigma),
        mu <- a + b_b*CondB + b_c*CondC,
        a ~ dnorm(0,10),
        c(b_b,b_c) ~ dnorm(0,1),
        sigma ~ dunif(0,10)),
  data=Round3Summary, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(condModel_cf)
compare(condModel_cf,nullModel_cf)


#overall copyScore models:
#rethinking


nullModel_cS <- map2stan(
  alist(Copied_score ~ dnorm(mu, sigma),
        mu <- a,
        a ~ dnorm(0,10),
        sigma ~ dunif(0,10)),
  data=Round3Summary, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(nullModel_cS)


condModel_cS <- map2stan(
  alist(Copied_score ~ dnorm(mu, sigma),
        mu <- a + b_b*CondB + b_c*CondC,
        a ~ dnorm(0,10),
        c(b_b,b_c) ~ dnorm(0,1),
        sigma ~ dunif(0,10)),
  data=Round3Summary, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(condModel_cS)
compare(condModel_cS,nullModel_cS)


#loading the info choice data isolated from the bigger qualtrics file: 

infoChoiceData <- read.delim('info_choice.txt')

#remove NA columns (questions on which no one copied):
infoChoiceData <- Filter(function(x)!all(is.na(x)), infoChoiceData)

colnames(infoChoiceData)

#reshape the info choice data 
prestigeData <- reshape(infoChoiceData, idvar = "idNum", 
                 varying = list(c(19:236)),
                 v.names = c("infoChoice"), 
                 direction = "long")

# the "time" column now referrs to different questions, 
# although, these have all been named incrementally during reshape and don't relate back to the original Q numbers, hence "Qtype"
# will have to fix this if wanting to control for diff questions (although v few copying events per Q)

colnames(prestigeData)[27] <- "Qtype"

#label the choice column more usefully from the qualtrics output
prestigeData$choice <- ifelse((prestigeData$infoChoice=="Their score on the quiz"),"score",
                          ifelse((prestigeData$infoChoice=="One of their top three hobbies"),"hobbies",
                                 ifelse((prestigeData$infoChoice=="How many times they were copied"),"prestige","NA")))

#now just isolating the data in which copying did occur:

copyData = prestigeData[!prestigeData$choice == "NA",]

copyData$choice_num <- ifelse((copyData$choice =="hobbies"),1,
                              ifelse((copyData$choice =="prestige"),2,3))

copyData$prestige <- ifelse((copyData$choice_num ==2), 1, 0)

#create condition A as baseline: 
copyData$CondB <- ifelse(copyData$Condition =="B", 1, 0)
copyData$CondC <- ifelse(copyData$Condition =="C", 1, 0)

#prestige models

null_prestige <- map2stan(
  alist(prestige ~ dbinom(1,p),
        logit(p) <- a,
        a ~ dnorm(0,10)
  ),
  data=copyData, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(null_prestige)

#prestige model
prestige_model <- map2stan(
  alist(prestige ~ dbinom(1, p),
        logit(p) <- a + b_b*CondB + b_c*CondC,  
        a ~ dnorm(0,10),
        c(b_b, b_c) ~ dnorm(0,4)
  ),
  data=copyData, warmup=1000, iter=6000, chains=1, cores=1 )

precis(prestige_model)
compare(null_prestige,prestige_model)

NParticipants = length(unique(copyData$idNum))
OldID <- copyData$idNum
ParticipantID <- array(0,length(copyData$idNum))
for (index in 1:NParticipants){
  ParticipantID[OldID == unique(OldID)[index]] = index
}
table(ParticipantID)
copyData$ID <- ParticipantID

#multi-level taking ppt into account? (should check for Q too?)
prestige_ml <- map2stan(
  alist(
    prestige ~ dbinom(1, p),
    logit(p) <- a + a_p[ID]*sigma_p +
      b_b*CondB +
      b_c*CondC,
    a ~ dnorm(0,10),
    c(b_b,b_c) ~ dnorm(0,4),
    a_p[ID] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1)
  ),
  data=copyData, constraints=list(sigma_p="lower=0"),
  warmup=1000, iter=2000, chains=1, cores=1)

#didn't initially run because idNum was all higgle-de-piggle-de, needed to use James' magic code (lines 199-206 inserted above)
precis(prestige_ml)

#compare to null multi-level:

null_multi <- map2stan(
  alist(prestige ~ dbinom(1,p),
        logit(p) <- a + a_p[ID]*sigma_p,
        a ~ dnorm(0,10),
        a_p[ID] ~ dnorm(0,1),
        sigma_p ~ dcauchy(0,1)
  ),
  data=copyData, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(null_multi)
compare(null_multi,prestige_ml,prestige_model)

