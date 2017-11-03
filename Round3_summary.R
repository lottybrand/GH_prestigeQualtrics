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
compare(null_multi,prestige_ml,prestige_model,null_prestige)
plot(precis(prestige_ml,pars=c("a","b_b","b_c")))
plot(precis(prestige_model))

#copyWho data:
library(gtools)
setwd("~/Desktop/Postdoc/Experiments:Ideas/QualtricsStudy/GH_prestigeQualtrics")
copywhoData <- read.delim('copyWho.txt')

#don't need this when re-named in excel. 
#names(copywhoData) <- gsub("Copied", "copied", names(copywhoData))
#names(copywhoData) <- gsub("copied", "", names(copywhoData))

#remove NA columns (questions on which no one copied):
copywhoData <- Filter(function(x)!all(is.na(x)), copywhoData)

colnames(copywhoData)
Qnums <- names(copywhoData[,colnames(copywhoData)[18:142]])
#reshape the info choice data 
copywhoData_r <- reshape(copywhoData, times = Qnums,
                        varying = list(c(18:142)),
                        v.names = c("copyWho"), 
                        direction = "long")
colnames(copywhoData_r)
colnames(copywhoData_r)[20] <- "Qnums"
copywhoData_r$id <- NULL
copywhoData <- copywhoData_r
copywhoData <- copywhoData[!(copywhoData$copyWho == "" | is.na(copywhoData$copyWho)), ]



#Below Qs, most copied:
mcopied5A <- c("Q1A","Q2A","Q3A","Q8A","QA9",
             "Q10A","Q11A","Q12A","Q13A","Q18A","Q19A",
             "Q20A","Q21A","Q22A","Q23A","Q28A","Q29A",
             "Q30A","Q31A","Q32A","Q33A","Q38A","Q39A",
             "Q40A","Q41A","Q42A","Q43A","Q48A","Q49A",
             "Q50A","Q51A","Q52A","Q53A","Q58A","Q59A",
             "Q60A","Q61A","Q62A","Q63A","Q68A","Q69A",
             "Q70A","Q71A","Q72A","Q73A","Q78A","Q79A",
             "Q80A","Q81A","Q82A","Q83A","Q88A","Q89A",
             "Q90A","Q91A","Q92A","Q93A","Q98A","Q99A",
             "Q100A")

mcopied4A <- c("Q4A", "Q5A", "Q6A","Q7A",
              "Q14A", "Q15A", "Q16A", "Q17A",
              "Q24A", "Q25A", "Q26A", "Q27A",
              "Q34A", "Q35A", "Q36A", "Q37A",
              "Q44A", "Q45A", "Q46A", "Q47A",
              "Q54A", "Q55A", "Q56A", "Q57A", 
              "Q64A", "Q65A", "Q66A", "Q67A",
              "Q74A", "Q75A", "Q76A", "Q77A",
              "Q84A", "Q85A", "Q86A", "Q87A",
              "Q94A", "Q95A", "Q96A", "Q97A")

mcopied6B <- c("Q1B", "Q3B", "Q5B", "Q6B", 
              "Q11B", "Q13B", "Q15B", "Q16B",
              "Q21B", "Q23B", "Q25B", "Q26B",
              "Q31B", "Q33B", "Q35B", "Q36B", 
              "Q41B", "Q43B", "Q45B", "Q46B",
              "Q51B", "Q53B", "Q55B", "Q56B", 
              "Q61B", "Q63B", "Q65B", "Q66B", 
              "Q71B", "Q73B", "Q75B", "Q76B", 
              "Q81B", "Q83B", "Q85B", "Q86B", 
              "Q91B", "Q93B", "Q95B", "Q96B")

mcopied5B <- c("Q2B", "Q7B", "Q9B", 
               "Q12B", "Q17B", "Q19B", 
               "Q22B", "Q27B", "Q29B", 
               "Q32B", "Q37B", "Q39B", 
               "Q42B", "Q47B", "Q49B", 
               "Q52B", "Q57B", "Q59B", 
               "Q62B", "Q67B", "Q69B", 
               "Q72B", "Q77B", "Q79B",
               "Q82B", "Q87B", "Q89B", 
               "Q92B", "Q97B", "Q99B")

mcopied4B <- c("Q4B", "Q8B", 
               "Q10B", "Q14B", "Q18B", 
               "Q20B", "Q24B", "Q28B",
               "Q30B", "Q34B", "Q38B", 
               "Q40B", "Q44B", "Q48B", 
               "Q50B", "Q54B", "Q58B", 
               "Q60B", "Q64B", "Q68B", 
               "Q70B", "Q74B", "Q78B", 
               "Q80B", "Q84B", "Q88B", 
               "Q90B", "Q94B", "Q98B", 
               "Q100B")

mcopied6C <- gsub("B", "C", mcopied6B)
mcopied5C <- gsub("B", "C", mcopied5B)
mcopied4C <- gsub("B", "C", mcopied4B)

mcopied6BC <- c(mcopied6B, mcopied6C)
mcopied5BC <- c(mcopied5B, mcopied5C)
mcopied4BC <- c(mcopied4B, mcopied4C)

copywhoData$mcopied <- ifelse((copywhoData$Qnums%in%mcopied5A & copywhoData$copyWho == "I was copied 5 times"),1,
                              ifelse((copywhoData$Qnums%in%mcopied4A & copywhoData$copyWho =="I was copied 4 times"),1,
                                     ifelse((copywhoData$Qnums%in%mcopied6BC & copywhoData$copyWho =="I was copied 6 times"),1,
                                            ifelse((copywhoData$Qnums%in%mcopied5BC & copywhoData$copyWho =="I was copied 5 times"),1,
                                                   ifelse((copywhoData$Qnums%in%mcopied4BC & copywhoData$copyWho =="I was copied 4 times"),1,0)))))

#thank god for that. now onto the model!                        


NParticipants = length(unique(copywhoData$Idnum))
OldID <- copywhoData$Idnum

ParticipantID <- array(0,length(copywhoData$Idnum))
for (index in 1:NParticipants){
  ParticipantID[OldID == unique(OldID)[index]] = index
}

table(ParticipantID)
copywhoData$ID <- ParticipantID

#create condition A as baseline: 
copywhoData$CondB <- ifelse(copywhoData$Condition =="B", 1, 0)
copywhoData$CondC <- ifelse(copywhoData$Condition =="C", 1, 0)

copywhoData$copyLeast <- ifelse(copywhoData$copyWho == "I was never copied", 1, 0)

write.csv(copywhoData, "copyWhoData_3.11.17.csv")

library(rethinking)

copyMost_ml <- map2stan(
  alist(
    mcopied ~ dbinom(1, p),
    logit(p) <- a + a_p[ID]*sigma_p +
      b_b*CondB +
      b_c*CondC,
    a ~ dnorm(0,10),
    c(b_b,b_c) ~ dnorm(0,4),
    a_p[ID] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1)
  ),
  data=copywhoData, constraints=list(sigma_p="lower=0"),
  warmup=1000, iter=2000, chains=1, cores=1)

precis(copyMost_ml)

null_copyMost_multi <- map2stan(
  alist(mcopied ~ dbinom(1,p),
        logit(p) <- a + a_p[ID]*sigma_p,
        a ~ dnorm(0,10),
        a_p[ID] ~ dnorm(0,1),
        sigma_p ~ dcauchy(0,1)
  ),
  data=copywhoData, warmup = 1000, iter=2000, chains=1, cores = 1)
precis(null_multi)

#null copied 

null_mcopied <- map2stan(
  alist(mcopied ~ dbinom(1,p),
        logit(p) <- a,
        a ~ dnorm(0,10)
  ),
  data=copywhoData, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(null_mcopied)

compare(null_copyMost_multi, copyMost_ml, null_mcopied)
plot(precis(copyMost_ml, pars = c("a","b_b","b_c")))

#copyLeast??

copyLeast_ml <- map2stan(
  alist(
    copyLeast ~ dbinom(1, p),
    logit(p) <- a + a_p[ID]*sigma_p +
      b_b*CondB +
      b_c*CondC,
    a ~ dnorm(0,10),
    c(b_b,b_c) ~ dnorm(0,4),
    a_p[ID] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1)
  ),
  data=copywhoData, constraints=list(sigma_p="lower=0"),
  warmup=1000, iter=2000, chains=1, cores=1)

precis(copyLeast_ml)


null_copyLeast_multi <- map2stan(
  alist(copyLeast ~ dbinom(1,p),
        logit(p) <- a + a_p[ID]*sigma_p,
        a ~ dnorm(0,10),
        a_p[ID] ~ dnorm(0,1),
        sigma_p ~ dcauchy(0,1)
  ),
  data=copywhoData, warmup = 1000, iter=2000, chains=1, cores = 1)
precis(null_copyLeast_multi)



null_Leastcopied <- map2stan(
  alist(copyLeast ~ dbinom(1,p),
        logit(p) <- a,
        a ~ dnorm(0,10)
  ),
  data=copywhoData, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(null_Leastcopied)

compare(null_copyLeast_multi, copyLeast_ml, null_Leastcopied)
plot(precis(null_Leastcopied))
