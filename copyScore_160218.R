setwd("~/Desktop/Postdoc/QualtricsStudy/GH_prestigeQualtrics")

scrdata <- read.delim("copyScore_16.02.18.txt")

#get rid of the text and rubbish from qualtrics:
newcolumns = list()
pos = 1 #counter for column index of new data frame
for(colindex in 6:105){
  newcolumns[[pos]] = sapply(scrdata[,colindex], function(x) substr(x, 8, 9))
  pos = pos+1
}
print(newcolumns)

#replace the old columns with the new
NEWscrData <- scrdata
NEWscrData[, 6:105] <- newcolumns
scrdata <- NEWscrData

r3 <- read.delim("Round3_prep.txt")
r3 <- r3[,1:6]
r3 <- r3[1:500,]


hscrs <- sapply(split(r3$SCORE, rep(1:(nrow(r3)/5), each=5)), max)
Rhscrs <- rep(hscrs, each=5)
r3$highScore <- Rhscrs
R3 <- r3[,c("QUESTION","highScore")]
R3 <- R3[!duplicated(R3$QUESTION),]


#################################################################
###### Okay, instead, I think we need to 1) turn data to long format
###### 2) merge R3 (add a "highest score" column for each q 1:100)
###### 3) then all we really need is an ifelse function...?? 
##### see previous script, e.g. Round3_summary.R ? 


#### The below was messing up question order..!! Save it for later..!! 
#remove NA columns (questions on which no one copied):
#scrdata <- Filter(function(x)!all(is.na(x)), scrdata)

colnames(scrdata)

#reshape the info choice data 
RESHAPEscrdata <- reshape(scrdata, idvar = "idNum", 
                        varying = list(c(6:105)),
                        v.names = c("choseScore"), 
                        direction = "long")

# the "time" column now referrs to different questions, 
# although, these have all been named incrementally during reshape and don't relate back to the original Q numbers, hence "Qtype"
# will have to fix this if wanting to control for diff questions (although v few copying events per Q)

colnames(RESHAPEscrdata)[6] <- "Qnum"

scrdata <- RESHAPEscrdata

#using Match!
scrdata$Hscore <- R3$highScore[match(scrdata$Qnum, R3$QUESTION)]

#still blanks in choseScore?
scrdata$choseScore <- ifelse(scrdata$choseScore=="",NA,scrdata$choseScore)

#okay can now remove na's
scrdata <- na.omit(scrdata)

scrdata$ChoseHighest <- ifelse(scrdata$choseScore == scrdata$Hscore, 1, 0)

table(scrdata$ChoseHighest)
