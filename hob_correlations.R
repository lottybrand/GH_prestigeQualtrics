library(rethinking)

setwd("~/Desktop/Postdoc/Experiments:Ideas/QualtricsStudy/GH_prestigeQualtrics")

hobData <- read.delim("hobbies_and_scores.txt")

Score <- hobData$Overall.Score

hobData$cook <- ifelse(hobData$HOB_NUM1==4, 1, 0)
hobData$read <- ifelse(hobData$HOB_NUM1==13, 1, 0)
hobData$football <- ifelse(hobData$HOB_NUM1==5, 1, 0)
hobData$gaming <- ifelse(hobData$HOB_NUM1==6, 1, 0)

cook <- hobData$cook
read <- hobData$read
football <- hobData$football
gaming <- hobData$gaming

hobbiesModel <- map2stan(
  alist(Score ~ dnorm(mu, sigma),
        mu <- a + b_c*cook + b_r*read + b_f*football * b_g*gaming,
        a ~ dnorm(61,50),
        c(b_c, b_r, b_f, b_g) ~ dnorm(0,10),
        sigma ~ dunif(0,50)),
  data=hobData, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(hobbiesModel)
