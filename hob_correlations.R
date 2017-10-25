library(rethinking)
library(lme4)

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
        a ~ dnorm(61,10),
        c(b_c, b_r, b_f, b_g) ~ dnorm(0,10),
        sigma ~ dunif(0,10)),
  data=hobData, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(hobbiesModel)

hobs <- glm(Score ~ cook + read + football + gaming, family = gaussian)
hobs
confint(hobs)

hobData$cook2 <- ifelse(hobData$HOB_NUM2==1, 1, 0)
hobData$game2 <- ifelse(hobData$HOB_NUM2==2, 1, 0)
hobData$gym2 <- ifelse(hobData$HOB_NUM2==3, 1, 0)
hobData$reading2 <- ifelse(hobData$HOB_NUM2==4, 1, 0)
hobData$running2 <- ifelse(hobData$HOB_NUM2==5, 1, 0)
hobData$walking2 <- ifelse(hobData$HOB_NUM2==6, 1, 0)
hobData$yoga2 <- ifelse(hobData$HOB_NUM2==7, 1, 0)

cook2 <- hobData$cook2
game2 <- hobData$game2 
gym2 <- hobData$gym2 
reading2 <- hobData$reading2 
running2 <- hobData$running2 
walking2 <- hobData$walking2 
yoga2 <- hobData$yoga2

hobs2 <- glm(Score ~ cook2 + game2 + gym2 + reading2 +running2 + walking2 + yoga2, family = gaussian)
hobs2
confint(hobs2)

hobData$game3 <- ifelse(hobData$HOB_NUM3==1, 1, 0)
hobData$swim3 <- ifelse(hobData$HOB_NUM3==2, 1, 0)
hobData$read3 <- ifelse(hobData$HOB_NUM3==3, 1, 0)
hobData$tv3 <- ifelse(hobData$HOB_NUM3==4, 1, 0)
hobData$gym3 <- ifelse(hobData$HOB_NUM3==5, 1, 0)
hobData$knit3 <- ifelse(hobData$HOB_NUM3==6,1,0)

game3 <- hobData$game3
swim3 <- hobData$swim3
read3 <- hobData$read3
tv3 <- hobData$tv3
gym3 <- hobData$gym3
knit3 <- hobData$knit3

hobs3 <- glm(Score ~ game3 + swim3 + read3 + tv3 + gym3 + knit3, family = gaussian)
hobs3
confint(hobs3)

hobDataALL <- read.delim("hobbies_and_scores_all.txt")
Score <- hobDataALL$Overall.Score
CAT <- hobDataALL$hob_cat
ID <- hobDataALL$ID

hobsALL <- glm(Score ~ CAT, family = gaussian)
hobsALL
confint(hobsALL)

hobDataALL$cook <- ifelse(CAT=="B", 1, 0)
hobDataALL$draw <- ifelse(CAT=="C", 1, 0)
hobDataALL$cycle <- ifelse(CAT=="D", 1, 0)
hobDataALL$dance <- ifelse(CAT=="E", 1, 0)
hobDataALL$yoga <- ifelse(CAT=="F", 1, 0)
hobDataALL$tv <- ifelse(CAT=="G", 1, 0)
hobDataALL$game <- ifelse(CAT=="H", 1, 0)
hobDataALL$football <- ifelse(CAT=="I", 1, 0)
hobDataALL$fish <- ifelse(CAT=="J", 1, 0)
hobDataALL$gym <- ifelse(CAT=="K", 1, 0)
hobDataALL$walk <- ifelse(CAT=="L", 1, 0)
hobDataALL$swim <- ifelse(CAT=="M", 1, 0)
hobDataALL$run <- ifelse(CAT=="N", 1, 0)
hobDataALL$garden <- ifelse(CAT=="O", 1, 0)
hobDataALL$read <- ifelse(CAT=="P", 1, 0)

stan_hobData <- hobDataALL[-c(2,4,6,7,8)]
stan_hobData <- stan_hobData[-c(2,4)]
Score <- stan_hobData$Overall.Score
ID <- stan_hobData$ID
cook <- stan_hobData$cook
draw <- stan_hobData$draw
cycle <- stan_hobData$cycle
dance <- stan_hobData$dance
yoga <- stan_hobData$yoga
tv <- stan_hobData$tv
game <- stan_hobData$game
football <- stan_hobData$football
fish <- stan_hobData$fish
gym <- stan_hobData$gym
walk <- stan_hobData$walk
swim <- stan_hobData$swim
run <- stan_hobData$run
garden <- stan_hobData$garden
read <- stan_hobData$read


hobs_all <- map2stan(
  alist(Score ~ dnorm(mu, sigma),
        mu <- a + b_b*cook + b_c*draw + b_d*cycle + b_e*dance + b_f*yoga + b_g*tv + b_h*game + b_i*football + b_j*fish + b_k*gym + b_l*walk + b_m*swim + b_n*run + b_o*garden + b_p*read,
        a ~ dnorm(61,20),
        c(b_b, b_c, b_d, b_e, b_f, b_g, b_h, b_i, b_j, b_k, b_l, b_m, b_n, b_o, b_p) ~ dnorm(0,10),
        sigma ~ dunif(0,50)
  ),
  data=stan_hobData, warmup = 1000, iter = 2000, chains = 1, cores = 1)