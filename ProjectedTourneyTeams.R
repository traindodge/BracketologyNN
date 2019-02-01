TourneyTeams = read.csv("Teams.csv",header=TRUE)

TourneyTeams = TourneyTeams[-c(24,25)]
NNTT = TourneyTeams[2:24]
NTT = TourneyTeams[1]

NTT$School = as.character(NTT$School)
NNTT = data.frame(sapply(NNTT, function(x) as.numeric(as.character(x))))

TrainSet = cbind(NTT,NNTT)
TrainSet = TrainSet[-28,]

Maxs = apply(TrainSet[,c(2:22,24)],2,max)
Mins = apply(TrainSet[,c(2:22,24)],2,min)

Scaled = as.data.frame(scale(TrainSet[,c(2:22,24)], center = Mins, scale = Maxs - Mins))
Scaled = cbind(TrainSet[,1],Scaled)

library(XML)
library(httr)
url = "https://www.sports-reference.com/cbb/seasons/2019-advanced-school-stats.html"
Teams2019 = GET(url)
Teams2019 = readHTMLTable(rawToChar(Teams2019$content))
Teams2019 = Teams2019[[1]]

Teams2019 = Teams2019[ grep("Rk", Teams2019$Rk, invert = TRUE) , ]
Teams2019 = Teams2019[ grep("Home", Teams2019$W, invert = TRUE) , ]

Teams2019 = Teams2019[-c(1,9:14,17)]
Teams2019$Seed = 0

T19NN = Teams2019[2:23]
T19N = Teams2019[1]

T19N$School = as.character(T19N$School)
T19NN = data.frame(sapply(T19NN, function(x) as.numeric(as.character(x))))

Teams2019 = cbind(T19N,T19NN)

Scaled_Now = as.data.frame(scale(Teams2019[c(2:23)], center = Mins, scale = Maxs - Mins))
Scaled_Now = cbind(Teams2019[,1],Scaled_Now)

thenames = colnames(Scaled)
colnames(Scaled_Now) = thenames 

Tourney = matrix(0,length(Scaled_Now$`TrainSet[, 1]`),1)
Tourney = as.data.frame(Tourney)
ProjectedRank = matrix(0,length(Scaled_Now$`TrainSet[, 1]`),1)
ProjectedRank = as.data.frame(ProjectedRank)

for (i in 1:1000){
library(neuralnet)
names = c("W","L","W.L.","SRS","SOS",
          "FTr","X3PAr","TS.","TRB.", 
          "AST.","BLK.","eFG.","TOV.","FT.FGA")
a <- as.formula(paste('Seed ~ ' ,paste(names,collapse='+')))
Network1 <- neuralnet(a, Scaled, hidden = c(7,5,3,2,1), lifesign = "minimal", 
                       linear.output = TRUE, threshold = 0.035, stepmax = 1e6)

test <- subset(Scaled_Now, select = c("W","L","W.L.","SRS","SOS",
                                      "FTr","X3PAr","TS.","TRB.", 
                                      "AST.","BLK.","eFG.","TOV.","FT.FGA"))

Network1.results <- compute(Network1, test)

Tourney[,i] = as.data.frame(c(ProjSeed = Network1.results$net.result[,1]))
ProjectedRank[,i] = rank(Tourney[,i])
}

TourneyAvg = rowMeans(Tourney)
ProjectedRankAvg = rowMeans(ProjectedRank)

colnames(Scaled_Now)[1] = "School"

Proj = cbind(TourneyAvg,ProjectedRankAvg)
Proj = as.data.frame(Proj)
Proj = cbind(Scaled_Now$School,Proj)
Proj$Rank = rank(Proj$ProjectedRankAvg)
Proj$RankMaybeBttr = rank(Proj$TourneyAvg)
colnames(Proj)[1] = "School"

Conf = read.csv("Conferences.csv",header=TRUE)
TeamConf = Conf[2:3]
TeamConf$School = as.character(TeamConf$School)
TeamConf$Conf = as.character(TeamConf$Conf)

TeamConf$Conf = gsub("\\(East)","",TeamConf$Conf)
TeamConf$Conf = gsub("\\(West)","",TeamConf$Conf)

Proj$Conf = TeamConf$Conf[match(Proj$School,TeamConf$School)]
ProjConfWinners = aggregate(Proj$RankMaybeBttr , by=list(Proj$Conf), min)

ProjConfWinners$School = Proj$School[match(ProjConfWinners$x,Proj$RankMaybeBttr)]
colnames(ProjConfWinners) = c("Conf","Rank","School")

Top68 = Proj[(Proj$RankMaybeBttr <= (68 - sum(ProjConfWinners$Rank > 48))),]
Top68 = Top68[c(1,5:6)]
colnames(Top68) = c("School","Rank","Conf")

FinalProjections = rbind(Top68,ProjConfWinners)
FinalProjections = unique(FinalProjections)
FinalProjections = as.data.frame(FinalProjections[order(FinalProjections$Rank),])

SeedNumb = 1:16
Seeds = rep(SeedNumb[1:16],each=4)
Seeds = as.data.frame(Seeds)
last2 = c(11,11,16,16)
last2 = as.data.frame(last2)
colnames(last2)[1] = "Seeds"
Seeds = rbind(Seeds,last2)

Seeds = Seeds[order(Seeds$Seeds),]
Seeds = as.data.frame(Seeds)

FinalProjections$Seed = Seeds$Seeds
FinalProjections$FinalRank = rank(FinalProjections$Rank)

ConfBreakdown = as.data.frame(table(FinalProjections$Conf))

FinalProjections$label = paste(as.character(FinalProjections$Seed), FinalProjections$School)

bracketteams = matrix(0,64,1)
bracketteams = as.data.frame(bracketteams)

bracketteams$V1[1:42] = FinalProjections$label[1:42]
bracketteams$V1[43] = paste(FinalProjections$label[43],FinalProjections$label[46],sep="/")
bracketteams$V1[44] = paste(FinalProjections$label[44],FinalProjections$label[45],sep="/")
bracketteams$V1[45:62] = FinalProjections$label[47:64]
bracketteams$V1[63] = paste(FinalProjections$label[65],FinalProjections$label[68],sep="/")
bracketteams$V1[64] = paste(FinalProjections$label[66],FinalProjections$label[67],sep="/")

bracketteams$V1 = as.character(bracketteams$V1)

d = bracketteams$V1

perregion = c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)

Quad1 = c(1,8,9,16,17,24,25,32,33,40,41,48,49,56,57,64)
Quad2 = c(4,5,12,13,20,21,28,29,36,37,44,45,52,53,60,61)
Quad3 = c(2,7,10,15,18,23,26,31,34,39,42,47,50,55,58,63)
Quad4 = c(3,6,11,14,19,22,27,30,35,38,43,46,51,54,59,62)

Quad1re = matrix(0,1,16)
Quad2re = matrix(0,1,16)
Quad3re = matrix(0,1,16)
Quad4re = matrix(0,1,16)

for (i in 1:length(perregion)){
  Quad1re[i] = Quad1[perregion[i]]
  Quad2re[i] = Quad2[perregion[i]]
  Quad3re[i] = Quad3[perregion[i]]
  Quad4re[i] = Quad4[perregion[i]]
}

bracknumb = cbind(Quad1re,Quad2re,Quad3re,Quad4re)

d1 = matrix(0,1,64)
for (i in 1:length(bracknumb)){
  d1[i] = d[bracknumb[i]]
}


x = c(rep(-6, 32), rep(6, 32), rep(-5, 16), rep(5, 16), rep(-4, 
                                                              8), rep(4, 8), rep(-3, 4), rep(3, 4), c(-2, -2), 2, 2, 
        -1, 1, 0)
y = c(rep(seq(63/64, 1/64, -1/32), 2), rep(seq(31/32, 1/32, 
                                                 -1/16), 2), rep(seq(15/16, 1/16, -1/8), 2), rep(seq(7/8, 
                                                                                                     1/8, -1/4), 2), rep(c(3/4, 1/4), 2), 3/5, 2/5, 1/2)
graphics::plot(NA, xlim = c(-6, 6), ylim = 0:1, xlab = "", 
                 ylab = "", axes = FALSE)
graphics::segments(x - 1/2, y, x + 1/2, y)
par(mar=c(1,1,1,1))
graphics::segments((x + (x < 0) - 1/2)[seq(1, length(x) - 
                                               3, 2)], y[seq(1, length(y) - 3, 2)], (x + (x < 0) - 1/2)[seq(2, 
                                                                                                            length(x) - 3, 2)], y[seq(2, length(y) - 3, 2)])
graphics::text(x[1:64] - 0.46, y[1:64] + 0.01, d1, 
                 cex = 0.59, adj = 0)
  
LastFourIn = as.character(FinalProjections$School[43:46])
Proj = Proj[(order(Proj$RankMaybeBttr)),]
FirstFourOut = as.character(Proj$School[47:50])
print(LastFourIn)
print(FirstFourOut)