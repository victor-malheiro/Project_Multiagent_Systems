install.packages("CreditMetrics")
install.packages("markovchain")
install.packages("bandit")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("hrbrthemes")
install.packages("viridis")
install.packages("ggridges")

library(CreditMetrics)
library(markovchain)
library(bandit)
library(readr)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)


set.seed(1)

#rating classes
RatingClasses <- c("AAA", "AA", "A", "BBB", "BB", "B", "CCC", "D")

#Transition Matrix
TransitionMatrix <- matrix(c(90.710, 8.340, 0.710, 0.075, 0.095, 0.025, 0.022, 0.023,
                             0.710, 90.550, 7.810, 0.720, 0.060, 0.120, 0.020, 0.010,
                             0.092, 2.220, 91.250, 5.420, 0.720, 0.230, 0.011, 0.057,
                             0.020, 0.420, 5.890, 85.880, 5.290, 1.190, 1.140, 0.170,
                             0.036, 0.124, 0.670, 7.730, 80.590, 8.790, 1.010, 1.050,
                             0.011, 0.119, 0.230, 0.440, 6.510, 83.440, 4.060, 5.190,
                             0.220, 0.000, 0.230, 1.330, 2.360, 11.210, 64.830, 19.820,
                             0, 0, 0, 0, 0, 0, 0, 100
                             )/100, 8, 8, dimnames = list(RatingClasses, RatingClasses), byrow = T)

#Markov Chain Model
MCModel <- new("markovchain", states = RatingClasses, transitionMatrix = TransitionMatrix, byrow = T, name = "MarkovChainModel")
MCModel

#credit spread for the migration matrix
#loss given default
LGD <- 0.40

#credit spread for t=1
CreditRiskSpread <- cm.cs(TransitionMatrix, LGD)

#rating of companies
Rating <- c("B", "BB", "CCC")

#Exposure at Default
EAD <- c(4000, 10000, 500000)

#riskless interest rate
Rindex <- 0.02

#value of the credit in one year
RefValue <- cm.ref(TransitionMatrix, LGD, EAD, Rindex, Rating)


absorbingStates(MCModel)




#Pricing Optimization
UsersContacted1 <- c(10000, 9580, 10011, 10007)
UsersContacted2 <- c(12350, 12001, 11950, 12500)
UsersContacted3 <- c(14864, 14990, 14762, 10073)

Purchases1 <- c(571, 579, 563, 312)
Purchases2 <- c(621, 625, 601, 520)
Purchases3 <- c(803, 825, 791, 141)

Prices <- c(299, 306, 312, 335)

#simulation of the posterior distribution for each arm at various prices

PostDistr1Month = sim_post(Purchases1, UsersContacted1, ndraws = 10000)
PostDistr2Month = sim_post(Purchases2, UsersContacted2, ndraws = 10000)
PostDistr3Month = sim_post(Purchases3, UsersContacted3, ndraws = 10000)

ProbWinner1 <- prob_winner(PostDistr1Month)
ProbWinner2 <- prob_winner(PostDistr2Month)
ProbWinner3 <- prob_winner(PostDistr3Month)

names(ProbWinner1) <- Prices
names(ProbWinner2) <- Prices
names(ProbWinner3) <- Prices

#check if the results are statistically significant

significance_analysis(Purchases3, UsersContacted3)

#calculation of the improvement amounts that another arm could have on the current best arm

ValueRemaining <- value_remaining(Purchases3, UsersContacted3)

PotentialValue <- quantile(ValueRemaining, 0.95)

PotentialValue


####EQUITY PORTFOLIOS

#Optimizing equity portfolios

#ETF dataset
dataset = read.csv("ETFs.csv") #Change "MAB/ETFs.csv" for the appropriate path
dataset

summary(dataset)
boxplot(dataset)


##Boxplot with individual data points

ggplot(stack(dataset), aes(x=ind, y=values, fill=ind)) +
  geom_boxplot(alpha = 0.5)+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("ETF Dataset") +
  xlab("")



##Upper confidence bounds

#If the arm returns a reward, the observed average will increase ans the confidence limit will also increase;
#If the arm returns a mistake, the observed average by the arm will decrease, as well as the confidence limit.

#The goal is to select the stock that showed maximum return for the highest number of observations

#Transforms the matrix with monthly returns: max=1; else=0

DataSel<- as.data.frame(matrix(0, nrow = 150, ncol = 6),col.names = Names)
rowmax = apply(dataset,1, max)
for(i in 1:150){
  for(j in 1:6){
    if(dataset[i, j] == rowmax[i])
      DataSel[i, j] <- 1
  }
}

DataSel

NumObs = 150 #Observations: months
NumArms = 6 

ETFSelected = integer() #The ETF selected in the iterative cycle
NumSelections = integer(NumArms) #Number of selections made
RewSum = integer(NumArms)#Sum of rewards for each arm
TotRew = 0 #Total reward obtained

#Two iterative cycles are needed to pass the entire matrix
#First cycle crosses all rows (NumObs), the second works on the columns (NumArms)

for (n in 1:NumObs){ #Rows
  ETF = 0 #For each line, ETF and MaxUpBound are initialized
  MaxUpBound = 0
  for (i in 1:NumArms){ #Columns
    if(NumSelections[i]> 0 ){ #Upperconfidence bound needs two variables computed: 
      AverageReward = RewSum[i]/NumSelections[i] #sum of rewards obtained by the lever
      #i after n plays and the number of time lever i is played by the strategy in the first n plays
      DeltaI  = sqrt(3/2 * log(n)/NumSelections[i]) #Confidence interval after n plays
      UpBound = AverageReward + DeltaI #Selects the lever that returns the maximum UCB 
    } else{ #Checks
      UpBound  = 1e400
    }
    if(UpBound > MaxUpBound){
      MaxUpBound = UpBound
      ETF = i
    }
  } #Updates the values:
  ETFSelected = append(ETFSelected, ETF)
  NumSelections[ETF] = NumSelections[ETF] + 1
  reward = DataSel[n, ETF]
  RewSum[ETF] = RewSum[ETF] + reward
  TotRew = TotRew + reward
}

ETFSelected



sd(dataset$SPY)
sd(dataset$IEF)
sd(dataset$XLF)
sd(dataset$IWM)
sd(dataset$VWO)
sd(dataset$GLD)

mean(dataset$SPY)
mean(dataset$IEF)
mean(dataset$XLF)
mean(dataset$IWM)
mean(dataset$VWO)
mean(dataset$GLD)

#CV (Variation Coefficient)
(sd(dataset$SPY)/mean(dataset$SPY))
(sd(dataset$IEF)/mean(dataset$IEF))
(sd(dataset$XLF)/mean(dataset$XLF))
(sd(dataset$IWM)/mean(dataset$IWM))
(sd(dataset$VWO)/mean(dataset$VWO))
(sd(dataset$GLD)/mean(dataset$GLD))


#plot of the stock that will suffer the shock for the period chosen
months <- c(1:36)
GLD <- data.frame(dataset$GLD[1:36],months)
ggplot(data=GLD, aes(x=months, y=dataset$GLD[1:36], group=1))+geom_line()

XLF <- data.frame(dataset$XLF[1:36],months)
ggplot(data=XLF, aes(x=months, y=dataset$XLF[1:36], group=1))+geom_line()

SPY <- data.frame(dataset$SPY[1:36],months)
ggplot(data=SPY, aes(x=months, y=dataset$SPY[1:36], group=1))+geom_line()


#creation of the dataset with the shocks
dataset2 <- dataset

sd(dataset2$SPY)
sd(dataset2$IEF)
sd(dataset2$XLF)
sd(dataset2$IWM)
sd(dataset2$VWO)
sd(dataset2$GLD)

#Shock for variable SPY
for(i in 1:24){
  if(dataset2$SPY[i]>0){
    dataset2$SPY[i] <- dataset2$SPY[i]*(1-0.3)
  }else{
    dataset2$SPY[i] <- dataset2$SPY[i]*(1+0.3)
  }  
}

SPY2 <- data.frame(dataset2$SPY[1:36],months)
ggplot(data=SPY2, aes(x=months, y=dataset2$SPY[1:36], group=1))+geom_line()


#Shock for variable XLF
for(i in 1:24){
  if(dataset2$XLF[i]>0){
    dataset2$XLF[i] <- dataset2$XLF[i]*(1-0.2)
  }else{
    dataset2$XLF[i] <- dataset2$XLF[i]*(1+0.2)
  }  
}

XLF2 <- data.frame(dataset2$XLF[1:36],months)
ggplot(data=XLF2, aes(x=months, y=dataset2$XLF[1:36], group=1))+geom_line()


#shock for variable GLD
for(i in 1:24){
  if(dataset2$GLD[i]>0){
    dataset2$GLD[i] <- dataset2$GLD[i]*(1-0.5)
  }else{
    dataset2$GLD[i] <- dataset2$GLD[i]*(1+0.5)
  }  
}

GLD2 <- data.frame(dataset2$GLD[1:36],months)
ggplot(data=GLD2, aes(x=months, y=dataset2$GLD[1:36], group=1))+geom_line()

#plot of each variable on both scenarios to compare them
ggplot() + 
  geom_line(data = SPY, aes(x = months, y = dataset$SPY[1:36]), color = "blue") +
  geom_line(data = SPY2, aes(x = months, y = dataset2$SPY[1:36]), color = "red") +
  xlab('months') +
  ylab('SPY Index')

ggplot() + 
  geom_line(data = GLD, aes(x = months, y = dataset$GLD[1:36]), color = "blue") +
  geom_line(data = GLD2, aes(x = months, y = dataset2$GLD[1:36]), color = "red") +
  xlab('months') +
  ylab('GLD Index')

ggplot() + 
  geom_line(data = XLF, aes(x = months, y = dataset$XLF[1:36]), color = "blue") +
  geom_line(data = XLF2, aes(x = months, y = dataset2$XLF[1:36]), color = "red") +
  xlab('months') +
  ylab('XLF Index')



#with crisis
DataSel2<- as.data.frame(matrix(0, nrow = 150, ncol = 6),col.names = Names)
rowmax2 = apply(dataset2,1, max)
for(i in 1:150){
  for(j in 1:6){
    if(dataset2[i, j] == rowmax2[i])
      DataSel2[i, j] <- 1
  }
}



NumObs2 = 150 
NumArms2 = 6 

ETFSelected2 = integer() 
NumSelections2 = integer(NumArms2) 
RewSum2 = integer(NumArms2)
TotRew2 = 0 

for (n in 1:NumObs2){ 
  ETF2 = 0 
  MaxUpBound2 = 0
  for (i in 1:NumArms2){ 
    if(NumSelections2[i]> 0 ){ 
      AverageReward2 = RewSum2[i]/NumSelections2[i] 
      DeltaI2  = sqrt(3/2 * log(n)/NumSelections2[i]) 
      UpBound2 = AverageReward2 + DeltaI2 
    } else{ 
      UpBound2  = 1e400
    }
    if(UpBound2 > MaxUpBound2){
      MaxUpBound2 = UpBound2
      ETF2 = i
    }
  } #Updates the values:
  ETFSelected2 = append(ETFSelected2, ETF2)
  NumSelections2[ETF2] = NumSelections2[ETF2] + 1
  reward2 = DataSel2[n, ETF2]
  RewSum2[ETF2] = RewSum2[ETF2] + reward2
  TotRew2 = TotRew2 + reward2
}

#histogram with shock
hist(ETFSelected2,
     col = 'blue',
     main = 'Histogram of ETFs selections after crisis',
     xlab = 'ETFs',
     ylab = 'Number of times each ETF was selected')

#histogram without shock
hist(ETFSelected,
     col = 'blue',
     main = 'Histogram of ETFs selections',
     xlab = 'ETFs',
     ylab = 'Number of times each ETF was selected')


##OTHER PLOTS

par(mfcol = c(1,3))
hist(dataset$SPY, col = 'blue', main = 'SPY stock', xlab = 'Index', las = 1)
hist(dataset$IEF, col = 'blue', main = 'IEF stock', xlab = 'Index', las = 1)
hist(dataset$XLF, col = 'blue', main = 'XLF stock', xlab = 'Index', las = 1)
par(mfcol = c(1,3))
hist(dataset$IWM, col = 'blue', main = 'IWM stock', xlab = 'Index', las = 1)
hist(dataset$VWO, col = 'blue', main = 'VWO stock', xlab = 'Index', las = 1)
hist(dataset$GLD, col = 'blue', main = 'GLD stock', xlab = 'Index', las = 1)



# Histogram 


ETFSelected
ETF_Sel <- as.data.frame(ETFSelected)

ggplot(ETF_Sel, aes(x=ETFSelected)) +
  geom_histogram(alpha = 0.5, color="#8A41BA",fill="#8A41BA", binwidth=1,boundary=-0.5)+ scale_x_continuous(breaks=1:11)

ggplot(ETF_Sel, aes(x=ETFSelected)) +
  geom_histogram(alpha = 0.5, color="#D93246",fill="#ABABAB", bins=12)


ggplot(df,aes(x))+geom_histogram(binwidth=1,boundary=-0.5)+ scale_x_continuous(breaks=1:11)


##Violin plot

ggplot(stack(dataset), aes(x=ind, y=values, fill=ind)) +
  geom_violin(alpha = 0.5)+
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("ETF Dataset") +
  xlab("")

#Ridgeline plot

ggplot(stack(dataset), aes(x = values, y = ind, fill = ind)) +
  geom_density_ridges(alpha = 0.5) +
  theme_ridges() + 
  theme(legend.position = "none")

#Multi density chart

ggplot(stack(dataset), aes(x=ind, group=ind, fill=ind)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

#ETF dataset - tranformed w/ month
dataset_M = read.csv("ETFs_T.csv") #Change "MAB/ETFs.csv" for the appropriate path
dataset_M

summary(dataset_M)

ggplot(dataset_M, aes(x=Month)) + 
  geom_line(aes(y = SPY), color = "darkred") 


dataset_M <- dataset_M %>%
  select(Month, SPY, IEF, XLF, IWM, VWO, GLD) %>%
  gather(key = "variable", value = "value", -Month)
head(dataset_M)

ggplot(dataset_M, aes(x = Month, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) 








