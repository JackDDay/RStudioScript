#r cookbook + ggplot
#1.17.18

#Libraries


library("psych")
library("ggplot2")

#Import script
import <- function(){
  dataSheet <- read.csv(file.choose(), header=TRUE, na.strings = "NA")
  attach(dataSheet)
}

#Broken

#Import for Excel
#import_excel <- function(){
#excelSheet <- read_excel("C:/Users/Jack/Downloads/PSYC_Data_Summer_2017_Revised.xlsx")
#attach(excelSheet)
#}

#> duration = faithful$eruptions      # the eruption durations 
#> waiting = faithful$waiting         # the waiting interval 
#> plot(duration, waiting,            # plot the variables 
#       +   xlab="Eruption duration",        # xâaxis label 
#       +   ylab="Time waited")              # yâaxis label


##################################################################################################################
#Summary function
summaryFunc <- function(DV, IV){
  print("CBIND Summary")
  M=tapply(DV,IV,mean)
  sd=tapply(DV,IV,sd)
  n=tapply(DV,IV,length)
  se= sd/sqrt(n)
  cbind(mean=M,std.dev=sd,std.error=se, n=n)
  print(cbind(mean=M,std.dev=sd,std.error=se, n=n))
}
##################################################################################################################
#POST HOCS

#For ANOVA Betweeen
ABPostHoc <- function(DV, IV){
  print("Pairwise T Test")
  print(pairwise.t.test(DV,IV))
  
  print("Tukey Test")
  sum <- aov(DV~IV)
  print(TukeyHSD(sum))
}

#For ANOVA Within
AWPostHoc <- function(DV, IV){
  print("Pairwise T Test")
  print(pairwise.t.test(DV,IV,paired=T))
  
  print("Tukey Test")
  sum <- aov(DV~IV, data=dataSheet)
  print(TukeyHSD(sum))
}

##################################################################################################################
#Cronbach's Alpha
#package 'psych' required
#cronbachAlpha <- function(dataSheet){
# print("Cronbach's Alpha Test")
#print(scale = )
#}


##################################################################################################################
#Choose what test you want

#-----------------------------------------------------------------------------------------------------------
# # of IVS    # of Levels of IV   # of DVs    Between or Within Subjects    Statistic Used
#-----------------------------------------------------------------------------------------------------------
# 1           2                   1           Between                       Unpaired t-test
# 1           2                   1           Within                        Paired t-test
# 1           3+                  1           Between                       One-way between subjects ANOVA
# 1           3+                  1           Within                        One-way within subjects ANOVA
# 2           2+ each             1           Between                       Two-way between subjects ANOVA
#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
#   _____     _____  __  __  _____
#  /__   \   /__   \/__\/ _\/__   \
#   / /\/____ / /\/_\  \ \   / /\/
#  / / |_____/ / //__  _\ \ / /  
#  \/        \/  \__/  \__/ \/
#-----------------------------------------------------------------------------------------------------------

#T Test guide
#Alt hypothesis change
#For independent t-test   H1: u1 < u2, choose less
#(aka "unpaired t-test")  H1: u1 > u2, choose greater
#For dependent t-test     H1: uD < 0, choose less
#(aka "paired t-test")    H1: uD > 0. choose greater

IndSampTTest <- function(DV, IV){
  print(t.test(DV~IV, alternative="less", var.equal=T, paired=F))
}

#APA results
#The score was not significantly different between time 1 versus time 2 for participants, [t(4)=Tobt, p=x.xx]
##################################################################################################################
DepSampTTest <- function(DV, IV){
  print(t.test(DV~IV, alternative="less", var.equal=T, paired=T))
}

##################################################################################################################
#-----------------------------------------------------------------------------------------------------------
#   _        __  ___         _  
#   /_\    /\ \ \/___\/\   /\/_\  
# //_\\  /  \/ //  //\ \ / //_\\
#/  _  \/ /\  / \_//  \ V /  _  \
#\_/ \_/\_\ \/\___/    \_/\_/ \_/
#-----------------------------------------------------------------------------------------------------------

#Attitudes towards individuals with disabilities significantly changes over time (F (2, 6) = 13.36, p < 0.05).
#In particular, attitudes were significantly different before training versus one week after training as well before training
#versus six months after training (both pâs < 0.05). However, attitudes were not different between one week after training and
#six months after training (p = 0.06).

OneWayBetweenANOVA <- function(DV, IV){
  #summary
  print("-----------------------------------------------------------------------------------------------------------")
  summary(aov(DV~IV))
  summaryFunc(IV, DV)
  print("-----------------------------------------------------------------------------------------------------------")
  
  #post hoc
  ABPostHoc(DV, IV)
  
  #graph
  psych2111plotL(DV, IV, iv2="none")
}
##################################################################################################################
OneWayWithinANOVA <- function(DV, IV, IV2){
  #steps
  #null/alt hyp
  #alpha, df (k-1)/ N-k (24-3 ex) =   8
  #Fcrit
  
  
  #summary
  print("-----------------------------------------------------------------------------------------------------------")
  print(summary(aov(DV~IV+Error(IV2/IV))))
  summaryFunc(DV, IV)
  print("-----------------------------------------------------------------------------------------------------------")
  
  #post hoc
  AWPostHoc(DV, IV)
  #graph
  psych2111plotL(DV, IV, iv2 = "none")
}
##################################################################################################################
TwoWayBetweenANOVA <- function(DV, IV, IV2){
  #No stated hypothesis
  #a=.05
  #List all three F values and their corresponding p-value
  #Interpret all three findings, starting with main effects, and then finding the interaction
  
  #APA reporting
  #[F(dfIV1, dfRESIDUALS) = f-statistic, p-value]
  #[F(dfIV2, dfRESIDUALS) = f-statistic, p-value]
  #[F(dfINTERACTION, dfRESIDUALS) = f-statistic, p-value]
  #if p <.05 then report as p < .05
  #if p => .05 then report actual value p = 0.12
  
  #There were significant main effects of both age and political affiliation on religiosity, F(1, 2) = 174.22, p < 0.001; F(1, 2) = 223.17, p < 0.001.
  #The interaction between age and political affiliation also significantly influenced religiosity (Interaction effect: F(2,24) = 23.39p < 0.001)
  
  #summary
  print("Summary")
  print(summary(aov(DV~(IV2*IV))))
  
  print("-----------------------------------------------------------------------------------------------------------")
  print("T-Tests")
  t1 = pairwise.t.test(DV,IV)
  t2 = pairwise.t.test(DV,IV2)
  
  print("T1")
  print(t1)
  print("T2")
  print(t2)
  
  #Easy read table
  #DAgemeans = c(26,22,15.4)
  #RAgemeans = c(24,11.6,5.4)
  #Allmeans = rbind(DAgemenas, Ragemeans)
  #Allmeans
  
  #colnames(Allmeans)=c("fifty, "forty", "thirty")
  #Allmeans
  #cbind function
  #post hoc
  #graph
  print("-----------------------------------------------------------------------------------------------------------")
  psych2111plotL(DV, IV, iv2=IV2)
}
##################################################################################################################

# for reporting a correlation:r (df) = X1, p= X2Where X1 is the value of the correlation and X2 is the p value.
#Marker points for determining strength of association:routl = -+ .10 to -+ .29 small (weak); r = -+ .30 to -+ .49
#medium (moderate); r = -+ .50 to -+ 1.0 large (strong)
#Sample APA-formatted Results SectionThe correlation between house size and salary was significant and in the positive direction,
#r(3) = 0.96, p <.05; the strength of the relationship was strong (Cohen, 1988)

correlationEffectSize <- function(DV, IV, n){
  (sum(DV*IV)-((sum(IV)*sum(DV))/n))/(sqrt((sum(IV^2)-((sum(IV)^2)/n))*(sum(DV^2)-((sum(DV)^2)/n))))
}

scatterWRegressionLine <- function(DV, IV){
  plot(DV~IV)
  abline(lm(DV~IV))
  lm(DV~IV)
}

psych2111plotL <- function(dv, iv1, iv2 = "none", lines = T) {
  # Version of 2017-01-30.
  require(ggplot2)
  if(iv2[1] == "none") {
    df1 <- na.omit(data.frame(DV = dv, IV = iv1))
    M <- tapply(df1$DV, df1$IV, mean)
    n <- tapply(df1$DV, df1$IV, length)
    nGroups <- length(n)
    vars <- tapply(df1$DV, df1$IV, var)
    SD <- tapply(df1$DV, df1$IV, sd)
    n <- tapply(df1$DV, df1$IV, length)
    sem <- SD/sqrt(n)
    #sd <- sqrt(sum((n-1)*vars)/(sum(n) - nGroups))
    #sem <- sd/sqrt(sum(n))
    #df <- (sum(vars/n)^2)/sum(((vars/n)^2)/(n-1))
    #width <- -qt(.025,df)*sem
    #data2 <- data.frame(M = M, t = rep(-qt(.025,df),nGroups), df = rep(df,nGroups), width = rep(width,nGroups), indpt.var = levels(data1$IV))
    data2 <- data.frame(M = M, sem = sem, indpt.var = levels(df1$IV))
    View(data2)
    PLOT <- ggplot(data = data2, aes(x = indpt.var, y = M, ymin = M - sem, ymax = M + sem, group=1)) + geom_point(size = 4) + geom_errorbar(width=0.3)
    if(lines == T) {PLOT <- PLOT + geom_line(stat = "identity")}
    PLOT
  }
  else {
    df1 <- na.omit(data.frame(DV = dv, IV1 = iv1, IV2 = iv2))
    M <- vector()
    SD <- vector()
    n <- vector()
    indpt.var2 <- vector()
    nlevels <- length(levels(df1$IV1))
    for(i in 1:length(levels(df1$IV2))) {
      M[((i - 1)*nlevels + 1):(i*nlevels)] <- tapply(df1$DV[df1$IV2 == levels(df1$IV2)[i]], df1$IV1[df1$IV2 == levels(df1$IV2)[i]], mean)
      SD[((i - 1)*nlevels + 1):(i*nlevels)] <- tapply(df1$DV[df1$IV2 == levels(df1$IV2)[i]], df1$IV1[df1$IV2 == levels(df1$IV2)[i]], sd)
      n[((i - 1)*nlevels + 1):(i*nlevels)] <- tapply(df1$DV[df1$IV2 == levels(df1$IV2)[i]], df1$IV1[df1$IV2 == levels(df1$IV2)[i]], length)
      indpt.var2[((i - 1)*nlevels + 1):(i*nlevels)] <- levels(df1$IV2)[i]
    }
    sem <- SD/sqrt(n)
    df2 <- data.frame(M = M, sem = sem, indpt.var1 = levels(df1$IV1), indpt.var2 = indpt.var2)
    View(df2)
    limits <- aes(ymax = M + sem, ymin=M - sem)
    PLOT <- ggplot(data = df2, aes(x = indpt.var1, y = M, ymin = M - sem, ymax = M + sem, colour = indpt.var2, shape = indpt.var2)) + geom_point(size = 4) + geom_errorbar(limits, width=0.5)
    if(lines == T) {PLOT <- PLOT + geom_line(stat = "identity", aes(group = indpt.var2))}
    PLOT
  }
}