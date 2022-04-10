rm(list=ls(all=TRUE))
setwd("/Users/brassjin/Library/Mobile Documents/com~apple~CloudDocs/in progress/it-cleft paper/CODE")
DATA_comp<-read.csv("/Users/brassjin/Library/Mobile Documents/com~apple~CloudDocs/in progress/it-cleft paper/CODE/Surprisal/it-cleft_DATA_COMP.csv",header=T,stringsAsFactors = T)

str(DATA_comp)
summary(DATA_comp)

####################
####################
#plots

#png(filename="SUR_comp.png",width=5680,height=4720,res=720, unit="px")
par(mfrow = c(1, 1), family="Times", mar=c(5,5,1.5,1.5))

#COMP_surprisal
plot(DATA_comp$sur ~ DATA_comp$comp, notch=T, xaxt="n", type="b",
     xlab="COMP", ylab="SURPRISAL", cex.lab=1.5, cex.axis=1.5, legend = F,
     pch=c(21,19,15))
axis(side = 1, at=c(1:2), labels = c("that", "who"), cex.axis=1.5)

#dev.off()

####################
#CASE
#png(filename="COMP*CASE.png",width=11360,height=4720,res=720, unit="px")
par(mfrow = c(1, 2), family="Times", mar=c(5,5,1.5,1.5))

#CASE_surprisal
plot(DATA_comp$sur ~ DATA_comp$case, notch=T, xaxt="n", type="b",
     xlab="CASE", ylab="SURPRISAL", cex.lab=1.5, cex.axis=1.5, legend = F,
     pch=c(21,19,15))
axis(side = 1, at=c(1:2), labels = c("ACC", "NOM"), cex.axis=1.5)

#Case_ntercation
interaction.plot(DATA_comp$case, DATA_comp$comp, DATA_comp$sur, ylim=c(0, 6), xaxt="n", type="b",
                 xlab="COMP-CASE", ylab="SURPRISAL", cex.lab=1.5, cex.axis=1.5, legend = F,
                 pch=c(21,19, 15), lty=c(1,2,3), col=c("black", "red"))
axis(side = 1, at=c(1:2), labels = c("ACC", "NOM"), cex.axis=1.5)
legend("bottom", legend = c("that", "who"),  pch=c(21,19, 15), lty = c(1:2), cex=1.5, bty="n",col=c("black", "red", "blue"))


for (i in 1:2)
{
  CUR=subset(DATA_comp, order.case==i & DATA_comp$comp=="that")
  themean<-mean(CUR$sur)
  thesd<-sd(CUR$sur)
  print(paste("that", i, themean, thesd))
  theerr<-1.96*thesd/10
  theupper<-themean+theerr
  thelower<-themean-theerr
  lines(c(i,i),c(theupper, thelower), col="black", lty=1) 
  lines(c(i-0.1,i+0.1),c(theupper,theupper), col="black", lty=1) 
  lines(c(i-0.1,i+0.1),c(thelower,thelower), col="black", lty=1) 
  
  CUR=subset(DATA_comp, order.case==i & DATA_comp$comp=="who")
  themean<-mean(CUR$sur)
  thesd<-sd(CUR$sur)
  print(paste("who", i, themean, thesd))  
  theerr<-1.96*thesd/10
  theupper<-themean+theerr
  thelower<-themean-theerr
  lines(c(i,i),c(theupper, thelower), col="red", lty=2)
  lines(c(i-0.1,i+0.1),c(theupper,theupper), col="red", lty=2)
  lines(c(i-0.1,i+0.1),c(thelower,thelower), col="red", lty=2)
}

#dev.off()

####################
####################

print("================")
print("default")
print("----------------")
print(bartlett.test(sur~ case, data=DATA_comp))
print(bartlett.test(sur~ comp, data=DATA_comp))
print("------t-test---------")
print(t.test(DATA_comp$sur~DATA_comp$case))
print("------wilcox-test---------")
print(wilcox.test(DATA_comp$sur~DATA_comp$case))
print("------two-way---------")
twoway_comp_case <- aov(sur ~ comp * case, data = DATA_comp)
print(summary(twoway_comp_case), digits=5)
print(model.tables(twoway_comp_case, type="mean"))
print("------posthoc---------")
print(TukeyHSD(twoway_comp_case))
plot(TukeyHSD(twoway_comp_case)) 
