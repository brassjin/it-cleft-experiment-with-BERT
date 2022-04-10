rm(list=ls(all=TRUE))
setwd("[pathname]")
DATA_case<-read.csv("[pathname]/it-cleft_DATA_CASE.csv", header=T, stringsAsFactors = T)

str(DATA_case)
summary(DATA_case)

####################
####################
#plots

#png(filename="case_surpirsal.png",width=5680,height=4720,res=720, unit="px")
par(family="Times", mar=c(5,5,1.5,1.5))

#CASE_surpirsal
plot(DATA_case$sur ~ DATA_case$case, notch=T, xaxt="n", type="b",
     xlab="CASE", ylab="SURPRISAL", cex.lab=1.5, cex.axis=1.5, legend = F,
     pch=c(21,19,15))
axis(side = 1, at=c(1:2), labels = c("acc", "nom"), cex.axis=1.5)
#dev.off()

####################
#COMP

#png(filename="COMP2.png",width=11360,height=4720,res=720, unit="px")
par(mfrow = c(1, 2), family="Times", mar=c(5,5,1.5,1.5))

#COMP_surprisal
plot(DATA_case$sur ~ DATA_case$comp, notch=T, xaxt="n", type="b",
     xlab="COMP", ylab="SURPRISAL", cex.lab=1.5, cex.axis=1.5, legend = F,
     pch=c(21,19,15))
axis(side = 1, at=c(1:2), labels = c("that", "who"), cex.axis=1.5)

#COMP_interaction
interaction.plot(DATA_case$comp, DATA_case$case, DATA_case$sur, ylim=c(4, 12), xaxt="n", type="b",
                 xlab="COMP", ylab="SURPRISAL", cex.lab=1.5, cex.axis=1.5, legend = F,
                 pch=c(21,19, 15), lty=c(1,2,3), col=c("black", "red"))
axis(side = 1, at=c(1:2), labels = c("that", "who"), cex.axis=1.5)
legend("bottom", legend = c("acc", "nom"),  pch=c(21,19, 15), lty = c(1:2), cex=1.5, bty="n",col=c("black", "red", "blue"))


for (i in 1:2)
{
  CUR=subset(DATA_case, order.comp==i & case=="NOM")
  themean<-mean(CUR$sur)
  thesd<-sd(CUR$sur)
  print(paste("NOM", i, themean, thesd))
  theerr<-1.96*thesd/10
  theupper<-themean+theerr
  thelower<-themean-theerr
  lines(c(i,i),c(theupper, thelower), col="red", lty=2)
  lines(c(i-0.1,i+0.1),c(theupper,theupper), col="red", lty=2)
  lines(c(i-0.1,i+0.1),c(thelower,thelower), col="red", lty=2)

  CUR=subset(DATA_case, order.comp==i & case=="ACC")
  themean<-mean(CUR$sur)
  thesd<-sd(CUR$sur)
  print(paste("ACC", i, themean, thesd))
  theerr<-1.96*thesd/10
  theupper<-themean+theerr
  thelower<-themean-theerr
  lines(c(i,i),c(theupper, thelower), col="black", lty=1)
  lines(c(i-0.1,i+0.1),c(theupper,theupper), col="black", lty=1)
  lines(c(i-0.1,i+0.1),c(thelower,thelower), col="black", lty=1)
}

#dev.off()

####################
#PERSON

#png(filename="PERSON.png",width=11360,height=4720,res=720, unit="px")
par(mfrow = c(1, 2), family="Times", mar=c(5,5,1.5,1.5))

#PER_surprisal
plot(DATA_case$sur ~ DATA_case$per, notch=T, xaxt="n", type="b",
     xlab="PER", ylab="SURPRISAL", cex.lab=1.5, cex.axis=1.5, legend = F,
     pch=c(21,19,15))
axis(side = 1, at=c(1:5), labels = c("1st-plr", "1st-sing", "3rd-plr", "3rd-sing-f","3rd-sing-m"), cex.axis=1)

#PER_surprisal_NOM only
a<-subset(DATA_case, DATA_case$case == "NOM")
plot(a$sur ~ a$per, notch=T, xaxt="n", type="b",
     xlab="PER", ylab="SURPRISAL", cex.lab=1.5, cex.axis=1.5, legend = F,
     pch=c(21,19,15))
axis(side = 1, at=c(1:5), labels = c("1st-plr", "1st-sing", "3rd-plr", "3rd-sing-f","3rd-sing-m"), cex.axis=1)

#dev.off()

####################
##GAP

#png(filename="GAP.png",width=11360,height=4720,res=720, unit="px")
par(mfrow = c(1, 2), family="Times", mar=c(5,5,1.5,1.5))

##GAP_surprisal
plot(DATA_case$sur ~ DATA_case$gap, notch=T, xaxt="n", type="b",
     xlab="GAP", ylab="SURPRISAL", cex.lab=1.5, cex.axis=1.5, legend = F,
     pch=c(21,19,15))
axis(side = 1, at=c(1:2), labels = c("obj", "subj"), cex.axis=1.5)

#GAP_interaction
interaction.plot(DATA_case$gap, DATA_case$case, DATA_case$sur, ylim=c(4, 12), xaxt="n", type="b",
                 xlab="GAP", ylab="SURPRISAL", cex.lab=1.5, cex.axis=1.5, legend = F,
                 pch=c(21,19, 15), lty=c(1,2,3), col=c("black", "red"))
axis(side = 1, at=c(1:2), labels = c("obj", "subj"), cex.axis=1.5)
legend("bottom", legend = c("acc", "nom"),  pch=c(21,19, 15), lty = c(1:2), cex=1.5, bty="n",col=c("black", "red", "blue"))

for (i in 1:2)
{
  CUR=subset(DATA_case, order.gap==i & case=="NOM")
  themean<-mean(CUR$sur)
  thesd<-sd(CUR$sur)
  print(paste("NOM", i, themean, thesd))
  theerr<-1.96*thesd/10
  theupper<-themean+theerr
  thelower<-themean-theerr
  lines(c(i,i),c(theupper, thelower), col="red", lty=2)
  lines(c(i-0.1,i+0.1),c(theupper,theupper), col="red", lty=2)
  lines(c(i-0.1,i+0.1),c(thelower,thelower), col="red", lty=2)

  CUR=subset(DATA_case, order.gap==i & case=="ACC")
  themean<-mean(CUR$sur)
  thesd<-sd(CUR$sur)
  print(paste("ACC", i, themean, thesd))
  theerr<-1.96*thesd/10
  theupper<-themean+theerr
  thelower<-themean-theerr
  lines(c(i,i),c(theupper, thelower), col="black", lty=1)
  lines(c(i-0.1,i+0.1),c(theupper,theupper), col="black", lty=1)
  lines(c(i-0.1,i+0.1),c(thelower,thelower), col="black", lty=1)
}

#dev.off()

####################
####################
#Statistics

#COMP
print("================")
print("default")
print("----------------")
print(bartlett.test(sur~case, data=DATA_case))
print(bartlett.test(sur~comp, data=DATA_case))
print("------t-test---------")
print(t.test(DATA_case$sur~DATA_case$comp))
print("------wilcox-test---------")
print(wilcox.test(DATA_case$sur~DATA_case$comp))
print("------two-way---------")
twoway_case_comp <- aov(sur~case*comp, data = DATA_case)
print(summary(twoway_case_comp), digits = 5)
print(model.tables(twoway_case_comp, type="mean"))
print("------posthoc---------")
print(TukeyHSD(twoway_case_comp))

####################

#PER
print("================")
print("default")
print("----------------")
print(bartlett.test(sur~case, data=DATA_case))
print(bartlett.test(sur~per, data=DATA_case))
print("------two-way---------")
twoway_case_per <- aov(sur~case*per, data = DATA_case)
print(summary(twoway_case_per), digits = 5)
print(model.tables(twoway_case_per, type="mean"))
print("------posthoc---------")
print(TukeyHSD(twoway_case_per))

####################

#GAP
print("================")
print("default")
print("----------------")
print(bartlett.test(sur~case, data=DATA_case))
print(bartlett.test(sur~gap, data=DATA_case))
print("------t-test---------")
print(t.test(DATA_case$sur~DATA_case$gap))
print("------wilcox-test---------")
print(wilcox.test(DATA_case$sur~DATA_case$gap))
print("------two-way---------")
twoway_comp_case <- aov(sur~case*gap, data = DATA_case)
print(summary(twoway_comp_case), digits = 5)
print(model.tables(twoway_comp_case, type="mean"))
print("------posthoc---------")
print(TukeyHSD(twoway_comp_case))
