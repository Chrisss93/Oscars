require(Hmisc)
require(bootStepAIC)
require(rms)
require(ggplot2)
require(BMA)
setwd("~/Desktop/Project/MATH 396")
main<-read.csv('Master4.csv')
main$other.noms[is.na(main$other.noms)==T]<-0
main$other.wins[is.na(main$other.wins)==T]<-0

win<-subset(main,Won==1)
#Exploratory Analysis
#Characteristics
a<-round(aggregate(main,by=list(main$Won),mean,na.rm=T),digits=3)
a<-a[,c(15:19,14,21,42,30,31,33,47,37,38,34,43:46,22:25,35,36,12,13)]
colnames(a)[c(6:10,13,14,20:25)]<-c('Runtime (min.)','TIFF premiere','Nov./Dec. release',
                                    'Domestic.Gross (million USD)','Budget (million USD)',
                                    'Hot topic','Dark topic','BAFTA win','Globes win',
                                    'SAG win','DGA win','WGA win','ACE Eddie win')
a<-data.frame(t(a))
latex(a,colheads=c('Nominees','Winners'),n.rgroup=c(5,9,5,8),rowlabel="",file=" ",
      rgroup=c('Genre','Film Traits','Nominee Traits','Precursor Awards'),table.env=F,
      insert.bottom='Films classified as "Hot Topic" contain the following keywords: "racism, slavery, gay rights, abortion, poverty, aids, hiv or homophobia" \\
      \\
      Films classified as "Dark Topic" contain the following keywords: "terrorism, rape, gang violence, sex trade, human trafficking, torture, heroin, cocaine, serial killer, mass murderer"')


#Divide up the main dataset
df.actor<-subset(main,award=="Best Actor")
df.actor<-df.actor[order(df.actor$Year),]
df.actress<-subset(main,award=="Best Actress")
df.actress<-df.actress[order(df.actress$Year),]
df.sactor<-subset(main,award=="Best Supporting Actor")
df.sactor<-df.sactor[order(df.sactor$Year),]
df.sactress<-subset(main,award=="Best Supporting Actress")
df.sactress<-df.sactress[order(df.sactress$Year),]
df.director<-subset(main,award=="Best Director")
df.director<-df.director[order(df.director$Year),]
df.picture<-subset(main,award=="Best Picture")
df.picture<-df.picture[order(df.picture$Year),]

#How good are preceding awards?
award2<-aggregate(win,by=list(win$award),mean,na.rm=T)
award2<-award2[c(1,2,5,6,3,4),c('globes','bafta','sag','critic','dga','eddie','wga')]
award2[c(5,6),3]<-NA
award2[c(1:4),c(5:7)]<-NA
latex(round(award2[,c(1:4)],digits=2),rowlabel="Oscar Won",
      rowname=c('Actor','Actress','Actor','Actress','Director','Picture'),
      file=" ",n.rgroup=c(2,2,2),rgroup=c('Leading','Supporting',''),
      colheads=c('Globes','BAFTA','SAG',"Critics'"),table.env=F)
latex(round(award2[c(5:6),-c(1:4)],digits=2),rowlabel='Oscar Won',
      rowname=c('Director','Picture'),file="",table.env=F,
      colheads=c('DGA','Eddie','WGA'))

#Power of concensus
(subset(df.actor,bafta==1&globes==1&sag==1&critic==1))
(subset(df.actress,bafta==1&globes==1&sag==1&critic==1))
(subset(df.sactor,bafta==1&globes==1&sag==1&critic==1))
(subset(df.sactress,bafta==1&globes==1&sag==1&critic==1))
(subset(df.director,dga==1&eddie==1))
(subset(df.picture,dga==1&eddie==1))
#Prediction Models
summary(glm(Won~globes+bafta+script.nom+Hot,df.actor,family=binomial(logit)))
summary(glm(Won~globes+bafta+script.nom+Hot,df.actress,family=binomial(logit)))
summary(glm(Won~globes+bafta+script.nom+Hot,df.sactor,family=binomial(logit)))
summary(glm(Won~globes+bafta+script.nom+Hot,df.sactress,family=binomial(logit)))
summary(glm(Won~globes+bafta+dga+eddie+Hot,df.director,family=binomial(logit)))
summary(glm(Won~globes+bafta+dga+wga+eddie+Hot,df.picture,family=binomial(logit)))




#For STEPWISE METHOD
df.actor$past.nom[is.na(df.actor$past.nom)==T]<-0
df.actor$past.win[is.na(df.actor$past.win)==T]<-0
df.actress$past.nom[is.na(df.actress$past.nom)==T]<-0
df.actress$past.win[is.na(df.actress$past.win)==T]<-0
df.sactress$past.nom[is.na(df.sactress$past.nom)==T]<-0
df.sactress$past.win[is.na(df.sactress$past.win)==T]<-0
df.director$past.nom[is.na(df.director$past.nom)==T]<-0
df.director$past.win[is.na(df.director$past.win)==T]<-0

r<-(glm(Won~globes+bafta+script.nom+Hot+Dark+past.win+past.nom+eddie+
          adapted+drama+biopic+comedy+rom+action+
          fall+picture.nom+edit.nom+dga+wga,df.actor,family=binomial(logit)))
r2<-(glm(Won~globes+bafta+script.nom+Hot+Dark+past.win+past.nom+eddie+
           adapted+drama+biopic+comedy+rom+action+
           fall+picture.nom+edit.nom+dga+wga,df.actress,family=binomial(logit)))
r3<-(glm(Won~globes+bafta+script.nom+Hot+Dark+past.win+past.nom+eddie+
           adapted+drama+biopic+comedy+rom+action+
           fall+picture.nom+edit.nom+dga+wga,df.sactor,family=binomial(logit)))
r4<-(glm(Won~globes+bafta+script.nom+Hot+Dark+past.win+past.nom+eddie+
           adapted+drama+biopic+comedy+rom+action+
           fall+picture.nom+edit.nom+dga+wga,df.sactress,family=binomial(logit)))
r5<-(glm(Won~globes+bafta+script.nom+Hot+Dark+past.win+past.nom+eddie+
           adapted+drama+biopic+comedy+rom+action+
           fall+edit.nom+dga+wga,df.director,family=binomial(logit)))
r6<-(glm(Won~globes+bafta+script.nom+Hot+Dark+eddie+direct.nom+
           adapted+drama+biopic+comedy+rom+action+
           fall+edit.nom+dga+wga,df.picture,family=binomial(logit)))
#BIC STEP actor
bic.step.actor<-stepAIC(r,df.actor,direction='backward',k=log(nrow(df.actor)))
#AIC STEP actor
aic.step.actor<-stepAIC(r,df.actor,direction='backward',k=2)
#BIC STEP actress
bic.step.actress<-stepAIC(r2,df.actress,direction='backward',k=log(nrow(df.actress)))
#AIC STEP actress
aic.step.actress<-stepAIC(r2,df.actor,direction='backward',k=2)
aic.step.actress<-update(aic.step.actress,.~.-action)
#BIC STEP supporting actor
bic.step.sactor<-stepAIC(r3,df.sactor,direction='backward',k=log(nrow(df.sactor)))
#AIC STEP supporting actor
aic.step.sactor<-stepAIC(r3,df.sactor,direction='backward',k=2)
#BIC STEP supporting actress
bic.step.sactress<-stepAIC(r4,df.sactress,direction='backward',k=log(nrow(df.sactress)))
#AIC STEP supporting actress
aic.step.sactress<-stepAIC(r4,df.sactress,direction='backward',k=2)
#BIC STEP director
bic.step.director<-stepAIC(r5,df.director,direction='backward',k=log(nrow(df.director)))
#AIC STEP director
aic.step.director<-stepAIC(r5,df.director,direction='backward',k=2)
#BIC STEP picture
bic.step.picture<-stepAIC(r6,df.picture,direction='backward',k=log(nrow(df.picture)))
#AIC STEP picture
aic.step.picture<-stepAIC(r6,df.picture,direction='backward',k=2)

#FOR BEST SUBSET METHOD
data<-df.actor[,c('globes','bafta','script.nom','Hot','Dark','past.win','past.nom','eddie',
                  'other.wins','other.noms','adapted','drama','biopic','comedy','rom','action',
                  'fall','picture.nom','edit.nom','dga','wga','age','Won')]
data2<-df.actress[,c('globes','bafta','script.nom','Hot','Dark','past.win','past.nom','eddie',
                     'other.wins','other.noms','adapted','drama','biopic','comedy','rom','action',
                     'fall','picture.nom','edit.nom','dga','wga','age','Won')]
data3<-df.sactor[,c('globes','bafta','script.nom','Hot','Dark','past.win','past.nom','eddie',
                    'other.wins','other.noms','adapted','drama','biopic','comedy','rom','action',
                    'fall','picture.nom','edit.nom','dga','wga','age','Won')]
data4<-df.sactress[,c('globes','bafta','script.nom','Hot','Dark','past.win','past.nom','eddie',
                      'other.wins','other.noms','adapted','drama','biopic','comedy','rom','action',
                      'fall','picture.nom','edit.nom','dga','wga','age','Won')]
data5<-df.director[,c('globes','bafta','script.nom','Hot','Dark','past.win','past.nom','eddie',
                      'other.wins','other.noms','adapted','drama','biopic','comedy','rom','action',
                      'fall','picture.nom','edit.nom','dga','wga','age','Won')]
data6<-df.picture[,c('globes','bafta','script.nom','Hot','Dark','eddie',
                     'other.wins','other.noms','adapted','drama','biopic','comedy','rom','action',
                     'fall','edit.nom','dga','wga','Won')]
best.actor<-bestglm(data,IC='BIC')
best.actress<-bestglm(data2,IC='BIC')
best.sactor<-bestglm(data3,IC='BIC')
best.sactress<-bestglm(data4,IC='BIC')
best.director<-bestglm (data5,IC='BIC')
best.picture<-bestglm(data6,IC='BIC')

#Now try using AICc or second order AIC which performs better for small samples
a<-get.models(dredge(r,rank=AICc),1)[[1]]

lm(y~x,data)

#Printing Regression Tables
#I must add the AUC ROC after tex conversion using lroc()$auc and BIC() for 
round(c(lroc(aic.step.actor)$auc,lroc(aic.step.actress)$auc,lroc(aic.step.sactor)$auc,
        lroc(aic.step.sactress)$auc,lroc(aic.step.director)$auc,lroc(aic.step.picture)$auc),digits=3)


#BIC MODELS
stargazer(bic.step.actor,bic.step.actress,bic.step.sactor,bic.step.sactress,bic.step.director,bic.step.picture,
          align=TRUE,dep.var.labels='Won Oscar',model.numbers=FALSE,font.size='footnotesize',
          column.labels=c('Best Actor','Best Actress','Best Supporting Actor',
                          'Best Supporting Actress','Best Director','Best Picture'),
          float=F,ci=T,apply.coef=exp,p.auto=F,keep.stat=c("n","lr"),
          ci.custom=list(exp(confint(bic.step.actor)),exp(confint(bic.step.actress)),
                         exp(confint(bic.step.sactor)),exp(confint(bic.step.sactress)),
                         exp(confint(bic.step.director)),exp(confint(bic.step.picture))))
#AIC MODELS
stargazer(aic.step.actor,aic.step.actress,aic.step.sactor,aic.step.sactress,aic.step.director,aic.step.picture,
          align=TRUE,dep.var.labels='Won Oscar',model.numbers=FALSE,font.size='footnotesize',
          column.labels=c('Best Actor','Best Actress','Best Supporting Actor',
                          'Best Supporting Actress','Best Director','Best Picture'),
          float=F,ci=T,apply.coef=exp,p.auto=F,keep.stat=c('n','aic','bic'),no.space=T,
          ci.custom=list(exp(confint(aic.step.actor)),exp(confint(aic.step.actress)),
                         exp(confint(aic.step.sactor)),exp(confint(aic.step.sactress)),
                         exp(confint(aic.step.director)),exp(confint(aic.step.picture))))
#BEST BIC SUBSET MODELS (don't bother)

#VALIDATION
df.actor$prob<-aic.step.actor$fitted
df.actress$prob<-aic.step.actress$fitted
df.sactor$prob<-aic.step.sactor$fitted
df.sactress$prob<-aic.step.sactress$fitted
df.director$prob<-aic.step.director$fitted
df.picture$prob<-aic.step.picture$fitted

test<-by(df.actor,df.actor$Year,function(x){ifelse(x$prob==max(x$prob),1,0)})
df.actor<-cbind(df.actor[order(df.actor$Year),],'Prediction'=unlist(test))
test<-by(df.actress,df.actress$Year,function(x){ifelse(x$prob==max(x$prob),1,0)})
df.actress<-cbind(df.actress[order(df.actress$Year),],'Prediction'=unlist(test))
test<-by(df.sactor,df.sactor$Year,function(x){ifelse(x$prob==max(x$prob),1,0)})
df.sactor<-cbind(df.sactor[order(df.sactor$Year),],'Prediction'=unlist(test))
test<-by(df.sactress,df.sactress$Year,function(x){ifelse(x$prob==max(x$prob),1,0)})
df.sactress<-cbind(df.sactress[order(df.sactress$Year),],'Prediction'=unlist(test))
test<-by(df.director,df.director$Year,function(x){ifelse(x$prob==max(x$prob),1,0)})
df.director<-cbind(df.director[order(df.director$Year),],'Prediction'=unlist(test))
test<-by(df.picture,df.picture$Year,function(x){ifelse(x$prob==max(x$prob),1,0)})
df.picture<-cbind(df.picture[order(df.picture$Year),],'Prediction'=unlist(test))

#Confusion Matrices
latex(confusionMatrix(df.actor$Prediction,df.actor$Won)$table,rowlabel="",
      rowname=c('Lost','Won'),n.rgroup=c(2),rgroup='Predicted',colheads='\\textbf{Real}',
      extracolheads=c('Lost','Won'),extracolsize='normalsize',file="")
latex(confusionMatrix(df.actress$Prediction,df.actress$Won)$table,rowlabel="",
      rowname=c('Lost','Won'),n.rgroup=c(2),rgroup='Predicted',colheads='\\textbf{Real}',
      extracolheads=c('Lost','Won'),extracolsize='normalsize',file="")
latex(confusionMatrix(df.sactor$Prediction,df.sactor$Won)$table,rowlabel="",
      rowname=c('Lost','Won'),n.rgroup=c(2),rgroup='Predicted',colheads='\\textbf{Real}',
      extracolheads=c('Lost','Won'),extracolsize='normalsize',file="")
confusionMatrix(df.actress$Prediction,df.actress$Won)
confusionMatrix(df.sactor$Prediction,df.sactor$Won)
confusionMatrix(df.sactress$Prediction,df.sactress$Won)
confusionMatrix(df.director$Prediction,df.director$Won)
confusionMatrix(df.picture$Prediction,df.picture$Won)


#MANUAL FUCKING JACKKNIFE SINCE i cant find it anywhere else
output<-matrix(data=0,ncol=1,nrow=length(unique(df.actor$Year)))
for(i in 1:length(unique(df.actor$Year))){
  #Drop out the i-th year from the dataframe for training set
  train<-df.actor[!df.actor$Year==unique(df.actor$Year)[i],]
  #Recover only the i-th year from the dataframe for validation set
  valid<-df.actor[df.actor$Year==unique(df.actor$Year)[i],]
  #Model using the training set
  r<-glm(Won~globes+bafta+script.nom+Hot+fall,train,family=binomial(logit))
  #Predict the missing validate set with training set model
  valid$p<-predict(r,newdata=valid,type='response')
  #Classification rule: choose highest predicted p as predicted winner
  valid$Prediction<-ifelse(valid$p==max(valid$p),1,0)
  #Do the prediction and real-life match up...1 if correct, 0 if incorrect
  output[i,]<-(mean(valid$Won==valid$Prediction))
}
jack.actor<-mean(output)
output<-matrix(data=0,ncol=1,nrow=length(unique(df.actress$Year)))
for(i in 1:length(unique(df.actress$Year))){
  train<-df.actress[!df.actress$Year==unique(df.actress$Year)[i],]
  valid<-df.actress[df.actress$Year==unique(df.actress$Year)[i],]
  r<-glm(Won~globes+bafta+script.nom+Hot+past.win+past.nom+drama+rom+edit.nom,train,family=binomial(logit))
  valid$p<-predict(r,newdata=valid,type='response')
  valid$Prediction<-ifelse(valid$p==max(valid$p),1,0)
  output[i,]<-(mean(valid$Won==valid$Prediction))
}
jack.actress<-mean(output)
output<-matrix(data=0,ncol=1,nrow=length(unique(df.sactor$Year)))
for(i in 1:length(unique(df.sactor$Year))){
  train<-df.sactor[!df.sactor$Year==unique(df.sactor$Year)[i],]
  valid<-df.sactor[df.sactor$Year==unique(df.sactor$Year)[i],]
  r<-glm(Won~globes+Dark+comedy+action+edit.nom,train,family=binomial(logit))
  valid$p<-predict(r,newdata=valid,type='response')
  valid$Prediction<-ifelse(valid$p==max(valid$p),1,0)
  output[i,]<-(mean(valid$Won==valid$Prediction))
}
jack.sactor<-mean(output)
output<-matrix(data=0,ncol=1,nrow=length(unique(df.sactress$Year)))
for(i in 1:length(unique(df.sactress$Year))){
  train<-df.sactress[!df.sactress$Year==unique(df.sactress$Year)[i],]
  valid<-df.sactress[df.sactress$Year==unique(df.sactress$Year)[i],]
  r<-glm(Won~globes+bafta+comedy+edit.nom+wga,train,family=binomial(logit))
  valid$p<-predict(r,newdata=valid,type='response')
  valid$Prediction<-ifelse(valid$p==max(valid$p),1,0)
  output[i,]<-(mean(valid$Won==valid$Prediction))
}
jack.sactress<-mean(output)
output<-matrix(data=0,ncol=1,nrow=length(unique(df.director$Year)))
for(i in 1:length(unique(df.director$Year))){
  train<-df.director[!df.director$Year==unique(df.director$Year)[i],]
  valid<-df.director[df.director$Year==unique(df.director$Year)[i],]
  r<-glm(Won~script.nom+eddie+comedy+dga,train,family=binomial(logit))
  valid$p<-predict(r,newdata=valid,type='response')
  valid$Prediction<-ifelse(valid$p==max(valid$p),1,0)
  output[i,]<-(mean(valid$Won==valid$Prediction))
}
jack.director<-mean(output)
output<-matrix(data=0,ncol=1,nrow=length(unique(df.picture$Year)))
for(i in 1:length(unique(df.picture$Year))){
  train<-df.picture[!df.picture$Year==unique(df.picture$Year)[i],]
  valid<-df.picture[df.picture$Year==unique(df.picture$Year)[i],]
  r<-glm(Won~bafta+Hot+eddie+comedy+edit.nom+dga+wga,train,family=binomial(logit))
  valid$p<-predict(r,newdata=valid,type='response')
  valid$Prediction<-ifelse(valid$p==max(valid$p),1,0)
  output[i,]<-(mean(valid$Won==valid$Prediction))
}
jack.picture<-mean(output)
c(jack.actor,jack.actress,jack.sactor,jack.sactress,jack.director,jack.picture)


#2014 Predictions
predicted<-read.csv("/Users/chrisss/Desktop/Project/MATH 396/predicted.csv")
fit1<-predict(object=aic.step.actor,newdata=subset(predicted,film.award=='Best Actor'),type='response',se.fit=T)
fit2<-predict(object=aic.step.actress,newdata=subset(predicted,film.award=='Best Actress'),type='response',se.fit=T)
fit3<-predict(object=aic.step.sactor,newdata=subset(predicted,film.award=='Best Supporting Actor'),type='response',se.fit=T)
fit4<-predict(object=aic.step.sactress,newdata=subset(predicted,film.award=='Best Supporting Actress'),type='response',se.fit=T)
fit5<-predict(object=aic.step.director,newdata=subset(predicted,film.award=='Best Director'),type='response',se.fit=T)
fit6<-predict(object=aic.step.picture,newdata=subset(predicted,film.award=='Best Picture'),type='response',se.fit=T)
predicted<-cbind(predicted,prob=matrix(c(fit1$se.fit,fit2$se.fit,fit3$se.fit,fit4$se.fit,fit5$se.fit,fit6$se.fit),ncol=1))
predicted<-predicted[order(predicted$film.award,predicted$prob,decreasing=T),]
tab5<-rbind(head(predicted[predicted$film.award=='Best Actor',c(2:3,26)],n=2),
            head(predicted[predicted$film.award=='Best Actress',c(2:3,26)],n=2),
            head(predicted[predicted$film.award=='Best Supporting Actor',c(2:3,26)],n=2),
            head(predicted[predicted$film.award=='Best Supporting Actress',c(2:3,26)],n=2),
            head(predicted[predicted$film.award=='Best Director',c(2:3,26)],n=2),
            head(predicted[predicted$film.award=='Best Picture',c(2:3,26)],n=3))
tab5[,3]<-round(tab5[,3],digits=3)
tab5[,1:2]<-apply(tab5[,1:2],2,as.character)
tab5$film.person[is.na(tab5$film.person)==T]<-tab5$film[is.na(tab5$film.person)==T]
tab5$odds<-c('3/10','20/1','1/25','20/1','1/12','66/1','5/6','7/5','1/16','66/1','7/2','250/1','1/3')
tab5$implied<-round(c(10/(10+3),1/(20+1),25/(25+1),1/(20+1),
                      12/(1+12),1/(66+1),6/(5+6),5/(7+5),
                      16/(1+16),1/(1+66),2/(7+2),1/(250+1),
                      3/(3+1)),digits=3)
latex(tab5[,c(4,5,3)],rowlabel='Nominees',rowname=tab5[,1],n.rgroup=c(2,2,2,2,2,3),
      rgroup=c('Best Leading Actor','Best Leading Actress','Best Supporting Actor',
               'Best Supporting Actress','Best Directing','Best Picture'),
      colheads=c('Betting odds','Implied \\emph{p}','\\widehat{p}'),tab.env=F,
      n.cgroup=c(2,1),cgroup=c('Bookies','Models'),col.just=c('c','c','c'),file="")

bet1<-cbind('Betting Odds'=c("125/1","55/1","11/2","20/1","3/10"),
            'Implied Probability'=round(c(1/(125+1),1/(55+1),2/(11+2),1/(20+1),10/(10+3)),digits=3),
            'Probability'=round(fit1$fit,digits=3))
row.names(bet1)<-subset(predicted,film.award=='Best Actor')$film.person
bet2<-cbind('Betting Odds'=c('20/1','1/25','33/1','40/1','100/1'),
            'Implied Probability'=round(c(1/(20+1),25/(25+1),1/(33+1),1/(40+1),1/(100+1)),digits=3),
            'Probability'=round(fit2$fit,digits=3))
row.names(bet2)<-subset(predicted,film.award=='Best Actress')$film.person
bet3<-cbind('Betting Odds'=c('16/1','100/1','14/1','66/1','1/12'),
            'Implied Probability'=round(c(1/(16+1),1/(100+1),1/(14+1),1/(66+1),12/(12+1)),digits=3),
            'Probability'=round(fit3$fit,digits=3))
row.names(bet3)<-subset(predicted,film.award=='Best Supporting Actor')$film.person
bet4<-cbind('Betting Odds'=c('50/1','7/5','5/6','66/1','50/1'),
            'Implied Probability'=round(c(1/(50+1),5/(7+5),6/(5+6),1/(66+1),1/(50+1)),digits=3),
            'Probability'=round(fit4$fit,digits=3))
row.names(bet4)<-subset(predicted,film.award=='Best Supporting Actress')$film.person
bet5<-cbind('Betting Odds'=c('66/1','1/16','150/1','14/1','100/1'),
            'Implied Probability'=round(c(1/(66+1),16/(1+16),1/(150+1),1/(14+1),1/(100+1)),digits=3),
            'Probability'=round(fit5$fit,digits=3))
row.names(bet5)<-subset(predicted,film.award=='Best Director')$film.person
bet6<-cbind('Betting Odds'=c('22/1','250/1','40/1','7/2','250/1','250/1','250/1','1/3','66/1'),
            'Implied Probability'=round(c(1/(22+1),1/(1+250),1/(40+1),2/(7+1),
                                          1/(250+1),1/(250+1),1/(250+1),3/(1+3),1/(66+1)),digits=3),
            'Probability'=round(fit6$fit,digits=3))
row.names(bet6)<-subset(predicted,film.award=='Best Picture')$film
latex(rbind(bet1,bet2,bet3,bet4,bet5,bet6),rowlabel='Nominee',cgroup=c('Bookie spread','Model prediction'),
      n.cgroup=c(2,1),n.rgroup=c(5,5,5,5,5,9),col.just=c('c','c','c'),file="",
      rgroup=c('Lead Actor','Lead Actress','Supporting Actor','Supporting Actress','Directing','Picture'))
