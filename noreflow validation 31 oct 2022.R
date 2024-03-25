#Validation analysis steps – No-reflow risk score

library(tableone); library(mice; library(dplyr); library(rms); library(pROC)
                           
#TABLE 1// Table 1 = continuous variables by outcome, Table 2 = continuous variables overall and missing, Table 3 = factor variables by outcome, Table 4 = factor variables overall and missing//
                             
data(datafact);
catvars1<-c("sex","smoke","cadhist","csind","ohca","accaha","isr","hpt","dyslipid","pmi","pvd","copd","rheum","chf","prevpci","prevcabg","dialysis","ostial","bifurcation");
factorvars<-c("f_dm", "f_multives","af","f_vesseltype","f_vessdia","f_stentlength","f_prestenosis","f_hr","f_dbp","f_sbp","f_acs","f_bml","f_age","f_timipre","sex","smoke","cadhist","csind","ohca","accaha","isr","hpt","dyslipid","pmi","pvd","copd","rheum","chf","prevpci","prevcabg","dialysis","ostial","bifurcation");
tableone1<-CreateTableOne(data-datafact,factorVars-factorvars, strata="out_noreflow");
tableone1;
summary(tableone1);
tableone2<-CreateTableOne(data-datafact.factorVars-factorvars);
tableone2;
summary(tableone2);
catvars2<-c("c_vessdia","c_stentlength","c_prestenosis","c_hr","c_dbp","c_sbp","c_bmi","c_age");
tableone3<-CreateTableOne/vars=catvars2,data-datacont, strata="out_noreflow");
tableone3;
summary(tableone3);
tableone4<-CreateTableOne(vars=catvars2,data-datacont);
tableone4;
summary(tableone4)

#MULTIPLE IMPUTATION STEP, 10 IMPUTATIONS, 10  ITERATIONS, ALL CATEGORISED VARIABLES//
  
  namesfact<-c(1:35);
  datafact[namesfact]<-lapply(datafact[namesfact],factor);
  imp.datafact<-mice(datafact.m-10,maxit-10,seed=1234);
  
#CHANGE IMPUTED DATASETS TO COMPLETE DATASETS
    
    com.datacat<-complete(imp.datacat,"long", FALSE)
    
    
#REFORMAT SCORE VARIABLES TO SCORE LEVELS RATHER THAN IMPUTATINO LEVELS 
                                                                                
com.datafact$stemi195<-fct_collapse(com.datafact$f_acs,"123"=c("1","2","3"),"4"=c("4"));
com.datafact$length20<-fct_collapse(com.datafact$f_stentlength,"<20"=c("1","2"),">20"=c("3"));
com.datafactSvesstype<-
fct_collapse(com.datafact$f_lloc2,"1245"=c("1","2","4","5"),"3"=c("3"),"6"=c("6"))
                                                                              
#CHANGE FACTOR VARIABLES TO SCORE VALUES (ALSO PROBABLY EASIER DONE IN STATA OR PRIOR TO IMPUTATION//
                                                                                                                           
com.datafact$stemi195num<-as.numeric(com.datafact$stemi195);
com.datafact$stemi195num<-com.datafact $stemi195num-1;
com.datafactScsindnum<-as.numeric/com.datafact$csind);
com.datafactScsindnum/com.datafactŚcsindnum =="1"]<-"0";
com.datafactScsindnum<-as.numeric(com.datafactŚcsindnum);
com.datafact$vesstypenum<-as.numeric(com.datafact$vesstype);
com.datafactSvesstypenumicom.datafact$vesstypenum =="1"]<-"0";
com.datafactSvesstypenum[com.datafactSvesstypenum =="3"]<-"2";
com.datafactSvesstypenum<-as.numeric(com.datafactSvesstypenum);
com.datafact$vessdianum<-as.numeric/com.datafact$f vessdia);
com.datafact$vessdianum[com.datafactSvessdianum =="3"]<-"0";
com.datafact$vessdianum[com.datafactSvessdianum =="1"]<-"3";
com.datafact$vessdianum[com.datafactSvessdianum =="2"]<-"1";
com.datafactSvessdianum<-as.numeric(com.datafactSvessdianum);
com.datafact$length20num<-as.numeric[com.datafact$length20);
com.datafact$length20num<-com.datafact$length20num-1;
com.datafactstimicomb <-fct_collapse(com.datafact$f_timipre,"1"=c("3"),"2"=c("2"),"3"=c("0","1"))};
com.datafact$timicombnum<-as.numeric(com.datafact$timicomb);
com.datafact$timicombnum[com.datafact$timicombnum=="3"]<-"0";
com.datafactStimicombnum[com.datafact$timicombnum=="2"]<-"3";
com.datafact$timicombnum[com.datafact$timicombnum=="1"]<-"4";
com.datafactŚtimicombnum<-as.numeric(com.datafact$timicombnum);

#CALCULATE SCORE VARIABLE//
  
com.datafact$score<-as.numeric(com.datafact$stemi195num+ com.datafactŚcsindnum+
com.datafact$vesstypenum + com.datafact$vessdianum+ com.datafact$length20num +
com.datafact$timicombnum)
  
#CALCULATE CSTATISTIC AND BRIER AND CONFIDENCE INTERVALS//
    
score.model<-with(com.datafact,glm(out_noreflow score,family="binomial"));
com.datafact$noreflownum<-as.numeric(com.datafact$out_noreflow)-1;
pred.score.model<-predict(score.model.type="response");
val.prob(pred.score.model,com.datafact$noreflownum.logit)
roc(com.datafact$noreflownum,pred.score.model) %>%pROC::ci(method="delong");
    
#CALCULATE CALIBRATION IN THE LARGE//
      
linearpredscore=predict(score.model,data=com.datafact);
fit.linearpredscore=glm(noreflownum~1,offset=linearpredscore,data=com.datafact,family="binomial");
summary(fit.linearpredscore)
    
#CALCULATE CALIBRATION SLOPE //
      
slopescore<-glm(out_noreflow linearpredscore data=com.datafact,family="binomial");
slopescore
    
#SUBGROUP STATISTICS//
      
score.model.men<-glm(out_noreflow~score,family="binomial",data=subset(com.datafact,sex="0"));
pred.score.model.men<-predict(score.model.men,type="response");
dfmen <-subset(com.datafact,sex=0,select=c(noreflownum));
val.prob(pred.score.model.men,dfmen$noreflownum,logit);
roc(dfmen$noreflownum,pred.score.model.men) %>%PROC::ci(method="delong");
    
score.model.women<-gim(out_noreflow~score,family="binomial",data=subset(com.datafact,sex=="1"));
pred.score.model.women<-predict(score.model.women,type="response");
dfwomen <-subset(com.datafact,sex==1,select=c(noreflownum));
val.prob(pred.score.model.women,dfwomen$noreflownum,logit);
roc(dfwomen$noreflownum,pred.score.model.women) %>%pROC::ci(method="delong");
    
score.model.agelow<-gim(out_noreflow ~score,family="binomial",data=subset(com.datafact,f_age=="1"));
pred.score.model.agelow<-predict(score.model.agelow,type="response");
dfagelow <-subset(com.datafact.fage==1,select=c(noreflownum));
val.prob(pred.score.model.agelow,dfagelow$noreflownum,logit);
roc(dfagelow$noreflownum,pred.score.model.agelow) %>% pROC::ci(method="delong");
    
score.model.agemid<-glm(out_noreflow ~score,family="binomial",data=subset(com.datafact,f_age=="2"));
pred.score.model.agemid<-predict(score.model.agemid,type="response");
dfagemid <-subset(com.datafact,f_age==2,select=c(noreflownum));
val.prob(pred.score.model.agemid,dfagemid$noreflownum,logit);
roc(dfagemid$noreflownum,pred.score.model.agemid) %>%pROC::ci(method="delong");
    
score.model.agehigh<-glm(out_noreflow ~score,family="binomial",data=subset(com.datafact,f_age=="3"));
pred.score.model.agehigh<-predict(score.model.agehigh,type="response");
dfagehigh <-subset(com.datafact,f_age==3,select=c(noreflownum));
val.prob(pred.score.model.agehigh,dfagehighSnoreflownum,logit);
roc(dfagehigh$noreflownum,pred.score.model.agehigh)%>pROC::ci(method="delong");
    
score.model.nsteacs<-glm(out_noreflow~score,family="binomial",data=subset(com.datafact,f_acs=="1"));
pred.score.model.nsteacs<-predict(score.model.nsteacs,type="response");
dfnsteacs <-subset(com.datafact,f_acs==1,select=c(noreflownum));
val.prob(pred.score.model.nsteacs,dfnsteacs$noreflownum,logit);
roc(dfnsteacs$noreflownum,pred.score.model.nsteacs) %>%pROC::ci(method="delong");
    
score.model.stemi<- glm(out_noreflow~score,family="binomial",data=subset(com.datafact,
                                                                             facs=2|f_acs==3|f_acs=4));
pred.score.model.stemi<-predict(score.model.stemi,type="response");
dfstemi <-subset(com.datafact,f_acs==2 | f_acs==3|f_acs==4,select=c(noreflownum));
val.prob(pred.score.model.stemi,dfstemi$noreflownum,logit);
roc(dfstemi$poreflownum,pred.score.model.stemi) %>%PROC::ci(method="delong");
    
    
#DISTRIBUTION FIGURES, TOP PANEL FIGURE
      
com.datafact.glm<-glm(out_noreflow~score,data=com.datafact,family="binomial")
com.datafactSpred<-predict(com.datafact.gim,type="response");
com.datafactSY<-as.numeric(com.datafact$out_noreflow)-1
    
ggplot(datafact.aes(x-score,fill-out_noreflow))+geom_bar(aes(y=(..count..)/sum(..count..)),colour-"black"+
theme_minimal()+scale_fill_brewer(labels=c("No", "Yes"),palette="Reds")+scale_y_continuous(limits=c(0
„.35),labels=c("0%","10%","20%","30%","40%"))+scale_x_continuous(breaks=c(0,2,4,6,8,10,12))+xlab("Score")+ylab("Percentage")+labs(fill="No reflow")
ggplot(datafact.aes(x=score,fill-out_noreflow))+geom_histogram(bins=12,color="black")+
theme_minimal()+scale_fill_brewer(labels=c("No","Yes"),palette="Reds")+xlab("Score")+ylab("Patients")
+labs(fill="No reflow")

#OUTCOME PANEL

df.prop<-datafact%>%group_by(score) %>% summarise(prop=100*mean(out_noreflow=='1'))
ggplot(dfprop.aes(x=score,y=prop))+geom_col(color="black",fill="dodgerblue")+geom_smooth(color="red",se=FALSE,size=1.2)+theme_minimal()+scale_y_continuous(limits=c(0,68),breaks=c(0,10,20,30,40,50,
60),labels=c("0%","10%", "20%","30%","40%","50%", "60%"))+scale_x_continuous(breaks=c(0,2,4,6,8,10,
12),labels-c(0,2,4,6,8,10,12))+xlab("Score")+ylab("No Reflow (%)")+theme(text-element_text(size=15))

#FULL CALIBRATION PLOT#

ggplot(df.cal.plot,aes(x=bin_pred,y=bin_prob,ymin=ll,ymax=ul))+geom_pointrange(size=0.5,color="blue")+scale_y_continuous(limits=c(0,1),breaks-seq(0,1,by=0.1))+scale_x_continuous(limits=c(0,1),breaks=s
eq(0,1,by-0.1))+geom_segment(aes(x=0,xend=1,y=0,yend=1))+geom_smooth(method="Im",se=FALSE,li
1+x)+geom_smooth(aes(x-pred,y=Y),color="red",se=FALSE,method="loess",fullrange=FALSE,linetype="dashed",color="blue",formula=y~-1+x)+xlab("Predicted")+ylab("Observed")+theme_minimal()+theme(text-element_text(size=15))

#SMALLER CALIBRATION PLOT#

ggplot(df.cal.plot.aes(x-bin_pred,y-bin_prob,ymin=ll,ymax=ul))+geom_pointrange(size-0.5,color="blue")+scale_y_continuous(limits=c(0,.25),breaks-seq(0,.25,by=0.05))+scale_x_continuous(limits=c(0,.25),br
eaks seq(0,.25,by=0.05))+geom_segment(aes(x=0,xend=.25,y=0,yend=.25))+geom_smooth(method="Im",se=FALSE,linetype="dashed",color="blue",formula=y~-1+x)+xlab("Predicted")+ylab("Observed")+theme_minimal()+theme(text-element_text(size=15))
