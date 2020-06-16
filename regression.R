#이상치 제거전이 R^2이 더 크다. 
#R^2 = 0.2588  #R^2=0.1282
install.packages("UsingR")
require(UsingR)
d<-read.csv('C:/Users/shimeunyoung/Desktop/final_train(1).csv',header=T,sep=',')
hist(d$logy,col="blue",breaks=100)
x<-subset(d[2:40])
head(x)
length(x)
#상관분석
cor.test(d$logy,x[,38])
#1,25,28,30,34
str(x[1]) #broad_kbs
str(x[25]) #ca_sing
str(x[28]) #ca_etc
str(x[30]) #log_max
str(x[34]) #log_pd_max

#이상치제거안함, logy train data
d<-read.csv('C:/Users/shimeunyoung/Desktop/final_train(1).csv',header=T,sep=',')
str(d)
length(d)
attach(d)
colnames(d)
d<-d[-1] #year제거
d<-d[-40] #y제거 
m1<-lm(d$logy~broad_kbs+broad_mbc+broad_sbs+broad_jtbc+broad_tvn+broad_mnet+broad_etc+age_12+age_15+age_19+age_all+week_mon+week_tue+week_wed+week_thu+week_fri+week_sat+week_sun+tw_gold+tw_week+tw_etc+ca_mc+ca_gag+ca_tal+ca_sing+ca_act+ca_bro+ca_etc+log_award+log_max+log_pro1+log_pro2+log_pd_award+log_pd_max+log_pd_pro1+log_pd_pro2+portal_naver+portal_daum+pd_portal,data=d)
anova(m1) #broad_kbs,week_thu, ca_sing, ca_bro, log_max,  log_pd_max
m2<-lm(d$logy~broad_kbs+week_thu+ca_sing+ca_bro+log_max+log_pd_max,data=d)
anova(m2) #ca_bro, week_thu 제거해야
anova(m2,m1) #m2
m3<-lm(d$logy~broad_kbs+ca_sing+log_max+log_pd_max,data=d)
anova(m3)
anova(m3,m2)#m3결정
summary(m3) #R^2=0.2422
#다중공선성 확인
car::vif(m3) 
sqrt(car::vif(m3)) > 2 
#없음
#변수선택
step(m3,direction="both")#단계적 
#d$logy ~ broad_kbs + ca_sing + log_max + log_pd_max   AIC=-979.82
step(m3,direction="forward")#전직적 
#d$logy ~ broad_kbs + ca_sing + log_max + log_pd_max  AIC=-979.82
step(m3,direction="backward")#후진적 
#d$logy ~ broad_kbs + ca_sing + log_max + log_pd_max    AIC=-979.82
#최종모델 d$logy ~ broad_kbs + ca_sing + log_max + log_pd_m
#2018test data   logy
t<-read.csv('C:/Users/shimeunyoung/Desktop/final_test.csv',header=T,sep=',')
str(t)
length(t)
attach(t)
t$logy
#예측
p<-predict(m3,t, type='response')
p
#rmse
rmse= function(m, o){ sqrt(mean((m - o)^2)) } 
rmse(t$logy,p)  #0.5926963

require(ROCR)
pred <- prediction(p, t$logy)
Number of classes is not equal to 2.
ROCR currently supports only evaluation of binary classification tasks.
roc <- performance(pred, measure = 'tpr', x.measure = 'fpr')
plot(roc, col='red')
legend('bottomright', c('base','logistic'), col=1:2, lty=2:1)
abline(a=0, b=1, lty='dotted')

auc <- performance(pred, measure = 'auc')
auc <- auc@y.values[[1]]
auc
[출처] ROC curve|작성자 Curycu



#이상치제거
install.packages("UsingR")
require(UsingR)
dd<-read.csv('C:/Users/shimeunyoung/Desktop/final_train_delete.csv',header=T,sep=',')
hist(dd$logy,col="blue",breaks=100)
xx<-subset(dd[2:40])
head(xx)
length(xx)
#상관분석
cor.test(dd$logy,xx[,39])
#28 30 31 32 34 
str(xx[28]) #ca_etc
str(xx[29]) #log_award
str(xx[30]) #log_max
str(xx[31]) #log_pro1
str(xx[32]) #log_pro2

#이상치제거, logy train data
dd<-read.csv('C:/Users/shimeunyoung/Desktop/final_train_delete.csv',header=T,sep=',')
str(dd)
length(dd)
attach(dd)
dd<-dd[-1] #year제거
dd<-dd[-40] #y제거 
m1<-lm(dd$logy~broad_kbs+broad_mbc+broad_sbs+broad_jtbc+broad_tvn+broad_mnet+broad_etc+age_12+age_15+age_19+age_all+week_mon+week_tue+week_wed+week_thu+week_fri+week_sat+week_sun+tw_gold+tw_week+tw_etc+ca_mc+ca_gag+ca_tal+ca_sing+ca_act+ca_bro+ca_etc+log_award+log_max+log_pro1+log_pro2+log_pd_award+log_pd_max+log_pd_pro1+log_pd_pro2+portal_naver+portal_daum+pd_portal,data=dd)
anova(m1) #broad_kbs, ca_sing, ca_bro, log_max, log_pd_max
m2<-lm(dd$logy~broad_kbs+ca_sing+ca_bro+log_max+log_pd_max,data=dd)
anova(m2) #ca_bro 제거해야
anova(m2,m1) #m2
m3<-lm(dd$logy~broad_kbs+ca_sing+log_max+log_pd_max,data=dd)
anova(m3)
anova(m3,m2)#m3결정
summary(m3) #R^2=0.1178
#다중공선성 확인
car::vif(m3) 
sqrt(car::vif(m3)) > 2 
#없음
#변수선택
step(m3,direction="both")#단계적 
#dd$logy ~ broad_kbs + ca_sing + log_max + log_pd_max  AIC=-959.55
step(m3,direction="forward")#전직적 
##dd$logy ~ broad_kbs + ca_sing + log_max + log_pd_max  AIC=-959.55
step(m3,direction="backward")#후진적 
##dd$logy ~ broad_kbs + ca_sing + log_max + log_pd_max  AIC=-959.55
#최종모델 #dd$logy ~ broad_kbs + ca_sing + log_max + log_pd_max  
#2018test data   logy
tt<-read.csv('C:/Users/shimeunyoung/Desktop/final_test.csv',header=T,sep=',')
str(t)
length(tt)
attach(tt)
tt<-tt[-1] #year제거
tt<-tt[-40] #y제거
tt$logy
#예측
pp<-predict(m3,tt)
#rmse
rmse= function(m, o){ sqrt(mean((m - o)^2)) } 
rmse(tt$logy,pp)  #o.5487852

