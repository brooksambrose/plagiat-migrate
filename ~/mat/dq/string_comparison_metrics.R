rm(list=ls())
cat('\014')
t0<-proc.time()
require(dendextend)
require(data.table)
require(stringdist)
require(parallel)
require(foreach)
require(doParallel)


# Load strings
setwd('/Users/bambrose/Dropbox/GitHub/knowledge-survival')
load('hd1.RData')
#hd<-sample(hd,1000)
sl<-lapply(hd,function(x) unlist(labels(x))) # list of string sets
save(sl,file='sl.RData')
rm(hd)
sll<-sapply(sl,length)
ncom<-sll*(sll-1)/2 # eventual number of comparisons
tcom<-sum(ncom)
cat('Total number of comparisons is ',format(tcom,big.interval=3,big.mark=','),'.',sep='')


sdt.f<-function(strl=sl,ix=i){
	# Define column inspectors
	flist<-list(
		py=function(x) grep('^[0-9]{4}$',x)
		,v=function(x) grep('^V([0-9]+|[IVXLC]+)$',x) # numeric or roman
		,ch=function(x) grep('^PCHR?([0-9]+|[IVXLC]+)$',x) # numeric or roman
		,p=function(x) grep('^P([A-Z]{0,3}[0-9]+|[IVXLC]+)[A-Z]?$',x) # numeric or roman
	)
	# initalize set
	nam<-c('au','py','so','m','d','v','p','ch','t')
	y<-data.table(matrix(NA_character_,ncol=length(nam)-1,nrow=length(strl[[ix]])))
	y[,'t':=NA_real_]
	setnames(y,nam)
	for(j in 1:length(strl[[ix]])) {
		pyi<-NULL
		s<-strsplit(strl[[ix]][[j]],split=', ')[[1]]
		m<-regexpr(' ((16)|(17)|(18)|(19)|(20))[0-9]{2}$',s)
		wm<-which(m>-1)
		if(length(wm)&all(!grepl('^[0-9]{4}$',s))) { # if year is included at end of source field and not otherwise extracted, assume publication year
			ns<-regmatches(s,m,invert=T)
			ns[[wm]][2]<-sub(' ','',regmatches(s,m))
			ns[[wm]]<-rev(ns[[wm]])
			s<-unlist(ns) # split so and switch year and source to match common au-py-so format
		}
		for(k in names(flist)){
			si<-flist[[k]](s)
			if(!length(s[si])) next
			if(k=='py') pyi<-si
			y[j,(k):=s[si]]
			s<-s[-si]
		}
		if(!is.null(pyi)&pyi!=1) {
			y[j,'au':=paste(s[1:(pyi-1)])]
			s<-s[-(1:(pyi-1))]
		}
		if(!length(s)) next
		s<-paste(s,collapse=', ')
		if(grepl(' [0-1][0-9][0-3][0-9]$',s)) { # for periodicals with month/day code
			nc<-nchar(s)
			y[j,'d':=substr(s,nc-1,nc)]
			y[j,'m':=substr(s,nc-3,nc-2)]
			y[j,'t':=as.numeric(strptime(apply(y[j,list(py,m,d)],1,paste,collapse=''),format='%Y%m%d'))/24/60/60]
			s<-sub(' +$','',sub('(.+) +[0-1][0-9][0-3][0-9]','\\1',s))
		}
		if(grepl(', P\\[',s)){ # for P including multiple pages
			s<-strsplit(s,', P\\[')[[1]]
			y[j,'p':=paste('P[',s[2],sep='')]
			s<-s[1]
		}
		y[j,'so':=s]
	}
	y[,k:=ix]
	y[,ij:=1:nrow(y)]
	y
}
sdt.f(ix=1)

t1<-proc.time()
cl <- makeCluster(detectCores() )
registerDoParallel(cl, cores = detectCores() )
sdt <- foreach(i = 1:length(sl),.packages = c("data.table"),.inorder=F) %dopar% {
sdt.f(ix=i)
}
stopCluster(cl)
t2<-proc.time()
round((t2-t1)/60,3)

sdt<-rbindlist(sdt)
sdt[,py:=as.integer(py)]
sdt[!is.na(v),nv:=as.integer(gsub('[A-Z]','',v))]
suppressWarnings(sdt[!is.na(p),np:=sapply(
		strsplit(p,'(, )|\\[|\\]')
		,function(x) {
			y<-as.integer(na.omit(as.integer(gsub('[A-Z]',"",x))))
			z<-as.integer(na.omit(as.roman(sub('P','',x))))
			x<-c(y,z)
			if(!length(x)) x<-NA_integer_
			unique(x)
		}
)])
suppressWarnings(sdt[!is.na(ch),nch:=unname(sapply(gsub('^PCHR?','',ch)
	,function(x) ifelse(
		is.na(as.integer(x))
		,as.integer(as.roman(x))
		,as.integer(x)
	)
))])

setkey(sdt,k,ij)
save(sdt,file='sdt.RData')

### Features ###
setwd('/Users/bambrose/Dropbox/GitHub/knowledge-survival')
load('sdt.RData')
load('sl.RData')
sll<-sapply(sl,length)
library(data.table)
library(stringdist)
ncom<-sll*(sll-1)/2 # eventual number of comparisons

t3<-proc.time()
# index of which table and which pair of rows for which to calculate features
comps<-data.table(do.call(rbind,lapply(sll,function(x) do.call(rbind,combn(1:x,m=2,simplify=F)))))
setnames(comps,c('i','j'))
comps[,'k':=rep(1:length(sl),ncom)]
setcolorder(comps,c(3,1,2))
setkey(comps,k)

# measure features
#rm(list=ls()[!ls()%in%c('sl','sll','sdt','t0','t1','t2','t3','ncom','tcom','comps')])
ix.f<-function(k,ij) k[ij]
comps[,jw:=stringdist(mapply(FUN=ix.f,k=sl[comps$k],ij=comps$i),mapply(FUN=ix.f,k=sl[comps$k],ij=comps$j),method='jw',p=.1)]
comps[,cpau:=as.integer(grepl('^\\*',sdt[list(comps$k,comps$i),au])&grepl('^\\*',sdt[list(comps$k,comps$j),au]))]
comps[is.na(sdt[list(comps$k,comps$i),au])|is.na(sdt[list(comps$k,comps$j),au]),cpau:=NA]
comps[,jwau:=stringdist(sdt[list(comps$k,comps$i),au],sdt[list(comps$k,comps$j),au],method='jw',p=.1)]
sl[sample(unique(comps$k[is.na(comps$jwau)]),3)]
comps[,dfpy:=abs(sdt[list(comps$k,comps$i),py]-sdt[list(comps$k,comps$j),py])]
comps[,dfpy2:=as.integer(dfpy^2)]
comps[,jwso:=stringdist(sdt[list(comps$k,comps$i),so],sdt[list(comps$k,comps$j),so],method='jw',p=.1)]
sl[sample(unique(comps$k[is.na(comps$jwso)]),3)]
comps[,dfv:=abs(sdt[list(comps$k,comps$i),nv]-sdt[list(comps$k,comps$j),nv])]
sl[sample(unique(comps$k[is.na(comps$dfv)]),3)]
comps[,mfv:=apply(data.frame(sdt[list(comps$k,comps$i),nv],sdt[list(comps$k,comps$j),nv]),1,FUN=mean)]
comps[,dfp:=mapply(FUN=
function(x,y) {
	if(is.null(x)|is.null(y)) {u<-NA} else {u<-min(abs(unlist(lapply(x,function(z) z-unlist(y)))))}
	u
}
,x=sdt[list(comps$k,comps$i),np]
,y=sdt[list(comps$k,comps$j),np])]
sl[sample(unique(comps$k[is.na(comps$dfp)]),3)]
comps[,mfp:=as.integer(mapply(FUN=
function(x,y) {
	if(is.null(x)|is.null(y)) {u<-NA} else {u<-round(min(unlist(lapply(x,function(z) mean(c(z,min(unlist(y))))))))}
	u
}
,x=sdt[list(comps$k,comps$i),np]
,y=sdt[list(comps$k,comps$j),np]))] # rounding caused data class problem on reading hand code from disk, so as.integer
comps[,dfch:=abs(sdt[list(comps$k,comps$i),nch]-sdt[list(comps$k,comps$j),nch])]
sl[sample(unique(comps$k[is.na(comps$dfch)]),3)]
comps[,dft:=abs(sdt[list(comps$k,comps$i),t]-sdt[list(comps$k,comps$j),t])]
sl[sample(unique(comps$k[is.na(comps$dft)]),3)]
comps[,pyxvxp:=mapply(FUN=function(w,x,y,z) prod(na.omit(c(w,x,y,z))),w=dfpy,x=dfv,y=dfp,z=dfch)]
comps[,cpauxpy:=mapply(FUN=function(v,w) prod(na.omit(c(v,w))),v=cpau,w=dfpy)] # wanted to try to interact corporate author
comps[,cpauxjw:=mapply(FUN=function(v,w) prod(na.omit(c(v,w))),v=cpau,w=jw)]
comps[,prob:=1/.N,by=k]
t4<-proc.time()
round((t4-t3)/60,3)

save(comps,file='comps.RData')

### which NA combos to model ###
setwd('/Users/bambrose/Dropbox/GitHub/knowledge-survival')
t5<-proc.time()
load('sdt.RData')
load('comps.RData')
library(data.table)

compl<-comps[,list(
	jwau=factor(!is.na(jwau),levels=c('FALSE','TRUE'))
	,dfpy=factor(!is.na(dfpy),levels=c('FALSE','TRUE'))
	,jwso=factor(!is.na(jwso),levels=c('FALSE','TRUE'))
	,dfv=factor(!is.na(dfv),levels=c('FALSE','TRUE'))
	,dfp=factor(!is.na(dfp),levels=c('FALSE','TRUE'))
	,dfch=factor(!is.na(dfch),levels=c('FALSE','TRUE'))
	,dft=factor(!is.na(dft),levels=c('FALSE','TRUE'))
)] #complete data

t<-table(compl)
w<-which(t>0,arr.ind=T)
tw<-w-1
t<-cbind(tw,freq=t[w],'perc'=round(prop.table(t)[w]*100,3))
t<-t[order(t[,'freq'],decreasing=T),]
t<-suppressWarnings(data.frame(t,cumul=cumsum(t[,'perc'])))
row.names(t)<-NULL
t
t[do.call(order,c(t[!names(t)%in%c('freq','perc','cumul')],decreasing=T)),]

lt<-t(!!t[,!colnames(t)%in%c('freq','perc','cumul')])
t5<-proc.time()
round((t5-t4)/60,3)

lcompl<-data.frame(t(compl))
comps[,miss:=sapply(lcompl,function(x) which(!apply(as.logical(x)-lt,2,any)))]
rm(lcompl)
t6<-proc.time()
round((t6-t5)/60,3)

setkey(comps,k)
setkey(comps,miss)

samp.batch<-function(x){
for(h in x){ #
	samp<-comps[list(h),list(k,i,j,miss)]
	samp<-samp[sample(1:nrow(samp),3,replace=T)]
	print(samp)
	cat('\n')
	for(g in 1:nrow(samp)){
	print(sdt[list(k=c(rep(samp$k[g],2)),ij=unlist(samp[g,list(i,j)])),list(au,py,so,m,d,v,p,ch,k,ij)])
	cat('\n')
	}
}}
samp.batch(c(17,19)) #1:nrow(t)

## results of analyzing: /Users/bambrose/Dropbox/Summer 2015/Diss/ML_partitions.rtf
setkey(comps,miss)
comps[list(1,13),batch:='A']
comps[list(2,15),batch:='B']
comps[list(3),batch:='C']
comps[list(4,20,21),batch:='D']
comps[list(5),batch:='E']
comps[list(7,8),batch:='F']
comps[list(6,18),batch:='G']
comps[list(9,17),batch:='H']
comps[list(10),batch:='I']
comps[list(12,16,14),batch:='J']
comps[list(11,22),batch:='K']

wtab<-comps[,list(w=sum(prob)),by=batch]
wtab<-wtab[order(wtab$w,decreasing=T),]
wtab<-wtab[c((1:nrow(wtab))[-which(is.na(wtab$batch))],which(is.na(wtab$batch))),]
wtab$cumul<-round(cumsum(prop.table(wtab$w))*100,2)
wtab
comps[,batch:=factor(comps$batch)]

load('sl.RData')
ix.f<-function(k,ij) k[ij]
comps[,str:=paste(mapply(FUN=ix.f,k=sl[comps$k],ij=comps$i),mapply(FUN=ix.f,k=sl[comps$k],ij=comps$j),sep='\r')]

setkey(comps,batch)
save(comps,file='comps.RData')

### SuperLearner
### Machine Learning
if(F){
	install.packages('devtools')
	library('devtools')
	install_github('hadley/stringr')
	install_github('ecpolley/Superlearner')
	install_github('ledell/subsemble')
}
setwd('/Users/bambrose/Dropbox/GitHub/knowledge-survival')
library(data.table)
load('comps.RData')
frame<-comps[c('A','B','C','D','E')] # sampling frame, 97.34% of data
rej<-comps[!c('A','B','C','D','E')] # rejected for limited info

frame[,train:=F]
frame[,match:=NA_integer_]
frame[,ix:=1:nrow(frame)]
samp.f<-function(x,y){
	set.seed(12345)
	sample(x,500,prob=y)
}
samp<-frame[,samp.f(x=ix,y=prob),by=batch]$V1
frame[samp,train:=T]
save(samp,file='samp.RData')

frame[,test:=F]
samp.f<-function(x,y){
	set.seed(54321)
	sample(x,100,prob=y)
}
samp.test<-frame[,samp.f(x=ix,y=prob),by=batch]$V1
frame[samp.test,test:=T]
save(samp.test,file='samp.test.RData')
save(frame,file='frame.RData')

### Define sample for hand coding
write.csv(frame[frame$train,],file='frame.train.csv',na='',row.names=F) # convert to .xls to protect hand coding
write.csv(frame[frame$test,],file='frame.test.csv',na='',row.names=F) # convert to .xls to protect hand coding

#cat('\014')
### Import hand codes
setwd('/Users/bambrose/Dropbox/GitHub/knowledge-survival')
load('frame.RData')
library(data.table)
hand<-data.table(read.csv('frame.train.hand.csv',colClasses=sapply(frame,class)))
hand[,match:=as.integer(!is.na(match))]
save(hand,file="hand.RData")

setkey(hand,batch)
brk<-copy(hand)
brk[,prob2:=1/.N,by=k]
brk[,count:=c((1/prob)*prob2)]
(brk<-brk[,list(prop=mean(replicate(10000,sample(match,prob=count,replace=T)))),by=batch]) # bootstrapped estimate of mean
brk[,se:=sqrt(brk$prop*(1-brk$prop)/500)]
setkey(brk,batch)
save(brk,file='brk.RData')

fitter<-function(b,hand){
	require(data.table)
	require(SuperLearner)
	require(cvAUC)
A<-hand[b]
cols<-apply(A,2,FUN=function(x) all(!is.na(x)))
cols<-cols[!names(cols)%in%c('prob','miss','batch','train','test','ix','str')&cols]
A<-A[,names(cols),with=F]
if(any(duplicated(t(A)))) A<-A[,!duplicated(t(A)),with=F]

SL.library <- c(
# "SL.glmnet",
 "SL.glm",
 "SL.knn",
 "SL.gam",
 "SL.mean"
)
fit<-mcSuperLearner(
	X=A[,!c('match','k','i','j'),with=F]
	,Y=A$match
	,id=A$k
	,family=binomial(link='cloglog')
	,SL.library=SL.library
	,method="method.NNLS"
	,verbose=T
)
auc <- AUC(predictions = fit$SL.predict, labels = A$match)
print(A)
print(b)
print(auc)  # Test set AUC is: 0.937
ret<-list(fit=fit,auc=auc,dat=A)
ret
}

t7<-proc.time()
lb<-list()
for(i in levels(hand$batch)) lb[[i]]<-fitter(i,hand)
t8<-proc.time()
round((t8-t7)/60,3)
save(lb,file='lb.RData')

### Testing
setwd('/Users/bambrose/Dropbox/GitHub/knowledge-survival')
library(data.table)
load('lb.RData')
test<-data.table(read.csv('frame.test.hand.csv',colClasses=sapply(frame,class)))
test[,match:=as.integer(!is.na(match))]
test[,list(prop=mean(match)),by=batch]
setkey(test,batch)

tester<-function(b,test,lb){
	require(data.table)
	require(SuperLearner)
	require(cvAUC)
	A<-test[b,colnames(lb[[b]]$dat),with=F]

	pred<-predict.SuperLearner(
		object=lb[[b]]$fit
		,newdata=A[,!c('match','k','i','j'),with=F]
		,X=lb[[b]]$dat[,!c('match','k','i','j'),with=F]
		,Y=lb[[b]]$dat$match
		,onlySL=T
	)
	p<-data.frame(pred$pred,pred$library.predict)
	colnames(p)<-c("SuperLearner",lb[[b]]$fit$libraryNames)
	p<-p[,!sapply(p,function(x) all(x==0))]
	auc<-sapply(p,FUN=function(x) AUC(predictions = x, labels = A$match))
	print(head(A))
	print(b)
	print(auc)  # Test set AUC is: 0.937
	ret<-list(pred=pred,auc=auc)
	ret
}

pb<-list()
for(i in levels(test$batch)) {
	pb[[i]]<-tester(b=i,test,lb)
	pb[[i]]$brk<-list(l95ci=brk[i,prop]-1.96*brk[i,se],mean=brk[i,prop],u95ci=brk[i,prop]+1.96*brk[i,se])
	pb[[i]]$cut<-list(
		l95ci=quantile(pb[[i]]$pred$pred,p=pb[[i]]$brk$l95ci)
		,mean=quantile(pb[[i]]$pred$pred,p=pb[[i]]$brk$mean)
		,u95ci=quantile(pb[[i]]$pred$pred,p=pb[[i]]$brk$u95ci)
	)
	hist(pb[[i]]$pred$pred,breaks=seq(0,1,.05),main=i,freq=F)
	hist(pb[[i]]$pred$pred,breaks=c(0,pb[[i]]$cut$mean,1),add=T,freq=F,border='blue')
	text(x=pb[[i]]$cut$mean,y=0,labels=round(pb[[i]]$cut$mean,3),pos=3,col='blue',cex=.75)
	hist(pb[[i]]$pred$pred,breaks=c(0,pb[[i]]$cut$l95ci,1),add=T,freq=F,border='red')
	text(x=pb[[i]]$cut$l95ci,y=0,labels=round(pb[[i]]$cut$l95ci,3),pos=3,col='red',cex=.75)
	hist(pb[[i]]$pred$pred,breaks=c(0,pb[[i]]$cut$u95ci,1),add=T,freq=F,border='green')
	text(x=pb[[i]]$cut$u95ci,y=0,labels=round(pb[[i]]$cut$u95ci,3),pos=3,col='green',cex=.75)
}
save(pb,file='pb.RData')




### Prediction
library(data.table)
# Manual recodes
setwd('/Users/bambrose/Dropbox/GitHub/knowledge-survival')
load('frame.RData')
load('sl.RData')
setkey(frame,k)
frame[list(which(sapply(sl,function(x) any(grepl('^NYFH',x))))),cpau:=1] # '^NYFH' should be a corporate author '^*NYFH'

load('lb.RData')
load('hand.RData')
predictor<-function(b,frame,lb){
	require(data.table)
	require(SuperLearner)
	setkey(frame,batch)
	A<-frame[b,colnames(lb[[b]]$dat),with=F]

	pred<-predict.SuperLearner(
		object=lb[[b]]$fit
		,newdata=A[,!c('match','k','i','j'),with=F]
		,X=lb[[b]]$dat[,!c('match','k','i','j'),with=F]
		,Y=lb[[b]]$dat$match
		,onlySL=T
	)
	A[,pred:=pred$pred]
	print(head(A))
	print(b)
	ret<-list(pred=pred,dat=A)
	ret
}

pba<-list()
pdf('pba.pdf',h=7.5,w=10)
for(i in levels(hand$batch)) {
	pba[[i]]<-predictor(b=i,frame,lb)
	pba[[i]]$brk<-list(
		l95ci=brk[i,prop]-1.96*brk[i,se]
		,mean=brk[i,prop]
		,u95ci=brk[i,prop]+1.96*brk[i,se]
	)
	pba[[i]]$cut<-list(
		l95ci=quantile(pba[[i]]$pred$pred,p=1-pba[[i]]$brk$l95ci)
		,mean=quantile(pba[[i]]$pred$pred,p=1-pba[[i]]$brk$mean)
		,u95ci=quantile(pba[[i]]$pred$pred,p=1-pba[[i]]$brk$u95ci)
	)
	hist(pba[[i]]$pred$pred,breaks=seq(0,1,.05),main=i,freq=F)
	hist(pba[[i]]$pred$pred,breaks=c(0,pba[[i]]$cut$mean,1),add=T,freq=F,border='blue')
	text(x=pba[[i]]$cut$mean,y=0
			 ,labels=paste(round(c(pba[[i]]$brk$mean*100,pba[[i]]$cut$mean),3),collapse='%\n')
			 ,pos=3,col='blue',cex=.75)
	hist(pba[[i]]$pred$pred,breaks=c(0,pba[[i]]$cut$l95ci,1),add=T,freq=F,border='red')
	text(x=pba[[i]]$cut$l95ci,y=1
			 ,labels=paste(round(c(pba[[i]]$brk$l95ci*100,pba[[i]]$cut$l95ci),3),collapse='%\n')
			 ,pos=3,col='red',cex=.75)
	hist(pba[[i]]$pred$pred,breaks=c(0,pba[[i]]$cut$u95ci,1),add=T,freq=F,border='green')
	text(x=pba[[i]]$cut$u95ci,y=2
			 ,labels=paste(round(c(pba[[i]]$brk$u95ci*100,pba[[i]]$cut$u95ci),3),collapse='%\n')
			 ,pos=3,col='green',cex=.75)

pba[[i]]$dat[,match:=NULL]
thresh<-pba[[i]]$cut$mean
pba[[i]]$dat[,match:=pred>=thresh]
}
dev.off()
save(pba,file='pba.RData')
t9<-proc.time()
round((t9-t8)/60,3)

### network evaluation of sets
setwd('/Users/bambrose/Dropbox/GitHub/knowledge-survival')
load('pba.RData')
load('sl.RData')
library(igraph)
library(data.table)

el<-list()
for(i in names(pba)){
	pass<-pba[[i]]$dat$match
	el[[i]]<-pba[[i]]$dat[,list(k,i,j,pred)]
	el[[i]][,`:=`(i=mapply(FUN=function(k,ij) sl[[k]][ij],k=k,ij=i)
		,j=mapply(FUN=function(k,ij) sl[[k]][ij],k=k,ij=j)
	)]
	el[[i]][!pass,pred:=pred-1]
}
el<-rbindlist(el)
setkey(el,k)
levs<-sort(unique(unlist(el)))
el[,`:=`(
	i=as.numeric(factor(i,levels=levs))
	,j=as.numeric(factor(j,levels=levs))
	)]
w<-el$i>el$j
eli<-el$i[w]
el[w,i:=j]
el[w,j:=eli]
rm(w,eli)
setkey(el,i,j)
bf<-nrow(el)
el<-unique(el)
cat('\n',bf-nrow(el),'duplicate edges removed\n')
setkey(el,k)
net<-graph.edgelist(as.matrix(el[,list(i,j)]), directed=F)
V(net)$name<-levs
E(net)$weight<-el$pred
mem<-el[,list(v=list(unique(c(i,j)))),by=k][,v]
system.time(compo<-lapply(mem,FUN=function(x) induced.subgraph(net,x)))

# exploratory plot of spinglass effectiveness
ns<-sapply(compo,function(x) length(V(x)))
par(mfrow=c(2,2),mar=rep(.9,4))
s<-sample(1:length(compo),4,p=ns)
lintran<-function(x,s1=c(0,1),s2=c(0,1)) {a=diff(s2)/diff(s1);b=s2[1]-a*s1[1];return(a*x+b)}
for(i in s){
wt<-E(compo[[i]])$weight
wt[wt<0]<-wt[wt<0]+1
wfr<-layout.fruchterman.reingold(compo[[i]],weights=wt)
plot.communities(
	spinglass.community(compo[[i]],spins=length(V(compo[[i]])),implementation="neg")
	,compo[[i]]
	,layout=wfr
	,main=i
	,edge.color=sapply(lintran(wt,s2=c(.1,1)),function(x) gray(0,alpha=x))
	,edge.label=as.character(round(wt,2))
	,edge.label.cex=.75
	,edge.label.family='Arial Narrow'
	,edge.label.color='darkgray'
	,vertex.label.cex=.75
	,vertex.label.family='Arial Narrow'

)
}

system.time(
oc<-lapply(compo,function(x) spinglass.community(x,spins=length(V(x)),implementation='neg'))
)



save(compo,file='compo.RData')
save(oc,file='oc.RData')

# could improve feature engineering, but will go with one more test, proportion of boundary crossing ties
#setwd('/Users/bambrose/Dropbox/GitHub/knowledge-survival')
#load('compo.RData')
#load('oc.RData')
library(igraph)
library(data.table)

mod<-sapply(1:length(oc),function(i) modularity(oc[[x]],weights=))
cmpx<-sapply(oc,FUN=function(x) length(unique(x$membership)))
cmpxs<-list()
for(i in which(cmpx>1)) cmpxs[[i]]<-split(1:oc[[i]]$vcount,f=oc[[i]]['membership'])

pbct<-list()
for(i in which(cmpx>1)){
	pbct[[i]]<-c(ix=i,
		it=sum(sapply(cmpxs[[i]],FUN=function(x) length(E(induced.subgraph(compo[[i]],x))))) # count of inside ties
		,at=length(E(compo[[i]])) # count of all ties
		,nv=length(V(compo[[i]])) # count of all citations
	)
}
(cmx<-data.table(do.call(rbind,pbct))) # matrix of complex sets
setkey(cmx,ix)
cmx[,`:=`(
	bct=at-it # count of boundary crossing ties
	,pbct=1-(it/at) # proportion of boundary crossing ties
	,mod=mod[ix] # modularity
)]
q<-list(
	bct=quantile(cmx$bct,seq(0,1,.01))
	,pbct=quantile(cmx$pbct,seq(0,1,.01))
	,nv=quantile(cmx$nv,seq(0,1,.01))
	,mod=quantile(cmx$mod,seq(0,1,.01))
)
cmx[,`:=`(
	qbct=sapply(bct,FUN=function(x) sum(x<q$bct)-1)
	,qpbct=sapply(pbct,FUN=function(x) sum(x<q$pbct)-1)
	,qmod=sapply(mod,FUN=function(x) sum(x>q$mod))
	,qnv=sapply(nv,FUN=function(x) sum(x>q$nv))
)]
cmx[,tot:=qmod+qnv] # qbct+qpbct+qmod+qnv
q$tot<-quantile(cmx$tot,seq(0,1,.01))
cmx[,qtot:=sapply(tot,FUN=function(x) sum(x>q$tot))] # high values mean use the community structure, low values ignore community structure


#inspect results
h<-hist(cmx$qtot,breaks=seq(0,100,5))
plot.new()
fam<-'Helvetica-Narrow' # names(pdfFonts())[order(sapply(names(pdfFonts()),function(x) strwidth(paste(LETTERS,collapse=''),family=x)))[1]]
graphics.off()
pdf('sticksnballs.pdf',h=7.5,w=10)
j=2947
par(mfrow=c(3,2),mar=rep(.9,4))
g<-compo[[j]]
E(g)$weight
plot(edge.betweenness.community(g),g,main=paste(j,'edge.betweenness.community'))
plot(fastgreedy.community(g),g,main=paste(j,'fastgreedy.community'))
plot(label.propagation.community(g),g,main=paste(j,'label.propagation.community'))
plot(multilevel.community(g),g,main=paste(j,'multilevel.community'))
plot(optimal.community(g),g,main=paste(j,'optimal.community'))
plot(spinglass.community(g,implementation='neg'),g,main=paste(j,'spinglass.community'))

#plot(walktrap.community(g),g,main=paste(j,'walktrap.community'))
par(mfrow=c(2,2),mar=c(5, 4, 4, 2) + 0.1)
setkey(cmx,ix)
ltot<-rep(0,length(mod))
ltot[cmx$ix]<-cmx$qtot
ins<-cmx[,list(ins=list(sample(ix,4))),keyby=qtot]
for(i in rev(unlist(ins$ins))) {
	wt<-E(compo[[i]])$weight
	wt[wt<0]<-wt[wt<0]+1
	wfr<-layout.fruchterman.reingold(compo[[i]],weights=wt)
	plot.communities(
		oc[[i]]
		,compo[[i]]
		,layout=wfr
		,edge.color=sapply(lintran(wt,s2=c(.1,1)),function(x) gray(0,alpha=x))
		,edge.label=as.character(round(wt,2))
		,edge.label.cex=.75
		,edge.label.family=fam
		,edge.label.color=gray(0,alpha=.5)
		,vertex.label.cex=.75
		,vertex.label.family=fam
		,main=paste(
			'comp',i
			,'qtot',ifelse(is.null(pbct[[i]]),'n/a',cmx[list(i),qtot])
			,'\nmod',round(mod[i],3)
		#	,'bct',ifelse(is.null(pbct[[i]]),'n/a',cmx[list(i),bct])
		#	,'pbct',ifelse(is.null(pbct[[i]]),'n/a',round(cmx[list(i),pbct],4))
			,'nv',ifelse(is.null(pbct[[i]]),'n/a',round(cmx[list(i),nv],4))
			,'\nqmod',ifelse(is.null(pbct[[i]]),'n/a',cmx[list(i),qmod])
		#	,'qbct',ifelse(is.null(pbct[[i]]),'n/a',cmx[list(i),qbct])
		#	,'qpbct',ifelse(is.null(pbct[[i]]),'n/a',cmx[list(i),qpbct])
			,'qnv',ifelse(is.null(pbct[[i]]),'n/a',cmx[list(i),qnv])
		)
	)
}
dev.off()
par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
t10<-proc.time()
round((t10-t9)/60,3)




### moving forward, pick a qtot threshold and develop recode procedure

fuzzy.sets<-sapply(oc,function(x) split(names(membership(x)),membership(x)))
fuzzy.sets<-do.call(c,fuzzy.sets)
Count<-sapply(fuzzy.sets,length)
tfs<-data.frame(table(Count))
tfs$Perc<-round(prop.table(tfs$Freq)*100,3)
rownames(tfs)<-NULL
print(tfs)
fuzzy.sets[which.max(Count)]
t11<-proc.time()
round((t11-t10)/60,3)
fuzzy.sets[Count==1]<-NULL
names(fuzzy.sets)<-NULL
save(fuzzy.sets,file='fuzzy-sets.RData')
