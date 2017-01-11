lintran<-function(x,s1=c(0,1),s2=c(0,1)) {a=diff(s2)/diff(s1);b=s2[1]-a*s1[1];return(a*x+b)}

#' Scrape master list of journals indexed by Thompson Reuters Web of Knowledge. Subsample based on regular expression.
#'
#' @param masterurl
#' @param out
#' @param njour
#' @param npp
#' @param check.for.saved.output
#' @param save
#' @param detect.language
#' @param grep
#'
#' @return
#' @export
#'
#' @examples
master2wok.f<-function(
	masterurl='http://ip-science.thomsonreuters.com/cgi-bin/jrnlst/jlresults.cgi?PC=MASTER&mode=print'
	,out=stop("Specify output directory for your project.")
	,njour=NULL
	,npp=500
	,check.for.saved.output=F
	,save=T
	,detect.usa=T
	,grep=NULL
)
{
	if(check.for.saved.output) if(any(grepl('master2wok.RData',dir(recursive=T,full.names=T,ignore.case=T)))) {
		warning('Loading and returning saved master2wok.RData.',call.=F)
		load(dir(pattern='master2wok.RData',full.names=T,recursive=T,ignore.case=F)[1])
		return(master2wok)
	}
	require(rvest)
	require(data.table)
	if(detect.language) require(cldr)
	if(is.null(njour)) { # get total number of journals from first p
		njour<-read_html(masterurl) %>% html_node("p") %>% html_text()
		njour<- regexpr('[0-9]+',njour) %>% regmatches(x=njour) %>% as.numeric()
	}
	batches<-paste(masterurl,'&Page=',1:ceiling(njour/npp),sep = '')
	imp<-function(pageurl){
		cat(pageurl,'\n',sep='')
		dt<-read_html(pageurl)
		dt<-html_nodes(dt,xpath='//dl') %>% html_text() %>% iconv(sub='?') %>% strsplit(split='\n+') %>% .[[1]]
		b<-grep('^[0-9]+\\.',dt)
		e<-c(b[-1]-1,length(dt))
		dt<-data.table(
			ix=as.integer(sub('^([0-9]+)\\..+','\\1',dt[b]))
			,SO=sub('^[0-9]+\\. (.+)','\\1',dt[b])
			,period=factor(sub('^ ?([^0-9]*) ISSN: .+$','\\1',dt[b+1]))
			,issn=sub('^ ?[^0-9]* ISSN: ([0-9*]{4}-[0-9*]{3}[0-9xX*]).+$','\\1',dt[b+1])
			,PU=factor(sub('^ ?[^0-9]* ISSN: [0-9*]{4}-[0-9*]{3}[0-9xX*]([^,]+).+$','\\1',dt[b+1]))
			,PA=factor(sub('^ ?[^0-9]* ISSN: [0-9*]{4}-[0-9*]{3}[0-9xX*][^,]+, (.+)$','\\1',dt[b+1]))
			,db=mapply(function(beg,end) dt[(beg+2):(end)],beg=b,end=e)
		)
		dt[grep('^ISSN',dt$period),period:=NA]
	}
	master2wok<-rbindlist(lapply(batches,imp))
	if(detect.usa) {
		cat('\nFiltering on publisher location as proxy for USA origin.')
		master2wok[,usa:=as.logical(ifelse(grepl(" USA,",PA),T,F))]
	}
	if(!is.null(grep)) master2wok[,tag:=sapply(SO,tag)]
	setattr(master2wok,'date.downloaded',Sys.time())
	if(save) save(master2wok,file=paste(out,'master2wok.RData',sep=.Platform$file.sep))
	master2wok
}

#' Utility for doing something?
#'
#' @param so 
#' @param srch 
#'
#' @return
#' @export
#'
#' @examples
tag<-function(so=SO,srch=c('ANTH', 'SOCI', 'ECON', 'POLI','PSYC')) {
	y<-sapply(srch
						,function(x) {
							if(grepl('SOCIETY',so)) return(all(grepl(x,so),!grepl('OF THE',so),!grepl('SOCIETY (OF)|(FOR)',so)))
							if(grepl('ASSOCI',so)) return(F)
							grepl(x,so)
						}
	)
	if(any(y)) {paste(tolower(names(y)[y]),collapse=', ')} else {'Out'}
}

jstor2dbw.f<-function(
	dir=stop("Choose input directory containing DFR.JSTOR.ORG batches of zip archives.")
	,out=stop("Specify output directory for your project.")
	,save=T
	,import.ngrams=F
	,sample.batches=T
	,sample.size=1
	,in.parallel=F # import batches in parallel
	,drop.nchar1=T # drop ngrams of 1 character
	,drop.freq1=T # drop ngrams that appear only once
	,drop.doc1=T # drop ngrams that appear in only one document
	,check.for.saved.output=F # will scan output directory for a 'jstor2dbw.RData' file, load it, and return it instead of running a new import
)
{
	if(check.for.saved.output) if(any(grepl('jstor2dbw.RData',dir(recursive=T,full.names=T,ignore.case=T)))) {
		warning('Loading and returning saved jstor2dbw.RData.',call.=F)
		load(dir(pattern='jstor2dbw.RData',full.names=T,recursive=T,ignore.case=F)[1])
		return(jstor2dbw)
	}

	zips<-grep('\\.zip$',list.files(dir,full.names=T,recursive=T,include.dirs=T),value=T)
	n<-length(zips)
	if(sample.batches) {
		zips<-sort(sample(x=zips,size=sample.size))
		cat("\n",sample.size," or ",round(sample.size/n*100,3)," % of batches drawn at random.\n\n",sep="")
	}

	import.dfr.jstor.f<-function(zip){
		require(tm)
		require(SnowballC)
		require(data.table)

		temp <- tempdir()
		unzip(zip,exdir=temp)
		f<-list.files(temp,recursive=T,include.dirs=T,full.names=T)
		dat<-try(data.table(read.table(grep('citations.tsv',f,value=T),header=F,skip=1,sep='\t',quote='',comment.char = "")))
		if(class(dat)[1]=="try-error") return(dat)
		dat[,ncol(dat):=NULL,with=F]
		setnames(dat,as.character(read.table(grep('citations.tsv',f,value=T),header=F,nrows=1,sep='\t',quote='',as.is=T)))
		dat[,id:=sub('/','_',id)]
		setkey(dat,id)

		bow.f<-function(x){
			x<-data.table(read.csv(grep(x,f,value=T),as.is=T))
			x<-x[!removeWords(x$WORDCOUNTS,stopwords('english'))=='']
			x[,WORDCOUNTS:=tolower(WORDCOUNTS)]
			x[,WORDCOUNTS:=removePunctuation(WORDCOUNTS)]
			x[,WORDCOUNTS:=removeNumbers(WORDCOUNTS)]
			x[,WORDCOUNTS:=stemDocument(WORDCOUNTS,language='english')]
			x<-x[,list(WEIGHT=sum(WEIGHT)),by=WORDCOUNTS]
			x<-x[,c(rep(WORDCOUNTS,WEIGHT))]
			x<-table(x)
			x
		}
		browser()
		if(import.ngrams) dat[,bow:=lapply(id,function(x) try(bow.f(x)))]
		unlink(temp)
		dat
	}

	if(in.parallel){
		require(doParallel)
		cl <- makeCluster(detectCores() )
		registerDoParallel(cl, cores = detectCores() )
		jstor2dbw <- foreach(i = zips,.packages = c('data.table','tm','SnowballC'),.inorder=F) %dopar% try(import.dfr.jstor.f(zip=i))
		stopCluster(cl)
	} else {jstor2dbw<-list();for(i in zips) jstor2dbw[[i]]<-try(import.dfr.jstor.f(zip=i))}

	jstor2dbw<-rbindlist(jstor2dbw[sapply(jstor2dbw,is.data.table)])

	# condense vocab
	if(import.ngrams){
		if(drop.nchar1) jstor2dbw[,bow:=list(lapply(bow,function(x) as.table(x[nchar(names(x))>1])))]
		if(drop.freq1) jstor2dbw[,bow:=list(lapply(bow,function(x) as.table(x[x>1])))]
		vocab<-sort(unique(unlist(lapply(jstor2dbw$bow,function(x) names(x)))))
		jstor2dbw[,bow:=list(lapply(bow,function(x) {x<-matrix(c(which(vocab%in%names(x)),as.integer(x)),byrow=TRUE,nrow=2);rownames(x)<-c('vocab.index','freq');x}))]
	}
	dfr.jour<-jstor2dbw[,.N,by=journaltitle]
	print(dfr.jour)
	if(import.ngrams) attributes(jstor2dbw)$vocab<-vocab
	if(save) save(jstor2dbw,file=paste(out,'jstor2dbw.RData',sep=.Platform$file.sep))
	jstor2dbw
}

wok2dbl.f<-function(
	dir=stop("Choose input directory containing WOK batches.")
	,out=stop("Specify output directory for your project.")
	,art_rev_only=T
	,sample.batches=F
	,sample.size=1000
	,save=T
	,verbose=T
	,check.for.saved.output=F
	,imp.rul=list( #import rules
		collapse=c(TI=' ',WC=' ',SC=' ')
		,split=c(WC='; ',SC='; '))
)
{
	if(check.for.saved.output) if(any(grepl('wok2dbl.RData',dir(path=out,recursive=T,full.names=T,ignore.case=T)))) {
		warning('Loading and returning saved wok2dbl.RData.',call. = F)
		load(dir(path=out,pattern='wok2dbl.RData',recursive=T,full.names=T,ignore.case=F)[1])
		return(wok2dbl)
	}

	#NEWER WOK Database Import
	require(data.table)

	files<-list.files(dir,full.names=T,recursive=T)

	c<-0
	n<-length(files)

	cat("\n",n," batches detected.",sep="")

	if(sample.batches) {
		files<-sort(sample(x=files,size=sample.size))
		cat("\n",sample.size," or ",round(sample.size/n*100,3)," % of batches drawn at random.\n\n",sep="")
	}

	n<-length(files)

	wok2dbl<-list()
	for(i in files){
		c<-c+1
		if(verbose) {flush.console();cat("\r",round(c/n,3),i,sep=" ")}
		b<-readLines(i,warn=F)
		field<-sub("^(.{2}).+","\\1",b)
		cut<-field%in%c("FN","VR","","ER","EF")
		b<-b[!cut]
		field<-field[!cut]
		t<-field=="  "
		x<-which(t)
		y<-which(!t)
		if(any(t)) {
			for(j in 1:length(x)) x[j]<-y[sum(y<x[j])]
			field[t]<-field[x]
		}
		b<-sub("^.. ?(.*)","\\1",b)
		ind<-1:length(b)
		t<-field!="UT"
		x<-which(t)
		y<-which(!t)
		ind[t]<-ind[y[sapply(lapply(lapply(as.list(x),"<",y),which),min)]]
		d<-data.table(b.ind=b[ind],field,b)
		if(art_rev_only){
			dt<-which(d$field=="DT")
			dtt<-grepl("(Article)|(Review)",d$b[dt])
			if(!all(dtt)) {
				dt<-d$b.ind[dt[dtt]]
				setkey(d,b.ind)
				d<-d[dt]
			}
		}
		wok2dbl[[i]]<-copy(d)
	}
	wok2dbl<-rbindlist(wok2dbl)
	col.ord<-unique(wok2dbl$field)
	setnames(wok2dbl,c("b.ind","b"),c("id","val"))
	if(!is.null(imp.rul)){
		setkey(wok2dbl,field)
		imp<-list()
		for(i in names(imp.rul$collapse)) {
			imp[[i]]<-wok2dbl[list(i),list(val=paste(val,collapse=imp.rul$collapse[[i]])),by=c('id','field')]
			wok2dbl<-wok2dbl[!list(i)]
		}
		for(i in names(imp.rul$split)) {
			if(i%in%names(imp)) {
				imp[[i]]<-imp[[i]][,list(val=unlist(strsplit(val,split = imp.rul$split[[i]]))),by=c('id','field')]
			} else {
				imp[[i]]<-wok2dbl[list(i),list(val=unlist(strsplit(val,split = imp.rul$split[[i]]))),by=c('id','field')]
				wok2dbl<-wok2dbl[!list(i)]
			}
		}
		wok2dbl<-rbindlist(c(list(wok2dbl),imp))
	}
	setkey(wok2dbl,id,field)
	o<-wok2dbl[,list(o=1:.N),keyby=c('id','field')]$o
	wok2dbl[,o:=o]
	setkey(wok2dbl,id,field,val)
	dup<-duplicated(wok2dbl)
	if(any(dup)){
		ud<-unique(wok2dbl$id[dup])
		cat('\n\n',length(ud),' duplicate records detected and removed, e.g.:\n',sep='')
		if(length(ud)>5) {cat(sample(ud,5),sep='\n')} else {cat(ud,sep='\n')}
		wok2dbl<-unique(wok2dbl)
		setkey(wok2dbl,id,field,o)
		o<-wok2dbl[,list(o=1:.N),keyby=c('id','field')]$o
		wok2dbl[,o:=o]
	}
	wok2dbl<-droplevels(wok2dbl)
	attributes(wok2dbl)$col.ord<-col.ord
	setkey(wok2dbl,id,field,o,val)
	if(save) save(wok2dbl,file=paste(out,.Platform$file.sep,"wok2dbl.RData",sep=""))
	wok2dbl
}

dbl2inspect.f<-function(wok2dbl,reps=100,drop.miss.vars=F){
	require(reshape2)
	require(data.table)
	require(igraph)
	setkey(wok2dbl,field)
	pw<-combn(unique(wok2dbl[,field]),m=2,simplify = FALSE)
	el<-list()
	ew<-list()
	for(i in 1:length(pw)){
		dbw<-wok2dbl[pw[[i]]]
		if(anyDuplicated(dbw,by=c('id','field'))) next
		dbw<-dcast.data.table(
			data=dbw
			,formula=id~field
			,value.var='val'
		)[,2:3,with=F]
		if(any(sapply(dbw,function(x) any(is.na(x))))) next
		dbf<-dbw[,.N,keyby=eval(colnames(dbw))]
		if(any(dbf$N==1)) next
		ew[[length(ew)+1]]<-summary(table(dbw))$statistic
		#ew[[length(ew)+1]]<-summary(xtabs(as.formula(paste('N',paste(names(dbf),collapse='+'),sep='~')),data=dbf))$statistic
		l<-sort(sapply(dbf[,-3,with=F],function(x) length(unique(x))))
		el[[length(el)+1]]<-paste(names(l),l,sep=' ')
	}
	el<-do.call(rbind,el)
	ew<-scale(unlist(ew),center=F)
	g<-graph.edgelist(el,directed=T)
	E(g)$weight<-ew
	V(g)$weight<-as.integer(sub('^[^ ]+ ','',V(g)$name))
	u<-graph.edgelist(el,directed=F)
	E(u)$weight<-ew
	par(mfrow=c(2,2))
	wu<-walktrap.community(u)
	plot.communities(wu,u,edge.width=E(u)$weight*3,edge.arrow.size=.25,edge.arrow.width=.25)
	g$layout<-layout.sugiyama(g,layers=V(g)$weight)$layout
	plot(g,layout=g$layout,edge.width=E(g)$weight*3,edge.arrow.size=.25,edge.arrow.width=.25,mark.groups=split(1:length(membership(wu)),membership(wu)))
	dendPlot(wu,use.modularity = T)
	f<-wok2dbl[,.N,by=field]
	h<-cut(f$N,breaks=hist(f$N,main='Variables by observation count')$breaks)
	drop<-f$field[as.numeric(h)<which.max(table(h))]
	cat('Recommend dropping:',paste('c(\'',paste(drop,collapse='\',\''),'\')\n',collapse='',sep=''))
	if(drop.miss.vars) {wok2dbl<-wok2dbl[!drop];warning('wok2dbl altered in memory, variables not included in modal bin automatically dropped',immediate. = TRUE)}
	f<-wok2dbl[,.N,by=field]
	setkey(f,N)
	print(f)
	list(g=g,f=f,drop=drop)
}

dbl2dbw.f<-function(wok2dbl,form='id~field'){
	dbl2dbw<-dcast.data.table(
		data=wok2dbl
		,formula=as.formula(form)
		,value.var='val'
		,fun.aggregate = list
		,fill='NA'
	)
	ord<-copy(colnames(dbl2dbw))
	cat(ord)
	for(i in ord) {
		if(any(sapply(dbl2dbw[[i]],length)>1)) next
		rep<-type.convert(as.character(unlist(dbl2dbw[[i]])))
		dbl2dbw[,(i):=NULL,with=F]
		dbl2dbw[,(i):=rep,with=F]
		rm(rep)
	}
	if('col.ord'%in%names(attributes(wok2dbl))) setcolorder(dbl2dbw,c('id',intersect(attributes(wok2dbl)$col.ord,colnames(dbl2dbw))))
	dbl2dbw
}

dbl2bel.f<-function(
	wok2dbl
	,out=stop("Specify output directory for your project.")
	,check.for.saved.output=F
	,saved_recode=NULL
	,save_og4recode=F
	,trim_doi=T
	,capitalize=T
	,trim_anon=T
	,cut_samp_def=0 # if sample is based on a common citation set that should be removed for analysis
	,trim_pendants=T
	,trim_loops=T
)
{
	if(check.for.saved.output) if(any(grepl('dbl2bel.RData',dir(path=out,recursive=T,full.names=T,ignore.case=T)))) {
		warning('Loading and returning saved dbl2bel.RData.',call. = F)
		load(dir(path=out,pattern='dbl2bel.RData',recursive=T,full.names=T,ignore.case=F)[1])
		return(dbl2bel)
	}
	### check function requirements ###
	require(data.table)
	out

	### draw only citation edge information from wok2dbl ###
	wok2dbl<-data.table(wok2dbl)
	setkey(wok2dbl,field)
	dbl2bel<-wok2dbl["CR",list(id,val)]
	rm("wok2dbl")

	### impose formatting and nomenclature ###
	setnames(dbl2bel,c("ut","cr"))
	if(trim_doi) dbl2bel[,cr:=sub(", DOI .+","",cr)] #remove DOI
	if(capitalize) dbl2bel[,cr:=gsub("(\\w)","\\U\\1",cr,perl=T)] #capitalize
	if(trim_anon&capitalize) dbl2bel[,cr:=sub("^\\[ANONYMOUS\\], ","",cr)] #remove ANONYMOUS author

	### recoding ###
	setkey(dbl2bel,cr)
	if(save_og4recode) {
		# save original codes to disk
		cat('\nSaving normalized original CR codes to pass to fuzzy set routine.')
		original.cr<-unique(dob2bel$cr)
		save(original.cr,file=paste(out,.Platform$file.sep,'original-cr.RData',sep=""))
	}
	if(!is.null(saved_recode)) {
		# recode using fuzzy sets
		lfs<-length(saved_recode)
		cat('\nRecoding CR from',lfs,'sets.\n')
		dbl2bel[,zcr:=cr]
		for(i in 1:lfs) {
			cat('\r',round(i/lfs*100,4),'%\t\t',sep='')
			ix<-saved_recode[[i]]
			recode<-dbl2bel[ix,list(cr)][,.N,by=cr]
			recode<-recode$cr[recode$N==max(recode$N)]
			recode<-tolower(recode[which.min(nchar(recode))])
			dbl2bel[ix,zcr:=recode]
		}
		cat('\n')
	}
	### pendants ###
	if(trim_pendants) {
		dbl2bel[,pend:=!(duplicated(cr)|duplicated(cr,fromLast=T))]
		if(!is.null(saved_recode)) dbl2bel[,zpend:=!(duplicated(zcr)|duplicated(zcr,fromLast=T))]
	}

	### cut sample ###
	crd<-dbl2bel[,list(cr)][,.N,by=cr]
	setkey(crd,N)
	if(!is.null(saved_recode)) {
		setkey(dbl2bel,ut,zcr)
		dbl2bel[,zdup:=duplicated(dbl2bel)]
		zcrd<-dbl2bel[!dbl2bel$zdup,list(zcr)][,.N,by=zcr]
		setkey(zcrd,N)
		setkey(dbl2bel,cr)
	}

	### loops ### or pendants in the first partition
	if(trim_loops) {
		setkey(dbl2bel,ut)
		if(trim_pendants) {
			utloop<-dbl2bel[!dbl2bel$pend,.N,by=ut]
			setkey(utloop,N)
			utloop<-utloop[list(1),ut]
			dbl2bel[,loop:=FALSE]
			dbl2bel[utloop,loop:=TRUE]
		} else {dbl2bel[utloop,loop:=!(duplicated(ut)|duplicated(ut,fromLast=T))]}
		if(!is.null(saved_recode)){
			if(trim_pendants) {
				utloop<-dbl2bel[!(dbl2bel$zdup|dbl2bel$zpend),.N,by=ut]
			} else {
				utloop<-dbl2bel[!dbl2bel$zdup,.N,by=ut]
			}
			setkey(utloop,N)
			utloop<-utloop[list(1),ut]
			dbl2bel[,zloop:=FALSE]
			dbl2bel[utloop,zloop:=TRUE]
			rm(utloop)
		}
	}

	if(cut_samp_def>0){
		#cut highest degree citations, argument is the length of the list in descending order of frequency
		cat("\nEnter -indices separated by spaces- to reject high degree citations such as those defining the sample, or -enter- to reject none.\n")
		cut<-crd[nrow(crd):(nrow(crd)-cut_samp_def),list(cr=cr,index=1:cut_samp_def,N=N)]
		print(cut)
		u<-readLines(n=1)
		if(u!=""){
			u<-unlist(strsplit(u," "))
			u<-as.integer(u)
			if(any(is.na(u))) {stop("\nTry again.")} else {dbl2bel<-dbl2bel[!list(cut[u]$cr)]}
		}
	}

	### results ###
	print(crd)
	cat('\n')
	if(!is.null(saved_recode)) print(zcrd)
	cat('\n')

	setkey(crd,N)
	if(!is.null(saved_recode)) setkey(zcrd,N)

	res<-data.frame(case=c('Total acts of reference'
												 ,'Unique citations'
												 ,'Citations referenced once'
												 ,'Loop references'
												 ,'Total referenced twice or more'
												 ,'  Unique referenced twice or more'
	)
	,cr=c(crd[,sum(N)]
				,nrow(crd)
				,nrow(crd[list(1)])
				,dbl2bel[,sum(loop&!pend)]
				,crd[!list(1),sum(N)]
				,nrow(crd[!list(1)])
	)
	)
	if(!is.null(saved_recode)) {
		res$zcr<-c(zcrd[,sum(N)]
							 ,nrow(zcrd)
							 ,nrow(zcrd[list(1)])
							 ,dbl2bel[,sum(zloop&!zpend)]
							 ,zcrd[!list(1),sum(N)]
							 ,nrow(zcrd[!list(1)]))
		res$change<-res$zcr-res$cr
		res$perc.change<-round(res$zcr/res$cr*100,5)
		res$alt.change<-res$perc.change-100
	}
	print(res)
	cat('\n')
	mres<-data.frame(case=c(
		'Pendants as % of total references'
		,'Pendants as % of unique citations')
		,cr=round(c(res$cr[3]/res$cr[1],res$cr[3]/res$cr[2])*100,4)
	)

	if(!is.null(saved_recode)) mres$zcr<-round(c(res$zcr[3]/res$zcr[1],res$zcr[3]/res$zcr[2])*100,4)
	print(mres)
	attributes(dbl2bel)$results<-list(res,mres)
	save(dbl2bel,file=paste(out,.Platform$file.sep,"dbl2bel.RData",sep=""))
	dbl2bel
}

bel2mel.f<-function(
	dbl2bel=NULL
	,subset=NULL # a vector of UT
	,type=c("utel","crel")
	,out=stop("Specify output directory for your project.")
	,check.for.saved.output=F
	,write2disk=F
)
{
	if(check.for.saved.output) if(any(grepl('bel2mel.RData',dir(path=out,recursive=T,full.names=T,ignore.case=T)))) {
		warning('Loading and returning saved bel2mel.RData.',call. = F)
		load(dir(path=out,pattern='bel2mel.RData',recursive=T,full.names=T,ignore.case=F)[1])
		return(bel2mel)
	}
	out
	require(data.table)
	if(ncol(dbl2bel)>2) stop('dbl2bel must be a bimodal edgelist as a two column data.table with UT in first column and CR in second. Selection on pendants, use of fuzzy replacement, etc. should be made prior to passing to bel2mel.f.')

	setnames(dbl2bel,c('ut','cr'))
	setkey(dbl2bel,ut,cr)
	dup<-duplicated(dbl2bel)
	if(sum(dup)) {
		warning(paste(sum(dup),'duplicate lines detected and removed. This may make sense after fuzzy set replacement if two items in the same bibligoraphy were treated as the same reference.'),call.=FALSE,immediate.=TRUE)
		dbl2bel<-unique(dbl2bel)
	}

	cat(c('\nBipartite edge list is now',nrow(dbl2bel),'rows long.'))

	if(write2disk){
		bel2mel<-list()
		dbl2bel[,`:=`(ut=factor(ut),cr=factor(cr))]
		if('utel'%in%type){
			lut<-gzfile(paste(out,'bel2mel-ut-levs.txt.gz',sep=.Platform$file.sep),'w')
			writeLines(dbl2bel[,levels(ut)],lut)
			close(lut)

		}
		if('crel'%in%type){
			lcr<-gzfile(paste(out,'bel2mel-cr-levs.txt.gz',sep=.Platform$file.sep),'w')
			writeLines(dbl2bel[,levels(cr)],lcr)
			close(lcr)

		}

		warn<-paste('Monopartite edgelist and levels written to',out,sep='')
		warning(warn,call.=FALSE,immediate.=TRUE)
		return(warn)
	}

	bel2mel<-list()
	dbl2bel[,ut:=as.character(ut)]
	dbl2bel[,cr:=as.character(cr)]
	if('crel'%in%type){
		cat('\nBeginning CR-UT-CR edgelist.')
		setkey(dbl2bel,ut,cr)
		utd<-dbl2bel[,.N,by=ut]
		setkey(utd,N,ut)
		if(any(utd$N>1)) {
			utiso<-utd[list(1),ut]
			bel2mel$crel<-dbl2bel[!list(utiso),data.table(do.call(rbind,lapply(1:(length(cr)-1),function(y) matrix(cr[c(rep(y,length(cr)-y),(y+1):length(cr))],ncol=2) ))),by=ut]
			setnames(bel2mel$crel,old=c('V1','V2'),new=c('cr1','cr2'))
			bel2mel$crel<-bel2mel$crel[,list(ew=.N,ut=list(ut)),keyby=c('cr1','cr2')]
		} else {bel2mel$crel<-'No connected crel.'}
		cat(' Done.')
		if('utel'%in%type) {
			save(bel2mel,file=paste(out,'bel2mel.RData',sep=.Platform$file.sep))
			cat(' Saved.')
		}
	}
	if('utel'%in%type){
		cat('\nBeginning UT-CR-UT edgelist.')
		setkey(dbl2bel,cr,ut)
		crd<-dbl2bel[,.N,by=cr]
		setkey(crd,N,cr)
		if(any(crd$N>1)) {
			criso<-crd[list(1),cr]
			bel2mel$utel<-dbl2bel[!list(criso),data.table(do.call(rbind,lapply(1:(length(ut)-1),function(y) matrix(ut[c(rep(y,length(ut)-y),(y+1):length(ut))],ncol=2) ))),by=cr]
			setnames(bel2mel$utel,old=c('V1','V2'),new=c('ut1','ut2'))
			bel2mel$utel<-bel2mel$utel[,list(ew=.N,cr=list(cr)),keyby=c('ut1','ut2')]
		} else {bel2mel$utel<-'No connected utel.'}
		cat(' Done.')
	}
	save(bel2mel,file=paste(out,'bel2mel.RData',sep=.Platform$file.sep))
	cat(' Saved.\n\n')
	bel2mel
}

mel2comps.f<-function(
	bel2mel
	,type=c('crel','utel')
	,out=stop('Specify output directory')
	,min.size=3
)
{
	require(data.table)
	require(igraph)
	out
	ret<-list()
	for(i in type) if(i%in%names(bel2mel)) {
		cat('\n',i,'\n',sep='')
		levs<-factor(bel2mel[[i]][,do.call(c,.SD),.SDcols=1:2])
		bel2mel[[i]]<-matrix(as.character(as.integer(levs)),ncol=2)
		levs<-levels(levs)
		g<-graph.edgelist(bel2mel[[i]],FALSE)
		g<-decompose.graph(g)
		comp.sizes<-sapply(g,vcount)
		print(table(comp.sizes))
		cat('Only networks of size',min.size,'and above returned.')
		cat('\nDirectories created:')
		mapply(
			function(net,id,vcount) {
				path<-paste(out,'mel2comps',i,paste(id,vcount,sep='-'),sep=.Platform$file.sep)
				dir.create(path,recursive=T)
				cat('\n',path)
				write.table(get.edgelist(net),file=paste(path,'mel2comps.txt',sep=.Platform$file.sep),sep='\t',quote=F,na='',row.names=F,col.names=F)
			}
			,net=g[comp.sizes>=min.size]
			,id=(1:length(g))[comp.sizes>=min.size]
			,vcount=comp.sizes[comp.sizes>=min.size]
		)
		writeLines(levs,con=paste(out,'mel2comps',i,'mel2comps-levs.txt',sep=.Platform$file.sep))
		ret[[i]]<-g[comp.sizes>=min.size]
	}
	ret
}

comps2cos.f<-function(
	mel2comps.dir=stop('Specify directory where mel2comps.txt edgelists are located.')
	,cosparallel.path=stop('Specify path to cosparallel executable (e.g. ~/cosparallel-code/cos)')
	,threads=1
)
{
	mel2comps.dir
	cosparallel.path

	mel2comps<-list.files(mel2comps.dir,pattern='mel2comps\\.txt$',recursive=T,full.names=T)

	for(i in mel2comps){
		com<-paste(
			'cd \'',sub('mel2comps.txt','\'',i)
			,' && ',sub('cos$','extras/maximal_cliques',cosparallel.path),' mel2comps.txt'
			,' && ',cosparallel.path,' -P ',threads,' mel2comps.txt.mcliques'
			,sep='')
		cat('Source data: ',i,'\n\nThreads used: ',threads,'\n\nsh: ',com,'\n\ncos stdout:\n\n',sep='')
		system(
			command=com
			#		,stdout='stdout.txt'
		)
	}
}

cos2kcliqdb.f<-function(
	mel2comps.dir=stop('Specify a mel2comps directory that includes cos output.')
	,out=stop('Specify output directory.')
	,type=c('crel','utel')
)
{
	require(data.table)
	mel2comps.dir
	for(i in type) if(i%in%dir(mel2comps.dir)) {
		p<-paste(mel2comps.dir,i,sep=.Platform$file.sep)
		d<-list.dirs(p)[-1]
		olevs<-readLines(list.files(p,pattern='levs',full.names=T))
		ret<-list()
		ret[[i]]$orig<-lapply(d,function(j) {
			f<-list.files(j,pattern='[0-9]_communities\\.txt$',full.names=T)
			if(length(f)) {
				coslevs<-read.table(list.files(j,full.names=T,pattern='map$'))$V1
				k<-as.integer(sub('^.+/([0-9]+)_.+$','\\1',f))
				f<-f[order(k)]
				k<-k[order(k)]
				cos<-lapply(f,function(x) {
					x<-readLines(x)
					x<-sapply(x,function(y) lapply(strsplit(y,split='[: ]'),as.integer))
					x<-data.table(id=sapply(x,function(y) y[1]),memb=lapply(x,function(y) y[-1]))
					x<-x[,list(memb=list(memb)),by=id]
					x<-lapply(x$memb,function(y) sort(unique(unlist(y))))
					x
				})
				dup<-duplicated(cos,fromLast=T)
				cos<-cos[!dup]
				k<-k[!dup]
				f<-f[!dup]
				names(cos)<-paste('k',k,'c',sub('^.+/([0-9]+)-[0-9]+/.+','\\1',f),'-',sep='')
				cos<-do.call(c,cos)
				names(cos)<-sub('-$','',names(cos))
				cos<-lapply(cos,function(x) sort(coslevs[x+1]))
				return(cos)
			}
		})
		ret[[i]]$orig<-do.call(c,ret[[i]]$orig)
		ret[[i]]$orig<-split(ret[[i]]$orig,f=sub('^k([0-9]+).+$','\\1',names(ret[[i]]$orig)))
		ret[[i]]$orig<-ret[[i]]$orig[order(as.integer(names(ret[[i]]$orig)))]
		names(ret[[i]]$orig)<-paste('k',names(ret[[i]]$orig),sep='')
		cat('\n',i,'k-clique community distribution (original)\n')
		print(sapply(ret[[i]]$orig,length))

		### strict membership interpretation
		x<-lapply(ret[[i]]$orig,function(x) sort(unique(unlist(x))))
		x<-rev(lapply(length(x):1, function(y) sort(unique(unlist(x[length(x):y])))))
		for(j in 1:(length(x)-1)) x[[j]] <- setdiff(x[[j]],x[[j+1]])
		ret[[i]]$strict<-mapply(function(com,reg) lapply(com,function(y) intersect(y,reg)),com=ret[[i]]$orig,reg=x) # com=communities reg=register
		ret[[i]]$strict<-lapply(ret[[i]]$strict,function(x) x[!!sapply(x,length)])
		ret[[i]]$strict<-ret[[i]]$strict[!!sapply(ret[[i]]$strict,length)]
		names(ret[[i]]$strict)<-paste(names(ret[[i]]$strict),'-',sep='')
		ret[[i]]$strict<-do.call(c,ret[[i]]$strict)
		names(ret[[i]]$strict)<-sub('^.+\\.','',sub('-$','',names(ret[[i]]$strict)))

		cat('\n',i,'k-clique community distribution (strict)\n')
		t<-table(as.integer(sub('^k([0-9]+).+$','\\1',names(ret[[i]]$strict))))
		names(t)<-paste('k',names(t),sep='')
		print(t)

		attributes(ret[[i]])$levels<-olevs
	}
	save(ret,file=paste(out,'cos2kcliqdb.RData',sep=.Platform$file.sep))
	ret
}

kcliqdb2viz.f<-function(
	cos2kcliqdb
	,mel2comps.dir=stop('Specify a mel2comps directory that includes cos output.')
	,out=stop('Specify output directory.')
	,type=c('crel','utel')
)
{
	require(igraph)
	mel2comps.dir
	ret<-list()
	for(i in type) if(i%in%dir(mel2comps.dir)){
		t0<-proc.time()
		p<-paste(mel2comps.dir,i,sep=.Platform$file.sep)
		olevs<-readLines(list.files(p,pattern='levs',full.names=T))
		els<-list.files(p,recursive=T,full.names=T,pattern='mel2comps.txt$')
		els<-as.matrix(do.call(rbind,lapply(els,function(x) read.delim(x,header=F,quote='',fill=F,colClasses='character'))))
		ret[[i]]$g<-graph.edgelist(els,F)
		key<-data.table(gph=1:vcount(ret[[i]]$g),src=as.integer(V(ret[[i]]$g)$name))
		setkey(key,src)

		# plot strict interpretation with terrain colors
		cos2kcliqdb[[i]]$strict<-lapply(cos2kcliqdb[[i]]$strict,function(x) key[list(x),gph])
		pdf(paste(out,paste('kcliqdb2vis',i,'strict.pdf',sep='-'),sep=.Platform$file.sep))
		cols<-as.integer(sub('^k([0-9]+).*$','\\1',names(cos2kcliqdb[[i]]$strict)))-2
		mark.col<-terrain.colors(max(cols))[cols]
		mark.border<-'white'
		plot(
			ret[[i]]$g
			# 			,mark.groups=tail(cos2kcliqdb[[i]]$strict,1)
			# 			,mark.expand=5
			#			,mark.col=mark.col
			#			,mark.border=mark.border
			,vertex.size=0
			,vertex.shape='square'
			,vertex.label=NA
			,vertex.color=gray(0,.1)
			,vertex.frame.color=NA
			#			,vertex.label.cex=0
			,edge.color=gray(0,.1)
			#,vertex.color=c('white','gray')[(1:length(V(gut4_1g))%in%unique(unlist(hulls1[samp1])))+1]
			,main="Strict"
			,layout=ret[[i]]$lout
		)
		dev.off()
		t1<-proc.time()
		t1-t0

		## Plot kcoms as nodes in hierarchical tree

		trg<-list()
		for(j in length(cos2kcliqdb[[i]]$orig):2) trg[[j]]<-graph.incidence(sapply(cos2kcliqdb[[i]]$orig[[j]],function(x) sapply(cos2kcliqdb[[i]]$orig[[j-1]],function(y) sum(x%in%y))),mode='in',weighted=T,directed=T)
		trg<-do.call(graph.union,trg)
		V(trg)$k<-as.integer(sub('^k([0-9]+).+$','\\1',V(trg)$name))
		V(trg)$layer<-max(V(trg)$k)-V(trg)$k+1
		tc<-as.integer((log(V(trg)$k-2)+.1)*10)
		V(trg)$terrain<-terrain.colors(round(max(tc)+.1*max(tc)),alpha=1)[tc]
		trg$layout<-layout.sugiyama(trg,hgap=15,layers=V(trg)$layer,maxiter=10000)$layout

		pdf(paste(out,paste('kcliqdb2vis',i,'sugiyama.pdf',sep='-'),sep=.Platform$file.sep))
		plot(trg
				 ,layout=trg$layout
				 ,vertex.label=NA
				 ,vertex.size=4
				 ,vertex.shape='square'
				 ,vertex.frame.color=NA # 'white'  #gray(0,.05)
				 ,vertex.color=V(trg)$terrain
				 ,edge.arrow.mode='-'
				 ,edge.width=.1
				 ,edge.color=gray(.1,1)
		)
		dev.off()
	}
}

mel2cfinder.f<-function(
	bel2mel
	,out=stop("Specify output directory for your project.")
)
{
	library(data.table)
	if('crel'%in%names(bel2mel)) {
		bel2mel$crel[,ut:=NULL]
		setkey(bel2mel$crel,cr1,cr2)
		lcr<-sort(unique(unlist(bel2mel$crel[,list(cr1,cr2)])))
		bel2mel$crel[,`:=`(cr1=as.integer(factor(cr1,levels=lcr)),cr2=as.integer(factor(cr2,levels=lcr)))]
		clcr<-gzfile(paste(out,'bel2mel-crlevs.txt.gz',sep=.Platform$file.sep),'w')
		writeLines(lcr,con=clcr)
		close(clcr)
		ccrel<-gzfile(paste(out,'bel2mel-crel.txt.gz',sep=.Platform$file.sep),'w')
		write.table(bel2mel$crel,file=ccrel,quote=FALSE,sep='\t',na='',row.names=FALSE,col.names=FALSE)
		close(ccrel)
	}
	if('utel'%in%names(bel2mel)) {
		bel2mel$utel[,cr:=NULL]
		setkey(bel2mel$utel,ut1,ut2)
		lut<-sort(unique(unlist(bel2mel$utel[,list(ut1,ut2)])))
		bel2mel$utel[,`:=`(ut1=as.integer(factor(ut1,levels=lut)),ut2=as.integer(factor(ut2,levels=lut)))]
		clut<-gzfile(paste(out,'bel2mel-utlevs.txt.gz',sep=.Platform$file.sep),'w')
		writeLines(lut,con=clut)
		close(clut)
		cutel<-gzfile(paste(out,'bel2mel-utel.txt.gz',sep=.Platform$file.sep),'w')
		write.table(bel2mel$utel,file=cutel,quote=FALSE,sep='\t',na='',row.names=FALSE,col.names=FALSE)
		close(cutel)
	}
}

cfinder2all.f<-function(
	cf.in=stop('cf.out.dir = Path to CFinder output.')
	,which=c('communities_links'
					 ,'communities'
					 ,'communities_cliques'
					 ,'degree_distribution'
					 ,'graph_of_communities'
					 ,'membership_distribution'
					 ,'overlap_distribution'
					 ,'size_distribution'
	)
)
{
	require(data.table)
	fileps<-list.files(cf.in,recursive=T,full.names=T)
	ret<-list()
	for(i in which){
		if(c('communities_links')%in%i){
			cat('compiling',i,'\n')

			makerocketgonow<-function(raw){
				raw<-readLines(raw)
				raw<-strsplit(raw[grep('^[0-9][^:]+$',raw)],' ')
				raw<-data.table(do.call(rbind,lapply(raw,function(x) sort(as.integer(x)))))
				setnames(raw,c('src','tgt'))
				raw<-unique(raw)
				raw
			}

			require(doParallel)
			cl <- makeCluster(detectCores() )
			registerDoParallel(cl, cores = detectCores() )

			ret[[i]] <- foreach(j = grep(paste('.+',.Platform$file.sep,i,'$',sep=''),fileps,value=T),.packages = c("data.table"),.inorder=F) %dopar% {
				makerocketgonow(raw=j)
			}

			stopCluster(cl)

			ret[[i]]<-rbindlist(ret[[i]])[,list(ew=.N),by=c('src','tgt')]
			setkey(ret[[i]],ew)
		}
		if(c('communities')%in%i){
			cat('compiling',i,'\n')

			makerocketgonow<-function(raw){
				stub<-sub('^.+k=([0-9]+).+$','k\\1',raw)
				raw<-readLines(raw)
				raw<-strsplit(grep('^[0-9]',raw,value=T),':? ')
				nams<-sapply(raw,function(x) x[1])
				raw<-lapply(raw,function(x) as.integer(x[-1]))
				names(raw)<-paste(stub,nams,sep='-')
				raw
			}

			require(doParallel)
			cl <- makeCluster(detectCores() )
			registerDoParallel(cl, cores = detectCores() )

			ret[[i]] <- foreach(j = grep(paste('.+',.Platform$file.sep,i,'$',sep=''),fileps,value=T),.packages = c("data.table"),.inorder=F) %dopar% {
				makerocketgonow(raw=j)
			}

			stopCluster(cl)

			names(ret[[i]])<-sapply(ret[[i]],function(x) sub('^([^-]+).+$','\\1',names(x[1])))
		}
	}
	ret
}

net2perm.f<-function(
	mel2net
	,nsim
)
{
	require(network)
	cat("\nPermuting random poisson edge distribution\nSimulating...")
	perm<-list()
	s<-network.size(mel2net)
	maxcombo<-s*(s-1)/2
	combos<-1:maxcombo
	choices<-sum(mel2net%e%"ew")
	t1<-proc.time()
	for(i in 1:nsim){
		cat("\r",i,sep="")
		rdist<-sample(combos,size=choices,replace=T)
		rdist<-table(rdist)
		z<-maxcombo-length(rdist)
		rdist<-table(rdist)
		n<-names(rdist)
		rdist<-c(z,rdist)
		names(rdist)<-c("0",n)
		rdist<-as.table(rdist)
		perm[[i]]<-rdist
	}
	cat("\nSeconds to simulate:")
	print(proc.time()-t1)

	edist<-table(mel2net%e%"ew")
	n<-length(unique(unlist(bel2mel[[i]][,1:2])))
	z<-(n*(n-1)/2)-sum(edist)
	n<-names(edist)
	edist<-c(z,edist)
	names(edist)<-c("0",n)
	edist<-as.table(edist)

	cn<-sort(unique(unlist(lapply(perm,names))))
	permdb<-data.frame(matrix(0,nrow=length(perm),ncol=length(cn)))
	permdb<-data.frame(permdb,matrix(0,nrow=length(perm),ncol=length(edist)-length(cn)))
	names(permdb)<-names(edist)
	for(i in 1:length(perm)) permdb[i,names(perm[[i]])]<-perm[[i]]

	dif<-matrix(edist,nrow=dim(permdb)[1],ncol=dim(permdb)[2],byrow=T)-permdb
	md<-apply(dif,2,mean)
	sdd<-apply(dif,2,sd)
	cid<-apply(dif,2,quantile,prob=c(.05,.95))
	tad<-apply(dif,2,table)
	num<-lapply(tad,names)
	num<-lapply(num,as.numeric)

	dens<-1-(permdb$`0`/maxcombo)
	mean((1-edist["0"]/maxcombo)-dens)*100
	sd((1-edist["0"]/maxcombo)-dens)*100
	mean((1-edist["0"]/maxcombo)-dens)/sd((1-edist["0"]/maxcombo)-dens)

	apply(t(apply(permdb,1,"*",as.numeric(colnames(permdb)))),sum)/maxcombo

	round(cbind(md,sdd,t=md/sdd,p=0,t(cid)),digits=3)

	round(cbind(md=md/maxcombo,sdd=sdd/maxcombo,t=md/sdd,p=0),digits=4)[1:7,]
	sum(edist[-(1:7)])/maxcombo

	plot(as.table((md/sdd)[1:7]),type="l",ylim=range((md/sdd)[1:7]),lwd=3)
	abline(h=0,lty="dotted",lwd=3)

	round(cbind(md,sdd,t=md/sdd,p=0),digits=1)[1:7,]
	sum(edist[-(1:7)])

	mp<-apply(dif/maxcombo,2,mean)
	sdp<-apply(dif/maxcombo,2,sd)
	cip<-apply(dif/maxcombo,2,quantile,prob=c(.05,.95))

	round(cbind(mp,sdp,p=0,t(cip)),digits=5)[1:7,]
	sum(edist[-(1:7)])

	mvd<-mean((sum(((as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2)*edist)/maxcombo)-(apply(apply(permdb,1,"*",(as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2),2,sum)/maxcombo))
	sddv<-sd((sum(((as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2)*edist)/maxcombo)-(apply(apply(permdb,1,"*",(as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2),2,sum)/maxcombo))
	cidv<-quantile((sum(((as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2)*edist)/maxcombo)-(apply(apply(permdb,1,"*",(as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2),2,sum)/maxcombo),prob=c(.05,.95))
	round(c(mvd=mvd,sddv=sddv,tvd=mvd/sddv,p=0,cidv=cidv),digits=4)

	m<-apply(permdb,2,mean)
	sd<-apply(permdb,2,sd)
	ci<-apply(permdb,2,quantile,prob=c(.05,.95))
	cbind(m,sd,ci)

	zscores<-(edist[1:7]-apply(permdb,2,mean))/apply(permdb,2,sd)

	apply(matrix(edist,nrow=dim(permdb)[1],ncol=dim(permdb)[2],byrow=T)-permdb,2,mean)
	apply(matrix(edist,nrow=dim(permdb)[1],ncol=dim(permdb)[2],byrow=T)-permdb,2,sd)
}

mel2net.f<-function(
	bel2mel
	,count=T
	,rcount=T
	,out=NULL
)
{
	cat("mel2net.f aka Plagiat!","Written by Brooks Ambrose\n",sep="\n")
	if(!count&rcount) warning("\nrcount iff count=T",call.=F)
	library(network)
	mel2net<-list()
	for(i in names(bel2mel)){
		if(is.na(bel2mel[[i]])) {mel2net[[i]]<-NA;next}
		bel2mel[[i]]<-bel2mel[[i]][order(bel2mel[[i]][,1],bel2mel[[i]][,2]),]
		mel2net[[i]]<-network(bel2mel[[i]][,1:2],matrix.type="edgelist",directed=F)
		mel2net[[i]]%e%"ew"<-bel2mel[[i]]$ew
		if(count) if("count"%in%names(attributes(bel2mel[[i]]))) {
			attributes(mel2net[[i]])$count<-attributes(bel2mel[[i]])$count
			if(rcount){

			}
		}

	}
	names(mel2net)<-sub("el$","",names(mel2net))
	if(!is.null(out)) save(mel2net,file=paste(out,"mel2net.RData",sep=.Platform$file.sep))
	mel2net
}

as.edgelist<-function(
	net
)
{
	require(network)
	el<-matrix(unlist(do.call(rbind,net$mel)[,2:1]),ncol=2)
	el<-cbind(
		s=network.vertex.names(net)[el[,1]]
		,r=network.vertex.names(net)[el[,2]]
	)
	el
}

plot.mode.projection<-function(
	dbl2bel
	,m1.stub="^m1"
	,out="/Users/bambrose/Dropbox/2013-2014/winter2014_isi_data/1941out/descriptive/" #may add a stub at the end of this line
	,vcx=1
	,vlw=1
	,vlt="solid"
	,elw=1
	,elt="solid"
	,loopcx=1
	,m1vsides=3
	,m2vsides=4
	,m1col="black"
	,m2col="white"
	,ecol="gray"
	,trim.pendants=c("don't","m1","m2")
	,bmain="Bimodal"
	,m1main="Mode 1"
	,m2main="Mode 2"
	,mar=1
	,omi=.25
	,cex=1
	,pnt.v=list() #named list of character vectors where name is color and contents are vertices to paint
	,pnt.e=list() #named list of directed character edgelists where name is color and contents are edges to paint
	,pnt.vlty=list() #named list of character vectors where name is lty and contents are vertices to paint
	,pnt.lty=list() #named list of directed character edgelists where name is lty and contents are edges to paint
	,pnt.v2e=T
	,layout=list()
)
{
	require(network)

	el<-data.frame(s=as.character(el[,1]),r=as.character(el[,2]),stringsAsFactors=F)
	el<-el[order(el$s,el$r),]
	suppressWarnings(if(trim.pendants=="m1") {
		t<-table(el[[1]])
		elo<-el
		el<-el[el[[1]]%in%names(t)[t>1],]
	})
	suppressWarnings(if(trim.pendants=="m2") {
		t<-table(el[[2]])
		elo<-el
		el<-el[el[[2]]%in%names(t)[t>1],]
	})
	trim<-grepl("^m",trim.pendants[1])
	if(trim){
		om1n<-sort(unique(elo[[1]]))
		om2n<-sort(unique(elo[[2]]))
		om1l<-length(om1n)
		om2l<-length(om2n)
		(pbmnet<-network(elo,bipartite=om1l,directed=T,matrix.type="edgelist"))
	}

	### paint logic
	if(length(pnt.v)&pnt.v2e) for(i in 1:length(pnt.v)) {
		pnt.e[[length(pnt.e)+1]]<-t(apply(combn(
			setdiff(unlist(el[apply(apply(el,2,is.element,pnt.v[[i]]),1,any),]),pnt.v[[i]])
			,2),2,sort))
		names(pnt.e)[length(pnt.e)]<-names(pnt.v[i])
	}

	if(length(pnt.e)) for(i in 1:length(pnt.e)) colnames(pnt.e[[i]])<-c("s","r")

	if(length(pnt.vlty)&pnt.v2e) for(i in 1:length(pnt.vlty)) {
		pnt.lty[[length(pnt.lty)+1]]<-t(apply(combn(
			setdiff(unlist(el[apply(apply(el,2,is.element,pnt.vlty[[i]]),1,any),]),pnt.vlty[[i]])
			,2),2,sort))
		names(pnt.lty)[length(pnt.lty)]<-names(pnt.vlty[i])
	}
	if(length(pnt.lty)) for(i in 1:length(pnt.lty)) colnames(pnt.lty[[i]])<-c("s","r")

	print(pnt.v)
	print(pnt.e)
	print(pnt.vlty)
	print(pnt.lty)

	m1n<-sort(unique(el[[1]]))
	m2n<-sort(unique(el[[2]]))
	m1l<-length(m1n)
	m2l<-length(m2n)
	(bmnet<-network(el,bipartite=m1l,directed=T,matrix.type="edgelist"))

	m<-bmnet[,]
	w<-grepl(m1.stub,colnames(m))
	m1<-m[w,!w]%*%t(m[w,!w])
	m2<-t(m[w,!w])%*%m[w,!w]
	m1[lower.tri(m1)]<-0
	m2[lower.tri(m2)]<-0

	m1<-data.frame(s=rownames(m1)[which(!!m1,arr.ind=T)[,1]],r=rownames(m1)[which(!!m1,arr.ind=T)[,2]],ew=m1[which(!!m1)],stringsAsFactors=F)
	m2<-data.frame(s=rownames(m2)[which(!!m2,arr.ind=T)[,1]],r=rownames(m2)[which(!!m2,arr.ind=T)[,2]],ew=m2[which(!!m2)],stringsAsFactors=F)

	m1<-m1[order(m1$s,m1$r),]
	m2<-m2[order(m2$s,m2$r),]

	(m1net<-network(m1[,1:2],loops=T,directed=F,matrix.type="edgelist"))
	(m2net<-network(m2[,1:2],loops=T,directed=F,matrix.type="edgelist"))

	network.vertex.names(m1net)<-m1n
	network.vertex.names(m2net)<-m2n

	set.edge.attribute(m1net, "ew", m1$ew)
	set.edge.attribute(m2net, "ew", m2$ew)

	#mats<-matrix(nrow=m1l,ncol=m2l,dimnames=list(m1n,m2n))
	#system.time(for(i in 1:nrow(el)) mats[el[[1]],el[[2]]]<-1)
	#matr<-matrix(sample(c(0,0,0,0,1),replace=T,size=24),ncol=6,nrow=4)
	#mat<-matr
	#mat<-mats

	pdf(paste(out,"mode_projection.pdf",sep=""),h=(8.5-2)/3*ifelse(trim,2,1),w=(8.5-2)/3*ifelse(trim,2,3))
	if(trim)  par(mfrow=c(2,2),mar=rep(mar,4),omi=rep(omi,4)) else par(mfrow=c(1,3),mar=rep(mar,4),omi=rep(omi,4))
	if(trim){
		vcol<-c(rep(m1col,om1l),rep(m2col,om2l))
		el<-data.frame(as.edgelist(pbmnet))
		if(!is.directed(pbmnet)) el<-data.frame(t(apply(el,1,sort)),stringsAsFactors=F)
		ecolp<-rep(ecol,nrow(el))
		eltp<-rep(elt,nrow(el))
		vltp<-rep(vlt,network.size(pbmnet))

		if(length(pnt.v)) for(i in 1:length(pnt.v)) vcol[network.vertex.names(pbmnet)%in%pnt.v[[i]]]<-names(pnt.v[i])
		if(length(pnt.e)) for(i in 1:length(pnt.e)) ecolp[which(duplicated(rbind(do.call(cbind,el),pnt.e[[i]]),fromLast=T))]<-names(pnt.e[i])
		if(length(pnt.vlty)) for(i in 1:length(pnt.vlty)) vltp[network.vertex.names(pbmnet)%in%pnt.vlty[[i]]]<-names(pnt.vlty[i])
		if(length(pnt.lty)) for(i in 1:length(pnt.lty)) eltp[which(duplicated(data.frame(rbind(do.call(cbind,el),pnt.lty[[i]])),fromLast=T))]<-names(pnt.lty[i])

		lay<-list()
		for(i in names(layout)) lay[[i]]<-layout[[i]](x=network.size(pbmnet))

		plot(pbmnet
				 ,vertex.sides=c(rep(m1vsides,om1l),rep(m2vsides,om2l))
				 ,vertex.col=vcol
				 ,edge.col=ecolp
				 ,vertex.cex=vcx
				 ,vertex.lwd=vlw
				 ,edge.lwd=elw
				 ,edge.lty=eltp
				 ,vertex.lty=vltp
				 ,layout.par=lay
		)
		box()
		mtext(paste("Original",bmain),cex=cex)
	}

	vcol<-c(rep(m1col,m1l),rep(m2col,m2l))
	el<-data.frame(as.edgelist(bmnet))
	if(!is.directed(bmnet)) el<-data.frame(t(apply(el,1,sort)),stringsAsFactors=F)
	ecolp<-rep(ecol,nrow(el))
	eltp<-rep(elt,nrow(el))
	vltp<-rep(vlt,network.size(bmnet))

	if(length(pnt.v)) for(i in 1:length(pnt.v)) vcol[network.vertex.names(bmnet)%in%pnt.v[[i]]]<-names(pnt.v[i])
	if(length(pnt.e)) for(i in 1:length(pnt.e)) ecolp[which(duplicated(rbind(do.call(cbind,el),pnt.e[[i]]),fromLast=T))]<-names(pnt.e[i])
	if(length(pnt.vlty)) for(i in 1:length(pnt.vlty)) vltp[network.vertex.names(bmnet)%in%pnt.vlty[[i]]]<-names(pnt.vlty[i])
	if(length(pnt.lty)) for(i in 1:length(pnt.lty)) eltp[which(duplicated(data.frame(rbind(do.call(cbind,el),pnt.lty[[i]])),fromLast=T))]<-names(pnt.lty[i])

	lay<-list()
	for(i in names(layout)) lay[[i]]<-layout[[i]](x=network.size(bmnet))

	plot(bmnet
			 ,vertex.sides=c(rep(m1vsides,m1l),rep(m2vsides,m2l))
			 ,vertex.col=vcol
			 ,edge.col=ecolp
			 ,vertex.cex=vcx
			 ,vertex.lwd=vlw
			 ,edge.lwd=elw
			 ,edge.lty=eltp
			 ,vertex.lty=vltp
			 ,layout.par=lay
	)
	box()
	mtext(ifelse(trim,paste(bmain,"w/o Pendants"),bmain),cex=cex)

	vcol<-rep(m1col,m1l)
	el<-data.frame(as.edgelist(m1net))
	if(!is.directed(m1net)) el<-data.frame(t(apply(el,1,sort)),stringsAsFactors=F)
	ecolp<-rep(ecol,nrow(el))
	eltp<-rep(elt,nrow(el))
	vltp<-rep(vlt,network.size(m1net))

	if(length(pnt.v)) for(i in 1:length(pnt.v)) vcol[network.vertex.names(m1net)%in%pnt.v[[i]]]<-names(pnt.v[i])
	if(length(pnt.e)) for(i in 1:length(pnt.e)) ecolp[which(duplicated(rbind(do.call(cbind,el),pnt.e[[i]]),fromLast=T))]<-names(pnt.e[i])
	if(length(pnt.vlty)) for(i in 1:length(pnt.vlty)) vltp[network.vertex.names(m1net)%in%pnt.vlty[[i]]]<-names(pnt.vlty[i])
	if(length(pnt.lty)) for(i in 1:length(pnt.lty)) eltp[which(duplicated(data.frame(rbind(do.call(cbind,el),pnt.lty[[i]])),fromLast=T))]<-names(pnt.lty[i])

	lay<-list()
	for(i in names(layout)) lay[[i]]<-layout[[i]](x=network.size(m1net))

	plot(m1net
			 ,attrname="ew"
			 ,loop.cex=loopcx
			 ,vertex.sides=rep(m1vsides,m1l)
			 ,vertex.col=vcol
			 ,edge.col=ecolp
			 ,vertex.cex=vcx
			 ,vertex.lwd=vlw
			 ,edge.lwd=elw
			 ,edge.lty=eltp
			 ,vertex.lty=vltp
			 ,layout.par=lay
	)
	box()
	mtext(m1main,cex=cex)

	vcol<-rep(m2col,m2l)
	el<-data.frame(as.edgelist(m2net))
	if(!is.directed(m2net)) el<-data.frame(t(apply(el,1,sort)),stringsAsFactors=F)
	ecolp<-rep(ecol,nrow(el))
	eltp<-rep(elt,nrow(el))
	vltp<-rep(vlt,network.size(m2net))

	if(length(pnt.v)) for(i in 1:length(pnt.v)) vcol[network.vertex.names(m2net)%in%pnt.v[[i]]]<-names(pnt.v[i])
	if(length(pnt.e)) for(i in 1:length(pnt.e)) ecolp[which(duplicated(rbind(do.call(cbind,el),pnt.e[[i]]),fromLast=T))]<-names(pnt.e[i])
	if(length(pnt.vlty)) for(i in 1:length(pnt.vlty)) vltp[network.vertex.names(m2net)%in%pnt.vlty[[i]]]<-names(pnt.vlty[i])
	if(length(pnt.lty)) for(i in 1:length(pnt.lty)) eltp[which(duplicated(data.frame(rbind(do.call(cbind,el),pnt.lty[[i]])),fromLast=T))]<-names(pnt.lty[i])

	lay<-list()
	for(i in names(layout)) lay[[i]]<-layout[[i]](x=network.size(m2net))

	plot(
		m2net
		,attrname="ew"
		,loop.cex=loopcx
		,vertex.sides=rep(m2vsides,m2l)
		,edge.col=ecolp
		,vertex.col=vcol
		,vertex.cex=vcx
		,vertex.lwd=vlw
		,edge.lwd=elw
		,edge.lty=eltp
		,vertex.lty=vltp
		,layout.par=lay
	)
	box()
	mtext(m2main,cex=cex)
	dev.off()
	cat("\nPlot saved to",out)
	ret<-list(bmnet=bmnet,m1net=m1net,m2net=m2net)
	if(trim) ret<-c(pbmnet=list(pbmnet),ret)
	ret
}

thatgirlis.f<-function(
	n
	,ew="ew"
	,s=1000
	,plot=F
)
{
	require(network)
	ns<-network.size(n)
	edist<-table(n%e%ew)
	z<-(ns*(ns-1)/2)-sum(edist)
	o<-names(edist)
	edist<-c(z,edist)
	names(edist)<-c("0",o)
	edist<-as.table(edist)
	print(cbind(Freq=edist,Prop=round(edist/sum(edist),digits=4)))
	cat("Tot:",sum(edist),"\n\n")

	perm<-list()
	maxcombo<-ns*(ns-1)/2
	combos<-1:maxcombo
	choices<-sum(n%e%ew)
	t1<-Sys.time()
	for(i in 1:s){
		cat("\r",i,"\t",sep="")
		rdist<-sample(combos,size=choices,replace=T)
		rdist<-table(rdist)
		z<-maxcombo-length(rdist)
		rdist<-table(rdist)
		o<-names(rdist)
		rdist<-c(z,rdist)
		names(rdist)<-c("0",o)
		rdist<-as.table(rdist)
		perm[[i]]<-rdist
	}
	t2<-Sys.time()
	cat(":",round((t2-t1)/60,2),"minutes to permute\n")
	maxcount<-max(sapply(perm,length))
	for(i in 1:length(perm)) perm[[i]]<-c(perm[[i]],rep(0,maxcount-length(perm[[i]])))
	perm<-do.call(rbind,perm)
	colnames(perm)<-0:(maxcount-1)
	if(dim(perm)[2]>4) {perm<-cbind(perm,apply(perm[,4:dim(perm)[2]],1,sum));colnames(perm)[dim(perm)[2]]<-">=3"}

	if(plot) for(i in which(edist>0)) {hist(perm[,i],breaks=(floor(min(perm[,i]))-.5):(ceiling(max(perm[,i]))+.5),freq=F,main=paste("Count:",i),xlab="",xlim=range(c(perm[,i],edist[i])));abline(v=edist[i],lty=2)}

	edist<-c(edist,rep(0,maxcount-length(edist)))
	names(edist)<-0:(maxcount-1)
	if(length(edist)>4) {edist<-c(edist,sum(edist[4:length(edist)]));names(edist)[length(edist)]<-">=3"}
	flush.console()
	edist<-cbind(observed=edist,expected=round(apply(perm,2,mean),1),sd=round(apply(perm,2,sd),3),t=round((edist-apply(perm,2,mean))/apply(perm,2,sd),3),"p o<=e"=round(apply(perm<=edist,2,mean),4),"p o>=e"=round(apply(perm>=edist,2,mean),4),t(round(apply(perm,2,quantile,prob=(c(seq(0,1,.1),.25,.75))),3)))
	edist
}

plotpois<-function(
	pois
	,year
	,jour
	,count
	,q1="0%"
	,q2="50%"
	,q3="100%"
)
{
	xlim<-range(year)
	plt<-do.call(c,pois[as.character(year),jour])
	plt<-plt[!is.na(plt)]
	for(i in 1:length(plt)) plt[[i]]<-cbind(year=as.numeric(names(plt[i])),plt[[i]])
	plt<-do.call(rbind,plt)
	plt<-plt[rownames(plt)==as.character(count),]

	ylim=range(plt[,c("observed",q1,q2,q3)])
	plot.new()
	plot.window(xlim=xlim,ylim=ylim)
	axis(side=1,lab=as.character(xlim[1]:xlim[2]),at=xlim[1]:xlim[2])
	axis(side=2)
	title(main=jour,xlab="Year",ylab=paste("Count of ",count,"'s",sep=""))
	w<-which(!!diff(plt[,"year"])-1)
	b<-1
	for(i in unique(c(w,dim(plt)[1]))){
		if(!length(w)) {r<-1:dim(plt)[1]} else {r<-b:i;b<-i+1}
		lines(plt[r,"year"],y=plt[r,q2],col="red",lty=1)
		lines(plt[r,"year"],plt[r,q1],lty=3,col="red")
		lines(plt[r,"year"],plt[r,q3],lty=3,col="red")
		lines(plt[r,"year"],plt[r,"observed"],lty=1,lwd=1)
		if(!length(w)) break
	}
}

subnet<-function(
	dbl2bel=stop("Supply original dbl2bel object",call.=F)
	,set=stop("set list(cr=incl cr,ut=incl ut,ncr=excl cr,nut=excl ut) and unused to NULL",call.=F)
	,source=stop("Supply source",call.=F)
)
{
	require(network)
	source(source)
	#if(!all(c("bel2mel.f","mel2net.f")%in%ls())) stop("Load correct source")
	#w<-!!sapply(lapply(dbl2bel$bel,"%in%",set),any)
	#if(!sum(w)) stop("Not a subset of either mode of this edgelist")
	#s<-set%in%dbl2bel$bel[,w]
	#if(!all(s)) {
	#	warning(paste(sum(!s),"or",round(sum(!s)/length(s)*100,1),"% of edges from subset are not in bel. First 10 excluded:"))
	#	print(head(set[s],10))
	#}
	sub<-rep(T,dim(dbl2bel$bel)[1])
	if(length(set$cr)) sub<-sub&dbl2bel$bel$cr%in%set$cr
	if(length(set$ut)) sub<-sub&dbl2bel$bel$ut%in%set$ut
	if(length(set$ncr)) sub<-sub&!dbl2bel$bel$cr%in%set$ncr
	if(length(set$nut)) sub<-sub&!dbl2bel$bel$ut%in%set$nut
	dbl2bel$bel<-dbl2bel$bel[sub,]
	if("pend"%in%names(dbl2bel)) dbl2bel$pend<-dbl2bel$pend[sub]
	cat("\n",nrow(dbl2bel$bel)," edges, ",length(unique(dbl2bel$bel$ut))," uts, and ",length(unique(dbl2bel$bel$cr)) ," crs fed to bel2mel.\n",sep="")
	bel2mel<-bel2mel.f(dbl2bel,out=getwd())
	mel2net<-mel2net.f(bel2mel)
	mel2net
}

dbl2w.f<-function(
	wok2dbl
	,out=stop("Specify output directory")
	,field=stop("field=c(\"field1\",\"field2\",...)")
	,variations=NULL
	,recode=NULL
)
{
	require(data.table)
	wok2dbl<-data.table(wok2dbl)
	setnames(wok2dbl,c("ut","field","b"))
	setkey(wok2dbl,field)
	wok2dbl<-wok2dbl[field]
	setkey(wok2dbl,field)

	field<-tolower(field)
	field<-field[field!="ut"]

	l<-list()
	for(i in field){
		l[[i]]<-wok2dbl[i=toupper(i)][j=list(ut,b)];setkey(l[[i]],ut);setnames(l[[i]],2,i)
		if(i%in%c("cr","af")) l[[i]][,i:=factor(toupper(sub(", DOI .+","",l[[i]][[i]]))),with=F]
		if(i=="af") l[[i]]<-l[[i]][i=!grepl("ANONYMOUS",l[[i]][[i]])]
		l[[i]][,c(i):=type.convert(as.character(l[[i]][[i]]))]
	}
	rm(wok2dbl)

	if(is.null(variations)) {
		cat("\nvariations=list(field1=dbl2bel_sets1,field2=dbl2bel_sets2,...)")
	}
	else
	{
		pre<-sort(unlist(c(0:9,strsplit("! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \\ ] ^ _ ` { | } ~"," "),letters,LETTERS)))[1]
		for(i in tolower(names(variations))) for(j in 1:length(variations[[i]])) levels(l[[i]][[i]])[levels(l[[i]][[i]])%in%variations[[i]][[j]]]<-paste(pre,"z",toupper(i),formatC(j,width=nchar(as.character(length(variations[[i]]))),format="d",flag="0"),sep="")
	}

	if(is.null(recode)) {
		cat("\nrecode=list(\n\tfield1=list(\n\t\t\"recode1\"=c(\"code1\",\"code2\",...)\n\t\t,\"recode2\"=c(\"code1\",\"code2\",...)\n\t)\n\t,field2=...\n)")
	}
	else
	{
		for(i in tolower(names(recode))) for(j in names(recode[[i]]))  levels(l[[i]][[i]])[levels(l[[i]][[i]])%in%recode[[i]][[j]]]<-j
	}

	if("cr"%in%field) {
		npen<-l$cr[,.N,by="cr"]
		npen<-as.character(npen$cr[npen$N>1])
		l$nr<-l$cr[,.N,by="ut"]
		setnames(l$nr,c("ut","nr"))
		setkey(l$cr,"cr")
		l$nrtp<-l$cr[npen,.N,keyby="ut"]
		setnames(l$nrtp,c("ut","nrtp"))
		setkey(l$cr,"ut")
	}

	if("so"%in%field) {
		### improve later for custom coding of natural text
		rc<-data.frame(c=c("soci","[ck]ono","anth","poli"),r=c("Sociology","Economics","Anthropology","Political Science"))
		l$ds<-copy(l$so)
		setnames(l$ds,2,"ds")
		for(i in 1:nrow(rc)) l$ds[grep(rc$c[i],l$so$so,ignore.case=T),ds:=rc$r[i]]
		l$ds$ds<-factor(l$ds$ds)
	}

	### merge
	dbl2w<-copy(l[[1]])
	for(i in names(l)[-1]) dbl2w<-merge(dbl2w,l[[i]],all=T,allow.cartesian=TRUE)
	rm(l)

	### code selection effect
	if("cr"%in%field) {
		dbl2w[is.na(dbl2w$nr),nr:=0]
		dbl2w[is.na(dbl2w$nrtp),nrtp:=0]
		dbl2w[,sel:=as.integer(nrtp>0)]
		dbl2w[,rej1:=as.integer(nr==0)] #document rejected if no citations
		dbl2w[,rej2:=as.integer(nrtp==0&nr!=0)] #document rejected if no citations after selection
	}

	w<-unique(c("ut",field[field%in%c("af","cr")]))
	lvs<-lapply(as.list(1:length(w)),FUN=function(x) apply(combn(w,x),2,paste,sep=""))
	lvs[[1]]<-matrix(lvs[[1]],ncol=length(w))
	for(i in 1:length(lvs)) for(j in 1:ncol(lvs[[i]])) {
		levs<-lvs[[i]][,j]
		setkeyv(dbl2w,levs)

		samp<-list()
		for(k in levs) samp[[k]]<-!is.na(dbl2w[[k]])
		cat("Proportion missing:\n")
		print(round(sapply(samp,FUN=function(x) sum(!x))/nrow(dbl2w),3))
		samp<-do.call(cbind,samp)
		if(ncol(samp)>1) for(i in 2:ncol(samp)) samp[,1]<-samp[,1]&samp[,i]
		samp<-as.vector(samp[,1])
		tp<-!!dbl2w$sel

		nm<-paste(c("w",levs),collapse="")

		dbl2w<-dbl2w[i=samp,1/.N,keyby=c(levs),][dbl2w]
		dbl2w[(!samp)|is.na(dbl2w$V1),V1:=0]
		setnames(dbl2w,"V1",nm)

		setkeyv(dbl2w,levs)
		dbl2w<-dbl2w[i=tp&samp,1/.N,keyby=c(levs)][dbl2w]
		dbl2w[(!tp)|is.na(dbl2w$V1),V1:=0]
		setnames(dbl2w,"V1",paste(nm,"tp",sep=""))
		cat(nm,"\n")
		rep<-rbind(c=c(nrow(dbl2w[i=samp,NA,keyby=levs]),nrow(dbl2w[i=(!!dbl2w$sel)&samp,NA,keyby=levs])),w=c(sum(dbl2w[[nm]]),sum(dbl2w[[paste(nm,"tp",sep="")]])))
		colnames(rep)<-c("unsel","sel")
		print(rep)
	}
	w<-unlist(sapply(lvs,FUN=function(x) apply(x,2,FUN=function(y) paste(c("w",y),sep="",collapse=""))))
	w<-c(w,paste(w,"tp",sep=""))
	dbl2w<-dbl2w[j=order(names(dbl2w)%in%w),with=F]
	if(!is.null(out)) save(dbl2w,file=paste(out,"dbl2w.RData",sep=.Platform$file.sep))
	setkeyv(dbl2w,levs)
	dbl2w
}

w2tab.f<-function(
	dbl2w
	,out=NULL
	,hg #higher (smaller) group classification
	,lg #lower (larger) group classification
	,sort #level by which all tables should be sorted in descending order
	,s.e=F
	,reps=10
	,decreasing=T
	,addlev=NULL
)
{
	require(data.table)
	w<-grep("w",names(dbl2w),value=T) #automatically processes any "weight" variables with a w- prefix
	nw<-nchar(w)
	lvar<-w[which.max(nw)]
	lvar<-gsub("(^w)|(tp$)","",lvar)
	nw<-nchar(lvar)
	lvar<-mapply(substr,lvar,seq(1,nw,2),seq(2,nw,2),USE.NAMES=F)
	lvar<-unique(c(lvar,addlev))
	dbl2w<-dbl2w[j=c(lvar,hg,lg,w),with=F]
	setkeyv(x=dbl2w,cols=lvar)

	t2prop<-function(x) if(!(is.character(x)|is.factor(x))) {prop.table(x)*100} else {x}

	tl.f<-function(dbl2w,wvar,levs,hg,lg,sort=c("key","order","given"),decreasing=T,subset=NULL){
		if(!is.null(subset)) {
			setkeyv(dbl2w,levs)
			dbl2w<-dbl2w[i=subset]
		}
		tl<-dbl2w[j=list(hc=sum(get(wvar))),keyby=hg]
		tl<-tl[dbl2w[j=list(lc=sum(get(wvar))),keyby=c(hg,lg)]]
		setkeyv(tl,c(hg,lg))
		if(sort[1]=="order") {o<-order(tl[[2]],tl[[4]],decreasing=decreasing);tl<-list(tl=tl,o=o)}
		if(sort[1]=="given") tl<-tl[i=sort,]
		tl
	}

	init<-tl.f(dbl2w,wvar=sort,sort="order",hg=hg,lg=lg)
	tl<-list(init$tl)
	names(tl)<-sort

	wo<-w[w!=sort]

	for(i in wo) tl[[i]]<-tl.f(dbl2w,wvar=i,hg=hg,lg=lg)
	tl<-tl[w]
	dup<-!duplicated(tl[[sort]][init$o,get(hg)])
	lh<-cumsum(dup)+1:nrow(tl[[sort]])
	h<-which(!(1:max(lh))%in%lh)
	hlo<-order(c(h,lh))
	ow<-grep("tp$",w,value=T,invert=T)
	if(s.e){
		se<-list()
		for(i in ow){
			wvar<-sub("tp$","",i)
			samplev<-unlist(sapply(as.list(addlev),FUN=function(x) sub(x,"",grep(x,wvar,value=T))))
			if(length(samplev)) {wvartp<-paste(samplev,"tp",sep="")} else {wvartp<-paste(wvar,"tp",sep="")}
			lvar<-gsub("(^w)|(tp$)","",wvartp) # level varnams cannot have w or tp in them, and must be exactly two characters, e.g. ut, af, cr
			levs<-mapply(substr,lvar,seq(1,nchar(lvar),2),seq(2,nchar(lvar),2),USE.NAMES=F)
			samp<-list()
			for(j in levs) samp[[j]]<-dbl2w[j=!is.na(get(j))]
			samp<-do.call(cbind,samp)
			if(ncol(samp)>1) for(j in 2:ncol(samp)) samp[,1]<-samp[,1]&samp[,j]
			samp<-as.vector(samp[,1])
			samp<-dbl2w[i=samp,NA,keyby=levs][,levs,with=F]
			cat(i,"\n")
			se[[i]]<-replicate(reps
												 ,tl.f(dbl2w,wvar=wvar
												 			,levs=levs
												 			,subset=samp[i=sample(1:nrow(samp),sum(dbl2w[[wvartp]]))]
												 			,hg=hg
												 			,lg=lg
												 )
												 ,simplify=F)

			J<-se[[i]][[1]][j=c(1,3,2,4),with=F][tl[[i]][j=c(1,3),with=F]]
			for(j in 2:length(se[[i]])) J[,paste(c("hc","lc"),j,sep=""):=as.list(se[[i]][[j]][j=c(1,3,2,4),with=F][tl[[i]][j=c(1,3,2,4),with=F]][,c("hc","lc"),with=F])]
			J[is.na(J)]<-0
			J<-J[init$o,]
			se[[i]]<-cbind(
				unlist(c(J[dup,j=hg,with=F],J[j=lg,with=F]))
				,rbindlist(list(
					J[i=dup,j=grep("hc",names(J),value=),with=F]
					,J[j=grep("lc",names(J),value=),with=F]))
			)[hlo]


			se[[i]]<-list(f=se[[i]],p=copy(se[[i]]))
			col<-colnames(se[[i]]$p)[-1]
			se[[i]]$p[h,col:=lapply(se[[i]]$p[h,col,with=F],t2prop),with=F]
			se[[i]]$p[!h,col:=lapply(se[[i]]$p[!h,col,with=F],t2prop),with=F]
			ssr<-function(x) sum(round(x,4)^2)
			sr<-function(x) sum(round(x,4))
			if(length(h)==nrow(se[[i]]$f)) {
				se[[i]]$p<-data.frame(level=c(as.character(se[[i]]$p[[1]]),"High Total","High H Index"),rbind(
					se[[i]]$p[h,!1,with=F]
					,se[[i]]$p[h,lapply(.SD, sr),.SDcols=-1]
					,se[[i]]$p[h,lapply(.SD, ssr),.SDcols=-1]
				))
				se[[i]]$f<-data.frame(level=c(as.character(se[[i]]$f[[1]]),"High Total"),rbind(
					se[[i]]$f[h,!1,with=F]
					,se[[i]]$f[h,lapply(.SD, sr),.SDcols=-1]
				))
			} else {
				se[[i]]$p<-data.frame(level=c(as.character(se[[i]]$p[[1]]),"High Total","Low Total","High H Index","Low H Index"),rbind(
					se[[i]]$p[,!1,with=F]
					,se[[i]]$p[h,lapply(.SD, sr),.SDcols=-1]
					,se[[i]]$p[!h,lapply(.SD, sr),.SDcols=-1]
					,se[[i]]$p[h,lapply(.SD, ssr),.SDcols=-1]
					,se[[i]]$p[!h,lapply(.SD, ssr),.SDcols=-1]
				))
				se[[i]]$f<-data.frame(level=c(as.character(se[[i]]$f[[1]]),"High Total","Low Total"),rbind(
					se[[i]]$f[,!1,with=F]
					,se[[i]]$f[h,lapply(.SD, sr),.SDcols=-1]
					,se[[i]]$f[!h,lapply(.SD, sr),.SDcols=-1]
				))
			}
			se[[i]]$sht.p<-try(do.call(rbind,apply(se[[i]]$p[,-1],1,FUN=function(x) shapiro.test(x)[1:2]))) #shapiro test of normality of p table
			for(j in c("f","p")) {se[[i]][[j]]<-data.frame(level=se[[i]][[j]][[1]],s.e.=apply(se[[i]][[j]][,-1],1,sd));se[[i]][[j]][[2]]<-round(se[[i]][[j]][[2]],4)}
		}
	}

	t2prop<-function(x) if(!(is.character(x)|is.factor(x))) {round(prop.table(x)*100,3)} else {x}
	for(j in names(tl)) {
		tl[[j]]<-tl[[j]][init$o]
		tl[[j]]<-cbind(
			unlist(c(tl[[j]][dup,j=hg,with=F],tl[[j]][j=lg,with=F]))
			,rbindlist(list(
				tl[[j]][i=dup,j=grep("hc",names(tl[[j]]),value=),with=F]
				,tl[[j]][j=grep("lc",names(tl[[j]]),value=),with=F]))
		)[hlo]

		tl[[j]]<-list(f=tl[[j]],p=copy(tl[[j]]))
		col<-colnames(tl[[j]]$p)[-1]
		tl[[j]]$p[h,col:=lapply(tl[[j]]$p[h,col,with=F],t2prop),with=F]
		tl[[j]]$p[!h,col:=lapply(tl[[j]]$p[!h,col,with=F],t2prop),with=F]

		ssr<-function(x) sum(round(x,3)^2)
		sr<-function(x) sum(round(x,3))
		if(length(h)==nrow(tl[[j]]$f)) {
			tl[[j]]$p<-data.frame(level=c(as.character(tl[[j]]$p[[1]]),"High Total","High H Index"),rbind(
				tl[[j]]$p[h,!1,with=F]
				,tl[[j]]$p[h,lapply(.SD, sr),.SDcols=-1]
				,tl[[j]]$p[h,lapply(.SD, ssr),.SDcols=-1]
			))
			tl[[j]]$f<-data.frame(level=c(as.character(tl[[j]]$f[[1]]),"High Total"),rbind(
				tl[[j]]$f[h,!1,with=F]
				,tl[[j]]$f[h,lapply(.SD, sr),.SDcols=-1]
			))
		} else {
			tl[[j]]$p<-data.frame(level=c(as.character(tl[[j]]$p[[1]]),"High Total","Low Total","High H Index","Low H Index"),rbind(
				tl[[j]]$p[,!1,with=F]
				,tl[[j]]$p[h,lapply(.SD, sr),.SDcols=-1]
				,tl[[j]]$p[!h,lapply(.SD, sr),.SDcols=-1]
				,tl[[j]]$p[h,lapply(.SD, ssr),.SDcols=-1]
				,tl[[j]]$p[!h,lapply(.SD, ssr),.SDcols=-1]
			))
			tl[[j]]$f<-data.frame(level=c(as.character(tl[[j]]$f[[1]]),"High Total","Low Total"),rbind(
				tl[[j]]$f[,!1,with=F]
				,tl[[j]]$f[h,lapply(.SD, sr),.SDcols=-1]
				,tl[[j]]$f[!h,lapply(.SD, sr),.SDcols=-1]
			))
		}

	}

	tab<-list()
	ow<-names(sort(dbl2w[,lapply(.SD,sum),.SDcols=ow]))
	for(j in ow){
		tab[[j]]<-data.frame(tl[[j]]$p,tl[[paste(j,"tp",sep="")]]$p)[,-3]
		tab[[j]]<-data.frame(tab[[j]],apply(tab[[j]][,-1],1,diff))
		if(s.e) tab[[j]]<-data.frame(tab[[j]],se[[j]]$p[[2]],tab[[j]][[4]]/se[[j]]$p[[2]])
		tab[[j]][,-1]<-round(tab[[j]][,-1],3)
		if(s.e) {colnames(tab[[j]])<-c("l","o","s","Δ","se","t")} else {colnames(tab[[j]])<-c("l","o","s","Δ")}
		wt<-grep("Total",tl[[j]]$f$level)
		tot<-tl[[paste(j,"tp",sep="")]]$f[wt,]
		tot[[1]]<-sub("Total","N",tot[[1]])
		if(s.e) {tot<-data.frame(tot[[1]],tl[[j]]$f[wt,2],tot[[2]],round((tot[[2]]-tl[[j]]$f[wt,2])/tl[[j]]$f[wt,2]*100,3),rep(NA,length(wt)),rep(NA,length(wt)))
		} else{tot<-data.frame(tot[[1]],tl[[j]]$f[wt,2],tot[[2]],round((tot[[2]]-tl[[j]]$f[wt,2])/tl[[j]]$f[wt,2]*100,3))}
		if(s.e) {colnames(tab[[j]])<-c("l","o","s","Δ","se","t")} else {colnames(tab[[j]])<-c("l","o","s","Δ")}
		if(s.e) {colnames(tot)<-c("l","o","s","Δ","se","t")} else {colnames(tot)<-c("l","o","s","Δ")}
		tab[[j]]<-rbind(tab[[j]],tot)
	}
	tab<-tab[ow]
	tab<-data.frame(tab[[1]],lapply(tab[-1],FUN=function(x) {x[[1]]<-NULL;x}))
	cn<-grep("\\.",names(tab),value=T,invert=T)
	colnames(tab)[colnames(tab)%in%cn]<-paste(ow[1],cn,sep=".")
	colnames(tab)<-sub("^w","",colnames(tab))
	colnames(tab)[1]<-"level"
	ro<-grepl("\\.o$",colnames(tab))
	tab<-data.frame(tab,replicate(sum(ro),rep(NA,nrow(tab)),simplify=F))[,order(c(1:ncol(tab),which(ro)-.5))]
	ro<-grepl("\\.o$",colnames(tab))
	colnames(tab)[which(ro)-1]<-""
	tab[grep("Total",tab[,1]),grep("(\\.se$)|(\\.t$)",colnames(tab))]<-NA
	if(!is.null(out)) write.table(tab,file=paste(out,"selection_table.tab",sep=""),sep="\t",quote=F,row.names=F,na="")
	w2tab
}

.ls.objects <- function (
	pos = 1
	, pattern
	, order.by
	, decreasing=FALSE
	, head=FALSE
	, n=5
)
{
	napply <- function(names, fn) sapply(names, function(x)
		fn(get(x, pos = pos)))
	names <- ls(pos = pos, pattern = pattern)
	obj.class <- napply(names, function(x) as.character(class(x))[1])
	obj.mode <- napply(names, mode)
	obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
	obj.prettysize <- napply(names, function(x) {
		capture.output(print(object.size(x), units = "auto")) })
	obj.size <- napply(names, object.size)
	obj.dim <- t(napply(names, function(x)
		as.numeric(dim(x))[1:2]))
	vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
	obj.dim[vec, 1] <- napply(names, length)[vec]
	out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
	names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
	if (!missing(order.by))
		out <- out[order(out[[order.by]], decreasing=decreasing), ]
	if (head)
		out <- head(out, n)
	out
}
lsos <- function(
	...
	, n=10
)
{
	.ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

if(F){
	library(linkcomm)
	load("bel2mel.RData")
	n<-names(bel2mel)
	n<-n[grep("el$",n)]
	mel2lc<-list()
	for(i in n){
		pdf(paste("mel2lc_",i,".pdf",sep=""))
		mel2lc[[i]]<-getLinkCommunities(bel2mel[[i]][,-(3:4)],removetrivial=F)
		dev.off()
		save(mel2lc,file="mel2lc_rt-f_uw.RData")
	}
}

rect.dendrogram<-function (tree, k = NULL, which = NULL, x = NULL, h = NULL, border = 2,
													 cluster = NULL, horiz = FALSE, density = NULL, angle = 45,
													 text = NULL, text_cex = 1, text_col = 1, xpd = TRUE, lower_rect, hpk=NULL,
													 ...
)
{
	if (!is.dendrogram(tree))
		stop("x is not a dendrogram object.")
	if (length(h) > 1L | length(k) > 1L)
		stop("'k' and 'h' must be a scalar(i.e.: of length 1)")
	if(is.null(hpk)) {tree_heights <- heights_per_k.dendrogram(tree)[-1]} else {tree_heights<-hpk[-1]}
	tree_order <- order.dendrogram(tree)
	if (!is.null(h)) {
		if (!is.null(k))
			stop("specify exactly one of 'k' and 'h'")
		ss_ks <- tree_heights < h
		k <- min(as.numeric(names(ss_ks))[ss_ks])
		k <- max(k, 2)
	}
	else if (is.null(k))
		stop("specify exactly one of 'k' and 'h'")
	if (k < 2 | k > length(tree_heights))
		stop(gettextf("k must be between 2 and %d", length(tree_heights)),
				 domain = NA)
	if (is.null(cluster))
		cluster <- cutree(tree, k = k)
	clustab <- table(cluster)[unique(cluster[tree_order])]
	m <- c(0, cumsum(clustab))
	if (!is.null(x)) {
		if (!is.null(which))
			stop("specify exactly one of 'which' and 'x'")
		which <- x
		for (n in seq_along(x)) which[n] <- max(which(m < x[n]))
	}
	else if (is.null(which))
		which <- 1L:k
	if (any(which > k))
		stop(gettextf("all elements of 'which' must be between 1 and %d",
									k), domain = NA)
	border <- rep_len(border, length(which))
	retval <- list()
	old_xpd <- par()["xpd"]
	par(xpd = xpd)
	for (n in seq_along(which)) {
		if (!horiz) {
			xleft = m[which[n]] + 0.66
			if (missing(lower_rect))
				lower_rect <- par("usr")[3L] - strheight("W") *
					(max(nchar(labels(tree))) + 1)
			ybottom = lower_rect
			xright = m[which[n] + 1] + 0.33
			ytop = tree_heights[names(tree_heights) == k]
		}
		else {
			ybottom = m[which[n]] + 0.66
			if (missing(lower_rect))
				lower_rect <- par("usr")[2L] + strwidth("X") *
					(max(nchar(labels(tree))) + 1)
			xright = lower_rect
			ytop = m[which[n] + 1] + 0.33
			xleft = tree_heights[names(tree_heights) == k]
		}
		rect(xleft, ybottom, xright, ytop, border = border[n],
				 density = density, angle = angle, ...)
		if (!is.null(text))
			text((m[which[n]] + m[which[n] + 1] + 1)/2, grconvertY(grconvertY(par("usr")[3L],
																																				"user", "ndc") + 0.02, "ndc", "user"), text[n],
					 cex = text_cex, col = text_col)
		retval[[n]] <- which(cluster == as.integer(names(clustab)[which[n]]))
	}
	par(xpd = old_xpd)
	invisible(retval)
}

strdist.dend.picker<-function(
	hd # must be dendrogram
	,s1=c(0,.075)
	,s2=c(0,1)
	,lightest.val=c(">"=.5,"="=.8)
	,maxlist=50
	,must.be.this.short.to.ride=.075
	,out=NULL
	,instruct=F
)
{

	require(dendextend)
	require(data.table)

	hs<-get_nodes_attr(hd,"height",simplify=T)
	l<-which(hs==0)
	n<-which(hs!=0)
	oc<-sapply(as.list(l),function(x) hs[n[which.max(which(n<x))]])
	names(oc)<-labels(hd)
	c<-lintran(oc,s1=s1,s2=s2)
	c[c>lightest.val[">"]]<-lightest.val["="]
	labels_colors(hd)<-gray(c)

	hk<-heights_per_k.dendrogram(hd)

	for(i in 1:length(hk)) if(max(table(cutree_1k.dendrogram(hd,k=as.integer(names(hk[i])),use_labels_not_values=FALSE,dend_heights_per_k=hk)))<=maxlist) break
	#if(i==1) maxlist<-as.integer(names(hk[i]))
	hdc<-cutree_1k.dendrogram(hd,k=as.integer(names(hk[i])),use_labels_not_values=T,dend_heights_per_k=hk)
	if(length(hdc)==2) hdc[2]<-1
	j<-i-1
	hkh<-hk[ifelse(j!=0,j,1)]

	df<-data.frame(get_nodes_xy(hd),get_nodes_attr(hd,"label"))
	names(df)<-c("x","y","lab")
	df<-df[!is.na(df$lab),]
	rownames(df)<-df$lab
	df[names(oc),"lh"]<-oc
	df[names(hdc),"hdc"]<-hdc
	rownames(df)<-NULL

	df<-data.table(df,key="lab")

	hdc<-split(hdc,hdc)
	hd<-color_branches(hd,k=length(hdc))
	lhdc<-length(hdc)

	graphics.off()
	sets<-list()
	if(instruct) cat("\rLeft-click to left of name to choose members of a set.\nWhen set is finished, right-click once to start a new set, or twice to advance the page.\nIf you want to accept the entire page, left-click five times, then right click twice.\nIf you make a mistake before starting a new set, select the same name three times, right-click to start a new set, and begin again.")
	c<-0

	for(i in 1:length(hdc)){
		sub<-names(hdc[[i]])
		if(min(df[sub,y])>must.be.this.short.to.ride) next
		yl<-range(df[sub,x],max(df[sub,x])-maxlist)
		#cx<-strheight(sub[1])*1.5*length(sub)/diff(yl)*10
		cx<-.7
		par(mai=c(.75,0,0,strwidth(sub[which.max(nchar(sub))],units="inches")*cx+1),cex=cx)
		plot(hd,"triangle",horiz=T,ylim=yl)
		te<-try(rect.dendrogram(hd,k=lhdc,which=lhdc-i+1,horiz=T,border="red",lty="dotted",hpk=hk),silent=T)
		if(!inherits(te,"try-error")) rect.dendrogram(hd,k=lhdc,which=lhdc-i+1,horiz=T,border="red",lty="dotted",hpk=hk)
		loc<-NA
		e<-1
		while(!is.null(loc)) {
			ind<-length(sets)+1
			nind<-rev(unlist(strsplit(as.character(ind),"")))[1]
			loc<-locator(type="p",pch=nind,col="red",cex=.5)$y
			if(is.null(loc)) break
			sets[[ind]]<-labels(hd)[round(loc)]
			if(any(table(sets[[ind]])%in%3:4)) sets[[ind]]<-sub
			segments(x0=0,y0=which(labels(hd)%in%sets[[ind]])
							 ,x1=0 #max(strwidth(sets[[ind]]))
							 ,lwd=10,col=hsv(.1*e,1,1,.5))
			e<-e+1
		}
		if(!is.null(out)) {
			c<-c+1
			dev.print(pdf,paste(out,c,file="picker_log.pdf",sep=""))
		}
	}
	sets[sapply(sets,function(x) any(table(x)>=5))]<-NULL
	sets<-lapply(sets,unique)
	sets[sapply(sets,function(x) any(sapply(setdiff(sets,list(x)),function(y) all(x%in%y))))]<-NULL
	if(!is.null(out)) save(sets,file=paste(out,file="picker_sets.RData",sep=""))
	sets
}

write.ergmm<-function(
	where=stop("\"where\" = folder filepath for scripts",call.=F)
	,dat=stop("\"dat\" = name of .RData file containing (stat)nets, w. ext",call.=F)
	,net=stop("\"net\" = named list of (stat)nets",call.=F)
	,dimensions=2
	,groups=1
	,verbosity=2
	,mcmc.size=10000
	,mcmc.burn=30000
	,mcmc.inter=10
)
{
	mod<-paste("d",dimensions,"G",groups,sep="")
	writeLines(c(
		"getwd()"
		,"rm(list=ls())"
		,"library(statnet)"
		,"library(latentnet)"
		,paste("dat<-\"",dat,"\"",sep="")
		,paste("load(dat)",sep="")
		,"hoff2fit<-list()"
		,"time<-round(unclass(Sys.time()))"
		,paste("for(i in rev(names(",net,"))){",sep="")
		,"cat(\"\\n<<<<<<<<< \",i,\" >>>>>>>>>\\n\\n\",sep=\"\")"
		,paste("hoff2fit[[paste(i,\"",mod,"\",sep=\"\")]]<-try(ergmm(",net,"[[i]]~euclidean(d=",dimensions,",G=",groups,"),verbose=",verbosity,",control=control.ergmm(\nsample.size=",mcmc.size,"\n,burnin=",mcmc.burn,"\n,interval=",mcmc.inter,"\n)))",sep="")
		,paste("save(hoff2fit,file=paste(\"hoff2fit_\",sub(\".RData\",\"\",dat),\"_\",\"",mod,"\",\"_\",time,\".RData\",sep=\"\"))",sep="")
		,"}"
	),con=paste(where,.Platform$file.sep,"write.ergmm_",sub(".RData","",dat),"_",mod,".R",sep=""))
}

cull.f<-function(
	a
	,b,cull=.2
	,noanon=F
)
{
	if(any(is.na(b))) {b<-na.omit(b);attributes(b)<-NULL}
	if(noanon) {
		if(grepl("\\[ANONYMOUS\\],? ?",a)) stop("k = \"",a,"\"\n",call.=F)
		b<-sub("\\[ANONYMOUS\\],? ?","",b)
	}
	b<-b[nchar(b)>13]
	if(!is.null(cull)) b<-b[stringdist(a=a,b=b,method="jw",p=.1)<cull]
	b
}

unicr<-function(
	index
)
{
	yu<-list()
	cbeg<-list()
	cend<-list()
	cat("    ")
	for(i in 1:dim(index)[1]){
		d<-dimnames(index)[[1]][i]
		cat("\b\b\b\b",d)
		flush.console()
		yu[[d]]<-length(levels(droplevels(comdb[J(unlist(index[i,]),"CR")]$b)))
		cbeg[[d]]<-length(levels(droplevels(comdb[J(unlist(index[1:i,]),"CR")]$b)))
		cend[[d]]<-length(levels(droplevels(comdb[J(unlist(index[i:dim(index)[1],]),"CR")]$b)))
	}
	u<-data.frame(yu=unlist(yu),cbeg=unlist(cbeg),cend=unlist(cend))
	u
}

poisson_permute<-function()
{
	#permute the feckin distribution
	perm<-list()
	maxcombo<-1216*1215/2
	combos<-1:maxcombo
	choices<-sum(lc2mel$ew)
	t1<-Sys.time()
	for(i in 1:1000){
		cat("\r",i,sep="")
		rdist<-sample(combos,size=choices,replace=T)
		rdist<-table(rdist)
		z<-maxcombo-length(rdist)
		rdist<-table(rdist)
		n<-names(rdist)
		rdist<-c(z,rdist)
		names(rdist)<-c("0",n)
		rdist<-as.table(rdist)
		perm[[i]]<-rdist
	}
	t2<-Sys.time()
	cat(t2-t1,"minutes")

	cn<-sort(unique(unlist(lapply(perm,names))))
	permdb<-data.frame(matrix(0,nrow=length(perm),ncol=length(cn)))
	permdb<-data.frame(permdb,matrix(0,nrow=length(perm),ncol=length(edist)-length(cn)))
	names(permdb)<-names(edist)
	for(i in 1:length(perm)) permdb[i,names(perm[[i]])]<-perm[[i]]

	dif<-matrix(edist,nrow=dim(permdb)[1],ncol=dim(permdb)[2],byrow=T)-permdb
	md<-apply(dif,2,mean)
	sdd<-apply(dif,2,sd)
	cid<-apply(dif,2,quantile,prob=c(.05,.95))
	tad<-apply(dif,2,table)
	num<-lapply(tad,names)
	num<-lapply(num,as.numeric)

	dens<-1-(permdb$`0`/maxcombo)
	mean((1-edist["0"]/maxcombo)-dens)*100
	sd((1-edist["0"]/maxcombo)-dens)*100
	mean((1-edist["0"]/maxcombo)-dens)/sd((1-edist["0"]/maxcombo)-dens)

	apply(t(apply(permdb,1,"*",as.numeric(colnames(permdb)))),sum)/maxcombo

	round(cbind(md,sdd,t=md/sdd,p=0,t(cid)),digits=3)

	round(cbind(md=md/maxcombo,sdd=sdd/maxcombo,t=md/sdd,p=0),digits=4)[1:7,]
	sum(edist[-(1:7)])/maxcombo

	plot(as.table((md/sdd)[1:7]),type="l",ylim=range((md/sdd)[1:7]),lwd=3)
	abline(h=0,lty="dotted",lwd=3)

	round(cbind(md,sdd,t=md/sdd,p=0),digits=1)[1:7,]
	sum(edist[-(1:7)])

	mp<-apply(dif/maxcombo,2,mean)
	sdp<-apply(dif/maxcombo,2,sd)
	cip<-apply(dif/maxcombo,2,quantile,prob=c(.05,.95))

	round(cbind(mp,sdp,p=0,t(cip)),digits=5)[1:7,]
	sum(edist[-(1:7)])

	mvd<-mean((sum(((as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2)*edist)/maxcombo)-(apply(apply(permdb,1,"*",(as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2),2,sum)/maxcombo))
	sddv<-sd((sum(((as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2)*edist)/maxcombo)-(apply(apply(permdb,1,"*",(as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2),2,sum)/maxcombo))
	cidv<-quantile((sum(((as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2)*edist)/maxcombo)-(apply(apply(permdb,1,"*",(as.numeric(names(edist))-sum(lc2mel$ew)/maxcombo)^2),2,sum)/maxcombo),prob=c(.05,.95))
	round(c(mvd=mvd,sddv=sddv,tvd=mvd/sddv,p=0,cidv=cidv),digits=4)

	m<-apply(permdb,2,mean)
	sd<-apply(permdb,2,sd)
	ci<-apply(permdb,2,quantile,prob=c(.05,.95))
	cbind(m,sd,ci)

	zscores<-(edist[1:7]-apply(permdb,2,mean))/apply(permdb,2,sd)


	apply(matrix(edist,nrow=dim(permdb)[1],ncol=dim(permdb)[2],byrow=T)-permdb,2,mean)
	apply(matrix(edist,nrow=dim(permdb)[1],ncol=dim(permdb)[2],byrow=T)-permdb,2,sd)
}

extract.components<-function(
	graph
	,quantile=0
)
{
	components<-component.dist(graph)
	subgraphs<-list()
	j<-1
	for(i in order(components$csize[which(components$csize>=quantile(components$csize, c(quantile)))],decreasing=T)){
		subgraphs[[j]]<-graph%s%which(components$membership==i)
		j<-j+1
	}
	cat("\nComponent size distribution:")
	print(table(components$csize))
	return(subgraphs)
}

igraph2statnet<-function(
	net
)
{
	require(igraph)
	require(network)
	require(data.table)

	if(class(net)=='igraph'){
		el<-data.table(get.edgelist(net))
		dir<-!igraph::is.directed(net)
		if(dir){
			w<-el[,V1>V2]
			r<-el[w,V1]
			el[w,V1:=V2]
			el[w,V2:=r]
		}
		setkey(el,V1,V2)
		bf<-nrow(el)
		el<-unique(el)
		cat('\n',bf-nrow(el),'duplicate edges deleted\n')
		el<-as.matrix(el)
		net<-network::network(el,matrix.type="edgelist",directed=ifelse(dir,T,F))
	}

	if(class(net)=='network'){

	}
	net
}

#' google books ngram 2 token time series
#'
#' @param out hot stuff
#' @param cfso 
#' @param query A character vector of terms or a long format database where the last column is the term and leading columns are factors. Factors will be returned on the result.
#' @param ys 
#' @param slug 
#' @param sm 
#' @param ye 
#'
#' @return
#' @export
#'
#' @examples
gbng2tts.f<-function(
  out='~/mat/dp'
  ,ys=1901
  ,ye=2000
  ,cfso=T
  ,slug='auto'
  ,query=c('social','cultural')
  ,sm=0
){
  require(magrittr)
  require(data.table)
  require(ngramr)
  query<-data.table(query)
  pfs<-.Platform$file.sep
  n<-ncol(query) 
  if(slug=='auto') {
    if(n==1) {
      slug<-query[,n,with=F] %>% gsub(pattern='[^A-Za-z ]',replacement='') %>% strsplit(split=' +')
    } else {
      slug<-query[,!n,with=F]
    }
    slug<-unlist(slug) %>% substr(0,3) %>% unique() %>% paste0(collapse='-')
  }
  fso<-sub(paste0(pfs,'+'),pfs,paste(out,pfs,'gbng',ys,ye,'-',slug,'.RData',sep=''))
  if(cfso) {
    if(!file.exists(fso)) {cat('\nNo saved ngramr output.');return(NULL)}
    ng<-paste('~',dir('~',pattern='gbng',recursive = T),sep=pfs)
    cat('Loading \"',fso,'\"\n',sep='')
    load(fso) 
  } else {
    gnq<-strsplit(query,',')
    ng<-list()
    for(i in 1:length(gnq)) ng[[i]]<-data.table(ngram(phrases=gnq[[i]],corpus = 'eng_us_2012',year_start=ys,year_end=ye,smoothing=sm,count=T),batch=i)
    ng<-rbindlist(ng)
    if(!is.null(names(query))) ng[,batch:=factor(batch,labels = names(query))]
    setattr(ng,'date.queried',Sys.Date())
    setkey(ng,batch,Phrase,Year)
		cat('%Saving \"',fso,'\"\n',sep='')
		save(ng,file=fso)
	}
	ng
}
tscore<-function(x) (x-mean(x))/sd(x)

tts2arima.f<-function(
  gbng2tts
  ,by=c('batch','Phrase')
)
{
  require(forecast)
  tts2arima<-gbng2tts[,list(aa=list(
    auto.arima(
      ts(Frequency,start = min(Year),frequency = 1)
      ,lambda = BoxCox.lambda(Frequency)
      ,stepwise = F
      ,seasonal = F 
      ,trace=F)
    )),keyby=by]
  tts2arima[,Predicted:=list(lapply(aa,fitted))]
  tts2arima[,`Predicted t-Score`:=list(lapply(fit,tscore))]
  tts2arima
}

arima2plot<-function(tts2arima){
  require(data.table)
  require(ggplot2)
  tts2arima[,]
}

if(F){
  
  for(i in cat) ngp[[i]]<-qplot(Year,nminmaxF,data=gng[expand.grid(1:0,i)],geom='line',color=cat,size=I(1)) + #size was 1.2
      scale_x_continuous(breaks=seq(1870,2010,10)) + scale_y_continuous(name=ytit) +
      scale_colour_grey() + theme_bw() + theme(axis.text.x = element_text(angle = 90),legend.position="bottom")
  
}

tts<-function(
  c='A. Generic'
  ,s='soci'
  ,o=c('Frequency','Year')
  ,n=c('f','y')
  ,tts=get('gbng2tts')$ts
){
  require(data.table)
  setkey(tts,stem,cat,Year)
  tts<-copy(tts[list(s,c),rev(o),with=F])
  setkey(tts,Year)
  setnames(tts,o,n)
  tts  
}

if(F){
  library(MASS)
  out <- boxcox(lm(tts()~1))
  range(out$x[out$y > max(out$y)-qchisq(0.95,1)/2])
}