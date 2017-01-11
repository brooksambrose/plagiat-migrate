# text formatting functions
nicen<-function(x,ns=3) format(x,big.mark=',',big.interval=3,nsmall=ns)

bold<-function(x) paste(' \\textbf{',x,'}',sep='')
ital<-function(x) paste(' \\textit{',x,'}',sep='')
undr<-function(x) paste(' \\underline{',x,'}',sep='')

sg<-function(x,lab,tit,sum=F,col.align=c(old='',new=''),rplc=c(old='^X\\. ',new=''),...) {
	require(stargazer)
	require(magrittr)
	if(!sum) x<-data.frame(lapply(x,gsub,pattern='&',replacement='\\&',fixed=T))
	x<-capture.output(stargazer(x,title = tit,label=lab,style='ajs',summary=sum,header=F,rownames=F,...)) %>%
		gsub(pattern='\\textbackslash ',replacement='\\',fixed=T) %>%
		gsub(pattern='\\}',replacement='}',fixed=T) %>%
		gsub(pattern='\\{',replacement='{',fixed=T) %>%
		sub(pattern=col.align[1],replacement=col.align[2]) %>%
	  gsub(pattern=rplc[1],replacement=rplc[2])
	cat(x,sep='\n')
}

#WOK Field Tags, https://images.webofknowledge.com/WOKRS58B4/help/WOK/hs_wos_fieldtags.html
wokfld<-data.table(read.table('~/mat/dq/wokfields.txt',sep = '\t',header = TRUE),key='field')

# While silly, we'll use chemistry's particles, elements, compounds, and mixtures

# interchangeable names for ontological element as event
one<-function(sp=1,n=NULL){
	r<-rbind(
		act=c(s='action',p='actions')
		,beh=c('behavior','behaviors')
		,per=c('performance','performances')
	)
	if(is.null(n)) n<-sample(rownames(r),1)
	r[n,sp]
}

# interchangeable names for ontological element as person
ones<-function(sp=1,n=NULL){
	r<-rbind(
		act=c(s='actor',p='actors')
		,beh=c('behavior','behaviors')
		,per=c('performer','performers')
		,sub=c('subject','subjects')
	)
	if(is.null(n)) n<-sample(rownames(r),1)
	r[n,sp]
}

# interchangeable names for ontological molecule
onm<-function(sp=1,n=NULL){
	r<-rbind(
		sf=c(s='subfield',p='subfields')
		,sd=c('subdiscipline','subdisciplines')
		,ic=c('invisible college','invisible colleges')
		,fp=c('field position','field positions')
		,sp=c('specialty','specialties')
		,fi=c('field of inquiry','fields of inquiry')
		,sc=c('subculture','subcultures')
		,mi=c('milieu','milieus')
	)
	if(is.null(n)) n<-sample(rownames(r),1)
	r[n,sp]
}

# interchangeable names for operational molecule
opm<-function(sp=1,n=NULL){
	r<-rbind(
	kcc=c(s='k-clique community',p='k-clique communities')
	,kca=c('kcc','kccs')
	)
	if(is.null(n)) n<-sample(rownames(r),1)
	r[n,sp]
}

# typology of levels
typ<-function(){
	hier<-rev(c('sub','par','super'))
	d<-lapply(list(
		relevance=list(closure=c('include','tend','exclude'),expectation=c('irregular','regular'))
		,relation=list(cast=hier,catch=hier)
		,meaning=list(affect=c('attracted','neutral','repulsed'),codification=c('explicit','implicit','cryptic'))
	),function(x) array(rep('',prod(sapply(x,length))),dim=sapply(x,length),dimnames=x))
	d$relevance['exclude','regular']<-'abject'
	d$relevance['exclude','irregular']<-'reject'
	d$relevance['tend',]<-'object'
	d$relevance['include',]<-'retroject'

	d$relation[throw='super',catch='sub']<-'subject' # stable
	d$relation[throw='par',catch='par']<-'citizen' # stable
	d$relation[throw='sub',catch='super']<-'subject' # stable

	d$relation[throw='super',catch='super']<-'contest' # strained
	d$relation[throw='super',catch='par']<-'challenge' # strained
	d$relation[throw='par',catch='super']<-'challenge' # strained
	d$relation[throw='par',catch='sub']<-''  # strained
	d$relation[throw='sub',catch='par']<-'' # strained
	d$relation[throw='sub',catch='sub']<-'genteel' # strained

	d$process<-data.frame(matrix(letters[1:4],nrow=2,ncol=2,dimnames=list(Scale=c('Micro','Macro'),Mechanism=c('Cultural','Social'))))

	d$stages<-data.frame(
		Stage=c('Prototyping','Assemblage','Facilitation','Accumulation')
		,Sensemaking=c('Obsession','Recognition','Mastery','Competition')
		,Control=c('Private','Peer','Provider','Professional')
	)

		#should be an "editing" nuance where throw is negotiated, a catch is refused while throw is appealed
	d
}

cat(
  '\nUseful links:'
  ,'http://localhost:8080/help/library/rmarkdown/html/render.html'
  ,'http://localhost:8080/help/library/knitr/html/knit.html'
  ,'http://pandoc.org/MANUAL.html#variables-for-latex'
  ,'http://stackoverflow.com/questions/31744576/combine-rmarkdown-generated-latex-documents-in-rstudio-without-having-to-manuall'
  ,'http://pandoc.org/MANUAL.html#templates'
  ,'https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf'
  ,'http://yihui.name/knitr/demo/minimal/'
  ,'http://www.matthewjockers.net/2013/09/03/tawr/'
  ,'http://rmarkdown.rstudio.com/developer_custom_formats.html'
  ,'https://www.bioconductor.org/help/course-materials/2015/CSAMA2015/lab/rr-authoring.html'
  ,'https://github.com/asardaes/R-LaTeX-Template'
  ,'http://pandoc.org/demo/example19/Extension-citations.html'
  ,'http://mirror.hmc.edu/ctan/macros/latex/contrib/docmute/docmute.pdf'
  ,'https://www.r-bloggers.com/chicken-or-the-egg-granger-causality-for-the-masses/'
  ,'http://www.kdnuggets.com/2015/02/avoiding-common-mistake-time-series.html'
  ,'http://www.tylervigen.com/spurious-correlations'
  ,'https://www.otexts.org/fpp/8/1'
  ,'https://people.duke.edu/~rnau/411arim.htm'
  ,'http://stackoverflow.com/questions/26617587/finding-optimal-lambda-for-box-cox-transform-in-r'
  ,'https://www.codecogs.com/latex/eqneditor.php'
  ,'https://onlinecourses.science.psu.edu/stat510/node/53'
  ,'http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html'
  ,'http://stackoverflow.com/questions/32257970/knitr-inherits-variables-from-a-users-environment-even-with-envir-new-env'
  ,'http://tex.stackexchange.com/questions/21838/replace-inputfilex-by-the-content-of-filex-automatically'
  ,sep='\n'
)