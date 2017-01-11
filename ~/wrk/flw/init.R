#Clean workspace
cat('\014')
rm(list=setdiff(ls(), 'ki')) # problematic but don't kill the index

# libraries
library(data.table)
library(knitr)
library(ggplot2)
library(stargazer)
library(magrittr)
library(tools)

# knitr package options
opts_knit$set(
  aliases=c(eval='e',include='i') # can rename chunk options
  ,concordance = T
  ,root.dir = '~'
  ,self.contained = F
  ,verbose=F
)

# knitr default chunk options
knitr::opts_chunk$set(
	fig.width=4.32 # <26 picas as required by AJS
	,fig.height=2.5
	,fig.align='center'
	,fig.path='~/prd/tex/fig'
	,echo=FALSE
	,eval=FALSE
	,warning=FALSE
	,message=FALSE
	,include=FALSE
	,error=FALSE
	,highlight=FALSE
	,tidy=TRUE
	,comment=NA
	,results='asis'
	#,out.extra = 'style="display:block; margin:auto;"' # to center figures https://github.com/yihui/knitr/issues/387
)

# R options
options(max.print=200,warn=1)

# code sources
source('~/tls/src.R')
source('~/wrk/flw/glossary.R')

# data sources / sinks
# mat<-list()
# mat$inn$wok0041<-'../mat/inwok/1900-1941'
# out$wok0041<-'../knowledge-survival/outwok0041'
# out$wok0099<-'../knowledge-survival/outwok0099'
# mat$jstor0041<-'../mat/jstor0041top5'
# out$jstor0041<-'../knowledge-survival/outjstor0041top5'

#initialize tables
t<-typ()
