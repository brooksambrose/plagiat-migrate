FROM rocker/verse:3.3.2
MAINTAINER "Brooks Ambrose" brooksambrose@berkeley.edu

## Install utilities
RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		git \
		subversion \
		software-properties-common

## Install igraph
RUN apt-get install -y --no-install-recommends \
		libigraph0 \
		libigraph0-dev

## Install cosparallel
RUN (cd ~/ \
	&& svn checkout svn://svn.code.sf.net/p/cosparallel/code/trunk cosparallel-code \
	&& cd cosparallel-code \
	&& ./configure \
	&& make \
	&& cd extras \
	&& make)

## Install scholdoc http://scholdoc.scholarlymarkdown.com/
RUN add-apt-repository ppa:hvr/ghc \
	&& apt-get update \ 
	&& apt-get install -y --no-install-recommends \
		ghc-7.8.3 \
		cabal-install-1.20 \
	&& cabal update \
	&& cabal install scholdoc \
	&& cabal install scholdoc-citeproc

## Install R packages
RUN install2.r --error \
    devtools \
    dplyr \
    ggplot2 \
    ggthemes \
    knitr \
    reshape2 \
    rmarkdown \
    rmdformats \
    rstudioapi \
    servr \
    shiny \
	alluvial \
	bit64 \
	cluster \
	colorspace \
	d3Network \
	data.table \
	DBI \
	dendextend \
	doParallel \
	entropy \
	fastcluster \
	fitdistrplus \
	foreach \
	fpc \
	gdata \
	ggbiplot \
	httr \
	igraph \
	ineq \
	latentnet \
	lattice \
	LDAvis \
	lineprof \
	linkcomm \
	logspline \
	lsa \
	magrittr \
	mallet \
	matrixStats \
	mclust \
	network \
	ngramr \
	NLP \
	openNLP \
	parallel \
	pvclust\
	qdap \
	qdapDictionaries \
	qtl \
	RCurl \
	rdd \
	rgl \
	RGoogleDocs \
	RGoogleDrive \
	RQDA \
	RSQLite \
	RTextTools \
	rvest \
	rzmq \
	scales \
	scatterplot3d \
	SnowballC \
	stargazer \
	statnet \
	stats \
	stm \
	stmBrowser \
	stmCorrViz \
	stringdist \
	strucchange \
	SuperLearner \
	tm \
	tools \
	venneuler \
	wordcloud \
	XML \
	zoo

## clone git repos
RUN git clone https://github.com/brooksambrose/clique-percolation

## test
CMD ["cd ~/clique-percolation && time bash -v test.sh"]

