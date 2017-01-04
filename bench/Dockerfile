FROM rocker/verse:3.3.2
MAINTAINER "Brooks Ambrose" brooksambrose@berkeley.edu

## Install rJava from apt-get
#RUN apt-get install -y r-cran-rjava

## Install Java and tell R where it is
RUN apt-get update && apt-get install -y --no-install-recommends \
	openjdk-7-jre \
&& R CMD javareconf


## Install system utilities
RUN apt-get update \
&& apt-get install -y --no-install-recommends \
#	libbz2-dev \
#	liblzma-dev \
#	libpcre3-dev \
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

## Install scholdoc https://github.com/timtylin/scholdoc/blob/master/INSTALL.md
RUN apt-get install -y --no-install-recommends \
	ghc \
	cabal-install \
&& cabal update \
&& cabal install scholdoc

## R packages: utilities
RUN export `cat etc/environment | grep MRAN` \
&& install2.r --repos $MRAN --deps TRUE \
	bit64 \
	colorspace \
	data.table \
	doParallel \
	fastcluster \
	foreach \
	httr \
	lineprof \
	magrittr \
	RCurl \
	SnowballC \
&& r -e 'warnings()'

## R packages: networks
RUN export `cat etc/environment | grep MRAN` \
&& install2.r --repos $MRAN --deps TRUE \
	bipartite \
	d3Network \
	igraph \
	latentnet \
	linkcomm \
	statnet \
&& r -e 'warnings()'

## R packages: text analysis
RUN export `cat etc/environment | grep MRAN` \
&& install2.r --repos $MRAN --deps TRUE \
	LDAvis \
	mallet \
	openNLP \
	qdap \
	RTextTools \
	stm \
	stmBrowser \
	stmCorrViz \
	stringdist \
&& Rscript -e 'devtools::install_github("benmarwick/JSTORr")' \
&& r -e 'warnings()'

## R packages: time series
RUN export `cat etc/environment | grep MRAN` \
&& install2.r --repos $MRAN --deps TRUE \
	lmtest \
	forecast \
&& r -e 'warnings()'

## R packages: other stats
RUN export `cat etc/environment | grep MRAN` \
&& install2.r --repos $MRAN --deps TRUE \
	alluvial \
	cluster \
	dendextend \
	entropy \
	fitdistrplus \
	ineq \
	lattice \
	logspline \
	matrixStats \
	mclust \
	pvclust \
	RQDA \
	SuperLearner \
	zoo \
&& r -e 'warnings()'

## R packages: graphics and typesetting
RUN export `cat etc/environment | grep MRAN` \
&& install2.r --repos $MRAN --deps TRUE \
	ggbiplot \
	rgl \
	scatterplot3d \
	stargazer \
	venneuler \
	wordcloud \
	formatR \
&& r -e 'warnings()'

## R packages: APIs
RUN export `cat etc/environment | grep MRAN` \
&& install2.r --repos $MRAN --deps TRUE \
	RSQLite \
&& Rscript -e 'devtools::install_github("seancarmody/ngramr")' \
&& Rscript -e 'devtools::install_github("1beb/RGoogleDrive")' \
&& r -e 'warnings()'

## R packages: uncategorized
RUN export `cat etc/environment | grep MRAN` \
&& install2.r --repos $MRAN --deps TRUE \
	DBI \
	fpc \
	gdata \
	lsa \
	qtl \
	rdd \
	rvest \
	rzmq \
	scales \
	strucchange \
&& r -e 'warnings()'

## R packages: hastily added

## ~fin~ to use run in -d detached mode and open rstudio server at localhost:8080
CMD ["\init"]

