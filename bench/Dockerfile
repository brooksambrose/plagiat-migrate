FROM rocker/verse:3.3.3
MAINTAINER "Brooks Ambrose" brooksambrose@berkeley.edu

### Heavier stuff ###

## Install that thing Aaron told me to that is big and has python
RUN apt-get update && apt-get install -y --no-install-recommends \
	software-properties-common

## Install igraph
RUN apt-get update && apt-get install -y --no-install-recommends \
	libigraph0 \
	libigraph0-dev

## Install Java and libraries to build rJava
RUN apt-get update && apt-get install -y --no-install-recommends \
	liblzma-dev \
	libbz2-dev \
	libicu-dev \
	default-jdk

### R packages ###

## R package prereqs
RUN apt-get update && apt-get install -y --no-install-recommends \
# topicmodels
	libgsl0-dev \
# rgl
	libgl1-mesa-glx \
	libglu1-mesa-dev \
# cairoDevice
	libcairo2 \
# RGtk2
	libgtk2.0-dev \
	libatk1.0-0 \
# rzmq
	libzmq3 \
	libzmq3-dev

## Install cairoDevice & RQDA through X virtual frame buffer to allow gui test loading of cairoDevice to pass
RUN apt-get update && apt-get install -y --no-install-recommends \
	xauth \
	xvfb \
&& . etc/environment \
&& xvfb-run install2.r --repos $MRAN --deps TRUE \
	cairoDevice \
	RQDA \
&& r -e 'warnings()'
	
## Tell R where Java is, then test install rJava
RUN R CMD javareconf \
&& . etc/environment \
&& install2.r --repos $MRAN --deps TRUE \
	rJava \
&& r -e 'warnings()'

## R packages: utilities
RUN . etc/environment \
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
RUN . etc/environment \
&& install2.r --repos $MRAN --deps TRUE \
	bipartite \
	d3Network \
	igraph \
	latentnet \
	linkcomm \
	statnet \
&& r -e 'warnings()'

## R packages: text analysis
RUN . etc/environment \
&& install2.r --repos $MRAN --deps TRUE \
	LDAvis \
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
RUN . etc/environment \
&& install2.r --repos $MRAN --deps TRUE \
	lmtest \
	forecast \
	vars \
&& r -e 'warnings()'

## R packages: other stats
RUN . etc/environment \
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
	SuperLearner \
	zoo \
&& r -e 'warnings()'

## R packages: graphics and typesetting
RUN . etc/environment \
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
RUN . etc/environment \
&& install2.r --repos $MRAN --deps TRUE \
	RSQLite \
&& Rscript -e 'devtools::install_github("seancarmody/ngramr")' \
&& Rscript -e 'devtools::install_github("1beb/RGoogleDrive")' \
&& Rscript -e 'devtools::install_github("mikeasilva/blsAPI")' \
&& r -e 'warnings()'

## R packages: uncategorized
RUN . etc/environment \
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


### Small useful stuff ###

## Install cosparallel
RUN apt-get update \
&& apt-get install -y --no-install-recommends \
	subversion \
&& (cd ~/ \
	&& svn checkout svn://svn.code.sf.net/p/cosparallel/code/trunk cosparallel-code \
	&& cd cosparallel-code \
	&& ./configure \
	&& make \
	&& cd extras \
	&& make)

## Install latexpand
#RUN apt-get update \
#&& apt-get install -y --no-install-recommends \
#	perl-doc
#RUN (cd ~/ \
#	&& git clone https://gitlab.com/latexpand/latexpand \
#	&& cd latexpand \
#	&& make \
#	&& mv dist/latexpand/latexpand /usr/local/bin)

## Install scholdoc https://github.com/timtylin/scholdoc/blob/master/INSTALL.md
#RUN apt-get install -y --no-install-recommends \
#	ghc \
#	cabal-install \
#&& cabal update \
#&& cabal install scholdoc

#last minute
RUN . etc/environment \
&& install2.r --repos $MRAN --deps TRUE \
	servr \
	RJSONIO \
	RColorBrewer \
&& r -e 'warnings()'

#last minute
RUN . etc/environment \
&& install2.r --repos $MRAN --deps TRUE \
	webshot \
	bookdown \
	shiny \
	htmltools \
	miniUI \
	DT \
	citr \
	svglite \
&& r -e 'warnings()'

# could not install:
# webshot::install_phantomjs()

#last minute
RUN r -e '{devtools::install_github("coolbutuseless/lipsum");devtools::install_github("rstudio/bookdown");warnings()}'


### ~fin~ ###

# to use run in -d detached mode and open rstudio server at localhost:8080
CMD ["\init"]

