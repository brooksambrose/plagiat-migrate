#!/bin/bash

docker pull zzrot/docker-clean && docker run --rm -v /var/run/docker.sock:/var/run/docker.sock zzrot/docker-clean stop --images --log

if docker ps -a | grep 'rstudio' ; then docker rm -f rstudio ; fi
            
docker run -v /Users/bambrose/Dropbox/GitHub/hos/plagiat/~:/home/rstudio --name rstudio -d -p 8080:8787 brooksambrose/bench /init
open http://localhost:8080
du -h `find ~/Library/Containers -iname '*.qcow2'`
echo 'consider rm `find ~/Library/Containers -iname '*.qcow2'` to save space'
