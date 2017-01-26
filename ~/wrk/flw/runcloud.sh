#!/bin/bash

# Navigate to project directory
cd /Users/bambrose/Dropbox/GitHub/hos/plagiat/~
# Obtain your cloud credentials/API keys from your account to use in the step below.
# log in https://cloud.google.com/sdk/docs/quickstart-mac-os-x
# https://console.developers.google.com/apis/api/compute_component/overview?project=996564382442
# https://cloud.google.com/docs/permissions-overview
# https://console.cloud.google.com/iam-admin/kms/zero
gcloud auth login
gcloud config set project plagiat-156221
gcloud auth application-default --help
# Use docker-machine to provision a cloud instance that will run your docker containers
docker-machine create --driver google --google-project plagiat-156400 --google-machine-type f1-micro cloud
# Configure your shell environment so docker will use your cloud provider to run docker containers (instead of your laptop):
eval $(docker-machine env cloud)
# check for running container
docker pull zzrot/docker-clean && docker run --rm -v /var/run/docker.sock:/var/run/docker.sock zzrot/docker-clean stop --images --log
# Run docker container for R Studio:
docker run -v $(pwd):/home/rstudio --name rstudio -d -p 8080:8787 brooksambrose/bench /init
# Create a secure tunnel for R Studio between your cloud provider and your laptop:
du -h `find ~/Library/Containers -iname '*.qcow2'`
# Open browser
open http://localhost:8080
