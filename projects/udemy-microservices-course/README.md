# Microservices with NodeJS and React

## Quick Start

### Infrastructure

This project uses GCP. 

Enable Google APIs:

```shell
gcloud services enable cloudbuild.googleapis.com
gcloud services enable gkehub.googleapis.com
```

Create GKE cluster:

```shell
gcloud config set compute/region europe-west2
gcloud container clusters create-auto "ticketing-dev" --region "europe-west2" --release-channel "regular" --network "projects/ticketing-dev-306115/global/networks/default" --subnetwork "projects/ticketing-dev-306115/regions/europe-west2/subnetworks/default" --cluster-ipv4-cidr "/17" --services-ipv4-cidr "/22"
```

Get authentication credentials for cluster:

```shell
gcloud container clusters get-credentials ticketing-dev --region "europe-west2" 
```

### Dev

With the GKE cluster available, start the dev environment:

```shell
skaffold dev
```