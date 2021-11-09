# Microservices - Ticketing App

## Overview

This app is part of Stephen Girder's [Microservices with NodeJS and React](https://www.udemy.com/course/microservices-with-node-js-and-react)
course. The system is based on [www.stubhub.co.uk](https://www.stubhub.co.uk/), we'll implement the following
features:

* Users can list a ticket for an event (sports, concert, etc.) for sale
* Other users can purchase this ticket
* Any user can list tickets for sale and purchase tickets
* When a user attempts to purchase a ticket, the ticket is 'locked' for 15 minutes. The user has 15 minutes
to enter their payment info.
* While locked, no other user can purchase this ticket. After 15 minutes, the ticket should become unlocked if
the user did not complete the purchase.
* Ticket prices can be edited if they are not locked.

The implemented solution will contain:

* Advanced Kubernetes-based development workflow using Cloud Code (WebStorm Plugin) with Skaffold.
* Event-driven NodeJS/Express TypeScript microservices using NATS streaming server and MongoDB.
* React/NextJS server-side rendered (SSR) frontend.
* Production-grade authentication and payment handling using StripeJS.
* A custom central NPM library to share code between our services 

See [notes](https://www.notion.so/gregbrowndev/Section-5-The-Ticketing-App-Architecture-of-Multi-Service-Apps-19d8d1a9a5244e3487b1981221054bd4).

## Quick Start

The Skaffold tooling can be configured to deploy the services locally

```yml
apiVersion: skaffold/v2beta25
kind: Config
metadata:
  name: ticket-app
build:
  # Uncomment one of these sections to determine if 
  local:
    push: false
#  googleCloudBuild:  
#    projectId: ticketing-dev-306115
```

When deploying locally, make sure to update _/etc/hosts_

```
127.0.0.1       ticketing.dev
```

Alternatively, you'll need to use the IP of the load balancer created in GCP. (See [notes](https://www.notion.so/gregbrowndev/Section-6-Leveraging-a-Cloud-Environment-for-Development-75513d509aa540738a9df2ccb6baf1bc#70c7623ec93a4ccda4ddb83516a2f94f)

Then you can visit _https://ticketing.dev/api/users/currentuser_ to see the application!
