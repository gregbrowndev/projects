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

This project requires Kubernetes. Easiest to install this via:

* [Docker for Windows](https://docs.docker.com/desktop/windows/install/)
* [Docker for Mac](https://docs.docker.com/desktop/mac/install/)

Make sure `kubectl` is pointing to your local Kubernetes context 
and not your work's production cluster:

```
kubectl config get-contexts

kubectl config set-context docker-desktop 
```

Next, install [ingress-nginx](https://kubernetes.github.io/ingress-nginx/deploy/#quick-start)
into the Kubernetes cluster. This will enable nginx as an ingress controller
(routes traffic to the backend services based on the rules in _infra/ingress-srv.yaml_). This can be installed
using Helm or with the following manifest file:

```
kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/controller-v1.0.4/deploy/static/provider/cloud/deploy.yaml
```

Test that the controller is running after a short time:

```
kubectl get pods --namespace=ingress-nginx
```

Next, install [Skaffold](https://skaffold.dev/docs/install/#standalone-binary):

```
brew install skaffold
```

> Note: Skaffold can now be installed in VS Code and PyCharm using an extremely powerful plugin: see
> [Managed IDE](https://skaffold.dev/docs/install/#managed-ide). This integrates the IDE deeply with the Skaffold workflow, creating
> run/debug configurations for all the services deployed onto Kubernetes.
> 
> Caveat! Cloud Code creates its own minikube Kubernetes cluster, so I think you may need to install ingress-nginx
> in that cluster too. Haven't tried yet!

Update _/etc/hosts_ to route traffic from `ticketing.dev` to IP address where our Kubernetes cluster is located,
i.e. loopback for local or remote IP for GKE, etc:

```
127.0.0.1       ticketing.dev
```

Now we can deploy the system onto the Kubernetes cluster. 

Use the `local` Skaffold profile to build images locally:

```
skaffold dev -p local
```

or use the `gcb` profile to use Google Cloud Build:

```
skaffold dev -p gcb
```


Alternatively, you'll need to use the IP of the load balancer created in GCP. (See [notes](https://www.notion.so/gregbrowndev/Section-6-Leveraging-a-Cloud-Environment-for-Development-75513d509aa540738a9df2ccb6baf1bc#70c7623ec93a4ccda4ddb83516a2f94f)

Then you can visit _https://ticketing.dev/api/users/currentuser_ to see the application!

> You'll be greated by Chrome's unsafe page when you visit this URL in the browser. Simply type
> `thisisunsafe` to bypass the warning. Alternatively, you can use Postman to interact with the system.


## Additional Dev Tooling

* [ESLint](https://typescript-eslint.io/docs/getting-started/linting/linting)
* [Prettier](https://prettier.io/docs/en/install.html)
* 
