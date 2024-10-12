# EKS Terraform Tutoral

## Overview

This project follows Anton Putra's YouTube tutorial series to set up [Kubernetes on AWS EKS using Terraform](https://www.youtube.com/watch?v=aRXg75S5DWA&list=PLiMWaCMwGJXnKY6XmeifEpjIfkWRo9v2l&index=1&ab_channel=AntonPutra).

GitHub source code: [AWS EKS Kubernetes Tutorial](https://github.com/antonputra/tutorials/blob/main/lessons/196/README.md)

Goals:

- Get a comprehensive understanding of best practices for setting up IAM roles and permissions both for managing
  AWS users and K8s.
- Learn how to deploy and manage Kubernetes on EKS
- Explore an alternative way to work with Terraform than I've used previously, e.g. managing environments and the tool chain
- Set up [SOPS](https://github.com/getsops/sops) to manage secrets in code and simplify development onboarding and deployment

## Table of Contents

<!-- toc -->

- [Development](#development)
  * [Prerequisites](#prerequisites)
  * [Getting Started](#getting-started)
- [Notes](#notes)
  * [Part 1: Create AWS VPC](#part-1-create-aws-vpc)
  * [Part 2: Create EKS Cluster](#part-2-create-eks-cluster)
  * [Part 3: Add IAM User and IAM Role](#part-3-add-iam-user-and-iam-role)
  * [Part 4: Horizontal Pod Autoscaler (HPA)](#part-4-horizontal-pod-autoscaler-hpa)
  * [Part 5: EKS Pod Identities and Cluster Autoscaler](#part-5-eks-pod-identities-and-cluster-autoscaler)
  * [Part 6: AWS Load Balancer Controller](#part-6-aws-load-balancer-controller)
    + [Example 1: Exposing a service directly to the internet](#example-1-exposing-a-service-directly-to-the-internet)
    + [Example 2: Exposing an ingress to the internet](#example-2-exposing-an-ingress-to-the-internet)
    + [Example 3: Using ingress with SSL termination](#example-3-using-ingress-with-ssl-termination)
  * [Part 7: Ingress Nginx Controller](#part-7-ingress-nginx-controller)

<!-- tocstop -->

## Development

### Prerequisites

- Install [Terraform](https://developer.hashicorp.com/terraform/tutorials/aws-get-started/install-cli)

- Install [SOPS](https://github.com/getsops/sops)

For MacOS:

```shell
curl -LO https://github.com/getsops/sops/releases/download/v3.9.0/sops-v3.9.0.linux.arm64
mv sops-v3.9.0.linux.arm64 /usr/local/bin/sops
chmod +x /usr/local/bin/sops
```

The table of contents in this README can be generated using `markdown-toc`:

- Install `npm install -g markdown-toc`
- Run `make docs` to generate the table of contents


### Getting Started

To get started:

```shell
cd terraform
terraform init
```

Plan:

```shell
export AWS_PROFILE=personal
terraform plan
```

Apply:

```shell
terraform apply
```

Configure local kubeconfig:

```shell
export AWS_PROFILE=personal
aws eks update-kubeconfig --region eu-west-2 --name eks-tutorial-dev-demo
```

or manually set profile to use temp Role-based auth:

```shell
aws eks update-kubeconfig --region eu-west-2 --name eks-tutorial-dev-demo --profile eks-admin
```

Destroy:

> Note: we created IAM User keys manually, for `developer` and `manager` IAM Users. These keys will need to be deleted manually to tear down the infra.

```shell
terraform destroy
```

## Notes

### Part 1: Create AWS VPC

Part 1: [YouTube Link](https://www.youtube.com/watch?v=aRXg75S5DWA&list=PLiMWaCMwGJXnKY6XmeifEpjIfkWRo9v2l&index=2&ab_channel=AntonPutra)

Ref: [WS networking requirements](https://docs.aws.amazon.com/eks/latest/userguide/network-reqs.html)

Subnet config:

- The `"kubernetes.io/role/internal-elb" = "1"` tag is required on private subnets to allow EKS to deploy private LBs.
- The `"kubernetes.io/cluster/${local.name_prefix}-${local.eks_name}" = "owned"` tag is recommended as it helps manage environments containing multipe EKS clusters.
- The `"kubernetes.io/role/elb" = "1"` tag is required on the public subnets to allow EKS to deploy public LBs.

NAT config:

- Highly recommended to assign a static public IP manually to the

NAT:

  - Useful in the future if clients require a webhook, e.g. OAuth, integrations, etc., the IP can be used and whitelisted. Otherwise, the IP will be created and managed automatically by the NAT gateway.

Route tables:

- We associate both the private subnets with a route table with a default route to the NAT gateway
- We associate both the public subnets with a route table with a default route to the internet gateway
- Instructor said he's never found creating multiple NAT gateways across AZs particular useful in production environments

At the end of part 1, we set up the Terraform ready to apply:

- `cd terraform && terraform init`
- Get your AWS credentials from the AWS console and run `aws configure` to create a profile
- The instructor said its not recommended to authenticate using long-lived AWS credentials, instead the best practice is to use IAM roles with short-lived credential tokens. However, this is out of scope of this tutorial.
- We also deleted the default VPC in AWS as we would never use it in a real environment. Its very easy to recreate is you ever do want to create a new one.

### Part 2: Create EKS Cluster

Part 2: [YouTube Link](https://www.youtube.com/watch?v=uiuoNToeMFE&list=PLiMWaCMwGJXnKY6XmeifEpjIfkWRo9v2l&index=3&ab_channel=AntonPutra)

K8s control plane:

- Etcd database to store state, e.g. all the K8s manifests. It is important to regularly backup the etcd database for recovery.
- Scheduler - its main job is to watch for newly created pods and assign them to the nodes based on available CPU and memory and pod requests.
- Controller Manager - combines many different controllers, it manages the reconciliation loop
- (Legacy) Cloud Manager Controller - its part of the K8s source code but only receives security and bug fixes from cloud providers, e.g. AWS, Azure, GCP. For example, a K8s Service with type "LoadBalancer" would provision an ALB in AWS for the deployment. Nowadays, cloud providers have developed their own external managers. Later in the tutorial, we'll install AWS Load Balancer Controller to manage the LBs for the K8s cluster.
- API server - a stateless API server that interacts with all of these components. It includes authentication and authorisation.

K8s nodes:

- kubelet - responsible for running containers
- kube-proxy - a network proxy that allows communication to the pods from the network inside or outside the cluster
- runtime - the runtime to run the containers, e.g. Docker runtime

Cluster management:

- We used to use kops to manage the K8s cluster and ensure it has high-availability, but you'd still need to run kops CLI to manage the cluster. Additionally, with kops we would need to run a virtual network and access management was a nightmare. Without using additional tools like dex, you would have to attach all IAM policies to the K8s nodes and all applications would get the same access which is not good.
- We can now delegate management of the control plane to cloud providers, e.g. using EKS. EKS clusters only cost 10 cents per hour per cluster.

IAM roles:

- We should use IAM roles rather than IAM users as roles provide short-lived access tokens that limit the risk of them being exposed to the user.
- In this tutorial, we will use the policy managed by Amazon, `AmazonEKSClusterPolicy`, which contains all the permissions that the cluster needs. However, this policy assumes you will use a legacy provider that additionally requires load balancing permissions. These permissions constitute over half of the policy (i.e the `elasticloadbalancing` permissions). In the following sections, we will install AWS Load Balancer Controller that will take on that responsibility. At that point, we will create our own separate role to remove these redundant permissions.
- K8s workers require several permissions of their own:
  - The `AmazonWorkerNodePolicy` policy provides core EC2 functionality permissions, e.g. `ec2:DescribeInstances`.
  - The latest v3 of `AmazonWorkerNodePolicy` also allows "EKS pod identity" for fine-grained access to control of pod permissions, e.g. giving access to a pod to read/write to a specific S3 bucket.
  - Before we had Pod Identities(`AmazonWorkerNodePolicy` v2), we had to use an OpenID Connect provider and IAM roles for service accounts to achieve the same thing.
  - We will look at the three separate methods for authentication later in the tutorial.
- We have to grant EKS access to modify IP address configuration on EKS worker nodes using the `AmazonEKS_CNI_Policy`.
  - When we create a pod on K8s, it is assigned an IP from the secondary IP address range assigned to the worker node. We're not using virtual networks like Flannel or Calico, we get native AWS IP addresses for each pod. Later in the course, we will create k8s Service of type `LoadBalancer` which allows `Instance mode` and `IP mode`. The former is used for `NodePort` Services, while the latter works with `LoadBalancer` Services to route traffic directly to pod IPs.
  - Using `NodePort` isn't recommended in production due to security, scalability, and latency issues.
    - `NodePort` opens a port on the underlying worker node, making the node vulnerable to attacks.
    - `NodePort` is limited to 2767 ports per node (30000-32767)
    - `NodePort` increases latency as there are more network hops involved
    - However, `NodePort` is a lot more cost efficient, as `LoadBalancer` Services create a Network Load Balancer per Service. For this reason, it is common to use Istio to handle cluster networking for both ingress (proxying) and load balancing, as well as the other benefits Istio brings (TLS, centralised network monitoring/tooling).
- Lastly, the `AmazonEC2ContainerRegistryReadOnly` policy is used to grant EKS permission to pull Docker images from ECR.

In the code:

_7-eks.tf_:

- We create an IAM role per cluster, e.g. `"${local.name_prefix}-${local.eks_name}-eks-cluster"` in case we have multiple clusters per environment.
- We set up the network settings with `endpoint_public_access=true` to make the cluster publicly accessible.
  - Later in the tutorial, we will see how to set up a private EKS cluster using AWS Client VPN and a private DNS that we can use for ingress and private services that we don't want to expose to the internet, e.g. Grafana dashboards, Temporal UI, etc.
  - Even with a public cluster, the worker nodes will still be deployed on the private subnets without public IP addresses.
- We specified the two private subnets to place the worker nodes. It is required to have 2 AZs. EKS will create cross-account elastic network interface in these subnets to allow communication between worker nodes and the K8s control plane.
- We configured authentication to use `API`.
  - We used to have to manage authentication using `aws_auth` ConfigMap in kube system namespace. However, this is deprecated now. It wasn't convenient to manage existing ConfigMaps in K8s using Terraform resources.
  - AWS developed an API that we can use to add users to the cluster. You can still use ConfigMap and even both ConfigMap and API. But the API is highly recommended for user management. In the next section, we cover how to add IAM roles and IAM users to access the cluster.
- We set the `bootstrap_cluster_creator_admin_permissions=true` to grant the Terraform user admin priveleges. This option defaults to `true` anyway, but we want to be explicit. For us, because we will use Terraform to deploy Helm Charts and plain YAML resources.

_8-nodes.tf_:

- We create an IAM role for the worker nodes. We add `ec2.amazonaws.com` as the trust relationship instead of `eks.amazonaws.com` like we did in the eks cluster. In the next setion, we'll create an IAM role with a trust policy that allows specific users to assume it.
- We attached the three IAM policies discussed earlier to the nodes.
- Finally, we add the node group itself.
  - Behind the scenes its managed as an EC2 autoscaling group.
  - EKS has three types of autoscaling groups:
    - Self-managed node groups:
      - We can create the nodes ourselves using Terraform and Packer templates to customise the EC2 VM to install any packages we need.
      - Limitations: EKS will not drain the node during upgrades. This can be done semi-manually.
    - Managed node groups:
      - We'll use these in this tutorial. They are a lot easier to manage and upgrade as this is managed by the EKS control plane.
    - Fargate:
      - Fully managed node groups, "serverless" K8s, that only require the user to deploy their containers and EKS will automatically provision and scale the worker nodes for you.
      - A lot easier to manage but a lot more expensive. It has limitations such as EBS volumes.
  - We set the `node_group_name` to general
    - In production, it is common to have different node groups for different workloads, e.g. CPU optimised node groups, memory optimised node groups, or even GPU node groups for machine learning.
    - Another common node group is for spot instances. This node group can be used for fault-tolerant workloads that can handle interuptions, such as batch/streaming jobs and async workflows. Spot instances can save up to 90%.
  - We add the two private subnets to place the worker nodes.
    - In production, if we have data intensive workloads, such as running Kafka and 100s of services that read and write to Kafka from different AZs, cross AZ data transfer costs can be super expensive, even more expensive than the compute!
    - We could place all the worker nodes in a single AZ - you don't always need to create a highly available cluster by spreading nodes in different zones.
  - For `capacity_type` we chose `ON_DEMAND` but we also have `STANDARD` and `SPOT`. We also set the `instance_types`.
    - In a future tutorial, the instructor will show a strategy to come up with the proper node size.
  - The `scaling_config` by itself will not autoscale but we can set the minimum and maximum nodes as well as the `desired_size`. We will need to deploy addition components called Cluster Autoscaler to adjust the `desired_size` based on the load, such as how many pending pods we have and their resource requirements.
  - The `update_config` is used for cluster upgrades.
  - The `labels` are used for pod affinity and node selectors. There are some built-in labels derived from the node group that serve the same purpose. However, in practice when you try to migrate applications from one node group to another with the same labels it is much easier to use custom labels.

At the end of this part:

Check you're connected with the correct user:

```shell
export AWS_PROFILE=personal
aws sts get-caller-identity
```

Next, update the local kubeconfig with the following command:

```shell
aws eks update-kubeconfig --region eu-west-2 --name eks-tutorial-dev-demo
```

Verify you can access the nodes (shows we have admin privileges):

```shell
kubectl get nodes
```

Double check we have admin privileges in the EKS cluster (should output "yes"):

```shell
kubectl auth can-i "*" "*"
```

### Part 3: Add IAM User and IAM Role

Part 3: [YouTube Link](https://www.youtube.com/watch?v=6COvT1Zu9o0&list=PLiMWaCMwGJXnKY6XmeifEpjIfkWRo9v2l&index=3&ab_channel=AntonPutra)

Overview:

- We have created the EKS cluster and our IAM User has admin privileges to the cluster. However, we will also have other team members that want to use/admin the cluster.
  - Other DevOps team members also need admin privileges
  - Dev team 1 needs read/write access to a specific namespace and team 2 to access another
- Note: We might want to create a namespace quota to restrict number of pods, cpu and memory that can be allocated by that team. We use quotas to prevent interference between teams.
- The best practice is to create a common IAM Role that team members can assume, e.g. admin, team1, team2, etc. which has only the permissions it needs.
- When we deploy our EKS cluster in production, we would typically only grant read-only access to namespaces for team 1 and 2, for example.
- In AWS, we use IAM Users and Roles for identities and permissions, but in EKS we use service accounts, users and RBAC (e.g. via the `ClusterRoleBinding` kind manifest).

In the first part of this video:

- We'll map IAM User and Roles to custom RBAC groups.
- For a detailed tutorial on EKS RBAC, see [Kubernetes RBAC Explained](https://www.youtube.com/watch?v=iE9Qb8dHqWI)
- We'll map IAM User to `AmazonEKSDeveloperPolicy` which grants minimum permissions, e.g. updating local K8s config and connecting to EKS cluster
- Then in K8s we'll create a viewer-role, using `ClusterRole`, with read-only access for certain objects, e.g. `deployments` and `configmaps`.
- Next, we'll use `ClusterRoleBinding` to bind the viewer RBAC role to a "my-viewer" RBAC group.
- Finally, we'll use EKS API to link IAM Users to this K8s RBAC group.

In the second part of this video:

- It's not ideal to use IAM Users with long-lived credentials to access the EKS cluster. Instead, we'll create an `eks-admin` IAM Role with `AmazonEKSAdminPolicy` which IAM Users can assume. IAM Roles can authenticate against EKS using short-lived auth tokens.
- In K8s, we cannot use built-in RBAC groups that start with system prefix, so we'll create a new group, `my-admin`, using a `ClusterRoleBinding` and grant them the `cluster-admin` role.
- We'll create a `manager` IAM User with `AmazonEKSAssumeAdminPolicy` policy that can assume the `eks-admin` IAM Role.
- Finally, we'll bind the `eks-admin` IAM Role with the K8s `my-admin` group using EKS API.

In the code:

- We created the two k8 manifests to create the `viewer` k8s role and a binding to bind the role to the `my-viewer` k8s group.

  ```shell
  kubectl apply -f ./k8s/1-rbac-viewer
  ```

- In _9-add-developer-user.tf_:
  - we create IAM User `developer`
  - create an IAM Policy, `AmazonEKSDeveloperPolicy`.
    - Has minimum permissions to let devs update their local kubeconfig and connect to the cluster via AWS CLI / Console
    - The k8s RBAC `viewer` role will give them further permissions when using `kubectl`
  - create a policy attachment to attach policy to user. Note: best practice is to attach policy to group then add user to group
  - next we create an `aws_eks_access_entry` to bind the developer IAM User to k8s RBAC `my-viewer` group using EKS API

After applying the TF, we need to go to the Console and create access creds for the `developer` user and add them as a local profile using `aws configure --profile developer`.

Check the profile is configured correctly:

```shell
aws sts get-caller-identity --profile developer
```

Next, connect to EKS and update local kubectl config:

```shell
aws eks update-kubeconfig \
  --region eu-west-2 \
  --name eks-tutorial-dev-demo \
  --profile developer
```

Output: _Updated context arn:aws:eks:eu-west-2:496487758480:cluster/eks-tutorial-dev-demo in /Users/gregbrown/.kube/config_

Check that the local Kubernetes config uses the `developer` profile:

```shell
kubectl config view --minify
```

Check we have access to the cluster:

```shell
kubectl get pods
# Out: No resources found in default namespace.
```

We can check if we have permissions:

```shell
kubectl auth can-i get pods
# Out: yes

kubectl auth can-i get nodes
# Out: no

kubectl get nodes
# Out: Error from server (Forbidden): nodes is forbidden: User "arn:aws:iam::0000000000:user/developer" cannot list resource "nodes" in API group "" at the cluster scope

# Check admin privileges
kubectl auth can-i "*" "*"
# Out: no
```

- In _k8s/2-rbac-admin/admin-cluster-role-binding.yaml_:
  - we create another IAM User and IAM Role and grant admin privileges in the cluster
  - k8s ships with a default cluster admin role and group, but the EKS API won't let use the default group (since it starts with the system prefix), so we create our own admin group, `my-admin`, and bind it to the existing `cluster-admin` role.

To apply this binding, we need to switch to the admin user again:

```shell
# Note: on work machine I used `export AWS_PROFILE=personal`
aws eks update-kubeconfig \
  --region eu-west-2 \
  --name eks-tutorial-dev-demo
```

Apply the binding:

```shell
kubectl apply -f ./k8s/2-rbac-admin
```

- In _10-add-manager-role.tf`:
  - First, we use `data` resource to get AWS account number for IAM Role.
  - We create an `eks-admin` IAM Role with the principal: `"arn:aws:iam::${data.aws_caller_identity.current.account_id}:root"`. This allows any user in the account to assume the IAM Role.
  - We create the `eks-admin` IAM policy with permission to grant all access to EKS. You can also grant additional permissions to view all EKS tabs (?)
  - We attach the `eks-admin` policy to the `eks-admin` role.
  - We create another IAM User, `manager`, using TF and will create keys manually. Note: when we want to tear down the infra, we'll have to manually delete the keys.
  - We create an IAM Policy, `AmazonEKSAssumeAdminPolicy`, to allow the `manager` IAM User to assume the `eks-admin` IAM Role. (I thought we already allowed these with the `"arn:aws:iam::${data.aws_caller_identity.current.account_id}:root"` principal?)
  - Next, we attach the IAM Policy to the `manager` IAM Role (Note: best practice is to first create an IAM Group)
  - Lastly, we create an `aws_eks_access_entry` resource to add the IAM User to the `my-admin` k8s RBAC group.

Lets apply the TF and check the Console. We should see we now have the `manager` IAM User. Create access keys and configure a `manager` profile.

We will not use the `manager` IAM User directly to access EKS. Instead, we will assume the `eks-admin` IAM Role.

Check you can assume the role using the `manager` IAM User:

```shell
export PERSONAL_ACCOUNT_ID=  # omitted for security
aws sts assume-role \
  --role-arn arn:aws:iam::${PERSONAL_ACCOUNT_ID}:role/eks-tutorial-dev-demo-eks-admin \
  --role-session-name manager-session \
  --profile manager
# Out: (temporary credentials)
```

We now need to create another AWS profile manually, edit aws config:

```shell
# vim ~/.aws/config
[profile eks-admin]
role_arn = arn:aws:iam::[PERSONAL_ACCOUNT_ID]:role/eks-tutorial-dev-demo-eks-admin
source_profile = manager
```

This profile doesn't have any credentials. We will use this profile with `kubectl`, we'll get a set of temporary credentials for the role using the `manager` IAM User to assume the `eks-admin` role.

Note: if we created a `manager` IAM Group and added specific users to the group, they could replace `source_profile` with their own profile rather than needing to configure the `manager` profile.

Now, lets update kube config, but this time use the `eks-admin` profile we just configured:

```shell
aws eks update-kubeconfig \
  --region eu-west-2 \
  --name eks-tutorial-dev-demo \
  --profile eks-admin
```

### Part 4: Horizontal Pod Autoscaler (HPA)

Part 4: [YouTube Link](https://www.youtube.com/watch?v=0EWsKSdmbz0&list=PLiMWaCMwGJXnKY6XmeifEpjIfkWRo9v2l&index=4&ab_channel=AntonPutra)

Overview:

- In this part, we'll cover K8s `HorizontalPodAutoscaler` (HPA) and the components you need to install to make it work.
- Commonly, we would use CPU or memory useage or both to decide when to scale the pod. In order to do this, we must make sure the `Deployment` defines CPU/memory limits.
- When following the GitOps approach, we must not set `replicas` on the `Deployment` or `StatefulSet`, otherwise you will get a conflict/race condition between the GitOps tool and the HPA, e.g. ArgoCD keeps trying to scale to 1 while HPA is trying to scale to 5.
- The HPA resource uses a `spec.scaleTargetRef.name` property to select the `metadata.name` property of the `Deployment`, not a label like a `Service`.
- To scale using custom metrics, e.g. queue size or requests per second, we will need another component. See [related tutorials](https://www.youtube.com/@AntonPutra/search?query=hpa%20custom%20metrics).
- To use HPA, we usually deploy `metrics-server` to the cluster that scrapes each kubelet and publishes them to the K8s metrics API. The `metrics-server` rarely needs any maintainance so its safe to deploy it using a Helm Chart.
- We will take a look at using the Terraform Helm provider so we can apply Helm Charts through Terraform
  - Note: Keiran mentioned that this is a bit flakey, if the apply fails it can screw your k8s deployment

In `11-helm-provider.tf`:

- we configured the Helm provider

In `12-metrics-server.tf`:

- we use the Helm provider to install the metrics server
- we also created a config file at `values/metrics-server.yaml` to store some configuration, e.g.
  - `metric-resolution=15s` defines the frequency the metrics are scraped.
  - `secure-port=10250` defines the port to scrape on the kubelets, so it needs to be defined correctly.

In order to apply TF, we need to use `terraform init` to initialise the new TF provider:

```shell
terraform init
terraform apply
```

We can check if we can see the metrics-server pod running:

```shell
kubectl get pods -n kube-system
```

We can also try to fetch some logs:

```shell
kubectl logs -l app.kubernetes.io/instance=metrics-server -f -n kube-system
```

This is especially important when provisioning a custer using kops.

Finally, the most important thing to check is we can get the metrics of the pods in the cluster:

```shell
kubectl top pods -n kube-system
```

or

```shell
kubectl top nodes -n kube-system
```

> Note: CPU (cores) such as "4m" means millicores or equivalent to 0.004 vCPUs in ECS. The memory unit "Mi" (mebibytes) is similar to MB, e.g.
> 1 MiB = 1,048,576 bytes (2^20 bytes)
> 1 MB  = 1,000,000 bytes (10^6 bytes)
> So 15Mi = 15.73 MB

Lets deploy the simple app:

```shell
kubectl apply -f ./k8s/3-simple-app
```

and check the pods are running:

```shell
watch -t kubectl get pods -n 3-example
# NAME                     READY   STATUS    RESTARTS   AGE
# myapp-86db698dcc-qrpmn   1/1     Running   0          4m51s
```

and the HPA is running:

```shell
watch -t kubectl get hpa -n 3-example
# NAME    REFERENCE          TARGETS          MINPODS   MAXPODS   REPLICAS   AGE
# myapp   Deployment/myapp   1%/80%, 0%/70%   1         5         1          4m34s
```

> Note: the HPA `TARGETS` shows "unknown" when you forget to define the resource limits on the deployment.

We can check the service is running:

```shell
kubectl get svc -n 3-example
# NAME    TYPE        CLUSTER-IP       EXTERNAL-IP   PORT(S)    AGE
# myapp   ClusterIP   172.20.238.204   <none>        8080/TCP   6m28s
```

We can use port-forwarding to access the service from our local machines:

```shell
kubectl port-forward svc/myapp 8080 -n 3-example
```

which we can hit with curl:

```shell
curl "localhost:8080/api/cpu?index=44"
# (after some time)
# {"message":"Testing CPU load: Fibonacci index is 44, number is 701408733"}%
```

This will start a job to generate Fibonacci numbers which is CPU intensive. We can see the load in the HPA:

```shell
watch -t kubectl get hpa -n 3-example
# NAME    REFERENCE          TARGETS            MINPODS   MAXPODS   REPLICAS   AGE
# myapp   Deployment/myapp   101%/80%, 0%/70%   1         5         2          10m
```

We can see that the HPA created a second pod to handle the load!

HPA will also scale down the pod after 5-10 mins of sub-threshold load. This is configurable in the HPA resource.

Let's clean up and delete the namespace we created:

```shell
kubectl delete ns 3-example
```

### Part 5: EKS Pod Identities and Cluster Autoscaler

Part 5: [YouTube Link](https://www.youtube.com/watch?v=HbRyRJnBEfw&list=PLiMWaCMwGJXnKY6XmeifEpjIfkWRo9v2l&index=14&ab_channel=AntonPutra)

Overview:

In this part, we'll cover:

- EKS Pod Identities to provide fine-grained permissions to pods, e.g. allow access to a specific S3 bucket
- Cluster Autoscaler to scale the number of nodes in our cluster

Notes:

- Cluster Autoscaler needs to be installed into the cluster
- Remember we need to disable `scaling_config[0].desired_size` in the `8-nodes.tf` Terraform code, to prevent TF from scaling the cluster size back to its initial configuration.
- Cluster Autoscaler requires permissions to interact with AWS to adjust to number of nodes.
  - Previously, we would use IAM OIDC provider. It was a bit complicated, we would need to:
    - Create an OIDC provider on the IAM side
    - Create an IAM Role
    - Establish trust with a particular k8s namespace and k8 service account
    - The most annoying part about this approach was needing to use an annotation on the ServiceAccount to link the IAM Role using the IAM Role ARN.
  - Now with EKS' Pod Identities, it is much easier.
    - We enable it using an EKS addon, `eks-pod-identity-agent`.
    - We still need to create an IAM Role for the K8s `ServiceAccount`, but the trust (`Principal`) is just `pods.eks.amazonaws.com` rather than an ARN.
    - To bind an IAM Role with an k8s `ServiceAccount`, we don't need to use an annotation anymore. Instead, we use EKS API with the `aws_eks_pod_identity_assocation` TF resource.

In `13-pod-Identity-addon.tf`:

- We deploy the EKS Pod Identity agent as a k8s `DaemonSet` so it runs on every single node.
- We deploy this simply as an EKS addon.

We can find the latest version of any addon using:

```shell
aws eks describe-addon-versions \
  --region eu-west-2 \
  --addon-name eks-pod-identity-agent
```

You can see the latest version at the top. At the time of writing the latest version was `v1.3.2-eksbuild.2` but I'll keep it the same as used in the tutorial.

Let's apply the TF: `terraform apply`, and check the Pod Identity pod is running:

```shell
kubectl get pods -n kube-system
# NAME                              READY   STATUS    RESTARTS   AGE
# eks-pod-identity-agent-ds4hb      1/1     Running   0          77s
# ...
```

We can also get the daemonsets, specifically:

```shell
kubectl get daemonset eks-pod-identity-agent -n kube-system
# NAME                     DESIRED   CURRENT   READY   UP-TO-DATE   AVAILABLE   NODE SELECTOR   AGE
# eks-pod-identity-agent   1         1         1       1            1           <none>          2m24s
```

Without the agent running, we won't be able to authorised our client with AWS Services.

In `14-cluster-autoscaler.tf`:

- We first need to create an IAM Role which the Cluster Autoscaler client will assume to scale the nodes.
- We always use `Service = "pods.eks.amazonaws.com"` for the trust relationship. Compared to using the OIDC provider approach, we would need to specify both the k8s namespace and service account provider.
- Next, we create a IAM Policy that allows the IAM Role (and Cluster Autoscaler) to access the autoscaling group.
- We attach the IAM Policy to the IAM Role
- Next, we need to associate this with a k8s account. We still need to provide the `namespace` where the Autoscaler is running and the `cluster-autoscaler` service account
- Finally, we deploy the Cluster Autoscaler using a Helm Chart

Let's apply the TF and check the autoscaler is deployed:

```shell
kubectl get pods -n kube-system
# NAME                                                 READY   STATUS             RESTARTS      AGE
# autoscaler-aws-cluster-autoscaler-56d4497b59-t524t   0/1     CrashLoopBackOff   5 (63s ago)   3m59s
# ...
```

Looks like it crashed!

```shell
kubectl logs -l app.kubernetes.io/instance=autoscaler -f -n kube-system
```

> The error looks like the cluster-autoscaler was not assuming the IAM role we associated
> with the `cluster-autoscaler` service account via Pod Identity. I upgraded the Cluster
> Autoscaler to Helm Chart version 9.43.0 which seemed to fix the issue.

We can apply the TF changes and deploy the `k8s/4-cluster-autoscaler-app` manifests:

```shell
kubectl apply -f ./k8s/4-cluster-autoscaler-app
```

Watch the nodes and pods. At first you will see only 2 pods running. Eventually, you will see a second node appear and the remaining pods will start running.

```shell
watch -t kubectl get nodes
# NAME                                        STATUS   ROLES    AGE     VERSION
# ip-10-0-4-49.eu-west-2.compute.internal     Ready    <none>   16m     v1.29.8-eks-a737599
# ip-10-0-40-138.eu-west-2.compute.internal   Ready    <none>   2m18s   v1.29.8-eks-a737599
```

```shell
watch -t kubectl get pods -n 4-example
# NAME                     READY   STATUS    RESTARTS   AGE
# myapp-5c94dbbc45-6ph79   1/1     Running   0          3m15s
# myapp-5c94dbbc45-8n42h   1/1     Running   0          3m15s
# myapp-5c94dbbc45-fh4h4   1/1     Running   0          3m15s
# myapp-5c94dbbc45-nlm6z   1/1     Running   0          3m15s
# myapp-5c94dbbc45-wb6ks   1/1     Running   0          3m15s
```

Clean up by deleting the namespace:

```shell
kubectl delete ns 4-example
```

> Note: it will take around 10 minutes for the second node to scale back down.

### Part 6: AWS Load Balancer Controller

Part 6: [YouTube Link](https://www.youtube.com/watch?v=5XpPiORNy1o&list=PLiMWaCMwGJXnKY6XmeifEpjIfkWRo9v2l&index=7&ab_channel=AntonPutra)

Overview:

In this part, we'll cover:

- How to expose your application running in k8s to the internet or within just the VPC
- Cloud Controller Manager (legacy) and how it provisions cloud resources when you create a LoadBalancer service (NLB in AWS)

Notes:

- Cloud Controller Manager (legacy)
  - For a long time would create a classic load balancer by default in AWS
  - You can use specific annotations in the manifest to control the type, internal, and cross-zone enabled, for example
  - Problems:
    - Cloud Providers have to integrate their changes into the Kubernetes release cycle, slowing down development
    - The legacy LB controller works by adding all your k8s workers to the target group using NodePorts. This adds additional network hops which increases latency. The target groups also have a hard limit of 500 IPs (?), so this solution doesn't scale for large k8s clusters.
  - This controller has been deprecated and only receives bug and security fixes. In Kubernetes 1.31, this controller has been removed altogether!
- AWS Load Balancer Controller
  - Cloud providers now develop their own external k8s controllers. AWS has the [AWS Load Balancer Controller](https://kubernetes-sigs.github.io/aws-load-balancer-controller/latest/).
  - This is usually installed using Helm Charts
  - This controller creates an NLB for each k8s `Service` and a ALB for a k8s `Ingress`
  - Since we now use the native VPC network when creating EKS cluster, each pod gets VPC routable IP addresses. This means you can add pod IP addresses directly to the LB target group
- Ingress
  - There are two ingress types available:
    - When using the AWS Load Balancer Controller to create an `Ingress`, it creates a Layer 7 ALB running outside the EKS cluster.
      - It understands HTTP protocol, so you can route via host/path headers and HTTP verbs.
      - `Instance Mode` adds the service's `NodePort` to the target group, like the legacy controller.
      - `IP Mode` adds the pods IP address directly to the target group and is more efficient
      - We can use annotations to add a ACM certificate ARN to secure the ALB with TLS (HTTPS).
      - To monitor network traffic and collect metrics, we'd need to get them from ALB.
      - The AWS LB Controller creates one ALB per `Ingress` by default. There is the `IngressGroup` resource that enables you to group multiple Ingress resources together and merge them into a single ALB.
    - When using the classic Nginx Ingress Controller to create an `Ingress`, it creates a Nginx server running inside the EKS cluster and a Layer 4 NLB outside the EKS cluster.
      - To secure the ingress with TLS, you'd need to deploy Kubernetes Cert Manager which keeps the SSL certs up to date with Lets Encrypt. However, you need to store the certificate and private key in the EKS cluster and mount them to the Nginx pod.
      - Having a proxy (the nginx server) adds complexity, but one benefit is that is creates a single place we can monitor network traffic for all applications, e.g. using Prometheus. We'd be able to get requests per second, latency, availability, etc.

Questions:

- Do AWS NLB's also have target groups?
- Does each `Ingress` create a new ALB or does it merge them together?
- Does each load balancer `Service` create a new NLB? Is this very expensive?
  - Note: NLBs are cheaper and faster than ALBs!
- How actually do the ALB/NLB interact?
  - The ingress ALB routes traffic from the internet/VPC to a target group. Does this target group then contain the NLB IP address which then load balances the requests directly to the pods?

In `15-aws-lbc.tf`:

- The AWS Load Balancer Controller will need access to create load balancers, so we create a policy to allow this. We used Pod Identities to grant access.
- In the previous example, we use Pod Identities with `assume_role_policy` on the role itself. It is the same in this case, but we use a data resource to create the assume policy and we've created the role as a separate resource.
- Next, we create a IAM Policy to define the permissions for the AWS Load Balancer Controller.
  - In this example, we've put the policy in a file. This is just another way of doing it.
- Next, we attach the policy to the role.
- Finally, we link the IAM Role with the Kubernetes ServiceAccount using Pod Identity Association
- With IAM permissions set up, we then deploy the AWS Load Balancer Controller using the Helm provider

In `./iam/AWSLoadBalancerController.json`:

- We need to create the IAM Policy file used in the TF above.
- This is a large IAM policy and doesn't have any constraints. For example, if you wanted to dynamically add resource constraints, you would use the `templatefile` Terraform function and parse it during creation.

Apply these TF changes and get the pod is running:

```shell
kubectl get pods -n kube-system
```

Let's test out the AWS LB controller with a set of examples.

#### Example 1: Exposing a service directly to the internet

In `k8s/5-lbc-example-1`:

- We've create a simple example using an internet facing service of type `LoadBalancer`
- The `service.beta.kubernetes.io/aws-load-balancer-type: external` annotation tells k8s to use the AWS Load Balancer Controller, otherwise the legacy controller will be
  used.
- The `service.beta.kubernetes.io/aws-load-balancer-nlb-target-type: ip` annotation tells the NLB to use IP mode to add the pod IPs directly to the target group
- The `service.beta.kubernetes.io/aws-load-balancer-scheme: internet-facing` annotation sets a public IP on the NLB so you can reach it from the internet
- The `service.beta.kubernetes.io/aws-load-balancer-proxy-protocol: "*"` annotation can be set optionally to use a proxy protocol if your application needs to know the actual client source
  IP address, otherwise it will get the load balancer source IP.

Apply and check its running:

```shell
kubectl apply -f ./k8s/5-lbc-example-1
# namespace/5-example created
# deployment.apps/myapp created
# service/myapp created

kubectl get pods -n 5-example
# NAME                     READY   STATUS    RESTARTS   AGE
# myapp-54c68646fc-2w6wf   1/1     Running   0          12s

kubectl get svc -n 5-example
# NAME    TYPE           CLUSTER-IP      EXTERNAL-IP                                                PORT(S)          AGE
# myapp   LoadBalancer   172.20.18.230   k8s-5example-myapp-[omitted].elb.eu-west-2.amazonaws.com   8080:32742/TCP   86s
```

> Note: it may take a few minutes for the LB to appear. If it doesn't get created, check the AWS load balancer controller logs.

Checking out the AWS Console:

- We can see the load balancer was created
  - It is a Network load balancer. Note: NLBs are cheaper and faster the ALBs, but they are Layer 4 as opposed to Layer 7 for ALBs. As such, they are limited to only route traffic directly to a target group, whereas an ALB can use protocol-specific routing.
  - It was created in the public subnets, because of the `internet-facing` annotation and because we added annotations to the subnets when we created the VPC.
  - Looking at the target group, we see that because we used IP mode, it does not add all the k8s NodePorts to target group and instead added a single pod IP address. This is the biggest benefit of IP mode: lower latency, better security, and no limit on how many nodes the cluster can have.

We can check the IP of the pod using the command below:

```shell
kubectl get pods -o wide -n 5-example
# NAME                     READY   STATUS    RESTARTS   AGE   IP          NODE                                      NOMINATED NODE   READINESS GATES
# myapp-54c68646fc-2w6wf   1/1     Running   0          53m   10.0.1.97   ip-10-0-4-49.eu-west-2.compute.internal   <none>           <none>
```

We can use CURL to hit the NLB: to test we can access the pod

```shell
curl -i http://k8s-5example-myapp-[omitted].elb.eu-west-2.amazonaws.com:8080/about
# HTTP/1.1 200 OK
# Date: Sat, 12 Oct 2024 14:04:43 GMT
# Content-Type: application/json
# Content-Length: 39
#
# {"service":"myapp","version:":"v0.1.4"}
```

Clean up:

```shell
kubectl delete ns 5-example
```

#### Example 2: Exposing an ingress to the internet

In addition to creating load balancers, the AWS Load Balancer Controller can create ingresses (forward proxies). It ships with support for ALB by default:

```shell
kubectl get ingressclass
# NAME   CONTROLLER            PARAMETERS   AGE
# alb    ingress.k8s.aws/alb   <none>       71m
```

In this example, `./k8s/6-lbc-example-2`:

- we have the same deployment object
- we create a service of type `ClusterIP`:
  - Note: if you want to use ingress with `Instance Mode`, you would use `NodePort`. For an ALB with `IP mode` we use `ClusterIP`
- we create an ingress with relevant annotations. Importantly, we need to add the `alb.ingress.kubernetes.io/healthcheck-path: /health` annotation to health check the pods attached to the target group
- The ingress contains a host-based routing rule for the host `ex6.antonputra.com` and path `/`. We won't create a DNS record in this example (the next one we will), but we'll see how we can use CURL to add this header for local testing.

Lets apply and check our objects have been created:

```shell
kubectl apply -f ./k8s/6-lbc-example-2
# namespace/6-example created
# deployment.apps/myapp created
# service/myapp created
# ingress.networking.k8s.io/myapp created

kubectl get ing -n 6-example
# NAME    CLASS   HOSTS                ADDRESS                                                                PORTS   AGE
# myapp   alb     ex6.antonputra.com   k8s-6example-myapp-363e976e6a-1006909819.eu-west-2.elb.amazonaws.com   80      62s

curl -i --header "Host: ex6.antonputra.com" http://k8s-6example-myapp-363e976e6a-1006909819.eu-west-2.elb.amazonaws.com/about
# HTTP/1.1 200 OK
# Date: Sat, 12 Oct 2024 14:27:49 GMT
# Content-Type: application/json
# Content-Length: 39
# Connection: keep-alive
#
# {"service":"myapp","version:":"v0.1.4"}%
```

> Note: Since we're using ALB, we don't need to use port 8080 like we did with the NLB

Clean up:

```shell
kubectl delete ns 6-example
```

#### Example 3: Using ingress with SSL termination

To use HTTPS and do SSL termination at the ALB, we cannot use Lets Encrypt to obtain an SSL certificate. Instead, we'll use ACM.

> I won't do this in this tutorial, because you have to create both an SSL certificate and a custom domain. (Can't think of a useful one I actual want to create, so will skip this example.)

We'll do this via the Console, but this can be done via Terraform. I guess for this course, we would't want the certificate to get deleted everytime we clean down our infrastructure as it costs money to issue an SSL cert!

Here are the steps:

- Go to Route53 and register a domain, e.g. `ex7.antonputra.com`.
- Go to AWS Certificate Manager (ACM)
- Request a public certificate
- Provide the domain name, `ex7.antonputra.com`
- Validation method: DNS validation - recommended
- Key algorithm: RSA 2048

Next, we need to prove that we own the domain name. When using Route53 to create the domain, you can just click "Create records in Route 53".

After the certificate is verified, we can copy the ARN into the Ingress annotation, e.g. `alb.ingress.kubernetes.io/certificate-arn: arn:aws:acm:us-east-2:424432388155:certificate/7f32327d-ad95-4977-91c2-8fae85e9e598`

> Note: because we're doing SSL termination outside of the EKS cluster, we don't need to keep the certficate and private key in k8s! In the next, section, we'll look at setting up Ingress Nginx using Cert Manager and handle SSL termination in the cluster.

Once the Ingress is deployed, the final step is to create a CNAME record in our DNS to route traffic to the ALB for the custom domain.

- For the record name, we'd use `ex7`
- For the CNAME value, we'd use the ALB DNS

Check you can resolve the DNS:

```shell
dig ex7.antonputra.com
```

The final check is to test we have a valid certificate in the browser at https://ex7.antonputra.com/about.

Clean up:

```shell
kubectl delete ns 7-example
```

### Part 7: Ingress Nginx Controller

Part 6: [YouTube Link](https://www.youtube.com/watch?v=xwiRjimKW9c&list=PLiMWaCMwGJXnKY6XmeifEpjIfkWRo9v2l&index=7&ab_channel=AntonPutra)

Overview:

In this part, we'll cover:

- Pretty much the same as in the last part, but using Ingress Nginx Controller for ingress
- The pros and cons and which one is recommended

Notes:

- Architecture:
  - When using Ingress Nginx, a Nginx pod is deployed in the cluster and a Layer 4 NLB is provisioned outside the cluster.
  - The NLB does not understand HTTP protocol and will not terminate TLS, so it will pass everything to the NGINX controller.
  - The ingress rules in the manifest are used to generate an Nginx config to route the traffic to the pods.
  - Nginx acts as a reverse proxy.
- We'll still use AWS Load Balancer Controller to create an NLB outside the cluster to route traffic to the Nginx pod.
  - We'll use `IP mode` like before in the NLB. This means:
  - AWS will not use NodePorts, so won't add all the k8s workers to the target group.
  - Instead, it will use NGINX controller pod IP addresses only in the target group.
- Since we have a central proxy in the cluster, we can scrape metrics using Prometheus to monitor all network traffic.
- In many organisations, its common to have two Ingress nginx for internal and external facing services.
  - The `external-nginx` will have a public LB (public IP) outside the cluster exposed to the internet
  - The `internal-nginx` will have private LB (private IP) outside the cluster that is only routable within the VPC. For this we use private Route53 hosted zones, private DNS, and Client VPNs, e.g. for Grafana dashboards. We'll look at this in a future section of the tutorial.
- Another less-known benefit of Ingress Nginx is that it can be used to route both TCL and UDP services. Usually we'd expose them with a service of type Load Balancer. In the AWS controller, this would create a separate NLB per service which results in higher costs, as the NLB are charged hourly. With Nginx controller, we can expose the Load Balancer services with a single NLB load balancer
- For TLS termination, there are two solutions:
  - Load the certificate and private key into k8s Secret
  - Automate this process with Cert Manager (Lets Encrypt)
- Cert Manager can be configured for one of two challenges:
  - HTTP-01 is easier to set up
  - DNS-01 is recommended for production but more involved to set up as it requires Cert Manager to have permissions to write TXT records to Route53
  - Lets Encrypt certificates are valid for 90 days, but Cert Manager will try to renew them every 60 days. If renewal fails, you can configure it to send you an email.

In `./terraform/16-nginx-ingress.tf`:

- We deploy the Nginx controller with Helm
- It is simple to set up as it doesn't require any IAM permissions
- We create a separate values file to configure an external, public facing NLB to route traffic into the cluster.

Deploy terraform and check:

```shell
kubectl get svc -n ingress
# NAME                                          TYPE           CLUSTER-IP       EXTERNAL-IP                                                                    PORT(S)                      AGE
# external-ingress-nginx-controller             LoadBalancer   172.20.214.127   k8s-ingress-external-c725936549-ed294beaf4e30b50.elb.eu-west-2.amazonaws.com   80:31431/TCP,443:30563/TCP   106s
# external-ingress-nginx-controller-admission   ClusterIP      172.20.136.195   <none>                                                                         443/TCP                      106s
```

- The `external-ingress-nginx-controller` LB is used for each ingress that we configure. This is an NLB created in AWS.
- You will always want to create a CNAME record with a value pointing to this LB, i.e. `k8s-ingress-external-c725936549-ed294beaf4e30b50.elb.eu-west-2.amazonaws.com`.
- By default, it exposes only port 80 and 443.
- There are options to expose custom TCP and UDP services using a single load balancer.
- The NLB target group contains the IP address of the Nginx ingress. If we have more replicas of Nginx, we would see these here too.

We can see the IP of the Nginx pod matches the IP in the NLB target group:

```shell
kubectl get pods -o wide -n ingress
# NAME                                                READY   STATUS    RESTARTS   AGE     IP            NODE                                      NOMINATED NODE   READINESS GATES
# external-ingress-nginx-controller-5574df9cd-bgm5q   1/1     Running   0          7m51s   10.0.15.186   ip-10-0-4-49.eu-west-2.compute.internal   <none>           <none>
```

After deploying Nginx Ingress controller, we can see we have the second ingress class:

```shell
kubectl get ingressclass
# NAME             CONTROLLER             PARAMETERS   AGE
# alb              ingress.k8s.aws/alb    <none>       4h36m
# external-nginx   k8s.io/ingress-nginx   <none>       9m21s
```

> Note: It's called `external-nginx` because thats what we put in the values.yaml. If we wanted to create an internal nginx, we'd need to deploy a separate nginx controller with an internal LB.

In `./k8s/8-nginx-example-1.yaml`:

- We deploy a simple app like before
- We created a service with clusterip type
- For ingress, we've referenced the `external-nginx` class in `ingressClassName`
- The ingress uses `ex8.antonputra.com` host-based routing. We won't create a DNS for this one, we'll test it like we did before.

Apply and check:

```shell
kubectl apply -f ./k8s/8-nginx-example-1
# namespace/8-example created
# deployment.apps/myapp created
# service/myapp created
# ingress.networking.k8s.io/myapp created

kubectl get pods -n 8-example
# NAME                     READY   STATUS    RESTARTS   AGE
# myapp-54c68646fc-r9h5j   1/1     Running   0          34s

kubectl get ing -n 8-example
# NAME    CLASS            HOSTS                ADDRESS                                                                        PORTS   AGE
# myapp   external-nginx   ex8.antonputra.com   k8s-ingress-external-c725936549-ed294beaf4e30b50.elb.eu-west-2.amazonaws.com   80      74s
```

We can use the ingress to CURL the service:

```shell
curl -i --header "Host: ex8.antonputra.com" http://k8s-ingress-external-c725936549-ed294beaf4e30b50.elb.eu-west-2.amazonaws.com/about
# HTTP/1.1 200 OK
# Date: Sat, 12 Oct 2024 17:44:26 GMT
# Content-Type: application/json
# Content-Length: 39
# Connection: keep-alive
#
# {"service":"myapp","version:":"v0.1.4"}%
```

Clean up:

```shell
kubectl delete ns 8-example
```
