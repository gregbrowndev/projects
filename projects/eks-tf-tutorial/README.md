# EKS Terraform Tutoral

## Overview

This project follows Anton Putra's YouTube tutorial series to set up [Kubernetes on AWS EKS using Terraform](https://www.youtube.com/watch?v=aRXg75S5DWA&list=PLiMWaCMwGJXnKY6XmeifEpjIfkWRo9v2l&index=1&ab_channel=AntonPutra).

Goals:

- Get a comprehensive understanding of best practices for setting up IAM roles and permissions both for managing
  AWS users and K8s.
- Learn how to deploy and manage Kubernetes on EKS
- Explore an alternative way to work with Terraform than I've used previously, e.g. managing environments and the tool chain
- Set up [SOPS](https://github.com/getsops/sops) to manage secrets in code and simplify development onboarding and deployment

## Development

### Create the EKS cluster

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

Destroy:

```shell
terraform destroy
```

## Notes

### Part 1

Part 1: https://www.youtube.com/watch?v=aRXg75S5DWA&list=PLiMWaCMwGJXnKY6XmeifEpjIfkWRo9v2l&index=2&ab_channel=AntonPutra

- AWS networking requirements: https://docs.aws.amazon.com/eks/latest/userguide/network-reqs.html
- Subnet config:
  - The `"kubernetes.io/role/internal-elb" = "1"` tag is required on private subnets to allow EKS to deploy private LBs.
  - The `"kubernetes.io/cluster/${local.name_prefix}-${local.eks_name}" = "owned"` tag is recommended as it helps manage environments containing multipe EKS clusters.
  - The `"kubernetes.io/role/elb" = "1"` tag is required on the public subnets to allow EKS to deploy public LBs.
- NAT config:
  - Highly recommended to assign a static public IP manually to the NAT
    - Useful in the future if clients require a webhook, e.g. OAuth, integrations, etc., the IP can be used and whitelisted. Otherwise, the IP will be created and managed automatically by the NAT gateway.
- Route tables:
  - We associate both the private subnets with a route table with a default route to the NAT gateway
  - We associate both the public subnets with a route table with a default route to the internet gateway
  - Instructor said he's never found creating multiple NAT gateways across AZs particular useful in production environments

At the end of part 1, we set up the Terraform ready to apply:
- `cd terraform && terraform init`
- Get your AWS credentials from the AWS console and run `aws configure` to create a profile
- The instructor said its not recommended to authenticate using long-lived AWS credentials, instead the best practice is to use IAM roles with short-lived credential tokens. However, this is out of scope of this tutorial.
- We also deleted the default VPC in AWS as we would never use it in a real environment. Its very easy to recreate is you ever do want to create a new one.

## Part 2

Part 2: https://www.youtube.com/watch?v=uiuoNToeMFE&list=PLiMWaCMwGJXnKY6XmeifEpjIfkWRo9v2l&index=3&ab_channel=AntonPutra

- K8s control plane:
  - Etcd database to store state, e.g. all the K8s manifests. It is important to regularly backup the etcd database for recovery.
  - Scheduler - its main job is to watch for newly created pods and assign them to the nodes based on available CPU and memory and pod requests.
  - Controller Manager - combines many different controllers, it manages the reconciliation loop
  - (Legacy) Cloud Manager Controller - its part of the K8s source code but only receives security and bug fixes from cloud providers, e.g. AWS, Azure, GCP. For example, a K8s Service with type "LoadBalancer" would provision an ALB in AWS for the deployment. Nowadays, cloud providers have developed their own external managers. Later in the tutorial, we'll install AWS Load Balancer Controller to manage the LBs for the K8s cluster.
  - API server - a stateless API server that interacts with all of these components. It includes authentication and authorisation.

- K8s nodes:
  - kubelet - responsible for running containers
  - kube-proxy - a network proxy that allows communication to the pods from the network inside or outside the cluster
  - runtime - the runtime to run the containers, e.g. Docker runtime

- Cluster management:
  - We used to use kops to manage the K8s cluster and ensure it has high-availability, but you'd still need to run kops CLI to manage the cluster. Additionally, with kops we would need to run a virtual network and access management was a nightmare. Without using additional tools like dex, you would have to attach all IAM policies to the K8s nodes and all applications would get the same access which is not good.
  - We can now delegate management of the control plane to cloud providers, e.g. using EKS. EKS clusters only cost 10 cents per hour per cluster.

- IAM roles:
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

- _7-eks.tf_:
  - We create an IAM role per cluster, e.g. `"${local.name_prefix}-${local.eks_name}-eks-cluster"` in case we have multiple clusters per environment.
  - We set up the network settings with `endpoint_public_access=true` to make the cluster publicly accessible.
    - Later in the tutorial, we will see how to set up a private EKS cluster using AWS Client VPN and a private DNS that we can use for ingress and private services that we don't want to expose to the internet, e.g. Grafana dashboards, Temporal UI, etc.
    - Even with a public cluster, the worker nodes will still be deployed on the private subnets without public IP addresses.
  - We specified the two private subnets to place the worker nodes. It is required to have 2 AZs. EKS will create cross-account elastic network interface in these subnets to allow communication between worker nodes and the K8s control plane.
  - We configured authentication to use `API`.
    - We used to have to manage authentication using `aws_auth` ConfigMap in kube system namespace. However, this is deprecated now. It wasn't convenient to manage existing ConfigMaps in K8s using Terraform resources.
    - AWS developed an API that we can use to add users to the cluster. You can still use ConfigMap and even both ConfigMap and API. But the API is highly recommended for user management. In the next section, we cover how to add IAM roles and IAM users to access the cluster.
  - We set the `bootstrap_cluster_creator_admin_permissions=true` to grant the Terraform user admin priveleges. This option defaults to `true` anyway, but we want to be explicit. For us, because we will use Terraform to deploy Helm Charts and plain YAML resources.
- _8-nodes.tf_:
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
