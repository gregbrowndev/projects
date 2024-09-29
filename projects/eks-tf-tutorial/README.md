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

To get started:

```shell
cd terraform
terraform init
```

Plan:

```shell
AWS_PROFILE=personal terraform plan
```

Apply:

```shell
AWS_PROFILE=personal terraform apply
```

Destroy:

```shell
AWS_PROFILE=personal terraform destroy
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
    - "EKS pod identity" allows fine-grained access to control of pod permissions, e.g. giving access to a pod to read/write to a specific S3 bucket.
    - Before the v3 of Pod Identities, we had to use an OpenID Connect provider and IAM roles for service accounts.
    - We will look at the three separate methods for authentication later in the tutorial.
  - We have to grant EKS access to modify IP address configuration on EKS worker nodes.
    - When we create a pod on K8s, it is assigned an IP from the secondary IP address range assigned to the worker node. We're not using virtual networks like Flannel or Calico,
