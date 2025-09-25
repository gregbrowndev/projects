// Note: latest version at time of writing: v1.3.2-eksbuild.2
// See:
// aws eks describe-addon-versions --region eu-west-2 --addon-name eks-pod-identity-agent
resource "aws_eks_addon" "pod_identity" {
  cluster_name  = aws_eks_cluster.eks.name
  addon_name    = "eks-pod-identity-agent"
  addon_version = "v1.2.0-eksbuild.1"
}
