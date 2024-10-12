locals {
  project     = "eks-tutorial"
  env         = "dev"
  name_prefix = "${local.project}-${local.env}"

  region      = "eu-west-2"
  zone1       = "eu-west-2a"
  zone2       = "eu-west-2b"
  eks_name    = "demo"
  eks_version = "1.29"
}
