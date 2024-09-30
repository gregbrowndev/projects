provider "aws" {
  region = local.region

  default_tags {
    tags = {
      Terraform     = true
      Project = local.project
      Environment   = local.env
    }
  }
}

terraform {
  required_version = ">= 1.0"

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.49"
    }
  }
}
