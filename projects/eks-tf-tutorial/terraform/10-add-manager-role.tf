data "aws_caller_identity" "current" {}

resource "aws_iam_role" "eks_admin" {
  name = "${local.name_prefix}-${local.eks_name}-eks-admin"

  assume_role_policy = jsonencode({
    Version = "2012-10-17",
    Statement = [
      {
        Effect = "Allow",
        Action = "sts:AssumeRole",
        Principal = {
          AWS = "arn:aws:iam::${data.aws_caller_identity.current.account_id}:root"
        }
      }
    ]
  })
}

resource "aws_iam_policy" "eks_admin" {
  name  = "AmazonEKSAdminPolicy"
  policy = jsonencode({
    Version = "2012-10-17",
    Statement = [
      {
        Effect = "Allow",
        Action = [
          "eks:*"
        ],
        Resource = "*"
      },
      {
        Effect = "Allow",
        Action = "iam:PassRole",
        Resource = "*",
        Condition = {
          StringEquals = {
            "iam:PassedToService": "eks.amazonaws.com"
          }
        }
      }
    ]
  })
}

resource "aws_iam_role_policy_attachment" "eks_admin" {
  role = aws_iam_role.eks_admin.name
  policy_arn = aws_iam_policy.eks_admin.arn
}

resource "aws_iam_user" "manager" {
  name = "manager"
}

resource "aws_iam_policy" "eks_assume_admin" {
  name = "AmazonEKSAssumeAdminPolicy"

  policy = jsonencode({
    Version = "2012-10-17",
    Statement = [
      {
        Effect = "Allow",
        Action = "sts:AssumeRole",
        Resource = aws_iam_role.eks_admin.arn
      }
    ]
  })
}

# Best practice: instead attach policy to IAM group and put `manager` in group
resource "aws_iam_user_policy_attachment" "manager" {
  user = aws_iam_user.manager.name
  policy_arn = aws_iam_policy.eks_assume_admin.arn
}

# Best practice: using IAM roles due to temporary credentials
resource "aws_eks_access_entry" "manager" {
  cluster_name = aws_eks_cluster.eks.name
  principal_arn = aws_iam_user.manager.arn
  kubernetes_groups = [ "my-admin" ]
}