Deploy the CloudFormation stack. If you
do not have your domain set up as a Route53 hosted zone, set
CreateDns=no and create DNS records manually for:
1. Domain verification for the ACM cert (during deployment)
2. Pointing the subdomain to CloudFront (after deployment)
```
cd infra

STACK=<your CloudFormation stack name>
DOMAIN=<your domain name>
SUBDOMAIN=<your subdomain e.g. www>
aws --region us-east-1 cloudformation deploy \
  --stack-name $STACK \
  --template-file template.yaml \
  --capabilities CAPABILITY_NAMED_IAM CAPABILITY_AUTO_EXPAND \
  --parameter-overrides DomainName=$DOMAIN SubDomain=$SUBDOMAIN CreateApex=yes CreateDns=no

echo "Create the following DNS record to point the site to AWS:"
aws --region us-east-1 cloudformation describe-stacks \
  --stack-name $STACK \
  --query "Stacks[0].Outputs[?OutputKey=='DNSRecord'].OutputValue" --output text
```

Deploy the site files:
```
export S3_BUCKET=`aws --region us-east-1 cloudformation describe-stacks \
  --stack-name $STACK \
  --query "Stacks[0].Outputs[?OutputKey=='S3BucketRoot'].OutputValue" --output text`
export CF_DIST=`aws --region us-east-1 cloudformation describe-stacks \
  --stack-name $STACK \
  --query "Stacks[0].Outputs[?OutputKey=='CloudFrontDistribution'].OutputValue" --output text`

cd ..
script/deploy-aws.sh
```

And browse!
```
URL=https://`aws --region us-east-1 cloudformation describe-stacks \
  --stack-name $STACK \
  --query "Stacks[0].Outputs[?OutputKey=='CloudFrontDomainName'].OutputValue" --output text`
open $URL
```

## With nested stacks and lambda

https://github.com/shoover/amazon-cloudfront-secure-static-site/tree/no-r53

Notes to package the template if using nested stack references and/or lambda
function:
```
OPS_BUCKET=<your bucket to store templates and lambda code>
aws s3 mb --region us-east-1 s3://$OPS_BUCKET

make package-function

aws --region us-east-1 cloudformation package \
    --template-file template.yaml \
    --s3-bucket $OPS_BUCKET \
    --output-template-file packaged.template

aws --region us-east-1 cloudformation deploy \
    --stack-name $STACK \
    --template-file packaged.template \
    --capabilities CAPABILITY_NAMED_IAM CAPABILITY_AUTO_EXPAND \
    --parameter-overrides DomainName=$DOMAIN SubDomain=$SUBDOMAIN CreateApex=yes CreateDns=no

# If initial deployment fails and you need to iterate, delete the stack
aws --region us-east-1 cloudformation delete-stack \
    --stack-name $STACK
```
