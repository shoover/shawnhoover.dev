# Just a website

Publishing a static site in the cloud era *could* still be as simple
as writing HTML and CSS and rsyncing it to a shared host, but there
are hosting costs to avoid and https complexity that has to be managed
regardless. You can run a static site on an AWS free plan, but there
are different ways and so many settings.

This is a static site with the following:
- fast enough hosting via S3 and CloudFront
- works on the AWS free plan
- https-only with certificate
- optional CloudFormation template to spin up the infrastructure with
  minimal manual configuration
- content deployment via AWS S3 CLI

## Manual infrastructure deployment

To bootstrap S3 and CloudFront manually, [How do I use CloudFront to serve a
static website hosted on Amazon S3?](https://aws.amazon.com/premiumsupport/knowledge-center/cloudfront-serve-static-website/):
*Using a REST API endpoint as the origin, with access restricted by an
OAI* works fine, but some details always get me and make it take
longer than it should. I list the basic steps point out common
gotchas. To avoid this altogether, consider skipping ahead to use the
CloudFormation template.

Create the web site bucket. Specifying region us-east-1 saves time and
headaches for CloudFront to resolve the REST endpoint of the new
bucket. Otherwise it counterintuitively returns 307 redirects for an
hour or so.

```sh
aws s3 mb --region us-east-1 s3://<your new bucket name>
```

Request the certificate for the custom domain name in ACM. Be sure to
complete the domain verification part and do this *before* clicking to
create a new CloudFront distribution. Otherwise the certificate will
not be available in the dropdown.

Create the CloudFront distribution.
- Select the bucket as origin
- Restrict bucket access. Create a new identify and update bucket
  permissions.
- Fill in your site details and select the certificate from ACM

## Infrastructure deployment via CloudFormation

This project includes a template `templates/static-site.yaml` to
create the S3 bucket, CloudFront distribution, and certificate with
reasonable defaults reasonably automatically. The template is derived
from sources in [Amazon CloudFront Secure Static
Website](https://github.com/aws-samples/amazon-cloudfront-secure-static-site).

Simply deploy a new stack using the template and parameter overrides.
If you do not have your domain set up as a Route53 hosted zone, set
CreateDns=no. You'll need to create DNS records manually for:
1. Domain verification for the ACM cert (during deployment)
2. Pointing the subdomain to CloudFront (after deployment)

```sh
STACK=<your CloudFormation stack name>
DOMAIN=<your domain name>
SUBDOMAIN=<your subdomain e.g. www>

aws --region us-east-1 cloudformation deploy \
  --stack-name $STACK \
  --template-file infra/static-site.yaml \
  --capabilities CAPABILITY_NAMED_IAM CAPABILITY_AUTO_EXPAND \
  --parameter-overrides DomainName=$DOMAIN SubDomain=$SUBDOMAIN CreateApex=yes CreateDns=no

# Deployment will block on certificate DNS validation. Go to the ACM
# console to get the validation record details. Create the required
# record at your DNS provider.

# Once deployment completes, point the domain to CloudFront
echo "Create the following record at your DNS provider to point the site to AWS:"
aws --region us-east-1 cloudformation describe-stacks \
  --stack-name $STACK \
  --query "Stacks[0].Outputs[?OutputKey=='DNSRecord'].OutputValue" --output text
```

## Deploy site files

```sh
export S3_BUCKET=`aws --region us-east-1 cloudformation describe-stacks \
  --stack-name $STACK \
  --query "Stacks[0].Outputs[?OutputKey=='S3BucketRoot'].OutputValue" --output text`
export CF_DIST=`aws --region us-east-1 cloudformation describe-stacks \
  --stack-name $STACK \
  --query "Stacks[0].Outputs[?OutputKey=='CloudFrontDistribution'].OutputValue" --output text`

script/deploy-aws.sh
```

And browse!
```
URL=https://`aws --region us-east-1 cloudformation describe-stacks \
  --stack-name $STACK \
  --query "Stacks[0].Outputs[?OutputKey=='CloudFrontDomainName'].OutputValue" --output text`
open $URL
```

## Secure static site

CloudFront cannot add response headers on its own. To serve a static
site with security headers, you need to deploy a lambda function to
add the headers on every origin response. See [Amazon CloudFront
Secure Static
Website](https://github.com/aws-samples/amazon-cloudfront-secure-static-site).
My [no-r53
branch](https://github.com/shoover/amazon-cloudfront-secure-static-site/tree/no-r53)
removes the Route53 requirement and adds a few tweaks.

## Private static site

[Authorization@Edge using
cookies](https://github.com/aws-samples/cloudfront-authorization-at-edge)
is a solution for private static site hosting on CloudFront/S3 and
authentication via Cognito. It can be yours for the low, low
complexity of a dozen lambda functions and 2000 lines of
CloudFormation. Thankfully the CloudFormation stack is available on
the Marketplace and works fine, including optimizations for both static
HTML and SPA modes.

To deploy a static site:

1. Deploy from the Marketplace with parameter tweaks:
   - Set `EnableSpa` to `false`
   - Set the "other" domain name to your domain+subdomain
   - Change HttpHeaders to add `font-src 'self'` if you will host font files
2. In the console, request a certificate via ACM and add your domain name to
   the CloudFront distribution.
3. Upload your content to S3.
