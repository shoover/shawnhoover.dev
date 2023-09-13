A static site for [my personal landing page](https://shawnhoover.dev)

# Just a website

The site is a bit of static HTML and a an orgmode-generated blog, plus styles
and supporting media assets, hosted on S3 and CloudFront.

In short:

```sh
pip install awscli
export AWS_ACCESS_KEY_ID=...
export AWS_SECRET_ACCESS_KEY=...

export S3_BUCKET=...
export CF_DIST=...
make build
script/deploy-aws.sh
```

Emacs 28+ is also required for the build.

## Orgmode-generated blog

The "Notes" blog is generated from orgmode files using the org project
publishing system. Orgmode is great. The project system is nice and
automatically supports links between files, including converting .org links to
.html when publishing. Sitemap and RSS functionality are more of the DIY nature,
and kind of painful. All the details are in `Makefile` and
`publish/publish-org-dir.el`.

## But also infrastructure

And then there's the 200 lines of YAML, because what's a cloud-era project with
a 1:4 ratio of content development to infrastructure work? This is roughly the
minimum to spin up a static site on S3 and CloudFront with https. I guess it's
easier and cheaper than running a server.

The remaining sections document various outposts on the journey to displaying a
web page on the internet. The first two are fine for this project.

1. Manual S3/CloudFront/ACM provisioning
1. Automate static site deployment with CloudFormation
1. Adding security headers with Lambda@Edge
1. Making a private site authenticated with Cognito

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

This project includes a template `templates/static-site.yaml` to create the S3
bucket, CloudFront distribution, and certificate with reasonable defaults
reasonably automatically. The template is derived from sources in [Amazon
CloudFront Secure Static Website](https://github.com/aws-samples/amazon-cloudfront-secure-static-site).

Simply deploy a new stack using the template and parameter overrides. If you do
not have your domain set up as a Route53 hosted zone, set CreateDns=no. You'll
need to create DNS records manually for:
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
```

Deployment will block on certificate DNS validation. Go to the ACM
console to get the validation record details. Create the required
record at your DNS provider.

Once deployment completes, point the domain to CloudFront. Create the following
record at your DNS provider to point the site to AWS:

```sh
aws --region us-east-1 cloudformation describe-stacks \
  --stack-name $STACK \
  --query "Stacks[0].Outputs[?OutputKey=='DNSRecord'].OutputValue" --output text
```

## Deploy site files

```sh
echo "export S3_BUCKET=`aws --region us-east-1 cloudformation describe-stacks \
  --stack-name $STACK \
  --query \"Stacks[0].Outputs[?OutputKey=='S3BucketRoot'].OutputValue\" --output text`" >> .env_deploy
echo "export CF_DIST=`aws --region us-east-1 cloudformation describe-stacks \
  --stack-name $STACK \
  --query \"Stacks[0].Outputs[?OutputKey=='CloudFrontDistribution'].OutputValue\" --output text`" >> .env_deploy
source .env_deploy
make build
script/deploy-aws.sh
```

And browse!
```
URL=https://`aws --region us-east-1 cloudformation describe-stacks \
  --stack-name $STACK \
  --query "Stacks[0].Outputs[?OutputKey=='CloudFrontDomainName'].OutputValue" --output text`
open $URL
```

## Configure GitHub Actions

Set up GitHub Actions access to AWS as in
[aws-actions](https://github.com/aws-actions/configure-aws-credentials#sample-iam-oidc-cloudformation-template).

1. Deploy the stack below.
2. Go to IAM and create a policy with write access to the bucket and CloudFront invalidation,
   e.g. `infra/www-ci-policy.json`.
3. Create a role linked to the GitHub OIDC provider. Attach the policy.

This template is setup for one GitHub user and repo. To tweak the subject
condition see [example subjects](https://docs.github.com/en/actions/deployment/security-hardening-your-deployments/about-security-hardening-with-openid-connect#example-subject-claims).

```sh
GH_USER=shoover
GH_REPO=$(basename `pwd`)

aws --region us-east-1 cloudformation deploy \
  --stack-name github-aws-credentials \
  --template-file infra/configure-aws-credentials.yaml \
  --capabilities CAPABILITY_NAMED_IAM CAPABILITY_AUTO_EXPAND \
  --parameter-overrides GitHubOrg=$GH_USER RepositoryName=$GH_REPO
```

## Secure static site

CloudFront cannot add response headers on its own (as of April 2021).
To serve a static site with security headers, you have to add a lambda
function to add headers on every origin response. See [Amazon
CloudFront Secure Static
Website](https://github.com/aws-samples/amazon-cloudfront-secure-static-site).
My [no-r53
branch](https://github.com/shoover/amazon-cloudfront-secure-static-site/tree/no-r53)
removes the Route53 requirement, tweaks content-security-policy, and
allows lambda updates.

## Private static site

[Authorization@Edge using
cookies](https://github.com/aws-samples/cloudfront-authorization-at-edge)
is a solution for private static site hosting on CloudFront/S3 with
authentication via Cognito. It can be yours for the low, low upcharge
of a dozen lambda functions and 2000 lines of CloudFormation.
Thankfully the CloudFormation stack is available on the Marketplace
and works fine, including optimizations for both static HTML and SPA
modes.

To deploy a static site:

1. Deploy from the Marketplace with parameter tweaks:
   - Application name: customize it
   - Set `EnableSpa` to `false`
   - HttpHeaders Content-Security-Policy: add `font-src 'self';` if you will host font files
   - AlternateDomainNames: set to your domain, matching what you put
     in the CloudFront distribution
2. In the console, request a certificate via ACM and add your domain name to
   the CloudFront distribution.
3. Upload your content to S3.
