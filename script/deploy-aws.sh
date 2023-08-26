#!/bin/bash

set -e
set -o pipefail
set -x # debug

[[ -z "$S3_BUCKET" ]] && { echo "S3_BUCKET is required" ; exit 1; }
[[ -z "$CF_DIST" ]] && { echo "CF_DIST is required" ; exit 1; }

# It's a static static site
TTL=604800

aws sts get-caller-identity

# Sync content to S3. Display output and capture for CloudFront invalidation.
# Annoyingly, this syncs all files in CI. Live with it; I don't want to risk
# using --size-only for text files.
aws s3 sync site s3://$S3_BUCKET \
    --delete \
    --no-progress \
    --cache-control max-age=$TTL \
  | tee sync.log

# Invalidate cache for synced files
sed -E -n "s/^upload: site\/(.+) to s3:\/\/$S3_BUCKET\/.*$/\1/p" sync.log \
  | tee upload-matches.log \
  | xargs -I {} -t aws cloudfront create-invalidation --distribution-id $CF_DIST --paths /{}

rm sync.log upload-matches.log
