#!/bin/bash

# Assets in content/assets/ext are ignored by git and the deploy script,
# allowing mixing versioned and unversioned assets. This script deploys the
# unversioned assets.

set -e
set -o pipefail
set -x

[[ -z "$S3_BUCKET" ]] && { echo "S3_BUCKET is required. `source ~/.env_deploy_main`" ; exit 1; }
[[ -z "$CF_DIST" ]] && { echo "CF_DIST is required" ; exit 1; }

# Middle ground TTL for HTML and assets
TTL=86400

rm -f sync.log
touch sync.log

# Sync external assets to S3. Capture output for CloudFront invalidation. Live
# with syncing all files every time in CI (mtime is always newer on fresh
# clones); --size-only for text files would skip nominal CSS changes.
aws s3 sync content/assets/ext s3://$S3_BUCKET/assets/ext \
    --delete \
    --size-only \
    --no-progress \
    --cache-control max-age=$TTL \
  | tee --append sync.log

# Invalidate cache for synced files
sed -E -n -e "s/^(upload: .+ to|delete:) s3:\/\/$S3_BUCKET\/(.+)$/\/\2/p" sync.log \
  | sed -e 's/ /%20/g' \
  | tee upload-matches.log \
  | xargs --no-run-if-empty --delimiter="\n" --exit -t aws cloudfront create-invalidation --distribution-id $CF_DIST --paths

rm sync.log upload-matches.log
