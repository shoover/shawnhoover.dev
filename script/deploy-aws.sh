#!/bin/bash

set -e
set -o pipefail
set -x

[[ -z "$S3_BUCKET" ]] && { echo "S3_BUCKET is required" ; exit 1; }
[[ -z "$CF_DIST" ]] && { echo "CF_DIST is required" ; exit 1; }

# Middle ground TTL for HTML and assets
TTL=86400

rm -f sync.log
touch sync.log

# Sync content to S3. Capture output for CloudFront invalidation. Live with
# syncing all files every time in CI (mtime is always newer on fresh clones);
# --size-only for text files would skip nominal CSS changes.
aws s3 sync build s3://$S3_BUCKET \
    --exclude 'assets/*' \
    --include 'assets/styles/*' \
    --delete \
    --no-progress \
    --cache-control max-age=$TTL \
  | tee --append sync.log

# Sync HTML indexes suitable for CloudFront. 'notes/index.html' becomes just 'notes'.
# This command is borrowed from https://github.com/brandur/sorg/blob/master/Makefile.
TARGET_DIR=build
find ${TARGET_DIR} -name index.html | egrep -v "${TARGET_DIR}/index.html" | sed "s|^${TARGET_DIR}/||" | xargs -I{} -n1 dirname {} | xargs -I{} -n1 aws s3 cp ${TARGET_DIR}/{}/index.html s3://${S3_BUCKET}/{} --cache-control max-age=${TTL} --content-type text/html \
  | tee --append sync.log

# Sync assets. Ignore externally managed assets. Styles are covered with the previous command.
aws s3 sync build/assets s3://$S3_BUCKET/assets \
    --exclude 'ext/*' \
    --exclude 'styles/*' \
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
