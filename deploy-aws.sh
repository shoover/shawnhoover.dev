set -e

[[ -z "$S3_BUCKET" ]] && { echo "S3_BUCKET is required" ; exit 1; }
[[ -z "$CF_DIST" ]] && { echo "CF_DIST is required" ; exit 1; }

# It's a static static site
TTL=604800

# Sync all assets to S3. Display output and capture for CloudFront invalidation.
aws s3 sync . s3://$S3_BUCKET/ \
    --no-progress \
    --cache-control max-age=$TTL \
    --exclude "*" \
    --include "*.html" \
    --include "*.css" \
    --include "*.ttf" \
    --include "*.pdf" \
    --include "*.png" \
    --include "*.ico" \
    --include "*.webmanifest" \
  | tee sync.log

# Invalidate cache for synced assets
sed -E -n "s/^upload: .\/(.+) to s3:\/\/$S3_BUCKET\/.*$/\1/p" sync.log \
  | tee upload-matches.log \
  | xargs -I {} -t aws cloudfront create-invalidation --distribution-id $CF_DIST --paths /{}

rm sync.log upload-matches.log
