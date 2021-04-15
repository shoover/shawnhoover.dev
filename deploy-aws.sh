[[ -z "$S3_BUCKET" ]] && { echo "S3_BUCKET is required" ; exit 1; }
[[ -z "$CF_DIST" ]] && { echo "CF_DIST is required" ; exit 1; }

aws s3 sync . s3://$S3_BUCKET/ \
    --exclude "*" \
    --include "*.html" \
    --include "*.css" \
    --include "*.ttf" \
    --include "*.pdf" \
    --include "*.png" \
    --include "*.ico" \
    --include "*.webmanifest"

aws cloudfront create-invalidation --distribution-id $CF_DIST --paths "/*"
