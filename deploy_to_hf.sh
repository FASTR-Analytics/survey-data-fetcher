#!/bin/bash
# Deploy Survey Data Fetcher to HuggingFace
# Usage: ./deploy_to_hf.sh "Your commit message"

set -e  # Exit on error

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Paths
DEV_DIR="/Users/claireboulange/Desktop/modules/survey_data_fetcher"
DEPLOY_DIR="/Users/claireboulange/Desktop/hf-clean"

# Check if commit message provided
if [ -z "$1" ]; then
    echo -e "${YELLOW}Usage: ./deploy_to_hf.sh \"Your commit message\"${NC}"
    exit 1
fi

COMMIT_MSG="$1"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Deploying to HuggingFace${NC}"
echo -e "${BLUE}========================================${NC}"

# Step 1: Ensure we're in dev directory
cd "$DEV_DIR"
echo -e "${GREEN}✓${NC} Working from: $DEV_DIR"

# Step 2: Sync files to deployment repo
echo -e "${BLUE}→${NC} Syncing files to deployment repo..."
rsync -av --delete \
  --exclude='.git' \
  --exclude='.Rproj.user' \
  --exclude='*.Rproj' \
  --exclude='.RData' \
  --exclude='.Rhistory' \
  --exclude='test_*.R' \
  --exclude='deploy_to_hf.sh' \
  --exclude='DEPLOY.md' \
  --exclude='*.log' \
  --exclude='.DS_Store' \
  "$DEV_DIR/" "$DEPLOY_DIR/"

echo -e "${GREEN}✓${NC} Files synced"

# Step 3: Commit and push to HuggingFace
cd "$DEPLOY_DIR"
echo -e "${BLUE}→${NC} Committing changes..."

# Check if there are changes
if git diff --quiet && git diff --cached --quiet; then
    echo -e "${YELLOW}No changes to deploy${NC}"
    exit 0
fi

git add .
git commit -m "$COMMIT_MSG"
echo -e "${GREEN}✓${NC} Changes committed"

echo -e "${BLUE}→${NC} Pushing to HuggingFace..."
git push
echo -e "${GREEN}✓${NC} Pushed to HuggingFace"

echo -e "${BLUE}========================================${NC}"
echo -e "${GREEN}✓ Deployment complete!${NC}"
echo -e "${BLUE}========================================${NC}"
echo -e "View your Space at:"
echo -e "${GREEN}https://huggingface.co/spaces/CIJBoulange/survey-data-fetcher${NC}"
echo -e ""
echo -e "Build logs:"
echo -e "${GREEN}https://huggingface.co/spaces/CIJBoulange/survey-data-fetcher/logs${NC}"
