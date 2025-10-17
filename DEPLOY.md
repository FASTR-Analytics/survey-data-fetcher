# Deployment Guide

## Development Workflow

### 1. Develop & Test Locally

Work in this directory (`/Users/claireboulange/Desktop/modules/survey_data_fetcher`):

```r
# Make your code changes to app.R, R/*.R, etc.

# Test locally
shiny::runApp()
```

### 2. Commit Your Changes

```bash
git add .
git commit -m "Add feature X"
git push  # Push to your main dev repo
```

### 3. Deploy to HuggingFace

Use the deployment script:

```bash
./deploy_to_hf.sh "Deploy: Add feature X"
```

This will:
- ✅ Sync files to `/Users/claireboulange/Desktop/hf-clean`
- ✅ Commit and push to HuggingFace
- ✅ Trigger automatic rebuild (~5 min)
- ✅ Show you the URLs to monitor

### File Syncing

The script automatically syncs:
- ✅ `app.R` - Main app file
- ✅ `R/` - All function files
- ✅ `www/` - CSS and static files
- ✅ `assets/` - Data files

And excludes:
- ❌ `.git/` - Git history
- ❌ `*.Rproj` - RStudio files
- ❌ `test_*.R` - Test files
- ❌ Temporary files

### HuggingFace URLs

- **Live App**: https://huggingface.co/spaces/CIJBoulange/survey-data-fetcher
- **Build Logs**: https://huggingface.co/spaces/CIJBoulange/survey-data-fetcher/logs

## Manual Deployment (if needed)

If the script doesn't work, manual steps:

```bash
# 1. Sync files
rsync -av --exclude='.git' \
  /Users/claireboulange/Desktop/modules/survey_data_fetcher/ \
  /Users/claireboulange/Desktop/hf-clean/

# 2. Deploy
cd /Users/claireboulange/Desktop/hf-clean
git add .
git commit -m "Your message"
git push
```

## Build Time

- First build: ~15-20 minutes
- Subsequent builds: ~5 minutes (with optimized Dockerfile)
- The app auto-restarts after build completes

## Troubleshooting

**Build fails?**
- Check logs at: https://huggingface.co/spaces/CIJBoulange/survey-data-fetcher/logs
- Common issues: missing packages, binary files (use CSV not Excel)

**App crashes?**
- Check runtime logs (same URL as build logs)
- Test locally first with `shiny::runApp()`

**Changes not appearing?**
- Wait for build to complete (~5 min)
- Check if files were synced: `ls -la /Users/claireboulange/Desktop/hf-clean/`
