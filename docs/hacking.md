# How to Hack CALM

## Step 1: Fork CALM

https://github.com/VitoVan/calm/fork

## Step 2: Install CALM binary

https://github.com/VitoVan/calm#-installation

## Step 3: Init Git Repository

```bash
# cd to your downloaded CALM binary directory
# e.g. on macOS:
cd /Applications/Calm.app/Contents/MacOS

# init git
git init .

# add your fork to remote
git remote add origin git@github.com:<your-name>/calm.git

# fetch
git fetch origin --depth=1

# reset hard to origin/main
git reset --hard origin/main
```

Now you can modify, push and create pull requests!
