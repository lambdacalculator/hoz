#!/bin/bash
# hoz update script for students

# Colors for output
GREEN='\033[0;32m'
NC='\033[0m' # No Color

echo -e "${GREEN}Checking for hoz updates...${NC}"

if [ -d .git ]; then
    echo "Updating via git..."
    git fetch origin main && git reset --hard origin/main
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}Update successful. Installing...${NC}"
        cabal install exe:hoz --overwrite-policy=always
    else
        echo "Git pull failed. Please check your connection or resolve conflicts."
        exit 1
    fi
else
    echo "This directory is not a git repository."
    echo "To update, please download the latest release from the project website"
    echo "or use 'git clone' to manage your installation."
fi
