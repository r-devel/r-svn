#!/usr/bin/env bash
#
#
# Git/SVN setup:
# git svn clone -T trunk https://svn.r-project.org/R
# git checkout origin/trunk -b trunk
# git remote add github https://github.com/r-devel/r-svn
# git checkout main
# git fetch github main
# git reset --hard github/main
set -e
set -x
git checkout trunk
git svn rebase
git push github trunk
git checkout main
git fetch github main
git reset --hard github/main
LASTMSG=$(git log main --grep="git-svn-id: https://svn.r-project.org/R/trunk" -n1 | grep -o 'git-svn-id: [^ ]*')
LASTSVN=$(git log trunk --grep="$LASTMSG" -n1 --pretty="%H")
if [ -z "$LASTSVN" ]; then
	echo "FAILED TO FIND LASTSVN"
	exit 1
fi
COMMIT_ARRAY=($(git log $LASTSVN...trunk --reverse --pretty='%H'))
if [ -z "$COMMIT_ARRAY" ]; then
	echo "No new commits found!"
	exit 0
fi
for hash in "${COMMIT_ARRAY[@]}"
do 
	GIT_COMMITTER_DATE="$(git log -1 $hash --pretty=format:%cd)" \
	GIT_COMMITTER_NAME="$(git log -1 $hash --pretty=format:%cn)" \
	GIT_COMMITTER_EMAIL="$(git log -1 $hash --pretty=format:%ce)" \
	git cherry-pick "$hash"
	git push github main
done

# Check that main and trunk are identical except for .github directory
# This verifies that we haven't omitted any commits
git diff --stat main trunk  -- . ':(exclude).github'
