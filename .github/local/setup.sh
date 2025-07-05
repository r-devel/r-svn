#!/bin/sh

set -ex

cd $(dirname $0)/../..
rm -f .gitignore
cp .github/local/dot-gitignore .gitignore

cd .git/hooks
rm -f post-checkout
cp ../../.github/local/post-checkout .
