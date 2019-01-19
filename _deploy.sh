
#!/bin/sh

set -e

[ -z "${GITHUB_PAT}" ] && exit 0
[ "${TRAVIS_BRANCH}" != "master" ] && exit 0

git config --global user.email "declaredesignbot@users.noreply.github.com"
git config --global user.name "declaredesignbot"

git clone -b gh-pages https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git book-output

cd book-output

cp -r ../_book/* ./

git add --all *

git commit -m "Update the book" || true

git push origin gh-pages
