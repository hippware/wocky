#!/bin/sh

if [ "${1}" = "help" ]; then
  script=$(basename "${0}")
  echo "Usage: ${script} <version> <previous_commit> <last_commit>"
  echo "  version - Version under which to list the changes"
  echo "  previous_commit - Last commit has from the previous version (will not be included)"
  echo "  last_commit - Last commit hash to include"
  exit 0
fi

version=$1
first_commit=$2
last_commit=$3

log=$(git log --oneline ${previous_commit}..${last_commit} \
    | head -n -1 \
    | grep -v "changelog \\[skip ci\\]" \
    | grep -v " Merge pull request #")

changes="${version}\n\n${log}"

echo -e "${changes}\n\n\n$(cat CHANGELOG.md)" > CHANGELOG.md

echo -e "${changes}"
