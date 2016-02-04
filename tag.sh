#!/bin/bash
if [ -z "${1}" ]; then
	echo "Please provide tag version"
	exit
fi
git add -A .
git commit -m "wip"
git push
git tag -d v${1}
git push origin :refs/tags/v${1}
git tag -a v${1} -m "${1} release"
git push --tags

