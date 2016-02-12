#!/bin/bash
if [ -z "${1}" ]; then
	echo "Please provide tag version"
	exit
fi

if [ -z "${2}" ]; then
	echo "Please provide commit message"
	exit
fi

perl -pe "s/{rel, \"cinched\", \"[^\"]+\",/{rel, \"cinched\", \"${1}\",/" -i rel/reltool.config

git add -A .
git commit -m "${2}"
git push
git tag -d v${1}
git push origin :refs/tags/v${1}
git tag -a v${1} -m "${1} release"
git push --tags

