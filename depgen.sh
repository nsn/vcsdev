#!/bin/sh
for f; do echo "${f/\.asm/.bin}: " $(grep -i include $f | grep -v "vcs\.h\|macro\.h" | awk '{print $2}' | sed -e 's/"\(.*\)"/\1/g'); done


