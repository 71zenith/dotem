#!/bin/sh

epubs=$(fd -e=epub . ~/kindle/)
IFS="
"
for file in $epubs; do
  image="${file%/*}/cover.jpg"
  label="${file##*/}"
  echo "label=${label%.*};image=$image;exec=zathura \"$file\""
done
