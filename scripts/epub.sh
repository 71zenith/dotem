#!/bin/sh
epubs=$(fd -e=epub . ~/kindle/)
IFS="
"
open() {
  file=$(cat -)
  [ -n "$file" ] && zathura "$file.epub"
}
for i in $epubs; do
  image="$(dirname "$i")/cover.jpg"
  echo -en "${i%.epub}\0icon\x1f$image\n"
done | rofi -i -dmenu -display-column-separator "/" -display-columns 7 | open
