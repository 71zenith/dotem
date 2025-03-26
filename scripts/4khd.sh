#!/bin/sh
player=debug
while [ $# -gt 0 ]; do
  case "$1" in
    -d | --download ) player=download ;;
    -p | --player )
      [ "$#" -lt 2 ] && printf "\033[2K\r\033[1;31m%s\033[0m\n" "missing argument!" && exit 1
      player=$2
      shift
      ;;
    -h | --help)
      printf "%s\n" "${0##*/} -d | --download | -p | --player | -h | --help"
      exit 0
      ;;
    *) query="${query} ${1}";;
  esac
  shift
done
[ -z "$query" ] && printf "%s\n" "${0##*/} -d | --download | -p | --player | -h | --help" && exit 1
for i in $query; do
  html=$(curl -Ls "$i" | tr -d '\0')
  links=$(printf "%s" "$html" | sed -nE 's|^(<p>)?<a href="([^"]*)"><img .*loading="lazy".*|\2|p' | sed 's|.*/-|https://img.4khd.com/-|')
  if ! printf "%s" "$i" | grep -Eq '/[0-9]$' ; then
    pages=$(printf "%s" "$html" | sed -nE 's/<li class="numpages"><a class="page-numbers.*">([^<]*).*/\1/p')

    for j in $pages; do
      extra_links=$(curl -Ls "${i}/${j}" | sed -nE 's|^(<p>)?<a href="([^"]*)"><img .*loading="lazy".*|\2|p' | sed 's|.*/-|https://img.4khd.com/-|')
      links="${links}
      ${extra_links}"
    done
  fi
  links=$(printf "%s\n" "$links" | tr -d ' ')
  case "$player" in
    debug) printf "%s\n" "$links" ;;
    download) printf "%s\n" "$links" | xargs -n1 -P5 curl -O ;;
    mpv) printf "%s\n" "$links" | xargs mpv ;;
  esac
done

