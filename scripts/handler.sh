#!/bin/sh
paste="$(wl-paste)"
types="$(wl-paste --list-types)"

case "$types" in
    *SAVE_TARGETS* | *COMPOUND_TEXT* )
        yt-dlp -f "best[ext=mp4]" --embed-thumbnail --force-overwrites "$paste" --paths "/tmp" --output post.mp4
        wl-copy -t text/uri-list "file:///tmp/post.mp4"
        ;;
    *image* )
        cat "$paste" | tesseract5 -l eng+jpn+jpn_vert - - | wl-copy 
        ;;
    *text* )
        echo "$paste" |  trans -no-warn -no-autocorrect -b -t en | wl-copy
        ;;
esac
