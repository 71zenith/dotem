#!/bin/sh
playerctl metadata mpris:artUrl -F -p spotify_player | while read -r url; do
    if [ -n "$url" ]; then
        curl "$url" > /tmp/cover.jpg
        pkill -RTMIN+8 waybar
    fi
done
