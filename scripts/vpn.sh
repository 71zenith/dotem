#!/bin/sh
menu() {
    all="$(systemctl list-unit-files --type=service --all | sed -nE 's/^(wg-quick.+).service.*/\1/p')"
    echo "$all" | rofi -dmenu -mesg "$1"
}
act() {
    [ -z "$1" ] && exit 1
    [ "$1" = "$2" ] && sudo systemctl stop "$1"
    [ -n "$2" ] && sudo systemctl stop "$2"
    sudo systemctl start "$1"
    exit 0
}

if [ -z "$(sudo wg)" ]; then
    act "$(menu "OFF")"
else
    cur=$(systemctl list-units --type=service | sed -nE 's/(wg-quick.+).service.*/\1/p' | tr -d ' ')
    act "$(menu "ON $cur")" "$cur"
fi
