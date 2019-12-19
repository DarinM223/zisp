#!/bin/sh

while inotifywait -qqre modify "./src"; do
    clear
    zig build
done
