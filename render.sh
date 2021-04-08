#!/bin/sh

# script is path sensitive
# find way for it not to be
DOT_GRAPHS="$(find . -iname "*.dot")"
for graph in $DOT_GRAPHS; do
    graph_svg="$(echo "$graph" | sed "s/\.dot/.svg/")"
    dot "$graph" -Tsvg > "$graph_svg"
done