#!/bin/sh

if [ "$#" -ne 1 ]; then
    OUT_FORMAT="png"
else
    OUT_FORMAT="$1"
fi

# script is path sensitive
# find way for it not to be
DOT_GRAPHS="$(find . -iname "*.dot")"
for graph in $DOT_GRAPHS; do
    graph_out="$(echo "$graph" | sed "s/\.dot/.$OUT_FORMAT/")"
    dot "$graph" -T"$OUT_FORMAT" > "$graph_out"
done