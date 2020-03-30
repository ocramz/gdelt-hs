#!/bin/bash
FILE=$1
while read LINE; do
    echo "(string \""$LINE"\" $> " $LINE ") <|>"
done < $FILE
