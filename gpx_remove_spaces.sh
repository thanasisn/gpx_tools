#!/bin/bash
## created on 2020-10-26

####  Reduce xml file size by removing spaces tabs and newlines.
##  This helps with sites that limit upload size of files.
##  Only \n we assume Unix line endings.

infile="$1"

extension="${infile##*.}"
filename="${infile%.*}"

outfile="${filename}_folded.${extension}"


if [ ! -f "$infile" ]; then
    echo "$infile is not a file"
    exit 1
fi

sed 's/^[ \t]*//' "$infile" |\
tr -d '\n' > "$outfile"

echo "In file:     $infile"
echo "Output file: $outfile"

stat --printf="%s\n" "$infile"
stat --printf="%s\n" "$outfile"

exit 0
