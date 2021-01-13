#!/bin/bash
## created on 2015-09-10

#### convert gdb to gpx

folderIN="$1"
folderOUT="$2"

if [[ -d $folderIN ]]; then
    echo "valid folder"
else
    echo "NOT VALID FOLDER!"
    exit
fi

if [[ -d $folderOUT ]]; then
    echo "valid folder"
else
    echo "NOT VALID FOLDER!"
    exit
fi

find "$folderIN" -type f -iname "*.gdb" | sed  's,'"$folderIN"',,' | while read line;do
    echo "${folderIN}${line}"
    mkdir -p "$(dirname "${folderOUT}${line}")"

    gpsbabel -w -r -t -i gdb -f - -o gpx -F - <"${folderIN}${line}" >"${folderOUT}${line%.*}.gpx"
done

exit 0
