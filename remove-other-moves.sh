echo "RUN THIS MANUALLY!  It's not yet been made safe".

exit 1

for folder in $(ls -1); do
    for round in $(ls -1 "$folder"); do
        rm -rf "$folder/$round/B - nothing"
        rm "$folder/$round/GlobalState.json"
    done
done
