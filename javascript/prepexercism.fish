if not set -q argv[1]
    echo "usage: "(status filename)" dirname/"
else
    if not test -d $argv[1]
        echo "no such dir: $argv[1]"
    else
        cd $argv[1]
        touch (awk -F "'" 'FNR == 1 {sub(/\.js$/, "", $2); print $2}' *.spec.js)".js"
        npm install
    end
end
