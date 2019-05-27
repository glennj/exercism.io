set -o errexit
set -o nounset
# set -x

assignVal(){
    (( $1 == 1 )) && { echo "1 bottle"; return 0; }
    (( $1 == 0 )) && { echo "no more bottles"; return 0; }
    (( $1 < 0 )) && { echo "99 bottles"; return 0; }

    echo "$1 bottles"
}

singVerse(){
    local top=$(assignVal $1)
    local bottom=$(assignVal $(( $1-1 )) )
    local first="$top of beer on the wall, $top of beer."
    
    echo $(tr '[:lower:]' '[:upper:]' <<< "${first:0:1}")${first:1}

    case "$bottom" in
	"no more bottles")
	    echo "Take it down and pass it around, $bottom of beer on the wall."
	    ;;
	"99 bottles")
	    echo "Go to the store and buy some more, $bottom of beer on the wall."
	    ;;
	*)
	    echo "Take one down and pass it around, $bottom of beer on the wall."
	    ;;
    esac
}
main(){

    case "${#@}" in
	1)
	    singVerse $1
	    ;;
	2)
	    local -i start=$1 end=$2
	    
	    (( $start <= $end )) && { echo "Start must be greater than End"; exit 255; }

	    IFS=''
	    #   echo $(for (( i=$start; i>=$end; i-- )); do singVerse $i; echo ""; done)
	    for (( i=$start; i>=$end; i-- )); do singVerse $i; echo ""; done
	    ;;
	*)
	    echo "1 or 2 arguments expected" && exit 255
	    ;;
    esac
}

main "$@"
