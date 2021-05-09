# requires bash 4.3+

# Multiselect -- select multiple elements from a list of choices
#
# usage: multiselect [-p "Prompt"] responseVarname option ...
#
# example:
#    multiselect -p "Select an option: " response foo bar baz qux blah
#    echo "you selected: ${response[*]}"
#
# notes:
# - if there are duplicates in the given options and one is selected,
#   all duplicates will be selected.
#
multiselect::usage() {
    (($# > 0)) && echo "$@" >&2
    echo "usage: multiselect [-p prompt] varname string [...]" >&2
}

multiselect() {
    local OPTIND OPTARG opt
    local PS3=$PS3
    while getopts :p: opt; do
        case $opt in
            p)  PS3=$OPTARG ;;
            ?)  multiselect::usage "invalid option: -$OPTARG"
                return 1
                ;;
        esac
    done
    shift $((OPTIND - 1))

    if (($# == 0)); then
        multiselect::usage "error: no varname given"
        return 1
    fi
    local -n chosen=$1    # the variable name to hold the chosen elements
    chosen=()
    shift

    if (($# == 0)); then
        multiselect::usage "error: No choices given" 
        return 1
    fi
    local -a options=("$@")
    local -A selected
    local -a choices
    local FS=$'\x1c' choice

    while true; do
        readarray -t choices < <(
            for choice in "${options[@]}"; do
                [[ -v "selected[$choice]" ]] && star='*' || star=' '
                printf '[%s] %s\n' "$star" "$choice"
            done
        )
        choices+=("Done")

        select choice in "${choices[@]}"; do
            [[ -z $choice ]] && continue
            [[ $choice == "Done" ]] && break 2
            choice=${choice#\[?\] }
            if [[ -v "selected[$choice]" ]]; then
                unset "selected[$choice]"
            else
                selected[$choice]=true
            fi
            break
        done
    done

    # return the selections in the original order
    for choice in "${options[@]}"; do
        [[ -v "selected[$choice]" ]] && chosen+=("$choice")
    done
    return 0
}
