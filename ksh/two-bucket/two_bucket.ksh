#!/usr/bin/env ksh
#
# Object Oriented programming in ksh
#
# https://blog.fpmurphy.com/2010/05/ksh93-using-types-to-create-object-orientated-scripts.html

source ./math_functions.ksh

function die { print -u2 "$*"; exit 1; }
function assertArith { (($1)) || die "$2"; }
function assertTest  { eval "[[ $1 ]]" || die "$2"; }

[[ $KSH_VERSION == *93* ]] || die "ksh version 93 required"

############################################################
# Define a "class"
#
# ksh uses `_` as "self" or "this"
#
typeset -T Bucket=(
    integer size=0
    integer amount=0
    typeset name="aBucket"

    function isFull    { ((_.amount == _.size)); }
    function isEmpty   { ((_.amount == 0)); }
    function available { print $((_.size - _.amount)); }

    function fill      { _.amount=${_.size}; }
    function empty     { _.amount=0; }

    function pourInto {
        nameref other=$1
        integer amt=$((min(_.amount, $(other.available))))
        ((_.amount -= amt))
        ((other.amount += amt))
        return
    }

    function toString {
        printf 'Bucket %s; size=%d; amount=%d' \
            "${_.name}" ${_.size} ${_.amount}
    }
)

############################################################
typeset -T Solver=(
    Bucket one two
    integer goal
    integer moves=0
    typeset startBucket=""

    function initialize {
        _.validate "$@"

        _.one.name='one'
        _.one.size=$1

        _.two.name='two'
        _.two.size=$2

        _.goal=$3
        _.startBucket=$4
    }

    function validate {
        integer a=$1 b=$2 goal=$3
        typeset startName=$4

        # the "." causes the function to execute in the context
        # of *this* function, allowing variables to be shared.
        . assertArith 'goal <= max(a, b)' 'invalid goal: too big'

        integer gcd=$((gcd(a, b)))
        . assertArith 'gcd == 1 || goal % gcd == 0' 'invalid goal: unsatisfiable'

        . assertTest '$startName == @(one|two)' 'invalid start bucket'
    }

    function run {
        case ${_.startBucket} in
            one) _.solve _.one _.two ;;
            two) _.solve _.two _.one ;;
        esac
    }

    function solve {
        nameref first=$1 second=$2

        first.fill
        ((_.moves += 1))

        if ((_.goal == ${second.size})); then
            second.fill
            ((_.moves += 1))
        fi

        while true; do
            if ((_.goal == ${first.amount})); then
                _.result first second
                return
            fi
            if ((_.goal == ${second.amount})); then
                _.result second first
                return
            fi

            if   first.isEmpty; then first.fill
            elif second.isFull; then second.empty
                                else first.pourInto second
            fi

            ((_.moves += 1))
        done
    }

    function result {
        nameref a=$1 b=$2
        printf 'moves: %d, goalBucket: %s, otherBucket: %s\n' \
            ${_.moves} "${a.name}" ${b.amount}
    }
)

############################################################
Solver solver
solver.initialize "$@"
solver.run
