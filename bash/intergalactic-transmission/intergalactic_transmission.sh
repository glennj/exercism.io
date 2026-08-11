#!/usr/bin/env bash

source ./utils.bash

# uses `bc` for hex <-> bin conversions

# Convert an space-separated string of hex numbers (each prefixed with "0x")
# to a string of binary digits. 
# Each hex number is converted to a left-padded 8-bit binary number.
# The binary numbers are concatenated in order without separators.
# Example: to_bits "0x01 0x02"  # => 0000000100000010
to_bits () {
    local -u byte_string=$1
    local -a bytes
    read -ra bytes <<< "${byte_string//0X/ }"

    # note to self: set obase _before_ ibase
    { echo "obase=2; ibase=16"; printf '%s\n' "${bytes[@]}"; } \
    | bc \
    | while IFS= read -r bits; do
        printf '%08d' "$bits"
      done
}

# Convert a string of binary digits to a decimal number.
bin2dec () { echo "ibase=2; $1" | bc; }

# Convert a string of binary digits to a hex number, prefixed with "0x"
bin2hex () { printf '0x%02x\n' "$(bin2dec "$1")"; }

# Determine the parity value of a string of binary digits.
parity_bit () {
    local ones=${1//0/}
    echo $(( ${#ones} & 1 ))
}

check_parity () {
    [[ ${1:7} == "$(parity_bit "${1:0:7}")" ]]
}

#########################################################################
transmit_sequence () {
    local msg_bits bits7 byte
    local -a result

    msg_bits=$(to_bits "$1")
    while [[ -n $msg_bits ]]; do
        if (( ${#msg_bits} < 7 )); then
            # add padding bits
            msg_bits+="0000000"
            msg_bits=${msg_bits:0:7}
        fi
        bits7=${msg_bits:0:7}
        byte="${bits7}$(parity_bit "$bits7")"
        result+=( "$(bin2hex "$byte")" )
        msg_bits=${msg_bits:7}
    done
    echo "${result[*]}"
}

decode_message () {
    local msg_bits bits8 all_bits
    local -a result

    msg_bits=$(to_bits "$1")
    while [[ -n $msg_bits ]]; do
        bits8=${msg_bits:0:8}
        check_parity "$bits8" || die -s 0 "wrong parity"
        all_bits+=${bits8:0:7}
        msg_bits=${msg_bits:8}
    done

    while (( ${#all_bits} >= 8 )); do
        result+=( "$(bin2hex "${all_bits:0:8}")")
        all_bits=${all_bits:8}
    done
    # any remaining all_bits are the padding zeroes added to the original message.
    echo "${result[*]}"
}

#########################################################################
case "$1" in
    transmit_sequence | decode_message) "$1" "$2" ;;
    *) die "Unknown subcommand $1" ;;
esac
