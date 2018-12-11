#!/bin/bash

phone=${1//[^0-9]/}

[[ ${#phone} -eq 11 ]] && phone=${phone#1}

if [[ ${#phone} -eq 10 && $phone != [01]* && $phone != ???[01]* ]]; then
    echo "$phone"
else
    echo "Invalid number.  [1]NXX-NXX-XXXX N=2-9, X=0-9" >&2
    status=1
fi
exit $status
