#!/bin/bash

if [[ $# -ne 1 ]]; then
    echo "Usage: ./error_handling <greetee>" >&2
    exit 1
fi

echo "Hello, $1"
