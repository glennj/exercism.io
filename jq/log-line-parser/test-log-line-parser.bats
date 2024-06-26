#!/usr/bin/env bats
load bats-extra
load bats-jq

@test error_message {
    ## task 1
    run jq -rn --arg line '[ERROR]: Stack overflow' \
        'include "log-line-parser"; $line | message'
    assert_success
    assert_output 'Stack overflow'
}

@test warning_message {
    ## task 1
    run jq -rn --arg line '[WARNING]: Disk almost full' \
        'include "log-line-parser"; $line | message'
    assert_success
    assert_output 'Disk almost full' 
}

@test info_message {
    ## task 1
    run jq -rn --arg line '[INFO]: File moved' \
        'include "log-line-parser"; $line | message'
    assert_success
    assert_output 'File moved' 
}

@test message_with_leading_and_trailing_space {
    ## task 1
    run jq -rn --arg line $'[WARNING]:   \tTimezone not set  \r\n' \
        'include "log-line-parser"; $line | message'
    assert_success
    assert_output 'Timezone not set'
}

@test message_contains_colon {
    ## task 1
    run jq -rn --arg line '[INFO]: The current time is 12:34:56' \
        'include "log-line-parser"; $line | message'
    assert_success
    assert_output 'The current time is 12:34:56'
}

@test error_log_level {
    ## task 2
    run jq -rn --arg line '[ERROR]: Disk full' \
        'include "log-line-parser"; $line | log_level'
    assert_success
    assert_output 'error' 
}

@test warning_log_level {
    ## task 2
    run jq -rn --arg line '[WARNING]: Unsafe password' \
        'include "log-line-parser"; $line | log_level'
    assert_success
    assert_output 'warning' 
}

@test info_log_level {
    ## task 2
    run jq -rn --arg line '[INFO]: Timezone changed' \
        'include "log-line-parser"; $line | log_level'
    assert_success
    assert_output 'info' 
}

@test error_reformat {
    ## task 3
    run jq -rn --arg line '[ERROR]: Segmentation fault' \
        'include "log-line-parser"; $line | reformat'
    assert_success
    assert_output 'Segmentation fault (error)' 
}

@test warning_reformat {
    ## task 3
    run jq -rn --arg line '[WARNING]: Decreased performance' \
        'include "log-line-parser"; $line | reformat'
    assert_success
    assert_output 'Decreased performance (warning)' 
}

@test info_reformat {
    ## task 3
    run jq -rn --arg line '[INFO]: Disk defragmented' \
        'include "log-line-parser"; $line | reformat'
    assert_success
    assert_output 'Disk defragmented (info)' 
}

@test reformat_with_leading_and_trailing_space {
    ## task 3
    run jq -rn --arg line $'[ERROR]: \t Corrupt disk\t \t \r\n' \
        'include "log-line-parser"; $line | reformat'
    assert_success
    assert_output 'Corrupt disk (error)'
}
