#!/usr/bin/env bats
load bats-extra

@test 'recognizes a command at the first position' {
    ## task 1
    run jq -R '
        include "regular-chatbot";
        is_valid_command
    ' <<< 'Chatbot, Do you understand this command?'
    assert_success
    assert_output true
}

@test 'does not recognize a command if not at the first position' {
    ## task 1
    run jq -R '
        include "regular-chatbot";
        is_valid_command
    ' <<< 'Hey Chatbot, please tell me what is the weather for tomorrow.'
    assert_success
    assert_output false
}

@test 'does not care about UPPERCASE or lowercase' {
    ## task 1
    run jq -R '
        include "regular-chatbot";
        is_valid_command
    ' << END_INPUT
CHATBOT Is it okey if I shout at you?
chatbot - please tell me what is happening here.
END_INPUT
    assert_success
    assert_equal 2 "${#lines[@]}"
    assert_line --index 0 true
    assert_line --index 1 true
}

@test 'does not recognize word characters after «chatbot»' {
    ## task 1
    run jq -R '
        include "regular-chatbot";
        is_valid_command
    ' << END_INPUT
CHATBOT123 how many of you are there?
chatbot_asdf
chatbot^this is ok, caret is a non-word character
END_INPUT
    assert_success
    assert_equal 3 "${#lines[@]}"
    assert_line --index 0 false
    assert_line --index 1 false
    assert_line --index 2 true
}

@test 'removes properly one single emoji encryption' {
    ## task 2
    run jq -Rr '
        include "regular-chatbot";
        remove_emoji
    ' <<< 'What was your name? emoji2134 Sorry I forgot about it.'
    assert_success
    assert_output 'What was your name?  Sorry I forgot about it.'
}

@test 'removes all the emoji encryption' {
    ## task 2
    run jq -Rr '
        include "regular-chatbot";
        remove_emoji
    ' <<< 'emoji5321 How about ordering emoji8921?'
    assert_success
    assert_output ' How about ordering ?'
}

@test 'with no encrypted emojis returns input unchanged' {
    ## task 2
    run jq -Rr '
        include "regular-chatbot";
        remove_emoji
    ' <<< 'hello world emoji 123'
    assert_success
    assert_output 'hello world emoji 123'
}

@test 'recognizes a phone number with the correct format' {
    ## task 3
    run jq -Rr '
        include "regular-chatbot";
        check_phone_number
    ' <<< 'chatbot, phone (+34) 643-876-459 please'
    assert_success
    assert_output 'Thanks! Your phone number is OK.'
}

@test 'recognizes a phone number with another correct format' {
    run jq -Rr '
        include "regular-chatbot";
        check_phone_number
    ' <<< '(+49) 543-928-190'
    assert_success
    assert_output 'Thanks! Your phone number is OK.'
}

@test 'informs the user that it is a wrong phone number format' {
    ## task 3
    run jq -rn '
        include "regular-chatbot";
        "322-787-654" | check_phone_number
    '
    assert_success
    assert_output "Oops, it seems like I can't reach out to 322-787-654."
}

@test 'informs the user that it is another wrong phone number format' {
    ## task 3
    run jq -rn '
        include "regular-chatbot";
        "4355-67-274" | check_phone_number
    '
    assert_success
    assert_output "Oops, it seems like I can't reach out to 4355-67-274."
}

@test 'returns only the link of the website' {
    ## task 4
    run jq -Rc '
        include "regular-chatbot";
        get_domains
    ' <<< 'You can check more info on youtube.com'
    assert_success
    assert_output '["youtube.com"]'
}

@test 'returns only the link of another website' {
    ## task 4
    run jq -Rc '
        include "regular-chatbot";
        get_domains
    ' <<< 'There is a cool website called theodinproject.com to learn from'
    assert_success
    assert_output '["theodinproject.com"]'
}

@test 'no links detected' {
    ## task 4
    run jq -nc '
        include "regular-chatbot";
        "hello Chatbot" | get_domains
    '
    assert_success
    assert_output '[]'
}

@test 'returns a stream of multiple websites links' {
    ## task 4
    run jq -Rc '
        include "regular-chatbot";
        get_domains
    ' <<< 'That was from reddit.com and notion.so'
    assert_success
    assert_output '["reddit.com","notion.so"]'
}

@test 'greets the user by their proper name' {
    ## task 5
    run jq -rn '
        include "regular-chatbot";
        "My name is Pablo and I am six years old" | nice_to_meet_you
    '
    assert_success
    assert_output 'Nice to meet you, Pablo'
}

@test 'greets another user by their proper name' {
    ## task 5
    run jq -rn '
        include "regular-chatbot";
        "Hello Chatbot, my name is Marie-Claire" | nice_to_meet_you
    '
    assert_success
    assert_output 'Nice to meet you, Marie-Claire'
}

@test 'simple csv parsing' {
    ## task 6
    run jq -Rc '
        include "regular-chatbot";
        parse_csv
    ' << END_INPUT
a, b,c
11,  22,		33,44
END_INPUT
    assert_success
    assert_equal 2 "${#lines[@]}"
    assert_line --index 0 '["a","b","c"]'
    assert_line --index 1 '["11","22","33","44"]'
}
