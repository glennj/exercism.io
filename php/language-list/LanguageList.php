<?php

function language_list(...$languages)
{
    return $languages;
}

function add_to_language_list($list, $language)
{
    return [...$list, $language];
}

function prune_language_list($list)
{
    return array_slice($list, 1);
}

function current_language($list)
{
    return $list[0];
}

function language_list_length($list)
{
    return count($list);        
}