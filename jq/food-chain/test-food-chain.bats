#!/usr/bin/env bats

# generated on 2024-07-17T15:57:54Z
load bats-extra
load bats-jq

@test 'fly' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f food-chain.jq << 'END_INPUT'
        {
          "startVerse": 1,
          "endVerse": 1
        }
END_INPUT

    expected="I know an old lady who swallowed a fly.
I don't know why she swallowed the fly. Perhaps she'll die."
    assert_success
    assert_equal "$output" "$expected"
}

@test 'spider' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f food-chain.jq << 'END_INPUT'
        {
          "startVerse": 2,
          "endVerse": 2
        }
END_INPUT

    expected="I know an old lady who swallowed a spider.
It wriggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed the fly. Perhaps she'll die."
    assert_success
    assert_equal "$output" "$expected"
}

@test 'bird' {
    #[[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f food-chain.jq << 'END_INPUT'
        {
          "startVerse": 3,
          "endVerse": 3
        }
END_INPUT

    expected="I know an old lady who swallowed a bird.
How absurd to swallow a bird!
She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed the fly. Perhaps she'll die."
    assert_success
    assert_equal "$output" "$expected"
}

@test 'cat' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f food-chain.jq << 'END_INPUT'
        {
          "startVerse": 4,
          "endVerse": 4
        }
END_INPUT

    expected="I know an old lady who swallowed a cat.
Imagine that, to swallow a cat!
She swallowed the cat to catch the bird.
She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed the fly. Perhaps she'll die."
    assert_success
    assert_equal "$output" "$expected"
}

@test 'dog' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f food-chain.jq << 'END_INPUT'
        {
          "startVerse": 5,
          "endVerse": 5
        }
END_INPUT

    expected="I know an old lady who swallowed a dog.
What a hog, to swallow a dog!
She swallowed the dog to catch the cat.
She swallowed the cat to catch the bird.
She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed the fly. Perhaps she'll die."
    assert_success
    assert_equal "$output" "$expected"
}

@test 'goat' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f food-chain.jq << 'END_INPUT'
        {
          "startVerse": 6,
          "endVerse": 6
        }
END_INPUT

    expected="I know an old lady who swallowed a goat.
Just opened her throat and swallowed a goat!
She swallowed the goat to catch the dog.
She swallowed the dog to catch the cat.
She swallowed the cat to catch the bird.
She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed the fly. Perhaps she'll die."
    assert_success
    assert_equal "$output" "$expected"
}

@test 'cow' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f food-chain.jq << 'END_INPUT'
        {
          "startVerse": 7,
          "endVerse": 7
        }
END_INPUT

    expected="I know an old lady who swallowed a cow.
I don't know how she swallowed a cow!
She swallowed the cow to catch the goat.
She swallowed the goat to catch the dog.
She swallowed the dog to catch the cat.
She swallowed the cat to catch the bird.
She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed the fly. Perhaps she'll die."
    assert_success
    assert_equal "$output" "$expected"
}

@test 'horse' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f food-chain.jq << 'END_INPUT'
        {
          "startVerse": 8,
          "endVerse": 8
        }
END_INPUT

    expected="I know an old lady who swallowed a horse.
She's dead, of course!"
    assert_success
    assert_equal "$output" "$expected"
}

@test 'multiple verses' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f food-chain.jq << 'END_INPUT'
        {
          "startVerse": 1,
          "endVerse": 3
        }
END_INPUT

    expected="I know an old lady who swallowed a fly.
I don't know why she swallowed the fly. Perhaps she'll die.

I know an old lady who swallowed a spider.
It wriggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed the fly. Perhaps she'll die.

I know an old lady who swallowed a bird.
How absurd to swallow a bird!
She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed the fly. Perhaps she'll die."
    assert_success
    assert_equal "$output" "$expected"
}

@test 'full song' {
    [[ $BATS_RUN_SKIPPED == "true" ]] || skip

    run jq -r -f food-chain.jq << 'END_INPUT'
        {
          "startVerse": 1,
          "endVerse": 8
        }
END_INPUT

    expected="I know an old lady who swallowed a fly.
I don't know why she swallowed the fly. Perhaps she'll die.

I know an old lady who swallowed a spider.
It wriggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed the fly. Perhaps she'll die.

I know an old lady who swallowed a bird.
How absurd to swallow a bird!
She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed the fly. Perhaps she'll die.

I know an old lady who swallowed a cat.
Imagine that, to swallow a cat!
She swallowed the cat to catch the bird.
She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed the fly. Perhaps she'll die.

I know an old lady who swallowed a dog.
What a hog, to swallow a dog!
She swallowed the dog to catch the cat.
She swallowed the cat to catch the bird.
She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed the fly. Perhaps she'll die.

I know an old lady who swallowed a goat.
Just opened her throat and swallowed a goat!
She swallowed the goat to catch the dog.
She swallowed the dog to catch the cat.
She swallowed the cat to catch the bird.
She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed the fly. Perhaps she'll die.

I know an old lady who swallowed a cow.
I don't know how she swallowed a cow!
She swallowed the cow to catch the goat.
She swallowed the goat to catch the dog.
She swallowed the dog to catch the cat.
She swallowed the cat to catch the bird.
She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed the fly. Perhaps she'll die.

I know an old lady who swallowed a horse.
She's dead, of course!"
    assert_success
    assert_equal "$output" "$expected"
}

