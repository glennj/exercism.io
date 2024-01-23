# Help

## Running the tests

## Running the tests

1. Go to the root of your PHP exercise directory, which is `<EXERCISM_WORKSPACE>/php`.
   To find the Exercism workspace run

       ➜ exercism debug | grep Workspace

1. Get [PHPUnit] if you don't have it already.

       ➜ wget -O phpunit https://phar.phpunit.de/phpunit-9.phar
       ➜ chmod +x phpunit
       ➜ ./phpunit --version

2. Execute the tests:

       ➜ ./phpunit file_to_test.php

   For example, to run the tests for the Hello World exercise, you would run:

       ➜ ./phpunit HelloWorldTest.php

[PHPUnit]: https://phpunit.de

## Submitting your solution

You can submit your solution using the `exercism submit Lasagna.php` command.
This command will upload your solution to the Exercism website and print the solution page's URL.

It's possible to submit an incomplete solution which allows you to:

- See how others have completed the exercise
- Request help from a mentor

## Need to get help?

If you'd like help solving the exercise, check the following pages:

- The [PHP track's documentation](https://exercism.org/docs/tracks/php)
- The [PHP track's programming category on the forum](https://forum.exercism.org/c/programming/php)
- [Exercism's programming category on the forum](https://forum.exercism.org/c/programming/5)
- The [Frequently Asked Questions](https://exercism.org/docs/using/faqs)

Should those resources not suffice, you could submit your (incomplete) solution to request mentoring.

To get help if you're having trouble, you can use one of the following resources:

 - [/r/php](https://www.reddit.com/r/php) is the PHP subreddit.
 - [StackOverflow](https://stackoverflow.com/questions/tagged/php) can be used to search for your problem and see if it has been answered already. You can also ask and answer questions.