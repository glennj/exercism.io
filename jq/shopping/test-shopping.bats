#!/usr/bin/env bats
load bats-extra

setup() {
    cat > input.json <<'END_JSON'
{
  "name": "Ingredients for pancakes",
  "ingredients": [
    {
      "item": "flour",
      "amount": {
        "quantity": 1,
        "unit": "cup"
      }
    },
    {
      "item": "sugar",
      "amount": {
        "quantity": 0.25,
        "unit": "cup"
      }
    },
    {
      "item": "baking powder",
      "amount": {
        "quantity": 1,
        "unit": "teaspoon"
      }
    },
    {
      "item": "baking soda",
      "amount": {
        "quantity": 0.25,
        "unit": "teaspoon"
      }
    },
    {
      "item": "salt",
      "amount": {
        "quantity": 1,
        "unit": "pinch"
      }
    },
    {
      "item": "buttermilk",
      "amount": {
        "quantity": 1,
        "unit": "cup"
      },
      "substitute": "regular milk"
    },
    {
      "item": "eggs",
      "amount": {
        "quantity": 1,
        "unit": "egg"
      }
    },
    {
      "item": "melted butter",
      "amount": {
        "quantity": 1,
        "unit": "tablespoon"
      },
      "substitute": "vegetable oil"
    }
  ],
  "optional ingredients": [
    {
      "item": "cinnamon",
      "amount": {
        "quantity": 0.25,
        "unit": "teaspoon"
      }
    },
    {
      "item": "vanilla extract",
      "amount": {
        "quantity": 0.5,
        "unit": "teaspoon"
      }
    },
    {
      "item": "blueberries",
      "amount": {
        "quantity": 0.25,
        "unit": "cup"
      },
      "substitute": "chopped apple"
    }
  ]
}
END_JSON

}

teardown() {
    rm input.json
}

@test "Show shopping list name" {
    ## task 1
    run jq -f shopping.jq input.json
    assert_success
    assert_line --index 0 '"Ingredients for pancakes"'
}

@test "Count the ingredients" {
    ## task 2
    run jq -f shopping.jq input.json
    assert_success
    assert_line --index 1 '8'
}

@test "Show how much sugar is needed" {
    ## task 3
    run jq -f shopping.jq input.json
    assert_success
    assert_line --index 2 '0.25'
}

@test "Map of substitutions" {
    ## task 4
    run jq -c -f shopping.jq input.json
    assert_success
    assert_line --index 3 '{"buttermilk":"regular milk","melted butter":"vegetable oil","blueberries":"chopped apple"}'
}
