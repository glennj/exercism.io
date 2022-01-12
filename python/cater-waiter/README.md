# Cater Waiter

Welcome to Cater Waiter on Exercism's Python Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

A [`set`][type-set] is a mutable and _unordered_ collection of _hashable_ objects.
Items within a `set` are distinct and duplicate members are not allowed.
Like most collections, `sets` can hold any (or multiple) data type(s) -- as long as those types can be [hashed][hashable].
Sets also come in an _immutable_ [`frozenset`][type-frozenset] flavor.

Like other collections, `sets` support membership testing through `in`, length calculation through `len()`, shallow copies through `copy()`, and iteration via `for item in <set>`.
_Unlike_ sequence type collections (_`string`, `list` & `tuple`_), `sets` are **neither ordered nor indexed**, and _do not support_ slicing, sorting, or other sequence-type behaviors.

Sets are most commonly used to quickly dedupe groups of items.
They're also used for fast membership testing, finding supersets & subsets of items, and performing "set math" (_calculating union, intersection, difference & symmetric difference between groups of items._).

Sets are more space-efficient than a keys-only dictionary and faster than a `list` or `array` for membership -- unless you need to keep track of sequenced or duplicated items.

## Construction

A `set` can be declared as a _set literal_ with curly `{}` brackets and commas between elements.

```python
>>> one_element = {'😀'}
>>> one_element
{'😀'}

>>> multiple_elements = {'😀', '😃', '😄', '😁'}
>>> multiple_elements
{'😀', '😃', '😄', '😁'}

>>> multiple_duplicates =  {'😀', '😃', '😄', '😁', '😃', '😄'}
>>> multiple_duplicates
{'😀', '😁', '😃', '😄'}
```

Set literals use the same curly braces as `dict` literals, so the `set()` constructor must be used to declare an empty `set`.

The `set()` constructor can also be used with any _iterable_ passed as an argument.
Elements inside the iterable are cycled through by the constructor and added to the `set` individually.
Order is not preserved and duplicates are silently omitted:

```python
>>> no_elements = set()
>>> no_elements
set()

# The tuple is unpacked and each distinct element is added.  Duplicates are removed.
>>> multiple_elements_from_tuple = set(("Parrot", "Bird", 334782, "Bird", "Parrot"))
>>> multiple_elements_from_tuple
{334782, 'Bird', 'Parrot'}

# The list is unpacked and each distinct element is added.
>>> multiple_elements_from_list = set([2, 3, 2, 3, 3, 3, 5, 7, 11, 7, 11, 13, 13])
>>> multiple_elements_from_set
{2, 3, 5, 7, 11}
```

Sets can hold heterogeneous datatypes, but all `set` elements must be _hashable_:

```python

>>> lists_as_elements = {['😅','🤣'], ['😂','🙂','🙃'], ['😜', '🤪', '😝']}

Traceback (most recent call last):

  File "<ipython-input-24-1bb7c3d22c52>", line 1, in <module>
    lists_as_elements = {['😅','🤣'], ['😂','🙂','🙃'], ['😜', '🤪', '😝']}

TypeError: unhashable type: 'list'

# Standard sets are mutable, so they cannot be hashed.
>>> sets_as_elements = {{'😅','🤣'}, {'😂','🙂','🙃'}, {'😜', '🤪', '😝'}}
Traceback (most recent call last):

  File "<ipython-input-25-92f22c7541b1>", line 1, in <module>
    sets_as_elements = {{'😅','🤣'}, {'😂','🙂','🙃'}, {'😜', '🤪', '😝'}}

TypeError: unhashable type: 'set'
```

## Working with Sets

Sets implement methods that generally mimic [mathematical set operations][mathematical-sets].
Most (_though not all_) of these methods can be performed using either operator(s) or method call(s).
Using operators requires that both inputs be `sets` or `frozensets`, while methods will generally take any iterable as an argument.

### Fast Membership Testing


**Subsets**:  `<set>.issubset(<other_collection>)` / `<set> <= <other_set>`
               are used to check if every element in `<set>` is also in `<other>`.

**Supersets**:  `<set>.issuperset(<other_collection>)` /  `<set> >= <other_set>`
                 are used to check the inverse -- if every element in `<other>` is also in `<set>`.


```python
>>> animals = {'chicken': 'white','sparrow': 'grey','eagle': 'brown and white',
                'albatross': 'grey and white','crow': 'black','elephant': 'grey',
                'dog': 'rust','cow': 'black and white','tiger': 'organge and black',
                'cat': 'grey','squirrel': 'black'}

>>>  mammals = {'squirrel','dog','cat','cow', 'tiger', 'elephant'}
>>>  birds   = {'crow','sparrow','eagle','chicken', 'albatross'}

# Methods will take any iterable as an argument
>>> mammals.issubset(animals)
True

# A set is always a loose subset of itself
>>> animals <= animals
True

>>> birds <= set(animals)
True

>>> birds <= mammals
False
```


The `<set>.isdisjoint(<other_collection>)` method is used to test if a `set` has **no elements in common** with another set or iterable.
It will accept any `iterable` or `set` as an argument, returning `True` if they are **disjoint**, `False` otherwise.
Note that for `dcts`, the iteration default is over`<dict>.keys()`.

```python
>>>  mammals = {'squirrel','dog','cat','cow', 'tiger', 'elephant'}
>>>  birds   = {'crow','sparrow','eagle','chicken', 'albatross'}

# Dictionary of animal names with colors
>>> animals = {'chicken': 'white','sparrow': 'grey','eagle': 'brown and white',
                'albatross': 'grey and white','crow': 'black','elephant': 'grey',
                'dog': 'rust','cow': 'black and white','tiger': 'orange and black',
                'cat': 'grey','squirrel': 'black'}

# List of additional animals
>>> additional_animals = ['pangolin', 'panda', 'parrot', 'lemur', 'tiger', 'pangolin']
...

>>> mammals.isdisjoint(birds)
True

>>> mammals.isdisjoint(animals)
False

>>> birds.isdisjoint(additional_animals)
True

>>> set(additional_animals).isdisjoint(animals)
False
```

### Operations Between Sets

**Union**: `<set>.union(*<other iterables>)` and `<set> | <other set 1> | <other set 2> | ... | <other set n>` return a new `set` with elements from `<set>` and all `<others>`.

```python
>>> perennial_vegetables = {'Asparagus', 'Broccoli', 'Sweet Potato', 'Kale'}
>>> annual_vegetables = {'Corn', 'Zucchini', 'Sweet Peas', 'Summer Squash'}

>>> more_perennials = ['Radicchio', 'Rhubarb', 'Spinach', 'Watercress']

# Methods will take any iterable as an argument.
>>> perennial_vegetables.union(more_perennials)
{'Asparagus','Broccoli','Kale','Radicchio','Rhubarb','Spinach','Sweet Potato','Watercress'}

# Operators require sets.
>>> perennial_vegetables | annual_vegetables
{'Asparagus','Broccoli','Corn','Kale','Summer Squash','Sweet Peas','Sweet Potato','Zucchini'}

```

**Difference**: `<set>.difference(*<other iterables>)` and `<set> - <other set 1> - <other set 2> - ...<other set n>` return a new `set` with elements from the original `<set>` that are not in `<others>`.

```python
>>> berries_and_veggies = {'Asparagus', 'Broccoli', 'Watercress', 'Goji Berries', 'Goose Berries', 'Ramps',
                           'Walking Onions', 'Raspberries','Blueberries', 'Blackberries', 'Strawberries',
                           'Rhubarb', 'Kale', 'Artichokes', 'Currants', 'Honeyberries'}

# Methods will take any iterable as an argument.
>>> veggies = ('Asparagus', 'Broccoli', 'Watercress', 'Ramps',
               'Walking Onions', 'Rhubarb', 'Kale', 'Artichokes')

>>> just_berries = berries_and_veggies.difference(veggies)
>>> just_berries
{'Blackberries','Blueberries','Currants','Goji Berries',
 'Goose Berries','Honeyberries','Raspberries','Strawberries'}

>>> berries_and_veggies - just_berries
{'Artichokes','Asparagus','Broccoli','Kale','Ramps','Rhubarb','Walking Onions','Watercress'}
```

**Intersection**: `<set>.intersection(*<other iterables>)` and `<set> & <other set> & <other set 2> & ... <other set n>` return a new `set` with elements common to the original `set` and all `<others>`.

```python
>>> perennials = {'Annatto','Asafetida','Asparagus','Azalea','Winter Savory', 'Blackberries','Broccoli','Curry Leaf',
                  'Fennel','French Sorrel','Fuchsia','Kaffir Lime','Kale','Lavender','Mint','Oranges',
                  'Oregano','Ramps','Roses','Tarragon','Watercress','Wild Bergamot'}

>>> annuals = {'Corn', 'Zucchini', 'Sweet Peas', 'Marjoram', 'Summer Squash', 'Okra',
               'Shallots', 'Basil', 'Cilantro', 'Cumin', 'Sunflower', 'Chervil', 'Summer Savory'}

>>> herbs = ['Annatto','Asafetida','Basil','Chervil','Cilantro','Curry Leaf','Fennel','Kaffir Lime',
             'Lavender','Marjoram','Mint','Oregano','Summer Savory' 'Tarragon','Wild Bergamot',
             'Wild Celery','Winter Savory']


# Methods will take any iterable as an argument.
>>> perennial_herbs = perennials.intersection(herbs)
>>> perennial_herbs
{'Mint', 'Annatto', 'Winter Savory', 'Curry Leaf', 'Lavender', 'Fennel',
 'Oregano', 'Kaffir Lime','Asafetida', 'Wild Bergamot', 'Tarragon'}

>>> annuals & set(herbs)
 {'Basil', 'Chervil', 'Marjoram', 'Cilantro'}
```

**Symmetric Difference**: `<set>.symmetric_difference(<other iterable>)` and `<set> ^ <other set>` return a new `set` that contains elements that are in `<set>` OR `<other>`, but **not in both**.

```python
>>> one = {'black pepper','breadcrumbs','celeriac','chickpea flour',
           'flour','lemon','parsley','salt','soy sauce','sunflower oil','water'}

>>> two = {'black pepper','cornstarch','garlic','ginger','lemon juice','lemon zest',
           'salt','soy sauce','sugar','tofu','vegetable oil','vegetable stock','water'}

>>> two_as_list  = ['black pepper','cornstarch','garlic','ginger','lemon juice','lemon zest',
                    'salt','soy sauce','sugar','tofu','vegetable oil','vegetable stock','water']

>>> one ^ two
...
{'breadcrumbs','celeriac','chickpea flour','cornstarch','flour','garlic','ginger', 'lemon',
'lemon juice','lemon zest','parsley','sugar','sunflower oil','tofu','vegetable oil','vegetable stock'}

>>> (one | two) - (one & two)
...
{'breadcrumbs','celeriac','chickpea flour','cornstarch','flour','garlic','ginger', 'lemon',
'lemon juice','lemon zest','parsley','sugar','sunflower oil','tofu','vegetable oil','vegetable stock'}

>>> one ^ two == (one | two) - (one & two)
...
True


# Methods will take any iterable as an argument.
>>> one.symmetric_difference(two_as_list)
...
{'breadcrumbs','celeriac','chickpea flour','cornstarch','flour','garlic','ginger', 'lemon',
'lemon juice','lemon zest','parsley','sugar','sunflower oil','tofu','vegetable oil','vegetable stock'}
```

A symmetric difference of more than two sets will result in a `set` that includes both the elements unique to each `set` AND elements shared between more than two sets in the series (_details in the Wikipedia article on [symmetric difference][symmetric_difference]_).
To obtain only items unique to each `set` in the series, intersections between all 2-set combinations need to be aggregated in a separate step, and removed.

```python
>>> one = {'black pepper','breadcrumbs','celeriac','chickpea flour',
           'flour','lemon','parsley','salt','soy sauce','sunflower oil','water'}

>>> two = {'black pepper','cornstarch','garlic','ginger','lemon juice','lemon zest',
           'salt','soy sauce','sugar','tofu','vegetable oil','vegetable stock','water'}

>>> three = {'black pepper','garlic','lemon juice','mixed herbs','nutritional yeast',
             'olive oil','salt','silken tofu','smoked tofu','soy sauce','spaghetti','turmeric'}

>>> four = {'barley malt','bell pepper','cashews','flour','fresh basil','garlic','garlic powder',
            'honey','mushrooms','nutritional yeast','olive oil','oregano','red onion',
            'red pepper flakes','rosemary','salt','sugar','tomatoes','water','yeast'}

>>> intersections = (one & two | one & three | one & four | two & three | two & four | three & four)
>>> intersections
 ...
 {'black pepper','flour','garlic','lemon juice','nutritional yeast', 'olive oil','salt','soy sauce', 'sugar','water'}

>>> one ^ two ^ three ^ four
...
{'barley malt','bell pepper','black pepper','breadcrumbs','cashews','celeriac','chickpea flour','cornstarch',
 'fresh basil','garlic','garlic powder','ginger','honey','lemon','lemon zest','mixed herbs','mushrooms',
 'oregano','parsley','red onion','red pepper flakes','rosemary','silken tofu','smoked tofu','soy sauce',
 'spaghetti','sunflower oil','tofu','tomatoes','turmeric','vegetable oil','vegetable stock','water','yeast'}

>>> (one ^ two ^ three ^ four) - intersections
...
{'barley malt','bell pepper','breadcrumbs', 'cashews','celeriac','chickpea flour','cornstarch','fresh basil',
 'garlic powder','ginger','honey','lemon','lemon zest','mixed herbs','mushrooms','oregano','parsley',
 'red onion','red pepper flakes','rosemary','silken tofu','smoked tofu','spaghetti','sunflower oil',
 'tofu', 'tomatoes','turmeric','vegetable oil','vegetable stock','yeast'}
```

[symmetric_difference]: https://en.wikipedia.org/wiki/Symmetric_difference
[type-set]: https://docs.python.org/3/library/stdtypes.html#set
[type-frozenset]: https://docs.python.org/3/library/stdtypes.html#frozenset
[mathematical-sets]: https://en.wikipedia.org/wiki/Set_theory#Basic_concepts_and_notation
[hashable]: https://docs.python.org/3.7/glossary.html#term-hashable

## Instructions

You and your business partners operate a small catering company. You've just agreed to run an event for a local cooking club that features "club favorite" dishes. The club is inexperienced in hosting large events, and needs help with organizing, shopping, prepping and serving. You've decided to write some small Python scripts to speed the whole planning process along.

## 1. Clean up Dish Ingredients

The event recipes were added from various sources and their ingredients appear to have duplicate (_or more_) entries -- you don't want to end up purchasing excess items!
 Before the shopping and cooking can commence, each dish's ingredient list needs to be "cleaned".

Implement the `clean_ingredients(<dish_name>, <dish_ingredients>)` function that takes the name of a dish and a `list` of ingredients.
 This function should return a `tuple` with the name of the dish as the first item, followed by the de-duped `set` of ingredients.


```python
>>> clean_ingredients('Punjabi-Style Chole', ['onions', 'tomatoes', 'ginger paste', 'garlic paste', 'ginger paste', 'vegetable oil', 'bay leaves', 'cloves', 'cardamom', 'cilantro', 'peppercorns', 'cumin powder', 'chickpeas', 'coriander powder', 'red chili powder', 'ground turmeric', 'garam masala', 'chickpeas', 'ginger', 'cilantro'])

>>> ('Punjabi-Style Chole', {'garam masala', 'bay leaves', 'ground turmeric', 'ginger', 'garlic paste', 'peppercorns', 'ginger paste', 'red chili powder', 'cardamom', 'chickpeas', 'cumin powder', 'vegetable oil', 'tomatoes', 'coriander powder', 'onions', 'cilantro', 'cloves'})
```

## 2. Cocktails and Mocktails

The event is going to include both cocktails and "mocktails" - mixed drinks _without_ the alcohol.
 You need to ensure that "mocktail" drinks are truly non-alcoholic and the cocktails do indeed _include_ alcohol.

Implement the `check_drinks(<drink_name>, <drink_ingredients>)` function that takes the name of a drink and a `list` of ingredients.
 The function should return the name of the drink followed by "Mocktail" if the drink has no alcoholic ingredients, and drink name followed by "Cocktail" if the drink includes alcohol.
  For the purposes of this exercise, cocktails will only include alcohols from the ALCOHOLS constant in `categories.py`:

```python
>>> from categories import ALCOHOLS 

>>> check_drinks('Honeydew Cucumber', ['honeydew', 'coconut water', 'mint leaves', 'lime juice', 'salt', 'english cucumber'])
...
'Honeydew Cucumber Mocktail'

>>> check_drinks('Shirley Tonic', ['cinnamon stick', 'scotch', 'whole cloves', 'ginger', 'pomegranate juice', 'sugar', 'club soda'])
...
'Shirley Tonic Cocktail'
```

## 3. Categorize Dishes

The guest list includes diners with different dietary needs, and your staff will need to separate the dishes into Vegan, Vegetarian, Paleo, Keto, and Omnivore.

Implement the `categorize_dish(<dish_name>, <dish_ingredients>)` function that takes a dish name and a `set` of that dish's' ingredients.
The function should return a string with the `dish name: <CATEGORY>` (_which meal category the dish belongs to_).
All dishes will "fit" into one of the categories imported from `categories.py` (VEGAN, VEGETARIAN, PALEO, KETO, or OMNIVORE).

```python
>>> from categories import VEGAN, VEGETARIAN, PALEO, KETO, OMNIVORE


>>> categorize_dish('Sticky Lemon Tofu', ['tofu', 'soy sauce', 'salt', 'black pepper', 'cornstarch', 'vegetable oil', 'garlic', 'ginger', 'water', 'vegetable stock', 'lemon juice', 'lemon zest', 'sugar'])
...
'Sticky Lemon Tofu: VEGAN'

>>> categorize_dish('Shrimp Bacon and Crispy Chickpea Tacos with Salsa de Guacamole', ['shrimp', 'bacon', 'avocado', 'chickpeas', 'fresh tortillas', 'sea salt', 'guajillo chile', 'slivered almonds', 'olive oil', 'butter', 'black pepper', 'garlic', 'onion'])
...
'Shrimp Bacon and Crispy Chickpea Tacos with Salsa de Guacamole: OMNIVORE'
```

## 4. Label Allergens and Restricted Foods

Some guests have allergies and additional dietary restrictions.
These ingredients need to be tagged/annotated for each dish so that they don't cause issues.

Implement the `tag_special_ingredients(<dish>)` function that takes a `tuple` with the dish name in the first position, and a `list` or `set` of ingredients for that dish in the second position.
Return the dish name followed by the `set` of ingredients that require a special note on the dish description.
Dish ingredients inside a `list` may or may not have duplicates.
 For the purposes of this exercise, all allergens or special ingredients that need to be labeled are in the SPECIAL_INGREDIENTS constant imported from `categories.py`.

```python
>>> from categories import SPECIAL_INGREDIENTS

>>> tag_special_ingredients(('Ginger Glazed Tofu Cutlets', ['tofu', 'soy sauce', 'ginger', 'corn starch', 'garlic', 'brown sugar', 'sesame seeds', 'lemon juice']))
...
('Ginger Glazed Tofu Cutlets', {'garlic','soy sauce','tofu'})

>>> tag_special_ingredients(('Arugula and Roasted Pork Salad', ['pork tenderloin', 'arugula', 'pears', 'blue cheese', 'pine nuts', 'balsamic vinegar', 'onions', 'black pepper']))
...
('Arugula and Roasted Pork Salad', {'pork tenderloin', 'blue cheese', 'pine nuts', 'onions'})
```

## 5. Compile a "Master List" of Ingredients

In preparation for ordering and shopping, you'll need to compile a "master list" of ingredients for everything on the menu (_quantities to be filled in later_).

Implement the `compile_ingredients(<dishes>)` function that takes a `list` of dishes and returns a set of all ingredients in all listed dishes.
Each individual dish is represented by its `set` of ingredients.

```python
dishes = [ {'tofu', 'soy sauce', 'ginger', 'corn starch', 'garlic', 'brown sugar', 'sesame seeds', 'lemon juice'},
           {'pork tenderloin', 'arugula', 'pears', 'blue cheese', 'pine nuts',
           'balsamic vinegar', 'onions', 'black pepper'},
           {'honeydew', 'coconut water', 'mint leaves', 'lime juice', 'salt', 'english cucumber'}]

>>> compile_ingredients(dishes)
...
{'arugula', 'brown sugar', 'honeydew', 'coconut water', 'english cucumber', 'balsamic vinegar', 'mint leaves', 'pears', 'pork tenderloin', 'ginger', 'blue cheese', 'soy sauce', 'sesame seeds', 'black pepper', 'garlic', 'lime juice', 'corn starch', 'pine nuts', 'lemon juice', 'onions', 'salt', 'tofu'}
```

## 6. Pull out Appetizers for Passing on Trays

The hosts have given you a list of dishes they'd like prepped as "bite-sized" appetizers to be served on trays.
 You need to pull these from the main list of dishes being prepared as larger servings.

Implement the `separate_appetizers(<dishes>, <appetizers>)` function that takes a `list` of dish names and a `list` of appetizer names.
The function should return the `list` of dish names with appetizer names removed.
Either the `<dishes>` or `<appetizers>` `list` could contain duplicates and may require de-duping.

```python
dishes =    ['Avocado Deviled Eggs','Flank Steak with Chimichurri and Asparagus', 'Kingfish Lettuce Cups',
             'Grilled Flank Steak with Caesar Salad','Vegetarian Khoresh Bademjan','Avocado Deviled Eggs',
             'Barley Risotto','Kingfish Lettuce Cups']
          
appetizers = ['Kingfish Lettuce Cups','Avocado Deviled Eggs','Satay Steak Skewers',
              'Dahi Puri with Black Chickpeas','Avocado Deviled Eggs','Asparagus Puffs',
              'Asparagus Puffs']
              
>>> separate_appetizers(dishes, appetizers)
...
['Vegetarian Khoresh Bademjan', 'Barley Risotto', 'Flank Steak with Chimichurri and Asparagus', 
 'Grilled Flank Steak with Caesar Salad']
```

## 7. Find Ingredients Used in Only One Recipe

Within in each category (_Vegan, Vegetarian, Paleo, Keto, Omnivore_), you're going to pull out ingredients that appear in only one dish.
These "singleton" ingredients will be assigned a special shopper to ensure they're not forgotten in the rush to get everything else done.

Implement the `singleton_ingredients(<dishes>, <INTERSECTIONS>)` function that takes a `list` of dishes and a `<CATEGORY>_INTERSECTIONS` constant for the same category.
Each dish is represented by a `set` of its ingredients.
Each `<CATEGORY>_INTERSECTIONS` is a `set` of ingredients that appear in more than one dish in the category.
Using set operations, your function should return a `set` of "singleton" ingredients (_ingredients appearing in only one dish in the category_).

```python
from categories import example_dishes, EXAMPLE_INTERSECTIONS

>>> singleton_ingredients(example_dishes, EXAMPLE_INTERSECTION)
...
{'vegetable oil', 'vegetable stock', 'barley malt', 'tofu', 'fresh basil', 'lemon', 'ginger', 'honey', 'spaghetti', 'cornstarch', 'yeast', 'red onion', 'breadcrumbs', 'mixed herbs', 'garlic powder', 'celeriac', 'lemon zest', 'sunflower oil', 'mushrooms', 'silken tofu', 'smoked tofu', 'bell pepper', 'cashews', 'oregano', 'tomatoes', 'parsley', 'red pepper flakes', 'rosemary'}
```

## Source

### Created by

- @bethanyg