# Inventory Management

Welcome to Inventory Management on Exercism's Python Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

A _**dictionary**_ is Python's primary mapping type that associates a _hashable key_ with a value. The lookup by key is more efficient than searching through an array, but does require more memory.

## Dict construction

Dictionaries can be created in various ways. Two simple options are the use the `dict()` class constructor or the dict literal declaration with key-value pairs.

### Use the `dict()` constructor

```python
>>> bear = dict(name="Black Bear", speed=40, land_animal=True)
{'name': 'Black Bear', 'speed': 40, 'land_animal': True}
```

### Declare a _dict_ literal

```python
>>> whale = {"name": "Blue Whale", "speed": 35, "land_animal": False}
{'name': 'Blue Whale', 'speed': 35, 'land_animal': False}
```

With the dict literal declaration keep in mind that _keys_ are of _data types_ `str` and the colon `:` is used instead of an equal sign `=`.

## Accessing values

You can access an item in a dictionary using the _key_ of the value.

### Using _square brackets_ after the dict object

```python
>>> bear["speed"]
40
```

### Using `.get()`

```python
>>> whale.get("name")
'Blue Whale'
```

## Changing values

You can easily change a value of an item using its _key_.

```python
>>> bear["name"] = "Grizzly Bear"
{'name': 'Grizzly Bear', 'speed': 40, 'land_animal': True}

>>> whale["speed"] = 25
{'name': 'Blue Whale', 'speed': 25, 'land_animal': False}
```

## Deleting values using keys

You can delete an item from a dictionary using `dict.pop(<key>)`. This will remove the `(key`, `value`) pair from the dictionary and return the `value` for use. `dict.pop(<key>)` accepts second argument, `default` that is returned if the `key` is not found  (`dict.pop(<key>, <default>)`). Otherwise, a `KeyError` will be raised for any `key` that is missing.

```python
>>> bear.pop("name")
'Grizzly Bear'
>>> bear.pop("name", "Unknown")
'Unknown'
>>> bear.pop("name")
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
KeyError: 'name'
```

## Looping through a dictionary

Looping through a dictionary using `for item in dict` will iterate over the _keys_, but you can access the _values_ by using _square brackets_.

```python
>>> for key in bear:
>>>     (key, bear[key])
('name', 'Black Bear')
('speed', 40)
('land_animal', True)
```

## Instructions

In this exercise, you will be managing an inventory system.

The inventory should be organized by the item name and it should keep track of the number of items available.

You will have to handle adding items to an inventory. Each time an item appears in a given list, increase the item's quantity by `1` in the inventory. Then, you will have to handle deleting items from an inventory.

To finish, you will have to implement a function which returns all the key-value pairs in an inventory as a list of `tuples`.

## 1. Create an inventory based on a list

Implement the `create_inventory()` function that creates an "inventory" from a list of items. It should return a `dict` containing each item name paired with their respective quantity.

```python
>>> create_inventory(["coal", "wood", "wood", "diamond", "diamond", "diamond"])
{"coal":1, "wood":2 "diamond":3}
```

## 2. Add items from a list to an existing dictionary

Implement the `add_items()` function that adds a list of items to an inventory:

```python
>>> add_items({"coal":1}, ["wood", "iron", "coal", "wood"])
{"coal":2, "wood":2, "iron":1}
```

## 3. Decrement items from the inventory

Implement the `decrement_items(<items>)` function that takes a `list` of items. The function should remove one from the available count in the inventory for each time an item appears on the `list`:

```python
>>> decrement_items({"coal":3, "diamond":1, "iron":5}, ["diamond", "coal", "iron", "iron"])
{"coal":2, "diamond":0, "iron":3}
```

Item counts in the inventory should not fall below 0. If the number of times an item appears on the list exceeds the count available, the quantity listed for that item should remain at 0 and additional requests for removing counts should be ignored.

```python
>>> decrement_items({"coal":2, "wood":1, "diamond":2}, ["coal", "coal", "wood", "wood", "diamond"])
{"coal":0, "wood":0, "diamond":1}
```

## 4. Remove an item entirely from the inventory

Implement the `remove_item(<inventory>, <item>)` function that removes an item and its count entirely from an inventory:

```python
>>> remove_item({"coal":2, "wood":1, "diamond":2}, "coal")
{"wood":1, "diamond":2}
```

If the item is not found in the inventory, the function should return the original inventory unchanged.

```python
>>> remove_item({"coal":2, "wood":1, "diamond":2}, "gold")
{"coal":2, "wood":1, "diamond":2}
```

## 5. Return the inventory content

Implement the `list_inventory()` function that takes an inventory and returns a list of `(item, quantity)` tuples. The list should only include the available items (with a quantity greater than zero):

```python
>>> list_inventory({"coal":7, "wood":11, "diamond":2, "iron":7, "silver":0})
[('coal', 7), ('diamond', 2), ('iron', 7), ('wood', 11)]
```

## Source

### Created by

- @j08k

### Contributed to by

- @valentin-p
- @bethanyG
- @mukeshgurpude
- @kotp