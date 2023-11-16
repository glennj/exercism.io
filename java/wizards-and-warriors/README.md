# Wizards and Warriors

Welcome to Wizards and Warriors on Exercism's Java Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Inheritance

Inheritance is a core concept in OOP (Object Oriented Programming). It donates IS-A relationship.
It literally means in programming as it means in english, inheriting features from parent(in programming features is normally functions
and variables).

Consider a class, `Animal` as shown,

```java
//Creating an Animal class with bark() as a member function.
public class Animal {

    public void bark() {
        System.out.println("This is a animal");
    }

}
```

`Animal` is a parent class, because the properties this class has can be extended to all the animals in general.

Consider an animal named `Lion`, having a class like,

```java
//Lion class is a child class of Animal.
public class Lion extends Animal {

    public void bark() {
        System.out.println("Lion here!!");
    }

}
```

Now whenever we do,

```java
Animal animal = new Lion(); //creating instance of Animal, of type Lion
animal.bark();
```

Note: Initialising the `Animal` class with `Lion`.
The output will look like

```java
Lion here!!
```

According to OOP, there are many types of inheritance, but Java supports only some of them(Multi-level and Hierarchical).
To read more about it, please read [this][java-inheritance].

[java-inheritance]: https://www.javatpoint.com/inheritance-in-java#:~:text=On%20the%20basis%20of%20class,will%20learn%20about%20interfaces%20later.

## Instructions

In this exercise you're playing a role-playing game named "Wizards and Warriors," which allows you to play as either a Wizard or a Warrior.

There are different rules for Warriors and Wizards to determine how much damage points they deal.

For a Warrior, these are the rules:

- Deal 6 points of damage if the fighter they are attacking is not vulnerable
- Deal 10 points of damage if the fighter they are attacking is vulnerable

For a Wizard, these are the rules:

- Deal 12 points of damage if the Wizard prepared a spell in advance
- Deal 3 points of damage if the Wizard did not prepare a spell in advance

In general, fighters are never vulnerable. However, Wizards _are_ vulnerable if they haven't prepared a spell.

You have six tasks that work with Warriors and Wizard fighters.

## 1. Describe a fighter

Override the `toString()` method on the `Fighter` class to return a description of the fighter, formatted as `"Fighter is a <FIGHTER_TYPE>"`.

```java
Fighter warrior = new Warrior();
warrior.toString();
// => "Fighter is a Warrior"
```

## 2. Make fighters not vulnerable by default

Ensure that the `Fighter.isVulnerable()` method always returns `false`.

```java
Fighter warrior = new Warrior();
warrior.isVulnerable();
// => false
```

## 3. Allow Wizards to prepare a spell

Implement the `Wizard.prepareSpell()` method to allow a Wizard to prepare a spell in advance.

```java
Wizard wizard = new Wizard();
wizard.prepareSpell();
```

## 4. Make Wizards vulnerable when not having prepared a spell

Ensure that the `isVulnerable()` method returns `true` if the wizard did not prepare a spell; otherwise, return `false`.

```java
Fighter wizard = new Wizard();
wizard.isVulnerable();
// => true
```

## 5. Calculate the damage points for a Wizard

Implement the `Wizard.damagePoints()` method to return the damage points dealt: 12 damage points when a spell has been prepared, 3 damage points when not.

```java
Wizard wizard = new Wizard();
Warrior warrior = new Warrior();

wizard.prepareSpell();
wizard.damagePoints(warrior);
// => 12
```

## 6. Calculate the damage points for a Warrior

Implement the `Warrior.damagePoints()` method to return the damage points dealt: 10 damage points when the target is vulnerable, 6 damage points when not.

```java
Warrior warrior = new Warrior();
Wizard wizard = new Wizard();

warrior.damagePoints(wizard);
// => 10
```

## Source

### Created by

- @himanshugoyal1065