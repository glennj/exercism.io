# Elixir

## Function capturing

<!-- -->

You'll often see short  functions written with anonymous "capturing" style:
```elixir
Map.update(acc, letter, 1, &(&1 + 1))
```
where `&1`, (`&2`, etc) are placeholders for the first (and second, etc) function parameters.

More info at
* [Function capturing][func-cap] in the [Getting Started][get-start] guide, and
* the `&/1` capture operator in [Kernel.SpecialForms][&/1]

[func-cap]: https://elixir-lang.org/getting-started/modules-and-functions.html#function-capturing
[get-start]: https://elixir-lang.org/getting-started/introduction.html
[&/1]: https://hexdocs.pm/elixir/Kernel.SpecialForms.html#&/1

---
