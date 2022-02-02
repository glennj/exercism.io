defmodule NameBadge do
  @spec print(id :: String.t, name :: String.t, department :: String.t) :: String.t

  # could do this to learn `if` and pass the tests:
  def print(id, name, department) do
    if id do
      if department do
        "[#{id}] - #{name} - #{String.upcase(department)}"
      else
        "[#{id}] - #{name} - OWNER"
      end
    else
      if department do
        "#{name} - #{String.upcase(department)}"
      else
        "#{name} - OWNER"
      end
    end
  end

  ### or this:
  # def print(nil, name, nil),        do: "#{name} - OWNER"
  # def print(nil, name, department), do: "#{name} - #{String.upcase(department)}"
  # def print(id,  name, nil),        do: "[#{id}] - #{name} - OWNER"
  # def print(id,  name, department), do: "[#{id}] - #{name} - #{String.upcase(department)}"
end
