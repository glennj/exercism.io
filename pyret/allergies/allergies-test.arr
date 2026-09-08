use context starter2024

include file("allergies.arr")

# Eggs

check "Eggs allergy - not allergic to anything":
  allergens = allergies(0)
  allergens.allergicTo("eggs") is false
end

check "Eggs allergy - allergic only to eggs":
  allergens = allergies(1)
  allergens.allergicTo("eggs") is true
end

check "Eggs allergy - allergic to eggs and something else":
  allergens = allergies(3)
  allergens.allergicTo("eggs") is true
end

check "Eggs allergy - allergic to something, but not eggs":
  allergens = allergies(2)
  allergens.allergicTo("eggs") is false
end

check "Eggs allergy - allergic to everything":
  allergens = allergies(255)
  allergens.allergicTo("eggs") is true
end

# Peanuts

check "peanuts allergy - not allergic to anything":
  allergens = allergies(0)
  allergens.allergicTo("peanuts") is false
end

check "peanuts allergy - allergic only to peanuts":
  allergens = allergies(2)
  allergens.allergicTo("peanuts") is true
end

check "peanuts allergy - allergic to peanuts and something else":
  allergens = allergies(7)
  allergens.allergicTo("peanuts") is true
end

check "peanuts allergy - allergic to something, but not peanuts":
  allergens = allergies(5)
  allergens.allergicTo("peanuts") is false
end

check "peanuts allergy - allergic to everything":
  allergens = allergies(255)
  allergens.allergicTo("peanuts") is true
end

# Shellfish

check "shellfish allergy - not allergic to anything":
  allergens = allergies(0)
  allergens.allergicTo("shellfish") is false
end

check "shellfish allergy - allergic only to shellfish":
  allergens = allergies(4)
  allergens.allergicTo("shellfish") is true
end

check "shellfish allergy - allergic to shellfish and something else":
  allergens = allergies(14)
  allergens.allergicTo("shellfish") is true
end

check "shellfish allergy - allergic to something, but not shellfish":
  allergens = allergies(10)
  allergens.allergicTo("shellfish") is false
end

check "shellfish allergy - allergic to everything":
  allergens = allergies(255)
  allergens.allergicTo("shellfish") is true
end

# Strawberries

check "strawberries allergy - not allergic to anything":
  allergens = allergies(0)
  allergens.allergicTo("strawberries") is false
end

check "strawberries allergy - allergic only to strawberries":
  allergens = allergies(8)
  allergens.allergicTo("strawberries") is true
end

check "strawberries allergy - allergic to strawberries and something else":
  allergens = allergies(28)
  allergens.allergicTo("strawberries") is true
end

check "strawberries allergy - allergic to something, but not strawberries":
  allergens = allergies(20)
  allergens.allergicTo("strawberries") is false
end

check "strawberries allergy - allergic to everything":
  allergens = allergies(255)
  allergens.allergicTo("strawberries") is true
end

# Tomatoes

check "tomatoes allergy - not allergic to anything":
  allergens = allergies(0)
  allergens.allergicTo("tomatoes") is false
end

check "tomatoes allergy - allergic only to tomatoes":
  allergens = allergies(16)
  allergens.allergicTo("tomatoes") is true
end

check "tomatoes allergy - allergic to tomatoes and something else":
  allergens = allergies(56)
  allergens.allergicTo("tomatoes") is true
end

check "tomatoes allergy - allergic to something, but not tomatoes":
  allergens = allergies(40)
  allergens.allergicTo("tomatoes") is false
end

check "tomatoes allergy - allergic to everything":
  allergens = allergies(255)
  allergens.allergicTo("tomatoes") is true
end

# Chocolate

check "chocolate allergy - not allergic to anything":
  allergens = allergies(0)
  allergens.allergicTo("chocolate") is false
end

check "chocolate allergy - allergic only to chocolate":
  allergens = allergies(32)
  allergens.allergicTo("chocolate") is true
end

check "chocolate allergy - allergic to chocolate and something else":
  allergens = allergies(112)
  allergens.allergicTo("chocolate") is true
end

check "chocolate allergy - allergic to something, but not chocolate":
  allergens = allergies(80)
  allergens.allergicTo("chocolate") is false
end

check "chocolate allergy - allergic to everything":
  allergens = allergies(255)
  allergens.allergicTo("chocolate") is true
end

# Pollen

check "pollen allergy - not allergic to anything":
  allergens = allergies(0)
  allergens.allergicTo("pollen") is false
end

check "pollen allergy - allergic only to pollen":
  allergens = allergies(64)
  allergens.allergicTo("pollen") is true
end

check "pollen allergy - allergic to pollen and something else":
  allergens = allergies(224)
  allergens.allergicTo("pollen") is true
end

check "pollen allergy - allergic to something, but not pollen":
  allergens = allergies(160)
  allergens.allergicTo("pollen") is false
end

check "pollen allergy - allergic to everything":
  allergens = allergies(255)
  allergens.allergicTo("pollen") is true
end

# Cats

check "cats allergy - not allergic to anything":
  allergens = allergies(0)
  allergens.allergicTo("cats") is false
end

check "cats allergy - allergic only to cats":
  allergens = allergies(128)
  allergens.allergicTo("cats") is true
end

check "cats allergy - allergic to cats and something else":
  allergens = allergies(192)
  allergens.allergicTo("cats") is true
end

check "cats allergy - allergic to something, but not cats":
  allergens = allergies(64)
  allergens.allergicTo("cats") is false
end

check "cats allergy - allergic to everything":
  allergens = allergies(255)
  allergens.allergicTo("cats") is true
end

# List

check "list when no allergies":
  allergens = allergies(0)
  allergens.list() is [list: ]
end

check "list when just eggs":
  allergens = allergies(1)
  allergens.list() is [list: "eggs"]
end

check "list when just peanuts":
  allergens = allergies(2)
  allergens.list() is [list: "peanuts"]
end

check "list when just strawberries":
  allergens = allergies(8)
  allergens.list() is [list: "strawberries"]
end

check "list when eggs and peanuts":
  allergens = allergies(3)
  allergens.list() is [list: "eggs", "peanuts"]
end

check "list when more than eggs but not peanuts":
  allergens = allergies(5)
  allergens.list() is [list: "eggs", "shellfish"]
end

check "list when lots of stuff":
  allergens = allergies(248)
  allergens.list() is [list: 
      "strawberries",
      "tomatoes",
      "chocolate",
      "pollen",
      "cats"]
end

check "list when everything":
  allergens = allergies(255)
  allergens.list() is [list: 
      "eggs",
      "peanuts",
      "shellfish",
      "strawberries",
      "tomatoes",
      "chocolate",
      "pollen",
      "cats"]
end

check "list when no allergen score parts":
  allergens = allergies(509)
  allergens.list() is [list: 
      "eggs", 
      "shellfish", 
      "strawberries", 
      "tomatoes", 
      "chocolate", 
      "pollen", 
      "cats"]
end

check "list when no allergen score parts without highest valid score":
  allergens = allergies(257)
  allergens.list() is [list: "eggs"]
end
