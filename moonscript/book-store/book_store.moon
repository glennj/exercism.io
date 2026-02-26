import contains, map from require 'lib/table'
import sum from require 'lib/math'

BookPrice = 800
Discounted = {[0]: 1.0, 1.0, 0.95, 0.90, 0.80, 0.75}

-- ------------------------------------------------------------
-- add each book to a "bundle", a collection of books with no duplicates
bundleBooks = (basket) ->
  bundles = {{}}
  local added

  for book in *basket
    added = false
    for bundle in *bundles
      if not contains bundle, book
        table.insert bundle, book
        added = true
        break
    if not added
      table.insert bundles, {book} -- new bundle
  bundles

-- ------------------------------------------------------------
-- optimize the bundles: two bundles of size 4 is cheaper
-- than a bundle of size 5 plus a bundle of size 3
optimize = (bundles) ->
  local b5, b3
  for bundle in *bundles
    if     #bundle == 5 then b5 = bundle
    elseif #bundle == 3 then b3 = bundle

  if not (b5 and b3) then return bundles

  for i, book in ipairs b5
    if not contains b3, book
      table.insert b3, book
      table.remove b5, i
      break

  -- repeat until no more (5-bundle plus 3-bundle) remain
  optimize bundles

-- ------------------------------------------------------------
total = (basket) ->
  bundles = optimize bundleBooks basket
  sum map bundles, (bundle) -> #bundle * BookPrice * Discounted[#bundle]

{ :total }
