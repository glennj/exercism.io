CREATE TEMP TABLE allergens (
    allergen TEXT NOT NULL,
    id       INTEGER PRIMARY KEY AUTOINCREMENT,
    value    INTEGER GENERATED AS (1 << (id - 1)) STORED
);

INSERT INTO allergens (allergen) VALUES
    ('eggs'),
    ('peanuts'),
    ('shellfish'),
    ('strawberries'),
    ('tomatoes'),
    ('chocolate'),
    ('pollen'),
    ('cats') ;


UPDATE allergies
SET result = IIF(score & allergens.value != 0, 'true', 'false')
FROM allergens
WHERE task = 'allergicTo'
  AND item = allergens.allergen ;


WITH grouped AS (
    SELECT score, GROUP_CONCAT(allergen, ', ') AS allergy_items
    FROM allergies, allergens
    WHERE task = 'list'
      AND score & value != 0
    GROUP BY score
)
UPDATE allergies
SET result = allergy_items
FROM grouped
WHERE task = 'list'
  AND allergies.score = grouped.score ;
