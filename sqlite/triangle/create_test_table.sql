DROP TABLE IF EXISTS tests;

CREATE TABLE IF NOT EXISTS tests (
  -- uuid and description are taken from the test.toml file
  uuid TEXT PRIMARY KEY,
  description TEXT NOT NULL,
  -- The following section is needed by the online test-runner
  status TEXT DEFAULT 'fail',
  message TEXT,
  output TEXT,
  test_code TEXT,
  task_id INTEGER DEFAULT NULL,
  -- Here are columns for the actual tests
  property TEXT NOT NULL,
  side_a REAL NOT NULL,
  side_b REAL NOT NULL,
  side_c REAL NOT NULL,
  expected BOOLEAN NOT NULL
);

INSERT INTO
  tests (
    uuid,
    description,
    property,
    side_a,
    side_b,
    side_c,
    expected
  )
VALUES
  (
    '8b2c43ac-7257-43f9-b552-7631a91988af',
    'all sides are equal',
    'equilateral',
    2,
    2,
    2,
    true
  ),
  (
    '33eb6f87-0498-4ccf-9573-7f8c3ce92b7b',
    'any side is unequal',
    'equilateral',
    2,
    3,
    2,
    false
  ),
  (
    'c6585b7d-a8c0-4ad8-8a34-e21d36f7ad87',
    'no sides are equal',
    'equilateral',
    5,
    4,
    6,
    false
  ),
  (
    '16e8ceb0-eadb-46d1-b892-c50327479251',
    'all zero sides is not a triangle',
    'equilateral',
    0,
    0,
    0,
    false
  ),
  (
    '3022f537-b8e5-4cc1-8f12-fd775827a00c',
    'sides may be floats',
    'equilateral',
    0.5,
    0.5,
    0.5,
    true
  ),
  (
    'cbc612dc-d75a-4c1c-87fc-e2d5edd70b71',
    'last two sides are equal',
    'isosceles',
    3,
    4,
    4,
    true
  ),
  (
    'e388ce93-f25e-4daf-b977-4b7ede992217',
    'first two sides are equal',
    'isosceles',
    4,
    4,
    3,
    true
  ),
  (
    'd2080b79-4523-4c3f-9d42-2da6e81ab30f',
    'first and last sides are equal',
    'isosceles',
    4,
    3,
    4,
    true
  ),
  (
    '8d71e185-2bd7-4841-b7e1-71689a5491d8',
    'equilateral triangles are also isosceles',
    'isosceles',
    4,
    4,
    4,
    true
  ),
  (
    '840ed5f8-366f-43c5-ac69-8f05e6f10bbb',
    'no sides are equal',
    'isosceles',
    2,
    3,
    4,
    false
  ),
  (
    '2eba0cfb-6c65-4c40-8146-30b608905eae',
    'first triangle inequality violation',
    'isosceles',
    1,
    1,
    3,
    false
  ),
  (
    '278469cb-ac6b-41f0-81d4-66d9b828f8ac',
    'second triangle inequality violation',
    'isosceles',
    1,
    3,
    1,
    false
  ),
  (
    '90efb0c7-72bb-4514-b320-3a3892e278ff',
    'third triangle inequality violation',
    'isosceles',
    3,
    1,
    1,
    false
  ),
  (
    'adb4ee20-532f-43dc-8d31-e9271b7ef2bc',
    'sides may be floats',
    'isosceles',
    0.5,
    0.4,
    0.5,
    true
  ),
  (
    'e8b5f09c-ec2e-47c1-abec-f35095733afb',
    'no sides are equal',
    'scalene',
    5,
    4,
    6,
    true
  ),
  (
    '2510001f-b44d-4d18-9872-2303e7977dc1',
    'all sides are equal',
    'scalene',
    4,
    4,
    4,
    false
  ),
  (
    'c6e15a92-90d9-4fb3-90a2-eef64f8d3e1e',
    'first and second sides are equal',
    'scalene',
    4,
    4,
    3,
    false
  ),
  (
    '3da23a91-a166-419a-9abf-baf4868fd985',
    'first and third sides are equal',
    'scalene',
    3,
    4,
    3,
    false
  ),
  (
    'b6a75d98-1fef-4c42-8e9a-9db854ba0a4d',
    'second and third sides are equal',
    'scalene',
    4,
    3,
    3,
    false
  ),
  (
    '70ad5154-0033-48b7-af2c-b8d739cd9fdc',
    'may not violate triangle inequality',
    'scalene',
    7,
    3,
    2,
    false
  ),
  (
    '26d9d59d-f8f1-40d3-ad58-ae4d54123d7d',
    'sides may be floats',
    'scalene',
    0.5,
    0.4,
    0.6,
    true
  );
