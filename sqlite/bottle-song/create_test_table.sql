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
  start_bottles INTEGER NOT NULL,
  take_down INTEGER NOT NULL,
  expected TEXT NOT NULL
);

INSERT INTO
  tests (
    uuid,
    description,
    start_bottles,
    take_down,
    expected
  )
VALUES
  (
    'd4ccf8fc-01dc-48c0-a201-4fbeb30f2d03',
    'first generic verse',
    10,
    1,
    'Ten green bottles hanging on the wall,
Ten green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be nine green bottles hanging on the wall.'
  ),
  (
    '0f0aded3-472a-4c64-b842-18d4f1f5f030',
    'last generic verse',
    3,
    1,
    'Three green bottles hanging on the wall,
Three green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be two green bottles hanging on the wall.'
  ),
  (
    'f61f3c97-131f-459e-b40a-7428f3ed99d9',
    'verse with 2 bottles',
    2,
    1,
    'Two green bottles hanging on the wall,
Two green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be one green bottle hanging on the wall.'
  ),
  (
    '05eadba9-5dbd-401e-a7e8-d17cc9baa8e0',
    'verse with 1 bottle',
    1,
    1,
    'One green bottle hanging on the wall,
One green bottle hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be no green bottles hanging on the wall.'
  ),
  (
    'a4a28170-83d6-4dc1-bd8b-319b6abb6a80',
    'first two verses',
    10,
    2,
    'Ten green bottles hanging on the wall,
Ten green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be nine green bottles hanging on the wall.

Nine green bottles hanging on the wall,
Nine green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be eight green bottles hanging on the wall.'
  ),
  (
    '3185d438-c5ac-4ce6-bcd3-02c9ff1ed8db',
    'last three verses',
    3,
    3,
    'Three green bottles hanging on the wall,
Three green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be two green bottles hanging on the wall.

Two green bottles hanging on the wall,
Two green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be one green bottle hanging on the wall.

One green bottle hanging on the wall,
One green bottle hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be no green bottles hanging on the wall.'
  ),
  (
    '28c1584a-0e51-4b65-9ae2-fbc0bf4bbb28',
    'all verses',
    10,
    10,
    'Ten green bottles hanging on the wall,
Ten green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be nine green bottles hanging on the wall.

Nine green bottles hanging on the wall,
Nine green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be eight green bottles hanging on the wall.

Eight green bottles hanging on the wall,
Eight green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be seven green bottles hanging on the wall.

Seven green bottles hanging on the wall,
Seven green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be six green bottles hanging on the wall.

Six green bottles hanging on the wall,
Six green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be five green bottles hanging on the wall.

Five green bottles hanging on the wall,
Five green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be four green bottles hanging on the wall.

Four green bottles hanging on the wall,
Four green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be three green bottles hanging on the wall.

Three green bottles hanging on the wall,
Three green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be two green bottles hanging on the wall.

Two green bottles hanging on the wall,
Two green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be one green bottle hanging on the wall.

One green bottle hanging on the wall,
One green bottle hanging on the wall,
And if one green bottle should accidentally fall,
There''ll be no green bottles hanging on the wall.'
  );
