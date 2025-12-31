-- Create database:
.read ./create_fixture.sql
-- Read user student solution and save any output as markdown in user_output.md:
.mode markdown
.output user_output.md
.read ./resistor-color-duo.sql
.output
-- Create a clean testing environment:
DROP TABLE IF EXISTS tests;

CREATE TABLE IF NOT EXISTS tests (
  -- uuid and name (description) are taken from the test.toml file
  uuid TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  -- The following section is needed by the online test-runner
  status TEXT DEFAULT 'fail',
  message TEXT,
  output TEXT,
  test_code TEXT,
  task_id INTEGER DEFAULT NULL,
  -- Here are columns for the actual tests
  color1 TEXT NOT NULL,
  color2 TEXT NOT NULL,
  expected INTEGER NOT NULL
);
