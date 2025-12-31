-- Create database:
.read ./create_fixture.sql
-- Read user student solution and save any output as markdown in user_output.md:
.mode markdown
.output user_output.md
.read ./resistor-color.sql
.output
-- Create a clean testing environment:
-- test_color_code tests the color_code values.
-- test_colors tests the colors values.
-- test_results combined the results for final output.
DROP TABLE IF EXISTS "test_color_code";

CREATE TABLE IF NOT EXISTS "test_color_code" (
  -- uuid and name (description) are taken from the test.toml file
  uuid TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  -- Here are columns for the actual tests
  color TEXT NOT NULL,
  result INT NOT NULL,
  -- The following section is needed by the online test-runner
  status TEXT DEFAULT 'fail',
  message TEXT,
  output TEXT,
  test_code TEXT,
  task_id INTEGER DEFAULT NULL
);
