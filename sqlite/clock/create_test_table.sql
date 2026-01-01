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
  input TEXT NOT NULL, -- json object
  expected TEXT NOT NULL
);

INSERT INTO
  tests (uuid, description, property, input, expected)
VALUES
  (
    'a577bacc-106b-496e-9792-b3083ea8705e',
    'on the hour',
    'create',
    '{"hour":8,"minute":0}',
    '08:00'
  ),
  (
    'b5d0c360-3b88-489b-8e84-68a1c7a4fa23',
    'past the hour',
    'create',
    '{"hour":11,"minute":9}',
    '11:09'
  ),
  (
    '473223f4-65f3-46ff-a9f7-7663c7e59440',
    'midnight is zero hours',
    'create',
    '{"hour":24,"minute":0}',
    '00:00'
  ),
  (
    'ca95d24a-5924-447d-9a96-b91c8334725c',
    'hour rolls over',
    'create',
    '{"hour":25,"minute":0}',
    '01:00'
  ),
  (
    'f3826de0-0925-4d69-8ac8-89aea7e52b78',
    'hour rolls over continuously',
    'create',
    '{"hour":100,"minute":0}',
    '04:00'
  ),
  (
    'a02f7edf-dfd4-4b11-b21a-86de3cc6a95c',
    'sixty minutes is next hour',
    'create',
    '{"hour":1,"minute":60}',
    '02:00'
  ),
  (
    '8f520df6-b816-444d-b90f-8a477789beb5',
    'minutes roll over',
    'create',
    '{"hour":0,"minute":160}',
    '02:40'
  ),
  (
    'c75c091b-47ac-4655-8d40-643767fc4eed',
    'minutes roll over continuously',
    'create',
    '{"hour":0,"minute":1723}',
    '04:43'
  ),
  (
    '06343ecb-cf39-419d-a3f5-dcbae0cc4c57',
    'hour and minutes roll over',
    'create',
    '{"hour":25,"minute":160}',
    '03:40'
  ),
  (
    'be60810e-f5d9-4b58-9351-a9d1e90e660c',
    'hour and minutes roll over continuously',
    'create',
    '{"hour":201,"minute":3001}',
    '11:01'
  ),
  (
    '1689107b-0b5c-4bea-aad3-65ec9859368a',
    'hour and minutes roll over to exactly midnight',
    'create',
    '{"hour":72,"minute":8640}',
    '00:00'
  ),
  (
    'd3088ee8-91b7-4446-9e9d-5e2ad6219d91',
    'negative hour',
    'create',
    '{"hour":-1,"minute":15}',
    '23:15'
  ),
  (
    '77ef6921-f120-4d29-bade-80d54aa43b54',
    'negative hour rolls over',
    'create',
    '{"hour":-25,"minute":0}',
    '23:00'
  ),
  (
    '359294b5-972f-4546-bb9a-a85559065234',
    'negative hour rolls over continuously',
    'create',
    '{"hour":-91,"minute":0}',
    '05:00'
  ),
  (
    '509db8b7-ac19-47cc-bd3a-a9d2f30b03c0',
    'negative minutes',
    'create',
    '{"hour":1,"minute":-40}',
    '00:20'
  ),
  (
    '5d6bb225-130f-4084-84fd-9e0df8996f2a',
    'negative minutes roll over',
    'create',
    '{"hour":1,"minute":-160}',
    '22:20'
  ),
  (
    'd483ceef-b520-4f0c-b94a-8d2d58cf0484',
    'negative minutes roll over continuously',
    'create',
    '{"hour":1,"minute":-4820}',
    '16:40'
  ),
  (
    '1cd19447-19c6-44bf-9d04-9f8305ccb9ea',
    'negative sixty minutes is previous hour',
    'create',
    '{"hour":2,"minute":-60}',
    '01:00'
  ),
  (
    '9d3053aa-4f47-4afc-bd45-d67a72cef4dc',
    'negative hour and minutes both roll over',
    'create',
    '{"hour":-25,"minute":-160}',
    '20:20'
  ),
  (
    '51d41fcf-491e-4ca0-9cae-2aa4f0163ad4',
    'negative hour and minutes both roll over continuously',
    'create',
    '{"hour":-121,"minute":-5810}',
    '22:10'
  ),
  (
    'd098e723-ad29-4ef9-997a-2693c4c9d89a',
    'add minutes',
    'add',
    '{"hour":10,"minute":0,"value":3}',
    '10:03'
  ),
  (
    'b6ec8f38-e53e-4b22-92a7-60dab1f485f4',
    'add no minutes',
    'add',
    '{"hour":6,"minute":41,"value":0}',
    '06:41'
  ),
  (
    'efd349dd-0785-453e-9ff8-d7452a8e7269',
    'add to next hour',
    'add',
    '{"hour":0,"minute":45,"value":40}',
    '01:25'
  ),
  (
    '749890f7-aba9-4702-acce-87becf4ef9fe',
    'add more than one hour',
    'add',
    '{"hour":10,"minute":0,"value":61}',
    '11:01'
  ),
  (
    'da63e4c1-1584-46e3-8d18-c9dc802c1713',
    'add more than two hours with carry',
    'add',
    '{"hour":0,"minute":45,"value":160}',
    '03:25'
  ),
  (
    'be167a32-3d33-4cec-a8bc-accd47ddbb71',
    'add across midnight',
    'add',
    '{"hour":23,"minute":59,"value":2}',
    '00:01'
  ),
  (
    '6672541e-cdae-46e4-8be7-a820cc3be2a8',
    'add more than one day (1500 min = 25 hrs)',
    'add',
    '{"hour":5,"minute":32,"value":1500}',
    '06:32'
  ),
  (
    '1918050d-c79b-4cb7-b707-b607e2745c7e',
    'add more than two days',
    'add',
    '{"hour":1,"minute":1,"value":3500}',
    '11:21'
  ),
  (
    '37336cac-5ede-43a5-9026-d426cbe40354',
    'subtract minutes',
    'subtract',
    '{"hour":10,"minute":3,"value":3}',
    '10:00'
  ),
  (
    '0aafa4d0-3b5f-4b12-b3af-e3a9e09c047b',
    'subtract to previous hour',
    'subtract',
    '{"hour":10,"minute":3,"value":30}',
    '09:33'
  ),
  (
    '9b4e809c-612f-4b15-aae0-1df0acb801b9',
    'subtract more than an hour',
    'subtract',
    '{"hour":10,"minute":3,"value":70}',
    '08:53'
  ),
  (
    '8b04bb6a-3d33-4e6c-8de9-f5de6d2c70d6',
    'subtract across midnight',
    'subtract',
    '{"hour":0,"minute":3,"value":4}',
    '23:59'
  ),
  (
    '07c3bbf7-ce4d-4658-86e8-4a77b7a5ccd9',
    'subtract more than two hours',
    'subtract',
    '{"hour":0,"minute":0,"value":160}',
    '21:20'
  ),
  (
    '90ac8a1b-761c-4342-9c9c-cdc3ed5db097',
    'subtract more than two hours with borrow',
    'subtract',
    '{"hour":6,"minute":15,"value":160}',
    '03:35'
  ),
  (
    '2149f985-7136-44ad-9b29-ec023a97a2b7',
    'subtract more than one day (1500 min = 25 hrs)',
    'subtract',
    '{"hour":5,"minute":32,"value":1500}',
    '04:32'
  ),
  (
    'ba11dbf0-ac27-4acb-ada9-3b853ec08c97',
    'subtract more than two days',
    'subtract',
    '{"hour":2,"minute":20,"value":3000}',
    '00:20'
  ),
  (
    'f2fdad51-499f-4c9b-a791-b28c9282e311',
    'clocks with same time',
    'equal',
    '{"clock1":{"hour":15,"minute":37},"clock2":{"hour":15,"minute":37}}',
    true
  ),
  (
    '5d409d4b-f862-4960-901e-ec430160b768',
    'clocks a minute apart',
    'equal',
    '{"clock1":{"hour":15,"minute":36},"clock2":{"hour":15,"minute":37}}',
    false
  ),
  (
    'a6045fcf-2b52-4a47-8bb2-ef10a064cba5',
    'clocks an hour apart',
    'equal',
    '{"clock1":{"hour":14,"minute":37},"clock2":{"hour":15,"minute":37}}',
    false
  ),
  (
    '66b12758-0be5-448b-a13c-6a44bce83527',
    'clocks with hour overflow',
    'equal',
    '{"clock1":{"hour":10,"minute":37},"clock2":{"hour":34,"minute":37}}',
    true
  ),
  (
    '2b19960c-212e-4a71-9aac-c581592f8111',
    'clocks with hour overflow by several days',
    'equal',
    '{"clock1":{"hour":3,"minute":11},"clock2":{"hour":99,"minute":11}}',
    true
  ),
  (
    '6f8c6541-afac-4a92-b0c2-b10d4e50269f',
    'clocks with negative hour',
    'equal',
    '{"clock1":{"hour":22,"minute":40},"clock2":{"hour":-2,"minute":40}}',
    true
  ),
  (
    'bb9d5a68-e324-4bf5-a75e-0e9b1f97a90d',
    'clocks with negative hour that wraps',
    'equal',
    '{"clock1":{"hour":17,"minute":3},"clock2":{"hour":-31,"minute":3}}',
    true
  ),
  (
    '56c0326d-565b-4d19-a26f-63b3205778b7',
    'clocks with negative hour that wraps multiple times',
    'equal',
    '{"clock1":{"hour":13,"minute":49},"clock2":{"hour":-83,"minute":49}}',
    true
  ),
  (
    'c90b9de8-ddff-4ffe-9858-da44a40fdbc2',
    'clocks with minute overflow',
    'equal',
    '{"clock1":{"hour":0,"minute":1},"clock2":{"hour":0,"minute":1441}}',
    true
  ),
  (
    '533a3dc5-59a7-491b-b728-a7a34fe325de',
    'clocks with minute overflow by several days',
    'equal',
    '{"clock1":{"hour":2,"minute":2},"clock2":{"hour":2,"minute":4322}}',
    true
  ),
  (
    'fff49e15-f7b7-4692-a204-0f6052d62636',
    'clocks with negative minute',
    'equal',
    '{"clock1":{"hour":2,"minute":40},"clock2":{"hour":3,"minute":-20}}',
    true
  ),
  (
    '605c65bb-21bd-43eb-8f04-878edf508366',
    'clocks with negative minute that wraps',
    'equal',
    '{"clock1":{"hour":4,"minute":10},"clock2":{"hour":5,"minute":-1490}}',
    true
  ),
  (
    'b87e64ed-212a-4335-91fd-56da8421d077',
    'clocks with negative minute that wraps multiple times',
    'equal',
    '{"clock1":{"hour":6,"minute":15},"clock2":{"hour":6,"minute":-4305}}',
    true
  ),
  (
    '822fbf26-1f3b-4b13-b9bf-c914816b53dd',
    'clocks with negative hours and minutes',
    'equal',
    '{"clock1":{"hour":7,"minute":32},"clock2":{"hour":-12,"minute":-268}}',
    true
  ),
  (
    'e787bccd-cf58-4a1d-841c-ff80eaaccfaa',
    'clocks with negative hours and minutes that wrap',
    'equal',
    '{"clock1":{"hour":18,"minute":7},"clock2":{"hour":-54,"minute":-11513}}',
    true
  ),
  (
    '96969ca8-875a-48a1-86ae-257a528c44f5',
    'full clock and zeroed clock',
    'equal',
    '{"clock1":{"hour":24,"minute":0},"clock2":{"hour":0,"minute":0}}',
    true
  );
