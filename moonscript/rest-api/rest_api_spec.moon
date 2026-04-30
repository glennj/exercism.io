RestApi = require 'rest_api'

describe 'rest-api', ->
  assert\set_parameter "TableFormatLevel", 4

  describe 'user management', ->
    it 'no users', ->
      database = {
        users: {}
      }
      api = RestApi database
      result = api\GET '/users'
      expected = {
        users: {}
      }
      assert.are.same expected, result

    it 'add user', ->
      database = {
        users: {}
      }
      api = RestApi database
      payload = {
        user: "Adam"
      }
      result = api\POST '/add', payload
      expected = {
        owed_by: {}
        balance: 0.0
        owes: {}
        name: "Adam"
      }
      assert.are.same expected, result

    it 'get single user', ->
      database = {
        users: {{
            owed_by: {}
            balance: 0.0
            owes: {}
            name: "Adam"
          }, {
            owed_by: {}
            balance: 0.0
            owes: {}
            name: "Bob"
          }}
      }
      api = RestApi database
      payload = {
        users: {"Bob"}
      }
      result = api\GET '/users', payload
      expected = {
        users: {{
            owed_by: {}
            balance: 0.0
            owes: {}
            name: "Bob"
          }}
      }
      assert.are.same expected, result

  describe 'iou', ->
    it 'both users have 0 balance', ->
      database = {
        users: {{
            owed_by: {}
            balance: 0.0
            owes: {}
            name: "Adam"
          }, {
            owed_by: {}
            balance: 0.0
            owes: {}
            name: "Bob"
          }}
      }
      api = RestApi database
      payload = {
        borrower: "Bob"
        lender: "Adam"
        amount: 3.0
      }
      result = api\POST '/iou', payload
      expected = {
        users: {{
            owed_by: {
              Bob: 3.0
            }
            balance: 3.0
            owes: {}
            name: "Adam"
          }, {
            owed_by: {}
            balance: -3.0
            owes: {
              Adam: 3.0
            }
            name: "Bob"
          }}
      }
      assert.are.same expected, result

    it 'borrower has negative balance', ->
      database = {
        users: {{
            owed_by: {}
            balance: 0.0
            owes: {}
            name: "Adam"
          }, {
            owed_by: {}
            balance: -3.0
            owes: {
              Chuck: 3.0
            }
            name: "Bob"
          }, {
            owed_by: {
              Bob: 3.0
            }
            balance: 3.0
            owes: {}
            name: "Chuck"
          }}
      }
      api = RestApi database
      payload = {
        borrower: "Bob"
        lender: "Adam"
        amount: 3.0
      }
      result = api\POST '/iou', payload
      expected = {
        users: {{
            owed_by: {
              Bob: 3.0
            }
            balance: 3.0
            owes: {}
            name: "Adam"
          }, {
            owed_by: {}
            balance: -6.0
            owes: {
              Chuck: 3.0
              Adam: 3.0
            }
            name: "Bob"
          }}
      }
      assert.are.same expected, result

    it 'lender has negative balance', ->
      database = {
        users: {{
            owed_by: {}
            balance: 0.0
            owes: {}
            name: "Adam"
          }, {
            owed_by: {}
            balance: -3.0
            owes: {
              Chuck: 3.0
            }
            name: "Bob"
          }, {
            owed_by: {
              Bob: 3.0
            }
            balance: 3.0
            owes: {}
            name: "Chuck"
          }}
      }
      api = RestApi database
      payload = {
        borrower: "Adam"
        lender: "Bob"
        amount: 3.0
      }
      result = api\POST '/iou', payload
      expected = {
        users: {{
            owed_by: {}
            balance: -3.0
            owes: {
              Bob: 3.0
            }
            name: "Adam"
          }, {
            owed_by: {
              Adam: 3.0
            }
            balance: 0.0
            owes: {
              Chuck: 3.0
            }
            name: "Bob"
          }}
      }
      assert.are.same expected, result

    it 'lender owes borrower', ->
      database = {
        users: {{
            owed_by: {}
            balance: -3.0
            owes: {
              Bob: 3.0
            }
            name: "Adam"
          }, {
            owed_by: {
              Adam: 3.0
            }
            balance: 3.0
            owes: {}
            name: "Bob"
          }}
      }
      api = RestApi database
      payload = {
        borrower: "Bob"
        lender: "Adam"
        amount: 2.0
      }
      result = api\POST '/iou', payload
      expected = {
        users: {{
            owed_by: {}
            balance: -1.0
            owes: {
              Bob: 1.0
            }
            name: "Adam"
          }, {
            owed_by: {
              Adam: 1.0
            }
            balance: 1.0
            owes: {}
            name: "Bob"
          }}
      }
      assert.are.same expected, result

    it 'lender owes borrower less than new loan', ->
      database = {
        users: {{
            owed_by: {}
            balance: -3.0
            owes: {
              Bob: 3.0
            }
            name: "Adam"
          }, {
            owed_by: {
              Adam: 3.0
            }
            balance: 3.0
            owes: {}
            name: "Bob"
          }}
      }
      api = RestApi database
      payload = {
        borrower: "Bob"
        lender: "Adam"
        amount: 4.0
      }
      result = api\POST '/iou', payload
      expected = {
        users: {{
            owed_by: {
              Bob: 1.0
            }
            balance: 1.0
            owes: {}
            name: "Adam"
          }, {
            owed_by: {}
            balance: -1.0
            owes: {
              Adam: 1.0
            }
            name: "Bob"
          }}
      }
      assert.are.same expected, result

    it 'lender owes borrower same as new loan', ->
      database = {
        users: {{
            owed_by: {}
            balance: -3.0
            owes: {
              Bob: 3.0
            }
            name: "Adam"
          }, {
            owed_by: {
              Adam: 3.0
            }
            balance: 3.0
            owes: {}
            name: "Bob"
          }}
      }
      api = RestApi database
      payload = {
        borrower: "Bob"
        lender: "Adam"
        amount: 3.0
      }
      result = api\POST '/iou', payload
      expected = {
        users: {{
            owed_by: {}
            balance: 0.0
            owes: {}
            name: "Adam"
          }, {
            owed_by: {}
            balance: 0.0
            owes: {}
            name: "Bob"
          }}
      }
      assert.are.same expected, result
