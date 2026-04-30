contains = (list, elem) ->
  for e in *list
    return true if e == elem
  false

-- perform a deep copy of the thing
clone = (thing) ->
  switch type thing
    when 'table' then {k, clone(v) for k, v in pairs thing}
    else thing

-- the input and the return values are cloned so the caller cannot
-- modify the database via table references

class RestApi
  new: (db) =>
    @db = clone db

  GET: (url, payload = {}) =>
    switch url
      when '/users' then @get_users payload
      else error '404'

  POST: (url, payload) =>
    switch url
      when '/add' then @add_user payload
      when '/iou' then @process_loan payload
      else error '404'

  get_user: (name) =>
    for user in *@db.users
      return user if user.name == name
    nil

  get_users: (payload) =>
    if payload.users
      {users: clone [@get_user name for name in *payload.users]}
    else
      {users: clone @db.users}

  add_user: (payload) =>
    @db[payload.user] = {
      name: payload.user
      balance: 0
      owes: {}
      owed_by: {}
    }
    clone @db[payload.user]

  process_loan: (payload) =>
    lender = @get_user payload.lender
    borrower = @get_user payload.borrower

    lender.balance += payload.amount
    borrower.balance -= payload.amount

    amt_owing = payload.amount + (lender.owed_by[borrower.name] or 0) - (lender.owes[borrower.name] or 0)

    -- clear the current relationship ...
    lender.owes[borrower.name], lender.owed_by[borrower.name] = nil, nil
    borrower.owes[lender.name], borrower.owed_by[lender.name] = nil, nil

    -- ... and set the new one
    if amt_owing > 0
      lender.owed_by[borrower.name] = amt_owing
      borrower.owes[lender.name] = amt_owing
    elseif amt_owing < 0
      lender.owes[borrower.name] = -amt_owing
      borrower.owed_by[lender.name] = -amt_owing

    if lender.name < borrower.name
      {users: clone {lender, borrower}}
    else
      {users: clone {borrower, lender}}
