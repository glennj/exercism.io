/* A somewhat over-engineered solution */

export class RestAPI {
  constructor({users}) {
    this.controller = new Controller(new UserCache(users));
  }

  get(url) {
    const c = this.controller;
    const [uri, query] = url.split("?");
    if (uri === '/users') return c.getUsers(query);
  }

  post(url, payload) {
    const c = this.controller;
    if (url === "/add") return c.postAdd(payload);
    if (url === "/iou") return c.postIOU(payload);
  }
}

// ------------------------------------------------------------
class Controller {
  constructor(cache) {
    this.cache = cache;
  }

  getUsers(query) {
    const users = [];
    if (query === undefined) {
      users.push(...this.cache.entries());
    }
    else {
      query.split("&").forEach(pair => {
        const [variable, value] = pair.split("=");
        if (variable === "users") {
          value.split(",").forEach(name => {
            users.push(this.cache.find(name));
          });
        }
      });
    }
    return {users: users.map(user => user.toObject())};
  }


  postAdd({user}) {
    return this.cache.add({name: user}).toObject();
  }

  postIOU({lender, borrower, amount}) {
    const userLend = this.cache.find(lender);
    const userBorr = this.cache.find(borrower);
    userLend.lendTo(userBorr, amount);
    return this.getUsers(`users=${[lender, borrower].sort().join(",")}`);
  }
}

// ------------------------------------------------------------
class UserCache {
  constructor(objects) {
    this.users = objects.map(obj => new User(obj));
  }

  entries() {
    return this.users;
  }

  add(obj) {
    const user = new User(obj);
    this.users.push(user);
    return user;
  }

  find(name) {
    return this.users.find(user => user.name === name);
  }
}

// ------------------------------------------------------------
class User {
  constructor({name, owes, owed_by, balance}) {
    this.name    = name;
    this.owes    = owes    ?? {};
    this.owed_by = owed_by ?? {};
    this.balance = balance ?? 0;
  }

  toObject() {
    return {
      name: this.name,
      owes: this.owes,
      owed_by: this.owed_by,
      balance: this.balance
    };
  }

  lendTo(borrower, amount) {
    const iOweBorrower   = this.owes[borrower.name]    ?? 0;
    const borrowerOwesMe = this.owed_by[borrower.name] ?? 0;
    const amountOwing    = borrowerOwesMe - iOweBorrower + amount;

    delete this.owes[borrower.name];
    delete this.owed_by[borrower.name];
    delete borrower.owes[this.name];
    delete borrower.owed_by[this.name];

    if (amountOwing > 0) {
      this.owed_by[borrower.name] = amountOwing;
      borrower.owes[this.name]    = amountOwing;
    }
    else if (amountOwing < 0) {
      this.owes[borrower.name]    = -amountOwing;
      borrower.owed_by[this.name] = -amountOwing;
    }
    this.balance     += amount;
    borrower.balance -= amount;
  }
}
