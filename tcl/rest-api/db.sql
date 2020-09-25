users
name
balance

transactions
lender
borrower
amount

ious
person1
person2
amount



PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS users (
    name TEXT PRIMARY KEY
);

--drop table transactions;
CREATE TABLE IF NOT EXISTS transactions (
    lender    TEXT NOT NULL,
    borrower  TEXT NOT NULL,
    amount    NUM  NOT NULL,
    timestamp TEXT DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (lender)   REFERENCES users(name),
    FOREIGN KEY (borrower) REFERENCES users(name)
);

# /add user
insert or ignore into users values ($name);
commit;

# /iou
insert into transactions values ($lender, $borrower, $amount);
commit;

# /get
select users.name, a.amount, a.borrower, b.amount, b.lender
from users
left join transactions a on a.lender = users.name
left join transactions b on b.borrower = users.name
;
