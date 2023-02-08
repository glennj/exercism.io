module BankAccount

type Account = Account of balance: decimal * isOpen: bool

let mkBankAccount(): Account = Account(0.0m, false)

let openAccount (Account(balance, isOpen)): Account = 
    Account(balance, true)

let closeAccount (Account(balance, isOpen)) =
    Account(balance, false)

let getBalance (Account(balance, isOpen)): decimal option = 
    match isOpen with
    | true -> Some balance
    | _    -> None

let updateBalance change (Account(balance, isOpen)) = 
    Account(balance + change, isOpen)
