module BankAccount

let private lockObj = new System.Object()

type Account = 
    | Closed
    | Open of decimal ref

let mkBankAccount(): Account = Closed

let openAccount (account: Account): Account = Open(ref 0.0m)

let closeAccount (account: Account): Account = Closed

let updateBalance (change: decimal) (account: Account): Account = 
    match account with
    | Closed -> Closed
    | Open(balance) ->
        lock lockObj (fun () -> balance.Value <- balance.Value + change)
        account

let getBalance (account: Account): decimal option = 
    match account with
    | Closed -> None
    | Open balance -> Some balance.Value
