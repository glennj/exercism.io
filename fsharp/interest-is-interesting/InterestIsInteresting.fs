module InterestIsInteresting

let interestRate (balance: decimal): single =
    if   balance <    0.0m then 3.213f
    elif balance < 1000.0m then 0.5f
    elif balance < 5000.0m then 1.621f
    else 2.475f

let interest (balance: decimal): decimal =
   balance * decimal (interestRate balance / 100.0f)

let annualBalanceUpdate(balance: decimal): decimal =
   balance + interest balance

let amountToDonate(balance: decimal) (taxFreePercentage: float): int =
    let generosityFactor = 2.0
    if balance <= 0.0m then
        0
    else
        int (balance * decimal (taxFreePercentage * generosityFactor / 100.0))
