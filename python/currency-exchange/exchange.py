"""
module docstring
"""

def exchange_money(budget, exchange_rate):
    """
    :param budget: float - amount of money you are planning to exchange.
    :param exchange_rate: float - unit value of the foreign currency.
    :return: float - estimated value of the foreign currency you can receive
    """

    return budget / exchange_rate


def get_change(budget, exchanging_value):
    """
    :param budget: float - amount of money you own.
    :param exchanging_value: int - amount of your money you want to exchange now.
    :return: float - amount left of your starting currency after exchanging
    """

    return budget - exchanging_value


def get_value_of_bills(denomination, number_of_bills):
    """
    :param denomination: int - the value of a bill.
    :param number_of_bills: int - amount of bills you received.
    :return: int - total value of bills you now have
    """

    return denomination * number_of_bills


def get_number_of_bills(budget, denomination):
    """
    :param budget: float - the amount of money you are planning to exchange.
    :param denomination: int - the value of a single bill.
    :return: int - number of bills after exchanging all your money
    """

    return int(budget / denomination)


def actual_exchange_rate(exchange_rate, spread):
    return exchange_rate * (1 + spread/100)


def exchangeable_value(budget, exchange_rate, spread, denomination):
    """
    :param budget: float - the amount of your money you are planning to exchange.
    :param exchange_rate: float - the unit value of the foreign currency.
    :param spread: int - percentage that is taken as an exchange fee.
    :param denomination: int - the value of a single bill.
    :return: int - maximum value you can get
    """

    estimated_value = exchange_money(budget, actual_exchange_rate(exchange_rate, spread))
    return denomination * get_number_of_bills(estimated_value, denomination)


def non_exchangeable_value(budget, exchange_rate, spread, denomination):
    """
    :param budget: float - the amount of your money you are planning to exchange.
    :param exchange_rate: float - the unit value of the foreign currency.
    :param spread: int - percentage that is taken as an exchange fee.
    :param denomination: int - the value of a single bill.
    :return: int - unexchangeable value
    """

    real_value = exchange_money(budget, actual_exchange_rate(exchange_rate, spread))
    exch_value = exchangeable_value(budget, exchange_rate, spread, denomination)
    return int(real_value - exch_value)
