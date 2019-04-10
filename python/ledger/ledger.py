# -*- coding: utf-8 -*-

from datetime import datetime
from collections import namedtuple
import locale
import re

Currency = {
    'USD': '$',
    'EUR': u'€',
}

MsgCat = {
    'heading': {
        'en_US': ('Date', 'Description', 'Change'),
        'nl_NL': ('Datum', 'Omschrijving', 'Verandering')
    }
}


def format_entries(currency, localename, entries):
    col_widths = (10, 25, 13)
    hdr_fmt = '{:%s} | {:%s} | {:%s}' % col_widths
    line_fmt = '{:%s} | {:%s} | {:>%s}' % col_widths

    header = hdr_fmt.format(*MsgCat['heading'][localename])
    table = [header]

    locale = Locale(localename)

    for entry in sorted(entries):
        date = locale.date(entry.datetime)
        desc = truncate(entry.description, col_widths[1])
        amount = locale.currency(entry.change, currency)
        table += [line_fmt.format(date, desc, amount)]

    return "\n".join(table)


def truncate(string, width):
    if len(string) > width:
        return string[:width-3] + '...'
    return string


class LedgerEntry(object):
    def __init__(self, date, description, change):
        self.datetime = datetime.strptime(date, '%Y-%m-%d')
        self.description = description
        self.change = change

    def __lt__(self, other):
        if self.datetime < other.datetime:
            return True
        elif self.datetime == other.datetime:
            if self.description < other.description:
                return True
            elif self.description == other.description:
                return self.change < other.change
        return False


create_entry = LedgerEntry


class Locale:
    '''
    I'm not at all happy I have to implement this by hand.
    The `locale` module was *almost* there, but the currency
    and date formatting were not what the tests wanted.

    >>> from datetime import datetime
    >>> import locale
    >>> now = datetime.now()
    >>> locale.setlocale(locale.LC_ALL, 'en_US.utf-8')
    'en_US.utf-8'
    >>> now.strftime('%x')
    '04/09/2019'
    >>> locale.setlocale(locale.LC_ALL, 'nl_NL.utf-8')
    'nl_NL.utf-8'
    >>> now.strftime('%x')
    '09-04-19'
    >>> amount = -12345.67
    >>> locale.setlocale(locale.LC_ALL, 'en_US.utf-8')
    'en_US.utf-8'
    >>> locale.currency(amount, grouping=True)
    '-$12,345.67'
    >>> locale.setlocale(locale.LC_ALL, 'nl_NL.utf-8')
    'nl_NL.utf-8'
    >>> locale.currency(amount, grouping=True)
    '€ 12 345,67-'

    I can see the locale's individual settings using
    `locale.localeconv()`, but I can't figure out how to
    tweak those settings.
    '''

    LocaleCatalog = namedtuple(
        'LocaleCatalog',
        'date_fmt c_pos_fmt c_neg_fmt n_grp_sep n_dec_sep'
    )

    LocaleSettings = {
        'en_US': LocaleCatalog(
            date_fmt='%m/%d/%Y',
            c_pos_fmt='{}{} ',
            c_neg_fmt='({}{})',
            n_grp_sep=',',
            n_dec_sep='.',
        ),
        'nl_NL': LocaleCatalog(
            date_fmt='%d-%m-%Y',
            c_pos_fmt='{} {} ',
            c_neg_fmt='{} -{} ',
            n_grp_sep='.',
            n_dec_sep=',',
        ),
    }

    def __init__(self, locale):
        self.locale = locale
        self.catalog = self.LocaleSettings[locale]

    def date(self, datetime):
        return datetime.strftime(self.catalog.date_fmt)

    def currency(self, cents, currency):
        negative = cents < 0
        dollars, cents = divmod(abs(cents), 100)
        amount = '{}{}{:02d}'.format(
            self.commify(dollars),
            self.catalog.n_dec_sep,
            cents
        )
        if negative:
            fmt = self.catalog.c_neg_fmt
        else:
            fmt = self.catalog.c_pos_fmt
        return fmt.format(Currency[currency], amount)

    def commify(self, num):
        return strrev(self.catalog.n_grp_sep.join(
            re.findall('.{,3}', strrev(str(num)))[:-1]
        ))


def strrev(string):
    return "".join(reversed(string))
