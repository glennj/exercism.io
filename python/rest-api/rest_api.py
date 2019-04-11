import re
import json
from functools import partialmethod


class RestAPI(object):
    def __init__(self, database=None):
        self.db = database or {}
        if 'users' not in self.db:
            self.db['users'] = []

        self.routes = {
            'get': {
                '/users': self.get_users,
            },
            'post': {
                '/add': self.post_add,
                '/iou': self.post_iou,
            }
        }

    def handle_req(self, url, payload=None, method=None):
        if method not in self.routes:
            resp = error(405, 'Method not allowed')
        elif url not in self.routes[method]:
            resp = error(404, 'No such url: ' + url)
        else:
            handler = self.routes[method][url]
            payload = json.loads(payload) if payload else None
            resp = handler(payload)
        return json.dumps(resp)

    get = partialmethod(handle_req, method='get')
    post = partialmethod(handle_req, method='post')

    ''' Here are the actual routes '''

    def get_users(self, payload):
        users = self.db['users']
        if payload:
            if 'users' not in payload:
                return error(400, 'Malformed request')
            users = [
                u for u in users
                if u['name'] in payload['users']
            ]
        return {'users': sorted_by_name(users)}

    def post_add(self, payload):
        if not payload or 'user' not in payload:
            return error(400, 'Malformed request')
        user = self.fetch_user(payload['user'])
        if user:
            return error(400, 'Already exists.')
        user = {
            'name': payload['user'],
            'owes': {},
            'owed_by': {},
            'balance': 0
        }
        self.db['users'].append(user)
        return user

    def post_iou(self, payload):
        if not (payload and all(
            key in payload
            for key in ('lender', 'borrower', 'amount')
        )):
            return error(400, 'Malformed request')

        lender = self.fetch_user(payload['lender'])
        if not lender:
            return error(400, 'No such lender')

        borrower = self.fetch_user(payload['borrower'])
        if not borrower:
            return error(400, 'No such borrower')

        loan(lender, borrower, payload)
        return {'users': sorted_by_name([lender, borrower])}

    ''' utility methods '''

    def fetch_user(self, name):
        user = [
            u for u in self.db['users']
            if u['name'] == name
        ]
        return user[0] if user else None


def error(code, msg):
    return {'code': code, 'error': msg}


def sorted_by_name(users):
    return sorted(users, key=lambda user: user['name'])


def loan(lender, borrower, payload):
    amount = payload['amount']
    borrower_owes_lender = True

    if borrower['name'] in lender['owes']:
        borrower_owes_lender = False
        if amount < lender['owes'][borrower['name']]:
            lender['owes'][borrower['name']] -= amount
            borrower['owed_by'][lender['name']] -= amount
        else:
            if amount > lender['owes'][borrower['name']]:
                amount -= lender['owes'][borrower['name']]
                borrower_owes_lender = True
            del lender['owes'][borrower['name']]
            del borrower['owed_by'][lender['name']]

    if borrower_owes_lender:
        if borrower['name'] not in lender['owed_by']:
            lender['owed_by'][borrower['name']] = 0
        lender['owed_by'][borrower['name']] += amount
        if lender['name'] not in borrower['owes']:
            borrower['owes'][lender['name']] = 0
        borrower['owes'][lender['name']] += amount

    lender['balance'] += payload['amount']
    borrower['balance'] -= payload['amount']
