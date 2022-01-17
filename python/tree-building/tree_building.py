class Record():
    def __init__(self, record_id, parent_id):
        self.record_id = record_id
        self.parent_id = parent_id

    def __repr__(self):
        return f'Record({self.record_id},{self.parent_id})'


class Node():
    def __init__(self, node_id):
        self.node_id = node_id
        self.children = []

    def __repr__(self):
        children = ', '.join(c.__repr__() for c in self.children)
        return f'Node({self.node_id},[{children}])'


def BuildTree(records):
    if not records:
        return
    records.sort(key=lambda rec: rec.record_id)
    validate_records(records)
    nodes = [Node(0)]
    for record in records[1:]:
        id, pid = record.record_id, record.parent_id
        nodes.append(Node(id))
        nodes[pid].children.append(nodes[id])
    return nodes[0]


def validate_records(rs):
    if any(r.record_id != i for i, r in enumerate(rs)):
        raise ValueError('Record id is invalid or out of order.')
    if rs[0].parent_id != 0:
        raise ValueError('Node record_id should be smaller than it\'s parent_id.')
    if any(r.record_id == r.parent_id for r in rs if r.record_id != 0):
        raise ValueError('Only root should have equal record and parent id.')
    if any(r.record_id < r.parent_id for r in rs if r.record_id != 0):
        raise ValueError('Node record_id should be smaller than it\'s parent_id.')
