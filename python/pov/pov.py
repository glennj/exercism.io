from json import dumps


class Tree(object):
    def __init__(self, label, children=[]):
        self.label = label
        self.children = children

    def __dict__(self):
        return {self.label: [c.__dict__() for c in sorted(self.children)]}

    def __str__(self, indent=None):
        return dumps(self.__dict__(), indent=indent)

    def __lt__(self, other):
        return self.label < other.label

    def __eq__(self, other):
        return self.__dict__() == other.__dict__()

    #########################################################

    def from_pov(self, from_label):
        path = self.path_from_root(from_label)
        if not path:
            raise ValueError('Tree could not be reoriented')
        '''
        Now, reparent: follow the path, removing the node
        from its parent's children, and adding the
        node's parent as its own child.
        '''
        root = self.deepcopy()
        path.pop(0)
        while path:
            child_label = path.pop(0)
            # get first child with this label
            child = next(filter(
                lambda c: c.label == child_label,
                root.children
            ))
            root.children.remove(child)
            child.children.append(root)
            root = child
        return root

    def path_to(self, from_label, to_label):
        from_path = self.path_from_root(from_label)
        if not from_path:
            raise ValueError('Tree could not be reoriented')

        to_path = self.path_from_root(to_label)
        if not to_path:
            raise ValueError('No path found')

        from_path, to_path = common_root(from_path, to_path)
        from_path.reverse()
        to_path.pop(0)
        return from_path + to_path

    # Utility methods

    def path_from_root(self, to_label):
        '''
        Return a list of labels representing the path
        from the root to the specified node.
        '''
        path = [self.label]
        if to_label == self.label:
            return path
        for child in self.children:
            child_path = child.path_from_root(to_label)
            if child_path:
                return path + child_path
        return None

    def deepcopy(self):
        return Tree(
            self.label,
            [child.deepcopy() for child in self.children]
        )


'''
Remove common prefix elements up to the shared "root".
Given:
    a = [1,2,3,4,5]
    b = [1,2,3,6,7]
Return the tuple:
    ([3,4,5], [3,6,7])
'''
def common_root(path1, path2):
    min_len = min(len(path1), len(path2))
    if path1[0] != path2[0]:
        # no common prefix
        return path1, path2
    for i in range(1, min_len):
        if path1[i] != path2[i]:
            return path1[i-1:], path2[i-1:]
    # all elements are same up to min_len
    return path1[min_len-1:], path2[min_len-1:]
