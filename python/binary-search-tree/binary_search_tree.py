class BinarySearchTree(object):
    def __init__(self, tree_data):
        ''' push the real work into the node class '''
        self.root = TreeNode(tree_data.pop(0))
        for item in tree_data:
            self.root.insert(item)

    def data(self):
        return self.root

    def sorted_data(self):
        return [item for item in self.root.inorder()]


class TreeNode(object):
    def __init__(self, data, left=None, right=None):
        self.data = data
        self.left = left
        self.right = right

    def insert(self, item):
        if item <= self.data:
            if self.left:
                self.left.insert(item)
            else:
                self.left = TreeNode(item)
        else:
            if self.right:
                self.right.insert(item)
            else:
                self.right = TreeNode(item)

    def inorder(self):
        if self.left:
            for item in self.left.inorder():
                yield item
        yield self.data
        if self.right:
            for item in self.right.inorder():
                yield item

    def __str__(self):
        fmt = 'TreeNode(data={}, left={}, right={})'
        return fmt.format(self.data, self.left, self.right)
