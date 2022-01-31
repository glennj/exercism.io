export const treeFromTraversals = (preorder, inorder) => {
  validate(preorder, inorder);
  return tft(preorder, inorder);
}

const tft = (preorder, inorder) => {
  if (preorder.length === 0)
    return {};

  // the root of the tree is the first element of the preorder
  const root = preorder[0];

  // find the root in the inorder list so we can determine
  // the left and right branches
  const idx = inorder.indexOf(root)
  if (idx === -1)
    throw new Error('root not found in inorder list');

  const lIn = inorder.slice(0, idx);
  const rIn = inorder.slice(idx+1);

  const lPre = preorder.slice(1, idx+1);
  const rPre = preorder.slice(idx+1);

  return {
    value: root,
    left:  lIn.length === 0 ? {} : tft(lPre, lIn),
    right: rIn.length === 0 ? {} : tft(rPre, rIn),
  };
};


const validate = (preorder, inorder) => {
  if (preorder.length !== inorder.length)
    throw new Error('traversals must have the same length');

  for (const list of [preorder, inorder])
    if (list.length !== new Set(list).size)
      throw new Error('traversals must contain unique items');
  
  const pre_elems = preorder.slice().sort();
  inorder.slice().sort().forEach((elem, i) => {
    if (elem !== pre_elems[i])
      throw new Error('traversals must have the same elements');
  });
};
