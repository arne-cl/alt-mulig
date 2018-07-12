from nltk.tree import ParentedTree


def t(root, children=None):
    "Create nltk.tree.ParentedTree from a root (str) and a list of (str, list) tuples."
    assert isinstance(root, basestring)

    # Beware: a ParentedTree is also a list!
    if isinstance(children, ParentedTree):
        child_trees = [children]
 
    elif isinstance(children, list):
        child_trees = []
        for child in children:
            if isinstance(child, ParentedTree):
                child_trees.append(child)
            else:  #isinstance(child, tuple)
                child_trees.append(t(*child))
    elif isinstance(children, basestring):
        # this tree does only have one child, a leaf node
        child_trees = [children]
    else:  #children == None
        # this tree only consists of one leaf node
        child_trees = []
    return ParentedTree(root, child_trees)

foo = t("foo")
x = t(root="bar", children=foo)
