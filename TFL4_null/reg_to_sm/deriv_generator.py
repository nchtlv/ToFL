import sys
from reg_to_sm import functions

priority = {'(': 0, '#': 1, '|': 2, '·': 3, '*': 4}


def makeConcat(regex):
    specialCurrent = ')*#|'
    specialPrev = '|(#'
    if len(regex) == 0:
        return 'ε'
    augmented = regex[0]
    for i in range(1, len(regex)):
        if (not regex[i - 1] in specialPrev) and (not regex[i] in specialCurrent):
            augmented += '·' + regex[i]
        else:
            augmented += regex[i]
    return augmented


class TreeNode:
    def __init__(self, data, left=None, right=None):
        self.val = data
        self.left = left
        self.right = right


c = 0


def postorder(root):
    global c
    if root is None:
        return
    postorder(root.left)
    postorder(root.right)
    if root.val in '|·*#':
        if root.val == '|':

            if root.left.val.isalpha() and root.right.val.isalpha():
                aa = []
                aa.append(root.left.val)
                aa.append(root.right.val)
                aa.sort()
                root.left.val = aa[0]
                root.right.val = aa[1]
            if root.left.val == '∅':
                root.val = root.right.val
                root.left = root.right.left
                root.right = root.right.right

            elif root.right.val == '∅':
                root.val = root.left.val
                root.right = root.left.right
                root.left = root.left.left

            elif functions.sameTree(root.right, root.left):
                dnode = clone(root.left)
                root.val = dnode.val
                root.left = dnode.left
                root.right = dnode.right

            elif (root.left.val == '|' and (
                    functions.sameTree(root.right, root.left.left) or functions.sameTree(root.right,
                                                                                         root.left.right))) or functions.sameTree(
                root.right, root.left):
                temp = clone(root.left)
                root.left = temp.left
                root.right = temp.right
                root.val = temp.val

        elif root.val == '·':
            if root.left.val == '∅' or root.right.val == '∅':
                root.val = '∅'
                root.left = None
                root.right = None

            elif root.right.val == 'ε':
                root.val = root.left.val
                root.left = root.left.left
                root.right = root.left.right

            elif root.left.val == 'ε':

                root.val = root.right.val
                root.left = root.right.left
                root.right = root.right.right
        elif root.val == '#':
            if root.left.val == 'ε':
                # root = root.right
                root.val = root.right.val
                root.left = root.right.left
                root.right = root.right.right
            elif root.right.val == 'ε':
                # root = root.left
                root.val = root.left.val
                root.right = root.left.right
                root.left = root.left.left
            elif root.right.val == '∅' or root.left.val == '∅':
                # print("HERE")
                root.val = '∅'
                root.left = None
                root.right = None
        elif root.val == '*':
            if root.left.val == '*':
                dnode = clone(root.left.left)
                root.left = dnode


def inorder(root):
    if root is None:
        return ''
    if root.val == '*':
        return "(" + inorder(root.left) + ")*"
    if root.val != '·':
        l = inorder(root.left)
        out = l + root.val
        r = inorder(root.right)
        out += r
    else:
        l = inorder(root.left)
        out = inorder(root.left)
        r = inorder(root.right)
        out += r

    if root.val in '·#|*':
        return '(' + out + ')'
    return out


def getPostfix(regex):
    stack = []
    output = ''
    for c in regex:
        if c.isalpha():
            output += (c)
        elif c == "(":
            stack.append(c)
        elif c == ")":
            while len(stack) > 0 and stack[-1] != "(":
                output += (stack.pop())
            else:
                stack.pop()
        else:
            while len(stack) > 0 and priority[stack[-1]] >= priority[c]:
                output += (stack.pop())
            stack.append(c)

    while len(stack) > 0:
        output += (stack.pop())
    return output


def getBinaryTree(postfix):
    if not postfix:
        return
    stack = []
    for c in postfix:
        if c in "#|·":
            r, l = stack.pop(), stack.pop()
            stack.append(TreeNode(c, l, r))
        elif c in "*":
            l = stack.pop()
            stack.append(TreeNode(c, l))
        else:
            stack.append(TreeNode(c))

    return stack[-1]


def clone(node):
    if node is None:
        return None
    return TreeNode(node.val, clone(node.left), clone(node.right))


def lambda_func(node):
    if node is None:
        return False
    elif node.val == 'ε' or node.val == '*':
        return True
    elif node.val == '·' or node.val == '#':
        return lambda_func(node.left) and lambda_func(node.right)
    elif node.val == '|':
        return lambda_func(node.left) or lambda_func(node.right)
    else:
        return False


def brzozowski(root, c):
    stack = [root]
    while len(stack) > 0:
        node = stack.pop()
        if not node is None:
            if node.val == c:
                node.val = 'ε'
            elif node.val == 'ε':
                node.val = '∅'
            elif node.val == '∅':
                continue
            elif node.val == "*":
                star = clone(node)
                node.val = "·"
                node.right = star
                stack.append(node.left)
            elif node.val == "·":
                if lambda_func(node.left):
                    node.val = "|"
                    dnode = TreeNode("·", node.left, node.right)
                    node.left = dnode
                    node.right = clone(dnode.right)
                    stack.append(node.left.left)
                    stack.append(node.right)
                else:
                    stack.append(node.left)
            elif node.val == "|":
                stack.append(node.left)
                stack.append(node.right)
            elif node.val == "#":
                node.val = '|'
                llnode = TreeNode('#', node.left, node.right)
                dnode = TreeNode('#', clone(node.left), clone(node.right))

                node.left = llnode
                node.right = dnode
                stack.append(node.left.left)
                stack.append(node.right.right)
            else:
                node.val = '∅'
    return root


def getDerived(regex, c):
    node = (getBinaryTree(getPostfix(makeConcat(regex))))
    postorder(node)

    node = functions.makeLeftSided(node)

    postorder(node)

    result = inorder(node)

    a = brzozowski(node, c)
    postorder(a)
    result = inorder(a)
    a = functions.makeLeftSided(a)
    postorder(a)
    result = inorder(a)
    return result


if __name__ == "__main__":
    getDerived(sys.argv[1], sys.argv[2])
