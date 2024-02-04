from cfg_to_cnf import helper

left, right = 0, 1
variablesJar = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U",
                "W", "X", "Y", "Z"]


def isUnitary(rule, variables):
    if rule[left] in variables and rule[right][0] in variables and len(rule[right]) == 1:
        return True
    return False


def isSimple(rule, V, K):
    if rule[left] in V and rule[right][0] in K and len(rule[right]) == 1:
        return True
    return False


def TERM(productions, V, K):
    newProductions = []
    dictionary = helper.setupDict(productions, V, terms=K)
    for production in productions:
        if isSimple(production, V, K):
            newProductions.append(production)
        else:
            for term in K:
                for index, value in enumerate(production[right]):
                    if term == value and not term in dictionary:
                        dictionary[term] = variablesJar.pop()
                        V.append(dictionary[term])
                        newProductions.append((dictionary[term], [term]))

                        production[right][index] = dictionary[term]
                    elif term == value:
                        production[right][index] = dictionary[term]
            newProductions.append((production[left], production[right]))

    return newProductions


def BIN(productions, variables):
    result = []
    for production in productions:
        k = len(production[right])
        if k <= 2:
            result.append(production)
        else:
            newVar = variablesJar.pop(0)
            variables.append(newVar + '1')
            result.append((production[left], [production[right][0]] + [newVar + '1']))
            i = 1
            # TODO
            for i in range(1, k - 2):
                var, var2 = newVar + str(i), newVar + str(i + 1)
                variables.append(var2)
                result.append((var, [production[right][i], var2]))
            result.append((newVar + str(k - 2), production[right][k - 2:k]))
    return result


def DEL(productions):
    newSet = []
    outlaws, productions = helper.seekAndDestroy(target='e', productions=productions)
    for outlaw in outlaws:
        for production in productions + [e for e in newSet if e not in productions]:
            if outlaw in production[right]:
                newSet = newSet + [e for e in helper.rewrite(outlaw, production) if e not in newSet]

    return newSet + ([productions[i] for i in range(len(productions))
                      if productions[i] not in newSet])


def unit_routine(rules, variables):
    unitaries, result = [], []
    for aRule in rules:
        if isUnitary(aRule, variables):
            unitaries.append((aRule[left], aRule[right][0]))
        else:
            result.append(aRule)
    for uni in unitaries:
        for rule in rules:
            if uni[right] == rule[left] and uni[left] != rule[left]:
                result.append((uni[left], rule[right]))

    return result


def UNIT(productions, variables):
    i = 0
    result = unit_routine(productions, variables)
    tmp = unit_routine(result, variables)
    while result != tmp and i < 1000:
        result = unit_routine(tmp, variables)
        tmp = unit_routine(result, variables)
        i += 1
    return result



def get_cnf(K, V, grammar):
    grammar = TERM(grammar, V, K)
    grammar = BIN(grammar, V)
    grammar = DEL(grammar)
    grammar = UNIT(grammar, V)

    return K, V, grammar


def remove_useless_rules(V, grammar):
    left_set, right_set = set(), set()
    for left, right in grammar:
        if left != 'S':
            left_set.add(left)
        for c in right:
            if c in V and c != left:
                right_set.add(c)

    used_non_terminals = (left_set & right_set)
    used_non_terminals.add('S')
    clear_grammar = [rule for rule in grammar if rule[0] in used_non_terminals]

    return used_non_terminals, clear_grammar


def prepare_grammar(path, print_steps=False):
    K, V, grammar = helper.loadModel(path)
    if print_steps:
        print('Input data:')
        print(helper.prettyForm(grammar))
    K, V, grammar = get_cnf(K, V, grammar)
    if print_steps:
        print('CNF:')
        print(helper.prettyForm(grammar))
    V, grammar = remove_useless_rules(V, grammar)
    if print_steps:
        print('Clear CNF:')
        print(helper.prettyForm(grammar))

    return K, V, grammar

