import sys
import re
from itertools import product


def get_grammar(filepath):
    grammar = []
    with open(filepath, 'r') as f:
        file = f.read().split('\n')
    for line in file:
        if len(line) < 6:
            continue
        left, right = line.split(' -> ')
        grammar.append((left, right))

    return grammar


def remove_useless_rules(grammar):
    left_set, right_set = set(), set()
    for left, right in grammar:
        if left != 'S':
            left_set.add(left)
        for c in right:
            if c.isalpha() and c.isupper():
                right_set.add(c)

    used_non_terminals = (left_set & right_set)
    used_non_terminals.add('S')
    clear_grammar = [rule for rule in grammar if rule[0] in used_non_terminals]

    return clear_grammar


def grammar_to_print(grammar):
    s = ''
    for left, right in grammar:
        s += f'{left} -> {right}\n'

    return s


def get_regexp(path):
    with open(path, 'r') as f:
        regexp = f.read()

    return regexp


def get_regexp_words(regexp, k):
    pattern = fr'{regexp}'
    alphabet = ''.join([i for i in regexp if i.isalpha()])
    reg_words, add = set(), set()
    for i in range(1, k + 1):
        for word_tup in product(alphabet, repeat=i):
            word = ''.join(word_tup)
            if re.fullmatch(pattern, word):
                reg_words.add(word)
            else:
                add.add(word)

    return reg_words, add


def terminal_len(word):
    cnt = 0
    for letter in word:
        if letter.isalpha() and letter.islower():
            cnt += 1

    return cnt


def add_word(word_set, word, prew_nt, arrow):
    clear_word = ''
    if word[-1] != '!':
        return -1

    for idx, letter in enumerate(word):
        if letter.isalpha() and letter.islower():
            clear_word += letter
        elif (letter not in prew_nt) or (arrow < idx):
            return -1

    word_set.add(clear_word)
    return 0


def grammar_req(grammar, k, word, word_set, prew_nt, idx=0):
    tlen = terminal_len(word)
    if tlen == k:
        add_word(word_set, word, prew_nt, idx)
    elif tlen < k:
        if word != 'S':
            add_word(word_set, word, prew_nt, idx)
        for left, right in grammar:
            if left in word:
                idx = word.index(left) + len(right) - 1
                new_word = word.replace(left, right, 1)
                word_set = grammar_req(grammar, k, new_word, word_set, [left] + prew_nt, idx)

    return word_set

def get_grammar_words(grammar, k):
    word_set = set()
    word_set = grammar_req(grammar, k, 'S', word_set, ['!'])
    return word_set


def main():
    grammar = get_grammar(sys.argv[1])
    regexp = get_regexp(sys.argv[2])
    print(regexp)
    k = int(sys.argv[3])
    print(f'k = {k}')
    print(f'Grammar:\n{grammar_to_print(grammar)}')
    clear_grammar = remove_useless_rules(grammar)
    print(f'Clear grammar:\n{grammar_to_print(clear_grammar)}')
    grammar_words = get_grammar_words(grammar, k)
    print(f'grammar_words: {grammar_words}')

    print(f'regexp = "{regexp}"')
    reg_words, add = get_regexp_words(regexp, k)
    print(f'reg words: {reg_words}')
    print(f'Addition: {add}')

    if len(add & grammar_words) > 0:
        print('Язык G не полностью покрывается языком R')

    intersection = sorted(sorted(reg_words & grammar_words), key=len)

    print(f'Intersection G и R: {",".join(intersection)}')


if __name__ == '__main__':
    main()

