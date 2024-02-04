import sys
import re
from collections import defaultdict
from itertools import product


from cfg_to_cnf.to_chomsky import prepare_grammar
from reg_to_sm.r2s import prepare_regex_to_sm


def get_base_rules(gr_term, gr_nonterm, grammar, initial, states, transitions, final_states):
    term = set()
    nonterm = set()
    new_grammar = []
    for left, right in grammar:
        for rule in right:
            if rule in gr_term:
                for t in transitions:
                    if t['trigger'] == rule:
                        nonterm_name = f'<{t["source"]}{left}{t["dest"]}>'
                        new_grammar.append((nonterm_name, [rule]))
                        nonterm.add(nonterm_name)
                        term.add(rule)

    return term, nonterm, new_grammar

def generate_all_common_rules(gr_term, gr_nonterm, grammar, transitions):
    states = set()
    new_grammar = []
    nonterm = set()
    for t in transitions:
        states.add(t['source'])
        states.add(t['dest'])

    for left, right in grammar:
        rule = [i for i in right if i in gr_nonterm]
        k = len(rule)
        if k <= 1:
            continue
        for pair in product(states, repeat=2):
            for mid_pairs in product(states, repeat=k-1):
                left_name = f'<{pair[0]}{left}{pair[1]}>'
                pairs = [pair[0]] + list(mid_pairs) + [pair[1]]
                right_names = []
                assert len(pairs[:-1]) == len(pairs[1:]) == len(rule)
                for left_, right_, nont in zip(pairs[:-1], pairs[1:], rule):
                    right_names.append(f'<{left_}{nont}{right_}>')
                new_grammar += [(left_name, right_names)]
                nonterm.add(left_name)
                nonterm = nonterm.union(right_names)

    return nonterm, new_grammar


def dfs(start, graph, visited):
    visited.add(start)
    for point in graph[start] - visited:
        dfs(point, graph, visited)

    return visited


def remove_rules(grammar, usless_nonterm):
    new_grammar = []
    for left, right in grammar:
        add = True
        if left in usless_nonterm:
            add = False
        for rule in right:
            if rule in usless_nonterm:
                add = False
        if add:
            new_grammar.append((left, right))

    return new_grammar


def remove_useless_rules(start, nonterm, grammar):
    graph = defaultdict(set)
    for left, right in grammar:
        for rule in right:
            graph[left].add(rule)

    visited = set()
    dfs(start, graph, visited)

    useless = nonterm - visited

    new_grammar = []
    for left, right in grammar:
        if left in useless or len(graph[left].intersection(useless)) > 0:
            continue
        new_grammar.append((left, right))

    return visited, new_grammar


def remove_nongenerative_grammar(base_nont, grammar, nonterm, start):
    old_cnt = len(base_nont)
    new_cnt = -1
    while old_cnt != new_cnt:
        old_cnt = len(base_nont)
        for left, right in grammar:
            r_set = set(right)
            if r_set.issubset(base_nont):
                base_nont.add(left)
        new_cnt = len(base_nont)

    useless = nonterm - base_nont
    grammar = remove_rules(grammar, useless)

    nonterm, grammar = remove_useless_rules(start, base_nont, grammar)
    print('Intersection:')
    for left, right in grammar:
        print(f'{left} -> {"".join(right)}')

    return nonterm, grammar


def intersection(gr_term, gr_nonterm, grammar, initial, states, transitions, final_states):
    term, nonterm, new_grammar = get_base_rules(gr_term, gr_nonterm, grammar, initial, states, transitions, final_states)
    # print('Base rules:')
    # print(term, nonterm, sep='\n')
    # print(prettyForm(new_grammar))

    common_nonterm, common_grammar = generate_all_common_rules(gr_term, gr_nonterm, grammar, transitions)
    # print('All common:')
    # print(common_nonterm)
    # for left, right in common_grammar:
    #     print(f'{left} -> {"".join(right)}')

    start = f'<{min(states)}S{max(states)}>'
    nonterm, grammar = remove_nongenerative_grammar(nonterm, new_grammar + common_grammar, nonterm | common_nonterm, start)
    if len(grammar) > 0:
        print('Язык G не полностью покрывается языком R')

    return term, nonterm, grammar

def get_regexp_words(regexp, k, alphabet):
    pattern = fr'{regexp}'
    reg_words, add = set(), set()
    for i in range(1, k + 1):
        for word_tup in product(alphabet, repeat=i):
            word = ''.join(word_tup)
            if re.fullmatch(pattern, word):
                reg_words.add(word)
            else:
                add.add(word)

    return reg_words, add


def get_regexp(path):
    with open(path, 'r') as f:
        res = f.read().split('\n')

    regexp = res[1]
    chars = list(res[0])

    return regexp, chars


def grammar_req(term, nonterm, grammar, k, word, word_set):
    tlen = 0
    for w in word:
        tlen += int(w in term)
    if tlen == len(word):
        word_set.add(''.join(word))
    elif tlen < k and len(word) <= k:
        for idx, val in enumerate(word):
            if val in nonterm:
                for left, right in grammar:
                    if left == val:
                        if idx == len(word) - 1:
                            new_word = word[:idx] + right
                        else:
                            new_word = word[:idx] + right + word[(idx + 1):]
                        word_set = grammar_req(term, nonterm, grammar, k, new_word, word_set)

    return word_set


def get_grammar_words(term, nonterm, grammar, k):
    word_set = set()
    word_set = grammar_req(term, nonterm, grammar, k, ['S'], word_set)
    return word_set


def compress_word(word):
    curr_letter = word[0]
    cnt = 1
    new = ''
    for l in word[1:]:
        if l == curr_letter:
           cnt += 1
        else:
            if cnt == 1:
                new += curr_letter
            else:
                new += f'{curr_letter}{cnt}'
            curr_letter = l
            cnt = 1

    if cnt == 1:
        new += curr_letter
    else:
        new += f'{curr_letter}{cnt}'

    return new


def main():
    k = int(sys.argv[3])
    print(f'k = {k}')
    gr_term, gr_nonterm, gr_grammar = prepare_grammar(sys.argv[1], True)
    initial, states, transitions, final_states = prepare_regex_to_sm(sys.argv[2], True)
    term, nonterm, grammar = intersection(gr_term, gr_nonterm, gr_grammar, initial, states, transitions, final_states)

    regex, chars = get_regexp(sys.argv[2])
    reg_words, _ = get_regexp_words(regex, k, chars)
    grammar_words = get_grammar_words(gr_term, gr_nonterm, gr_grammar, k)
    print(reg_words)
    print(grammar_words)
    intersection_ = sorted(sorted(reg_words & grammar_words), key=len)

    print(f'Intersection G и R: {",".join([compress_word(i) for i in intersection_])}')


if __name__ == '__main__':
    main()

