from reg_to_sm import FSMgenerator


def isin(arr, el):# el in arr
    for a in arr:
        if a == el:
            return True
    return False


def rename(transitions, states, endstates, initial):
    states.sort()
    newstates = []
    newendstates = []
    counter = 0
    for i in range(len(states)):
        if initial == states[i]:
            initial = counter
        newstates.append(counter)
        for j in range(len(transitions)):
            if transitions[j].get('source') == states[i]:
                if isin(endstates, transitions[j].get('source')) and not isin(newendstates, counter):
                    newendstates.append(counter)
                transitions[j].update({'source': counter})

            if transitions[j].get('dest') == states[i]:
                if isin(endstates, transitions[j].get('dest')) and not isin(newendstates, counter):
                    newendstates.append(counter)
                transitions[j].update({'dest': counter})
        counter += 1
    return transitions, newstates, newendstates, initial


def negation(initial, states, transitions, endstates):
    newendstates = []
    for i in range(len(states)):
        if not isin(endstates, states[i]):
            newendstates.append(states[i])
    return initial, states, transitions, newendstates


def reg_to_sm(chars, regex):
    final_states, states, transitions, initial = FSMgenerator.main(chars, regex)
    # print( final_states, states, initial)
    # for t in transitions:
    #     print(t)
    # print('=' *100)

    transitions, states, final_states, initial = rename(transitions, states, final_states, initial)
    # print(final_states, states, initial)
    # for t in transitions:
    #     print(t)
    # print('=' *100)

    initial, states, transitions, final_states = negation(initial, states, transitions, final_states)
    # print(final_states, states, initial)
    # for t in transitions:
    #     print(t)
    # print('=' *100)

    return initial, states, transitions, final_states


def read_regex(path, printable=False):
    with open(path, 'r') as f:
        result = f.read().split('\n')
    chars = list(result[0])
    regex = result[1]

    if printable:
        print(f'chars: {chars}\nregex: {regex}')

    return chars, regex


def prepare_regex_to_sm(path, printable=False):
    initial, states, transitions, final_states = reg_to_sm(*(read_regex(path, printable)))
    if printable:
        print('Regex state machine:')
        print(*transitions, sep='\n')
    return initial, states, transitions, final_states


if __name__ == '__main__':
    chars = ['a', 'b', 'c', 'd']
    regex = '((c(d)*)(b|(d#c)))'
    initial, states, transitions, final_states = reg_to_sm(chars, regex)
    print(initial, states, transitions, final_states, sep='\n')
