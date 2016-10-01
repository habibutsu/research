'''
viterbi.png

SPEECH and LANGUAGE PROCESSING

An Introduction to Natural Language Processing,
Computational Linguistics, and Speech Recognition
Second Edition

Usefull links:

http://www.machinelearning.ru/wiki/images/8/83/GM12_3.pdf
http://www.kanungo.com/software/hmmtut.pdf
http://logic.pdmi.ras.ru/~sergey/teaching/asr/09-hmm.pdf
http://iitp.ru/upload/userpage/146/2_hmm.pdf

'''
states = ('Healthy', 'Fever')

observations = ('normal', 'cold', 'dizzy')

start_probability = {'Healthy': 0.6, 'Fever': 0.4}

transition_probability = {
   'Healthy' : {'Healthy': 0.7, 'Fever': 0.3},
   'Fever' : {'Healthy': 0.4, 'Fever': 0.6}
   }

emission_probability = {
   'Healthy' : {'normal': 0.5, 'cold': 0.4, 'dizzy': 0.1},
   'Fever' : {'normal': 0.1, 'cold': 0.3, 'dizzy': 0.6}
   }

# Helps visualize the steps of Viterbi.
def print_dptable(V):
    s = "    " + " ".join(("%7d" % i) for i in range(len(V))) + "\n"
    for y in V[0]:
        s += "%.8s: " % y
        s += " ".join("%.8s" % ("%f" % v[y]) for v in V)
        s += "\n"
    print(s)


def viterbi(S, pi, Y, A, B):
    '''
    Obsrervation space: O = {o1,o2,... ,oN}
    State space: S = {s1,s2,... ,sK}
    Sequence of observations: Y = {y1,y2,... ,yT}
    Transition matrix: A of size K x K
    Emission matrix: B of size K x N

    A.ij - stores the transition probability of transiting from state `s.i` to state `s.j`
    B.ij - stores the probability of observing `oj` from state `si`,
           an array of initial probabilities `pi` of size `K` such
           that `pi.i` stores the probability that x1 == s.i

    Ppath X = {x1,x2,... ,xT} is a sequence of states that generate the observations
    Y = {y1,y2,... ,yT}

    '''
    V = [{}]
    X = {}

    # Initialize base cases (t == 0)
    for k in S:
        V[0][k] = pi[k] * B[k][Y[0]]
        X[k] = [k]

    # Run Viterbi for t > 0
    for t in range(1, len(Y)):
        V.append({})
        newpath = {}

        for st in states:
            tmp = [(V[t-1][prev_st] * A[prev_st][st] * B[st][Y[t]], prev_st) for prev_st in states]
            (prob, state) = max(tmp)
            V[t][st] = prob
            newpath[st] = X[state] + [st]

        # Don't need to remember the old paths
        X = newpath

    print_dptable(V)
    (prob, state) = max((V[t][st], st) for st in states)
    return (prob, X[state])

result = viterbi(
    states,
    start_probability,
    observations,
    transition_probability,
    emission_probability)
print(result)

# http://neerc.ifmo.ru/wiki/index.php?title=Алгоритм_Витерби
# https://nlpub.ru/Алгоритм_Витерби
def new_matrix(n, m):
    return [[0 for j in range(m)] for i in range(n)]

class S:
    Healthy = 0
    Fever = 1

    __all__ = [Healthy, Fever]

class O:
    normal = 0
    cold = 1
    dizzy = 2

    __all__ = [normal, cold, dizzy]

K = len(S.__all__)
N = len(O.__all__)

Y = [O.normal, O.cold, O.dizzy]
T = len(Y)

A = new_matrix(K, K)
A[S.Healthy][S.Healthy] = 0.7
A[S.Healthy][S.Fever] = 0.3

A[S.Fever][S.Healthy] = 0.4
A[S.Fever][S.Fever] = 0.6


B = new_matrix(K, N)
B[S.Healthy][O.normal] = 0.5
B[S.Healthy][O.cold] = 0.4
B[S.Healthy][O.dizzy] = 0.1

B[S.Fever][O.normal] = 0.1
B[S.Fever][O.cold] = 0.3
B[S.Fever][O.dizzy] = 0.6

pi = [0, range(K)]

pi[S.Healthy] = 0.6
pi[S.Fever] = 0.4

def arg_max(array):
    return array.index(max(array))

def my_viterbi(S, pi, Y, A, B):
    '''
    Implementation from wikipedia https://en.wikipedia.org/wiki/Viterbi_algorithm
    '''
    T = len(Y)
    K = len(S.__all__)
    X = [0 for i in range(T)]
    TState = new_matrix(K, T)
    TIndex = new_matrix(K, T)

    for i in range(K):
        TState[i][0] = pi[i] * B[i][Y[0]]
        TIndex[i][0] = 0

    for i in range(1, T):
        for j in range(K):
            tmp = [TState[k][i-1] * A[k][j]* B[j][Y[i]] for k in range(K)]
            TState[j][i] = max(tmp)
            TIndex[j][i] = arg_max(tmp)

    tmp = [TState[k][T-1] for k in range(K)]
    X[T-1] = arg_max(tmp)
    for i in list(reversed(range(1, T))):
        X[i-1] = TIndex[X[i]][i]

    for row in TState: print("\t%s" % "  ".join(map(str, row)))
    return X

print("")
result = my_viterbi(S, pi, Y, A, B)
print('result: ', result)



'''
https://www.quora.com/What-is-the-best-Python-library-for-Hidden-Markov-Models

!!
https://www.researchgate.net/publication/235958269_The_Viterbi_algorithm_demystified

https://web.stanford.edu/~jurafsky/slp3/8.pdf
(https://web.stanford.edu/~jurafsky/slp3/)

!!
http://www.uio.no/studier/emner/matnat/ifi/INF4820/h11/undervisningsmateriale/20111014-notes.pdf
'''