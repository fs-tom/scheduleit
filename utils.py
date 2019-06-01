import itertools
import random
import collections
import operator
from functools import *
from itertools import *

def first(t):  return t[0]
def second(t): return t[1]
def third(t): return t[2]

def groupby(keyf,xs):
    res= {}
    for x in xs:
        k= keyf(x)
        if k in res:
            res[k].append(x)
        else:
            res[k] = [x]
    return res

def sortby (keyf,xs):
    xs.sort(key=keyf)
    return xs

def columns(tbl):
    width = len(tbl[0])
    height = len(tbl)
    return ([tbl[i][j] for i in range(height)] for j in range(width))

def rows (tbl):
    return (r for r in tbl)

def cells(tbl):
   width = len(tbl[0])
   height = len(tbl)
   return ([((i,j), tbl[i][j]) for i in range(height)] for j in range(width))

def colreduce(tbl,f,init):
    return reduce(f,columns(tbl),init)

def rowreduce(tbl,f,init):
    return reduce(f,rows(tbl),init)


#helper function for converting a sequence of [fieldnames, values]
#to a set of records.
def asRecords(xs):
    fields = xs[0]
    def makerec(vals):
        return {fields[i] : vals[i] for i in range(fields.count)}
    return map(makerec,xs[1:])

def srecord(name,src, compo, home, qty):
    return {"NAME"  : name,
            "SRC"   : src,
            "COMPO" : compo,
            "QTY"   : qty,
            "HOME"   : home}

def drecord(name, src, compo, qty, start, duration, dtype, overlap):
    return {"NAME"       : name,
            "SRC"        : src,
            "COMPO"      : compo,
            "QTY"        : qty,
            "START"      : start,
            "DURATION"   : duration,
            "DEMANDTYPE" : dtype,
            "OVERLAP"    : overlap,
            "DemandGroup" : dtype}

def distinctby(keyf,xs):
    return set(map(keyf,xs))

#wrapper around set, maybe useless.
def distinct(xs):
    return set(xs)
#transforms a list of pairs (l,r) into a list of (l r ..ln rn)
def unzip(pairs):
    res = []
    for l,r in pairs:
        res.append(l)
        res.append(r)
    return res

def rest(xs): return xs[1:]

def butlast(xs):
    return xs[:len(xs) - 1]

#from itertools recipes:
def pairwise(iterable):
    "s -> (s0,s1), (s1,s2), (s2, s3), ..."
    a, b = itertools.tee(iterable)
    next(b, None)
    return zip(a, b)

def nassoc(d,k,v):
    d[k] = v
    return d

def nassocP(d,kv):
    (k,v) = kv
    d[k] = v
    return d

def assoc(d,k,v, *kvs):
    res = d.copy()
    res[k] = v
    for k,v in pairwise(kvs):
        res[k] = v
    return res

def assocP(d,kv, *kvs):
    res = d.copy()
    k,v = kv
    res[k] = v
    for k,v in kvs:
        res[k] = v
    return res

def merge (*dicts):
    res = {}
    for d in dicts:
        reduce(lambda acc, kv: nassocP(acc,kv),d.items(),res)
    return res

def is_dict(x):
    return isinstance(x,dict)

##COW functions that return copies of collections.
#conjoin multiple items into a set.
def conj(s,x, *xs):
    if isdict(s):
        return assocP(s,x,xs)
    res = s.copy()
    res.add(s)
    reduce(lambda acc,x: res.add(x), xs, res)
    return res

#alias for itertools.chain
def concat(*collections):
    return chain(collections)

def mapcat(f,*collections):
    return concat(map(f,collections))

def unions(xs): return reduce(lambda l,r: l.union(r),xs)

def getKey(k):
    return lambda x : x[k]

def get(m,k):
    return m[k]
def get_keys(m,*ks):
    return map(lambda k: get(m,k),ks)

#itertools recipes.
def take(n, iterable):
    "Return first n items of the iterable as a list"
    return list(islice(iterable, n))

def tabulate(function, start=0):
    "Return function(0), function(1), ..."
    return map(function, count(start))


def consume(iterator, n):
    "Advance the iterator n-steps ahead. If n is none, consume entirely."
    # Use functions that consume iterators at C speed.
    if n is None:
        # feed the entire iterator into a zero-length deque
        collections.deque(iterator, maxlen=0)
    else:
        # advance to the empty slice starting at position n
        next(islice(iterator, n, n), None)

def nth(iterable, n, default=None):
    "Returns the nth item or a default value"
    return next(islice(iterable, n, None), default)

def quantify(iterable, pred=bool):
    "Count how many times the predicate is true"
    return sum(map(pred, iterable))

def padnone(iterable):
    """Returns the sequence elements and then returns None indefinitely.

    Useful for emulating the behavior of the built-in map() function.
    """
    return chain(iterable, repeat(None))

def ncycles(iterable, n):
    "Returns the sequence elements n times"
    return chain.from_iterable(repeat(tuple(iterable), n))

def dotproduct(vec1, vec2):
    return sum(map(operator.mul, vec1, vec2))

def flatten(listOfLists):
    "Flatten one level of nesting"
    return chain.from_iterable(listOfLists)

def repeatfunc(func, times=None, *args):
    """Repeat calls to func with specified arguments.

    Example:  repeatfunc(random.random)
    """
    if times is None:
        return starmap(func, repeat(args))
    return starmap(func, repeat(args, times))

def grouper(iterable, n, fillvalue=None):
    "Collect data into fixed-length chunks or blocks"
    # grouper('ABCDEFG', 3, 'x') --> ABC DEF Gxx"
    args = [iter(iterable)] * n
    return zip_longest(*args, fillvalue=fillvalue)

def roundrobin(*iterables):
    "roundrobin('ABC', 'D', 'EF') --> A D E B F C"
    # Recipe credited to George Sakkis
    pending = len(iterables)
    nexts = cycle(iter(it).__next__ for it in iterables)
    while pending:
        try:
            for next in nexts:
                yield next()
        except StopIteration:
            pending -= 1
            nexts = cycle(islice(nexts, pending))

def partition(pred, iterable):
    'Use a predicate to partition entries into false entries and true entries'
    # partition(is_odd, range(10)) --> 0 2 4 6 8   and  1 3 5 7 9
    t1, t2 = tee(iterable)
    return filterfalse(pred, t1), filter(pred, t2)

def powerset(iterable):
    "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(len(s)+1))

def unique_everseen(iterable, key=None):
    "List unique elements, preserving order. Remember all elements ever seen."
    # unique_everseen('AAAABBBCCDAABBB') --> A B C D
    # unique_everseen('ABBCcAD', str.lower) --> A B C D
    seen = set()
    seen_add = seen.add
    if key is None:
        for element in filterfalse(seen.__contains__, iterable):
            seen_add(element)
            yield element
    else:
        for element in iterable:
            k = key(element)
            if k not in seen:
                seen_add(k)
                yield element

# def unique_justseen(iterable, key=None):
#     "List unique elements, preserving order. Remember only the element just seen."
#     # unique_justseen('AAAABBBCCDAABBB') --> A B C D A B
#     # unique_justseen('ABBCcAD', str.lower) --> A B C A D
#     return map(next, map(itemgetter(1), groupby(iterable, key)))

def iter_except(func, exception, first=None):
    """ Call a function repeatedly until an exception is raised.

    Converts a call-until-exception interface to an iterator interface.
    Like builtins.iter(func, sentinel) but uses an exception instead
    of a sentinel to end the loop.

    Examples:
        iter_except(functools.partial(heappop, h), IndexError)   # priority queue iterator
        iter_except(d.popitem, KeyError)                         # non-blocking dict iterator
        iter_except(d.popleft, IndexError)                       # non-blocking deque iterator
        iter_except(q.get_nowait, Queue.Empty)                   # loop over a producer Queue
        iter_except(s.pop, KeyError)                             # non-blocking set iterator

    """
    try:
        if first is not None:
            yield first()            # For database APIs needing an initial cast to db.first()
        while 1:
            yield func()
    except exception:
        pass

def first_true(iterable, default=False, pred=None):
    """Returns the first true value in the iterable.

    If no true value is found, returns *default*

    If *pred* is not None, returns the first item
    for which pred(item) is true.

    """
    # first_true([a,b,c], x) --> a or b or c or x
    # first_true([a,b], x, f) --> a if f(a) else b if f(b) else x
    return next(filter(pred, iterable), default)

##def random_product(*args, repeat=1):
##    "Random selection from itertools.product(*args, **kwds)"
##    pools = [tuple(pool) for pool in args] * repeat
##    return tuple(random.choice(pool) for pool in pools)

def random_permutation(iterable, r=None):
    "Random selection from itertools.permutations(iterable, r)"
    pool = tuple(iterable)
    r = len(pool) if r is None else r
    return tuple(random.sample(pool, r))

def random_combination(iterable, r):
    "Random selection from itertools.combinations(iterable, r)"
    pool = tuple(iterable)
    n = len(pool)
    indices = sorted(random.sample(range(n), r))
    return tuple(pool[i] for i in indices)

def random_combination_with_replacement(iterable, r):
    "Random selection from itertools.combinations_with_replacement(iterable, r)"
    pool = tuple(iterable)
    n = len(pool)
    indices = sorted(random.randrange(n) for i in range(r))
    return tuple(pool[i] for i in indices)
