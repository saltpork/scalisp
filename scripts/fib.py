import timeit

def fib(a):
    if (a < 2):
        return 1
    else:
        return fib(a - 1) + fib(a - 2)

print timeit.timeit('fib(20)', "from __main__ import fib", number = 10) / 10
print timeit.timeit('fib(30)', "from __main__ import fib", number = 10) / 10
print timeit.timeit('fib(40)', "from __main__ import fib", number = 3) / 3
