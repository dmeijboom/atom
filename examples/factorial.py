def factorial(n, acc):
    if n == 1:
        return acc

    return factorial(n - 1, n * acc)

print(factorial(15, 1))
