# square
def square(x: int):
    return x * x

# factorial
def factorial(x: int):
    if x < 0:
        return -1
    elif x == 0:
        return 1
    else:
        return x * factorial(x - 1)
