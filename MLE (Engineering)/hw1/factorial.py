def factorial(n: int) -> int:
    if n < 0:
        return "Factorial does not exist for negative numbers"
    elif n == 0:
        return 1
    else:
        fact = 1
        while n > 1:
            fact *= n
            n -= 1
        return fact
