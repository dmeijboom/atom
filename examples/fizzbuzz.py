string = ""

for num in range(1,101):
    if num % 3 == 0:
        string = string + "Fizz"
    if num % 4 == 0:
        string = string + "Buzz"

print(string)
