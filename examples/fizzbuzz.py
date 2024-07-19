string = ""

for i in range(0, 101):
    if i % 3 == 0:
        string = string + "Fizz"

    if i % 4 == 0:
        string = string + "Buzz"

print(string)
