class Fib:
    def run(self, n):
        if n < 2:
            return n

        return self.run(n - 1) + self.run(n - 2)

f = Fib();
print(f.run(30))
