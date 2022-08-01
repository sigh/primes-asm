# Prime Sieve

A prime generator using the Sieve of Eratosthenes.

Mostly just a way to learn assembly.

Run (om macOS) with:

```
nasm -fmacho64 primes.asm && gcc primes.o && ./a.out
```

Generates the first 1e8 primes in 1s. Change `MAX_P` to change the number of
primes generated.
