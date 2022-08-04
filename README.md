# Segmented Prime Sieve

A prime generator using a segmented Sieve of Eratosthenes.

Mostly just a way to learn assembly.

Run (on macOS) with:

```shell
nasm -fmacho64 primes.asm && gcc primes.o && ./a.out
```

Generates the primes up to 100M in ~0.5s.
Compile with `-DSQRT_SIZE=<size>` to generate primes up to `SQRT_SIZE**2`.
