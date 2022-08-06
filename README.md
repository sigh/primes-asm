# Segmented Prime Sieve

A prime generator using a segmented Sieve of Eratosthenes.

Mostly just a way to learn assembly.

Run (on macOS) with:

```shell
nasm -fmacho64 primes.asm && gcc primes.o && ./a.out
```

By default generates all 32-bit primes (~5 seconds).

Compile with `-DSQRT_SIZE=<size>` to generate primes up to `SQRT_SIZE**2`.
`SQRT_SIZE` must be 32-bit, but the primes generated can be larger.
