# Segmented Prime Sieve

A prime generator using a segmented Sieve of Eratosthenes.
By default generates all 32-bit primes (~5 seconds).

Mostly just a way to learn x86 assembly. Code is not portable.

## Compiling and running

Compile with [NASM](https://www.nasm.us/).
Run (on macOS) with:

```shell
nasm -fmacho64 primes.asm && gcc primes.o && ./a.out
```

Compile with `-DLIMIT=<limit>` to generate primes up to `LIMIT`. Maximum size of `LIMIT` is 10^16.

By default, a dedicated thread for writing the output. For smaller limits
(e.g. smaller than the default of 2^32), the overhead may add time.
To disable threading, compile with `-DTHREADING=0`.
