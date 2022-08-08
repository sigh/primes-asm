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

Compile with `-DLIMIT=<limit>` to generate primes up to `LIMIT`.
Maximum size of `LIMIT` is 10^16.

If `LIMIT` is greater than 2^32 then `-DSEGMENT_HINT=<size>` must be set.
If `LIMIT > SEGMENT_HINT*SEGMENT_HINT` the program won't compile.
