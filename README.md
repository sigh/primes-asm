# Segmented Prime Sieve

A prime generator using a segmented Sieve of Eratosthenes.

Mostly just a way to learn assembly.

Run (on macOS) with:

```shell
nasm -fmacho64 primes.asm && gcc primes.o && ./a.out
```

By default generates all 32-bit primes (~5 seconds).

Compile with `-DLIMIT=<limit>` to generate primes up to `LIMIT`. If
`LIMIT` is greater than 2^32 then `-DSEGMENT_HINT=<size>` must be increased. If `LIMIT > SEGMENT_HINT*SEGMENT_HINT` the program won't compile.
