# Prime Sieve

A prime generator using a segmented Sieve of Eratosthenes.

Mostly just a way to learn assembly.

Run (on macOS) with:

```shell
nasm -fmacho64 primes.asm && gcc primes.o && ./a.out
```

Generates the first 1e8 primes in 1s.
Change `SEGMENT_SIZE` to change the number of primes generated
(`SEGMENT_SIZE**2` primes are generated).
