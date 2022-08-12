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

## Algorithm

### Segmentation

To seive all primes up to `LIMIT` we only need `sqrt(LIMIT)` primes to do the sieving. To reduce memory, we can also process the rest of the numbers in segments of ~`sqrt(LIMIT)` without changing the time complexity.

In this program `SEGMENT_SIZE` is chosen to be smallest power of 2 larger than `sqrt(LIMIT)`.

### Prime generation

Note: In these steps, sieving of factors always starts from the square of the primes. All smaller factors would have been sieved by smaller primes.

Prime generation is split up into 3 phases.

1. Generate the wheel template.

    * Find and sieve as prime while their product does not exceed `SEGMENT_SIZE`.
    * The sieved segment is saved as a template.
      This template is used to initialize the sieve for each subsequent segment so the wheel primes don't need to be explicitly sieved.

2. Generate the sieve primes.

    * Find primes and sieve primes up to `sqrt(SEGMENT_SIZE)`.
    * Find the remaining primes in the first segment. These do not need to be sieved.
    * Store all seive primes in an array along with the smallest factor that has yet to be sieved.

3. Generate the rest of the primes. \
   For each segment:

   * Initialize the segment by copying the wheel template. In general, the segment will be some offset into the wheel.
   * Determine how many sieve primes are relevant to this segment. This way we can ignore sieve primes which are too large to affect the segment.
   * Sieve the sieve primes from the segment, and update their sieve factor.

### Output

Primes are written to `stdout` at the end of each segment (including the first). Primes are found by scanning the segment for any unsieved numbers. These are converted to strings and added to a buffer array before signaling the writer thread to write the output.

The conversion to string is done without any division or multiplication operations.

Before processing the segments:

* Create a 16-byte buffer on the stack which will represent the Binary-Coded Decimal (BCD) representation of the prime. Each byte corresponds to a decimal digit, encoded in binary. This will allow us to generate strings up to 16 characters long.
* Initialize the buffer to `0x01` - this represents the number `1`.
* Initilize a variable with the current buffer in ordinary binary also to `1`.
* Create a lookup table for `n < 1200` to its 4-byte BCD representation. This is sufficient to represent all prime gaps up to the programs limit.

For each prime:

* Calculate its delta from the current buffer value (as we store its binary representation also).
* Lookup the delta in the BCD lookup table.
* Add the BCD delta to the buffer. This can be done without multiplication, division or branching.
* Copy the buffer and convert to an ASCII string:
  * Reverse the order of digits.
  * Shift the copy to eliminate leading zeros.
  * Add `48` (ASCII code for `'0'`) to each byte.
* Write the copy into the print buffer, and add a newline afterwards.
