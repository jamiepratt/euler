This is a deps.edn project so jack-in should work with this command in calva for example:

clojure -Sdeps '{:deps {nrepl/nrepl {:mvn/version,"1.0.0"},cider/cider-nrepl {:mvn/version,"0.28.5"}}}' -M:dev:portal -m nrepl.cmdline --middleware "[cider.nrepl/cider-middleware]"

After running command you should see a port number output to the terminal, open in your Clojure editor of choice.

# TODO

test_bed.clj is incomplete but is intended to run all the versions of the code and see to how fast it is to calculate up to 10^8 of the series if it can do that before a time out.

## Further idea for a possible improvement of the algorithm:

For series to 100 for example

- first prime is 2
- 3 is prime
- Therefor 3 * 2 is not prime
- 3 * 2 * 2 is not prime
- etc. for all combinations of factors of 2 and 3 up to 100.
- So we already know 4 is not prime but
- 5 is prime. 
- Therefore 2 * 5 is not prime
- 2 * 2 * 5 ....
- We then iterate through combinations of all primes we know of saw far plus at least one factor of 5. etc.
- And on to 7 because we have already found 6 is not prime.
- We will iterate through all integers just once since each unique combination of powers of primes is a different integer.
- Etc.

An advantage of doing the sieve this way is that we can easily calculate the smallest-m as we go along. Since for each step of the iteration we know the smallest m for the product of a previous combination of primes and are multiplying that by another prime, smallest m for this new product is easy to calculate as I have done in version 10 of my code.

The problem is that for a very long series of primes already discovered we may need to keep track of how many powers of each prime we have iterated through. This might be solved by just keeping track of the product and what prime to multiply by next or something like that.