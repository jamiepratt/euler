This is a deps.edn project so jack-in should work with this command in calva for example:

clojure -Sdeps '{:deps {nrepl/nrepl {:mvn/version,"1.0.0"},cider/cider-nrepl {:mvn/version,"0.28.5"}}}' -M:dev:portal -m nrepl.cmdline --middleware "[cider.nrepl/cider-middleware]"

After running command you should see a port number output to the terminal, open in your Clojure editor of choice.

# TODO

test_bed.clj is incomplete but is intended to run all the versions of the code and see to how fast it is to calculate up to 10^8 of the series if it can do that before a time out.

## Further idea for a possible improvement of the algorithm:

For series to 100 for example

1. Start with n is 2 and least-m-series is a range from 0 to max-m + 1 ie. start with same value in all cells as the key for that cell.
2. We test if n is prime if the value in the least-m-series is still the same as the key then m is prime (excpeot for 4 which we ignore). If n is not prime then repeat this step incrementing n until we find a prime.
3. If n is prime we iterate through the product of all combination of prime n with all previously discovered primes (always with at least one or more powers of the new prime). We calculate least-m for each of these products and store in least-m-series. For products up to a maximum of max-m
4. If have not iterated n up to max-n then go back to step 2.

An advantage of doing the sieve this way is that we can easily calculate the smallest-m as we go along. Since for each step of the iteration we know the smallest m for the product of a previous combination of primes and are multiplying that by another prime, smallest m for this new product is easy to calculate as I have done in version 10 of my code.

The problem is that for a very long series of primes already discovered we may need to keep track of how many powers of each prime we have iterated through. This might be solved by just keeping track of the product and what prime to multiply by next or something like that.