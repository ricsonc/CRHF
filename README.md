Collision Resistant Hash Function
====

This is a collision resistant hash function written in haskell. It was written fun while I was learning haskell. As a result, it should not be used for any serious applications.

This hash function uses the standard merkel damgard construction, and is also inspired from the Keccak sponge function family. 

Preliminary tests seem to show that this function exhibits the desirable avalanche effect, and it should be non-trivial to force a hash collision.
