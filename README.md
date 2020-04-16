# Ergo Mixer, Web Version 

This is web application doing non-interactive mixing following ErgoMix (aka ZeroJoin) protocol. Details can be found in a paper (by koshti and scalahub) coming.

PLEASE NOTE THAT THE QUALITY OF THIS APPLICATION IS EXPERIMENTAL AT THE MOMENT! USE IT FOR EXPERIMENTS ONLY!

Advantages of ErgoMix:
 * it is non-interactive: this application works only with the Ergo blockchain (concretely, an external node and an explorer, so use Tor!). No interaction with other mix participants in needed.
 * no trusted setup or complex cryptography, only DDH assumption and a simple protocol 
 * coins pool is not limited by a mixing transaction, thus the pool can be very big potentially
 * privacy leaks caused by mining fee mitigated by using special fee emission boxes 

Limitations:
 * many mixes needed for good privacy, mixing is not fast then
 * value is not hidden, a coin can be mixed only with a coin of same nominal then

## Installing and Running

Currently only sources available; there are no binaries. This is done intentionally, as quality of the mixer is experimental at the moment, so I am limiting number of users. 
Binaries will be available a bit later.

You need for Java (JDK, 8+) and SBT to be installed on your machine. The pull the codebase with `git clone ` and run the application with `sbt clean stage` . After compilation (which takes a lot of time) 
go to *localhost:9000* in your browser.

## Contributions

Feel free to improve it. No rights reserved. Privacy is a basic human right!

What is needed:

* better UI, current is truly terrible kiss, I know
* current interactions with the node and the explorer not scalable probably
* improve code quality
* documentation: describe how this tool is working, to make it reproducible in wallets
* support for multiple pools (with different nominals)
* auto-splitting of arbitrary deposit into coins of mixing nominals

What I will work on:
* token mixing (as Ergo supports for custom tokens)



