/*

12.1: Propagate and search

This section is an introduction to search problems with a discussion of 
computation spaces, in the form of a simple worked example.

The first time I read this section, I failed to comprehend any of it.

Returning a second time, I decided the core problem was that I had not learned
of any intuitive way to think about a "computation space" so I focused on that.
Working through the example problem, I drew the space as a normal graph of X
and Y, with axis values between 0 and 9. The constraint X<=Y meant drawing the
line Y=X and ruling out everything below it. Constraining values for X and Y
similarly meant drawing horizontal and vertical lines and marking one side as
ruled out. Similarly, search amounts to drawing an arbitrary line in the not-
yet-ruled-out space and then considering the two sides of that line separately.

This visual intuition, though it obviously doesn't extend beyond very simple
problems, helped me to understand what is meant by "computation space" and
follow the example.

So after reading this section a second time, I think I understand the general
idea, but I am interested to learn more about:
	- how can propagators be written?
	- how are spaces implemented?
	- how does the search strategy actually make choices?
	- what requirements are put on the searched space? can arbitrary, symbolic
	  spaces be searched through? Or are care numeric assumptions in use?

*/