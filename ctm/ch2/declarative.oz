/*

2.1: Defining programming languages & the kernel language approach.

This section provides a justification of the authors' approach to
teaching programming through gradual extension of a kernel language,
paired with its operational semantics. Fortunately, the material also
provides a glimpse into other choices: other models for defining and
exploring languages.

While most of the authors' choices seem well justified, I am slightly
sad that the language extensions will be defined in abstract semantics
only, with their implementations in Oz hidden from view. In comparison
with SICP, where many concepts are directly implemented during the
course of the text, this seems like a loss. The authors protest that
building interpreters for these concepts changes their performance
characteristics. But we are already using a research language, so this
hardly seems like strong enough justification.

Perhaps this is similar to the justification for MIT no longer using SICP:
that programming has transitioned from a domain where a single person
could likely understand a full system to one where we must treat many
tools that we use as black boxes because we are too many layers above
the base reality of circuits to hope to understand all of them.

*/

/*

2.2: The single-assignment store

I was blown away by the amazing properties that follow just by using a
single assignment store, rather than a value or mutable store. It would
never have occured to me that dataflow variables were useful even in a
single-thread setting.
	- It allows procedures to be the basic abstraction unit and for
	  functions to easily be desugared into them.
	- It allows natural tail-recursive definitions of list functions
	  that normally need an accumulator and a final reverse step.
	- It makes logic programming compatible with the rest of the
	  language's semantics.

Very eye-opening!

The explanation of the difference between variables on the store and
the environment that binds names to them is very clear and I imagine
quite helpful to someone not previously familiar with this material.
The small examples here also help a student get used to the difference
between code, abstract syntax, and losose combinations of the two.

And again we have a case where, in reviewing the contents of this
section, the authors provide a little peek into the value of dataflow
in concurrent systems and logic languages, topics that won't be pursued
until later. I like the way these teases are thrown in.

*/

/*

2.3: Kernel language

This section has a quick walkthrough of the core data types and
operations for the basic kernel language. The meaty portion is a
defense of the selection of procedures, rather than functions or
objects, as the basic means of abstraction.

*/

/*

2.4: Kernel language semantics

Phew! There's a lot of material in this section, especially if you
stop to carefully work out the other sample programs, as the text
suggests. Each step of the semantics is pretty easy to carry out,
so the main task of this section is to show that such simple steps
can actually explain the behavior of a full program, and also to
point out several unexpected behaviors that emerge from them. Tail
calls and syntactic scope are pointed out as two complex and useful
behaviors which, on the semantic level, have quite simple explanations.

There is also a discussion of memory management: both automatic
garbage collection, and the programmer's responsibility to deal with
external resources. 

*/

/*

2.5: Syntactic extensions to the kernel language

This section lays out several desugaring steps and abstractions added
on to the kernel language to make it more palatable for real work.

I liked the way that record-destructuring assignment is described and
am interested in the use of the nesting marker `$` to turn parts of an
expression into its value.

The desugaring of functions to procedures is very neat and elegant,
but in practice I often use function calls in statement positions and
receive a not-very-helpful arity exception. It would be nice if the
desugaring left enough of a marker behind to allow the generation of
the nicer message: "You used a function in statement position."

One big surprise: dataflow variables allow the naive versions of many
functions to be tail-recursive.

For example:
*/

fun {Map Xs F}
   case Xs
      of nil then nil
      [] X|Xr then {F X}|{Map Xr F}
   end
end

/* 

desugars to code that includes the sequence:

local Y Yr in
   Ys=Y|Yr
   {F X Y}
   {Map Xr F Yr}
end

Now that's pretty damn cool. Because the variable can be assigned a
partial structure and the details filled in later, the argument to the
procedure that represents the function's return value can be assigned
to be a list, then the first item in the list can be assigned, and
finally the rest of the list can be passed on to a recursive call in
the last step of the function. 


The introduction to the interactive environment (`declare` and `Browse`)
is helpful to writing simple examples. But I can't help feeling a bit
sad that there isn't a real REPL environment. I'm sure there are some
technical limitations on this I don't understand, but it would be quite
convenient to have one.

*/

/*

2.6: exceptions

This is a nice short example of adding a small feature on to the existing
kernel language and explaining the new semantics. I like that it shows
semantic composition by desugaring catch pattern matching into simple catch
and then simple pattern matching. This helps to show that adding features
may add more than just a linear amount of power to the language.

*/

/*

2.7: Advanced topics

I appreciated the definitions provided here for the unification and entailment
procedures familiar from logic programming. That these patterns are useful
even without backtracing search is a bit of a surprise to me.

The discussion of dynamic and static typing seems to be at a lower level
than the surrounding material, as it avoids discussion of the many kinds of
static type checking that are available and the different benefits these
methods might provide. A first- or second-year student who only has
experience with Java might reasonably finish reading this discussion of
tradeoffs and think they know what there is to know about static typing.
This seems to be in contrast to my general experience of this book, that it
consistently exposes students to the wide array of choices that are ahead.
Given that general trend, and that the authors clearly know more than I do,
I wonder whether this seeming lack of discussion is due to a specific
pedagogical choice, an artifact of the book being written circa 2000, or
something the authors think that I don't yet understand.

*/