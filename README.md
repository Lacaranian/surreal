surreal
=======

A Haskell library representing the class of surreal numbers

### Theory

Surreals are constructed recursively by composing two sets of surreals, a left and a right set, denoted as {L|R} where L represents the left set and R represents the right set. For numerical surreal numbers, the number defined by two sets of surreal numbers lies between the largest element of the left set and the smallest element of the right set. Surreals where the largest element of the left set is not strictly less than the smallest element are the right set are not considered numerical.

The simplest surreal number is {|}, which contains only empty sets. For convenience, this can be set as zero. Thus, {{|}|} = {0|}, which we can then denote as one. Fractional numbers can be obtained by placing integer values in both the left and right sets. For example, {0|1} represents 1/2 (or 0.5), and (1/2|1) represents 3/4. These processes may repeat forever; this allows a surreal number like {1,2,3...|} to represent infinite ordinals like ω, infinitesimals like 1/ω, repeating decimals like 1/3, and irrationals like π.

The surreal numbers are an ordered field containing all other ordered fields. Surreals, whether numeric or non-numeric, may be used as a representation of the possible branching choices in a formal mathematical definition of games.

### Capabilities

This library aims to expose as many of the capabilities of surreals as possible using Haskell. Currently, it can:
- Construct a surrreal number from a Haskell integer
- Perform basic arithmetic on surreal numbers (addition, multiplication, and subtraction)
- Display the raw representation of a surreal (currently messy, plan on cleaning this up)
