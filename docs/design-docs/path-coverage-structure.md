# Path Coverage Structure

By [Definition of Grammar](./fundamental-definitions.md#Grammar), suppose that there 
is a grammar 'g : Gr' such that 

$g := {Name: "g", Nodes: [N1,N2,N3,N4,...]}$.

Let's define Path Coverage Structure.
 
$PCS := [[N]]$
: [[N1_s1, N2_s1, N3_s1, ...], [N1_s2, N2_s2, N3_s2, ...], ... ]

## Operations on PCS
### Cover : Gr -> PCS
The Cover operation is defined as 'Cover : Gr -> PCS', that transform to PCS from a given Gr, 
the PCS contain all states of GrammarNodes of Gr.

Invariants of Cover
: 
