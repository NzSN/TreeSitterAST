# Fundamental Defitions

## Grammar

### GrammarNode
Before to define Grammar have to define GrammarNode.
$N := GrammarNode of TreeSitter Grammar$

For convenience define GrammarNode in this repository as GrammarNode define 
in TreeSitter [The Grammar DSL](https://tree-sitter.github.io/tree-sitter/creating-parsers/2-the-grammar-dsl.html).

For the purposes of to improve expresness via GrammarNode, extra external operation 
is defined as below.

Suppose N1 is an instance of N, written as N1 : N. Then

- N1_n indicate the nth child of N1.
  This operation can be nested, for example, N1_n_m indicate the mth child of nth child
  of N1.
- N1_sn indicate the nth state of N1, if N1 has only m state and n in N1_sn is greather
  thant m then N1_sm indicate the last state of N1.
  
  The meaning of state of GrammarNode here is combinations of child nodes for interior node, for simplicity assume that leaf node has only one states. There is a concrete example following, a json to express a GrammarNode.
  
```json
{
  'A': {
    "type": "CHOICE",
    "members": [
      {
        "type": "STRING",
        "value": "S1"
      },
      {
        "type": "STRING",
        "value": "S2"
      }
    ]
  },
  'B': {
    "type": "SEQ",
    "members": [
      {
        "type": "STRING",
        "value": "S1"
      },
      {
        "type": "STRING",
        "value": "S2"
      }
    ]
  }

}
```
Suppose there is a grammar node, n : N, correspond to A then state of n is two, S1 or S2. In such case can say that $n_s1 := S1$ and $n_s2 := S2$
Suppose there is a grammar node, m : N, correspond to B then state of m is only one, 
cause possible combinations of child is S1 and S2. In such case can say that 
$m_s1 := [S1,S2]$ and $m_s2 := [S1,S2]$.

### Definition of Grammar
Grammar in this repos is defined as following which is written as Gr

$Gr := {Name: string, Nodes: [N]}$

Gr can be used as type, for example, g1 : Gr indicate that g1 is an instance of Grammar.

### Operations on Grammar
- Select the n node, where n is in Natural numbers.
$Gr_n := the nth member of Nodes of a grammar$
$Gr_Name indicate the Name of Gr$
