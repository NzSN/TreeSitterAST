# Template Harness Code Generation System (THCGS)
The function ProgBuilderForECMA.descript spawn a 'Template System' 
in language the specified by ECMAScript. In short, the 
'Template System' is called as T. 

The puroses of THCGS is to verify that all 
states of T will result in sentences which is valid for grammar that  used to spawn T. 

# Prerequisitions
1. [Path Coverage Structure]( ../path-coverage-structure.md)
2. Template Spawning System, which is defined as 
   ProgBuilderForECMA.descript

# Architecture of THCGS
REMARK: Architecture is explain in math formula in text.

```tla
(*--algorithm THCGS
variables 
  Cover = [x \in Gr |-> PCS]
  T     = [x \in Gr |-> Ts]
begin

end algorithm; *)
```
