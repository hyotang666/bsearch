# bsearch 0.0.0
## What is this?
Trivial binary searching.
### Current lisp world
Common Lisp support vector.
### Issues
There is no api to binary search.
(And I could not find any library to do this.)
### Proposal
Bsearch provides it.
## Usage
Return 2 values.

1. Found item.
2. Index of vector.

```lisp
(bsearch 1 #(1 2 3 4 5)) => 1 ; 0
```
## From developer
### Product's goal
Integrated by apropreate utility libraries. (e.g. alexandria)
### License
Public domain.
### Developed with
CLISP/2.49
### Tested with
CCL/1.11 ECL/16.1.3 SBCL/1.3.2

## Installation

