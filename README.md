# copland-parser
General purpose Copland parser

A good place to start is [Interface.hs], it has the most useful functions

# Notes
- This parser uses Right Associativity for the Branch Operators. This is not currently in-line with the Copland Tutorial, but without Right Associativity, we must enforce explicit parenthesization around all branch phrases to ensure that we do not create ambiguity.
- This parser also leaves out NULL / "{}", from parsing. The Coq equivalent does not exist
