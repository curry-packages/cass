-- Tests for the RootReplaced analysis
--
-- Runt test with:
-- > cass RootReplaced RootReplacedTest.curry

loop = loop
--> root replacements: [loop] --> indicates infinite loop

f x = g x
--> root replacements: [g,h]

g x = h x
--> root replacements: [h]

h x = k x : []
--> root replacements: []

k x = x
--> root replacements: []
