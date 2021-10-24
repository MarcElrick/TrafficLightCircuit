module TrafficLight where
import HDL.Hydra.Core.Lib

-- Version (b) of the 4 bit counter taken from examples/counter/Count4.hs
-- This is used to keep count of which clock cycle we are on.
-- Modified to also reset once the value 1001 is reached.

count4 :: CBit a => a -> [a]
count4 reset = [x0,x1,x2,x3]
  where 
        restart = or2 reset x0
        (c0,x0) = cbit restart c1
        (c1,x1) = cbit restart c2
        (c2,x2) = cbit restart c3
        (c3,x3) = cbit restart one

cbit :: CBit a => a -> a -> (a,a)
cbit reset cin = (cout,s)
  where
    s = dff (mux1 reset s' zero)
    (cout,s') = halfAdd cin s

 -- Traffic light circuit
 -- Input: 4 bit word from counter keeping track of clock
 -- Output: 3 bit word - whichever bit is "lit up" indicates which light is on

version1 :: Bit a => a -> a -> a -> a -> [a]
version1 x0 x1 x2 x3 = [red, amber, green]
    where red = x1
          green = and2 (nand2 x2 x3) (nor2 x0 x1)
          amber = inv (or2 red green)


 -- Traffic light circuit wired up to count4
 -- Input: reset bit
 -- Ouput: 3 bit word - whichever bit is "lit up" indicates which light is on

combinedVersion1 :: CBit a => a ->[a]
combinedVersion1 reset = [red, amber, green]
    where [x0, x1, x2, x3] = count4 reset
          [red, amber, green] = version1 x0 x1 x2 x3