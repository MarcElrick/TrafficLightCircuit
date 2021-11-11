module TrafficLight where
import HDL.Hydra.Core.Lib (halfAdd, mux1, CBit(..), Logic(inv, or4, and2, and3, nand4, or2, or3, one, zero, and4), andw)
import HDL.Hydra.Circuits.Combinational ()

------------------------------------------------------------------------
-- Simulate controller1 circuit
------------------------------------------------------------------------

 -- Traffic light circuit v1
 -- Input: 1 bit word representing reset
 -- Output: 3 bit tuple - whichever bit is "lit up" indicates which light is on


controller1 :: CBit a => a -> (a, a, a)
controller1 reset = (red, amber, green)
    where green = or3 x0 x1 x2
          amber = or2 x3 x8
          red = or4 x4 x5 x6 x7
          reset' = inv reset

          -- dffs numbered in order of light transition on clock tick
          x0 = dff (or2 reset x8)
          x1 = dff (and2 reset' x0)
          x2 = dff (and2 reset' x1)
          x3 = dff (and2 reset' x2)
          x4 = dff (and2 reset' x3)
          x5 = dff (and2 reset' x4)
          x6 = dff (and2 reset' x5)
          x7 = dff (and2 reset' x6)
          x8 = dff (and2 reset' x7)



------------------------------------------------------------------------
-- Simulate controller2 circuit
------------------------------------------------------------------------

controller2 :: CBit a => a -> a -> (a, a, a, a, a, [[a]])
controller2 reset walkRequest = (red, amber, green, wait, walk, requestCount)
    where green = and3 idleState (inv red) (inv amber)
          amber = or2 x0 x4
          red = or3 x1 x2 x3
          wait = or2 green amber
          walk = red

          -- walkPressed signal wired into 16 bit counter
          walkPressed = and3 reset' walkRequest (inv (and2 countBuffer idleState))
          countBuffer = dff (and2 reset' walkPressed)
          requestCount = count16 (and2 reset' walkPressed) reset
          
          reset' = inv reset

          -- determines if walk button is pressed and to ignore multiple presses in a cycle
          idleState = dff (inv cycleOccuring) 
          cycleOccuring = and3 reset' buttonPressBuffer (inv x4) 
          buttonPressBuffer = and3 reset' walkRequest (inv x0)

          -- dffs numbered in order of light transition on clock tick
          x0 = dff buttonPressBuffer
          x1 = dff (and2 reset' x0)
          x2 = dff (and2 reset' x1)
          x3 = dff (and2 reset' x2)
          x4 = dff (and2 reset' x3)



-- 16 bit counter made by connecting 4 4-bit counters.
-- Each counter is given logical and of the output signals of all previous counters.
count16 :: CBit a => a -> a -> [[a]]
count16 active reset = [w0,w1,w2,w3]
  where  w0 = count4ripple reset (head w1) -- First bit of each word is carry bit
         w1 = count4ripple reset (head w2) -- Each counter given c_out from previous counter
         w2 = count4ripple reset (head w3)
         w3 = count4ripple reset active -- First 4 bit counter is given 1 as c_in to start the chain of counters


-- Modified version of count4b taken from /examples/counter
-- Modified to accept a carry input bit and output a carry output bit.
count4ripple :: CBit a => a -> a -> [a]
count4ripple reset c_in = [c_out,x0,x1,x2,x3]
  where 
    c_out = and2 c_in (and4 x0 x1 x2 x3) -- c_out is one when counter is full and all previous counters are full
    (c0,x0) = cbit reset c1
    (c1,x1) = cbit reset c2
    (c2,x2) = cbit reset c3
    (c3,x3) = cbit reset c_in -- counter only starts counting when c_in is true


-- cbit also taken from /examples/counter
-- This is completely unchanged
cbit :: CBit a => a -> a -> (a,a)
cbit reset cin = (cout,s)
  where
    s = dff (mux1 reset s' zero)
    (cout,s') = halfAdd cin s
