module TrafficLight where
import HDL.Hydra.Core.Lib (halfAdd, mux1, CBit(..), Logic(inv, or4, and2, and3, nand4, or2, or3, one, zero, and4), andw)
import HDL.Hydra.Circuits.Combinational ()


 -- Traffic light circuit v1
 -- Input: 1 bit word representing reset
 -- Output: 3 bit tuple - whichever bit is "lit up" indicates which light is on


controller1 :: CBit a => a -> (a, a, a)
controller1 reset = (red, amber, green)
    where green = or3 x0 x1 x2
          amber = or2 x3 x8
          red = or4 x4 x5 x6 x7
          reset' = inv reset

          x0 = dff (or2 reset x8)
          x1 = dff (and2 reset' x0)
          x2 = dff (and2 reset' x1)
          x3 = dff (and2 reset' x2)
          x4 = dff (and2 reset' x3)
          x5 = dff (and2 reset' x4)
          x6 = dff (and2 reset' x5)
          x7 = dff (and2 reset' x6)
          x8 = dff (and2 reset' x7)



-- 16 bit counter made by connecting 4 4-bit counters.
-- Each counter is given logical and of the output signals of all previous counters.

count16 :: CBit a => a -> [[a]]
count16 reset = [w0,w1,w2,w3]
  where  w0 = count4ripple reset (head w1)
         w1 = count4ripple reset (head w2)
         w2 = count4ripple reset (head w3)
         w3 = count4ripple reset one


-- Modified version of count4b taken from /examples/counter
-- Modified to accept a carry input bit and output a carry output bit.
count4ripple :: CBit a => a -> a -> [a]
count4ripple reset c_in = [c_out,x0,x1,x2,x3]
  where c_out = and2 c_in (and2 (and2 x0 x1) (and2 x2 x3))
        (c0,x0) = cbit reset c1
        (c1,x1) = cbit reset c2
        (c2,x2) = cbit reset c3
        (c3,x3) = cbit reset c_in



cbit :: CBit a => a -> a -> (a,a)
cbit reset cin = (cout,s)
  where
    s = dff (mux1 reset s' zero)
    (cout,s') = halfAdd cin s




