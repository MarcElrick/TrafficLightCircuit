module TrafficLight where
import HDL.Hydra.Core.Lib (CBit(..), Logic(inv, or4, and2, or2, or3))
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
