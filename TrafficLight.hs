module TrafficLight where
import HDL.Hydra.Core.Lib (CBit(..), Logic(inv, or4, and2, or2, or3, and3))
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

controller2 :: CBit a => a -> a -> (a, a, a, a, a)
controller2 reset walkRequest = (red, amber, green, wait, walk)
    where green = and3 idleState (inv red) (inv amber)
          amber = or2 x0 x4
          red = or3 x1 x2 x3
          wait = or2 green amber
          walk = red

          --walkPressed = and3 reset' walkRequest (inv (and2 countBuffer idleState))
          --countBuffer = dff (and2 reset' walkPressed)

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