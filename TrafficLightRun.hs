-- Driver Circuit

module Main where
import HDL.Hydra.Core.Lib
import TrafficLight ( controller1, controller2 )


------------------------------------------------------------------------
-- Running executable functions of type IO ()
------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "*** controller1 simulation ***"
  controller1Driver
  putStrLn "*** controller2 simulation ***"
  controller2Driver

------------------------------------------------------------------------
-- Simulate controller1 circuit
------------------------------------------------------------------------

controller1TestData :: [String]
controller1TestData =
--------------
--   Normal use (reset to begin sequence only)  --
--------------
  [ "1"  --  
  , "0"  --  
  , "0"  --  
  , "0"  --  
  , "0"  --  
  , "0"  --  
  , "0"  --  
  , "0"  --  
  , "0"  --  
  , "0"  --  
  , "0"  -- 
  , "0"  --  
  , "0"  --  
  , "0"  -- 
  , "0"  --  
  , "0"  -- 
  ]

controller1Driver :: IO ()
controller1Driver = driver $ do

-- Input data
  useData controller1TestData

-- Input ports (we only have a reset input)
  in_reset <- inPortBit "reset"

-- Input signals
  let reset = inbsig in_reset

-- Circuit, with outputs red, amber and green. As it is a state machine only one
-- of these will be '1' at one time.
  let (red, amber, green) = controller1 reset

-- Format the outputs for each cycle
  format
    [string "  reset=", bit reset,
     string "  output red=", bit red,
     string "  output amber=", bit amber,
     string "  output green=", bit green
    ]

-- Run the circuit on the test data
  runSimulation


------------------------------------------------------------------------
-- Simulate controller2 circuit
------------------------------------------------------------------------

controller2TestData :: [String]
controller2TestData =
------------------------
--  reset  walkRequest 
------------------------
  [ "0  0"  --  
  , "0  0"  --  
  , "0  0"  --  
  , "0  1"  --  counted walkRequest, cycle 3
  , "0  0"  --  
  , "0  0"  --  
  , "0  0"  --  
  , "0  1"  --  uncounted walkRequest, cycle 8
  , "0  0"  --  
  , "0  1"  --  counted walkRequest, cycle 10
  , "0  0"  --  
  , "1  0"  --  reset, cycle 12
  , "0  0"  --  
  , "0  0"  -- 
  , "0  0"  --  
  , "0  0"  -- 
  ]

controller2Driver :: IO ()
controller2Driver = driver $ do

-- Input data
  useData controller2TestData

-- Input ports  
  in_reset <- inPortBit "reset"
  in_walkRequest <- inPortBit "walkRequest"


-- Input signals
  let reset = inbsig in_reset
  let walkRequest = inbsig in_walkRequest

-- Circuit
  let (red, amber, green, wait, walk, requestCount) = controller2 reset walkRequest

-- Format the results  
  format
    [string "  reset=", bit reset,
     string "  walkRequest=", bit walkRequest,
     string "  output red=", bit red,
     string "  output amber=", bit amber,
     string "  output green=", bit green,
     string "  output wait=", bit wait,
     string "  output walk=", bit walk,
     

     -- Four 4-bit words. When combined, this is the output of the 16 bit counter. 
     string "  output countWord0 =", bindec 4 (requestCount !! 0), -- Most significant 4 bits 
     string "  output countWord1 =", bindec 4 (requestCount !! 1), 
     string "  output countWord2 =", bindec 4 (requestCount !! 2),
     string "  output countWord3 =", bindec 4 (requestCount !! 3) -- Least significant 4 bits 
    ]

-- Run the circuit on the test data
  runSimulation