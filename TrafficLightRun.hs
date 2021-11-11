-- Mux1Run: simulation driver for mux1 circuit
-- This file is part of Hydra.  John O'Donnell, 2021

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
--   reset  --
--------------
  [ "0"  --  
  , "0"  --  
  , "1"  --  
  , "0"  --  
  , "0"  --  
  , "0"  --  
  , "0"  --  
  , "0"  --  
  , "0"  --  
  , "0"  --  
  , "0"  --  reset, cycle 11
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

-- Input ports  
  in_reset <- inPortBit "reset"


-- Input signals
  let reset = inbsig in_reset

-- Circuit
  let (red, amber, green) = controller1 reset

-- Format the results  
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
  , "0  1"  --  
  , "0  0"  --  
  , "0  0"  --  
  , "0  0"  --  
  , "0  1"  --  
  , "0  0"  --  
  , "0  0"  --  
  , "1  0"  --  reset, cycle 11
  , "0  0"  --  
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
  let (red, amber, green, wait, walk) = controller2 reset walkRequest

-- Format the results  
  format
    [string "  reset=", bit reset,
     string "  walkRequest=", bit walkRequest,
     string "  output red=", bit red,
     string "  output amber=", bit amber,
     string "  output green=", bit green,
     string "  output wait=", bit wait,
     string "  output walk=", bit walk

    ]

-- Run the circuit on the test data
  runSimulation