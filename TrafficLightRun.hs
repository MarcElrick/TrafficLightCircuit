-- Mux1Run: simulation driver for mux1 circuit
-- This file is part of Hydra.  John O'Donnell, 2021

module Main where
import HDL.Hydra.Core.Lib
import TrafficLight

test_data_1 :: [String]
test_data_1 =
--------------
--   reset  --
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
  , "0"  --  reset, cycle 11
  , "0"  --  
  , "0"  --  
  , "0"  -- 
  , "0"  --  
  , "0"  -- 
  ]

main :: IO ()
main = driver $ do

-- Input data
  useData test_data_1

-- Input ports  
  in_reset <- inPortBit "reset"


-- Input signals
  let reset = inbsig in_reset

-- Circuit
  let [red, amber, green] = controller1 reset

-- Format the results  
  format
    [string "  reset=", bit reset,
     string "  output red=", bit red,
     string "  output amber=", bit amber,
     string "  output green=", bit green
    ]

-- Run the circuit on the test data
  runSimulation
