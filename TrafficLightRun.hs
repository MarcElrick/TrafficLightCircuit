-- Driver Circuit

module Main where
import HDL.Hydra.Core.Lib
import TrafficLight

--Declaration of all test data sets
test_data_v1_1, test_data_v1_2 :: [String]

test_data_v1_1 =
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
  , "0"  --
  , "0"  --
  , "0"  --
  , "0"  --
  , "0"  --
  , "0"  --
  ]

test_data_v1_2 =
  --------------
  --   Reset pressed during sequence  --
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
    , "1"  --
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

main :: IO ()
main = driver $ do

-- Input data. Specifies what test data set we use
  useData test_data_v1_1

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
