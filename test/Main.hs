module Main (main) where
import Hedgehog
import Test.StateMachine

main :: IO Bool
main = checkSequential $
  Group "API Tests"
  [ ("API State Machine", propApiTests env reset)
  ]
  where
    env = undefined
    reset = undefined
