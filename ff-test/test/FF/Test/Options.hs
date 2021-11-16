{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module FF.Test.Options (optionsTest) where

import Data.Set qualified as Set
import Hedgehog (evalIO, property, withTests, (===))
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

import FF.Options (ActionOptions (..), Agenda (..), Cmd (CmdAction),
                   CmdAction (CmdAgenda), Options (..), Tags (..), parseOptions)

optionsTest :: TestTree
optionsTest =
  testProperty "options smoke test" $
    withTests 1 $
    property do
      options <- evalIO $ parseOptions Nothing
      expectedOptions === options
  where
    expectedOptions =
      Options
        { customDir = Nothing
        , cmd =
            CmdAction $
            CmdAgenda
              Agenda
                { limit = Nothing
                , tags = Tags{require = Set.empty, exclude = Set.empty}
                }
        , actionOptions = ActionOptions{brief = False, json = False}
        , debug = False
        }
