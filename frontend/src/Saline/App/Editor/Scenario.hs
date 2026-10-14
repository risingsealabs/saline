{-# LANGUAGE OverloadedStrings #-}
module Saline.App.Editor.Scenario where

import Control.Applicative
import Control.Monad.State (evalState, runState)
import Data.Dependent.Map qualified as DMap
import Data.Functor (void)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Reflex.Dom.Core hiding (Alt, tag)
import Test.QuickCheck.Random (mkQCGen)

import Saline.App.Editor.Diagrams
import Saline.App.Layout
import Saline.Crypto
import Saline.Intents
import Saline.Intents.Condition
import Saline.Intents.Satisfying qualified as Sat
import Saline.Utils
import Saline.Utils.Satisfying qualified as Sat

scenario :: (DomBuilder t m, Prerender t m) => (VariableNames, Intent) -> m ()
scenario (_vns, int) = do
  let
    center = divClass "flex flex-row justify-center w-full"
  center $ diagramSymbolic int
  {-
  sequence_ $ ffor [0..0] $ \i -> divClass "flex flex-col" $ do
    seed <- divClass "flex flex-row justify-center items-center gap-4 mt-4" $ do
      el "label" $ text "RNG seed"
      Editor.word $ Just $ 1e3 + toEnum i

    runHold_ $ ffor seed $ maybe blank (example (vns, int) . fromEnum) . justFieldValue
  -}
example :: forall t m. (DomBuilder t m, Prerender t m) => (VariableNames, Intent) -> Int -> m ()
example (params, int) seed = do
  let
    --moated = elAttr "div" $ style $ moatCss Nothing
    center = divClass "flex flex-row justify-center w-full"
    --evenly = divClass "flex flex-row justify-evenly w-full"

    pp :: VariableTag a -> a -> Text
    pp = \case
      VariableTag_Address -> addressEllipsis
      VariableTag_Amount  -> tshow
      VariableTag_Count   -> tshow
      VariableTag_List    -> Text.unwords . fmap addressEllipsis

    (vals, seed') = flip runState (mkQCGen seed) $
      DMap.forWithKey params $ \tag (Const names) -> fmap Compose $ do
        sequence $ flip Map.fromSet names $ \_ -> do
          generate $ Sat._satisfying_gen $ Sat.identity $ Sat.variable tag

  center $ divClass "flex flex-col items-center" $
    DMap.forWithKey_ vals $ \tag (Compose vs) -> do
      void $ flip Map.traverseWithKey vs $ \name (Identity v) -> do
        el "div" $ text $ name <> " = " <> pp tag v

  case Sat.condition vals int of
    Left err -> center $ text $ tshow err
    Right c -> column blank $ do
      let
        c' = flip evalState seed' $ generate $ Sat._satisfying_gen c

      -- evenly $ moated . conditionConcrete $ c' --TODO: add back
      center $ diagramConcrete c'
