{-# LANGUAGE OverloadedStrings #-}
module Saline.Integrations.Python where

import Prelude hiding (lines, unlines, writeFile)

import Control.Lens (ifor_, strict, view)
import Control.Monad (replicateM)
import Control.Monad.State (evalState)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.Aeson (ToJSON)
import Data.Aeson.Text qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.Foldable
import Data.Function (fix)
import Data.Functor (void)
import Data.Functor.Const (Const (..))
import Data.Functor.Contravariant (Contravariant)
import Data.Functor.Invariant (Invariant)
import Data.List (sort)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Some
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Universe
import System.Process
import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Random (mkQCGen)

import Saline.Integrations.Bindings
import Saline.Intents.Arbitrary (rngSeed)
import Saline.Intents.Condition
import Saline.Intents.Witness
import Saline.Network.Node.Transaction
import Saline.Utils

type Python = ReaderT Word (Writer Source)

type Source = Text

newtype Reference = Reference Name
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (IsString)

dev :: FilePath -> IO ()
dev root = do
  let
    sdkDir = root <> "/saline-sdk"
    testsDir = sdkDir <> "/tests/unit/transaction"
    bindingsPath = sdkDir <> "/saline_sdk/transaction/bindings.py"
    roundtripsPath = testsDir <> "/test_roundtrips.py"

    trim = Text.unlines . fmap Text.stripEnd . Text.lines
    eval = execWriter . flip runReaderT 0
    write path = ByteString.writeFile path . Text.encodeUtf8 . trim . eval

    (source, tests) = sdk

  write bindingsPath source
  write roundtripsPath tests

  (_,_,_,ph) <- createProcess (proc "poetry" ["run", "pytest", roundtripsPath]) { cwd = Just sdkDir }
  void $ waitForProcess ph

sdk :: (Python (), Python ())
sdk = (source, tests)
  where
    source = spaced
      [ header
      , sourceImports

      , nonEmpty
      , utils

      , enum relation_
      , enum token_
      , enum arithmetic_
      , enum variableTag_

      , struct Nothing $ variable_ @Double --TODO: keep abstract
      , subclasses witness_ $ pure ()
      , expr
      , intent
      , subclasses bridgeInstruction_ $ pure ()
      , subclasses instruction_ $ pure ()
      , struct Nothing tx_
      , struct Nothing signedTx_
      ]

    tests :: Python ()
    tests = spaced
      [ header
      , testImports
      , roundtrip
      , heading "class TestRoundtrips(unittest.TestCase)" $ do
          function "test_roundtrips" [("self", Nothing)] $ spaced roundtripsList
      ]

    roundtripsList :: [Python ()]
    roundtripsList =
      [ roundtripsU boolType

      , roundtripsG word64Type
      , roundtripsG timeType
      , roundtripsG publicKeyType --TODO: generate signature?

      , roundtripsU relationType
      , roundtripsU tokenType
      , roundtripsU arithmeticType

      , roundtripsSome variableTagType universeF
      , roundtripsG $ variableType @Address
      , roundtripsG $ exprType @Double
      , roundtripsG $ exprType @Address
      , roundtripsG intentType
      , roundtripsG witnessType
      , roundtripsG bridgeInstructionType
      , roundtripsG instructionType
      , roundtripsG txType
      , roundtripsG signedTxType
      ]

    roundtripsU :: (Finite a, ToJSON a) => Binding a -> Python ()
    roundtripsU t = roundtrips t universeF

    roundtripsG :: (Arbitrary a, ToJSON a) => Binding a -> Python ()
    roundtripsG t = roundtrips t samples
      where
      seed = mkQCGen rngSeed
      samples = flip evalState seed $ generate $ replicateM 50 arbitrary

    roundtrips :: ToJSON a => Binding a -> [a] -> Python ()
    roundtrips = unsafeRoundtrips

    roundtripsSome :: ToJSON (Some f) => Binding (f a) -> [Some f] -> Python ()
    roundtripsSome = unsafeRoundtrips

    -- Don't use directly
    unsafeRoundtrips :: ToJSON b => Binding a -> [b] -> Python ()
    unsafeRoundtrips t xs = line $ call "roundtrip" [wrapping wrapFromJSON, wrapping wrapToJSON, jsons xs]
      where
        wrapping w = parens $ "lambda x: " <> w (Just $ Some t) "x"

    utils :: Python ()
    utils = do
      function "dumps" [("x", Nothing)] $
        line "return json.dumps(x, separators=(',', ':'))"

      function "loads" [("x", Nothing)] $
        line "return json.loads(x)"

    roundtrip :: Python ()
    roundtrip = do
      function "roundtrip" [("from_json", Nothing), ("to_json", Nothing), ("values", Nothing)] $ do
        heading "for serialized in values" $ do
          line "parsed = from_json(loads(serialized))"
          line "reserialized = dumps(to_json(parsed))"
          heading ("if serialized != reserialized") $ do
            line "print('Failed to roundtrip')"
            line "print('Expected: ' + serialized)"
            line "print('Got: ' + reserialized)"
            line "exit(1)"

    jsons objs = list $ flip fmap objs $
      singleQuotes . view strict . Aeson.encodeToLazyText

    list = brackets . Text.intercalate ", "

    header :: Python ()
    header = line "# Auto-generated - do not edit manually"

    sourceImports, testImports :: Python ()
    sourceImports = lines
      [ "import json"
      , "import string"
      , "import typing"
      , "from blspy import G1Element, G2Element"
      , "from enum import Enum"
      , "from numpy import uint64, float64"
      , "from typing import Optional"
      , "from uuid import UUID"
      ]

    testImports = lines
      [ "import json"
      , "import unittest"
      , "from saline_sdk.transaction.bindings import *"
      ]

    intent :: Python ()
    intent = subclasses intent_ $ do
      operator2 "and" $ line "return All([self, other])"
      operator2 "or"  $ line "return Any(1, [self, other])"

    expr :: Python ()
    expr = subclasses0 (root, [], operators) $ flip fmap universeF $ \(Some t) -> fmap Some $ fields t
      where
        Named root (Union fields) = expr_ @Double genericArg --TODO: keep abstract
        operators = do
          let
            method name wrapper lhs op rhs = operator2 name $ do
              heading "if (isinstance(other, int) | isinstance(other, float))" $
                line "other = Lit(other)"
              line $ "return " <> wrapper <> parens (Text.intercalate ", " [lhs, op, rhs])

            op2Scalar name wrapper op = do
              method name wrapper "self" op "other"
              method ("r" <> name) wrapper "other" op "self"

          traverse_
            (\(name, op) -> op2Scalar name "Arithmetic2" op)
            [ ("add", "Arithmetic.Add")
            , ("mul", "Arithmetic.Mul")
            , ("sub", "Arithmetic.Sub")
            , ("div", "Arithmetic.Div")
            ]

          traverse_
            (\(name, op) -> method name "Restriction" "self" op "other")
            [ ("gt", "Relation.GT")
            , ("lt", "Relation.LT")
            , ("ge", "Relation.GE")
            , ("le", "Relation.LE")
            ] -- ("eq", "Relation.EQ") -- TODO: TROUBLE

    subclasses
      :: Finite (Some tag)
      => Named (Union tag)
      -> Python ()
      -> Python ()
    subclasses (Named root (Union fields)) extra = subclasses0 (root, [], extra) $ flip fmap universeF $ \(Some t) -> fmap Some $ fields t

    subclasses0
      :: (Name, [Name], Python ())
      -> [Named (Some Struct)] --TODO: unsafe
      -> Python ()
    subclasses0 (root, tvs, extra) fields = spaced $ comment : super : fmap sub fields
      where
        tags = fmap _named_name fields
        comment = line ("# " <> root <> " types")
        sub (Named t p) = withSome p $ struct (Just root) . Named t
        super = cls root Nothing tvs [] $ do
          let ix = index "d" . doubleQuotes
          fromJSON "d" $ do
            match (ix "tag") $ flip fmap tags $ \t ->
              (doubleQuotes t,) $ line $ "return " <> t <> ".from_json(d)"
          toJSON "x" root $ do
            match "x" $ flip fmap tags $ \t ->
              (t <> "()",) $ do
                line $ Text.unwords
                  [ "d ="
                  , curly (doubleQuotes "tag" <> " : " <> doubleQuotes t)
                  , "|"
                  , t <> ".to_json(x)"
                  ]
                line "return dict(sorted(d.items()))"
          extra

    --TODO: enforce type standalone
    struct :: Maybe Name -> Named (Struct a) -> Python ()
    struct super (Named self fields) = cls self super [] attrs $ do
      let
        ix = index "d" . doubleQuotes
        args = Text.intercalate ", " $ flip fmap attrs $ \(name, t) -> wrapFromJSON t (ix name)
      fromJSON "d" $ line $ "return " <> self <> parens args
      toJSON "x" self $ do
        line "d = dict()"
        for_ attrs $ \(name, t) ->
            line $ ix name <> " = " <> wrapToJSON t ("x." <> name)
        line $ "return d"
      where
        PyType (Const attrs) = evalStruct evalPyType fields

    wrapToJSON, wrapFromJSON :: Maybe (Some Binding) -> Text -> Text
    wrapToJSON = maybe id $ fix $ \go -> foldSome $ flip $ \x -> \case
      Binding_Defined t -> call (t <> ".to_json") [x]
      Binding_Dict {} -> call "list" [x <> ".items()"]
      Binding_Dict' {} -> call "list" [x <> ".items()"]
      Binding_NEList t -> go (Some $ Binding_List t) (x <> ".list")
      Binding_List (Binding_Defined t) -> call "list" [call "map" [t <> ".to_json", x]]
      Binding_Optional t -> "None if " <> x <> " == None else " <> go (Some t) x
      _ -> x
    wrapFromJSON = maybe id $ fix $ \go -> foldSome $ flip $ \x -> \case
      Binding_Defined t -> call (t <> ".from_json") [x]
      Binding_Dict {} -> "dict" <> parens x
      Binding_Dict' {} -> "dict" <> parens x
      Binding_NEList t -> call "NonEmpty.from_list" [go (Some $ Binding_List t) (parens x)]
      Binding_List (Binding_Defined t) -> call "list" [call "map" [t <> ".from_json", x]]
      Binding_Optional t -> "None if " <> x <> " == None else " <> go (Some t) x
      _ -> x

    nonEmpty :: Python ()
    nonEmpty = do
      lines
        [ "class NonEmpty[T]():"
        , "  def __init__(self, head: T, tail:list[T]):"
        , "    self.list = [head] + tail"
        , ""
        , "  @staticmethod"
        , "  def from_list(elements: list[T]):"
        , "    match elements:"
        , "      case []: raise ValueError"
        , "      case _: return NonEmpty(elements[0], elements[1:])"
        , ""
        ]
      nest $ do
        toJSON "x" "NonEmpty" $ line "return x.list"
        fromJSON "x" $ line "return NonEmpty.from_list(x)"

    cls :: Name -> Maybe Name -> [Name] -> Sig -> Python () -> Python ()
    cls name super typevars attributes extra = heading h $ do
      function "__init__" signature ini
      extra

      where
        h = "class " <> name <> tvs <> parens (fromMaybe "" super)
        tvs = case typevars of
          [] -> ""
          _ -> brackets $ Text.intercalate "," typevars
        signature = (("self", Nothing) : attributes)
        ini = case attributes of
          [] -> line "pass"
          _ -> do
            line "super().__init__()"
            for_ attributes $ \(a, _) ->
              line $ fold ["self.", a, " = ", a]

    enum :: (Finite a, Ord a) => Named (Enumeration a) -> Python ()
    enum (Named n (Enumeration memberName)) = heading h body
      where
        h = "class " <> n <> parens "Enum"
        body = do
          ifor_ (sort universeF) $ \i m -> do
            line $ memberName m <> " = " <> tshow i
          line ""
          fromJSON "s" $ line $ "return " <> n <> brackets "s"
          toJSON "x" n $ line "return x.name"

    --TODO: receive name as binding?
    toJSON :: Name -> Name -> Python () -> Python ()
    toJSON arg t body = static "to_json" [(arg, Just (Some $ Binding_Defined $ singleQuotes t))] $ do
      heading ("if (not isinstance(" <> arg <> "," <> t <> "))") $ do
        let
          err = Text.intercalate "+ '\\n' + '  ' + "
            [ "''"
            , singleQuotes "Expected: " <> " + str(" <> t <> ")"
            , singleQuotes "Got: " <> " + str(type" <> parens arg <> ")"
            ]
        line $ "raise TypeError " <> parens err
      body

    fromJSON :: Name -> Python () -> Python ()
    fromJSON arg = static "from_json" [(arg, Nothing)]

    static :: Text -> Sig -> Python () -> Python ()
    static name signature body = do
      line "@staticmethod"
      function name signature body

    operator2 :: Name -> Python () -> Python ()
    operator2 name = function (wrappedSym "__" name) [("self", Nothing), ("other", Nothing)]

    function :: Text -> Sig -> Python () -> Python ()
    function name signature body = do
      heading ("def " <> name <> parens (sig signature)) body
      line ""

    sig :: Sig -> Source
    sig = Text.intercalate ", " . fmap argument

    argument :: Argument -> Source
    argument (n, h) = n <> maybe "" hint h

    hint :: Hint -> Source
    hint h = ": " <> foldSome typeName h

    call :: Source -> [Source] -> Source
    call name args = name <> parens (Text.intercalate ", " args)

    index :: Source -> Source -> Source
    index target key = target <> brackets key

    match :: Source -> [(Source, Python ())] -> Python ()
    match target cases = heading ("match " <> target) $ do
      for_ cases $ \(pat, body) ->
        heading ("case " <> pat) body

    heading :: Source -> Python () -> Python ()
    heading h b = do
      line $ h <> ":"
      nest b

    nest :: Python () -> Python ()
    nest = local (+2)

    line :: Source -> Python ()
    line = lines . pure

    lines :: [Source] -> Python ()
    lines ss = do
      i <- ask
      lift $ tell $ Text.unlines $ flip fmap ss $ \s ->
        Text.replicate (fromEnum i) " " <> s

    spaced :: [Python ()] -> Python ()
    spaced = sequence_ . List.intersperse (line "")

    typeName :: Binding a -> Name
    typeName = \case
      Binding_Bool -> "bool"
      Binding_Str -> "str"
      Binding_Time -> "int" --TODO: safe?
      Binding_Optional t -> "Optional" <> brackets (typeName t)
      Binding_List a -> "list" <> brackets (typeName a)
      Binding_NEList a -> "NonEmpty" <> brackets (typeName a) --TODO: smart constructor?
      Binding_Dict k v -> "dict" <> brackets (typeName k <> "," <> typeName v)
      Binding_Dict' k v -> typeName (Binding_Dict k v)
      Binding_Imported name -> name
      Binding_Defined "Flow" -> "'Flow'" --TODO: https://peps.python.org/pep-0484/#forward-references
      Binding_Defined name -> name
      Binding_Generic -> "typing.Any"

newtype PyType a = PyType (Const Sig a)
  deriving newtype (Applicative, Contravariant, Functor, Invariant, Monoidal)

evalPyType :: StructEvaluator PyType
evalPyType = StructEvaluator
  { _structEvaluator_field = \(Named name b) -> PyType $ Const [(name, Just (Some b))]
  , _structEvaluator_todo   = PyType $ Const mempty
  }
