{-# LANGUAGE CPP, TemplateHaskell, FlexibleInstances, TupleSections #-}

{-|
Module:      Data.Aeson.Canonical

Based on:
  Module:      Data.Aeson.TH
  Copyright:   (c) 2011 Bryan O'Sullivan
               (c) 2011 MailRank, Inc.
  License:     Apache
  Stability:   experimental
  Portability: portable

-}

module Data.Aeson.Canonical
    ( Aeson (..)
    , deriveAeson
    , (.=)
    , (.:!)
    , (.:?)
    , typeMismatch
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from aeson:
import Data.Aeson ( toJSON, Object, object
                  , ToJSON, toJSON
                  , FromJSON, parseJSON
                  )
import Data.Aeson.Types ( Value(..), Parser, typeMismatch, Pair )
-- from base:
import Control.Applicative ( pure, (<$>), (<*>) )
import Control.Monad       ( return, mapM, liftM2, fail )
import Data.Bool           ( Bool (..), otherwise )
import Data.Eq             ( (==), (/=) )
import Data.Function       ( ($), (.), id )
import Data.Functor        ( fmap )
import Data.Hashable
import Data.Default
import Data.Traversable    ( traverse )
import Data.List           ( (++), foldl, foldl', intercalate
                           , length, map, zip, zip3, genericLength
                           )
import Data.Maybe          ( Maybe(Nothing, Just), fromMaybe )
import Text.Printf         ( printf )
import Text.Show           ( show )
#if __GLASGOW_HASKELL__ < 700
import Control.Monad       ( (>>=) )
import Prelude             ( fromInteger )
#endif
import qualified Data.Set as S
import qualified Data.Map as M 
-- from unordered-containers:
import qualified Data.HashMap.Strict as H
-- from template-haskell:
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
-- from text:
import qualified Data.Text as T ( Text, pack, unpack )
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
-- from vector:
import qualified Data.Vector as V ( unsafeIndex, null, length, create, toList, fromList)
import qualified Data.Vector.Mutable as VM ( unsafeNew, unsafeWrite )

class Aeson a where
  toAeson    :: a -> Value
  parseAeson :: Value -> Parser a

instance (Aeson a) => Aeson [a] where
  toAeson             = Array . V.fromList . map toAeson
  {-# INLINE toAeson #-}
  parseAeson (Array a) = mapM parseAeson (V.toList a)
  parseAeson v         = typeMismatch "[a]" v
  {-# INLINE parseAeson #-}

instance (Ord a, Aeson a) => Aeson (S.Set a) where
  toAeson             = toAeson . S.toList
  {-# INLINE toAeson #-}
  parseAeson x        = S.fromList <$> parseAeson x

instance Aeson () where
  toAeson             = toJSON
  {-# INLINE toAeson #-}
  parseAeson          = parseJSON
  {-# INLINE parseAeson #-}

instance Aeson T.Text where
  toAeson             = toJSON
  {-# INLINE toAeson #-}
  parseAeson          = parseJSON
  {-# INLINE parseAeson #-}

instance Aeson BS.ByteString where
  toAeson             = toJSON
  {-# INLINE toAeson #-}
  parseAeson          = parseJSON
  {-# INLINE parseAeson #-}

instance Aeson Int where
  toAeson             = toJSON
  {-# INLINE toAeson #-}
  parseAeson          = parseJSON
  {-# INLINE parseAeson #-}

instance Aeson Bool where
  toAeson             = toJSON
  {-# INLINE toAeson #-}
  parseAeson          = parseJSON
  {-# INLINE parseAeson #-}

instance (Aeson a) => Aeson (Maybe a) where
  toAeson Nothing     = Null
  toAeson (Just x)    = toAeson x
  {-# INLINE toAeson #-}
  parseAeson Null     = return Nothing
  parseAeson x        = Just <$> parseAeson x
  {-# INLINE parseAeson #-}

instance (Aeson a) => Aeson (H.HashMap T.Text a) where
  toAeson = Object . H.map toAeson
  {-# INLINE toAeson #-}
  parseAeson (Object o) = traverse parseAeson o
  parseAeson v          = typeMismatch "Hashmap Text a" v

instance (Aeson a) => Aeson (H.HashMap BS.ByteString a) where
  toAeson               = Object . mapKeyVal TE.decodeUtf8 toAeson
  {-# INLINE toAeson #-}
  parseAeson            = fmap (mapKey TE.encodeUtf8) . parseAeson
  {-# INLINE parseAeson #-}

instance (Aeson a) => Aeson (M.Map T.Text a) where
  toAeson               = Object . M.foldrWithKey (\k -> H.insert k . toAeson) H.empty
  {-# INLINE toAeson #-}
  parseAeson (Object o) = H.foldrWithKey M.insert M.empty <$> traverse parseAeson o
  parseAeson v          = typeMismatch "Map Text a" v
  {-# INLINE parseAeson #-}

instance (Aeson a, Aeson b) => Aeson (a,b) where
  toAeson (a,b) = Array $ V.create $ do
                     mv <- VM.unsafeNew 2
                     VM.unsafeWrite mv 0 (toAeson a)
                     VM.unsafeWrite mv 1 (toAeson b)
                     return mv
  {-# INLINE toAeson #-}
  parseAeson (Array ab)
        | n == 2    = (,) <$> parseAeson (V.unsafeIndex ab 0)
                          <*> parseAeson (V.unsafeIndex ab 1)
        | otherwise = fail $ "cannot unpack array of length " ++
                        show n ++ " into a pair"
          where
            n = V.length ab
  parseAeson v = typeMismatch "(a,b)" v
  {-# INLINE parseAeson #-}

instance Aeson Value where
  toAeson a    = a
  {-# INLINE toAeson #-}
  parseAeson a = pure a
  {-# INLINE parseAeson #-}

deriveAeson :: (String -> String)
             -- ^ Function to change field names.
             -> Name
             -- ^ Name of the type for which to generate a 'ToJSON' instance
             -- declaration.
             -> Q [Dec]
deriveAeson withField name =
    withType name $ \tvbs cons -> fmap (:[]) $ fromCons tvbs cons
  where
    fromCons :: [TyVarBndr] -> [Con] -> Q Dec
    fromCons tvbs cons =
        instanceD (return $ map (\t -> ClassP ''Aeson [VarT t]) typeNames)
                  (classType `appT` instanceType)
                  [ funD 'toAeson
                         [ clause []
                                  (normalB $ consToJSON withField cons)
                                  []
                         ]
                  , funD 'parseAeson (parseAesonClauses name withField cons)
                  ]
             
      where
        classType = conT ''Aeson
        typeNames = map tvbName tvbs
        instanceType = foldl' appT (conT name) $ map varT typeNames

mkToJSON :: (String -> String) -- ^ Function to change field names.
         -> Name -- ^ Name of the type to encode.
         -> Q Exp
mkToJSON withField name = withType name (\_ cons -> consToJSON withField cons)

consToJSON :: (String -> String)
           -- ^ Function to change field names.
           -> [Con]
           -- ^ Constructors for which to generate JSON generating code.
           -> Q Exp
consToJSON _ [] = error $ "Data.Aeson.TH.consToJSON: "
                          ++ "Not a single constructor given!"
consToJSON withField cons = do
    value <- newName "value"
    lam1E (varP value)
          $ caseE (varE value)
                  [ encodeArgs id withField con
                  | con <- cons
                  ]
  where
    wrap :: Name -> Q Exp -> Q Exp
    wrap name exp =
        let fieldName = [e|T.pack|] `appE` litE (stringL $ nameBase name)
        in [e|object|] `appE` listE [ infixApp fieldName
                                               [e|(.=)|]
                                               exp
                                    ]

encodeArgs :: (Q Exp -> Q Exp) -> (String -> String) -> Con -> Q Match
encodeArgs withExp fField constr = 
 do args       <- case constr of
                    (NormalC _      ts) -> sequence [ (,'_':(show i),t)        <$> newName "arg" | (i,t)   <- zip [1..] (map snd ts) ]
                    (RecC    _      ts) -> sequence [ (,fField $ nameBase n,t) <$> newName "arg" | (n,_,t) <- ts                     ]
    let js = [ infixApp ([e|T.pack|] `appE` (litE $ stringL $ field))
                        [e|(.=)|]
                        (varE arg)
             | (arg, field, t) <- args
             , not (isMaybe t)
             ]
             ++
             [ infixApp ([e|T.pack|] `appE` litE (stringL "_"))
                        [e|(.=)|]
                        ([e|T.pack|] `appE` (litE $ stringL $ nameBase $ getConName constr))
             ]
    match (conP (getConName constr) $ map varP $ map (\(a,_,_)->a) args)
          (normalB $ withExp $ [e|object|] `appE` listE js)
          []

parseAesonClauses :: Name -> (String -> String) -> [Con] -> [Q Clause] 
parseAesonClauses tName withField cons
  = [ do obj <- newName "obj"
         clause 
           [conP 'Object [varP obj]]
           (normalB $ dispatchConstructors obj)
           []
    , do other <- newName "other"
         clause
           [varP other]
           (normalB $ parseTypeMismatch 
                        tName
                        (mkName "foo") 
                        (litE $ stringL "Object")
                        ([|valueConName|] `appE` varE other)
           )
           []
    ]
  where
    dispatchConstructors obj
      = caseE
          ([e|H.lookup|] `appE` litE (stringL "_") `appE` varE obj)
          ( map (matchConstructor obj) cons
            ++
            [match
              wildP
              (normalB $ [|missingConstructorFail|]
                         `appE` (litE $ stringL $ show tName)
                         `appE` ([|show|] `appE` varE obj)
              ) 
              []
            ]
          )
    matchConstructor obj con
      = match
          (conP 'Just [litP $ stringL $ nameBase $ getConName con])
          (normalB $ extractArgs obj con)
          []
    extractArgs obj con
      = do let ts = case con of
                     (NormalC _ xs) -> [(mkName $ ('_':) $ show i,t)|(i,(_,t)) <- zip [1..] xs]
                     (RecC _ xs)    -> map (\(n,_,t)->(n,t)) xs
           let xs = [ let fieldName =  [e|T.pack|] `appE` fieldNameExp withField field
                      in  caseE
                            ( [|H.lookup|] `appE` fieldName `appE` varE obj )
                            [ (\e-> match [p|Nothing|] e []) $ normalB $ do b <- isClassInstance ''Default [t]
                                                                            if b || isMaybe t
                                                                              then [|return def|] -- 'Nothing == def' for 'Maybe a'!
                                                                              else [|\x-> fail $ "Missing field '" ++ x ++ "'."|] `appE` fieldNameExp withField field 
                            , newName "x" >>= \x-> match (conP 'Just [varP x]) (normalB $ [|parseAeson|] `appE` varE x) [] 
                            ]
                    | (field, t) <- ts
                    ]
           args <- mapM (const $ newName "x") ts
           doE $ ( zipWith
                     (\v a-> bindS (varP v) a)
                     args
                     xs
                 )
                 ++
                 [ noBindS $ foldr -- strictness!
                              (\m n-> infixApp
                                        m
                                        [|seq|]
                                        n
                              )
                              ( [|return|] `appE` (foldl
                                                     appE
                                                     (conE $ getConName con)
                                                     (map varE args)
                                                  )
                              )
                              (map varE args)
                 ]


--------------------------------------------------------------------------------
-- Parsing errors
--------------------------------------------------------------------------------

isMaybe :: Type -> Bool
isMaybe (AppT a _) = case a of
                      (ConT n) -> n == ''Maybe 
                      _        -> False
isMaybe _          = False


matchFailed :: Name -> Name -> String -> MatchQ
matchFailed tName conName expected = do
  other <- newName "other"
  match (varP other)
        ( normalB $ parseTypeMismatch tName conName
                      (litE $ stringL expected)
                      ([|valueConName|] `appE` varE other)
        )
        []

parseTypeMismatch :: Name -> Name -> ExpQ -> ExpQ -> ExpQ
parseTypeMismatch tName conName expected actual =
    foldl appE
          [|parseTypeMismatch'|]
          [ litE $ stringL $ nameBase conName
          , litE $ stringL $ show tName
          , expected
          , actual
          ]

lookupField :: (Aeson a) => String -> String -> Object -> T.Text -> Parser a
lookupField tName rec obj key =
    case H.lookup key obj of
      Nothing -> unknownFieldFail tName rec (T.unpack key)
      Just v  -> parseAeson v

missingConstructorFail :: String -> String -> Parser fail
missingConstructorFail tName rec =
    fail $ printf "When parsing the record %s of type %s the constructor was not present or unknown in '_'."
                  rec tName

unknownFieldFail :: String -> String -> String -> Parser fail
unknownFieldFail tName rec key =
    fail $ printf "When parsing the record %s of type %s the key %s was not present."
                  rec tName key

noObjectFail :: String -> String -> Parser fail
noObjectFail t o =
    fail $ printf "When parsing %s expected Object but got %s." t o

wrongPairCountFail :: String -> String -> Parser fail
wrongPairCountFail t n =
    fail $ printf "When parsing %s expected an Object with a single name/value pair but got %s pairs."
                  t n

conNotFoundFail :: String -> [String] -> String -> Parser fail
conNotFoundFail t cs o =
    fail $ printf "When parsing %s expected an Object with a name/value pair where the name is one of [%s], but got %s."
                  t (intercalate ", " cs) o

parseTypeMismatch' :: String -> String -> String -> String -> Parser fail
parseTypeMismatch' tName conName expected actual =
    fail $ printf "When parsing the constructor %s of type %s expected %s but got %s."
                  conName tName expected actual


--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- | Boilerplate for top level splices.
--
-- The given 'Name' must be from a type constructor. Furthermore, the
-- type constructor must be either a data type or a newtype. Any other
-- value will result in an exception.
withType :: Name
         -> ([TyVarBndr] -> [Con] -> Q a)
         -- ^ Function that generates the actual code. Will be applied
         -- to the type variable binders and constructors extracted
         -- from the given 'Name'.
         -> Q a
         -- ^ Resulting value in the 'Q'uasi monad.
withType name f = do
    info <- reify name
    case info of
      TyConI dec ->
        case dec of
          DataD    _ _ tvbs cons _ -> f tvbs cons
          NewtypeD _ _ tvbs con  _ -> f tvbs [con]
          other -> error $ "Data.Aeson.TH.withType: Unsupported type: "
                          ++ show other
      _ -> error "Data.Aeson.TH.withType: I need the name of a type."

-- | Extracts the name from a constructor.
getConName :: Con -> Name
getConName (NormalC name _)  = name
getConName (RecC name _)     = name
getConName (InfixC _ name _) = name
getConName (ForallC _ _ con) = getConName con

-- | Extracts the name from a type variable binder.
tvbName :: TyVarBndr -> Name
tvbName (PlainTV  name  ) = name
tvbName (KindedTV name _) = name

-- | Makes a string literal expression from a constructor's name.
conNameExp :: Con -> Q Exp
conNameExp = litE . stringL . nameBase . getConName

-- | Creates a string literal expression from a record field name.
fieldNameExp :: (String -> String) -- ^ Function to change the field name.
             -> Name
             -> Q Exp
fieldNameExp f = litE . stringL . f . nameBase

-- | The name of the outermost 'Value' constructor.
valueConName :: Value -> String
valueConName (Object _) = "Object"
valueConName (Array  _) = "Array"
valueConName (String _) = "String"
valueConName (Number _) = "Number"
valueConName (Bool   _) = "Boolean"
valueConName Null       = "Null"

(.=) :: Aeson a => T.Text -> a -> Pair
name .= value = (name, toAeson value)
{-# INLINE (.=) #-}

(.:?) :: (Aeson a) => Object -> T.Text -> Parser (Maybe a)
obj .:? key = case H.lookup key obj of
               Nothing -> pure Nothing
               Just v  -> Just <$> parseAeson v
{-# INLINE (.:?) #-}

(.:!) :: (Aeson a, Default a) => Object -> T.Text -> Parser a 
o .:! t = fromMaybe def <$> (o .:? t)
{-# INLINE (.:!) #-}

-- | Transform the keys and values of a 'H.HashMap'.
mapKeyVal :: (Eq k2, Hashable k2) => (k1 -> k2) -> (v1 -> v2)
          -> H.HashMap k1 v1 -> H.HashMap k2 v2
mapKeyVal fk kv = H.foldrWithKey (\k v -> H.insert (fk k) (kv v)) H.empty
{-# INLINE mapKeyVal #-}

-- | Transform the keys of a 'H.HashMap'.
mapKey :: (Eq k2, Hashable k2) => (k1 -> k2) -> H.HashMap k1 v -> H.HashMap k2 v
mapKey fk = mapKeyVal fk id
{-# INLINE mapKey #-}


