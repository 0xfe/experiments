{-# LANGUAGE TypeSynonymInstances #-}

data JValue = JBool Bool |
              JString String
              deriving (Show)

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Maybe a

instance JSON JValue where
  toJValue = id
  fromJValue = Just

instance JSON Bool where
  toJValue = JBool
  fromJValue (JBool b) = Just b
  fromJValue _ = Nothing

fromMaybe defval Nothing = defval
fromMaybe defval (Just v) = v

main = do
  print $ fromMaybe "Nothing" (Just "A")
  print $ fromMaybe "Nothing" Nothing
  print $ toJValue True
  print $ ((fromJValue (toJValue False)) :: Maybe Bool)
