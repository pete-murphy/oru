{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Oru.Comment
  ( Comment (..),
    Preview (..),
    Full (..),
    Internals (..),
    PreviewComment (..),
    FullComment (..),
    PreviewCommentSpec,
    FullCommentSpec,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.JsonSpec
import Data.Text (Text)
import Prelude

data Comment a
  = Comment Internals a

data Internals = Internals
  { commentTitle :: Text,
    commentSlug :: Text,
    commentRating :: Maybe Int
  }
  deriving (Show)

data Preview = Preview

newtype Full = Full Text

type CommonFields =
  '[ '("title", JsonString),
     '("slug", JsonString),
     '("rating", JsonNullable JsonInt)
   ]

newtype PreviewComment
  = PreviewComment (Comment Preview)
  deriving (ToJSON, FromJSON) via (SpecJSON (Comment Preview))

type PreviewCommentSpec =
  JsonObject CommonFields

newtype FullComment
  = FullComment (Comment Full)
  deriving (ToJSON, FromJSON) via (SpecJSON (Comment Full))

type FullCommentSpec =
  JsonObject
    ( CommonFields
        ++ '[ '("body", JsonString)]
    )

instance HasJsonEncodingSpec (Comment Preview) where
  type EncodingSpec (Comment Preview) = PreviewCommentSpec
  toJSONStructure (Comment (Internals {..}) Preview) =
    Field @"title" commentTitle
      /\ Field @"slug" commentSlug
      /\ Field @"rating" commentRating
      /\ ()

instance HasJsonDecodingSpec (Comment Preview) where
  type DecodingSpec (Comment Preview) = PreviewCommentSpec
  fromJSONStructure
    ( Field @"title" commentTitle,
      ( Field @"slug" commentSlug,
        ( Field @"rating" commentRating,
          ()
          )
        )
      ) = pure (Comment (Internals {..}) Preview)

instance HasJsonEncodingSpec (Comment Full) where
  type EncodingSpec (Comment Full) = FullCommentSpec
  toJSONStructure (Comment (Internals {..}) (Full commentBody)) =
    Field @"title" commentTitle
      /\ Field @"slug" commentSlug
      /\ Field @"rating" commentRating
      /\ Field @"body" commentBody
      /\ ()

instance HasJsonDecodingSpec (Comment Full) where
  type DecodingSpec (Comment Full) = FullCommentSpec
  fromJSONStructure
    ( Field @"title" commentTitle,
      ( Field @"slug" commentSlug,
        ( Field @"rating" commentRating,
          ( Field @"body" commentBody,
            ()
            )
          )
        )
      ) = pure (Comment (Internals {..}) (Full commentBody))

-- Appendix

(/\) :: a -> b -> (a, b)
(/\) = (,)

infixr 9 /\

type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[] ++ bs = bs
  (a :< as) ++ bs = a :< (as ++ bs)

infixr 5 ++

type (:<) = '(:)

infixr 5 :<
