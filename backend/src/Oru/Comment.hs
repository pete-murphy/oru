module Oru.Comment where

import Data.JsonSpec
import Data.Text (Text)
import Prelude

data Comment = Comment
  { commentTitle :: Text,
    commentSlug :: Text,
    commentRating :: Maybe Int,
    commentBody :: Text
  }
  deriving (Show)

type CommentSpec =
  JsonObject
    '[ '("title", JsonString),
       '("slug", JsonString),
       '("rating", JsonNullable JsonInt),
       '("body", JsonString)
     ]

instance HasJsonEncodingSpec Comment where
  type EncodingSpec Comment = CommentSpec
  toJSONStructure Comment {..} =
    Field @"title" commentTitle
      /\ Field @"slug" commentSlug
      /\ Field @"rating" commentRating
      /\ Field @"body" commentBody
      /\ ()

instance HasJsonDecodingSpec Comment where
  type DecodingSpec Comment = CommentSpec
  fromJSONStructure
    ( Field @"title" commentTitle,
      ( Field @"slug" commentSlug,
        ( Field @"rating" commentRating,
          ( Field @"body" commentBody,
            ()
            )
          )
        )
      ) = pure Comment {..}

(/\) :: a -> b -> (a, b)
(/\) = (,)

infixr 9 /\
