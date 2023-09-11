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

(/\) :: a -> b -> (a, b)
(/\) = (,)

infixr 9 /\

instance HasJsonEncodingSpec Comment where
  type
    EncodingSpec Comment =
      JsonObject
        '[ '("title", JsonString),
           '("slug", JsonString),
           '("rating", JsonNullable JsonInt),
           '("body", JsonString)
         ]
  toJSONStructure Comment {..} =
    Field @"title" commentTitle
      /\ Field @"slug" commentSlug
      /\ Field @"rating" commentRating
      /\ Field @"body" commentBody
      /\ ()

instance HasJsonDecodingSpec Comment where
  type
    DecodingSpec Comment =
      JsonObject
        '[ '("title", JsonString),
           '("slug", JsonString),
           '("rating", JsonNullable JsonInt),
           '("body", JsonString)
         ]
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
