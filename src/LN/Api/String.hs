{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.Api.String where




import Data.Int                   (Int64)
import Data.Monoid                ((<>))
import Data.Text                  (Text)
import Haskell.Api.Helpers.Shared (ApiEff, ApiError, QueryParam, qp)
import Haskell.Api.Helpers        (SpecificApiOptions, handleError, getAt, putAt, postAt, deleteAt)
import Prelude


import LN.T

getUserSanitizedPack :: forall qp. QueryParam qp => [qp] -> Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedPackResponse)
getUserSanitizedPack params user_name = handleError <$> getAt params ["user_sanitized_pack", user_name]

getUserSanitizedPack' :: Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedPackResponse)
getUserSanitizedPack' user_name = handleError <$> getAt ([] :: [(Text, Text)]) ["user_sanitized_pack", user_name]

getForumPack :: forall qp. QueryParam qp => [qp] -> Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponse)
getForumPack params forum_sid = handleError <$> getAt params ["forum_pack", forum_sid]

getForumPack' :: Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponse)
getForumPack' forum_sid = handleError <$> getAt ([] :: [(Text, Text)]) ["forum_pack", forum_sid]

getBoardPack :: forall qp. QueryParam qp => [qp] -> Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponse)
getBoardPack params board_sid = handleError <$> getAt params ["board_pack", board_sid]

getBoardPack' :: Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponse)
getBoardPack' board_sid = handleError <$> getAt ([] :: [(Text, Text)]) ["board_pack", board_sid]

getThreadPack :: forall qp. QueryParam qp => [qp] -> Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPackResponse)
getThreadPack params thread_sid = handleError <$> getAt params ["thread_pack", thread_sid]

getThreadPack' :: Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPackResponse)
getThreadPack' thread_sid = handleError <$> getAt ([] :: [(Text, Text)]) ["thread_pack", thread_sid]

-- footer