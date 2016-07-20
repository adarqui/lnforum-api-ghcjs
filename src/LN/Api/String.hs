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


import LN.T

getUserSanitizedPack :: forall qp. QueryParam qp => [qp] -> Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedPackResponse)
getUserSanitizedPack params user_name = handleError <$> getAt params ["user_sanitized_pack", user_name]

getUserSanitizedPack' :: Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedPackResponse)
getUserSanitizedPack' user_name = handleError <$> getAt ([] :: [(Text, Text)]) ["user_sanitized_pack", user_name]

getOrganization :: forall qp. QueryParam qp => [qp] -> Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationResponse)
getOrganization params organization_name = handleError <$> getAt params ["organization", organization_name]

getOrganization' :: Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationResponse)
getOrganization' organization_name = handleError <$> getAt ([] :: [(Text, Text)]) ["organization", organization_name]

getOrganizationPack :: forall qp. QueryParam qp => [qp] -> Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationPackResponse)
getOrganizationPack params organization_name = handleError <$> getAt params ["organization_pack", organization_name]

getOrganizationPack' :: Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationPackResponse)
getOrganizationPack' organization_name = handleError <$> getAt ([] :: [(Text, Text)]) ["organization_pack", organization_name]

getTeam_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamResponse)
getTeam_ByOrganizationId params team_name _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["team", team_name]

getTeam_ByOrganizationId' :: Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamResponse)
getTeam_ByOrganizationId' team_name _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["team", team_name]

getTeamPack_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamPackResponse)
getTeamPack_ByOrganizationId params team_name _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["team_pack", team_name]

getTeamPack_ByOrganizationId' :: Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamPackResponse)
getTeamPack_ByOrganizationId' team_name _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["team_pack", team_name]

getGlobalGroup_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupResponse)
getGlobalGroup_ByOrganizationId params global_group_name _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["global_group", global_group_name]

getGlobalGroup_ByOrganizationId' :: Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupResponse)
getGlobalGroup_ByOrganizationId' global_group_name _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["global_group", global_group_name]

getGlobalGroupPack_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupPackResponse)
getGlobalGroupPack_ByOrganizationId params global_group_name _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["global_group_pack", global_group_name]

getGlobalGroupPack_ByOrganizationId' :: Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupPackResponse)
getGlobalGroupPack_ByOrganizationId' global_group_name _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["global_group_pack", global_group_name]

getGroup_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupResponse)
getGroup_ByOrganizationId params group_name _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["group", group_name]

getGroup_ByOrganizationId' :: Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupResponse)
getGroup_ByOrganizationId' group_name _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["group", group_name]

getGroupPack_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupPackResponse)
getGroupPack_ByOrganizationId params group_name _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["group_pack", group_name]

getGroupPack_ByOrganizationId' :: Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupPackResponse)
getGroupPack_ByOrganizationId' group_name _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["group_pack", group_name]

getForum_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponse)
getForum_ByOrganizationId params forum_name _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["forum", forum_name]

getForum_ByOrganizationId' :: Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponse)
getForum_ByOrganizationId' forum_name _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["forum", forum_name]

getForumPack_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponse)
getForumPack_ByOrganizationId params forum_name _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["forum_pack", forum_name]

getForumPack_ByOrganizationId' :: Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponse)
getForumPack_ByOrganizationId' forum_name _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["forum_pack", forum_name]

getBoard_ByForumId :: forall qp. QueryParam qp => [qp] -> Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
getBoard_ByForumId params board_name _ByForumId = handleError <$> getAt (map qp params <> map qp [ByForumId _ByForumId]) ["board", board_name]

getBoard_ByForumId' :: Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
getBoard_ByForumId' board_name _ByForumId = handleError <$> getAt [ByForumId _ByForumId] ["board", board_name]

getBoardPack_ByForumId :: forall qp. QueryParam qp => [qp] -> Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponse)
getBoardPack_ByForumId params board_name _ByForumId = handleError <$> getAt (map qp params <> map qp [ByForumId _ByForumId]) ["board_pack", board_name]

getBoardPack_ByForumId' :: Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponse)
getBoardPack_ByForumId' board_name _ByForumId = handleError <$> getAt [ByForumId _ByForumId] ["board_pack", board_name]

getThread_ByBoardId :: forall qp. QueryParam qp => [qp] -> Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponse)
getThread_ByBoardId params thread_name _ByBoardId = handleError <$> getAt (map qp params <> map qp [ByBoardId _ByBoardId]) ["thread", thread_name]

getThread_ByBoardId' :: Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponse)
getThread_ByBoardId' thread_name _ByBoardId = handleError <$> getAt [ByBoardId _ByBoardId] ["thread", thread_name]

getThreadPack_ByBoardId :: forall qp. QueryParam qp => [qp] -> Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPackResponse)
getThreadPack_ByBoardId params thread_name _ByBoardId = handleError <$> getAt (map qp params <> map qp [ByBoardId _ByBoardId]) ["thread_pack", thread_name]

getThreadPack_ByBoardId' :: Text -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPackResponse)
getThreadPack_ByBoardId' thread_name _ByBoardId = handleError <$> getAt [ByBoardId _ByBoardId] ["thread_pack", thread_name]

-- footer