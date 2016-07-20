{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.Api where




import Data.Int                   (Int64)
import Data.Monoid                ((<>))
import Data.Text                  (Text)
import qualified Data.Text        as T (pack)
import Haskell.Api.Helpers.Shared (ApiEff, ApiError, QueryParam, qp)
import Haskell.Api.Helpers        (SpecificApiOptions, handleError, getAt, putAt, postAt, deleteAt)
import Data.Default               (Default, def)


import LN.T

getApis :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ApiResponses)
getApis params = handleError <$> getAt params ["apis"]

getApis' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ApiResponses)
getApis'  = handleError <$> getAt ([] :: [(Text, Text)]) ["apis"]

postApi :: forall qp. QueryParam qp => [qp] -> ApiRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ApiResponse)
postApi params api_request = handleError <$> postAt params ["api"] api_request

postApi' :: ApiRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ApiResponse)
postApi' api_request = handleError <$> postAt ([] :: [(Text, Text)]) ["api"] api_request

getApi :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ApiResponse)
getApi params api_id = handleError <$> getAt params ["api", T.pack $ show api_id]

getApi' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ApiResponse)
getApi' api_id = handleError <$> getAt ([] :: [(Text, Text)]) ["api", T.pack $ show api_id]

putApi :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ApiResponse)
putApi params api_id api_request = handleError <$> putAt params ["api", T.pack $ show api_id] api_request

putApi' :: Int64 -> ApiRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ApiResponse)
putApi' api_id api_request = handleError <$> putAt ([] :: [(Text, Text)]) ["api", T.pack $ show api_id] api_request

deleteApi :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteApi params api_id = handleError <$> deleteAt params ["api", T.pack $ show api_id]

deleteApi' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteApi' api_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["api", T.pack $ show api_id]

getBoards :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponses)
getBoards params = handleError <$> getAt params ["boards"]

getBoards' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponses)
getBoards'  = handleError <$> getAt ([] :: [(Text, Text)]) ["boards"]

getBoards_ByBoardsIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponses)
getBoards_ByBoardsIds params _ByBoardsIds = handleError <$> getAt (map qp params <> map qp [ByBoardsIds _ByBoardsIds]) ["boards"]

getBoards_ByBoardsIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponses)
getBoards_ByBoardsIds' _ByBoardsIds = handleError <$> getAt [ByBoardsIds _ByBoardsIds] ["boards"]

getBoards_ByForumId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponses)
getBoards_ByForumId params _ByForumId = handleError <$> getAt (map qp params <> map qp [ByForumId _ByForumId]) ["boards"]

getBoards_ByForumId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponses)
getBoards_ByForumId' _ByForumId = handleError <$> getAt [ByForumId _ByForumId] ["boards"]

postBoard_ByForumId :: forall qp. QueryParam qp => [qp] -> Int64 -> BoardRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
postBoard_ByForumId params _ByForumId board_request = handleError <$> postAt (map qp params <> map qp [ByForumId _ByForumId]) ["board"] board_request

postBoard_ByForumId' :: Int64 -> BoardRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
postBoard_ByForumId' _ByForumId board_request = handleError <$> postAt [ByForumId _ByForumId] ["board"] board_request

postBoard_ByBoardId :: forall qp. QueryParam qp => [qp] -> Int64 -> BoardRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
postBoard_ByBoardId params _ByBoardId board_request = handleError <$> postAt (map qp params <> map qp [ByBoardId _ByBoardId]) ["board"] board_request

postBoard_ByBoardId' :: Int64 -> BoardRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
postBoard_ByBoardId' _ByBoardId board_request = handleError <$> postAt [ByBoardId _ByBoardId] ["board"] board_request

getBoard :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
getBoard params board_id = handleError <$> getAt params ["board", T.pack $ show board_id]

getBoard' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
getBoard' board_id = handleError <$> getAt ([] :: [(Text, Text)]) ["board", T.pack $ show board_id]

putBoard :: forall qp. QueryParam qp => [qp] -> Int64 -> BoardRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
putBoard params board_id board_request = handleError <$> putAt params ["board", T.pack $ show board_id] board_request

putBoard' :: Int64 -> BoardRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
putBoard' board_id board_request = handleError <$> putAt ([] :: [(Text, Text)]) ["board", T.pack $ show board_id] board_request

deleteBoard :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteBoard params board_id = handleError <$> deleteAt params ["board", T.pack $ show board_id]

deleteBoard' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteBoard' board_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["board", T.pack $ show board_id]

getBoardStats :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardStatResponses)
getBoardStats params = handleError <$> getAt params ["board_stats"]

getBoardStats' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardStatResponses)
getBoardStats'  = handleError <$> getAt ([] :: [(Text, Text)]) ["board_stats"]

getBoardStat :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardStatResponse)
getBoardStat params board_id = handleError <$> getAt params ["board_stat", T.pack $ show board_id]

getBoardStat' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardStatResponse)
getBoardStat' board_id = handleError <$> getAt ([] :: [(Text, Text)]) ["board_stat", T.pack $ show board_id]

getUsersCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getUsersCount params = handleError <$> getAt params ["users_count"]

getUsersCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getUsersCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["users_count"]

getOrganizationsCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getOrganizationsCount params = handleError <$> getAt params ["organizations_count"]

getOrganizationsCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getOrganizationsCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["organizations_count"]

getTeamsCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getTeamsCount params = handleError <$> getAt params ["teams_count"]

getTeamsCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getTeamsCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["teams_count"]

getTeamsCount_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getTeamsCount_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["teams_count"]

getTeamsCount_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getTeamsCount_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["teams_count"]

getTeamMembersCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getTeamMembersCount params = handleError <$> getAt params ["team_members_count"]

getTeamMembersCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getTeamMembersCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["team_members_count"]

getTeamMembersCount_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getTeamMembersCount_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["team_members_count"]

getTeamMembersCount_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getTeamMembersCount_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["team_members_count"]

getTeamMembersCount_ByTeamId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getTeamMembersCount_ByTeamId params _ByTeamId = handleError <$> getAt (map qp params <> map qp [ByTeamId _ByTeamId]) ["team_members_count"]

getTeamMembersCount_ByTeamId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getTeamMembersCount_ByTeamId' _ByTeamId = handleError <$> getAt [ByTeamId _ByTeamId] ["team_members_count"]

getGlobalGroupsCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getGlobalGroupsCount params = handleError <$> getAt params ["global_groups_count"]

getGlobalGroupsCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getGlobalGroupsCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["global_groups_count"]

getGlobalGroupsCount_ByUserId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getGlobalGroupsCount_ByUserId params _ByUserId = handleError <$> getAt (map qp params <> map qp [ByUserId _ByUserId]) ["global_groups_count"]

getGlobalGroupsCount_ByUserId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getGlobalGroupsCount_ByUserId' _ByUserId = handleError <$> getAt [ByUserId _ByUserId] ["global_groups_count"]

getGroupsCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getGroupsCount params = handleError <$> getAt params ["groups_count"]

getGroupsCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getGroupsCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["groups_count"]

getGroupsCount_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getGroupsCount_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["groups_count"]

getGroupsCount_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getGroupsCount_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["groups_count"]

getGroupMembersCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getGroupMembersCount params = handleError <$> getAt params ["group_members_count"]

getGroupMembersCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getGroupMembersCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["group_members_count"]

getGroupMembersCount_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getGroupMembersCount_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["group_members_count"]

getGroupMembersCount_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getGroupMembersCount_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["group_members_count"]

getGroupMembersCount_ByGroupId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getGroupMembersCount_ByGroupId params _ByGroupId = handleError <$> getAt (map qp params <> map qp [ByGroupId _ByGroupId]) ["group_members_count"]

getGroupMembersCount_ByGroupId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getGroupMembersCount_ByGroupId' _ByGroupId = handleError <$> getAt [ByGroupId _ByGroupId] ["group_members_count"]

getForumsCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getForumsCount params = handleError <$> getAt params ["forums_count"]

getForumsCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getForumsCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["forums_count"]

getBoardsCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getBoardsCount params = handleError <$> getAt params ["boards_count"]

getBoardsCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getBoardsCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["boards_count"]

getThreadsCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadsCount params = handleError <$> getAt params ["threads_count"]

getThreadsCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadsCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["threads_count"]

getThreadsCount_ByBoardId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadsCount_ByBoardId params _ByBoardId = handleError <$> getAt (map qp params <> map qp [ByBoardId _ByBoardId]) ["threads_count"]

getThreadsCount_ByBoardId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadsCount_ByBoardId' _ByBoardId = handleError <$> getAt [ByBoardId _ByBoardId] ["threads_count"]

getThreadPostsCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadPostsCount params = handleError <$> getAt params ["thread_posts_count"]

getThreadPostsCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadPostsCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["thread_posts_count"]

getThreadPostsCount_ByThreadId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadPostsCount_ByThreadId params _ByThreadId = handleError <$> getAt (map qp params <> map qp [ByThreadId _ByThreadId]) ["thread_posts_count"]

getThreadPostsCount_ByThreadId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadPostsCount_ByThreadId' _ByThreadId = handleError <$> getAt [ByThreadId _ByThreadId] ["thread_posts_count"]

getResourcesCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getResourcesCount params = handleError <$> getAt params ["resources_count"]

getResourcesCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getResourcesCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["resources_count"]

getLeuronsCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getLeuronsCount params = handleError <$> getAt params ["leurons_count"]

getLeuronsCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getLeuronsCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["leurons_count"]

getForums :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponses)
getForums params = handleError <$> getAt params ["forums"]

getForums' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponses)
getForums'  = handleError <$> getAt ([] :: [(Text, Text)]) ["forums"]

getForums_ByOrganizationName :: forall qp. QueryParam qp => [qp] -> Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponses)
getForums_ByOrganizationName params _ByOrganizationName = handleError <$> getAt (map qp params <> map qp [ByOrganizationName _ByOrganizationName]) ["forums"]

getForums_ByOrganizationName' :: Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponses)
getForums_ByOrganizationName' _ByOrganizationName = handleError <$> getAt [ByOrganizationName _ByOrganizationName] ["forums"]

getForums_ByForumsIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponses)
getForums_ByForumsIds params _ByForumsIds = handleError <$> getAt (map qp params <> map qp [ByForumsIds _ByForumsIds]) ["forums"]

getForums_ByForumsIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponses)
getForums_ByForumsIds' _ByForumsIds = handleError <$> getAt [ByForumsIds _ByForumsIds] ["forums"]

getForums_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponses)
getForums_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["forums"]

getForums_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponses)
getForums_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["forums"]

postForum_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ForumRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponse)
postForum_ByOrganizationId params _ByOrganizationId forum_request = handleError <$> postAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["forum"] forum_request

postForum_ByOrganizationId' :: Int64 -> ForumRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponse)
postForum_ByOrganizationId' _ByOrganizationId forum_request = handleError <$> postAt [ByOrganizationId _ByOrganizationId] ["forum"] forum_request

getForum :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponse)
getForum params forum_id = handleError <$> getAt params ["forum", T.pack $ show forum_id]

getForum' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponse)
getForum' forum_id = handleError <$> getAt ([] :: [(Text, Text)]) ["forum", T.pack $ show forum_id]

putForum :: forall qp. QueryParam qp => [qp] -> Int64 -> ForumRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponse)
putForum params forum_id forum_request = handleError <$> putAt params ["forum", T.pack $ show forum_id] forum_request

putForum' :: Int64 -> ForumRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponse)
putForum' forum_id forum_request = handleError <$> putAt ([] :: [(Text, Text)]) ["forum", T.pack $ show forum_id] forum_request

deleteForum :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteForum params forum_id = handleError <$> deleteAt params ["forum", T.pack $ show forum_id]

deleteForum' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteForum' forum_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["forum", T.pack $ show forum_id]

getForumStats :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumStatResponses)
getForumStats params = handleError <$> getAt params ["forum_stats"]

getForumStats' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumStatResponses)
getForumStats'  = handleError <$> getAt ([] :: [(Text, Text)]) ["forum_stats"]

getForumStat :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumStatResponse)
getForumStat params forum_id = handleError <$> getAt params ["forum_stat", T.pack $ show forum_id]

getForumStat' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumStatResponse)
getForumStat' forum_id = handleError <$> getAt ([] :: [(Text, Text)]) ["forum_stat", T.pack $ show forum_id]

getGlobalGroups :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupResponses)
getGlobalGroups params = handleError <$> getAt params ["global_groups"]

getGlobalGroups' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupResponses)
getGlobalGroups'  = handleError <$> getAt ([] :: [(Text, Text)]) ["global_groups"]

getGlobalGroups_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupResponses)
getGlobalGroups_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["global_groups"]

getGlobalGroups_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupResponses)
getGlobalGroups_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["global_groups"]

postGlobalGroup :: forall qp. QueryParam qp => [qp] -> GlobalGroupRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupResponse)
postGlobalGroup params global_group_request = handleError <$> postAt params ["global_group"] global_group_request

postGlobalGroup' :: GlobalGroupRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupResponse)
postGlobalGroup' global_group_request = handleError <$> postAt ([] :: [(Text, Text)]) ["global_group"] global_group_request

getGlobalGroup :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupResponse)
getGlobalGroup params global_group_id = handleError <$> getAt params ["global_group", T.pack $ show global_group_id]

getGlobalGroup' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupResponse)
getGlobalGroup' global_group_id = handleError <$> getAt ([] :: [(Text, Text)]) ["global_group", T.pack $ show global_group_id]

putGlobalGroup :: forall qp. QueryParam qp => [qp] -> Int64 -> GlobalGroupRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupResponse)
putGlobalGroup params global_group_id global_group_request = handleError <$> putAt params ["global_group", T.pack $ show global_group_id] global_group_request

putGlobalGroup' :: Int64 -> GlobalGroupRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupResponse)
putGlobalGroup' global_group_id global_group_request = handleError <$> putAt ([] :: [(Text, Text)]) ["global_group", T.pack $ show global_group_id] global_group_request

deleteGlobalGroup :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteGlobalGroup params global_group_id = handleError <$> deleteAt params ["global_group", T.pack $ show global_group_id]

deleteGlobalGroup' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteGlobalGroup' global_group_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["global_group", T.pack $ show global_group_id]

getGroups :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupResponses)
getGroups params = handleError <$> getAt params ["groups"]

getGroups' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupResponses)
getGroups'  = handleError <$> getAt ([] :: [(Text, Text)]) ["groups"]

getGroups_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupResponses)
getGroups_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["groups"]

getGroups_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupResponses)
getGroups_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["groups"]

postGroup_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> GroupRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupResponse)
postGroup_ByOrganizationId params _ByOrganizationId group_request = handleError <$> postAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["group"] group_request

postGroup_ByOrganizationId' :: Int64 -> GroupRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupResponse)
postGroup_ByOrganizationId' _ByOrganizationId group_request = handleError <$> postAt [ByOrganizationId _ByOrganizationId] ["group"] group_request

getGroup :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupResponse)
getGroup params group_id = handleError <$> getAt params ["group", T.pack $ show group_id]

getGroup' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupResponse)
getGroup' group_id = handleError <$> getAt ([] :: [(Text, Text)]) ["group", T.pack $ show group_id]

putGroup :: forall qp. QueryParam qp => [qp] -> Int64 -> GroupRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupResponse)
putGroup params group_id group_request = handleError <$> putAt params ["group", T.pack $ show group_id] group_request

putGroup' :: Int64 -> GroupRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupResponse)
putGroup' group_id group_request = handleError <$> putAt ([] :: [(Text, Text)]) ["group", T.pack $ show group_id] group_request

deleteGroup :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteGroup params group_id = handleError <$> deleteAt params ["group", T.pack $ show group_id]

deleteGroup' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteGroup' group_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["group", T.pack $ show group_id]

getGroupMembers :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberResponses)
getGroupMembers params = handleError <$> getAt params ["group_members"]

getGroupMembers' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberResponses)
getGroupMembers'  = handleError <$> getAt ([] :: [(Text, Text)]) ["group_members"]

getGroupMembers_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberResponses)
getGroupMembers_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["group_members"]

getGroupMembers_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberResponses)
getGroupMembers_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["group_members"]

getGroupMembers_ByGlobalGroupId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberResponses)
getGroupMembers_ByGlobalGroupId params _ByGlobalGroupId = handleError <$> getAt (map qp params <> map qp [ByGlobalGroupId _ByGlobalGroupId]) ["group_members"]

getGroupMembers_ByGlobalGroupId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberResponses)
getGroupMembers_ByGlobalGroupId' _ByGlobalGroupId = handleError <$> getAt [ByGlobalGroupId _ByGlobalGroupId] ["group_members"]

getGroupMembers_ByGroupId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberResponses)
getGroupMembers_ByGroupId params _ByGroupId = handleError <$> getAt (map qp params <> map qp [ByGroupId _ByGroupId]) ["group_members"]

getGroupMembers_ByGroupId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberResponses)
getGroupMembers_ByGroupId' _ByGroupId = handleError <$> getAt [ByGroupId _ByGroupId] ["group_members"]

postGroupMember_ByGlobalGroupId :: forall qp. QueryParam qp => [qp] -> Int64 -> GroupMemberRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberResponse)
postGroupMember_ByGlobalGroupId params _ByGlobalGroupId group_member_request = handleError <$> postAt (map qp params <> map qp [ByGlobalGroupId _ByGlobalGroupId]) ["group_member"] group_member_request

postGroupMember_ByGlobalGroupId' :: Int64 -> GroupMemberRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberResponse)
postGroupMember_ByGlobalGroupId' _ByGlobalGroupId group_member_request = handleError <$> postAt [ByGlobalGroupId _ByGlobalGroupId] ["group_member"] group_member_request

getGroupMember :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberResponse)
getGroupMember params group_member_id = handleError <$> getAt params ["group_member", T.pack $ show group_member_id]

getGroupMember' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberResponse)
getGroupMember' group_member_id = handleError <$> getAt ([] :: [(Text, Text)]) ["group_member", T.pack $ show group_member_id]

putGroupMember :: forall qp. QueryParam qp => [qp] -> Int64 -> GroupMemberRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberResponse)
putGroupMember params group_member_id group_member_request = handleError <$> putAt params ["group_member", T.pack $ show group_member_id] group_member_request

putGroupMember' :: Int64 -> GroupMemberRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberResponse)
putGroupMember' group_member_id group_member_request = handleError <$> putAt ([] :: [(Text, Text)]) ["group_member", T.pack $ show group_member_id] group_member_request

deleteGroupMember :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteGroupMember params group_member_id = handleError <$> deleteAt params ["group_member", T.pack $ show group_member_id]

deleteGroupMember' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteGroupMember' group_member_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["group_member", T.pack $ show group_member_id]

getLeurons :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronResponses)
getLeurons params = handleError <$> getAt params ["leurons"]

getLeurons' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronResponses)
getLeurons'  = handleError <$> getAt ([] :: [(Text, Text)]) ["leurons"]

postLeuron_ByResourceId :: forall qp. QueryParam qp => [qp] -> Int64 -> LeuronRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronResponse)
postLeuron_ByResourceId params _ByResourceId leuron_request = handleError <$> postAt (map qp params <> map qp [ByResourceId _ByResourceId]) ["leuron"] leuron_request

postLeuron_ByResourceId' :: Int64 -> LeuronRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronResponse)
postLeuron_ByResourceId' _ByResourceId leuron_request = handleError <$> postAt [ByResourceId _ByResourceId] ["leuron"] leuron_request

getLeuron :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronResponse)
getLeuron params leuron_id = handleError <$> getAt params ["leuron", T.pack $ show leuron_id]

getLeuron' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronResponse)
getLeuron' leuron_id = handleError <$> getAt ([] :: [(Text, Text)]) ["leuron", T.pack $ show leuron_id]

putLeuron :: forall qp. QueryParam qp => [qp] -> Int64 -> LeuronRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronResponse)
putLeuron params leuron_id leuron_request = handleError <$> putAt params ["leuron", T.pack $ show leuron_id] leuron_request

putLeuron' :: Int64 -> LeuronRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronResponse)
putLeuron' leuron_id leuron_request = handleError <$> putAt ([] :: [(Text, Text)]) ["leuron", T.pack $ show leuron_id] leuron_request

deleteLeuron :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteLeuron params leuron_id = handleError <$> deleteAt params ["leuron", T.pack $ show leuron_id]

deleteLeuron' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteLeuron' leuron_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["leuron", T.pack $ show leuron_id]

getLikes :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponses)
getLikes params = handleError <$> getAt params ["likes"]

getLikes' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponses)
getLikes'  = handleError <$> getAt ([] :: [(Text, Text)]) ["likes"]

getLikes_ByThreadPostsIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponses)
getLikes_ByThreadPostsIds params _ByThreadPostsIds = handleError <$> getAt (map qp params <> map qp [ByThreadPostsIds _ByThreadPostsIds]) ["likes"]

getLikes_ByThreadPostsIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponses)
getLikes_ByThreadPostsIds' _ByThreadPostsIds = handleError <$> getAt [ByThreadPostsIds _ByThreadPostsIds] ["likes"]

getLikes_ByThreadPostId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponses)
getLikes_ByThreadPostId params _ByThreadPostId = handleError <$> getAt (map qp params <> map qp [ByThreadPostId _ByThreadPostId]) ["likes"]

getLikes_ByThreadPostId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponses)
getLikes_ByThreadPostId' _ByThreadPostId = handleError <$> getAt [ByThreadPostId _ByThreadPostId] ["likes"]

getLikes_ByResourceId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponses)
getLikes_ByResourceId params _ByResourceId = handleError <$> getAt (map qp params <> map qp [ByResourceId _ByResourceId]) ["likes"]

getLikes_ByResourceId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponses)
getLikes_ByResourceId' _ByResourceId = handleError <$> getAt [ByResourceId _ByResourceId] ["likes"]

postLike_ByThreadPostId :: forall qp. QueryParam qp => [qp] -> Int64 -> LikeRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
postLike_ByThreadPostId params _ByThreadPostId like_request = handleError <$> postAt (map qp params <> map qp [ByThreadPostId _ByThreadPostId]) ["like"] like_request

postLike_ByThreadPostId' :: Int64 -> LikeRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
postLike_ByThreadPostId' _ByThreadPostId like_request = handleError <$> postAt [ByThreadPostId _ByThreadPostId] ["like"] like_request

postLike_ByLeuronId :: forall qp. QueryParam qp => [qp] -> Int64 -> LikeRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
postLike_ByLeuronId params _ByLeuronId like_request = handleError <$> postAt (map qp params <> map qp [ByLeuronId _ByLeuronId]) ["like"] like_request

postLike_ByLeuronId' :: Int64 -> LikeRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
postLike_ByLeuronId' _ByLeuronId like_request = handleError <$> postAt [ByLeuronId _ByLeuronId] ["like"] like_request

getLike :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
getLike params like_id = handleError <$> getAt params ["like", T.pack $ show like_id]

getLike' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
getLike' like_id = handleError <$> getAt ([] :: [(Text, Text)]) ["like", T.pack $ show like_id]

putLike :: forall qp. QueryParam qp => [qp] -> Int64 -> LikeRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
putLike params like_id like_request = handleError <$> putAt params ["like", T.pack $ show like_id] like_request

putLike' :: Int64 -> LikeRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
putLike' like_id like_request = handleError <$> putAt ([] :: [(Text, Text)]) ["like", T.pack $ show like_id] like_request

deleteLike :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteLike params like_id = handleError <$> deleteAt params ["like", T.pack $ show like_id]

deleteLike' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteLike' like_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["like", T.pack $ show like_id]

getLikeStats_ByThreadPostsIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeStatResponses)
getLikeStats_ByThreadPostsIds params _ByThreadPostsIds = handleError <$> getAt (map qp params <> map qp [ByThreadPostsIds _ByThreadPostsIds]) ["like_stats"]

getLikeStats_ByThreadPostsIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeStatResponses)
getLikeStats_ByThreadPostsIds' _ByThreadPostsIds = handleError <$> getAt [ByThreadPostsIds _ByThreadPostsIds] ["like_stats"]

getLikeStat :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeStatResponse)
getLikeStat params like_id = handleError <$> getAt params ["like_stat", T.pack $ show like_id]

getLikeStat' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeStatResponse)
getLikeStat' like_id = handleError <$> getAt ([] :: [(Text, Text)]) ["like_stat", T.pack $ show like_id]

getStars :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars params = handleError <$> getAt params ["stars"]

getStars' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars'  = handleError <$> getAt ([] :: [(Text, Text)]) ["stars"]

getStars_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["stars"]

getStars_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["stars"]

getStars_ByUserId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars_ByUserId params _ByUserId = handleError <$> getAt (map qp params <> map qp [ByUserId _ByUserId]) ["stars"]

getStars_ByUserId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars_ByUserId' _ByUserId = handleError <$> getAt [ByUserId _ByUserId] ["stars"]

getStars_ByBoardId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars_ByBoardId params _ByBoardId = handleError <$> getAt (map qp params <> map qp [ByBoardId _ByBoardId]) ["stars"]

getStars_ByBoardId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars_ByBoardId' _ByBoardId = handleError <$> getAt [ByBoardId _ByBoardId] ["stars"]

getStars_ByThreadId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars_ByThreadId params _ByThreadId = handleError <$> getAt (map qp params <> map qp [ByThreadId _ByThreadId]) ["stars"]

getStars_ByThreadId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars_ByThreadId' _ByThreadId = handleError <$> getAt [ByThreadId _ByThreadId] ["stars"]

getStars_ByThreadPostsIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars_ByThreadPostsIds params _ByThreadPostsIds = handleError <$> getAt (map qp params <> map qp [ByThreadPostsIds _ByThreadPostsIds]) ["stars"]

getStars_ByThreadPostsIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars_ByThreadPostsIds' _ByThreadPostsIds = handleError <$> getAt [ByThreadPostsIds _ByThreadPostsIds] ["stars"]

getStars_ByThreadPostId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars_ByThreadPostId params _ByThreadPostId = handleError <$> getAt (map qp params <> map qp [ByThreadPostId _ByThreadPostId]) ["stars"]

getStars_ByThreadPostId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars_ByThreadPostId' _ByThreadPostId = handleError <$> getAt [ByThreadPostId _ByThreadPostId] ["stars"]

getStars_ByResourceId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars_ByResourceId params _ByResourceId = handleError <$> getAt (map qp params <> map qp [ByResourceId _ByResourceId]) ["stars"]

getStars_ByResourceId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars_ByResourceId' _ByResourceId = handleError <$> getAt [ByResourceId _ByResourceId] ["stars"]

getStars_ByLeuronId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars_ByLeuronId params _ByLeuronId = handleError <$> getAt (map qp params <> map qp [ByLeuronId _ByLeuronId]) ["stars"]

getStars_ByLeuronId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponses)
getStars_ByLeuronId' _ByLeuronId = handleError <$> getAt [ByLeuronId _ByLeuronId] ["stars"]

postStar_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> StarRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
postStar_ByOrganizationId params _ByOrganizationId star_request = handleError <$> postAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["star"] star_request

postStar_ByOrganizationId' :: Int64 -> StarRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
postStar_ByOrganizationId' _ByOrganizationId star_request = handleError <$> postAt [ByOrganizationId _ByOrganizationId] ["star"] star_request

postStar_ByUserId :: forall qp. QueryParam qp => [qp] -> Int64 -> StarRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
postStar_ByUserId params _ByUserId star_request = handleError <$> postAt (map qp params <> map qp [ByUserId _ByUserId]) ["star"] star_request

postStar_ByUserId' :: Int64 -> StarRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
postStar_ByUserId' _ByUserId star_request = handleError <$> postAt [ByUserId _ByUserId] ["star"] star_request

postStar_ByBoardId :: forall qp. QueryParam qp => [qp] -> Int64 -> StarRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
postStar_ByBoardId params _ByBoardId star_request = handleError <$> postAt (map qp params <> map qp [ByBoardId _ByBoardId]) ["star"] star_request

postStar_ByBoardId' :: Int64 -> StarRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
postStar_ByBoardId' _ByBoardId star_request = handleError <$> postAt [ByBoardId _ByBoardId] ["star"] star_request

postStar_ByThreadId :: forall qp. QueryParam qp => [qp] -> Int64 -> StarRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
postStar_ByThreadId params _ByThreadId star_request = handleError <$> postAt (map qp params <> map qp [ByThreadId _ByThreadId]) ["star"] star_request

postStar_ByThreadId' :: Int64 -> StarRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
postStar_ByThreadId' _ByThreadId star_request = handleError <$> postAt [ByThreadId _ByThreadId] ["star"] star_request

postStar_ByThreadPostId :: forall qp. QueryParam qp => [qp] -> Int64 -> StarRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
postStar_ByThreadPostId params _ByThreadPostId star_request = handleError <$> postAt (map qp params <> map qp [ByThreadPostId _ByThreadPostId]) ["star"] star_request

postStar_ByThreadPostId' :: Int64 -> StarRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
postStar_ByThreadPostId' _ByThreadPostId star_request = handleError <$> postAt [ByThreadPostId _ByThreadPostId] ["star"] star_request

postStar_ByResourceId :: forall qp. QueryParam qp => [qp] -> Int64 -> StarRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
postStar_ByResourceId params _ByResourceId star_request = handleError <$> postAt (map qp params <> map qp [ByResourceId _ByResourceId]) ["star"] star_request

postStar_ByResourceId' :: Int64 -> StarRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
postStar_ByResourceId' _ByResourceId star_request = handleError <$> postAt [ByResourceId _ByResourceId] ["star"] star_request

postStar_ByLeuronId :: forall qp. QueryParam qp => [qp] -> Int64 -> StarRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
postStar_ByLeuronId params _ByLeuronId star_request = handleError <$> postAt (map qp params <> map qp [ByLeuronId _ByLeuronId]) ["star"] star_request

postStar_ByLeuronId' :: Int64 -> StarRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
postStar_ByLeuronId' _ByLeuronId star_request = handleError <$> postAt [ByLeuronId _ByLeuronId] ["star"] star_request

getStar :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
getStar params star_id = handleError <$> getAt params ["star", T.pack $ show star_id]

getStar' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
getStar' star_id = handleError <$> getAt ([] :: [(Text, Text)]) ["star", T.pack $ show star_id]

putStar :: forall qp. QueryParam qp => [qp] -> Int64 -> StarRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
putStar params star_id star_request = handleError <$> putAt params ["star", T.pack $ show star_id] star_request

putStar' :: Int64 -> StarRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
putStar' star_id star_request = handleError <$> putAt ([] :: [(Text, Text)]) ["star", T.pack $ show star_id] star_request

deleteStar :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteStar params star_id = handleError <$> deleteAt params ["star", T.pack $ show star_id]

deleteStar' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteStar' star_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["star", T.pack $ show star_id]

getStarStats_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponses)
getStarStats_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["star_stats"]

getStarStats_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponses)
getStarStats_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["star_stats"]

getStarStats_ByUserId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponses)
getStarStats_ByUserId params _ByUserId = handleError <$> getAt (map qp params <> map qp [ByUserId _ByUserId]) ["star_stats"]

getStarStats_ByUserId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponses)
getStarStats_ByUserId' _ByUserId = handleError <$> getAt [ByUserId _ByUserId] ["star_stats"]

getStarStats_ByBoardId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponses)
getStarStats_ByBoardId params _ByBoardId = handleError <$> getAt (map qp params <> map qp [ByBoardId _ByBoardId]) ["star_stats"]

getStarStats_ByBoardId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponses)
getStarStats_ByBoardId' _ByBoardId = handleError <$> getAt [ByBoardId _ByBoardId] ["star_stats"]

getStarStats_ByThreadId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponses)
getStarStats_ByThreadId params _ByThreadId = handleError <$> getAt (map qp params <> map qp [ByThreadId _ByThreadId]) ["star_stats"]

getStarStats_ByThreadId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponses)
getStarStats_ByThreadId' _ByThreadId = handleError <$> getAt [ByThreadId _ByThreadId] ["star_stats"]

getStarStats_ByThreadPostsIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponses)
getStarStats_ByThreadPostsIds params _ByThreadPostsIds = handleError <$> getAt (map qp params <> map qp [ByThreadPostsIds _ByThreadPostsIds]) ["star_stats"]

getStarStats_ByThreadPostsIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponses)
getStarStats_ByThreadPostsIds' _ByThreadPostsIds = handleError <$> getAt [ByThreadPostsIds _ByThreadPostsIds] ["star_stats"]

getStarStats_ByThreadPostId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponses)
getStarStats_ByThreadPostId params _ByThreadPostId = handleError <$> getAt (map qp params <> map qp [ByThreadPostId _ByThreadPostId]) ["star_stats"]

getStarStats_ByThreadPostId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponses)
getStarStats_ByThreadPostId' _ByThreadPostId = handleError <$> getAt [ByThreadPostId _ByThreadPostId] ["star_stats"]

getStarStats_ByResourceId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponses)
getStarStats_ByResourceId params _ByResourceId = handleError <$> getAt (map qp params <> map qp [ByResourceId _ByResourceId]) ["star_stats"]

getStarStats_ByResourceId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponses)
getStarStats_ByResourceId' _ByResourceId = handleError <$> getAt [ByResourceId _ByResourceId] ["star_stats"]

getStarStats_ByLeuronId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponses)
getStarStats_ByLeuronId params _ByLeuronId = handleError <$> getAt (map qp params <> map qp [ByLeuronId _ByLeuronId]) ["star_stats"]

getStarStats_ByLeuronId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponses)
getStarStats_ByLeuronId' _ByLeuronId = handleError <$> getAt [ByLeuronId _ByLeuronId] ["star_stats"]

getStarStat :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponse)
getStarStat params star_id = handleError <$> getAt params ["star_stat", T.pack $ show star_id]

getStarStat' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarStatResponse)
getStarStat' star_id = handleError <$> getAt ([] :: [(Text, Text)]) ["star_stat", T.pack $ show star_id]

getMe :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
getMe params = handleError <$> getAt params ["me"]

getMe' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
getMe'  = handleError <$> getAt ([] :: [(Text, Text)]) ["me"]

getMePack :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserPackResponse)
getMePack params = handleError <$> getAt params ["me_pack"]

getMePack' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserPackResponse)
getMePack'  = handleError <$> getAt ([] :: [(Text, Text)]) ["me_pack"]

getOrganizations :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationResponses)
getOrganizations params = handleError <$> getAt params ["organizations"]

getOrganizations' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationResponses)
getOrganizations'  = handleError <$> getAt ([] :: [(Text, Text)]) ["organizations"]

postOrganization :: forall qp. QueryParam qp => [qp] -> OrganizationRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationResponse)
postOrganization params organization_request = handleError <$> postAt params ["organization"] organization_request

postOrganization' :: OrganizationRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationResponse)
postOrganization' organization_request = handleError <$> postAt ([] :: [(Text, Text)]) ["organization"] organization_request

getOrganization :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationResponse)
getOrganization params organization_id = handleError <$> getAt params ["organization", T.pack $ show organization_id]

getOrganization' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationResponse)
getOrganization' organization_id = handleError <$> getAt ([] :: [(Text, Text)]) ["organization", T.pack $ show organization_id]

putOrganization :: forall qp. QueryParam qp => [qp] -> Int64 -> OrganizationRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationResponse)
putOrganization params organization_id organization_request = handleError <$> putAt params ["organization", T.pack $ show organization_id] organization_request

putOrganization' :: Int64 -> OrganizationRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationResponse)
putOrganization' organization_id organization_request = handleError <$> putAt ([] :: [(Text, Text)]) ["organization", T.pack $ show organization_id] organization_request

deleteOrganization :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteOrganization params organization_id = handleError <$> deleteAt params ["organization", T.pack $ show organization_id]

deleteOrganization' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteOrganization' organization_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["organization", T.pack $ show organization_id]

getOrganizationStats :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationStatResponses)
getOrganizationStats params = handleError <$> getAt params ["organization_stats"]

getOrganizationStats' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationStatResponses)
getOrganizationStats'  = handleError <$> getAt ([] :: [(Text, Text)]) ["organization_stats"]

getOrganizationStat :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationStatResponse)
getOrganizationStat params organization_id = handleError <$> getAt params ["organization_stat", T.pack $ show organization_id]

getOrganizationStat' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationStatResponse)
getOrganizationStat' organization_id = handleError <$> getAt ([] :: [(Text, Text)]) ["organization_stat", T.pack $ show organization_id]

getPms :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmResponses)
getPms params = handleError <$> getAt params ["pms"]

getPms' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmResponses)
getPms'  = handleError <$> getAt ([] :: [(Text, Text)]) ["pms"]

postPm_ByUsersIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> PmRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmResponse)
postPm_ByUsersIds params _ByUsersIds pm_request = handleError <$> postAt (map qp params <> map qp [ByUsersIds _ByUsersIds]) ["pm"] pm_request

postPm_ByUsersIds' :: [Int64] -> PmRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmResponse)
postPm_ByUsersIds' _ByUsersIds pm_request = handleError <$> postAt [ByUsersIds _ByUsersIds] ["pm"] pm_request

postPm_ByUserId :: forall qp. QueryParam qp => [qp] -> Int64 -> PmRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmResponse)
postPm_ByUserId params _ByUserId pm_request = handleError <$> postAt (map qp params <> map qp [ByUserId _ByUserId]) ["pm"] pm_request

postPm_ByUserId' :: Int64 -> PmRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmResponse)
postPm_ByUserId' _ByUserId pm_request = handleError <$> postAt [ByUserId _ByUserId] ["pm"] pm_request

getPm :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmResponse)
getPm params pm_id = handleError <$> getAt params ["pm", T.pack $ show pm_id]

getPm' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmResponse)
getPm' pm_id = handleError <$> getAt ([] :: [(Text, Text)]) ["pm", T.pack $ show pm_id]

putPm :: forall qp. QueryParam qp => [qp] -> Int64 -> PmRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmResponse)
putPm params pm_id pm_request = handleError <$> putAt params ["pm", T.pack $ show pm_id] pm_request

putPm' :: Int64 -> PmRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmResponse)
putPm' pm_id pm_request = handleError <$> putAt ([] :: [(Text, Text)]) ["pm", T.pack $ show pm_id] pm_request

deletePm :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deletePm params pm_id = handleError <$> deleteAt params ["pm", T.pack $ show pm_id]

deletePm' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deletePm' pm_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["pm", T.pack $ show pm_id]

getPmIns :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmInResponses)
getPmIns params = handleError <$> getAt params ["pm_ins"]

getPmIns' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmInResponses)
getPmIns'  = handleError <$> getAt ([] :: [(Text, Text)]) ["pm_ins"]

getPmIns_ByUserId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmInResponses)
getPmIns_ByUserId params _ByUserId = handleError <$> getAt (map qp params <> map qp [ByUserId _ByUserId]) ["pm_ins"]

getPmIns_ByUserId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmInResponses)
getPmIns_ByUserId' _ByUserId = handleError <$> getAt [ByUserId _ByUserId] ["pm_ins"]

getPmIn :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmInResponse)
getPmIn params pm_in_id = handleError <$> getAt params ["pm_in", T.pack $ show pm_in_id]

getPmIn' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmInResponse)
getPmIn' pm_in_id = handleError <$> getAt ([] :: [(Text, Text)]) ["pm_in", T.pack $ show pm_in_id]

putPmIn :: forall qp. QueryParam qp => [qp] -> Int64 -> PmInRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmInResponse)
putPmIn params pm_in_id pm_in_request = handleError <$> putAt params ["pm_in", T.pack $ show pm_in_id] pm_in_request

putPmIn' :: Int64 -> PmInRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmInResponse)
putPmIn' pm_in_id pm_in_request = handleError <$> putAt ([] :: [(Text, Text)]) ["pm_in", T.pack $ show pm_in_id] pm_in_request

deletePmIn :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deletePmIn params pm_in_id = handleError <$> deleteAt params ["pm_in", T.pack $ show pm_in_id]

deletePmIn' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deletePmIn' pm_in_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["pm_in", T.pack $ show pm_in_id]

getPmOuts :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmOutResponses)
getPmOuts params = handleError <$> getAt params ["pm_outs"]

getPmOuts' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmOutResponses)
getPmOuts'  = handleError <$> getAt ([] :: [(Text, Text)]) ["pm_outs"]

getPmOuts_ByUserId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmOutResponses)
getPmOuts_ByUserId params _ByUserId = handleError <$> getAt (map qp params <> map qp [ByUserId _ByUserId]) ["pm_outs"]

getPmOuts_ByUserId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmOutResponses)
getPmOuts_ByUserId' _ByUserId = handleError <$> getAt [ByUserId _ByUserId] ["pm_outs"]

getPmOut :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmOutResponse)
getPmOut params pm_out_id = handleError <$> getAt params ["pm_out", T.pack $ show pm_out_id]

getPmOut' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmOutResponse)
getPmOut' pm_out_id = handleError <$> getAt ([] :: [(Text, Text)]) ["pm_out", T.pack $ show pm_out_id]

putPmOut :: forall qp. QueryParam qp => [qp] -> Int64 -> PmOutRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmOutResponse)
putPmOut params pm_out_id pm_out_request = handleError <$> putAt params ["pm_out", T.pack $ show pm_out_id] pm_out_request

putPmOut' :: Int64 -> PmOutRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmOutResponse)
putPmOut' pm_out_id pm_out_request = handleError <$> putAt ([] :: [(Text, Text)]) ["pm_out", T.pack $ show pm_out_id] pm_out_request

deletePmOut :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deletePmOut params pm_out_id = handleError <$> deleteAt params ["pm_out", T.pack $ show pm_out_id]

deletePmOut' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deletePmOut' pm_out_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["pm_out", T.pack $ show pm_out_id]

getResources :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourceResponses)
getResources params = handleError <$> getAt params ["resources"]

getResources' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourceResponses)
getResources'  = handleError <$> getAt ([] :: [(Text, Text)]) ["resources"]

postResource :: forall qp. QueryParam qp => [qp] -> ResourceRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourceResponse)
postResource params resource_request = handleError <$> postAt params ["resource"] resource_request

postResource' :: ResourceRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourceResponse)
postResource' resource_request = handleError <$> postAt ([] :: [(Text, Text)]) ["resource"] resource_request

getResource :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourceResponse)
getResource params resource_id = handleError <$> getAt params ["resource", T.pack $ show resource_id]

getResource' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourceResponse)
getResource' resource_id = handleError <$> getAt ([] :: [(Text, Text)]) ["resource", T.pack $ show resource_id]

putResource :: forall qp. QueryParam qp => [qp] -> Int64 -> ResourceRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourceResponse)
putResource params resource_id resource_request = handleError <$> putAt params ["resource", T.pack $ show resource_id] resource_request

putResource' :: Int64 -> ResourceRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourceResponse)
putResource' resource_id resource_request = handleError <$> putAt ([] :: [(Text, Text)]) ["resource", T.pack $ show resource_id] resource_request

deleteResource :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteResource params resource_id = handleError <$> deleteAt params ["resource", T.pack $ show resource_id]

deleteResource' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteResource' resource_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["resource", T.pack $ show resource_id]

getResourceStats_ByResourcesIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourceStatResponses)
getResourceStats_ByResourcesIds params _ByResourcesIds = handleError <$> getAt (map qp params <> map qp [ByResourcesIds _ByResourcesIds]) ["resource_stats"]

getResourceStats_ByResourcesIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourceStatResponses)
getResourceStats_ByResourcesIds' _ByResourcesIds = handleError <$> getAt [ByResourcesIds _ByResourcesIds] ["resource_stats"]

getResourceStat :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourceStatResponse)
getResourceStat params resource_id = handleError <$> getAt params ["resource_stat", T.pack $ show resource_id]

getResourceStat' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourceStatResponse)
getResourceStat' resource_id = handleError <$> getAt ([] :: [(Text, Text)]) ["resource_stat", T.pack $ show resource_id]

getTeams :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamResponses)
getTeams params = handleError <$> getAt params ["teams"]

getTeams' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamResponses)
getTeams'  = handleError <$> getAt ([] :: [(Text, Text)]) ["teams"]

getTeams_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamResponses)
getTeams_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["teams"]

getTeams_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamResponses)
getTeams_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["teams"]

postTeam_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> TeamRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamResponse)
postTeam_ByOrganizationId params _ByOrganizationId team_request = handleError <$> postAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["team"] team_request

postTeam_ByOrganizationId' :: Int64 -> TeamRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamResponse)
postTeam_ByOrganizationId' _ByOrganizationId team_request = handleError <$> postAt [ByOrganizationId _ByOrganizationId] ["team"] team_request

getTeam :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamResponse)
getTeam params team_id = handleError <$> getAt params ["team", T.pack $ show team_id]

getTeam' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamResponse)
getTeam' team_id = handleError <$> getAt ([] :: [(Text, Text)]) ["team", T.pack $ show team_id]

putTeam :: forall qp. QueryParam qp => [qp] -> Int64 -> TeamRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamResponse)
putTeam params team_id team_request = handleError <$> putAt params ["team", T.pack $ show team_id] team_request

putTeam' :: Int64 -> TeamRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamResponse)
putTeam' team_id team_request = handleError <$> putAt ([] :: [(Text, Text)]) ["team", T.pack $ show team_id] team_request

deleteTeam :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteTeam params team_id = handleError <$> deleteAt params ["team", T.pack $ show team_id]

deleteTeam' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteTeam' team_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["team", T.pack $ show team_id]

getTeamMembers :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberResponses)
getTeamMembers params = handleError <$> getAt params ["team_members"]

getTeamMembers' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberResponses)
getTeamMembers'  = handleError <$> getAt ([] :: [(Text, Text)]) ["team_members"]

getTeamMembers_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberResponses)
getTeamMembers_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["team_members"]

getTeamMembers_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberResponses)
getTeamMembers_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["team_members"]

getTeamMembers_ByTeamId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberResponses)
getTeamMembers_ByTeamId params _ByTeamId = handleError <$> getAt (map qp params <> map qp [ByTeamId _ByTeamId]) ["team_members"]

getTeamMembers_ByTeamId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberResponses)
getTeamMembers_ByTeamId' _ByTeamId = handleError <$> getAt [ByTeamId _ByTeamId] ["team_members"]

postTeamMember_ByTeamId :: forall qp. QueryParam qp => [qp] -> Int64 -> TeamMemberRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberResponse)
postTeamMember_ByTeamId params _ByTeamId team_member_request = handleError <$> postAt (map qp params <> map qp [ByTeamId _ByTeamId]) ["team_member"] team_member_request

postTeamMember_ByTeamId' :: Int64 -> TeamMemberRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberResponse)
postTeamMember_ByTeamId' _ByTeamId team_member_request = handleError <$> postAt [ByTeamId _ByTeamId] ["team_member"] team_member_request

postTeamMember_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> TeamMemberRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberResponse)
postTeamMember_ByOrganizationId params _ByOrganizationId team_member_request = handleError <$> postAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["team_member"] team_member_request

postTeamMember_ByOrganizationId' :: Int64 -> TeamMemberRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberResponse)
postTeamMember_ByOrganizationId' _ByOrganizationId team_member_request = handleError <$> postAt [ByOrganizationId _ByOrganizationId] ["team_member"] team_member_request

getTeamMember :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberResponse)
getTeamMember params team_member_id = handleError <$> getAt params ["team_member", T.pack $ show team_member_id]

getTeamMember' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberResponse)
getTeamMember' team_member_id = handleError <$> getAt ([] :: [(Text, Text)]) ["team_member", T.pack $ show team_member_id]

putTeamMember :: forall qp. QueryParam qp => [qp] -> Int64 -> TeamMemberRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberResponse)
putTeamMember params team_member_id team_member_request = handleError <$> putAt params ["team_member", T.pack $ show team_member_id] team_member_request

putTeamMember' :: Int64 -> TeamMemberRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberResponse)
putTeamMember' team_member_id team_member_request = handleError <$> putAt ([] :: [(Text, Text)]) ["team_member", T.pack $ show team_member_id] team_member_request

deleteTeamMember :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteTeamMember params team_member_id = handleError <$> deleteAt params ["team_member", T.pack $ show team_member_id]

deleteTeamMember' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteTeamMember' team_member_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["team_member", T.pack $ show team_member_id]

getThreads :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponses)
getThreads params = handleError <$> getAt params ["threads"]

getThreads' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponses)
getThreads'  = handleError <$> getAt ([] :: [(Text, Text)]) ["threads"]

postThread_ByBoardId :: forall qp. QueryParam qp => [qp] -> Int64 -> ThreadRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponse)
postThread_ByBoardId params _ByBoardId thread_request = handleError <$> postAt (map qp params <> map qp [ByBoardId _ByBoardId]) ["thread"] thread_request

postThread_ByBoardId' :: Int64 -> ThreadRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponse)
postThread_ByBoardId' _ByBoardId thread_request = handleError <$> postAt [ByBoardId _ByBoardId] ["thread"] thread_request

getThread :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponse)
getThread params thread_id = handleError <$> getAt params ["thread", T.pack $ show thread_id]

getThread' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponse)
getThread' thread_id = handleError <$> getAt ([] :: [(Text, Text)]) ["thread", T.pack $ show thread_id]

putThread :: forall qp. QueryParam qp => [qp] -> Int64 -> ThreadRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponse)
putThread params thread_id thread_request = handleError <$> putAt params ["thread", T.pack $ show thread_id] thread_request

putThread' :: Int64 -> ThreadRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponse)
putThread' thread_id thread_request = handleError <$> putAt ([] :: [(Text, Text)]) ["thread", T.pack $ show thread_id] thread_request

deleteThread :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteThread params thread_id = handleError <$> deleteAt params ["thread", T.pack $ show thread_id]

deleteThread' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteThread' thread_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["thread", T.pack $ show thread_id]

getThreadStats :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadStatResponses)
getThreadStats params = handleError <$> getAt params ["thread_stats"]

getThreadStats' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadStatResponses)
getThreadStats'  = handleError <$> getAt ([] :: [(Text, Text)]) ["thread_stats"]

getThreadStat :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadStatResponse)
getThreadStat params thread_id = handleError <$> getAt params ["thread_stat", T.pack $ show thread_id]

getThreadStat' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadStatResponse)
getThreadStat' thread_id = handleError <$> getAt ([] :: [(Text, Text)]) ["thread_stat", T.pack $ show thread_id]

getThreadPosts :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponses)
getThreadPosts params = handleError <$> getAt params ["thread_posts"]

getThreadPosts' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponses)
getThreadPosts'  = handleError <$> getAt ([] :: [(Text, Text)]) ["thread_posts"]

getThreadPosts_ByThreadId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponses)
getThreadPosts_ByThreadId params _ByThreadId = handleError <$> getAt (map qp params <> map qp [ByThreadId _ByThreadId]) ["thread_posts"]

getThreadPosts_ByThreadId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponses)
getThreadPosts_ByThreadId' _ByThreadId = handleError <$> getAt [ByThreadId _ByThreadId] ["thread_posts"]

postThreadPost_ByThreadId :: forall qp. QueryParam qp => [qp] -> Int64 -> ThreadPostRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponse)
postThreadPost_ByThreadId params _ByThreadId thread_post_request = handleError <$> postAt (map qp params <> map qp [ByThreadId _ByThreadId]) ["thread_post"] thread_post_request

postThreadPost_ByThreadId' :: Int64 -> ThreadPostRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponse)
postThreadPost_ByThreadId' _ByThreadId thread_post_request = handleError <$> postAt [ByThreadId _ByThreadId] ["thread_post"] thread_post_request

getThreadPost :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponse)
getThreadPost params thread_post_id = handleError <$> getAt params ["thread_post", T.pack $ show thread_post_id]

getThreadPost' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponse)
getThreadPost' thread_post_id = handleError <$> getAt ([] :: [(Text, Text)]) ["thread_post", T.pack $ show thread_post_id]

putThreadPost :: forall qp. QueryParam qp => [qp] -> Int64 -> ThreadPostRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponse)
putThreadPost params thread_post_id thread_post_request = handleError <$> putAt params ["thread_post", T.pack $ show thread_post_id] thread_post_request

putThreadPost' :: Int64 -> ThreadPostRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponse)
putThreadPost' thread_post_id thread_post_request = handleError <$> putAt ([] :: [(Text, Text)]) ["thread_post", T.pack $ show thread_post_id] thread_post_request

deleteThreadPost :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteThreadPost params thread_post_id = handleError <$> deleteAt params ["thread_post", T.pack $ show thread_post_id]

deleteThreadPost' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteThreadPost' thread_post_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["thread_post", T.pack $ show thread_post_id]

getThreadPostStats_ByThreadPostsIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostStatResponses)
getThreadPostStats_ByThreadPostsIds params _ByThreadPostsIds = handleError <$> getAt (map qp params <> map qp [ByThreadPostsIds _ByThreadPostsIds]) ["thread_post_stats"]

getThreadPostStats_ByThreadPostsIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostStatResponses)
getThreadPostStats_ByThreadPostsIds' _ByThreadPostsIds = handleError <$> getAt [ByThreadPostsIds _ByThreadPostsIds] ["thread_post_stats"]

getThreadPostStat :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostStatResponse)
getThreadPostStat params thread_post_id = handleError <$> getAt params ["thread_post_stat", T.pack $ show thread_post_id]

getThreadPostStat' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostStatResponse)
getThreadPostStat' thread_post_id = handleError <$> getAt ([] :: [(Text, Text)]) ["thread_post_stat", T.pack $ show thread_post_id]

getUsers :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponses)
getUsers params = handleError <$> getAt params ["users"]

getUsers' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponses)
getUsers'  = handleError <$> getAt ([] :: [(Text, Text)]) ["users"]

getUsers_ByUsersIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponses)
getUsers_ByUsersIds params _ByUsersIds = handleError <$> getAt (map qp params <> map qp [ByUsersIds _ByUsersIds]) ["users"]

getUsers_ByUsersIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponses)
getUsers_ByUsersIds' _ByUsersIds = handleError <$> getAt [ByUsersIds _ByUsersIds] ["users"]

getUsers_ByUsersNames :: forall qp. QueryParam qp => [qp] -> [Text] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponses)
getUsers_ByUsersNames params _ByUsersNames = handleError <$> getAt (map qp params <> map qp [ByUsersNames _ByUsersNames]) ["users"]

getUsers_ByUsersNames' :: [Text] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponses)
getUsers_ByUsersNames' _ByUsersNames = handleError <$> getAt [ByUsersNames _ByUsersNames] ["users"]

postUser :: forall qp. QueryParam qp => [qp] -> UserRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
postUser params user_request = handleError <$> postAt params ["user"] user_request

postUser' :: UserRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
postUser' user_request = handleError <$> postAt ([] :: [(Text, Text)]) ["user"] user_request

getUser :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
getUser params user_id = handleError <$> getAt params ["user", T.pack $ show user_id]

getUser' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
getUser' user_id = handleError <$> getAt ([] :: [(Text, Text)]) ["user", T.pack $ show user_id]

putUser :: forall qp. QueryParam qp => [qp] -> Int64 -> UserRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
putUser params user_id user_request = handleError <$> putAt params ["user", T.pack $ show user_id] user_request

putUser' :: Int64 -> UserRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
putUser' user_id user_request = handleError <$> putAt ([] :: [(Text, Text)]) ["user", T.pack $ show user_id] user_request

deleteUser :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteUser params user_id = handleError <$> deleteAt params ["user", T.pack $ show user_id]

deleteUser' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteUser' user_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["user", T.pack $ show user_id]

getUserProfiles :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ProfileResponses)
getUserProfiles params = handleError <$> getAt params ["user_profiles"]

getUserProfiles' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ProfileResponses)
getUserProfiles'  = handleError <$> getAt ([] :: [(Text, Text)]) ["user_profiles"]

getUserProfiles_ByUserId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ProfileResponses)
getUserProfiles_ByUserId params _ByUserId = handleError <$> getAt (map qp params <> map qp [ByUserId _ByUserId]) ["user_profiles"]

getUserProfiles_ByUserId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ProfileResponses)
getUserProfiles_ByUserId' _ByUserId = handleError <$> getAt [ByUserId _ByUserId] ["user_profiles"]

getUserProfiles_ByUsersIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ProfileResponses)
getUserProfiles_ByUsersIds params _ByUsersIds = handleError <$> getAt (map qp params <> map qp [ByUsersIds _ByUsersIds]) ["user_profiles"]

getUserProfiles_ByUsersIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ProfileResponses)
getUserProfiles_ByUsersIds' _ByUsersIds = handleError <$> getAt [ByUsersIds _ByUsersIds] ["user_profiles"]

getUserProfile :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ProfileResponse)
getUserProfile params profile_id = handleError <$> getAt params ["user_profile", T.pack $ show profile_id]

getUserProfile' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ProfileResponse)
getUserProfile' profile_id = handleError <$> getAt ([] :: [(Text, Text)]) ["user_profile", T.pack $ show profile_id]

putUserProfile :: forall qp. QueryParam qp => [qp] -> Int64 -> ProfileRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ProfileResponse)
putUserProfile params profile_id profile_request = handleError <$> putAt params ["user_profile", T.pack $ show profile_id] profile_request

putUserProfile' :: Int64 -> ProfileRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ProfileResponse)
putUserProfile' profile_id profile_request = handleError <$> putAt ([] :: [(Text, Text)]) ["user_profile", T.pack $ show profile_id] profile_request

deleteUserProfile :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteUserProfile params profile_id = handleError <$> deleteAt params ["user_profile", T.pack $ show profile_id]

deleteUserProfile' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteUserProfile' profile_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["user_profile", T.pack $ show profile_id]

getUsersSanitized :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedResponses)
getUsersSanitized params = handleError <$> getAt params ["users_sanitized"]

getUsersSanitized' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedResponses)
getUsersSanitized'  = handleError <$> getAt ([] :: [(Text, Text)]) ["users_sanitized"]

getUsersSanitized_ByUsersIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedResponses)
getUsersSanitized_ByUsersIds params _ByUsersIds = handleError <$> getAt (map qp params <> map qp [ByUsersIds _ByUsersIds]) ["users_sanitized"]

getUsersSanitized_ByUsersIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedResponses)
getUsersSanitized_ByUsersIds' _ByUsersIds = handleError <$> getAt [ByUsersIds _ByUsersIds] ["users_sanitized"]

getUsersSanitized_ByUsersNames :: forall qp. QueryParam qp => [qp] -> [Text] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedResponses)
getUsersSanitized_ByUsersNames params _ByUsersNames = handleError <$> getAt (map qp params <> map qp [ByUsersNames _ByUsersNames]) ["users_sanitized"]

getUsersSanitized_ByUsersNames' :: [Text] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedResponses)
getUsersSanitized_ByUsersNames' _ByUsersNames = handleError <$> getAt [ByUsersNames _ByUsersNames] ["users_sanitized"]

getUserSanitized :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedResponse)
getUserSanitized params user_id = handleError <$> getAt params ["user_sanitized", T.pack $ show user_id]

getUserSanitized' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedResponse)
getUserSanitized' user_id = handleError <$> getAt ([] :: [(Text, Text)]) ["user_sanitized", T.pack $ show user_id]

getUserSanitizedStats :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedStatResponse)
getUserSanitizedStats params = handleError <$> getAt params ["user_sanitized_stats"]

getUserSanitizedStats' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedStatResponse)
getUserSanitizedStats'  = handleError <$> getAt ([] :: [(Text, Text)]) ["user_sanitized_stats"]

getUserSanitizedStats_ByUsersIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedStatResponse)
getUserSanitizedStats_ByUsersIds params _ByUsersIds = handleError <$> getAt (map qp params <> map qp [ByUsersIds _ByUsersIds]) ["user_sanitized_stats"]

getUserSanitizedStats_ByUsersIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedStatResponse)
getUserSanitizedStats_ByUsersIds' _ByUsersIds = handleError <$> getAt [ByUsersIds _ByUsersIds] ["user_sanitized_stats"]

getUserSanitizedStat :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedStatResponse)
getUserSanitizedStat params user_id = handleError <$> getAt params ["user_sanitized_stat", T.pack $ show user_id]

getUserSanitizedStat' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedStatResponse)
getUserSanitizedStat' user_id = handleError <$> getAt ([] :: [(Text, Text)]) ["user_sanitized_stat", T.pack $ show user_id]

getOrganizationPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationPackResponses)
getOrganizationPacks params = handleError <$> getAt params ["organization_packs"]

getOrganizationPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationPackResponses)
getOrganizationPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["organization_packs"]

getOrganizationPacks_ByOrganizationsIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationPackResponses)
getOrganizationPacks_ByOrganizationsIds params _ByOrganizationsIds = handleError <$> getAt (map qp params <> map qp [ByOrganizationsIds _ByOrganizationsIds]) ["organization_packs"]

getOrganizationPacks_ByOrganizationsIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationPackResponses)
getOrganizationPacks_ByOrganizationsIds' _ByOrganizationsIds = handleError <$> getAt [ByOrganizationsIds _ByOrganizationsIds] ["organization_packs"]

getOrganizationPack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationPackResponse)
getOrganizationPack params organization_id = handleError <$> getAt params ["organization_pack", T.pack $ show organization_id]

getOrganizationPack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) OrganizationPackResponse)
getOrganizationPack' organization_id = handleError <$> getAt ([] :: [(Text, Text)]) ["organization_pack", T.pack $ show organization_id]

getTeamPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamPackResponses)
getTeamPacks params = handleError <$> getAt params ["team_packs"]

getTeamPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamPackResponses)
getTeamPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["team_packs"]

getTeamPacks_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamPackResponses)
getTeamPacks_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["team_packs"]

getTeamPacks_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamPackResponses)
getTeamPacks_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["team_packs"]

getTeamPack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamPackResponse)
getTeamPack params team_id = handleError <$> getAt params ["team_pack", T.pack $ show team_id]

getTeamPack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamPackResponse)
getTeamPack' team_id = handleError <$> getAt ([] :: [(Text, Text)]) ["team_pack", T.pack $ show team_id]

getTeamMemberPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberPackResponses)
getTeamMemberPacks params = handleError <$> getAt params ["team_member_packs"]

getTeamMemberPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberPackResponses)
getTeamMemberPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["team_member_packs"]

getTeamMemberPacks_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberPackResponses)
getTeamMemberPacks_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["team_member_packs"]

getTeamMemberPacks_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberPackResponses)
getTeamMemberPacks_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["team_member_packs"]

getTeamMemberPacks_ByTeamId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberPackResponses)
getTeamMemberPacks_ByTeamId params _ByTeamId = handleError <$> getAt (map qp params <> map qp [ByTeamId _ByTeamId]) ["team_member_packs"]

getTeamMemberPacks_ByTeamId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberPackResponses)
getTeamMemberPacks_ByTeamId' _ByTeamId = handleError <$> getAt [ByTeamId _ByTeamId] ["team_member_packs"]

getTeamMemberPack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberPackResponse)
getTeamMemberPack params team_member_id = handleError <$> getAt params ["team_member_pack", T.pack $ show team_member_id]

getTeamMemberPack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) TeamMemberPackResponse)
getTeamMemberPack' team_member_id = handleError <$> getAt ([] :: [(Text, Text)]) ["team_member_pack", T.pack $ show team_member_id]

getUserPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserPackResponses)
getUserPacks params = handleError <$> getAt params ["user_packs"]

getUserPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserPackResponses)
getUserPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["user_packs"]

getUserPacks_ByUsersIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserPackResponses)
getUserPacks_ByUsersIds params _ByUsersIds = handleError <$> getAt (map qp params <> map qp [ByUsersIds _ByUsersIds]) ["user_packs"]

getUserPacks_ByUsersIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserPackResponses)
getUserPacks_ByUsersIds' _ByUsersIds = handleError <$> getAt [ByUsersIds _ByUsersIds] ["user_packs"]

getUserPack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserPackResponse)
getUserPack params user_id = handleError <$> getAt params ["user_pack", T.pack $ show user_id]

getUserPack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserPackResponse)
getUserPack' user_id = handleError <$> getAt ([] :: [(Text, Text)]) ["user_pack", T.pack $ show user_id]

getUserSanitizedPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedPackResponses)
getUserSanitizedPacks params = handleError <$> getAt params ["user_sanitized_packs"]

getUserSanitizedPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedPackResponses)
getUserSanitizedPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["user_sanitized_packs"]

getUserSanitizedPacks_ByUsersIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedPackResponses)
getUserSanitizedPacks_ByUsersIds params _ByUsersIds = handleError <$> getAt (map qp params <> map qp [ByUsersIds _ByUsersIds]) ["user_sanitized_packs"]

getUserSanitizedPacks_ByUsersIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedPackResponses)
getUserSanitizedPacks_ByUsersIds' _ByUsersIds = handleError <$> getAt [ByUsersIds _ByUsersIds] ["user_sanitized_packs"]

getUserSanitizedPack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedPackResponse)
getUserSanitizedPack params user_id = handleError <$> getAt params ["user_sanitized_pack", T.pack $ show user_id]

getUserSanitizedPack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserSanitizedPackResponse)
getUserSanitizedPack' user_id = handleError <$> getAt ([] :: [(Text, Text)]) ["user_sanitized_pack", T.pack $ show user_id]

getGlobalGroupPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupPackResponses)
getGlobalGroupPacks params = handleError <$> getAt params ["global_group_packs"]

getGlobalGroupPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupPackResponses)
getGlobalGroupPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["global_group_packs"]

getGlobalGroupPacks_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupPackResponses)
getGlobalGroupPacks_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["global_group_packs"]

getGlobalGroupPacks_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupPackResponses)
getGlobalGroupPacks_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["global_group_packs"]

getGlobalGroupPack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupPackResponse)
getGlobalGroupPack params global_group_id = handleError <$> getAt params ["global_group_pack", T.pack $ show global_group_id]

getGlobalGroupPack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GlobalGroupPackResponse)
getGlobalGroupPack' global_group_id = handleError <$> getAt ([] :: [(Text, Text)]) ["global_group_pack", T.pack $ show global_group_id]

getGroupPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupPackResponses)
getGroupPacks params = handleError <$> getAt params ["group_packs"]

getGroupPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupPackResponses)
getGroupPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["group_packs"]

getGroupPacks_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupPackResponses)
getGroupPacks_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["group_packs"]

getGroupPacks_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupPackResponses)
getGroupPacks_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["group_packs"]

getGroupPack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupPackResponse)
getGroupPack params group_id = handleError <$> getAt params ["group_pack", T.pack $ show group_id]

getGroupPack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupPackResponse)
getGroupPack' group_id = handleError <$> getAt ([] :: [(Text, Text)]) ["group_pack", T.pack $ show group_id]

getGroupMemberPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberPackResponses)
getGroupMemberPacks params = handleError <$> getAt params ["group_member_packs"]

getGroupMemberPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberPackResponses)
getGroupMemberPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["group_member_packs"]

getGroupMemberPacks_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberPackResponses)
getGroupMemberPacks_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["group_member_packs"]

getGroupMemberPacks_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberPackResponses)
getGroupMemberPacks_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["group_member_packs"]

getGroupMemberPacks_ByGlobalGroupId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberPackResponses)
getGroupMemberPacks_ByGlobalGroupId params _ByGlobalGroupId = handleError <$> getAt (map qp params <> map qp [ByGlobalGroupId _ByGlobalGroupId]) ["group_member_packs"]

getGroupMemberPacks_ByGlobalGroupId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberPackResponses)
getGroupMemberPacks_ByGlobalGroupId' _ByGlobalGroupId = handleError <$> getAt [ByGlobalGroupId _ByGlobalGroupId] ["group_member_packs"]

getGroupMemberPacks_ByGroupId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberPackResponses)
getGroupMemberPacks_ByGroupId params _ByGroupId = handleError <$> getAt (map qp params <> map qp [ByGroupId _ByGroupId]) ["group_member_packs"]

getGroupMemberPacks_ByGroupId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberPackResponses)
getGroupMemberPacks_ByGroupId' _ByGroupId = handleError <$> getAt [ByGroupId _ByGroupId] ["group_member_packs"]

getGroupMemberPack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberPackResponse)
getGroupMemberPack params group_member_id = handleError <$> getAt params ["group_member_pack", T.pack $ show group_member_id]

getGroupMemberPack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) GroupMemberPackResponse)
getGroupMemberPack' group_member_id = handleError <$> getAt ([] :: [(Text, Text)]) ["group_member_pack", T.pack $ show group_member_id]

getForumPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponses)
getForumPacks params = handleError <$> getAt params ["forum_packs"]

getForumPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponses)
getForumPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["forum_packs"]

getForumPacks_ByForumId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponses)
getForumPacks_ByForumId params _ByForumId = handleError <$> getAt (map qp params <> map qp [ByForumId _ByForumId]) ["forum_packs"]

getForumPacks_ByForumId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponses)
getForumPacks_ByForumId' _ByForumId = handleError <$> getAt [ByForumId _ByForumId] ["forum_packs"]

getForumPacks_ByForumsIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponses)
getForumPacks_ByForumsIds params _ByForumsIds = handleError <$> getAt (map qp params <> map qp [ByForumsIds _ByForumsIds]) ["forum_packs"]

getForumPacks_ByForumsIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponses)
getForumPacks_ByForumsIds' _ByForumsIds = handleError <$> getAt [ByForumsIds _ByForumsIds] ["forum_packs"]

getForumPacks_ByOrganizationId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponses)
getForumPacks_ByOrganizationId params _ByOrganizationId = handleError <$> getAt (map qp params <> map qp [ByOrganizationId _ByOrganizationId]) ["forum_packs"]

getForumPacks_ByOrganizationId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponses)
getForumPacks_ByOrganizationId' _ByOrganizationId = handleError <$> getAt [ByOrganizationId _ByOrganizationId] ["forum_packs"]

getForumPacks_ByOrganizationName :: forall qp. QueryParam qp => [qp] -> Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponses)
getForumPacks_ByOrganizationName params _ByOrganizationName = handleError <$> getAt (map qp params <> map qp [ByOrganizationName _ByOrganizationName]) ["forum_packs"]

getForumPacks_ByOrganizationName' :: Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponses)
getForumPacks_ByOrganizationName' _ByOrganizationName = handleError <$> getAt [ByOrganizationName _ByOrganizationName] ["forum_packs"]

getForumPack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponse)
getForumPack params forum_id = handleError <$> getAt params ["forum_pack", T.pack $ show forum_id]

getForumPack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponse)
getForumPack' forum_id = handleError <$> getAt ([] :: [(Text, Text)]) ["forum_pack", T.pack $ show forum_id]

getBoardPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponses)
getBoardPacks params = handleError <$> getAt params ["board_packs"]

getBoardPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponses)
getBoardPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["board_packs"]

getBoardPacks_ByForumId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponses)
getBoardPacks_ByForumId params _ByForumId = handleError <$> getAt (map qp params <> map qp [ByForumId _ByForumId]) ["board_packs"]

getBoardPacks_ByForumId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponses)
getBoardPacks_ByForumId' _ByForumId = handleError <$> getAt [ByForumId _ByForumId] ["board_packs"]

getBoardPacks_ByBoardsIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponses)
getBoardPacks_ByBoardsIds params _ByBoardsIds = handleError <$> getAt (map qp params <> map qp [ByBoardsIds _ByBoardsIds]) ["board_packs"]

getBoardPacks_ByBoardsIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponses)
getBoardPacks_ByBoardsIds' _ByBoardsIds = handleError <$> getAt [ByBoardsIds _ByBoardsIds] ["board_packs"]

getBoardPacks_ByBoardId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponses)
getBoardPacks_ByBoardId params _ByBoardId = handleError <$> getAt (map qp params <> map qp [ByBoardId _ByBoardId]) ["board_packs"]

getBoardPacks_ByBoardId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponses)
getBoardPacks_ByBoardId' _ByBoardId = handleError <$> getAt [ByBoardId _ByBoardId] ["board_packs"]

getBoardPack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponse)
getBoardPack params board_id = handleError <$> getAt params ["board_pack", T.pack $ show board_id]

getBoardPack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponse)
getBoardPack' board_id = handleError <$> getAt ([] :: [(Text, Text)]) ["board_pack", T.pack $ show board_id]

getThreadPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPackResponses)
getThreadPacks params = handleError <$> getAt params ["thread_packs"]

getThreadPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPackResponses)
getThreadPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["thread_packs"]

getThreadPacks_ByThreadsIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPackResponses)
getThreadPacks_ByThreadsIds params _ByThreadsIds = handleError <$> getAt (map qp params <> map qp [ByThreadsIds _ByThreadsIds]) ["thread_packs"]

getThreadPacks_ByThreadsIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPackResponses)
getThreadPacks_ByThreadsIds' _ByThreadsIds = handleError <$> getAt [ByThreadsIds _ByThreadsIds] ["thread_packs"]

getThreadPacks_ByForumId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPackResponses)
getThreadPacks_ByForumId params _ByForumId = handleError <$> getAt (map qp params <> map qp [ByForumId _ByForumId]) ["thread_packs"]

getThreadPacks_ByForumId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPackResponses)
getThreadPacks_ByForumId' _ByForumId = handleError <$> getAt [ByForumId _ByForumId] ["thread_packs"]

getThreadPacks_ByBoardId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPackResponses)
getThreadPacks_ByBoardId params _ByBoardId = handleError <$> getAt (map qp params <> map qp [ByBoardId _ByBoardId]) ["thread_packs"]

getThreadPacks_ByBoardId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPackResponses)
getThreadPacks_ByBoardId' _ByBoardId = handleError <$> getAt [ByBoardId _ByBoardId] ["thread_packs"]

getThreadPack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPackResponse)
getThreadPack params thread_id = handleError <$> getAt params ["thread_pack", T.pack $ show thread_id]

getThreadPack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPackResponse)
getThreadPack' thread_id = handleError <$> getAt ([] :: [(Text, Text)]) ["thread_pack", T.pack $ show thread_id]

getThreadPostPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostPackResponses)
getThreadPostPacks params = handleError <$> getAt params ["thread_post_packs"]

getThreadPostPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostPackResponses)
getThreadPostPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["thread_post_packs"]

getThreadPostPacks_ByThreadPostsIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostPackResponses)
getThreadPostPacks_ByThreadPostsIds params _ByThreadPostsIds = handleError <$> getAt (map qp params <> map qp [ByThreadPostsIds _ByThreadPostsIds]) ["thread_post_packs"]

getThreadPostPacks_ByThreadPostsIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostPackResponses)
getThreadPostPacks_ByThreadPostsIds' _ByThreadPostsIds = handleError <$> getAt [ByThreadPostsIds _ByThreadPostsIds] ["thread_post_packs"]

getThreadPostPacks_ByThreadId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostPackResponses)
getThreadPostPacks_ByThreadId params _ByThreadId = handleError <$> getAt (map qp params <> map qp [ByThreadId _ByThreadId]) ["thread_post_packs"]

getThreadPostPacks_ByThreadId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostPackResponses)
getThreadPostPacks_ByThreadId' _ByThreadId = handleError <$> getAt [ByThreadId _ByThreadId] ["thread_post_packs"]

getThreadPostPacks_ByForumId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostPackResponses)
getThreadPostPacks_ByForumId params _ByForumId = handleError <$> getAt (map qp params <> map qp [ByForumId _ByForumId]) ["thread_post_packs"]

getThreadPostPacks_ByForumId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostPackResponses)
getThreadPostPacks_ByForumId' _ByForumId = handleError <$> getAt [ByForumId _ByForumId] ["thread_post_packs"]

getThreadPostPacks_ByBoardId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostPackResponses)
getThreadPostPacks_ByBoardId params _ByBoardId = handleError <$> getAt (map qp params <> map qp [ByBoardId _ByBoardId]) ["thread_post_packs"]

getThreadPostPacks_ByBoardId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostPackResponses)
getThreadPostPacks_ByBoardId' _ByBoardId = handleError <$> getAt [ByBoardId _ByBoardId] ["thread_post_packs"]

getThreadPostPack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostPackResponse)
getThreadPostPack params thread_post_id = handleError <$> getAt params ["thread_post_pack", T.pack $ show thread_post_id]

getThreadPostPack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostPackResponse)
getThreadPostPack' thread_post_id = handleError <$> getAt ([] :: [(Text, Text)]) ["thread_post_pack", T.pack $ show thread_post_id]

getResourcePacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourcePackResponses)
getResourcePacks params = handleError <$> getAt params ["resource_packs"]

getResourcePacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourcePackResponses)
getResourcePacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["resource_packs"]

getResourcePacks_ByResourcesIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourcePackResponses)
getResourcePacks_ByResourcesIds params _ByResourcesIds = handleError <$> getAt (map qp params <> map qp [ByResourcesIds _ByResourcesIds]) ["resource_packs"]

getResourcePacks_ByResourcesIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourcePackResponses)
getResourcePacks_ByResourcesIds' _ByResourcesIds = handleError <$> getAt [ByResourcesIds _ByResourcesIds] ["resource_packs"]

getResourcePack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourcePackResponse)
getResourcePack params resource_id = handleError <$> getAt params ["resource_pack", T.pack $ show resource_id]

getResourcePack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ResourcePackResponse)
getResourcePack' resource_id = handleError <$> getAt ([] :: [(Text, Text)]) ["resource_pack", T.pack $ show resource_id]

getLeuronPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronPackResponses)
getLeuronPacks params = handleError <$> getAt params ["leuron_packs"]

getLeuronPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronPackResponses)
getLeuronPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["leuron_packs"]

getLeuronPacks_ByLeuronsIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronPackResponses)
getLeuronPacks_ByLeuronsIds params _ByLeuronsIds = handleError <$> getAt (map qp params <> map qp [ByLeuronsIds _ByLeuronsIds]) ["leuron_packs"]

getLeuronPacks_ByLeuronsIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronPackResponses)
getLeuronPacks_ByLeuronsIds' _ByLeuronsIds = handleError <$> getAt [ByLeuronsIds _ByLeuronsIds] ["leuron_packs"]

getLeuronPacks_ByResourceId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronPackResponses)
getLeuronPacks_ByResourceId params _ByResourceId = handleError <$> getAt (map qp params <> map qp [ByResourceId _ByResourceId]) ["leuron_packs"]

getLeuronPacks_ByResourceId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronPackResponses)
getLeuronPacks_ByResourceId' _ByResourceId = handleError <$> getAt [ByResourceId _ByResourceId] ["leuron_packs"]

getLeuronPack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronPackResponse)
getLeuronPack params leuron_id = handleError <$> getAt params ["leuron_pack", T.pack $ show leuron_id]

getLeuronPack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronPackResponse)
getLeuronPack' leuron_id = handleError <$> getAt ([] :: [(Text, Text)]) ["leuron_pack", T.pack $ show leuron_id]

getPmInPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronPackResponses)
getPmInPacks params = handleError <$> getAt params ["pm_in_packs"]

getPmInPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronPackResponses)
getPmInPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["pm_in_packs"]

getPmInPack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmInPackResponse)
getPmInPack params pm_in_id = handleError <$> getAt params ["pm_in_pack", T.pack $ show pm_in_id]

getPmInPack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmInPackResponse)
getPmInPack' pm_in_id = handleError <$> getAt ([] :: [(Text, Text)]) ["pm_in_pack", T.pack $ show pm_in_id]

getPmOutPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronPackResponses)
getPmOutPacks params = handleError <$> getAt params ["pm_out_packs"]

getPmOutPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LeuronPackResponses)
getPmOutPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["pm_out_packs"]

getPmOutPack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmOutPackResponse)
getPmOutPack params pm_out_id = handleError <$> getAt params ["pm_out_pack", T.pack $ show pm_out_id]

getPmOutPack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) PmOutPackResponse)
getPmOutPack' pm_out_id = handleError <$> getAt ([] :: [(Text, Text)]) ["pm_out_pack", T.pack $ show pm_out_id]

-- footer