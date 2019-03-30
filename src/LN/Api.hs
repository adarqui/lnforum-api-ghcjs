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
import Prelude


import LN.T

getBoardsCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getBoardsCount params = handleError <$> getAt params ["boards_count"]

getBoardsCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getBoardsCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["boards_count"]

getThreadsCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadsCount params = handleError <$> getAt params ["threads_count"]

getThreadsCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadsCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["threads_count"]

getThreadPostsCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadPostsCount params = handleError <$> getAt params ["thread_posts_count"]

getThreadPostsCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadPostsCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["thread_posts_count"]

getLikesCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getLikesCount params = handleError <$> getAt params ["likes_count"]

getLikesCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getLikesCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["likes_count"]

getUsersCount :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getUsersCount params = handleError <$> getAt params ["users_count"]

getUsersCount' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getUsersCount'  = handleError <$> getAt ([] :: [(Text, Text)]) ["users_count"]

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

getMe :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
getMe params = handleError <$> getAt params ["me"]

getMe' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
getMe'  = handleError <$> getAt ([] :: [(Text, Text)]) ["me"]

getMePack :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserPackResponse)
getMePack params = handleError <$> getAt params ["me_pack"]

getMePack' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserPackResponse)
getMePack'  = handleError <$> getAt ([] :: [(Text, Text)]) ["me_pack"]

postForum :: forall qp. QueryParam qp => [qp] -> ForumRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponse)
postForum params forum_request = handleError <$> postAt params ["forums"] forum_request

postForum' :: ForumRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponse)
postForum' forum_request = handleError <$> postAt ([] :: [(Text, Text)]) ["forums"] forum_request

getForum :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponse)
getForum params forum_id = handleError <$> getAt params ["forums", T.pack $ show forum_id]

getForum' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponse)
getForum' forum_id = handleError <$> getAt ([] :: [(Text, Text)]) ["forums", T.pack $ show forum_id]

putForum :: forall qp. QueryParam qp => [qp] -> Int64 -> ForumRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponse)
putForum params forum_id forum_request = handleError <$> putAt params ["forums", T.pack $ show forum_id] forum_request

putForum' :: Int64 -> ForumRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumResponse)
putForum' forum_id forum_request = handleError <$> putAt ([] :: [(Text, Text)]) ["forums", T.pack $ show forum_id] forum_request

getForumStat :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumStatResponse)
getForumStat params forum_id = handleError <$> getAt params ["forum_stat", T.pack $ show forum_id]

getForumStat' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumStatResponse)
getForumStat' forum_id = handleError <$> getAt ([] :: [(Text, Text)]) ["forum_stat", T.pack $ show forum_id]

getBoards :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponses)
getBoards params = handleError <$> getAt params ["boards"]

getBoards' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponses)
getBoards'  = handleError <$> getAt ([] :: [(Text, Text)]) ["boards"]

postBoard :: forall qp. QueryParam qp => [qp] -> BoardRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
postBoard params board_request = handleError <$> postAt params ["boards"] board_request

postBoard' :: BoardRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
postBoard' board_request = handleError <$> postAt ([] :: [(Text, Text)]) ["boards"] board_request

postBoard_ByForumId :: forall qp. QueryParam qp => [qp] -> Int64 -> BoardRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
postBoard_ByForumId params _ByForumId board_request = handleError <$> postAt (map qp params <> map qp [ByForumId _ByForumId]) ["boards"] board_request

postBoard_ByForumId' :: Int64 -> BoardRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
postBoard_ByForumId' _ByForumId board_request = handleError <$> postAt [ByForumId _ByForumId] ["boards"] board_request

postBoard_ByBoardId :: forall qp. QueryParam qp => [qp] -> Int64 -> BoardRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
postBoard_ByBoardId params _ByBoardId board_request = handleError <$> postAt (map qp params <> map qp [ByBoardId _ByBoardId]) ["boards"] board_request

postBoard_ByBoardId' :: Int64 -> BoardRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
postBoard_ByBoardId' _ByBoardId board_request = handleError <$> postAt [ByBoardId _ByBoardId] ["boards"] board_request

getBoard :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
getBoard params board_id = handleError <$> getAt params ["boards", T.pack $ show board_id]

getBoard' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
getBoard' board_id = handleError <$> getAt ([] :: [(Text, Text)]) ["boards", T.pack $ show board_id]

putBoard :: forall qp. QueryParam qp => [qp] -> Int64 -> BoardRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
putBoard params board_id board_request = handleError <$> putAt params ["boards", T.pack $ show board_id] board_request

putBoard' :: Int64 -> BoardRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardResponse)
putBoard' board_id board_request = handleError <$> putAt ([] :: [(Text, Text)]) ["boards", T.pack $ show board_id] board_request

deleteBoard :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteBoard params board_id = handleError <$> deleteAt params ["boards", T.pack $ show board_id]

deleteBoard' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteBoard' board_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["boards", T.pack $ show board_id]

getBoardStats_ByBoardsIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardStatResponses)
getBoardStats_ByBoardsIds params _ByBoardsIds = handleError <$> getAt (map qp params <> map qp [ByBoardsIds _ByBoardsIds]) ["board_stats"]

getBoardStats_ByBoardsIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardStatResponses)
getBoardStats_ByBoardsIds' _ByBoardsIds = handleError <$> getAt [ByBoardsIds _ByBoardsIds] ["board_stats"]

getBoardStat :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardStatResponse)
getBoardStat params board_id = handleError <$> getAt params ["board_stat", T.pack $ show board_id]

getBoardStat' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardStatResponse)
getBoardStat' board_id = handleError <$> getAt ([] :: [(Text, Text)]) ["board_stat", T.pack $ show board_id]

getThreads :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponses)
getThreads params = handleError <$> getAt params ["threads"]

getThreads' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponses)
getThreads'  = handleError <$> getAt ([] :: [(Text, Text)]) ["threads"]

postThread :: forall qp. QueryParam qp => [qp] -> ThreadRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponse)
postThread params thread_request = handleError <$> postAt params ["threads"] thread_request

postThread' :: ThreadRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponse)
postThread' thread_request = handleError <$> postAt ([] :: [(Text, Text)]) ["threads"] thread_request

postThread_ByBoardId :: forall qp. QueryParam qp => [qp] -> Int64 -> ThreadRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponse)
postThread_ByBoardId params _ByBoardId thread_request = handleError <$> postAt (map qp params <> map qp [ByBoardId _ByBoardId]) ["threads"] thread_request

postThread_ByBoardId' :: Int64 -> ThreadRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponse)
postThread_ByBoardId' _ByBoardId thread_request = handleError <$> postAt [ByBoardId _ByBoardId] ["threads"] thread_request

getThread :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponse)
getThread params thread_id = handleError <$> getAt params ["threads", T.pack $ show thread_id]

getThread' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponse)
getThread' thread_id = handleError <$> getAt ([] :: [(Text, Text)]) ["threads", T.pack $ show thread_id]

putThread :: forall qp. QueryParam qp => [qp] -> Int64 -> ThreadRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponse)
putThread params thread_id thread_request = handleError <$> putAt params ["threads", T.pack $ show thread_id] thread_request

putThread' :: Int64 -> ThreadRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadResponse)
putThread' thread_id thread_request = handleError <$> putAt ([] :: [(Text, Text)]) ["threads", T.pack $ show thread_id] thread_request

deleteThread :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteThread params thread_id = handleError <$> deleteAt params ["threads", T.pack $ show thread_id]

deleteThread' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteThread' thread_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["threads", T.pack $ show thread_id]

getThreadStats_ByThreadsIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadStatResponses)
getThreadStats_ByThreadsIds params _ByThreadsIds = handleError <$> getAt (map qp params <> map qp [ByThreadsIds _ByThreadsIds]) ["thread_stats"]

getThreadStats_ByThreadsIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadStatResponses)
getThreadStats_ByThreadsIds' _ByThreadsIds = handleError <$> getAt [ByThreadsIds _ByThreadsIds] ["thread_stats"]

getThreadStat :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadStatResponse)
getThreadStat params thread_id = handleError <$> getAt params ["thread_stat", T.pack $ show thread_id]

getThreadStat' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadStatResponse)
getThreadStat' thread_id = handleError <$> getAt ([] :: [(Text, Text)]) ["thread_stat", T.pack $ show thread_id]

getThreadsCount_ByBoardId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadsCount_ByBoardId params _ByBoardId = handleError <$> getAt (map qp params <> map qp [ByBoardId _ByBoardId]) ["threads_count"]

getThreadsCount_ByBoardId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadsCount_ByBoardId' _ByBoardId = handleError <$> getAt [ByBoardId _ByBoardId] ["threads_count"]

getThreadPosts :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponses)
getThreadPosts params = handleError <$> getAt params ["thread_posts"]

getThreadPosts' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponses)
getThreadPosts'  = handleError <$> getAt ([] :: [(Text, Text)]) ["thread_posts"]

postThreadPost :: forall qp. QueryParam qp => [qp] -> ThreadPostRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponse)
postThreadPost params thread_post_request = handleError <$> postAt params ["thread_posts"] thread_post_request

postThreadPost' :: ThreadPostRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponse)
postThreadPost' thread_post_request = handleError <$> postAt ([] :: [(Text, Text)]) ["thread_posts"] thread_post_request

postThreadPost_ByThreadId :: forall qp. QueryParam qp => [qp] -> Int64 -> ThreadPostRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponse)
postThreadPost_ByThreadId params _ByThreadId thread_post_request = handleError <$> postAt (map qp params <> map qp [ByThreadId _ByThreadId]) ["thread_posts"] thread_post_request

postThreadPost_ByThreadId' :: Int64 -> ThreadPostRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponse)
postThreadPost_ByThreadId' _ByThreadId thread_post_request = handleError <$> postAt [ByThreadId _ByThreadId] ["thread_posts"] thread_post_request

getThreadPost :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponse)
getThreadPost params thread_post_id = handleError <$> getAt params ["thread_posts", T.pack $ show thread_post_id]

getThreadPost' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponse)
getThreadPost' thread_post_id = handleError <$> getAt ([] :: [(Text, Text)]) ["thread_posts", T.pack $ show thread_post_id]

putThreadPost :: forall qp. QueryParam qp => [qp] -> Int64 -> ThreadPostRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponse)
putThreadPost params thread_post_id thread_post_request = handleError <$> putAt params ["thread_posts", T.pack $ show thread_post_id] thread_post_request

putThreadPost' :: Int64 -> ThreadPostRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostResponse)
putThreadPost' thread_post_id thread_post_request = handleError <$> putAt ([] :: [(Text, Text)]) ["thread_posts", T.pack $ show thread_post_id] thread_post_request

deleteThreadPost :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteThreadPost params thread_post_id = handleError <$> deleteAt params ["thread_posts", T.pack $ show thread_post_id]

deleteThreadPost' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteThreadPost' thread_post_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["thread_posts", T.pack $ show thread_post_id]

getThreadPostStats_ByThreadPostsIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostStatResponses)
getThreadPostStats_ByThreadPostsIds params _ByThreadPostsIds = handleError <$> getAt (map qp params <> map qp [ByThreadPostsIds _ByThreadPostsIds]) ["thread_post_stats"]

getThreadPostStats_ByThreadPostsIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostStatResponses)
getThreadPostStats_ByThreadPostsIds' _ByThreadPostsIds = handleError <$> getAt [ByThreadPostsIds _ByThreadPostsIds] ["thread_post_stats"]

getThreadPostStat :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostStatResponse)
getThreadPostStat params threadpost_id = handleError <$> getAt params ["thread_post_stat", T.pack $ show threadpost_id]

getThreadPostStat' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostStatResponse)
getThreadPostStat' threadpost_id = handleError <$> getAt ([] :: [(Text, Text)]) ["thread_post_stat", T.pack $ show threadpost_id]

getThreadPostsCount_ByBoardId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadPostsCount_ByBoardId params _ByBoardId = handleError <$> getAt (map qp params <> map qp [ByBoardId _ByBoardId]) ["thread_posts_count"]

getThreadPostsCount_ByBoardId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadPostsCount_ByBoardId' _ByBoardId = handleError <$> getAt [ByBoardId _ByBoardId] ["thread_posts_count"]

getThreadPostsCount_ByThreadId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadPostsCount_ByThreadId params _ByThreadId = handleError <$> getAt (map qp params <> map qp [ByThreadId _ByThreadId]) ["thread_posts_count"]

getThreadPostsCount_ByThreadId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) CountResponses)
getThreadPostsCount_ByThreadId' _ByThreadId = handleError <$> getAt [ByThreadId _ByThreadId] ["thread_posts_count"]

getLikes :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponses)
getLikes params = handleError <$> getAt params ["likes"]

getLikes' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponses)
getLikes'  = handleError <$> getAt ([] :: [(Text, Text)]) ["likes"]

postLikes :: forall qp. QueryParam qp => [qp] -> LikeRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
postLikes params like_request = handleError <$> postAt params ["likes"] like_request

postLikes' :: LikeRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
postLikes' like_request = handleError <$> postAt ([] :: [(Text, Text)]) ["likes"] like_request

postLikes_ByThreadPostId :: forall qp. QueryParam qp => [qp] -> Int64 -> LikeRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
postLikes_ByThreadPostId params _ByThreadPostId like_request = handleError <$> postAt (map qp params <> map qp [ByThreadPostId _ByThreadPostId]) ["likes"] like_request

postLikes_ByThreadPostId' :: Int64 -> LikeRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
postLikes_ByThreadPostId' _ByThreadPostId like_request = handleError <$> postAt [ByThreadPostId _ByThreadPostId] ["likes"] like_request

getLike :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
getLike params like_id = handleError <$> getAt params ["likes", T.pack $ show like_id]

getLike' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
getLike' like_id = handleError <$> getAt ([] :: [(Text, Text)]) ["likes", T.pack $ show like_id]

putLike :: forall qp. QueryParam qp => [qp] -> Int64 -> LikeRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
putLike params like_id like_request = handleError <$> putAt params ["likes", T.pack $ show like_id] like_request

putLike' :: Int64 -> LikeRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
putLike' like_id like_request = handleError <$> putAt ([] :: [(Text, Text)]) ["likes", T.pack $ show like_id] like_request

deleteLike :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteLike params like_id = handleError <$> deleteAt params ["likes", T.pack $ show like_id]

deleteLike' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ())
deleteLike' like_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["likes", T.pack $ show like_id]

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

postUsers :: forall qp. QueryParam qp => [qp] -> UserRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
postUsers params user_request = handleError <$> postAt params ["users"] user_request

postUsers' :: UserRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
postUsers' user_request = handleError <$> postAt ([] :: [(Text, Text)]) ["users"] user_request

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

getUserPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserPackResponses)
getUserPacks params = handleError <$> getAt params ["user_packs"]

getUserPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserPackResponses)
getUserPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["user_packs"]

getUserPacks_ByUsersIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserPackResponses)
getUserPacks_ByUsersIds params _ByUsersIds = handleError <$> getAt (map qp params <> map qp [ByUsersIds _ByUsersIds]) ["user_packs"]

getUserPacks_ByUsersIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserPackResponses)
getUserPacks_ByUsersIds' _ByUsersIds = handleError <$> getAt [ByUsersIds _ByUsersIds] ["user_packs"]

getUserPacks_ByEmail :: forall qp. QueryParam qp => [qp] -> Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserPackResponses)
getUserPacks_ByEmail params _ByEmail = handleError <$> getAt (map qp params <> map qp [ByEmail _ByEmail]) ["user_packs"]

getUserPacks_ByEmail' :: Text -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserPackResponses)
getUserPacks_ByEmail' _ByEmail = handleError <$> getAt [ByEmail _ByEmail] ["user_packs"]

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

getForumPack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponse)
getForumPack params forum_id = handleError <$> getAt params ["forum_pack", T.pack $ show forum_id]

getForumPack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ForumPackResponse)
getForumPack' forum_id = handleError <$> getAt ([] :: [(Text, Text)]) ["forum_pack", T.pack $ show forum_id]

getBoardPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponses)
getBoardPacks params = handleError <$> getAt params ["board_packs"]

getBoardPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponses)
getBoardPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["board_packs"]

getBoardPacks_ByBoardsIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponses)
getBoardPacks_ByBoardsIds params _ByBoardsIds = handleError <$> getAt (map qp params <> map qp [ByBoardsIds _ByBoardsIds]) ["board_packs"]

getBoardPacks_ByBoardsIds' :: [Int64] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponses)
getBoardPacks_ByBoardsIds' _ByBoardsIds = handleError <$> getAt [ByBoardsIds _ByBoardsIds] ["board_packs"]

getBoardPacks_ByForumId :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponses)
getBoardPacks_ByForumId params _ByForumId = handleError <$> getAt (map qp params <> map qp [ByForumId _ByForumId]) ["board_packs"]

getBoardPacks_ByForumId' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) BoardPackResponses)
getBoardPacks_ByForumId' _ByForumId = handleError <$> getAt [ByForumId _ByForumId] ["board_packs"]

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

getThreadPostPack :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostPackResponse)
getThreadPostPack params thread_post_id = handleError <$> getAt params ["thread_post_pack", T.pack $ show thread_post_id]

getThreadPostPack' :: Int64 -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostPackResponse)
getThreadPostPack' thread_post_id = handleError <$> getAt ([] :: [(Text, Text)]) ["thread_post_pack", T.pack $ show thread_post_id]

getRecentThreadPostPacks :: forall qp. QueryParam qp => [qp] -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostPackResponses)
getRecentThreadPostPacks params = handleError <$> getAt params ["recent_thread_post_packs"]

getRecentThreadPostPacks' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) ThreadPostPackResponses)
getRecentThreadPostPacks'  = handleError <$> getAt ([] :: [(Text, Text)]) ["recent_thread_post_packs"]

-- footer