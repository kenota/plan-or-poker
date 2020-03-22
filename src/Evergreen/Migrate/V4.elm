module Evergreen.Migrate.V4 exposing (..)

import Evergreen.Type.V3 as Old
import Evergreen.Type.V4 as New
import Lamdera.Migrations exposing (..)
import Url exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    let
        initUrl =
            { protocol = Https
            , host = ""
            , port_ = Nothing
            , path = "/"
            , query = Nothing
            , fragment = Nothing
            }
    in
    ModelMigrated <| New.initFrontend initUrl old.key


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgUnchanged
