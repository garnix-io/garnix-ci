{-# LANGUAGE TemplateHaskell #-}
-- HasCodec instances make more sense here
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Garnix.YamlConfig
  ( Action (..),
    ActionSandboxType (..),
    ActionTrigger (..),
    sandboxType,
    trigger,
    withRepoContents,
    AttributeMatcher (..),
    BuildSection (..),
    DeploySection (OnBranch, OnPullRequest),
    ExcludeBranches (..),
    GarnixConfig,
    IncrementalizeBuildsSection (..),
    ModuleSection (..),
    ServerSection (..),
    _garnixConfigActions,
    actions,
    asAttributeMatcher,
    branchSection,
    buildSections,
    configuration,
    decodeConfig,
    deploySection,
    deployTypeExplanation,
    excludeBranches,
    excludeSection,
    firstPart,
    getConfig,
    includeSection,
    incrementalizeBuildsSection,
    moduleSection,
    parseAttributeMatcher,
    secondPart,
    serverSection,
    thirdPart,
    fodChecks,
    flakeDir,
    safeGetAbsoluteFlakeDir,
  )
where

import Autodocodec
import Cradle qualified
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Tuple.Extra (fst3, snd3, thd3, uncurry3)
import Data.Void (Void)
import Data.Yaml (decodeEither', decodeFileEither, prettyPrintParseException)
import GHC.IsList (fromList)
import Garnix.Hosting.ServerPool.Types
import Garnix.Log
import Garnix.Monad
import Garnix.NixConfig (addNixConfigEnvironment)
import Garnix.Prelude
import Garnix.Sandbox
import Garnix.Types
import System.Directory (doesFileExist)

getConfigFromFlake :: (HasCallStack) => M (Maybe GarnixConfig)
getConfigFromFlake = do
  cacheDir <- getNixXdgCacheDir
  nixConfig <- view #userNixConfig
  dir <- view #workingDir
  result <-
    (>>= Cradle.run)
      $ Cradle.cmd "nix"
      & Cradle.addArgs @Text
        [ "eval",
          ".#garnix.config",
          "--json"
        ]
      & addNixConfigEnvironment nixConfig
      & Cradle.setWorkingDir dir
      & Cradle.silenceStderr
      & pure
      & inNixSandbox [] (Just cacheDir)
  case result of
    (Cradle.ExitFailure _, _) -> pure Nothing
    (Cradle.ExitSuccess, Cradle.StdoutRaw stdout) ->
      case decodeEither' stdout of
        Left _ -> pure Nothing
        Right config -> pure $ Just config

getConfig :: (HasCallStack) => M GarnixConfig
getConfig = do
  getConfigFromFlake >>= \case
    Just config -> pure config
    Nothing -> do
      dir <- view #workingDir
      exists' <- liftIO . doesFileExist $ dir </> "garnix.yaml"
      if exists'
        then do
          eDecoded <- liftIO . decodeFileEither $ dir </> "garnix.yaml"
          case eDecoded of
            Left e -> throw $ DecodeConfigError . cs $ prettyPrintParseException e
            Right decoded -> pure decoded
        else pure def

decodeConfig :: ByteString -> Either String GarnixConfig
decodeConfig = first prettyPrintParseException . decodeEither'

newtype AttributePartMatcher = AttributePartMatcher {getAttributePartMatcher :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)

instance ConvertibleStrings Text AttributePartMatcher where
  convertString = AttributePartMatcher

instance ConvertibleStrings AttributePartMatcher Text where
  convertString = getAttributePartMatcher

-- E.g. 'packages.x86_64-linux.*' or 'nixosConfigurations.bar'
data AttributeMatcher = AttributeMatcher
  { _attributeMatcherFirstPart :: AttributePartMatcher,
    _attributeMatcherSecondPart :: AttributePartMatcher,
    _attributeMatcherThirdPart :: Maybe AttributePartMatcher
  }
  deriving stock (Eq, Show, Generic)

parseAttributeMatcher :: Text -> Either Text AttributeMatcher
parseAttributeMatcher x = case T.splitOn "." x of
  [a, b, c]
    | not (T.null a) && not (T.null b) && not (T.null c) ->
        pure $ AttributeMatcher (cs a) (cs b) (Just $ cs c)
  [a, b]
    | not (T.null a) && not (T.null b) ->
        pure $ AttributeMatcher (cs a) (cs b) Nothing
  _ -> Left "Expected 'x.y' or 'x.y.z' with non-empty parts"

renderAttributeMatcher :: AttributeMatcher -> Text
renderAttributeMatcher a = case _attributeMatcherThirdPart a of
  Nothing -> cs (_attributeMatcherFirstPart a) <> "." <> cs (_attributeMatcherSecondPart a)
  Just t ->
    cs (_attributeMatcherFirstPart a)
      <> "."
      <> cs (_attributeMatcherSecondPart a)
      <> "."
      <> cs t

asAttributeMatcher :: Prism' Text AttributeMatcher
asAttributeMatcher = prism renderAttributeMatcher parseAttributeMatcher

instance HasCodec AttributeMatcher where
  codec = bimapCodec (first cs . parseAttributeMatcher) renderAttributeMatcher textCodec

data BuildSection = BuildSection
  { _buildSectionIncludeSection :: [AttributeMatcher],
    _buildSectionExcludeSection :: [AttributeMatcher],
    _buildSectionBranchSection :: Maybe Branch
  }
  deriving stock (Eq, Show, Generic)

defaultIncludeSection :: [AttributeMatcher]
defaultIncludeSection =
  [ AttributeMatcher "*" "x86_64-linux" (Just "*"),
    AttributeMatcher "defaultPackage" "x86_64-linux" Nothing,
    AttributeMatcher "devShell" "x86_64-linux" Nothing,
    AttributeMatcher "homeConfigurations" "*" Nothing,
    AttributeMatcher "darwinConfigurations" "*" Nothing,
    AttributeMatcher "nixosConfigurations" "*" Nothing
  ]

instance Default BuildSection where
  def = BuildSection defaultIncludeSection [] Nothing

attributeMatcherExplanation :: Text
attributeMatcherExplanation =
  "This is a list of *attribute matchers*, of the form `x.y.z` or `x.y`. "
    <> "For example, `packages.x86_64-linux.*`, or `*.*`. Two-place matchers only "
    <> "match two-place matchers, and three-place matchers only match three-place "
    <> "matchers. '*' is the wildcard."

instance HasCodec BuildSection where
  codec =
    object "builds"
      $ BuildSection
      <$> optionalFieldWithDefault
        "include"
        defaultIncludeSection
        ("What builds to include. " <> attributeMatcherExplanation)
      .= _buildSectionIncludeSection
      <*> optionalFieldWithDefault
        "exclude"
        []
        ( "What builds to exclude. "
            <> attributeMatcherExplanation
            <> " This is applied *after* the 'include'. Thus, if something matches"
            <> " both the 'include' and the 'exclude', it will be excluded."
        )
      .= _buildSectionExcludeSection
      <*> optionalField
        "branch"
        "What (optional) branch this build section is enabled for."
      .= _buildSectionBranchSection

data ExcludeBranches = ExcludeBranches {_excludeBranchesExcludeBranches :: [Branch]}
  deriving stock (Eq, Show, Generic)

instance HasCodec ExcludeBranches where
  codec =
    object "ExcludesBranches"
      $ ExcludeBranches
      <$> requiredField
        "excludeBranches"
        "What branches *not* to incrementalize"
      .= _excludeBranchesExcludeBranches

data IncrementalizeBuildsSection
  = IncrementalizeBuilds Bool
  | IncrementalBuildsExcludeBranches ExcludeBranches
  deriving stock (Eq, Show, Generic)

instance HasCodec IncrementalizeBuildsSection where
  codec = dimapCodec there back $ disjointEitherCodec simpleCodec codec
    where
      simpleCodec = boolCodec
      there = \case
        Left v -> IncrementalizeBuilds v
        Right v -> IncrementalBuildsExcludeBranches v
      back = \case
        IncrementalizeBuilds v -> Left v
        IncrementalBuildsExcludeBranches v -> Right v

instance Default IncrementalizeBuildsSection where
  def = IncrementalizeBuilds False

data ServerSection = ServerSection
  { _serverSectionConfiguration :: PackageName,
    _serverSectionDeploySection :: DeploySection
  }
  deriving stock (Eq, Show, Generic)

instance HasCodec ServerSection where
  codec =
    object "servers"
      $ ServerSection
      <$> requiredField
        "configuration"
        "What attribute to deploy (e.g.: 'myServer' for 'nixosConfigurations.myServer')"
      .= _serverSectionConfiguration
      <*> requiredField
        "deployment"
        "When to deploy a new server, or redeploy an existing one"
      .= _serverSectionDeploySection

data DeploySection
  = OnPullRequest
  | OnBranch
      { branch :: Branch,
        tier :: ServerTier,
        isPrimary :: Bool
      }
  deriving stock (Eq, Show, Generic)

deployTypeExplanation :: Text
deployTypeExplanation =
  "When and how to deploy. The current available types "
    <> "are: \n"
    <> " - on-branch: deploy a new version every time the HEAD of the specified "
    <> "branch changes."

instance HasCodec DeploySection where
  codec =
    object "deployment"
      $ discriminatedUnionCodec "type" serialize deserialize
    where
      branchCodec =
        (,,)
          <$> requiredField "branch" "What git branch to deploy from"
          .= fst3
          <*> optionalFieldWithDefault "machine" (def :: ServerTier) "What server tier to deploy"
          .= snd3
          <*> optionalFieldWithDefault "isPrimary" False "If this deploy should also be reachable at «repo-name».«org-name».garnix.me"
          .= thd3
      serialize :: DeploySection -> (Discriminator, ObjectCodec DeploySection ())
      serialize = \case
        OnBranch branch serverType isPrimary ->
          ( "on-branch",
            mapToEncoder (branch, serverType, isPrimary) branchCodec
          )
        OnPullRequest -> ("on-pull-request", mapToEncoder () $ pureCodec ())
      deserialize :: HashMap Discriminator (Text, ObjectCodec Void DeploySection)
      deserialize =
        HashMap.fromList
          [ ("on-branch", ("", mapToDecoder (uncurry3 OnBranch) branchCodec)),
            ("on-pull-request", ("", mapToDecoder (const OnPullRequest) $ pureCodec ()))
          ]

instance HasCodec Branch where
  codec = dimapCodec Branch getBranch textCodec

instance HasCodec ServerTier where
  codec =
    bimapCodec deserialize serialize textCodec
    where
      deserialize :: Text -> Either String ServerTier
      deserialize t =
        case lookup t (map swap serverTierTextMapping) of
          Just serverType -> Right serverType
          Nothing -> do
            let serverTypes = map snd serverTierTextMapping
            Left $ cs ("Wrong server type. Supported server types are: " <> T.intercalate ", " serverTypes)
      serialize :: ServerTier -> Text
      serialize serverType =
        case lookup serverType serverTierTextMapping of
          Just t -> t
          Nothing -> error "Unknown server type"

instance HasCodec PackageName where
  codec = dimapCodec PackageName getPackageName textCodec

data ActionSandboxType = FastStartup | SharedResources
  deriving (Eq, Show)

instance HasCodec ActionSandboxType where
  codec =
    stringConstCodec
      $ fromList
        [(FastStartup, "fast-startup"), (SharedResources, "shared-resources")]

-- | Currently only one value, so we don't even need to inspect it. But we
-- add it for documentation, and so we can remain backwards compatible
-- (otherwise, 'push' will always have to be the default).
data ActionTrigger = ActionTriggerPush
  deriving (Eq, Show)

instance HasCodec ActionTrigger where
  codec = stringConstCodec $ fromList [(ActionTriggerPush, "push")]

data Action = Action
  { _actionName :: PackageName,
    _actionTrigger :: ActionTrigger,
    _actionSandboxType :: ActionSandboxType,
    _actionWithRepoContents :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance HasCodec Action where
  codec =
    object "actions"
      $ Action
      <$> requiredField "run" "Name of the nix app to run as an action."
      .= _actionName
      <*> requiredField "on" "Event that triggers this action"
      .= _actionTrigger
      <*> optionalFieldWithDefault "sandboxType" FastStartup "What sandbox type. If you want to use the 'SharedResources' type, get in touch with us."
      .= _actionSandboxType
      <*> optionalFieldWithDefault "withRepoContents" False "Whether the action should run with access to the entire repo. If false (default), only the closure of the action is available."
      .= _actionWithRepoContents

newtype ModuleSection = ModuleSection
  { publish :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance HasCodec ModuleSection where
  codec = object "modules" $ do
    ModuleSection <$> optionalFieldWithDefault "publish" False "Whether to publish modules from this repository." .= publish

instance HasCodec FlakeDir where
  codec = dimapCodec FlakeDir __unsafeGetFlakeDir stringCodec

instance Default ModuleSection where
  def =
    ModuleSection
      { publish = False
      }

data GarnixConfig = GarnixConfig
  { _garnixConfigBuildSections :: [BuildSection],
    _garnixConfigIncrementalizeBuildsSection :: IncrementalizeBuildsSection,
    _garnixConfigServerSection :: [ServerSection],
    _garnixConfigActions :: [Action],
    _garnixConfigModuleSection :: ModuleSection,
    _garnixConfigFodChecks :: Bool,
    _garnixConfigFlakeDir :: FlakeDir
  }
  deriving stock (Eq, Show, Generic)

instance Default GarnixConfig where
  def = GarnixConfig [def] def [] [] def False (FlakeDir ".")

instance FromJSON GarnixConfig where
  parseJSON = parseJSONViaCodec

instance ToJSON GarnixConfig where
  toJSON = toJSONViaCodec

instance HasCodec GarnixConfig where
  codec = obj `parseAlternative` fmap (const def) nullCodec
    where
      obj :: JSONCodec GarnixConfig
      obj =
        object "config"
          $ GarnixConfig
          <$> ( optionalFieldWithDefaultWith
                  "builds"
                  ( dimapCodec
                      (either pure identity)
                      ( \case
                          [a] -> Left a
                          a -> Right a
                      )
                      $ disjointEitherCodec
                        (codec :: JSONCodec BuildSection)
                        (codec :: JSONCodec [BuildSection])
                  )
                  [def]
                  ( "Specifies what should be built. Everything in the `include` "
                      <> "section, minus everything in the `exclude` section, is built."
                  )
                  .= _garnixConfigBuildSections
              )
          <*> ( optionalFieldWithDefault
                  "incrementalizeBuilds"
                  def
                  ( "Whether to override the `garnix-incrementalize` flake input "
                      <> "to point to an parent built commit. This allows incremental "
                      <> "builds. See our https://garnix.io/docs for more information."
                  )
                  .= _garnixConfigIncrementalizeBuildsSection
              )
          <*> ( optionalFieldWithDefault
                  "servers"
                  []
                  "Specifies what servers to deploy."
                  .= _garnixConfigServerSection
              )
          <*> ( optionalFieldWithDefault
                  "actions"
                  []
                  "Specifies which actions to run."
                  .= _garnixConfigActions
              )
          <*> ( optionalFieldWithDefault
                  "modules"
                  def
                  "Specifies which actions to run."
                  .= _garnixConfigModuleSection
              )
          <*> ( optionalFieldWithDefault
                  "fodChecks"
                  False
                  "Whether FOD checks are enabled for the repo. See https://garnix.io/docs/fod-checks for more information."
                  .= _garnixConfigFodChecks
              )
          <*> ( optionalFieldWithDefault
                  "flakeDir"
                  (FlakeDir ".")
                  "The directory containing your flake.nix relative from the repo root (if not in the repo root)."
                  .= _garnixConfigFlakeDir
              )

instance Loggable GarnixConfig where
  asLog = const []

makeFields ''ExcludeBranches
makeFields ''GarnixConfig
makeFields ''AttributeMatcher
makeFields ''BuildSection
makeFields ''ServerSection
makeFields ''Action
