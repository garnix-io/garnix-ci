module Garnix.BuildSpec (spec) where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.Async.Lifted qualified as Async
import Control.Concurrent.Extra (newEmptyMVar, putMVar)
import Control.Exception (ErrorCall (ErrorCall), throwIO)
import Control.Lens
import Control.Lens.Regex.Text
import Cradle
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key)
import Data.Maybe (fromJust)
import Data.Semigroup
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Yaml (encode)
import GHC.IO.Unsafe (unsafePerformIO)
import Garnix.API.Builds
import Garnix.Build (buildFlake)
import Garnix.Build.Types (derivation)
import Garnix.BuildLogs.Types (LogLine (LogLine))
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Entitlements (addDefaultEntitlements, hasRemainingCiTime, setExtraUsageLimits)
import Garnix.Monad
import Garnix.Monad.Async (resolve)
import Garnix.Nix.Types qualified as Nix
import Garnix.NixConfig (nixConfDefaults)
import Garnix.Orchestrator
import Garnix.Prelude
import Garnix.Reporters.GithubReporter (mkGithubReporter)
import Garnix.Reporters.OpenSearchReporter (openSearchReporter)
import Garnix.TestHelpers
import Garnix.TestHelpers.Common
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.Types hiding (context, head)
import Garnix.UserLogs (getLogLines)
import Garnix.YamlConfig
import Network.HTTP.Types (ok200, status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, getRequestBodyChunk, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Handler.Warp qualified as Warp
import System.Directory (copyFile, createDirectoryIfMissing)
import System.Random
import Test.HUnit
import Test.Hspec hiding (shouldReturn)
import Text.Regex.PCRE.Light (compile, dollar_endonly)

spec :: Spec
spec = do
  describe "builds" $ do
    inM . aroundM_ suppressLogsWhenPassing . beforeM_ truncateDBM $ do
      it "reports successes" $ GH.withFakeGithubInterface $ \ghState -> do
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup "{ outputs = {self}: { packages = {}; }; }") $ \commitInfo -> do
          testHandleCommit commitInfo
          res <- GH.assertSingleRunForReport "Evaluate flake.nix" =<< GH.getReports ghState
          res
            `GH.reposAndReportsShouldBe` [ ("Evaluate flake.nix", RunReportStatusInProgress),
                                           ("Evaluate flake.nix", RunReportStatusSuccess)
                                         ]

      it "reports successful builds to github" $ GH.withFakeGithubInterface $ \ghState -> do
        randomness :: Int <- randomIO
        let flake =
              cs
                [i|
                  { outputs = {self}: {
                      packages.x86_64-linux.succeeding = derivation {
                        name = "succeeding";
                        builder = "/bin/sh";
                        system = "x86_64-linux";
                        args = [ "-c" "echo test build log ; echo #{randomness} > $out" ];
                      };
                    };
                  }
                |]
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
          testHandleCommit commitInfo
          reports <- GH.getReports ghState
          let logLines = T.lines $ last $ mconcat (reports & mapped . mapped %~ (^. _2 . logs . to getRawLogs))
          logLines `shouldContainM` ["succeeding> test build log"]

      it "reports successful builds to opensearch" $ GH.withFakeGithubInterface $ \ghState -> do
        randomness :: Int <- randomIO
        let flake =
              cs
                [i|
                  { outputs = {self}: {
                      packages.x86_64-linux.succeeding = derivation {
                        name = "succeeding";
                        builder = "/bin/sh";
                        system = "x86_64-linux";
                        args = [ "-c" "echo test build log ; echo #{randomness} > $out" ];
                      };
                    };
                  }
                |]
        user <- DB.newUser (GhLogin "owner") "owner@owner.com" FreeSubscription True
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
          testHandleCommit commitInfo
          build <- fromSingleton <$> filter (\b -> b ^. packageType /= TypeOverall) <$> DB.getBuilds user
          logLines <-
            getLogLines build 1000 Nothing
              <&> mapped %~ (\msg -> (msg ^. package, msg ^. logMessage))
          logLines `shouldContainM` [(Just "succeeding", "test build log")]

      it "skips builds if the same repoOwner, repoName, commit, and branch are pushed multiple times" $ GH.withFakeGithubInterface $ \ghState -> do
        user <- DB.newUser (GhLogin "owner") "owner@owner.com" FreeSubscription True
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup "{ outputs = {self}: { packages = {}; }; }") $ \commitInfo -> do
          let reporter = mkGithubReporter (commitInfo ^. repoInfo) (commitInfo ^. commit) <> openSearchReporter
          resolve =<< handleCommit reporter False commitInfo
          (length <$> DB.getBuilds user) `shouldReturnM` 1
          resolve =<< handleCommit reporter False commitInfo
          (length <$> DB.getBuilds user) `shouldReturnM` 1

      describe "recording builds that have already been built" $ do
        it "marks builds that have been already built as such" $ GH.withFakeGithubInterface $ \ghState -> do
          randomness :: Int <- randomIO
          let flake =
                cs
                  [i|
                    { outputs = {self}: {
                        packages.x86_64-linux.succeeding = derivation {
                          name = "not-already-built";
                          builder = "/bin/sh";
                          system = "x86_64-linux";
                          args = [ "-c" "echo #{randomness} > $out" ];
                        };
                      };
                    }
                  |]
          user <- DB.newUser (GhLogin "owner") "owner@owner.com" FreeSubscription True
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
            testHandleCommit commitInfo
            build1 <-
              DB.getBuilds user
                <&> filter (\b -> b ^. packageType /= TypeOverall)
                <&> fromSingleton
            testHandleCommit commitInfo
            build2 <-
              DB.getBuilds user
                <&> filter (\b -> b ^. packageType /= TypeOverall && b ^. id /= build1 ^. id)
                <&> fromSingleton
            build1 ^. alreadyBuilt `shouldBeM` Just False
            build2 ^. alreadyBuilt `shouldBeM` Just True

        it "sets already_built to false if evaluation fails" $ GH.withFakeGithubInterface $ \ghState -> do
          let flake =
                cs
                  [i|
                    { outputs = {self}: {
                        packages.x86_64-linux.bad-evaluation = derivation { };
                      };
                    }
                  |]
          user <- DB.newUser (GhLogin "owner") "owner@owner.com" FreeSubscription True
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
            void $ try $ testHandleCommit commitInfo
            build <-
              DB.getBuilds user
                <&> filter (\b -> b ^. packageType /= TypeOverall)
                <&> fromSingleton
            build ^. alreadyBuilt `shouldBeM` Just False

      context "failing evaluations" $ do
        let testDebounceDuration = fromMilliSeconds @Int 100
        aroundM_ (local (#githubLogDebounceDuration .~ testDebounceDuration)) $ do
          it "should report flake.nix syntax errors" $ GH.withFakeGithubInterface $ \ghState -> do
            GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup "{ abc") $ \commitInfo -> do
              expectErr $ testHandleCommit commitInfo
              threadDelay (testDebounceDuration `addDuration` testDebounceDuration)
              res <- GH.assertSingleRunForReport "Evaluate flake.nix" =<< GH.getReports ghState
              res
                `GH.reposAndReportsShouldBe` [ ("Evaluate flake.nix", RunReportStatusInProgress),
                                               ("Evaluate flake.nix", RunReportStatusFailure)
                                             ]
              let expected = [regex|.*Command '.*' failed with exit code 1.\nStandard err was:.*\n(.*warning:.*\n)*.*error: syntax error.*|]
              logs <- GH.getFinalLogs ghState (commitInfo ^. commit) "Evaluate flake.nix"
              liftIO $ logs `shouldMatch` expected

          it "should report flake.nix missing required fields" $ GH.withFakeGithubInterface $ \ghState -> do
            GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup "{ }") $ \commitInfo -> do
              expectErr $ testHandleCommit commitInfo
              threadDelay (testDebounceDuration `addDuration` testDebounceDuration)
              res <- GH.assertSingleRunForReport "Evaluate flake.nix" =<< GH.getReports ghState
              res
                `GH.reposAndReportsShouldBe` [ ("Evaluate flake.nix", RunReportStatusInProgress),
                                               ("Evaluate flake.nix", RunReportStatusFailure)
                                             ]
              let expected = [regex|.*Command '.*' failed with exit code 1.\nStandard err was:.*\n.*(.*warning:.*\n)*.*error: flake '.*' lacks attribute 'outputs'.*|]
              logs <- GH.getFinalLogs ghState (commitInfo ^. commit) "Evaluate flake.nix"
              liftIO $ logs `shouldMatch` expected

          it "should report flake.nix type errors" $ GH.withFakeGithubInterface $ \ghState -> do
            GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup "{ outputs = {}; }") $ \commitInfo -> do
              expectErr $ testHandleCommit commitInfo
              threadDelay (testDebounceDuration `addDuration` testDebounceDuration)
              res <- GH.assertSingleRunForReport "Evaluate flake.nix" =<< GH.getReports ghState
              res
                `GH.reposAndReportsShouldBe` [ ("Evaluate flake.nix", RunReportStatusInProgress),
                                               ("Evaluate flake.nix", RunReportStatusFailure)
                                             ]
              let expected = [regex|.*Command '.*' failed with exit code 1.\nStandard err was:.*\n.*(.*warning:.*\n)*.*error: expected a function but got a set.*|]
              logs <- GH.getFinalLogs ghState (commitInfo ^. commit) "Evaluate flake.nix"
              liftIO $ logs `shouldMatch` expected

      describe "flake input checks" $ do
        let flakeWithInput :: Text -> Text
            flakeWithInput flakeInput =
              cs
                $ unindent
                  [i|
                      {
                        inputs.foo = {
                          url = "#{flakeInput}";
                          flake = false;
                        };
                        outputs = {...} : {};
                      }
                    |]

            tryBuildFlake flake = do
              GH.withFakeGithubInterface $ \ghState -> do
                let repoSetup repo = do
                      GH.simpleSetup flake repo
                      liftIO $ writeFile (repo </> "some-repo-file") "contents"
                GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo repoSetup $ \commitInfo -> do
                  try (testHandleCommit commitInfo)

            withHttpAuthority :: (Text -> M a) -> M a
            withHttpAuthority action = do
              let app :: Application
                  app _req send = do
                    send $ responseLBS ok200 [] "test response"
              liftBaseOp (testWithApplication (pure app)) $ \port ->
                action $ "localhost:" <> show port

        it "disallows `file+file:` inputs" $ do
          result <- tryBuildFlake $ flakeWithInput "file+file:/etc/hostname"
          first err result
            `shouldBeM` Left (OtherError "flake input disallowed: file:/etc/hostname")

        it "disallows `file:` inputs" $ do
          result <- tryBuildFlake $ flakeWithInput "file:/etc/hostname"
          first err result
            `shouldBeM` Left (OtherError "flake input disallowed: file:/etc/hostname")

        it "allows `git+https` inputs" $ do
          result <- tryBuildFlake $ flakeWithInput "git+https://github.com/garnix-io/garnix-lib"
          first err result `shouldBeM` Right ()

        it "allows `file+http:` inputs" $ do
          withHttpAuthority $ \authority -> do
            result <- tryBuildFlake $ flakeWithInput $ "file+http://" <> authority
            first err result `shouldBeM` Right ()

        it "allows `http:` inputs" $ do
          withHttpAuthority $ \authority -> do
            result <- tryBuildFlake $ flakeWithInput $ "http://" <> authority
            first err result `shouldBeM` Right ()

        it "allows `file+https:` inputs" $ do
          result <- tryBuildFlake $ flakeWithInput "file+https://garnix.io"
          first err result `shouldBeM` Right ()

        it "allows `https:` inputs" $ do
          result <- tryBuildFlake $ flakeWithInput "https://garnix.io"
          first err result `shouldBeM` Right ()

        it "disallows absolute `path:` inputs" $ do
          result <- tryBuildFlake $ flakeWithInput "path:/etc/hostname"
          first err result
            `shouldBeM` Left (OtherError "flake inputs of type 'path:' not allowed: path:/etc/hostname")

        it "disallows absolute inputs with implicit `path:` type" $ do
          result <- tryBuildFlake $ flakeWithInput "/etc/hostname"
          first err result
            `shouldBeM` Left (OtherError "flake inputs of type 'path:' not allowed: path:/etc/hostname")

        it "allows `path:` inputs for paths in the repo" $ do
          result <- tryBuildFlake $ flakeWithInput "path:./some-repo-file"
          first err result `shouldBeM` Right ()

        it "disallows `path:` inputs for paths that escape the repo with `..`" $ do
          Left result <- tryBuildFlake $ flakeWithInput "path:./../bar"
          let expected = [regex|flake inputs of type 'path:' not allowed: path:./../bar|]
          liftIO $ show (pretty $ err result) `shouldMatch` expected

        it "disallows `path:` inputs for paths that escape the repo with `..`" $ suppressLogs $ do
          StdoutTrimmed helloStorePath <-
            run $ cmd "nix"
              & addArgs
                [ "build" :: String,
                  "github:nixos/nixpkgs/d3c490e9c812d0a9dcb0593663d9430451fb8f96#hello",
                  "--no-link",
                  "--print-out-paths"
                ]
              & nixConfDefaults
          let Just pathInStore = T.stripPrefix "/nix/store/" helloStorePath
          Left result <- tryBuildFlake $ flakeWithInput $ "./../" <> pathInStore
          let expected = [regex|flake inputs of type 'path:' not allowed: path:./../[a-z0-9]+-hello-[.\d]+|]
          liftIO $ show (pretty $ err result) `shouldMatch` expected

      it "allows pipe-operators" $ GH.withFakeGithubInterface $ \ghState -> do
        let flake =
              cs
                $ unindent
                  [i|
                    {
                      outputs = {self}: {
                        foo = 42 |> builtins.toJSON;
                      };
                    }
                  |]
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
          testHandleCommit commitInfo

      it "should report build result to the database" $ do
        GH.withFakeGithubInterface $ \ghState -> do
          user <- DB.newUser (GhLogin "owner") "owner@owner.com" FreeSubscription True
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup "{ outputs = {self}: { packages = {}; }; }") $ \commitInfo -> do
            testHandleCommit commitInfo
            build <- fromSingleton <$> DB.getBuilds user
            (build ^. status) `shouldBeM` Just Success

      it "should report build end time to the database" $ do
        GH.withFakeGithubInterface $ \ghState -> do
          user <- DB.newUser (GhLogin "owner") "owner@owner.com" FreeSubscription True
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup "{ outputs = {self}: { packages = {}; }; }") $ \commitInfo -> do
            beforeBuild <- liftIO getCurrentTime
            testHandleCommit commitInfo
            build <- fromSingleton <$> DB.getBuilds user
            afterBuild <- liftIO getCurrentTime
            (build ^. endTime)
              `shouldSatisfyM` \case
                Just endTime -> endTime >= beforeBuild && endTime <= afterBuild
                Nothing -> False

      it "should not allow unauthenticated users to view private repos" $ do
        user <- DB.newUser (GhLogin "owner") "owner@owner.com" FreeSubscription True
        GH.withFakeGithubInterface $ \ghState -> do
          let commitInfo = defaultCommitInfo & repoPublicity .~ RepoIsPublic False
          GH.withLocalRepo ghState "owner" "repo" identity commitInfo (GH.simpleSetup "{ outputs = _: {}; }") $ \commitInfo -> do
            testHandleCommit commitInfo
            build <- fromSingleton <$> DB.getBuilds user
            result <- try $ getBuild' Nothing (build ^. id)
            result `shouldSatisfyM` isLeft

      it "does not throw if it is unable to update the github report" $ GH.withFakeGithubInterface $ \ghState -> do
        randomness :: Int <- randomIO
        let flake =
              cs
                [i|
                  { outputs = {self}: {
                      packages.x86_64-linux.succeeding = derivation {
                        name = "succeeding";
                        builder = "/bin/sh";
                        system = "x86_64-linux";
                        args = [ "-c" "echo #{randomness} > $out" ];
                      };
                    };
                  }
                |]
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
          curGithubIface <- (^. #githubInterface) <$> ask
          let throwingGithubIface = curGithubIface {_githubInterfaceUpdateBuildReport = const $ const $ const $ throw $ OtherError "failed to update build report"}
          local (#githubInterface .~ throwingGithubIface) $ testHandleCommit commitInfo

      it "adds the build output paths to the builds table" $ GH.withFakeGithubInterface $ \ghState -> do
        user <- DB.newUser (GhLogin "owner") "owner@owner.com" FreeSubscription True
        randomness :: Int <- randomIO
        let flake =
              cs
                [i|
                  { outputs = {self}: {
                      packages.x86_64-linux.multi-output = derivation {
                        name = "multi-output";
                        builder = "/bin/sh";
                        system = "x86_64-linux";
                        outputs = [ "out" "foo" "bar" ];
                        args = [ "-c" ''
                          echo #{randomness} > $out
                          echo #{randomness} > $foo
                          echo #{randomness} > $bar
                        '' ];
                      };
                    };
                  }
                |]
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
          testHandleCommit commitInfo
          build <- fromSingleton . filter (\b -> b ^. packageType == TypePackage) <$> DB.getBuilds user
          let BuildOutputsPgColumn outputs = fromJust $ build ^. outputPaths
          Nix.getOutputByName "out" outputs `shouldSatisfyM` isJust
          Nix.getOutputByName "foo" outputs `shouldSatisfyM` isJust
          Nix.getOutputByName "bar" outputs `shouldSatisfyM` isJust

      it "reports the correct url for builds" $ GH.withFakeGithubInterface $ \ghState -> do
        user <- DB.newUser (GhLogin "owner") "owner@owner.com" FreeSubscription True
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup "{ outputs = _: {}; }") $ \commitInfo -> do
          testHandleCommit commitInfo
          build <- fromSingleton <$> DB.getBuilds user
          let expectedUrl = "/build/" <> (build ^. id . to getBuildId . re hashIdText)
          res <- GH.assertSingleRunForReport "Evaluate flake.nix" =<< GH.getReports ghState
          map (\(_repoInfo, runReport) -> runReport ^. url) res `shouldBeM` [Just expectedUrl, Just expectedUrl]

      it "still attempts to upload even if part of the build failed" $ do
        random :: Int <- randomIO
        let flake =
              cs
                [i|
                  {
                    outputs = {self}: {
                      packages.x86_64-linux = let
                        mkDerivation = name: script: derivation {
                          inherit name;
                          builder = "/bin/sh";
                          system = "x86_64-linux";
                          args = [ "-c" "# #{random}\n${script}" ];
                        };
                        good = mkDerivation "good" "echo > $out";
                      in {
                        parent = mkDerivation "parent" "# ${good}\nexit 1";
                      };
                    };
                  }
                |]

        GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
            _result <- try $ testHandleCommit commitInfo
            call <- fromSingleton . map (_1 .~ ()) <$> getMockCalls #s3CacheUploadMock
            sort ((^.. _4 . #toUpload . each . to Nix.getName) call) `shouldBeM` ["good", "parent"]

      it "pins its packages until upload is finished" $ do
        random :: Int <- randomIO
        mvar <- liftIO newEmptyMVar
        let flake =
              cs
                [i|
                  { outputs = {self}: {
                      packages.x86_64-linux.output = derivation {
                        name = "output";
                        builder = "/bin/sh";
                        system = "x86_64-linux";
                        outputs = [ "out" ];
                        args = [ "-c" ''
                          echo #{random} > $out
                        '' ];
                      };
                    };
                  }
                |]
            tryDeleting (_, _, _, evalResult, _) = do
              let pathStr = Nix.getStorePath $ Nix.getDrvPath $ derivation evalResult
              (_exitCode :: ExitCode, StderrRaw err, StdoutTrimmed _out) <- run $ cmd "nix" & addArgs ["store", "delete", pathStr, "--extra-experimental-features", "nix-command flakes"]
              liftIO $ putMVar mvar (show err)
        GH.withFakeGithubInterface $ \ghState -> withMock #s3CacheUploadMock tryDeleting
          $ GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake)
          $ \commitInfo -> do
            _ <- try $ testHandleCommit commitInfo
            result <- liftIO $ readMVar mvar
            cs result `shouldContainM` "Cannot delete path"

      describe "flakes in subdirectories" $ do
        let setupWithConfiguredFlakeDir :: FlakeDir -> FilePath -> FilePath -> M ()
            setupWithConfiguredFlakeDir flakeDir' pathToFlakeWithinRepo repoPath = do
              random :: Int <- randomIO
              let config :: GarnixConfig = def & flakeDir .~ flakeDir'
              let flake =
                    [i|
                        {
                          outputs = {self}: {
                            packages.x86_64-linux.test = derivation {
                              name = "test";
                              builder = "/bin/sh";
                              system = "x86_64-linux";
                              args = [ "-c" "echo 'success!' && echo #{random} > $out" ];
                            };
                          };
                        }
                      |]
              liftIO $ do
                T.writeFile (repoPath </> "garnix.yaml") (cs $ encode config)
                createDirectoryIfMissing True (repoPath </> pathToFlakeWithinRepo)
                T.writeFile (repoPath </> pathToFlakeWithinRepo </> "flake.nix") (cs flake)
                copyFile "../flake.lock" (repoPath </> pathToFlakeWithinRepo </> "flake.lock")

        it "allows flakes to be in a subdirectory" $ GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (setupWithConfiguredFlakeDir (FlakeDir "some/nested/dir") "some/nested/dir") $ \commitInfo -> do
            testHandleCommit commitInfo
            logs <- GH.getFinalLogs ghState (commitInfo ^. commit) "package test [x86_64-linux]"
            cs logs `shouldContainM` "success!"

        it "prevents specifying absolute paths" $ GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (setupWithConfiguredFlakeDir (FlakeDir "/some/absolute/path") "some/nested/dir") $ \commitInfo -> do
            result <- try $ testHandleCommit commitInfo
            (result & _Left %~ err) `shouldBeM` Left (OtherError "'/some/absolute/path' is not a path within the repo")

        it "prevents specifying relative paths outside of the repo root" $ GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (setupWithConfiguredFlakeDir (FlakeDir "../../../etc/passwd") "some/nested/dir") $ \commitInfo -> do
            result <- try $ testHandleCommit commitInfo
            (result & _Left %~ err) `shouldBeM` Left (OtherError "'../../../etc/passwd' is not a path within the repo")

      context "incremental builds" $ do
        let normalLog = [regex|inc> Starting\ninc> Finished\n|]
            incrementalLog = [regex|inc> Starting\ninc> hi\ninc> Finished\n|]
            mkFlake :: M Text
            mkFlake = do
              rand :: Int <- liftIO randomIO
              pure
                $ cs
                  [i|
                    {
                      inputs.garnix-incrementalize.url = "github:garnix-io/incrementalize/main";
                      # If you update this, update also places where it matches.
                      # Search for INNER_NIXPKGS_MATCHES
                      inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11-small";
                      outputs = { garnix-incrementalize, nixpkgs, ... } :
                        let pkgs = nixpkgs.legacyPackages.x86_64-linux;
                        in garnix-incrementalize.lib.withCaches {
                          packages.x86_64-linux.pkg = cache : derivation {
                              name = "inc";
                              builder = "/bin/sh";
                              system = "x86_64-linux";
                              outputs = [ "out" "intermediates" ];
                              args = [ "-c" ''
                                # #{rand}
                                set -o errexit
                                echo Starting
                                ${pkgs.coreutils}/bin/mkdir $intermediates
                                ${pkgs.coreutils}/bin/mkdir $out
                                ${pkgs.coreutils}/bin/touch "$intermediates"/file
                                if [ -f ${cache}/file ]; then
                                  ${pkgs.coreutils}/bin/cat ${cache}/file > "$intermediates"/file
                                  ${pkgs.coreutils}/bin/cat ${cache}/file
                                fi
                                echo hi > "$intermediates"/file
                                echo Finished
                              ''];
                          };
                        };
                    }
                  |]
        let shouldNotIncrementalize :: (HasCallStack) => GH.GithubFakeState -> CommitHash -> M ()
            shouldNotIncrementalize ghState hash = do
              logs <- GH.getFinalLogs ghState hash "package pkg [x86_64-linux]"
              liftIO $ do
                logs `shouldMatch` normalLog
                logs `shouldNotMatch` incrementalLog

            shouldIncrementalize :: (HasCallStack) => GH.GithubFakeState -> CommitHash -> M ()
            shouldIncrementalize ghState hash = do
              logs <- GH.getFinalLogs ghState hash "package pkg [x86_64-linux]"
              liftIO $ logs `shouldMatch` incrementalLog

            createFakeCommit ghState = do
              testRepoPath <- fromJust . (^. #localPath) . fromJust <$> GH.lookupRepo ghState "owner" "repo"
              randomness :: Int <- randomIO
              liftIO $ T.writeFile (testRepoPath </> "some-file") (show randomness)
              commitAll testRepoPath

        it "inputs an empty flake if there are no previous builds" $ do
          GH.withFakeGithubInterface $ \ghState -> do
            flake <- mkFlake
            GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
              testHandleCommit commitInfo
              shouldNotIncrementalize ghState (commitInfo ^. commit)

        it "inputs an empty flake if incremental builds are not enabled" $ do
          GH.withFakeGithubInterface $ \ghState -> do
            flake <- mkFlake
            GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
              testHandleCommit commitInfo
              commit' <- createFakeCommit ghState
              testHandleCommit (commitInfo & commit .~ commit')
              shouldNotIncrementalize ghState (commitInfo ^. commit)

        it "inputs an empty flake if incremental builds are not enabled on this branch" $ do
          let config =
                def
                  & incrementalizeBuildsSection
                    .~ IncrementalBuildsExcludeBranches (ExcludeBranches [defaultCommitInfo ^?! branch . _Just])
          GH.withFakeGithubInterface $ \ghState -> do
            flake <- mkFlake
            GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.setupWithGarnixConfig config flake) $ \commitInfo -> do
              testHandleCommit commitInfo
              commit' <- createFakeCommit ghState
              testHandleCommit (commitInfo & commit .~ commit')
              shouldNotIncrementalize ghState (commitInfo ^. commit)

        it "inputs the previous commit if the previous build is done and incrementalism is always enabled" $ do
          let config = def & incrementalizeBuildsSection .~ IncrementalizeBuilds True
          GH.withFakeGithubInterface $ \ghState -> do
            flake <- mkFlake
            GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.setupWithGarnixConfig config flake) $ \commitInfo -> do
              testHandleCommit commitInfo
              commit' <- createFakeCommit ghState
              testHandleCommit (commitInfo & commit .~ commit')
              shouldIncrementalize ghState commit'

        it "inputs the previous commit if the previous build is done and the branch is not excluded" $ do
          let config =
                def
                  & incrementalizeBuildsSection
                    .~ IncrementalBuildsExcludeBranches (ExcludeBranches ["somethingthatwontmatch"])
          GH.withFakeGithubInterface $ \ghState -> do
            flake <- mkFlake
            GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.setupWithGarnixConfig config flake) $ \commitInfo -> do
              testHandleCommit commitInfo
              commit' <- createFakeCommit ghState
              testHandleCommit (commitInfo & commit .~ commit')
              shouldIncrementalize ghState commit'

      it "should allow unauthenticated users to view public repos" $ do
        user <- DB.newUser (GhLogin "owner") "owner@owner.com" FreeSubscription True
        GH.withFakeGithubInterface $ \ghState -> do
          let commitInfo = defaultCommitInfo & repoPublicity .~ RepoIsPublic True
          GH.withLocalRepo ghState "owner" "repo" identity commitInfo (GH.simpleSetup "{ outputs = _: {}; }") $ \commitInfo -> do
            testHandleCommit commitInfo
            build <- fromSingleton <$> DB.getBuilds user
            void $ getBuild' Nothing (build ^. id)

      it "returns an error if a user has exhausted their quota" $ do
        let flake = "{ outputs = {self}: { packages = {}; }; }"
        GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
            useUpAllBuildQuota "owner"
            result <- try $ testHandleCommit commitInfo
            (result & _Left %~ err) `shouldBeM` Left (EntitlementError "You have exhausted your monthly CI quota")

      it "notifies the user when the CI minutes quota is exhausted" $ do
        let flake = "{ outputs = {self}: { packages = {}; }; }"
        GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
            useUpAllBuildQuota "owner"
            void . try $ testHandleCommit commitInfo
            logs <- GH.getAllReportLogs ghState
            logs `shouldContainM` ["You have exhausted your monthly CI quota\n"]

      it "allows builds to be comped" $ do
        let flake = "{ outputs = {self}: { packages = {}; }; }"
        GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
            useUpAllBuildQuota "owner"
            compAllUserBuilds "owner"
            testHandleCommit commitInfo

      it "respects the extra ci minutes if all plan minutes are used" $ do
        let flake = "{ outputs = {self}: { packages = {}; }; }"
        GH.withFakeGithubInterface $ \ghState -> do
          user <- DB.newUser (GhLogin "owner") "owner@owner.com" FreeSubscription True
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
            useUpAllBuildQuota "owner"
            setExtraUsageLimits "owner" (emptyUsageLimits & #ciTime .~ fromHours @Int 3)
            testHandleCommit commitInfo
            build <- fromSingleton <$> DB.getBuilds user
            (build ^. status) `shouldBeM` Just Success

      it "does not log Critical errors when building src derivations" $ do
        let flake =
              cs
                [i|
                  {
                    outputs = _: {
                      packages.x86_64-linux.src = ./.;
                    };
                  }
                |]
        logs <- captureLogs_ $ do
          GH.withFakeGithubInterface $ \ghState -> do
            let commitInfo = defaultCommitInfo & repoPublicity .~ RepoIsPublic True
            GH.withLocalRepo ghState "owner" "repo" identity commitInfo (GH.simpleSetup flake) $ \commitInfo -> do
              testHandleCommit commitInfo
        let critical = filter (\(LogItem severity _ _) -> severity == Critical) logs
        liftIO $ critical `shouldBe` []

      it "correctly calculates outputs" $ do
        let flake =
              cs
                [i|
                  { outputs = {self}: {
                      packages.x86_64-linux = {
                        foo = derivation {
                          name = "foo";
                          builder = "/bin/sh";
                          system = "x86_64-linux";
                          args = [ "-c" ''
                            echo 1
                          ''];
                        };
                      };
                    };
                  }|]
        GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
            useUpAllBuildQuota "owner"
            result <- try $ testHandleCommit commitInfo
            calls <- getMockCalls #s3CacheUploadMock
            liftIO $ map (_1 .~ ()) calls `shouldBe` []
            (result & _Left %~ err) `shouldBeM` Left (EntitlementError "You have exhausted your monthly CI quota")

      it "returns an error if the flake has more than 100 packages" $ do
        let flake =
              cs
                [i|
                  {
                    outputs = {self}:
                      let
                        someDrv = derivation {
                          name = "some-pkg";
                          builder = "/bin/bash";
                          system = "x86_64-linux";
                        };
                        mkPkgs = count:
                          if count == 0 then {}
                          else { "pkg${toString count}" = someDrv; } // mkPkgs (count - 1);
                      in
                        { packages.x86_64-linux = mkPkgs 101; };
                  }
                |]
        GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
            result <- try $ testHandleCommit commitInfo
            (result & _Left %~ err) `shouldBeM` Left (OtherError "Number of packages too large. Maximum is 100, you have 101")

      it "builds more than 100 packages if the entitlements allow it @slow" $ do
        withTestEntitlement "test" (maximumPackagesPerFlake .~ 110) "mock-user" $ do
          let flake =
                cs
                  [i|
                    {
                      outputs = {self}:
                        let
                          someDrv = derivation {
                            name = "some-pkg";
                            builder = "/bin/bash";
                            system = "x86_64-linux";
                          };
                          mkPkgs = count:
                            if count == 0 then {}
                            else { "pkg${toString count}" = someDrv; } // mkPkgs (count - 1);
                        in
                          { packages.x86_64-linux = mkPkgs 110; };
                    }
                  |]
          GH.withFakeGithubInterface $ \ghState -> do
            let commitInfo =
                  defaultCommitInfo
                    & Garnix.Types.reqUser .~ "mock-user"
                    & repoInfo . ghRepoOwner .~ "mock-user"
            GH.withLocalRepo ghState "mock-user" "repo" identity commitInfo (GH.simpleSetup flake) $ \commitInfo -> do
              result <- try $ testHandleCommit commitInfo
              (result & _Left %~ err) `shouldBeM` Right ()

      it "shows the correct limit with custom package limit entitlements" $ do
        withTestEntitlement "test" (maximumPackagesPerFlake .~ 115) "mock-user" $ do
          let flake =
                cs
                  [i|
                    {
                      outputs = {self}:
                        let
                          someDrv = derivation {
                            name = "some-pkg";
                            builder = "/bin/bash";
                            system = "x86_64-linux";
                          };
                          mkPkgs = count:
                            if count == 0 then {}
                            else { "pkg${toString count}" = someDrv; } // mkPkgs (count - 1);
                        in
                          { packages.x86_64-linux = mkPkgs 116; };
                    }
                  |]
          GH.withFakeGithubInterface $ \ghState -> do
            let commitInfo =
                  defaultCommitInfo
                    & Garnix.Types.reqUser .~ "mock-user"
                    & repoInfo . ghRepoOwner .~ "mock-user"
            GH.withLocalRepo ghState "mock-user" "repo" identity commitInfo (GH.simpleSetup flake) $ \commitInfo -> do
              result <- try $ testHandleCommit commitInfo
              (result & _Left %~ err) `shouldBeM` Left (OtherError "Number of packages too large. Maximum is 115, you have 116")

      it "updates github" $ GH.withFakeGithubInterface $ \ghState -> do
        let flake =
              cs
                [i|
                  { outputs = {self}: {
                      packages.x86_64-linux = {
                        stream = derivation {
                          name = "foo";
                          builder = "/bin/sh";
                          system = "x86_64-linux";
                          args = [ "-c" ''
                            echo test log
                          ''];
                        };
                      };
                    };
                  }
                |]
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
          testHandleCommit commitInfo
          let expected = regexing $ compile "\nfoo> test log\n" []
          logs <- GH.getFinalLogs ghState (commitInfo ^. commit) "package stream [x86_64-linux]"
          liftIO $ logs `shouldMatch` expected

      it "updates github periodically @slow" $ GH.withFakeGithubInterface $ \ghState -> do
        let flake =
              cs
                [i|
             { outputs = {self}: {
                 packages.x86_64-linux = {
                   stream = derivation {
                     name = "stream";
                     builder = "/bin/sh";
                     system = "x86_64-linux";
                     args = [ "-c" ''
                       echo 1
                       # We can't use 'sleep' since it's not available in
                       # this environment, and we don't want to depend on
                       # nixpkgs
                       read -rt 21 <> <(:) || :
                       echo 2
                     ''];
                   };
                 };
               };
             }|]
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
          testHandleCommit commitInfo
          let expected = regexing $ compile "stream> 1\n$" [dollar_endonly]
          logs <- GH.getAllReportLogs ghState
          liftIO $ logs `shouldMatchOnce` expected

      let emptyFlake = "{ outputs = {self}: {}; }"
          simpleFlake =
            cs
              [i|
                { outputs = {self}: {
                    packages.x86_64-linux.succeeding = derivation {
                      name = "succeeding";
                      builder = "/bin/sh";
                      system = "x86_64-linux";
                      args = [ "-c" ''
                        echo "succeeding" > $out
                      ''];
                    };
                  };
                }
              |]

      let mockBuildPkg mResult =
            withMock
              #buildPkgMock
              ( \(_, _, _, _, _, _, b) -> do
                  st <- mResult
                  pure $ b & status ?~ st
              )
      context "commits table" $ do
        it "writes and reads commits to and from the db correctly" $ do
          DB.newCommit "owner" "repo" "aaaaaa"
          DB.setCommitStatus "owner" "repo" "aaaaaa" Evaluated
          commit <- DB.getCommit "owner" "repo" "aaaaaa"
          commit
            `shouldBeM` Just
              ( Commit
                  { _commitHash = "aaaaaa",
                    _commitRepoOwner = "owner",
                    _commitRepoName = "repo",
                    _commitStatus = Evaluated,
                    _commitMetaCheck = CheckPending
                  }
              )

        it "sets the commit status to Evaluated when the eval fails" $ GH.withFakeGithubInterface $ \ghState -> do
          let invalidFlake = "{"
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup invalidFlake) $ \commitInfo -> do
            expectErr $ testHandleCommit commitInfo
            commit' <- DB.getCommit "owner" "repo" (commitInfo ^. commit)
            (commit' ^? _Just . status) `shouldBeM` Just Evaluated

        it "sets the commit status to Evaluating during evaluation" $ GH.withFakeGithubInterface $ \ghState -> do
          let invalidFlake = "{"
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup invalidFlake) $ \commitInfo -> do
            withGithubMock
              getRemoteLens
              ( \_ -> do
                  commit' <- DB.getCommit "owner" "repo" (commitInfo ^. commit)
                  (commit' ^? _Just . status) `shouldBeM` Just Evaluating
                  throw $ OtherError "test should stop here"
              )
              $ do
                expectErr $ testHandleCommit commitInfo

        it "sets the commit status to Evaluated after packages are created in the db" $ GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup simpleFlake) $ \commitInfo -> do
            testHandleCommit commitInfo
            commit' <- DB.getCommit "owner" "repo" (commitInfo ^. commit)
            (commit' ^? _Just . status) `shouldBeM` Just Evaluated

        it "sets commit meta-check to successful after build succeeds" $ GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup simpleFlake) $ \commitInfo -> do
            testHandleCommit commitInfo
            commit' <- DB.getCommit "owner" "repo" (commitInfo ^. commit)
            (commit' ^? _Just . metaCheck) `shouldBeM` Just CheckSuccess

        it "sets commit meta-check to failure after build fails" $ GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup simpleFlake) $ \commitInfo -> do
            mockBuildPkg (pure Failure) $ do
              testHandleCommit commitInfo
              commit' <- DB.getCommit "owner" "repo" (commitInfo ^. commit)
              (commit' ^? _Just . metaCheck) `shouldBeM` Just CheckFail

      context "Github meta-check" $ do
        let random :: Int = unsafePerformIO randomIO
            failingFlake =
              cs
                [i|
                  { outputs = {self }: {
                      packages.x86_64-linux.succeeding = derivation {
                        name = "succeeding";
                        builder = "/bin/sh";
                        system = "x86_64-linux";
                        args = [ "-c" ''
                          echo "succeeding #{random}" > $out
                        ''];
                      };
                      packages.x86_64-linux.failing = derivation {
                        name = "failing";
                        builder = "/bin/sh";
                        system = "x86_64-linux";
                        args = [ "-c" ''
                          echo "failing #{random}"
                        ''];
                      };
                    };
                  }
                |]
        it "succeeds with trivial builds" $ GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup emptyFlake) $ \commitInfo -> do
            testHandleCommit commitInfo
          report <- GH.getReports ghState >>= GH.assertSingleRunForReport "All Garnix checks"
          report `GH.reportsShouldBe` [RunReportStatusInProgress, RunReportStatusSuccess]

        it "succeeds with non-trivial builds" $ GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup simpleFlake) $ \commitInfo -> do
            testHandleCommit commitInfo
          report <- GH.getReports ghState >>= GH.assertSingleRunForReport "All Garnix checks"
          report `GH.reportsShouldBe` [RunReportStatusInProgress, RunReportStatusSuccess]

        it "fails when one build fails" $ GH.withFakeGithubInterface $ \ghState -> do
          GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup failingFlake) $ \commitInfo -> do
            testHandleCommit commitInfo
          report <- GH.getReports ghState >>= GH.assertSingleRunForReport "All Garnix checks"
          report `GH.reportsShouldBe` [RunReportStatusInProgress, RunReportStatusFailure]

        context "rerunning builds" $ aroundM_ suppressLogsWhenPassing $ do
          let rerunSingleCheckRun :: PackageName -> CommitInfo -> M ()
              rerunSingleCheckRun p _ = do
                builds <- DB.getBuilds $ User undefined "owner" undefined undefined undefined
                build <-
                  maybe
                    (error $ "Test setup failure. Could not find build " <> cs (show p))
                    pure
                    $ find (\b -> b ^. package == p) builds
                let event =
                      RerunEvent
                        { reqUser = "owner",
                          ghRunId = fromJust $ build ^. githubRunId,
                          installAuth = undefined,
                          token = undefined,
                          repoIsPublic = RepoIsPublic True
                        }
                handleRerun event
          let rerunWholeCheckSuite :: PackageName -> CommitInfo -> M ()
              rerunWholeCheckSuite _ commitInfo = do
                testHandleCommit commitInfo
          let testCases =
                [ ("single check rerun", rerunSingleCheckRun),
                  ("whole check suite rerun", rerunWholeCheckSuite)
                ]
          forM_ testCases $ \(name, rerun) -> do
            context name $ do
              it "updates the meta-check if the single failing build succeeds" $ GH.withFakeGithubInterface $ \ghState -> do
                GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup failingFlake) $ \commitInfo -> do
                  testHandleCommit commitInfo
                  mockBuildPkg (pure Success) $ rerun "failing" commitInfo

                reports <- GH.getReports ghState >>= assertMultipleRunsForReport "All Garnix checks"
                reports
                  `metacheckReportsShouldBe` [ RunReportStatusInProgress,
                                               RunReportStatusFailure,
                                               RunReportStatusInProgress,
                                               RunReportStatusSuccess
                                             ]

              it "goes back to failure if rerunning the only failed flake" $ GH.withFakeGithubInterface $ \ghState -> do
                GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup failingFlake) $ \commitInfo -> do
                  testHandleCommit commitInfo
                  mockBuildPkg (pure Failure) $ rerun "failing" commitInfo

                reports <- GH.getReports ghState >>= assertMultipleRunsForReport "All Garnix checks"
                reports
                  `metacheckReportsShouldBe` [ RunReportStatusInProgress,
                                               RunReportStatusFailure,
                                               RunReportStatusInProgress,
                                               RunReportStatusFailure
                                             ]

              it "handles timed-out builds" $ GH.withFakeGithubInterface $ \ghState -> do
                GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup failingFlake) $ \commitInfo -> do
                  testHandleCommit commitInfo
                  mockBuildPkg (pure Timeout) $ rerun "failing" commitInfo
                  reports <- GH.getReports ghState >>= assertMultipleRunsForReport "All Garnix checks"
                  reports
                    `metacheckReportsShouldBe` [ RunReportStatusInProgress,
                                                 RunReportStatusFailure,
                                                 RunReportStatusInProgress,
                                                 RunReportStatusFailure
                                               ]

              it "handles cancelled builds" $ GH.withFakeGithubInterface $ \ghState -> do
                GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup failingFlake) $ \commitInfo -> do
                  testHandleCommit commitInfo
                  mockBuildPkg (pure Cancelled) $ rerun "failing" commitInfo
                  reports <- GH.getReports ghState >>= assertMultipleRunsForReport "All Garnix checks"
                  reports
                    `metacheckReportsShouldBe` [ RunReportStatusInProgress,
                                                 RunReportStatusFailure,
                                                 RunReportStatusInProgress,
                                                 RunReportStatusFailure
                                               ]

              it "fails meta-check when build errors" $ GH.withFakeGithubInterface $ \ghState -> do
                GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup failingFlake) $ \commitInfo -> do
                  testHandleCommit commitInfo
                  mockBuildPkg (throw $ OtherError "test") $ rerun "failing" commitInfo
                  reports <- GH.getReports ghState >>= assertMultipleRunsForReport "All Garnix checks"
                  reports
                    `metacheckReportsShouldBe` [ RunReportStatusInProgress,
                                                 RunReportStatusFailure,
                                                 RunReportStatusInProgress,
                                                 RunReportStatusFailure
                                               ]

              it "fails meta-check when IO exceptions are thrown" $ GH.withFakeGithubInterface $ \ghState -> do
                GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup failingFlake) $ \commitInfo -> do
                  testHandleCommit commitInfo
                  mockBuildPkg (liftIO $ throwIO $ ErrorCall "test") $ rerun "failing" commitInfo
                  reports <- GH.getReports ghState >>= assertMultipleRunsForReport "All Garnix checks"
                  reports
                    `metacheckReportsShouldBe` [ RunReportStatusInProgress,
                                                 RunReportStatusFailure,
                                                 RunReportStatusInProgress,
                                                 RunReportStatusFailure
                                               ]
          let multiFailFlake :: Int -> Text
              multiFailFlake num =
                cs
                  [i|
                    { outputs = {self }: {
                        packages.x86_64-linux.succeeding = derivation {
                          name = "succeeding";
                          builder = "/bin/sh";
                          system = "x86_64-linux";
                          args = [ "-c" ''
                            echo "succeeding" > $out
                          ''];
                        };
                  |]
                  <> mconcat
                    ( flip fmap [1 .. num] $ \n ->
                        cs
                          [i|
                            packages.x86_64-linux.failing#{n} = derivation {
                              name = "failing#{n}";
                              builder = "/bin/sh";
                              system = "x86_64-linux";
                              args = [ "-c" "echo #{n} #{random}"];
                            };
                          |]
                    )
                  <> cs
                    [i|
                        };
                      }
                    |]

          it "does not update if there are other failing builds" $ GH.withFakeGithubInterface $ \ghState -> do
            GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup $ multiFailFlake 2) $ \commitInfo -> do
              testHandleCommit commitInfo
              mockBuildPkg (pure Success) $ rerunSingleCheckRun "failing1" commitInfo
              reports <- GH.getReports ghState >>= assertMultipleRunsForReport "All Garnix checks"
              reports
                `metacheckReportsShouldBe` [RunReportStatusInProgress, RunReportStatusFailure]

          it "updates if all builds succeed" $ GH.withFakeGithubInterface $ \ghState -> do
            GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup $ multiFailFlake 2) $ \commitInfo -> do
              testHandleCommit commitInfo
              mockBuildPkg (pure Success) $ rerunSingleCheckRun "failing1" commitInfo
              mockBuildPkg (pure Success) $ rerunSingleCheckRun "failing2" commitInfo

              reports <- GH.getReports ghState >>= assertMultipleRunsForReport "All Garnix checks"
              reports
                `metacheckReportsShouldBe` [ RunReportStatusInProgress,
                                             RunReportStatusFailure,
                                             RunReportStatusInProgress,
                                             RunReportStatusSuccess
                                           ]

          it "behaves correctly when dealing with many threads" $ GH.withFakeGithubInterface $ \ghState -> do
            GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup $ multiFailFlake 10) $ \commitInfo -> do
              testHandleCommit commitInfo
              Async.forConcurrently_ [1 .. 10] $ \n ->
                mockBuildPkg (pure Success) $ rerunSingleCheckRun (PackageName $ "failing" <> show @Int n) commitInfo
              reports <- GH.getReports ghState >>= assertMultipleRunsForReport "All Garnix checks"
              reports
                `metacheckReportsShouldBe` [ RunReportStatusInProgress,
                                             RunReportStatusFailure,
                                             RunReportStatusInProgress,
                                             RunReportStatusSuccess
                                           ]

        context "deployments" $ do
          let serverInfo =
                ServerInfo
                  { _serverInfoId = ServerId $ 1 ^. from hashIdInt,
                    _serverInfoHetznerServerId = HetznerServerId 1,
                    _serverInfoIpv4Addr = "<none>",
                    _serverInfoIpv6Addr = "<none>",
                    _serverInfoCreatedAt = error "not set",
                    _serverInfoEndedAt = Nothing,
                    _serverInfoConfigurationBuildId = BuildId $ 1 ^. from hashIdInt,
                    _serverInfoPullRequest = Nothing,
                    _serverInfoReadyAt = Nothing,
                    _serverInfoBuildPersistenceName = Nothing,
                    _serverInfoTier = def,
                    _serverInfoIsPrimary = False
                  }
              succeededDeployment now = serverInfo & createdAt .~ now & readyAt ?~ now
              failedDeployment now = serverInfo & createdAt .~ now & readyAt .~ Nothing

          it "succeeds when deployments succeed" $ GH.withFakeGithubInterface $ \ghState -> do
            now <- liftIO getCurrentTime
            GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup emptyFlake) $ \commitInfo ->
              withMockReturning #executeDeployPlanMock [succeededDeployment now]
                $ void
                $ try (testHandleCommit commitInfo)
            report <- GH.getReports ghState >>= GH.assertSingleRunForReport "All Garnix checks"
            report `GH.reportsShouldBe` [RunReportStatusInProgress, RunReportStatusSuccess]

          it "fails when deployments fail" $ GH.withFakeGithubInterface $ \ghState -> do
            now <- liftIO getCurrentTime
            GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup emptyFlake) $ \commitInfo ->
              withMockReturning #executeDeployPlanMock [succeededDeployment now, failedDeployment now]
                $ void
                $ try (testHandleCommit commitInfo)
            report <- GH.getReports ghState >>= GH.assertSingleRunForReport "All Garnix checks"
            report `GH.reportsShouldBe` [RunReportStatusInProgress, RunReportStatusFailure]

          it "fails when deployments throw monadic errors" $ GH.withFakeGithubInterface $ \ghState -> do
            GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup emptyFlake) $ \commitInfo ->
              withMock #executeDeployPlanMock (const $ throw (OtherError ""))
                $ void
                $ try (testHandleCommit commitInfo)
            report <- GH.getReports ghState >>= GH.assertSingleRunForReport "All Garnix checks"
            report `GH.reportsShouldBe` [RunReportStatusInProgress, RunReportStatusFailure]

      describe "build logs" $ do
        let flake =
              cs
                $ unindent
                  [i|
                    {
                      outputs = { self } : {
                        packages.x86_64-linux.default = derivation {
                          name = "test-package";
                          system = "x86_64-linux";
                          builder = "/bin/sh";
                          args = [
                            "-c"
                            ''
                              echo test-error
                            ''
                          ];
                        };
                      };
                    }
                  |]

            mkBuildInfo user commit =
              CommitInfo
                (user ^. githubLogin)
                (RepoIsPublic True)
                (RepoInfo undefined (GhToken "test-token") "owner" "repo")
                (Just "branch")
                Nothing
                commit

        it "writes logs for package builds to opensearch" $ do
          withMockReturning #storeLogLineMock () $ do
            GH.withFakeGithubInterface $ \ghState -> do
              GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
                user <- testUser
                resolve =<< buildFlake openSearchReporter (mkBuildInfo user (commitInfo ^. commit))
                storedLogLines <- getMockCalls #storeLogLineMock
                liftIO $ map snd storedLogLines `shouldContain` [LogLine (Just "test-package") Nothing "test-error"]

        it "uploads logs over HTTP, including metadata fields" $ do
          var :: MVar [Value] <- liftIO $ newMVar []
          let app request respond = do
                body <- getRequestBodyChunk request
                case Aeson.eitherDecodeStrict' body of
                  Right buildLog -> do
                    modifyMVar_ var $ \prev -> pure $ prev ++ [buildLog]
                    respond
                      $ responseLBS status200 [(hContentType, "text/plain")] ""
                  Left err -> error $ cs err
          liftBaseOp (Warp.testWithApplication (pure app)) $ \port -> do
            withUnmock #storeLogLineMock $ local (#buildLogsReportingPort ?~ port) $ do
              GH.withFakeGithubInterface $ \ghState -> do
                GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (GH.simpleSetup flake) $ \commitInfo -> do
                  user <- testUser
                  resolve =<< buildFlake openSearchReporter (mkBuildInfo user (commitInfo ^. commit))
                  build <- fromSingleton . filter (\p -> p ^. packageType /= TypeOverall) <$> DB.getBuilds user
                  request <- liftIO $ waitFor (fromSeconds @Int 5) $ do
                    requests <- readMVar var <&> filter (\r -> r ^? key "package" == Just "test-package")
                    map (\l -> (l ^? key "package", l ^? key "message")) requests `shouldBe` [(Just "test-package", Just "test-error")]
                    pure $ fromSingleton requests
                  let commitHash = commitInfo ^. commit
                  request
                    `shouldBeM` [aesonQQ|
                                  {
                                    "package": "test-package",
                                    "phase": null,
                                    "message": "test-error",
                                    "buildId": #{build ^. id},
                                    "repoOwner": "owner",
                                    "repoName": "repo",
                                    "branch": "branch",
                                    "commit": #{commitHash},
                                    "requestingUser": #{user ^. githubLogin}
                                  }
                                |]

testHandleCommit :: CommitInfo -> M ()
testHandleCommit commitInfo = do
  let reporter = mkGithubReporter (commitInfo ^. repoInfo) (commitInfo ^. commit) <> openSearchReporter
  resolve =<< handleCommit reporter True commitInfo

useUpAllBuildQuota :: GhRepoOwner -> M ()
useUpAllBuildQuota owner = do
  addDefaultEntitlements owner
  ended <- liftIO getCurrentTime
  let started = subTime (fromHours @Int 2) ended
  _ <- testBuild ((repoUser .~ "owner") . (startTime .~ started) . (endTime ?~ ended))
  hasCiTime <- hasRemainingCiTime owner
  if not hasCiTime then pure () else useUpAllBuildQuota owner

shouldMatchOnce ::
  (HasCallStack) =>
  [Text] ->
  Getting (Endo [Text]) Text Match ->
  IO ()
shouldMatchOnce res regex = do
  let matches = res ^.. traversed . (regex . match)
  case length matches of
    1 -> pure ()
    0 -> do
      expectationFailure
        $ "given regex didn't match:\n\n"
        <> unlines (fmap cs res)
    n -> do
      expectationFailure
        $ "given regex matched more than once ("
        <> cs (show n)
        <> "):\n\n"
        <> unlines (fmap cs res)

shouldMatch ::
  (HasCallStack) =>
  Text ->
  Getting (Endo [Text]) Text Match ->
  IO ()
shouldMatch res regex = do
  let matches = res ^.. regex . match
  case length matches of
    0 -> do
      expectationFailure
        $ "given regex didn't match:\n\n"
        <> cs res
    _ -> pure ()

shouldNotMatch ::
  (HasCallStack) =>
  Text ->
  Getting (Endo [Text]) Text Match ->
  IO ()
shouldNotMatch res regex = do
  let matches = res ^.. regex . match
  case length matches of
    0 -> pure ()
    _ -> do
      expectationFailure
        $ "given regex:\n"
        <> cs res
        <> "\nmatched text:\n "
        <> unlines (fmap cs matches)
        <> "\n when it shouldn't have.\n"

assertMultipleRunsForReport :: (HasCallStack) => Text -> [[(RepoInfo, GhRunReport)]] -> M [[GhRunReport]]
assertMultipleRunsForReport name reports =
  case go (fmap snd <$> reports) of
    [] -> liftIO $ assertFailure $ "could not find expected report '" <> cs name <> "'"
    result -> pure $ init result
  where
    matchesName :: Text -> GhRunReport -> Bool
    matchesName name GhRunReport {..} = _ghRunReportName == name

    go :: [[GhRunReport]] -> [[GhRunReport]]
    go =
      \case
        [] -> pure []
        (r : rs) -> do
          if all (matchesName name) r
            then r : go rs
            else go rs

metacheckReportsShouldBe :: (HasCallStack) => [[GhRunReport]] -> [RunReportStatus] -> M ()
metacheckReportsShouldBe actual expected = mconcat (fmap _ghRunReportStatus <$> actual) `shouldBeM` expected

expectErr :: (HasCallStack) => M () -> M ()
expectErr a =
  try a >>= \case
    Right _ -> liftIO $ expectationFailure "Expected Left, got Right"
    Left _ -> pure ()
