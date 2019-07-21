module Shellbit.OptionsSpec (spec) where

import Shellbit.Options (Options(Options), Command(List), Arg(Arg), optArgs,
                            optCommand, optProject, optVersion, options)
import Shellbit.Project (Project(Project))
import Shellbit.Version (Version(Version))
import Options.Applicative (briefDesc, defaultPrefs, execParserPure,
                            getParseResult, info)
import Test.Hspec          (Expectation, HasCallStack, Spec, context, describe,
                            it, shouldBe)


spec :: Spec
spec =
    describe "options" $ do
      let opts = Options
            { optProject = Nothing
            , optVersion = Nothing
            , optCommand = Nothing
            , optArgs    = []
            }

      it "can be empty" $
        parseOpts [] `shouldParse` opts

      context "project" $ do
        it "has long project option" $
          parseOpts ["--project", "abc"]
          `shouldParse`
            opts
              { optProject = Just (Project "abc") }

        it "has short project option" $
          parseOpts ["-p", "abc"]
          `shouldParse`
            opts
              { optProject = Just (Project "abc") }

        it "uses rightmost project if specified multiple times" $
          parseOpts ["-p", "abc", "--project", "def"]
          `shouldParse`
            opts
              { optProject = Just (Project "def") }

        context "version" $ do
          it "has long version option" $
            parseOpts ["--version", "1.2.3"]
            `shouldParse`
              opts
                { optVersion = Just (Version "1.2.3") }

          it "has short version option" $
            parseOpts ["-v", "1.2.3"]
            `shouldParse`
              opts
                { optVersion = Just (Version "1.2.3") }

          it "uses rightmost version if specified multiple times" $
            parseOpts ["--version", "1.2.3", "-v", "4.5.6"]
            `shouldParse`
              opts
                { optVersion = Just (Version "4.5.6") }

        context "list" $ do
          it "has long list flag" $
            parseOpts ["--list"]
            `shouldParse`
              opts
                { optCommand = Just List }

          it "has short list flag" $
            parseOpts ["-l"]
            `shouldParse`
              opts
                { optCommand = Just List }

          it "can be specified multiple times" $
            parseOpts ["--list", "-l", "--list"]
            `shouldParse`
              opts
                { optCommand = Just List }

        context "args" $ do
          it "accepts trailing arguments" $
            parseOpts ["FOO", "BAR"]
            `shouldParse`
              opts
                { optArgs = map Arg ["FOO", "BAR"] }

          it "recognizes arguments intermixed with other options" $
            parseOpts [ "-p", "abc"
                      , "FOO"
                      , "-l"
                      , "BAR"
                      , "-p", "def"
                      , "BAZ" ]
            `shouldParse`
              opts
                { optProject = Just (Project "def")
                , optCommand = Just List
                , optArgs    = map Arg ["FOO", "BAR", "BAZ"]
                }

          it "treats all arguments after -- as trailing args" $
            parseOpts [ "-p", "abc"
                      , "FOO"
                      , "--"
                      , "-l"
                      , "BAR"
                      , "-p", "def"
                      , "BAZ" ]
            `shouldParse`
              opts
                { optProject = Just (Project "abc")
                , optArgs    = map Arg ["FOO", "-l", "BAR", "-p", "def", "BAZ"]
                }
  where
    parseOpts :: [String] -> Maybe Options
    parseOpts = getParseResult
              . execParserPure
                  defaultPrefs
                  (info options briefDesc)

    shouldParse
      :: HasCallStack
      => Maybe Options
      -> Options
      -> Expectation
    shouldParse result expected =
      result `shouldBe` Just expected
