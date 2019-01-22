-- |
-- Module:      System.Editor
-- Description: Strategies for finding suitable editor command and executing it.
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- Strategies for finding suitable editor command and executing it.
module System.Editor
    (
    -- * Lookup Strategy
    --
    -- | Command lookup strategy specialised for editors.  See also
    -- "System.Command.LookupStrategy" module.
      EditorLookupStrategy
    , stdEditorLookupStrategy

    -- * Editor Command
    , EditorCommand(..)
    , simpleEditorCommand
    , getEditorCommand

    -- * Editor
    , File(..)
    , Editor
    , editor
    , createEditorProcess

    -- ** Spawn Editor
--  , editAsFile
    , editFileAndWait
    , execEditorCommand
    , runEditorCommand
    , runEditorCommandAndWait
    )
  where

import Control.Monad (guard)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.String (fromString)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Exit (ExitCode)

import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Dhall (Inject, Interpret)
import System.Posix.Process (executeFile)
import System.Process
    ( CreateProcess(delegate_ctlc)
    , ProcessHandle
    , createProcess_
    , proc
    , waitForProcess
    , withCreateProcess
    )

import qualified System.Command.LookupStrategy as Command


-- {{{ Lookup Strategy --------------------------------------------------------

type EditorLookupStrategy command =
    Command.LookupStrategy Command.IgnoreOnDumbTerminal command

-- | Try @VISUAL@ environment variable first then @EDITOR@.  If on a dumb
-- terminal then skip @VISUAL@.  Defaults to provided @command@ if neither
-- variable is available or if both faild to provide a command that is
-- available on the system.
stdEditorLookupStrategy :: command -> EditorLookupStrategy command
stdEditorLookupStrategy command =
    Command.EnvironmentVariables
        { variables = ["VISUAL"]
        , config = Command.IgnoreOnDumbTerminal
            { ignoreOnDumbTerminal = True
            }
        }
    :|  [ Command.EnvironmentVariables
            { variables = ["EDITOR"]
            , config = Command.IgnoreOnDumbTerminal
                { ignoreOnDumbTerminal = False
                }
            }
        , Command.Command
            { command
            , config = Command.IgnoreOnDumbTerminal
                { ignoreOnDumbTerminal = False
                }
            }
        ]

-- }}} Lookup Strategy --------------------------------------------------------

-- {{{ Editor Command ---------------------------------------------------------

data EditorCommand = EditorCommand
    { command :: Text
    -- ^ Editor command or full path to an executable.
    , arguments :: [Text]
    -- ^ Arguments passed to editor 'command'.
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

-- | Smart constructor for 'EditorCommand' that sets 'arguments' to empty list.
simpleEditorCommand :: Text -> EditorCommand
simpleEditorCommand command = EditorCommand
    { command
    , arguments = []
    }

-- | Figure out what editor to use for text files.
getEditorCommand
    :: Maybe [FilePath]
    -- ^ Use alternative search path or use @$PATH@?
    -> EditorLookupStrategy command
    -> (command -> EditorCommand)
    -- ^ Interpret command as 'EditorCommand' before checking if it's a valid
    -- executable.
    -> IO Bool
    -- ^ Check if terminal is dumb.
    --
    -- * 'True' - Terminal is dumb and @VISUAL@ environment variable will be
    --   ignored.
    -- * 'False' - Terminal is not dumb and @VISUAL@ environment variable will
    --   be respected.
    -> IO (Maybe EditorCommand)
    -- ^ Returns:
    --
    -- * 'Nothing' if no suitable editor command was found.
    -- * 'Just' if an editor command was found while evaluating
    --   'EditorLookupStrategy'.
getEditorCommand searchPath strategy interpretCommand isTerminalDumb = do
    dumbTerminal <- isTerminalDumb
    flip Command.evalLookupStrategy strategy' \case
        Command.Command
            { command
            , config = Command.IgnoreOnDumbTerminal{ignoreOnDumbTerminal}
            } -> do
                guard (not (ignoreOnDumbTerminal && dumbTerminal))
                Command.lookupCommand searchPath
                    (\EditorCommand{command = c} -> Text.unpack c) command

        Command.EnvironmentVariables
            { variables
            , config = Command.IgnoreOnDumbTerminal{ignoreOnDumbTerminal}
            } -> do
                guard (not (ignoreOnDumbTerminal && dumbTerminal))
                guard (not (null variables))
                var <- Command.lookupVariables searchPath variables
                pure (simpleEditorCommand (fromString var))
  where
    strategy' = fmap interpretCommand <$> strategy

-- }}} Editor Command ---------------------------------------------------------

-- {{{ Editor -----------------------------------------------------------------

-- | Represents file to open on a specific line.
data File = File
    { file :: Text
    -- ^ Path to a file.
    , line :: Natural
    -- ^ Line on which we want to open the file.  Zero is interpreted as don't
    -- pass any line number to editor.  This is useful for e.g. opening file on
    -- last edited line, if supported by editor.
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

type Editor = Maybe File -> EditorCommand

-- | Smart constructor for 'Editor' that ignores line number and only appends
-- file path to the 'arguments' list.
editor :: EditorCommand -> Editor
editor command@EditorCommand{arguments} = \possiblyFile -> command
    { arguments =
        maybe arguments (\File{file} -> arguments <> [file]) possiblyFile
    }

-- | Smart constructor for 'CreateProcess' that can be used to spawn editor
-- using functions from @process@ package ("System.Process").
createEditorProcess :: Editor -> Maybe File -> CreateProcess
createEditorProcess mkEditorCommand file =
    (proc (Text.unpack command) (Text.unpack <$> arguments))
        { delegate_ctlc = True
        }
  where
    EditorCommand{command, arguments} = mkEditorCommand file

-- }}} Editor -----------------------------------------------------------------

-- {{{ Spawn Editor -----------------------------------------------------------

-- | Execute editor in a separate thread and wait for it to finish.
runEditorCommandAndWait :: Editor -> Maybe File -> IO ExitCode
runEditorCommandAndWait mkEditorCommand file =
    withCreateProcess process \_ _ _ ->
        waitForProcess
  where
    process = createEditorProcess mkEditorCommand file

-- | Spawn editor command in a separate process.  Function doesn't wait for it
-- to finish, instead it returns 'ProcessHandle'.
runEditorCommand :: Editor -> Maybe File -> IO ProcessHandle
runEditorCommand mkEditorCommand file = do
    (_, _, _, h) <- createProcess_ "runEditorCommand" process
    pure h
  where
    process = createEditorProcess mkEditorCommand file

-- | Execute editor process via @'executeFile'@ call.  Program calling it won't
-- get control back if the editor is executed successfully.
execEditorCommand :: Editor -> Maybe File -> IO a
execEditorCommand mkEditorCommand file =
    executeFile (Text.unpack command) True (Text.unpack <$> arguments) Nothing
  where
    EditorCommand{command, arguments} = mkEditorCommand file

-- | Simplified version of 'runEditorCommandAndWait' where 'File' is mandatory.
editFileAndWait :: Editor -> File -> IO ExitCode
editFileAndWait f file = runEditorCommandAndWait f (Just file)

{- TODO:
editAsFile
    :: (a -> IO File)
    -- ^ Turn value of type @a@ into a 'File' that can be edited.
    -> (File -> IO b)
    -- ^ Turn the edited value back into a Haskell value, this time of type @b@.
    -> Editor
    -> a
    -> IO b
editAsFile = editAsFile
-}

-- }}} Spawn Editor -----------------------------------------------------------
