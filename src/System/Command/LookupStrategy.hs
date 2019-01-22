-- |
-- Module:      System.Command.LookupStrategy
-- Description: Strategies for figuring up what command should be used.
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Strategies for figuring up what command should be used.
module System.Command.LookupStrategy
    (
    -- * Lookup Strategy
      LookupStrategy
    , LookupCommand(..)

    -- ** Lookup Strategy Evaluation
    , evalLookupStrategy
    , lookupCommand
    , lookupVariables
    , findExecutable

    -- * Utilities
    , IgnoreOnDumbTerminal(..)
    )
  where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Foldable (asum)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (listToMaybe)
import GHC.Generics (Generic)
import qualified System.Environment as IO (lookupEnv)

import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Dhall (Inject, Interpret)
import qualified System.Directory as Directory
    ( findExecutable
    , findExecutablesInDirectories
    )


-- | Strategy for looking up a command is a non empty list of smaller
-- strategies called 'LookupCommand' that are evaluated in order untill command
-- is either found or the wohole strategy fails.
type LookupStrategy config command = NonEmpty (LookupCommand config command)

data LookupCommand config command
    -- | Try specified command, if it's not present not present then this
    -- strategy is skipped, unless it's a last strategy in which case we failed
    -- to find a suitable command.
    = Command
        { command :: command
        -- ^ Command to use if it's present on the system.
        , config :: config
        -- ^ Custom parameters that are further used to modify the lookup
        -- strategy.
        }

    -- | Look for these environment variables in specified order.
    | EnvironmentVariables
        { variables :: [Text]
        -- ^ List of environment variable names in order as they will be tried.
        , config :: config
        -- ^ Skip these environment variables on dumb terminal.
        }
  deriving stock (Foldable, Functor, Generic, Show, Traversable)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

instance Bifunctor LookupCommand where
    bimap
        :: (config -> config')
        -> (command -> command')
        -> LookupCommand config command
        -> LookupCommand config' command'
    bimap f g = \case
        Command{command, config} -> Command
            { command = g command
            , config = f config
            }
        EnvironmentVariables{variables, config} -> EnvironmentVariables
            { variables
            , config = f config
            }

evalLookupStrategy
    :: Monad m
    => (LookupCommand config command -> MaybeT m command)
    -> LookupStrategy config command
    -> m (Maybe command)
evalLookupStrategy f = runMaybeT . asum . fmap f

lookupCommand
    :: MonadIO io
    => Maybe [FilePath]
    -> (command -> String)
    -> command
    -> MaybeT io command
lookupCommand possiblyPath getCommandName cmd =
    cmd <$ findExecutable possiblyPath (getCommandName cmd)

-- | Lookup variables in order they are presented.  Value of the first one
-- which refers to an existing executable is returned.
lookupVariables :: MonadIO io => Maybe [FilePath] -> [Text] -> MaybeT io String
lookupVariables possiblyPath = asum . map (lookupEnv >=> findExecutable')
  where
    lookupEnv var = MaybeT . liftIO $ IO.lookupEnv (Text.unpack var)

    -- We want to return content of the variable, not full path to an
    -- executable.
    findExecutable' cmd = cmd <$ findExecutable possiblyPath cmd

findExecutable :: MonadIO io => Maybe [FilePath] -> String -> MaybeT io FilePath
findExecutable possiblyPath cmd = MaybeT (liftIO findExe)
  where
    -- Use 'PATH' or provided directories to search for 'cmd', and check that
    -- it's executable.
    findExe = (cmd <$) <$> case possiblyPath of
        Nothing ->
            Directory.findExecutable cmd

        Just path ->
            listToMaybe <$> Directory.findExecutablesInDirectories path cmd

-- | This is useful for strategies that need to use terminal capabilities when
-- evaluating 'LookupStrategy'.
newtype IgnoreOnDumbTerminal = IgnoreOnDumbTerminal
    { ignoreOnDumbTerminal :: Bool
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)
