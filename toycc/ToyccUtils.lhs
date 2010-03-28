
 Toy C Compiler - a learning project that should eventually do what
 its name says.
 Copyright (C) 2008 Miron Brezuleanu

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.

 See the COPYING file for the full text of the license.

 -----------------------------------------------------------------------------

 This file contains various general purpose functions and types used
 in Toycc.

> module ToyccUtils (ErrorState,
>                    runErrorState)
> where

> import Control.Monad.State
> import Control.Monad.Error
> import Control.Monad.Error.Class
> import Control.Monad.Identity
> import qualified Data.Map as Map

 Monad transformers are useful, and ready-made combinations of monads
 seem to be useful too.

 This is the type synonim that defines the monad that provides state
 and exceptions. When run, the result is an (Either error value,
 finalState) pair.

> type ErrorState e s a = ErrorT e (StateT s Identity) a

 A function that runs an ErrorState action, and returns the 'value',
 which is an Either instance

> runErrorState :: ErrorState e s a -> s -> (Either e a, s)
> runErrorState errorStateV state0 = runIdentity $ runStateT (runErrorT errorStateV) state0

