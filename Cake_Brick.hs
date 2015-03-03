module Cake_Brick where

import Development.Cake3
import Development.Cake3.Ext.UrWeb as UW
import qualified Cake_Bootstrap as Bootstrap hiding(main)
import qualified Cake_Prelude as Prelude hiding(main)
import qualified Cake_MonadPack as MonadPack hiding(main)
import qualified Cake_Soup as Soup hiding(main)
import Cake_Brick_P

(app,db) = uwapp_postgres (file "Brick.urp") $ do
  allow mime "text/javascript";
  allow mime "text/css";
  allow mime "image/jpeg";
  allow mime "image/png";
  allow mime "image/gif";
  allow mime "application/octet-stream";
  library MonadPack.lib
  library Prelude.lib
  library Bootstrap.lib
  library Soup.lib
  ur (sys "list")
  ur (sys "option")
  ur (sys "string")
  ur (sys "char")
  ur (file "Brick.ur")

main = writeDefaultMakefiles $ do

  rule $ do
    phony "dropdb"
    depend db

  rule $ do
    phony "all"
    depend app

