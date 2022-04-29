module Tests.Runtime.Interaction (tests) where

import Control.Concurrent.Classy
import qualified Data.Map.Strict as Map
import Glow.Prelude
import Glow.Runtime.Interaction
import Glow.Runtime.Interaction.STM
import Test.Hspec

tests :: Spec
tests =
  describe "Glow.Runtime.Interaction" $ do
    describe "STM" $ do
      let setup = atomically $ do
            srv <- newSTMServer
            h1 <- toHandle <$> newSTMHandle srv
            h2 <- toHandle <$> newSTMHandle srv
            pure (toConsensusServer srv, h1, h2)
          sendInt h val =
            submit
              h
              Message
                { messageData = val,
                  messageAssetTransfers = Map.empty
                }
          r `dataShouldBe` val = messageData (mwpMessage r) `shouldBe` val
      describe "newHandle" $ do
        it "should return different participants each time it's called." $ do
          (_srv, h1, h2) <- setup
          myParticipantId h1 `shouldNotBe` myParticipantId h2
      describe "submit" $ do
        it "should deliver messages to the server, in order" $ do
          (srv, h1, h2) <- setup
          sendInt h1 0
          sendInt h1 1
          sendInt h2 2
          r0 <- receive srv
          r1 <- receive srv
          r2 <- receive srv
          r0 `dataShouldBe` 0
          r1 `dataShouldBe` 1
          r2 `dataShouldBe` 2
          -- make sure the participants are right:
          mwpParticipant r0 `shouldBe` myParticipantId h1
          mwpParticipant r1 `shouldBe` myParticipantId h1
          mwpParticipant r2 `shouldBe` myParticipantId h2
      describe "emit" $ do
        it "should broadcast messages to participants, in order." $ do
          (srv, h1, h2) <- setup
          let mkMwp h v =
                MessageWithParticipant
                  { mwpParticipant = myParticipantId h,
                    mwpMessage =
                      Message
                        { messageData = v,
                          messageAssetTransfers = Map.empty
                        }
                  }
              mwp0 = mkMwp h1 0
              mwp1 = mkMwp h1 1
              mwp2 = mkMwp h1 2
          emit srv mwp0
          emit srv mwp1
          emit srv mwp2
          let checkReceipt h = do
                r0 <- listenNext h
                r1 <- listenNext h
                r2 <- listenNext h
                r0 `shouldBe` mwp0
                r1 `shouldBe` mwp1
                r2 `shouldBe` mwp2
          checkReceipt h1
          checkReceipt h2
