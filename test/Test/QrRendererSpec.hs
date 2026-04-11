{-# LANGUAGE OverloadedStrings #-}

module Test.QrRendererSpec where

import           RIO
import qualified RIO.Text as T

import           Test.Hspec          (Spec, describe, it, shouldBe, shouldSatisfy)

import           Todo.QrRenderer     (buildDeeplink, renderQrInvite)

-- | Required for auto-discovery
spec :: Spec
spec = describe "QrRenderer" $ do

  describe "buildDeeplink" $ do
    it "percent-encodes the server URL" $ do
      -- Regression guard: the URL scheme delimiter (://) and query
      -- params (?, &, =) must not leak through raw, or the Android
      -- deeplink intent filter silently fails to match on the phone.
      let link = buildDeeplink "https://sync.example.com" "code-123"
      link `shouldBe`
        "todo-sync://register?server=https%3A%2F%2Fsync.example.com&code=code-123"

    it "percent-encodes ampersands inside the server URL" $ do
      -- If the user puts the server URL behind a query-string-proxied
      -- path (unusual but possible), we still have to escape the
      -- ampersand so it doesn't terminate the 'server' param early.
      let link = buildDeeplink "https://sync.example.com/path?foo=bar&baz=qux" "code-1"
      -- The & in the server URL must become %26; the & between
      -- server= and code= stays as a literal.
      T.isInfixOf "server=https%3A%2F%2Fsync.example.com%2Fpath%3Ffoo%3Dbar%26baz%3Dqux" link
        `shouldBe` True
      T.isInfixOf "&code=code-1" link `shouldBe` True

    it "uses the todo-sync://register scheme and host" $ do
      let link = buildDeeplink "https://foo" "c"
      T.isPrefixOf "todo-sync://register?" link `shouldBe` True

    it "handles an empty invite code without crashing" $ do
      -- Not a real code, just a defensive guard against edge cases.
      let link = buildDeeplink "https://foo" ""
      T.isPrefixOf "todo-sync://register?server=" link `shouldBe` True
      T.isInfixOf "code=" link `shouldBe` True

  describe "renderQrInvite" $ do
    let sampleDeeplink = buildDeeplink "https://sync.example.com" "abc-123"

    it "returns Right for a typical todo-sync deeplink" $
      renderQrInvite sampleDeeplink `shouldSatisfy` isRight

    it "renders output containing half-block glyphs" $ do
      -- The renderer uses U+2580/U+2584/U+2588 plus space. Any of
      -- those should appear in the output since a QR with real
      -- data can never be all-white.
      let out = either (const "") id (renderQrInvite sampleDeeplink)
      out `shouldSatisfy` T.any (`elem` ("\x2580\x2584\x2588" :: String))

    it "always ends with a trailing newline" $ do
      -- Output always ends with a newline so stdout printing
      -- leaves the cursor on a fresh line.
      let out = either (const "") id (renderQrInvite sampleDeeplink)
      T.takeEnd 1 out `shouldBe` "\n"

    it "is deterministic for identical input" $ do
      -- QR encoding is deterministic for fixed mask + error level,
      -- so two calls with the same input must produce byte-identical
      -- output. This catches accidental randomness creeping in.
      let deeplink = "todo-sync://register?server=foo&code=bar"
      renderQrInvite deeplink `shouldBe` renderQrInvite deeplink

    it "includes a row of quiet-zone padding at the top" $ do
      -- QR readers need a 4-module quiet border; the renderer pads
      -- with blank rows on top, which must be present in the output.
      -- The first line should contain no block characters — it's
      -- all quiet zone.
      let out = either (const "") id (renderQrInvite "todo-sync://register?server=foo&code=bar")
          firstLine = T.takeWhile (/= '\n') out
      firstLine `shouldSatisfy` T.all (`notElem` ("\x2580\x2584\x2588" :: String))
