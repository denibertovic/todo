{-# LANGUAGE OverloadedStrings #-}

-- | Shared onboarding-helper utilities: building a
-- @todo-sync://register@ deeplink from a server URL + invite code,
-- and rendering that deeplink as a scannable QR code in the
-- terminal.
--
-- Both the @todo-sync-server generate-invite-code@ admin command
-- (used for the one-time bootstrap of the very first device) and
-- the @todo sync invite@ CLI subcommand (used for every subsequent
-- device) re-use these helpers so the onboarding UX looks
-- identical regardless of where the invite is minted.
--
-- The QR renderer uses Unicode half-block characters (U+2580,
-- U+2584, U+2588, space) so every terminal row represents two QR
-- modules stacked vertically — that keeps the aspect ratio roughly
-- square and halves the vertical space compared to one-cell-per-
-- module renderers.
module Todo.QrRenderer
  ( -- * Deeplink
    buildDeeplink
    -- * QR
  , renderQrInvite
  ) where

import           RIO
import qualified RIO.Text                         as T

import qualified Data.Text.Encoding               as TE
import qualified Network.HTTP.Types.URI           as URI

import           Codec.QRCode                     (QRImage (..),
                                                   defaultQRCodeOptions,
                                                   encodeText, toMatrix)
import           Codec.QRCode.Data.ErrorLevel     (ErrorLevel (..))
import           Codec.QRCode.Data.TextEncoding   (TextEncoding (..))

-- | Build the @todo-sync://register@ deeplink that the Android app's
-- 'MainActivity' deeplink intent filter consumes. Both the public
-- server URL and the invite code are percent-encoded so arbitrary
-- characters (@?@, @&@, @#@, @\/@) in a public URL behind nginx
-- don't break query parsing on the phone.
buildDeeplink :: Text -> Text -> Text
buildDeeplink serverUrl code =
  "todo-sync://register?server="
    <> urlEncode serverUrl
    <> "&code="
    <> urlEncode code
  where
    urlEncode :: Text -> Text
    urlEncode = TE.decodeUtf8 . URI.urlEncode True . TE.encodeUtf8

-- | Render a text payload as a UTF-8 QR code suitable for printing
-- in a terminal. Uses error level M and an automatic version so
-- short deeplinks render as small as possible.
--
-- Returns 'Left' if the payload could not be encoded (in practice
-- only happens if the text is longer than the max QR capacity —
-- around 2KB for version 40 at error level L, which no
-- todo-sync deeplink will ever hit).
renderQrInvite :: Text -> Either Text Text
renderQrInvite payload =
  case encodeText (defaultQRCodeOptions M) Utf8WithoutECI payload of
    Nothing  -> Left "failed to encode QR code"
    Just img -> Right $ renderImage img

-- | Convert a 'QRImage' to the rendered half-block string. A
-- 4-module quiet-zone is drawn around every side so readers that
-- rely on the border can lock on to the finder patterns.
renderImage :: QRImage -> Text
renderImage img =
  let size      = qrImageSize img
      matrix    = toMatrix True False img :: [[Bool]]
      quiet     = 4
      totalCols = size + 2 * quiet
      -- Pad every row with quiet-zone columns of @False@, and
      -- sandwich the matrix between quiet-zone rows.
      padRow row = replicate quiet False <> row <> replicate quiet False
      emptyRow   = replicate totalCols False
      padded     = replicate quiet emptyRow
                <> map padRow matrix
                <> replicate quiet emptyRow
  in T.intercalate "\n" (renderRowPairs totalCols padded) <> "\n"
  where
    -- Consume two logical rows at a time and emit one terminal row.
    -- If we end on an odd row, pad the bottom with a quiet-zone
    -- (white) row so the last cell is correctly drawn as "top only".
    renderRowPairs :: Int -> [[Bool]] -> [Text]
    renderRowPairs cols (top : bot : rest) = renderPair top bot : renderRowPairs cols rest
    renderRowPairs cols [top]              = [renderPair top (replicate cols False)]
    renderRowPairs _    []                 = []

    renderPair :: [Bool] -> [Bool] -> Text
    renderPair top bot =
      T.pack $ zipWith cell top bot
      where
        cell True  True  = '█'  -- both modules on → full block
        cell True  False = '▀'  -- top half
        cell False True  = '▄'  -- bottom half
        cell False False = ' '  -- neither
