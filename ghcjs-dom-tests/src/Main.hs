{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Smoke tests for the generated ghcjs-dom bindings.  Each check targets
-- something the webref regeneration changed: typed events (click is a
-- PointerEvent), nullable results (including nullable numbers), optional
-- arguments passed as undefined, variadic arguments, unions, Promises,
-- enums, callbacks and Int53/Word53.
--
-- Results go into <pre id="results">: one "PASS name" / "FAIL name: why"
-- line per check, then "DONE passed/total".
module Main (main) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryPutMVar, tryTakeMVar)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, void)
import Control.Monad.Catch (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T

import GHCJS.DOM (currentDocumentUnchecked, currentWindowUnchecked)
import GHCJS.DOM.Types hiding (Text, Event)
import GHCJS.DOM.Enums
import qualified GHCJS.DOM.Blob as Blob
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.EventTarget as EventTarget
import GHCJS.DOM.EventM (on, mouseClientXY, event)
import qualified GHCJS.DOM.AnimationFrameProvider as AnimationFrameProvider
import qualified GHCJS.DOM.FrameRequestCallback as FrameRequestCallback
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import qualified GHCJS.DOM.HTMLInputElement as Input
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.NonElementParentNode as NonElementParentNode
import qualified GHCJS.DOM.ParentNode as ParentNode
import qualified GHCJS.DOM.MouseEventInit as MouseEventInit
import qualified GHCJS.DOM.PointerEvent as PointerEvent
import qualified GHCJS.DOM.PointerEventInit as PointerEventInit
import qualified GHCJS.DOM.RTCIceServer as RTCIceServer
import qualified GHCJS.DOM.ResizeObserver as ResizeObserver
import qualified GHCJS.DOM.ResizeObserverCallback as ResizeObserverCallback
import qualified GHCJS.DOM.XMLHttpRequest as XHR
import qualified JavaScript.Object as Object

import Existence (existenceCheck)

#if !defined(javascript_HOST_ARCH)
import Language.Javascript.JSaddle.Warp (run)
import System.IO (BufferMode (..), hSetBuffering, stdout)
#endif

main :: IO ()
#if defined(javascript_HOST_ARCH)
main = tests
#else
main = hSetBuffering stdout LineBuffering >> run 3709 tests
#endif

type Check = (Text, JSM (Either Text ()))

-- | Poll an MVar every 20ms, @n@ times.
waitFor :: Int -> MVar a -> JSM (Maybe a)
waitFor 0 _ = pure Nothing
waitFor n v = liftIO (tryTakeMVar v) >>= maybe (liftIO (threadDelay 20000) >> waitFor (n - 1) v) (pure . Just)

expect :: (Eq a, Show a) => a -> a -> Either Text ()
expect wanted got
  | wanted == got = Right ()
  | otherwise = Left ("expected " <> T.pack (show wanted) <> ", got " <> T.pack (show got))

tests :: JSM ()
tests = do
  doc <- currentDocumentUnchecked
  body <- Document.getBodyUnchecked doc
  results <- Document.createElement doc ("pre" :: Text)
  Element.setAttribute results ("id" :: Text) ("results" :: Text)
  Node.appendChild_ body results
  passed <- liftIO (newIORef (0 :: Int))
  let report line = do
        -- also to stdout (the jsaddle-warp server; the console on the JS backend)
        liftIO (putStrLn (T.unpack line))
        t <- Document.createTextNode doc (line <> "\n")
        Node.appendChild_ results t
  forM_ (checks report doc body) $ \(name, check) -> do
    r <- try check
    case r of
      Right (Right ()) -> liftIO (modifyIORef passed (+ 1)) >> report ("PASS " <> name)
      Right (Left why) -> report ("FAIL " <> name <> ": " <> why)
      Left (e :: SomeException) -> report ("FAIL " <> name <> ": exception " <> T.pack (show e))
  n <- liftIO (readIORef passed)
  report ("DONE " <> T.pack (show n) <> "/" <> T.pack (show (length (checks report doc body))))

checks :: (Text -> JSM ()) -> Document -> HTMLElement -> [Check]
checks report doc body =
  [ ("attributes", do
      el <- Document.createElement doc ("div" :: Text)
      Element.setAttribute el ("data-x" :: Text) ("42" :: Text)
      a <- Element.getAttribute el ("data-x" :: Text)
      missing <- Element.getAttribute el ("data-missing" :: Text)
      pure (expect (Just "42" :: Maybe Text, Nothing :: Maybe Text) (a, missing)))

  , ("nullable object result", do
      r <- NonElementParentNode.getElementById doc ("no-such-id" :: Text)
      pure (if isNothing r then Right () else Left "expected Nothing"))

  , ("nullable number result is Nothing for null", do
      -- selectionStart is null for a checkbox (used to come back as Just 0)
      input <- uncheckedCastTo HTMLInputElement <$> Document.createElement doc ("input" :: Text)
      Input.setType input ("checkbox" :: Text)
      s <- Input.getSelectionStart input
      pure (expect Nothing s))

  , ("variadic append and querySelector", do
      el <- Document.createElement doc ("div" :: Text)
      a <- Document.createElement doc ("span" :: Text)
      b <- Document.createElement doc ("span" :: Text)
      ParentNode.append el [a, b]
      ParentNode.append el ["text" :: Text]
      n <- ParentNode.getChildElementCount el
      q <- ParentNode.querySelector el ("span" :: Text)
      pure (expect (2, True) (n, maybe False (const True) q)))

  , ("optional argument omitted (cloneNode)", do
      el <- Document.createElement doc ("div" :: Text)
      c <- Document.createElement doc ("p" :: Text)
      Node.appendChild_ el c
      shallow <- Node.cloneNode el Nothing
      deep <- Node.cloneNode el (Just True)
      ns <- Node.hasChildNodes shallow
      nd <- Node.hasChildNodes deep
      pure (expect (False, True) (ns, nd)))

  , ("Blob: union BlobPart, Word53 size, slice with Int53, Promise text", do
      blob <- Blob.newBlob (Just ["hello" :: Text]) (Nothing :: Maybe BlobPropertyBag)
      size <- Blob.getSize blob
      part <- Blob.slice blob (Just (int53 1)) Nothing (Nothing :: Maybe Text)
      t <- Blob.text part
      pure (expect (5 :: Integer, "ello" :: Text) (fromWord53 size, t)))

  , ("enum round trip", do
      xhr <- XHR.newXMLHttpRequest
      XHR.setResponseType xhr XMLHttpRequestResponseTypeArraybuffer
      r <- XHR.getResponseType xhr
      st <- Document.getReadyState doc
      pure (expect (XMLHttpRequestResponseTypeArraybuffer, True) (r, st `elem` [DocumentReadyStateInteractive, DocumentReadyStateComplete])))

  , ("click is a PointerEvent with MouseEvent fields", do
      el <- uncheckedCastTo HTMLButtonElement <$> Document.createElement doc ("button" :: Text)
      Node.appendChild_ body el
      got <- liftIO newEmptyMVar
      release <- on el Events.click $ do
        e <- event
        xy <- mouseClientXY
        pid <- PointerEvent.getPointerId e
        liftIO (putMVar got (xy, pid))
      -- a dictionary builder, with members inherited from MouseEventInit
      dict <- PointerEventInit.newPointerEventInit
      MouseEventInit.setClientX dict 12
      MouseEventInit.setClientY dict 34
      PointerEventInit.setPointerId dict 7
      ev <- PointerEvent.newPointerEvent ("click" :: Text) (Just dict)
      void (EventTarget.dispatchEvent el ev)
      -- the handler may complete on another thread (jsaddle-warp), so wait
      r <- waitFor 100 got
      release
      pure (expect (Just ((12, 34), 7)) r))

  , ("dictionary builders: getters, Maybe for unset members, required members", do
      dict <- PointerEventInit.newPointerEventInit
      PointerEventInit.setPointerId dict 7
      MouseEventInit.setClientX dict 12
      pid <- PointerEventInit.getPointerId dict
      x <- MouseEventInit.getClientX dict
      pressure <- PointerEventInit.getPressure dict
      server <- RTCIceServer.newRTCIceServer ("stun:example.invalid" :: JSString)
      urls <- RTCIceServer.getUrls server >>= toJSVal >>= fromJSVal
      pure (expect (Just 7, Just 12, Nothing, Just ("stun:example.invalid" :: Text)) (pid, x, pressure, urls)))

  , ("ToJSVal / FromJSVal Object (null and undefined are Nothing)", do
      o <- Object.create
      v <- toJSVal o
      back <- fromJSVal v :: JSM (Maybe Object.Object)
      n <- toJSVal (Nothing :: Maybe Text) >>= fromJSVal :: JSM (Maybe Object.Object)
      pure (expect (True, True) (maybe False (const True) back, isNothing n)))

  , ("element arguments: Node.contains (Just el), ResizeObserver.observe", do
      -- leksah's pattern; on the JS backend with ghcjs-dom -jsffi the
      -- Element marshalling used to diverge here
      outer <- Document.createElement doc ("div" :: Text)
      inner <- Document.createElement doc ("span" :: Text)
      Node.appendChild_ outer inner
      Node.appendChild_ body outer
      yes <- Node.contains outer (Just inner)
      no <- Node.contains inner (Just outer)
      none <- Node.contains outer (Nothing :: Maybe Node)
      fired <- liftIO newEmptyMVar
      cb <- ResizeObserverCallback.newResizeObserverCallback $ \entries _ ->
        liftIO (void (tryPutMVar fired (length entries)))
      ro <- ResizeObserver.newResizeObserver cb
      ResizeObserver.observe ro outer Nothing
      r <- waitFor 100 fired
      ResizeObserver.disconnect ro
      pure (expect (True, False, False, Just 1) (yes, no, none, r)))

  , ("callback (requestAnimationFrame)", do
      win <- currentWindowUnchecked
      done <- liftIO newEmptyMVar
      cb <- FrameRequestCallback.newFrameRequestCallback $ \t -> liftIO (putMVar done t)
      void (AnimationFrameProvider.requestAnimationFrame win cb)
      -- wait up to ~2s (headless Chrome may throttle frames)
      t <- waitFor 100 done
      pure (if maybe False (> 0) t then Right () else Left ("no frame callback: " <> T.pack (show t))))

  , ("existence of every binding (vs browser-compat-data)", existenceCheck report)
  ]
