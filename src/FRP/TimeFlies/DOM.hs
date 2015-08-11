{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module FRP.TimeFlies.DOM where

import           Control.Concurrent.MVar
import           Data.JSString (JSString)
import qualified Data.JSString as JSString (unpack)
import           Data.JSString.Text (textToJSString, textFromJSString)
import qualified Data.Text as T
import           FRP.TimeFlies
import           GHCJS.Foreign.Callback
import           GHCJS.Foreign.QQ
import           GHCJS.VDOM
import           GHCJS.VDOM.Attribute (Attribute)
import           GHCJS.VDOM.Element (custom, text)
import           GHCJS.VDOM.Event
import           GHCJS.VDOM.Render
import           Prelude hiding (filter)
import           System.IO (fixIO)


-- | Lifted kind for position of a widget in our DOM representation.
data WidgetPath = WidgetStop | WidgetLeft WidgetPath | WidgetRight WidgetPath

type Widget (path :: WidgetPath) svIn svOut = (SVEvent (WidgetSum path GnatEvent) :^: svIn) :~> (SVSignal (WidgetProd path GnatDOM) :^: svOut)

type PreConcatWidget (path :: WidgetPath) svIn svOut =   ((SVEvent (WidgetSum (WidgetLeft path) GnatEvent) :^: SVEvent (WidgetSum (WidgetRight path) GnatEvent)) :^: svIn)
                                                     :~> ((SVSignal (WidgetProd (WidgetLeft path) GnatDOM) :^: SVSignal (WidgetProd (WidgetRight path) GnatDOM)) :^: svOut)

-- | Routing by widget paths
data WidgetSum (path :: WidgetPath) a where
  WidgetSumStop :: a -> WidgetSum path a
  WidgetSumLeft :: WidgetSum (WidgetLeft path) a -> WidgetSum path a
  WidgetSumRight :: WidgetSum (WidgetRight path) a -> WidgetSum path a

class RouteWidget (path :: WidgetPath) where
  routeWidget :: WidgetSum path a -> WidgetSum WidgetStop a

instance RouteWidget WidgetStop where
  routeWidget = id 

instance (RouteWidget path) => RouteWidget (WidgetLeft path) where
  routeWidget = routeWidget . WidgetSumLeft 

instance (RouteWidget path) => RouteWidget (WidgetRight path) where
  routeWidget = routeWidget . WidgetSumRight

-- | Combining by widget paths
data WidgetProd (path :: WidgetPath) a where
  WidgetProdUnit :: a -> WidgetProd path a
  WidgetProdConcat :: WidgetProd (WidgetLeft path) a -> WidgetProd (WidgetRight path) a -> WidgetProd path a
  WidgetProdParentChild :: a -> WidgetProd (WidgetRight path) a -> WidgetProd path a

-- | Two signal functions combined so that the input/output of the left is the left input/output
-- (mutatis mutandis right)
(***) :: svInLeft :~> svOutLeft -> svInRight :~> svOutRight -> (svInLeft :^: svInRight) :~> (svOutLeft :^: svOutRight)
left *** right = first left >>> second right

-- | Run two signal functions on a common input
(&&&) :: svIn :~> svOutLeft -> svIn :~> svOutRight -> svIn :~> (svOutLeft :^: svOutRight)
left &&& right = copy >>> (left *** right)

-- | Concatenate two widgets so that one follows the other in the DOM
concatWidgets :: PreConcatWidget path svIn svOut
              -> Widget path svIn svOut
concatWidgets sv =
  let inputTransformer = 
        first $ filter widgetSumLeft &&& filter widgetSumRight
      outputTransformer = first $ combineSignals $ uncurry WidgetProdConcat
  in inputTransformer >>> sv >>> outputTransformer
  where
    widgetSumLeft :: WidgetSum path a -> Maybe (WidgetSum (WidgetLeft path) a)
    widgetSumLeft (WidgetSumLeft sum) = Just sum
    widgetSumLeft _ = Nothing

    widgetSumRight :: WidgetSum path a -> Maybe (WidgetSum (WidgetRight path) a)
    widgetSumRight (WidgetSumRight sum) = Just sum
    widgetSumRight _ = Nothing

-- | Make the left widget the parent of the right widget in the DOM
containWidget :: (forall c . (Children c) => [Attribute] -> c -> VNode)
              -> Widget (WidgetRight path) svIn svOut
              -> Widget path svIn (SVEvent GnatEvent :^: svOut)
containWidget parentF sv =
  let inputTransformer = copy >>>
                         second (second ignore >>> cancelRight >>> filter widgetSumStop) >>>
                         first (first $ filter widgetSumRight)
      middle = sv *** (uncancelLeft >>> (first $ constant $ Element parentF))
  in inputTransformer >>> middle >>> (first swap >>> associate >>> second unassociate >>> swap >>> associate >>> first (combineSignals $ uncurry $ flip WidgetProdParentChild))
  where
    widgetSumRight :: WidgetSum path a -> Maybe (WidgetSum (WidgetRight path) a)
    widgetSumRight (WidgetSumRight sum) = Just sum
    widgetSumRight _ = Nothing

    widgetSumStop :: WidgetSum path a -> Maybe a
    widgetSumStop (WidgetSumStop x) = Just x
    widgetSumStop _ = Nothing

-- | Gnats representation of DOM
data GnatDOM
  = Element (forall c. (Children c) => [Attribute] -> c -> VNode)
  | Text T.Text

-- | Compile an output tree to a VNode
compileGnat :: (RouteWidget path)
            => (WidgetSum WidgetStop GnatEvent -> IO ())
            -> WidgetProd path GnatDOM
            -> [VNode]
compileGnat _ (WidgetProdUnit (Text t)) = [text $ textToJSString t]
compileGnat handler prod@(WidgetProdUnit (Element f)) = 
  let eventAttributes = map ($ handler . routeWidgetProd prod . WidgetSumStop) gnatEvents
  in [f eventAttributes ()]
compileGnat handler (WidgetProdConcat left right) = compileGnat handler left ++ compileGnat handler right
compileGnat handler prod@(WidgetProdParentChild (Element parentF) child) =
  let eventAttributes = map ($ handler . routeWidgetProd prod . WidgetSumStop) gnatEvents
  in [parentF eventAttributes $ compileGnat handler child]
compileGnat handler prod@(WidgetProdParentChild (Text t) _) = -- need to fix this :\
  [text $ textToJSString t]

-- | Events to watch on each element
gnatEvents :: [(GnatEvent -> IO ()) -> Attribute]
gnatEvents = 
  [ mkMouseEvent click MouseClick
  , mkMouseEvent dblclick MouseDoubleClick
  , mkMouseEvent mousedown MouseDown
  , mkMouseEvent mouseenter MouseEnter
  , mkMouseEvent mouseleave MouseLeave
  , mkMouseEvent mousemove MouseMove
  , mkMouseEvent mouseout MouseOut
  , mkMouseEvent mouseover MouseOver
  , mkMouseEvent mouseup MouseUp
  , mkKeyEvent keydown KeyDown
  , mkKeyEvent keypress KeyPress
  , mkKeyEvent keyup KeyUp
  , mkFocusEvent focus Focus
  , mkFocusEvent blur Blur
  , mkGenericEvent submit Submit
  , mkGenericEvent change Change
  ]

  where
    mkMouseEvent :: ((MouseEvent -> IO ()) -> Attribute)
                 -> (MouseLocation -> GnatEvent)
                 -> (GnatEvent -> IO ())
                 -> Attribute
    mkMouseEvent attributeMaker constructor handler =
      attributeMaker (\mouseEvt -> handler (constructor $ MouseLocation (clientX mouseEvt) (clientY mouseEvt) (button mouseEvt)))

    mkKeyEvent :: ((KeyboardEvent -> IO ()) -> Attribute)
               -> (Key -> GnatEvent)
               -> (GnatEvent -> IO ())
               -> Attribute
    mkKeyEvent attributeMaker constructor handler =
      attributeMaker (\keyEvt -> handler (constructor $ Key (head $ JSString.unpack $ key keyEvt)
                                                            (ctrlKey keyEvt)
                                                            (metaKey keyEvt)
                                                            (shiftKey keyEvt)))

    mkFocusEvent :: ((FocusEvent -> IO ()) -> Attribute)
                 -> GnatEvent
                 -> (GnatEvent -> IO ())
                 -> Attribute
    mkFocusEvent attributeMaker constructor handler = 
      attributeMaker $ const $ handler constructor

    mkGenericEvent :: ((Event -> IO ()) -> Attribute)
                   -> GnatEvent
                   -> (GnatEvent -> IO ())
                   -> Attribute
    mkGenericEvent attributeMaker constructor handler =
     attributeMaker $ const $ handler constructor
              
   

-- | lets us force the type variable in the path
routeWidgetProd :: (RouteWidget path) => WidgetProd path b -> WidgetSum path a -> WidgetSum WidgetStop a
routeWidgetProd = const routeWidget

-- | Gnats representation of DOM event
data GnatEvent
  = MouseClick MouseLocation
  | MouseDoubleClick MouseLocation
  | MouseDown MouseLocation
  | MouseEnter MouseLocation
  | MouseLeave MouseLocation
  | MouseMove MouseLocation
  | MouseOut MouseLocation
  | MouseOver MouseLocation
  | MouseUp MouseLocation
  | MouseWheel WheelDelta
  | KeyDown Key 
  | KeyPress Key
  | KeyUp Key
  | Focus
  | Blur
  | Submit
  | Change

data MouseLocation
  = MouseLocation
    { _mouseX :: Int
    , _mouseY :: Int
    , _button  :: Int
    }

data WheelDelta
  = WheelDelta
    { _wheelDeltaX :: Double
    , _wheelDeltaY :: Double
    , _wheelDeltaZ :: Double
    , _wheelDeltaMode :: Double
    }

data Key
  = Key
  { _keyWhich :: Char
  , _keyCtrl :: Bool
  , _keyMeta :: Bool
  , _keyShift :: Bool
  }

domText :: T.Text -> Widget path SVEmpty SVEmpty
domText text = ignore >>> (constant $ WidgetProdUnit $ Text text) >>> uncancelRight

dynamicDomText :: Widget path (SVSignal T.Text) SVEmpty
dynamicDomText = first ignore >>> cancelLeft >>> pureSignalTransformer (WidgetProdUnit . Text) >>> uncancelRight

domElement :: T.Text -> [Attribute] -> Widget path SVEmpty (SVEvent GnatEvent)
domElement tag attributes =
  swap >>> (second $ filter sumStop) >>> (first $ constant $ WidgetProdUnit element)
  where
    sumStop :: WidgetSum path a -> Maybe a
    sumStop (WidgetSumStop x) = Just x
    sumStop _ = Nothing
    
    element :: GnatDOM
    element = Element (\addedAttributes children -> custom (textToJSString tag) (attributes ++ addedAttributes) children)

dynamicDomElement :: T.Text -> [Attribute] -> Widget path (SVSignal [Attribute]) (SVEvent GnatEvent)
dynamicDomElement tag attributes = 
  swap >>> (second $ filter sumStop) >>> (first $ pureSignalTransformer $ WidgetProdUnit . element)
  where
    sumStop :: WidgetSum path a -> Maybe a
    sumStop (WidgetSumStop x) = Just x
    sumStop _ = Nothing

    element :: [Attribute] -> GnatDOM
    element signalAttributes = Element (\addedAttributes children -> custom (textToJSString tag) (signalAttributes ++ attributes ++ addedAttributes) children)

-- Copied from ghcjs-vdom/example/Example.hs
atAnimationFrame :: IO () -> IO ()
atAnimationFrame m = do
  cb <- fixIO $ \cb ->
    syncCallback ContinueAsync (releaseCallback cb >> m)
  [js_| window.requestAnimationFrame(`cb); |]

epochMillisecondsNow :: IO Int
epochMillisecondsNow = [js| Date.now(); |]

epochSecondsNow :: IO Double
epochSecondsNow = fmap ((/ 1000) . fromIntegral) epochMillisecondsNow

createRoot :: IO DOMNode
createRoot = [js| document.createElement('div') |]

attachRoot :: DOMNode -> IO ()
attachRoot root = [js_| document.body.appendChild(`root); |]

mkRoot :: IO DOMNode
mkRoot = do
  root <- createRoot
  attachRoot root
  return root

runWidget :: Widget WidgetStop (SVEvent a) (SVEvent (IO a)) -> IO ()
runWidget widget = do
  root <- mkRoot
  vmount <- mount root $ text ""
  renderer <- mkRenderer
  timeNow <- epochSecondsNow
  widgetStateMVar <- newEmptyMVar
  putMVar widgetStateMVar $ initSFEval
    (signalHandler (\gnat ->
       let pushDomEvent = (\domEvt -> do
             evalState <- takeMVar widgetStateMVar
             ((), evalState') <- runSFEvalT (push $ svLeft $ svOcc domEvt) evalState
             putMVar widgetStateMVar evalState')
           vnodes = compileGnat pushDomEvent gnat
           vnode = custom "div" () vnodes
       in render renderer vmount vnode)
    `combineHandlers`
    eventHandler (const $ return ()))
    (sampleEvt `combineSamples` sampleEvt)
    timeNow
    widget
  atAnimationFrame $ do
    evalState <- takeMVar widgetStateMVar
    timeNow <- epochSecondsNow
    ((), evalState') <- runSFEvalT (step timeNow) evalState
    putMVar widgetStateMVar $ evalState'
 
